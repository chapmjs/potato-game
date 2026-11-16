# The Potato Game -- a supply-chain ordering game for SCM 361 (BYU-Idaho).
# R Shiny + SQLite. See README.md for how to run and deploy.

library(shiny)
library(bslib)
library(DBI)

for (f in c("engine.R", "db.R", "charts.R")) source(file.path("R", f), local = FALSE)

con <- db_connect()
onStop(function() try(dbDisconnect(con), silent = TRUE))

# Shared by every browser connected to this R process: when one player does
# something, the others' screens are told at once instead of waiting to poll.
# (Polling stays on as a slower fallback in case several R processes are used.)
pulse <- reactiveValues()
ring <- function(...) for (k in c(...)) pulse[[k]] <- (isolate(pulse[[k]]) %||% 0) + 1
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

admin_password <- function() {
  pw <- Sys.getenv("POTATO_ADMIN_PW", "")
  f <- file.path(dirname(db_path()), "instructor_password.txt")
  if (!nzchar(pw) && file.exists(f)) pw <- trimws(paste(readLines(f, warn = FALSE), collapse = ""))
  pw
}

money <- function(x) paste0("$", formatC(round(x), format = "d", big.mark = ","))
tile <- function(label, value, sub = NULL, class = "") {
  div(class = paste("tile", class), div(class = "tile-label", label),
      div(class = "tile-value", value), if (!is.null(sub)) div(class = "tile-sub", sub))
}
send_btn <- function(label, input, value, class = "btn btn-outline-primary btn-sm") {
  tags$button(type = "button", class = class, `data-send` = input, `data-value` = value, label)
}

rules_box <- function(sess, role, open = FALSE) {
  tags$details(class = "rules", open = if (open) NA else NULL,
    tags$summary("How the game works"),
    tags$ul(
      tags$li(HTML(sprintf("You run the <b>%s</b>. Each week you receive an order from %s, ship what you have, and decide how much to order from %s.",
                           ROLE_NAMES[role], ROLE_SELLS_TO[role], ROLE_BUYS_FROM[role]))),
      tags$li(HTML(if (role == N_ROLES)
        "What you order goes into production and reaches your stock <b>two weeks later</b>."
        else "Your order reaches your supplier <b>next week</b>; what they ship takes <b>two more weeks</b> to arrive. If they are out of stock, it takes longer.")),
      tags$li(HTML(sprintf("Every unit left in stock costs <b>%s</b> a week. Every unit you owe but cannot ship (backlog) costs <b>%s</b> a week, and you still have to ship it later.",
                           money(sess$hold_cost), money(sess$backlog_cost)))),
      tags$li(HTML(sprintf("The game lasts <b>%d weeks</b>. Lowest total cost wins.", sess$weeks)))
    ))
}

ui <- page_fluid(
  theme = bs_theme(version = 5, primary = "#7a4b22", secondary = "#5f6b3a",
                   bg = "#fbf8f3", fg = "#2b2622", "border-radius" = "0.6rem"),
  title = "The Potato Game",
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$script(src = "app.js")
  ),
  div(class = "shell",
    div(class = "topbar",
      div(class = "brand", span(class = "spud", HTML("&#129364;")), "The Potato Game"),
      uiOutput("who", inline = TRUE)),
    uiOutput("tabs"),
    uiOutput("main"),
    div(class = "footer", actionLink("to_instructor", "Instructor"))
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(player = NULL, tab = "solo", tick = 0, solo_gid = NULL, watch = NULL,
                       new_team = FALSE, admin = FALSE, admin_view = FALSE, nonce = 0)
  screen  <- reactiveVal("loading")
  sess_rv <- reactiveVal(NULL)    # class session row
  board   <- reactiveVal(NULL)    # my position this week
  mates   <- reactiveVal(NULL)    # who has ordered (team)
  lobby   <- reactiveVal(NULL)    # teams forming
  done_g  <- reactiveVal(NULL)    # id of finished game being shown
  esc <- htmltools::htmlEscape
  bump <- function() rv$tick <- isolate(rv$tick) + 1
  oops <- function(e) showNotification(conditionMessage(e), type = "error", duration = 6)

  # ---- sign in ---------------------------------------------------------------
  observeEvent(input$stored_token, {
    p <- player_by_token(con, input$stored_token)
    if (!is.null(p)) rv$player <- p
    if (!is.null(getQueryString()$instructor)) rv$admin_view <- TRUE
    bump()
  })

  observeEvent(input$join, {
    tryCatch({
      s <- get_session(con, input$join_code %||% "")
      if (is.null(s)) stop("That class code was not found. Check the code on the screen.")
      p <- join_player(con, s$id, input$join_name %||% "")
      rv$player <- p; rv$tab <- "solo"; rv$solo_gid <- NULL
      session$sendCustomMessage("store_token", p$token)
      bump()
    }, error = oops)
  })

  observeEvent(input$sign_out, {
    rv$player <- NULL; rv$solo_gid <- NULL; rv$new_team <- FALSE
    session$sendCustomMessage("store_token", "")
    bump()
  })

  observeEvent(input$tab, { rv$tab <- input$tab$v; bump() })

  # ---- the heartbeat ----------------------------------------------------------
  # Each browser asks one cheap question on a timer ("has anything I can see
  # changed?") and only re-reads the game when the answer is yes.
  seen <- new.env()
  observe({
    rv$tick
    if (is.null(input$stored_token)) return()
    if (rv$admin_view) { screen(if (rv$admin) "admin" else "admin_login"); return() }
    p <- rv$player
    if (is.null(p)) { screen("join"); return() }
    invalidateLater(if (rv$tab == "team") 5000 else 20000)
    rung <- paste(rv$tab, rv$tick, pulse[[paste0("s", p$session_id)]],
                  if (!is.null(rv$watch)) pulse[[paste0("g", rv$watch)]])
    isolate({
      if (identical(seen$rung, rung)) {
        # woken by the timer only: one cheap query decides whether to look closer
        stamp <- player_stamp(con, p, rv$tab, screen() == "team_lobby")
        if (identical(seen$stamp, stamp)) return()
        seen$stamp <- stamp
      } else seen$stamp <- NULL
      seen$rung <- rung
      s <- if (rv$tab == "team" && screen() != "team_play") get_session_id(con, p$session_id)
           else session_rules(con, p$session_id)
      sess_rv(s)
      if (rv$tab == "solo") {
        v <- if (!is.null(rv$solo_gid)) game_view(con, rv$solo_gid) else NULL
        if (is.null(v)) { rv$solo_gid <- solo_game_id(con, p); v <- game_view(con, rv$solo_gid) }
      } else {
        # mid-game: one query. Otherwise find out where this player stands.
        v <- if (!is.null(rv$watch) && screen() == "team_play") game_view(con, rv$watch) else NULL
        if (is.null(v) || v$game$status != "active" || !any(v$pos$player_id %in% p$id)) {
          if (!s$team_open) { screen("team_closed"); return() }
          gid <- team_game_id(con, p$id)
          v <- if (is.na(gid)) NULL else game_view(con, gid)
        }
        if (!is.null(v) && v$game$status != "done") rv$new_team <- FALSE
        if (is.null(v) || v$game$status == "lobby" || rv$new_team) {
          teams <- list_teams(con, s$id)
          teams <- teams[teams$status == "lobby", , drop = FALSE]
          lobby(list(mine = if (!is.null(v) && v$game$status == "lobby") v else NULL,
                     teams = teams,
                     roster = lapply(teams$id, function(i) game_view(con, i)$pos[, c("role", "player_id", "player_name")])))
          rv$watch <- NULL
          screen("team_lobby"); return()
        }
      }
      me <- v$pos[!is.na(v$pos$player_id) & v$pos$player_id == p$id, ]
      if (v$game$status == "done") {
        done_g(v$game$id)
        screen(paste0(rv$tab, "_done"))
      } else {
        rv$watch <- v$game$id
        board(list(game = v$game$id, mode = v$game$mode, team = v$game$team_name, week = v$game$week,
                   me = me, nonce = rv$nonce))
        mates(v$pos[, c("role", "player_id", "player_name", "pending_order")])
        screen(paste0(rv$tab, "_play"))
      }
    })
  })

  # ---- frame -----------------------------------------------------------------
  output$who <- renderUI({
    p <- rv$player
    if (is.null(p) || rv$admin_view) return(NULL)
    span(class = "who", p$name, " | ", actionLink("sign_out", "not you?"))
  })

  output$tabs <- renderUI({
    if (is.null(rv$player) || rv$admin_view) return(NULL)
    mk <- function(id, label) tags$button(type = "button", `data-send` = "tab", `data-value` = id,
      class = paste("tabbtn", if (rv$tab == id) "active"), label)
    div(class = "tabs", mk("solo", "Round 1: On your own"), mk("team", "Round 2: As a team"))
  })

  output$main <- renderUI({
    switch(screen(),
      loading = div(class = "muted pad", "Loading..."),
      join = div(class = "card-ish narrow",
        h2("Join your class"),
        p(class = "muted", "Enter the class code on the screen and your name as it appears on the roll."),
        textInput("join_code", "Class code", value = toupper(getQueryString()$code %||% ""), width = "100%"),
        textInput("join_name", "First and last name", width = "100%"),
        actionButton("join", "Join the game", class = "btn-primary btn-lg w-100")),
      solo_play = , team_play = tagList(uiOutput("play_head"), uiOutput("play_rules"), uiOutput("play_board"),
                                        uiOutput("play_mates"), uiOutput("play_hist")),
      solo_done = , team_done = uiOutput("results"),
      team_closed = div(class = "card-ish narrow", h2("Round 2 has not opened yet"),
        p("Your instructor will open the team round when the class is ready. This page will update on its own.")),
      team_lobby = uiOutput("lobby_ui"),
      admin_login = div(class = "card-ish narrow", h2("Instructor"),
        if (!nzchar(admin_password()))
          p(class = "warn", "No instructor password is set on the server yet. See README.md, \"Instructor password\".")
        else tagList(passwordInput("admin_pw", "Password", width = "100%"),
                     actionButton("admin_go", "Sign in", class = "btn-primary w-100")),
        p(class = "mt-3", actionLink("admin_back", "Back to the game"))),
      admin = uiOutput("admin_ui"))
  })

  # ---- playing ---------------------------------------------------------------
  # These three redraw on every order in the room, so they are built as plain
  # HTML strings (about 20x faster than nested tag objects).
  tile_html <- function(label, value, sub, bad = FALSE) sprintf(
    "<div class='tile%s'><div class='tile-label'>%s</div><div class='tile-value'>%s</div><div class='tile-sub'>%s</div></div>",
    if (bad) " bad" else "", label, value, sub)

  my_role <- reactiveVal(NULL)   # changes only when the player's role does
  observe({ b <- board(); if (!is.null(b) && nrow(b$me) == 1) my_role(c(b$game, b$me$role)) })
  output$play_rules <- renderUI({
    r <- my_role(); s <- sess_rv(); req(r, s)
    rules_box(s, r[2], open = isolate(board()$week) == 1)
  })

  output$play_head <- renderUI({
    b <- board(); s <- sess_rv(); req(b, s, nrow(b$me) == 1)
    HTML(sprintf(paste0(
      "<div class='playhead'><div><div class='role'>%s</div><div class='muted'>%s</div></div>",
      "<div class='week'>Week %d of %d</div></div>",
      "<div class='progress' style='height:6px'><div class='progress-bar' style='width:%d%%'></div></div>"),
      ROLE_NAMES[b$me$role],
      if (b$mode == "team") paste("Team", esc(b$team)) else "The computer runs the rest of your chain",
      b$week, s$weeks, round(100 * (b$week - 1) / s$weeks)))
  })

  output$play_board <- renderUI({
    b <- board(); s <- isolate(sess_rv()); req(b, s, nrow(b$me) == 1)
    m <- b$me
    if (nzchar(Sys.getenv("POTATO_DEBUG"))) message(format(Sys.time(), "%H:%M:%OS3"), " board render p", rv$player$id, " g", b$game, " w", b$week, " pend ", m$pending_order)
    this_cost <- m$stock * s$hold_cost + m$backlog * s$backlog_cost
    tiles <- paste0(
      tile_html(paste("Order from", ROLE_SELLS_TO[m$role]), m$demand, "received this week"),
      tile_html("You shipped", m$shipped,
                if (m$backlog > 0) sprintf("of %d owed - short by %d", m$shipped + m$backlog, m$backlog) else "everything owed"),
      tile_html("In stock", m$stock, sprintf("%s per unit each week", money(s$hold_cost))),
      tile_html("Backlog", m$backlog, sprintf("%s per unit each week", money(s$backlog_cost)), m$backlog > 0),
      tile_html(if (m$role == N_ROLES) "In production" else "On its way to you",
                sprintf("%d <span class='tile-and'>then</span> %d", m$pipe1, m$pipe2),
                "arriving next week, then the week after"),
      tile_html("Cost", money(m$total_cost + this_cost), sprintf("%s this week", money(this_cost))))
    form <- if (!is.na(m$pending_order))
      sprintf("<div class='placed'>You ordered <b>%d</b>. Waiting for your teammates...</div>", m$pending_order)
    else sprintf(paste0(
      "<label for='order_qty'>How many units will you order from %s?</label><div class='orderrow'>",
      "<input id='order_qty' type='number' min='0' step='1' inputmode='numeric' autocomplete='off' class='form-control form-control-lg'>",
      "<button id='place_order' type='button' class='btn btn-primary btn-lg' data-week='%d' data-game='%d'>Place order</button>",
      "</div><div id='order_msg' class='warn small'></div>"),
      if (m$role == N_ROLES) ROLE_BUYS_FROM[m$role] else paste("the", ROLE_BUYS_FROM[m$role]), b$week, b$game)
    HTML(paste0("<div class='grid'>", tiles, "</div><div class='orderbox'>", form, "</div>"))
  })

  observeEvent(input$order_submit, {
    o <- input$order_submit; p <- rv$player; req(p)
    cur <- isolate(board())
    # only the game on screen right now can be ordered for
    res <- if (is.null(cur) || !identical(as.numeric(cur$game), as.numeric(o$game))) "stale"
           else tryCatch(submit_order(con, o$game, p$id, o$week, o$qty), error = function(e) "no")
    if (nzchar(Sys.getenv("POTATO_DEBUG"))) message(format(Sys.time(), "%H:%M:%OS3"), " submit p", p$id, " g", o$game, " w", o$week, " q", o$qty, " -> ", res)
    if (res == "no") showNotification("Enter a whole number, 0 or more.", type = "error")
    if (res == "ok") ring(paste0("g", o$game))
    rv$nonce <- rv$nonce + 1
    bump()
  })

  output$play_mates <- renderUI({
    b <- isolate(board()); m <- mates(); req(b, m)
    if (b$mode != "team") return(NULL)
    ghost <- is.na(m$player_id); placed <- !is.na(m$pending_order)
    HTML(paste0("<div class='mates'>", paste0(sprintf(
      "<div class='mate%s'><div class='mate-role'>%s</div><div class='mate-name'>%s</div><div class='mate-state'>%s</div></div>",
      ifelse(ghost | placed, " ready", ""), ROLE_SHORT[m$role],
      ifelse(ghost, "Computer", esc(m$player_name)),
      ifelse(ghost, "plays automatically", ifelse(placed, "ordered", "deciding..."))), collapse = ""), "</div>"))
  })

  output$play_hist <- renderUI({
    b <- board(); req(b, nrow(b$me) == 1)
    h <- game_history(con, b$game)
    h <- h[h$role == b$me$role, ]
    if (!nrow(h)) return(NULL)
    h <- h[order(-h$week), ]
    rows <- sprintf("<tr><td>%d</td><td>%d</td><td>%d</td><td>%d</td><td%s>%d</td><td><b>%d</b></td><td>%s</td></tr>",
                    h$week, h$demand, h$shipped, h$stock, ifelse(h$backlog > 0, " class='bad'", ""),
                    h$backlog, h$order_qty, money(h$cost))
    HTML(paste0("<h3>Your weeks so far</h3><div class='tablewrap'><table class='table table-sm hist'><thead><tr>",
                "<th>Wk</th><th>Order in</th><th>Shipped</th><th>Stock</th><th>Backlog</th><th>You ordered</th><th>Cost</th>",
                "</tr></thead><tbody>", paste(rows, collapse = ""), "</tbody></table></div>"))
  })

  # ---- team lobby ------------------------------------------------------------
  # The create-team box is drawn once and left alone, so a name being typed is
  # not wiped when other teams in the room change.
  has_team <- reactiveVal(FALSE)   # a reactiveVal only fires when its value really changes
  observe(has_team(!is.null(lobby()$mine)))
  output$lobby_ui <- renderUI({
    req(rv$player)
    tagList(
      if (has_team()) uiOutput("lobby_mine")
      else div(class = "card-ish",
        h2("Start a team"),
        p(class = "muted", "One person creates the team. Everyone else joins it from the list below."),
        div(class = "orderrow", textInput("team_name", NULL, placeholder = "Team name", width = "100%"),
            actionButton("team_create", "Create team", class = "btn-primary"))),
      uiOutput("lobby_teams"))
  })

  role_row <- function(gid, pos, me) div(class = "roles", lapply(1:N_ROLES, function(r) {
    holder <- pos$player_id[r]
    div(class = "rolecard",
      div(class = "mate-role", ROLE_NAMES[r]),
      if (is.na(holder)) tagList(div(class = "muted", "open"), send_btn("Take this role", "claim", paste(gid, r)))
      else if (holder == me) div(class = "you", "You")
      else div(pos$player_name[r]))
  }))

  output$lobby_mine <- renderUI({
    my <- lobby()$mine; p <- rv$player; req(my, p)
    holding <- sum(!is.na(my$pos$player_id))
    div(class = "card-ish",
      h2(paste("Team", my$game$team_name)),
      p(class = "muted", "Teammates open Round 2 on their own device and take a role in this team. One person per role."),
      role_row(my$game$id, my$pos, p$id),
      if (holding < N_ROLES) p(class = "warn mt-2", sprintf("%d role%s still open. The computer will play any role left open when you start.",
                                                           N_ROLES - holding, if (N_ROLES - holding == 1) " is" else "s are")),
      div(class = "mt-3 d-flex gap-2 flex-wrap",
        send_btn("Start the game for the whole team", "team_start", my$game$id, "btn btn-primary btn-lg"),
        send_btn("Leave this team", "team_leave", 1, "btn btn-outline-secondary")))
  })

  # Redrawn for everyone in the lobby whenever any team changes, so: plain strings.
  output$lobby_teams <- renderUI({
    L <- lobby(); p <- rv$player; req(L, p)
    mine <- if (is.null(L$mine)) -1L else L$mine$game$id
    others <- which(L$teams$id != mine)
    if (!length(others)) return(NULL)
    team_html <- vapply(others, function(i) {
      pos <- L$roster[[i]]; gid <- L$teams$id[i]; open <- is.na(pos$player_id)
      cards <- sprintf("<div class='rolecard'><div class='mate-role'>%s</div>%s</div>", ROLE_NAMES[pos$role],
        ifelse(open, sprintf("<div class='muted'>open</div><button type='button' class='btn btn-outline-primary btn-sm' data-send='claim' data-value='%d %d'>Take this role</button>", gid, pos$role),
               ifelse(!open & pos$player_id %in% p$id, "<div class='you'>You</div>",
                      sprintf("<div>%s</div>", esc(ifelse(open, "", pos$player_name))))))
      sprintf("<div class='teamrow'><h3>%s</h3><div class='roles'>%s</div></div>",
              esc(L$teams$team_name[i]), paste(cards, collapse = ""))
    }, character(1))
    HTML(sprintf("<div class='card-ish'><h2>%s</h2>%s</div>",
                 if (mine < 0) "Or join a team" else "Other teams forming", paste(team_html, collapse = "")))
  })

  observeEvent(input$team_create, {
    tryCatch({
      gid <- create_team(con, rv$player, input$team_name %||% "")
      claim_role(con, rv$player, gid, 1L)
      ring(paste0("s", rv$player$session_id)); rv$new_team <- FALSE; bump()
    }, error = oops)
  })
  observeEvent(input$claim, {
    x <- as.integer(strsplit(as.character(input$claim$v), " ")[[1]])
    tryCatch({ claim_role(con, rv$player, x[1], x[2]); ring(paste0("s", rv$player$session_id))
               rv$new_team <- FALSE; bump() }, error = oops)
  })
  observeEvent(input$team_start, {
    tryCatch({ start_team(con, rv$player, as.integer(input$team_start$v))
               ring(paste0("s", rv$player$session_id)); bump() }, error = oops)
  })
  observeEvent(input$team_leave, { leave_team(con, rv$player); ring(paste0("s", rv$player$session_id)); bump() })
  observeEvent(input$team_again, { rv$new_team <- TRUE; bump() })

  # ---- results ---------------------------------------------------------------
  res <- reactive({
    gid <- done_g(); p <- rv$player; req(gid, p, grepl("_done$", screen()))
    v <- game_view(con, gid); req(v)
    s <- get_session_id(con, v$game$session_id)
    h <- game_history(con, gid)
    demand <- parse_demand(s$demand_seq, s$weeks)
    my_role <- v$pos$role[!is.na(v$pos$player_id) & v$pos$player_id == p$id]
    all <- export_results(con, s$id)
    all <- all[all$status == "done" & all$round == (if (v$game$mode == "solo") "1-individual" else "2-team"), ]
    mine <- all[all$player == p$name & (v$game$mode == "solo" | all$team == (v$game$team_name %||% "")), ]
    n <- if (v$game$mode == "solo") nrow(all) else length(unique(all$team))
    rk <- if (v$game$mode == "solo") mine$rank[1] else match(mine$score[1], sort(unique(all$score)))
    list(v = v, s = s, h = h, demand = demand, role = my_role, bw = bullwhip(h, demand),
         rank = rk, n = n, team = v$game$mode == "team")
  })

  output$results <- renderUI({
    r <- res(); v <- r$v
    own <- v$pos$total_cost[r$role]; chain <- sum(v$pos$total_cost)
    tagList(
      div(class = "card-ish",
        h2(if (r$team) paste("Team", v$game$team_name, "- results") else "Your results"),
        div(class = "grid",
          if (r$team) tile("Team total cost", money(chain), "all four roles")
          else tile("Your total cost", money(own), sprintf("over %d weeks", r$s$weeks)),
          tile(if (r$team) "Team rank" else "Class rank", sprintf("%s of %d", r$rank, r$n),
               "among those finished so far"),
          tile(if (r$team) "Your cost" else "Chain total cost", money(if (r$team) own else chain),
               if (r$team) ROLE_NAMES[r$role] else "you plus the three computer roles"),
          tile("Your bullwhip ratio", if (is.na(r$bw[r$role])) "-" else sprintf("%.1fx", r$bw[r$role]),
               "swing in your orders vs. shoppers'")),
        p(class = "muted mt-2", "Shoppers' demand changed only once, and only a little. Compare that with how far each role's orders swung.")),
      div(class = "card-ish", h3("Orders placed by each role, against what shoppers bought"),
        plotOutput("res_orders", height = "360px"),
        div(class = "tablewrap", tableOutput("res_roles"))),
      div(class = "card-ish", h3(sprintf("%s: stock and backlog each week", ROLE_NAMES[r$role])),
        plotOutput("res_stock", height = "300px")),
      div(class = "card-ish", h3("Week by week"),
        div(class = "tablewrap", tableOutput("res_weeks")),
        downloadButton("dl_mine", "Download my results (CSV)", class = "btn-outline-primary"),
        if (r$team) send_btn("Play again with a new team", "team_again", 1, "btn btn-outline-secondary ms-2")))
  })

  output$res_orders <- renderPlot({
    r <- res()
    orders_chart(r$h, r$demand, highlight = r$role, title = NULL,
                 narrow = isTRUE(session$clientData$output_res_orders_width < 520))
  }, res = 96)
  output$res_stock <- renderPlot({
    r <- res()
    stock_chart(r$h, r$role, title = NULL, narrow = isTRUE(session$clientData$output_res_stock_width < 520))
  }, res = 96)
  output$res_roles <- renderTable({
    r <- res(); pos <- r$v$pos
    data.frame(Role = ROLE_NAMES, `Played by` = ifelse(is.na(pos$player_id), "Computer", pos$player_name),
               `Total cost` = money(pos$total_cost), `Largest order` = tapply(r$h$order_qty, r$h$role, max),
               `Bullwhip ratio` = sprintf("%.1fx", r$bw), check.names = FALSE)
  }, striped = TRUE, align = "llrrr")
  my_weeks <- reactive({
    r <- res(); h <- r$h[r$h$role == r$role, ]
    data.frame(Week = h$week, `Order received` = h$demand, Received = h$received, Shipped = h$shipped,
               Stock = h$stock, Backlog = h$backlog, `You ordered` = h$order_qty, Cost = h$cost,
               check.names = FALSE)
  })
  output$res_weeks <- renderTable(my_weeks(), digits = 0, striped = TRUE)
  output$dl_mine <- downloadHandler(
    filename = function() sprintf("potato-game-%s-%s.csv", if (res()$team) "team" else "individual",
                                  gsub("[^A-Za-z0-9]+", "-", rv$player$name)),
    content = function(file) {
      r <- res(); d <- my_weeks()
      team <- r$v$game$team_name
      out <- data.frame(Player = rv$player$name, Role = ROLE_NAMES[r$role],
                        Team = if (is.na(team)) "" else team, d,
                        `Shopper demand` = r$demand[d$Week], check.names = FALSE)
      write.csv(out, file, row.names = FALSE)
    })

  # ---- instructor ------------------------------------------------------------
  observeEvent(input$to_instructor, { rv$admin_view <- TRUE; bump() })
  observeEvent(input$admin_back, { rv$admin_view <- FALSE; bump() })
  observeEvent(input$admin_go, {
    pw <- admin_password()
    if (nzchar(pw) && identical(input$admin_pw, pw)) { rv$admin <- TRUE; bump() }
    else showNotification("Wrong password.", type = "error")
  })

  admin_tick <- reactiveVal(0)   # live tables changed
  sess_tick  <- reactiveVal(0)   # list of classes changed
  a_sessions <- reactive({ req(rv$admin); sess_tick(); list_sessions(con) })
  a_sess <- reactive({
    s <- a_sessions(); req(nrow(s) > 0)
    id <- as.integer(input$a_session %||% s$id[1])
    s[s$id == (if (id %in% s$id) id else s$id[1]), ]
  })
  a_live <- reactive({
    req(rv$admin, screen() == "admin"); invalidateLater(3000); admin_tick()
    s <- a_sess()
    list(solo = solo_progress(con, s$id), team = team_progress(con, s$id),
         open = get_session_id(con, s$id)$team_open == 1)
  })

  output$admin_ui <- renderUI({
    req(rv$admin); sess_tick()
    s <- list_sessions(con)
    cur <- isolate(input$a_session) %||% (if (nrow(s)) s$id[1] else NULL)
    tagList(
      div(class = "card-ish",
        div(class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
          h2("Instructor"), actionLink("admin_back", "Back to the student view")),
        if (nrow(s)) selectInput("a_session", "Class", width = "100%", selected = cur,
          choices = setNames(s$id, sprintf("%s - %s (%s)", s$code, s$title, substr(s$created_at, 1, 10)))),
        tags$details(open = if (!nrow(s)) NA else NULL, tags$summary("Create a new class"),
          div(class = "formgrid mt-2",
            textInput("n_code", "Class code students type", placeholder = "e.g. SPUD1"),
            textInput("n_title", "Label for you", placeholder = "e.g. SCM 361 section 2, Fall 2026"),
            numericInput("n_weeks", "Weeks", 20, 5, 60),
            numericInput("n_stock", "Starting stock", 12, 0, 100),
            numericInput("n_hold", "Stock cost per unit per week ($)", 5, 0),
            numericInput("n_back", "Backlog cost per unit per week ($)", 25, 0)),
          textInput("n_demand", "Shopper demand by week (comma-separated; the last number repeats)",
                    value = "4,4,4,4,8", width = "100%"),
          actionButton("n_create", "Create class", class = "btn-primary"))),
      if (nrow(s)) tagList(
        uiOutput("a_join"),
        div(class = "card-ish", h3("Round 1: Individual"), uiOutput("a_solo_sum"),
          div(class = "tablewrap", tableOutput("a_solo")),
          div(class = "orderrow", uiOutput("a_reset_pick"),
              actionButton("a_reset", "Reset this student's Round 1", class = "btn-outline-danger"))),
        div(class = "card-ish", h3("Round 2: Teams"), uiOutput("a_team_switch"),
          div(class = "tablewrap", tableOutput("a_team")),
          div(class = "orderrow", uiOutput("a_team_pick"),
              actionButton("a_takeover", "Computer takes over roles that haven't ordered", class = "btn-outline-primary"),
              actionButton("a_delteam", "Delete team", class = "btn-outline-danger"))),
        div(class = "card-ish", h3("Class debrief"),
          radioButtons("a_round", NULL, inline = TRUE,
                       choices = c("Round 1: Individual" = "solo", "Round 2: Teams" = "team")),
          plotOutput("a_plot", height = "360px"),
          div(class = "tablewrap", tableOutput("a_bw"))),
        div(class = "card-ish", h3("Export"),
          p(class = "muted", "Results has one row per student per round. Weekly detail has every order, shipment, stock level and cost."),
          downloadButton("dl_results", "Results (CSV)", class = "btn-primary"),
          downloadButton("dl_history", "Weekly detail (CSV)", class = "btn-outline-primary ms-2"))))
  })

  observeEvent(input$n_create, {
    tryCatch({
      s <- create_session(con, input$n_code %||% "", input$n_title %||% "", input$n_weeks,
                          input$n_hold, input$n_back, input$n_stock, input$n_demand)
      sess_tick(sess_tick() + 1)
      updateSelectInput(session, "a_session", selected = s$id)
      showNotification(sprintf("Class %s created.", s$code), type = "message")
    }, error = oops)
  })

  output$a_join <- renderUI({
    s <- a_sess(); cd <- session$clientData
    url <- sprintf("%s//%s%s%s?code=%s", cd$url_protocol, cd$url_hostname,
                   if (nzchar(cd$url_port)) paste0(":", cd$url_port) else "", cd$url_pathname, s$code)
    div(class = "card-ish joincard", div(class = "muted", "Students go to"), div(class = "joinurl", url),
        div(class = "muted", "or open the game and type class code"), div(class = "joincode", s$code),
        div(class = "muted small", sprintf("%d weeks | stock %s, backlog %s per unit per week | shopper demand %s",
            s$weeks, money(s$hold_cost), money(s$backlog_cost), s$demand_seq)))
  })

  output$a_solo_sum <- renderUI({
    d <- a_live()$solo
    p(sprintf("%d joined, %d playing, %d finished", nrow(d),
              sum(d$status == "active", na.rm = TRUE), sum(d$status == "done", na.rm = TRUE)))
  })
  output$a_solo <- renderTable({
    d <- a_live()$solo; req(nrow(d) > 0)
    d$total_cost[is.na(d$total_cost)] <- 0
    d <- d[order(-(d$status %in% "done"), ifelse(d$status %in% "done", d$total_cost, -d$week)), ]
    done <- d$status %in% "done"
    data.frame(Place = ifelse(done, as.character(rank(ifelse(done, d$total_cost, Inf), ties.method = "min")), ""),
               Student = d$name,
               Status = ifelse(done, "Finished", ifelse(is.na(d$week), "Joined", paste("Week", d$week))),
               `Cost so far` = money(d$total_cost), check.names = FALSE)
  }, striped = TRUE, align = "lllr")
  a_names <- reactiveVal(NULL); a_teams <- reactiveVal(NULL)
  observe({ L <- a_live(); a_names(L$solo[, c("game_id", "name")]); a_teams(L$team[, c("game_id", "team_name")]) })
  output$a_reset_pick <- renderUI({
    d <- a_names(); req(d)
    selectInput("a_reset_who", NULL, choices = c("Choose a student" = "", setNames(d$game_id[!is.na(d$game_id)], d$name[!is.na(d$game_id)])),
                selected = isolate(input$a_reset_who))
  })
  observeEvent(input$a_reset, {
    req(nzchar(input$a_reset_who %||% ""))
    showModal(modalDialog(title = "Reset this student's Round 1?",
      "Their individual game and its results are erased and they start again at week 1.",
      footer = tagList(modalButton("Cancel"), actionButton("a_reset_yes", "Reset", class = "btn-danger"))))
  })
  observeEvent(input$a_reset_yes, {
    delete_game(con, as.integer(input$a_reset_who)); removeModal(); admin_tick(admin_tick() + 1)
    ring(paste0("s", a_sess()$id))
  })

  output$a_team_switch <- renderUI({
    open <- a_live()$open
    div(class = "d-flex align-items-center gap-3 flex-wrap mb-2",
      span(class = if (open) "pill on" else "pill", if (open) "Team round is OPEN" else "Team round is closed"),
      actionButton("a_toggle", if (open) "Close the team round" else "Open the team round",
                   class = if (open) "btn-outline-secondary" else "btn-primary"))
  })
  observeEvent(input$a_toggle, {
    s <- a_sess(); set_team_open(con, s$id, !a_live()$open); admin_tick(admin_tick() + 1)
    ring(paste0("s", s$id))
  })
  output$a_team <- renderTable({
    d <- a_live()$team; req(nrow(d) > 0)
    done <- d$status == "done"
    d <- d[order(-done, ifelse(done, d$total_cost, -d$week)), ]; done <- d$status == "done"
    data.frame(Place = ifelse(done, as.character(rank(ifelse(done, d$total_cost, Inf), ties.method = "min")), ""),
               Team = d$team_name, Players = ifelse(is.na(d$members), "", d$members),
               Status = ifelse(done, "Finished", ifelse(d$status == "lobby", "Forming",
                        sprintf("Week %d - waiting on %d", d$week, d$waiting_on))),
               `Chain cost so far` = money(d$total_cost), check.names = FALSE)
  }, striped = TRUE, align = "llllr")
  output$a_team_pick <- renderUI({
    d <- a_teams(); req(d)
    selectInput("a_team_who", NULL, choices = c("Choose a team" = "", setNames(d$game_id, d$team_name)),
                selected = isolate(input$a_team_who))
  })
  observeEvent(input$a_takeover, {
    req(nzchar(input$a_team_who %||% "")); ghost_takeover(con, as.integer(input$a_team_who))
    admin_tick(admin_tick() + 1); ring(paste0("g", input$a_team_who), paste0("s", a_sess()$id))
  })
  observeEvent(input$a_delteam, {
    req(nzchar(input$a_team_who %||% ""))
    showModal(modalDialog(title = "Delete this team?", "The team's game and results are erased.",
      footer = tagList(modalButton("Cancel"), actionButton("a_delteam_yes", "Delete", class = "btn-danger"))))
  })
  observeEvent(input$a_delteam_yes, {
    delete_game(con, as.integer(input$a_team_who)); removeModal(); admin_tick(admin_tick() + 1)
    ring(paste0("g", input$a_team_who), paste0("s", a_sess()$id))
  })

  # Class-average orders by role for finished games in the chosen round.
  a_done <- reactiveVal(NULL)    # finished-game counts; the debrief redraws only when these move
  observe({ L <- a_live(); a_done(c(sum(L$solo$status == "done", na.rm = TRUE), sum(L$team$status == "done"))) })
  a_debrief <- reactive({
    s <- a_sess(); a_done()
    h <- export_history(con, s$id)
    rd <- if ((input$a_round %||% "solo") == "solo") "1-individual" else "2-team"
    h <- h[h$round == rd, ]
    full <- names(which(table(h$game_id) == s$weeks * N_ROLES))
    h <- h[h$game_id %in% full, ]
    req(nrow(h) > 0)
    demand <- parse_demand(s$demand_seq, s$weeks)
    avg <- aggregate(order_placed ~ week + role, h, mean)
    bw <- do.call(rbind, lapply(split(h, h$game_id), function(g)
      bullwhip(data.frame(role = g$role, order_qty = g$order_placed), demand)))
    list(avg = data.frame(week = avg$week, role = avg$role, order_qty = avg$order_placed),
         demand = demand, bw = colMeans(bw, na.rm = TRUE), n = length(full),
         cost = tapply(h$cost, h$role, sum) / length(full))
  })
  output$a_plot <- renderPlot({
    d <- a_debrief()
    orders_chart(d$avg, d$demand, narrow = isTRUE(session$clientData$output_a_plot_width < 520),
                 title = sprintf("Average order by role across %d finished game%s",
                                 d$n, if (d$n == 1) "" else "s"))
  }, res = 96)
  output$a_bw <- renderTable({
    d <- a_debrief()
    data.frame(Role = ROLE_NAMES, `Average bullwhip ratio` = sprintf("%.1fx", d$bw),
               `Average total cost` = money(d$cost), check.names = FALSE)
  }, striped = TRUE, align = "lrr")

  output$dl_results <- downloadHandler(
    filename = function() sprintf("potato-game-results-%s-%s.csv", a_sess()$code, Sys.Date()),
    content = function(file) write.csv(export_results(con, a_sess()$id), file, row.names = FALSE))
  output$dl_history <- downloadHandler(
    filename = function() sprintf("potato-game-weekly-%s-%s.csv", a_sess()$code, Sys.Date()),
    content = function(file) write.csv(export_history(con, a_sess()$id), file, row.names = FALSE))
}

shinyApp(ui, server)
