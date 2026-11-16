# db.R -- SQLite storage for the Potato Game.
#
# Every change to a game happens inside one BEGIN IMMEDIATE transaction, so two
# teammates ordering in the same instant (or two R processes, if Shiny Server
# ever runs more than one) cannot both close the same week.

library(DBI)

SCHEMA <- c(
"CREATE TABLE IF NOT EXISTS sessions (
   id INTEGER PRIMARY KEY, code TEXT NOT NULL UNIQUE, title TEXT,
   weeks INTEGER NOT NULL, hold_cost REAL NOT NULL, backlog_cost REAL NOT NULL,
   init_stock INTEGER NOT NULL, demand_seq TEXT NOT NULL,
   team_open INTEGER NOT NULL DEFAULT 0, created_at TEXT NOT NULL)",
"CREATE TABLE IF NOT EXISTS players (
   id INTEGER PRIMARY KEY, session_id INTEGER NOT NULL REFERENCES sessions(id),
   name TEXT NOT NULL, name_key TEXT NOT NULL, token TEXT NOT NULL UNIQUE,
   created_at TEXT NOT NULL, UNIQUE(session_id, name_key))",
"CREATE TABLE IF NOT EXISTS games (
   id INTEGER PRIMARY KEY, session_id INTEGER NOT NULL REFERENCES sessions(id),
   mode TEXT NOT NULL, team_name TEXT, team_key TEXT,
   status TEXT NOT NULL, week INTEGER NOT NULL DEFAULT 0,
   started_at TEXT, finished_at TEXT, created_at TEXT NOT NULL)",
"CREATE UNIQUE INDEX IF NOT EXISTS games_team ON games(session_id, team_key) WHERE mode = 'team'",
"CREATE INDEX IF NOT EXISTS games_session ON games(session_id, mode)",
"CREATE TABLE IF NOT EXISTS positions (
   game_id INTEGER NOT NULL REFERENCES games(id), role INTEGER NOT NULL,
   player_id INTEGER, pending_order INTEGER,
   stock INTEGER, backlog INTEGER, pipe1 INTEGER, pipe2 INTEGER,
   demand INTEGER, received INTEGER, shipped INTEGER, last_order INTEGER,
   forecast REAL, total_cost REAL, PRIMARY KEY (game_id, role))",
"CREATE INDEX IF NOT EXISTS positions_player ON positions(player_id)",
"CREATE TABLE IF NOT EXISTS history (
   game_id INTEGER NOT NULL REFERENCES games(id), week INTEGER NOT NULL,
   role INTEGER NOT NULL, is_ghost INTEGER NOT NULL, player_id INTEGER,
   demand INTEGER, received INTEGER, shipped INTEGER, stock INTEGER,
   backlog INTEGER, order_qty INTEGER, cost REAL,
   PRIMARY KEY (game_id, week, role))"
)

STATE_COLS <- c("stock", "backlog", "pipe1", "pipe2", "demand", "received",
                "shipped", "last_order", "forecast", "total_cost")

db_path <- function() {
  p <- Sys.getenv("POTATO_DB", "")
  if (nzchar(p)) p else file.path("data", "potato.sqlite")
}

db_connect <- function(path = db_path()) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  con <- dbConnect(RSQLite::SQLite(), path)
  dbExecute(con, "PRAGMA journal_mode = WAL")
  dbExecute(con, "PRAGMA synchronous = NORMAL")
  dbExecute(con, "PRAGMA busy_timeout = 10000")
  dbExecute(con, "PRAGMA foreign_keys = ON")
  for (s in SCHEMA) dbExecute(con, s)
  con
}

now_utc <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC")
key_of  <- function(x) tolower(gsub("\\s+", " ", trimws(x)))
qry <- function(con, sql, ...) {
  a <- unname(list(...))
  if (length(a)) dbGetQuery(con, sql, params = a) else dbGetQuery(con, sql)
}
exe <- function(con, sql, ...) {
  a <- unname(list(...))
  if (length(a)) dbExecute(con, sql, params = a) else dbExecute(con, sql)
}

in_txn <- function(con, code) {
  dbExecute(con, "BEGIN IMMEDIATE")
  ok <- FALSE
  on.exit(if (!ok) try(dbExecute(con, "ROLLBACK"), silent = TRUE))
  out <- force(code)
  dbExecute(con, "COMMIT")
  ok <- TRUE
  out
}

new_token <- function() {
  paste(sample(c(letters, LETTERS, 0:9), 32, replace = TRUE), collapse = "")
}

# ---- class sessions --------------------------------------------------------

create_session <- function(con, code, title = "", weeks = 20L, hold_cost = 5,
                           backlog_cost = 25, init_stock = 12L, demand_seq = NULL) {
  code <- toupper(gsub("[^A-Za-z0-9-]", "", code))
  if (!nzchar(code)) stop("The class code needs letters or numbers.")
  weeks <- as.integer(weeks)
  if (is.na(weeks) || weeks < 5 || weeks > 60) stop("Weeks must be between 5 and 60.")
  if (is.null(demand_seq) || !nzchar(trimws(demand_seq))) demand_seq <- default_demand(weeks)
  demand_seq <- paste(parse_demand(demand_seq, weeks), collapse = ",")
  if (nrow(qry(con, "SELECT 1 FROM sessions WHERE code = ?", code)))
    stop("That class code is already in use.")
  exe(con, "INSERT INTO sessions (code, title, weeks, hold_cost, backlog_cost, init_stock,
            demand_seq, team_open, created_at) VALUES (?,?,?,?,?,?,?,0,?)",
      code, title, weeks, hold_cost, backlog_cost, as.integer(init_stock), demand_seq, now_utc())
  get_session(con, code)
}

get_session <- function(con, code) {
  s <- qry(con, "SELECT * FROM sessions WHERE code = ?", toupper(trimws(code)))
  if (nrow(s)) s else NULL
}
get_session_id <- function(con, id) qry(con, "SELECT * FROM sessions WHERE id = ?", id)

# A class's rules never change after it is created, so read them once.
.rules <- new.env()
session_rules <- function(con, id) {
  k <- as.character(id)
  if (is.null(.rules[[k]])) .rules[[k]] <- get_session_id(con, id)
  .rules[[k]]
}
list_sessions <- function(con) qry(con, "SELECT * FROM sessions ORDER BY id DESC")
set_team_open <- function(con, session_id, open) {
  exe(con, "UPDATE sessions SET team_open = ? WHERE id = ?", as.integer(open), session_id)
}

# ---- players ---------------------------------------------------------------

# Same name + same class code gets the same player back, so a student who
# closes the tab or switches device carries on where they stopped.
join_player <- function(con, session_id, name) {
  name <- substr(gsub("\\s+", " ", trimws(name)), 1, 60)
  if (nchar(name) < 2) stop("Please enter your first and last name.")
  in_txn(con, {
    p <- qry(con, "SELECT * FROM players WHERE session_id = ? AND name_key = ?",
             session_id, key_of(name))
    if (!nrow(p)) {
      exe(con, "INSERT INTO players (session_id, name, name_key, token, created_at)
                VALUES (?,?,?,?,?)", session_id, name, key_of(name), new_token(), now_utc())
      p <- qry(con, "SELECT * FROM players WHERE session_id = ? AND name_key = ?",
               session_id, key_of(name))
    }
    p
  })
}

player_by_token <- function(con, token) {
  if (is.null(token) || !nzchar(token)) return(NULL)
  p <- qry(con, "SELECT * FROM players WHERE token = ?", token)
  if (nrow(p)) p else NULL
}

# ---- games -----------------------------------------------------------------

# One statement, four rows: RSQLite runs the UPDATE once per element.
write_positions <- function(con, game_id, pos) {
  dbExecute(con, "UPDATE positions SET stock=?, backlog=?, pipe1=?, pipe2=?, demand=?, received=?,
            shipped=?, last_order=?, forecast=?, total_cost=?, pending_order=NULL
            WHERE game_id=? AND role=?",
    params = list(pos$stock, pos$backlog, pos$pipe1, pos$pipe2, pos$demand, pos$received,
                  pos$shipped, pos$last_order, pos$forecast, pos$total_cost,
                  rep(game_id, nrow(pos)), pos$role))
}

insert_game <- function(con, sess, mode, team_name = NA_character_) {
  exe(con, "INSERT INTO games (session_id, mode, team_name, team_key, status, week, created_at)
            VALUES (?,?,?,?, 'lobby', 0, ?)",
      sess$id, mode, team_name, if (is.na(team_name)) NA_character_ else key_of(team_name), now_utc())
  gid <- qry(con, "SELECT last_insert_rowid() AS id")$id
  pos <- new_positions(sess$init_stock, parse_demand(sess$demand_seq, sess$weeks)[1])
  dbExecute(con, "INSERT INTO positions (game_id, role) VALUES (?,?)",
            params = list(rep(gid, N_ROLES), 1:N_ROLES))
  write_positions(con, gid, pos)
  gid
}

read_positions <- function(con, game_id) {
  qry(con, "SELECT * FROM positions WHERE game_id = ? ORDER BY role", game_id)
}

begin_game <- function(con, game_id, sess) {
  pos <- read_positions(con, game_id)
  pos <- start_week(pos, parse_demand(sess$demand_seq, sess$weeks)[1])
  write_positions(con, game_id, pos)
  exe(con, "UPDATE games SET status='active', week=1, started_at=? WHERE id=?", now_utc(), game_id)
}

# Round 1: the student is the Potato Processor; the other three are ghosts.
solo_game_id <- function(con, player) {
  in_txn(con, {
    g <- qry(con, "SELECT g.id FROM games g JOIN positions p ON p.game_id = g.id
                   WHERE g.mode = 'solo' AND p.player_id = ? ORDER BY g.id DESC LIMIT 1", player$id)
    if (nrow(g)) g$id else {
      sess <- get_session_id(con, player$session_id)
      gid <- insert_game(con, sess, "solo")
      exe(con, "UPDATE positions SET player_id = ? WHERE game_id = ? AND role = ?",
          player$id, gid, N_ROLES)
      begin_game(con, gid, sess)
      gid
    }
  })
}

# Round 2
create_team <- function(con, player, team_name) {
  team_name <- substr(gsub("\\s+", " ", trimws(team_name)), 1, 40)
  if (nchar(team_name) < 2) stop("Give your team a name.")
  in_txn(con, {
    sess <- get_session_id(con, player$session_id)
    if (nrow(qry(con, "SELECT 1 FROM games WHERE session_id=? AND mode='team' AND team_key=?",
                 sess$id, key_of(team_name))))
      stop("That team name is taken. Pick another, or join it from the list.")
    release_lobby_roles(con, player$id)
    insert_game(con, sess, "team", team_name)
  })
}

release_lobby_roles <- function(con, player_id, except_game = -1L) {
  exe(con, "UPDATE positions SET player_id = NULL WHERE player_id = ? AND game_id != ? AND game_id IN
            (SELECT id FROM games WHERE mode = 'team' AND status = 'lobby')", player_id, except_game)
}

claim_role <- function(con, player, game_id, role) {
  in_txn(con, {
    g <- qry(con, "SELECT * FROM games WHERE id = ? AND mode = 'team'", game_id)
    if (!nrow(g) || g$session_id != player$session_id) stop("That team was not found.")
    if (g$status != "lobby") stop("That team has already started.")
    taken <- qry(con, "SELECT player_id FROM positions WHERE game_id=? AND role=?", game_id, role)$player_id
    if (!is.na(taken) && taken != player$id) stop("Someone just took that role. Pick another.")
    release_lobby_roles(con, player$id)
    exe(con, "UPDATE positions SET player_id = NULL WHERE game_id = ? AND player_id = ?", game_id, player$id)
    exe(con, "UPDATE positions SET player_id = ? WHERE game_id = ? AND role = ?", player$id, game_id, role)
    TRUE
  })
}

leave_team <- function(con, player) {
  in_txn(con, { release_lobby_roles(con, player$id); TRUE })
}

start_team <- function(con, player, game_id) {
  in_txn(con, {
    g <- qry(con, "SELECT * FROM games WHERE id = ?", game_id)
    if (!nrow(g) || g$status != "lobby") return(FALSE)
    mine <- qry(con, "SELECT 1 FROM positions WHERE game_id=? AND player_id=?", game_id, player$id)
    if (!nrow(mine)) stop("Pick a role first.")
    begin_game(con, game_id, get_session_id(con, g$session_id))
    TRUE
  })
}

# Latest team game this player holds a role in (lobby, active or done).
team_game_id <- function(con, player_id) {
  g <- qry(con, "SELECT g.id FROM games g JOIN positions p ON p.game_id = g.id
                 WHERE g.mode = 'team' AND p.player_id = ? ORDER BY g.id DESC LIMIT 1", player_id)
  if (nrow(g)) g$id else NA_integer_
}

list_teams <- function(con, session_id) {
  qry(con, "SELECT g.id, g.team_name, g.status, g.week,
              SUM(CASE WHEN p.player_id IS NULL THEN 0 ELSE 1 END) AS members
            FROM games g JOIN positions p ON p.game_id = g.id
            WHERE g.session_id = ? AND g.mode = 'team' GROUP BY g.id ORDER BY g.id", session_id)
}

# Everything one screen needs about a game, in one query.
game_view <- function(con, game_id) {
  r <- qry(con, "SELECT g.session_id, g.mode, g.team_name, g.status, g.week,
                   p.*, pl.name AS player_name
                 FROM positions p JOIN games g ON g.id = p.game_id
                 LEFT JOIN players pl ON pl.id = p.player_id
                 WHERE p.game_id = ? ORDER BY p.role", game_id)
  if (!nrow(r)) return(NULL)
  gcols <- c("session_id", "mode", "team_name", "status", "week")
  list(game = cbind(id = r$game_id[1], r[1, gcols]), pos = r[, setdiff(names(r), gcols)])
}

# Cheap fingerprint a browser can poll to learn "something changed".
game_stamp <- function(con, game_id) {
  r <- qry(con, "SELECT g.status || ':' || g.week || ':' ||
                   (SELECT COUNT(pending_order) FROM positions WHERE game_id = g.id) || ':' ||
                   (SELECT COALESCE(GROUP_CONCAT(COALESCE(player_id, 0)), '') FROM positions WHERE game_id = g.id) AS s
                 FROM games g WHERE g.id = ?", game_id)
  if (nrow(r)) r$s else "gone"
}

# One query per heartbeat: everything this player could be waiting on.
player_stamp <- function(con, player, tab, in_lobby = FALSE) {
  if (tab == "solo") {
    return(qry(con, "SELECT COALESCE(MAX(g.id || ':' || g.status || ':' || g.week), '-') AS s
                     FROM games g JOIN positions p ON p.game_id = g.id
                     WHERE g.mode = 'solo' AND p.player_id = ?", player$id)$s)
  }
  r <- qry(con, "SELECT s.team_open AS open,
                   COALESCE((SELECT MAX(g.id) FROM games g JOIN positions p ON p.game_id = g.id
                             WHERE g.mode = 'team' AND p.player_id = ?), 0) AS gid
                 FROM sessions s WHERE s.id = ?", player$id, player$session_id)
  out <- paste(r$open, r$gid, if (r$gid > 0) game_stamp(con, r$gid) else "")
  if (in_lobby) out <- paste(out, qry(con,
    "SELECT COALESCE(GROUP_CONCAT(g.id || '.' || p.role || '.' || COALESCE(p.player_id, 0)), '') AS s
     FROM games g JOIN positions p ON p.game_id = g.id
     WHERE g.session_id = ? AND g.mode = 'team' AND g.status = 'lobby'", player$session_id)$s)
  out
}

game_history <- function(con, game_id) {
  qry(con, "SELECT * FROM history WHERE game_id = ? ORDER BY week, role", game_id)
}

# Close the week if every human has ordered. Call inside a transaction.
advance_if_ready <- function(con, game_id) {
  g <- qry(con, "SELECT * FROM games WHERE id = ?", game_id)
  pos <- read_positions(con, game_id)
  human <- !is.na(pos$player_id)
  if (g$status != "active" || any(human & is.na(pos$pending_order))) return(FALSE)
  sess <- session_rules(con, g$session_id)
  demand <- parse_demand(sess$demand_seq, sess$weeks)
  orders <- ghost_orders(pos, sess$init_stock, demand[1])
  orders[human] <- pos$pending_order[human]
  res <- close_week(pos[, c("role", STATE_COLS)], orders, g$week,
                    sess$hold_cost, sess$backlog_cost, !human)
  h <- res$history
  h$player_id <- pos$player_id
  dbExecute(con, "INSERT INTO history (game_id, week, role, is_ghost, player_id, demand, received,
            shipped, stock, backlog, order_qty, cost) VALUES (?,?,?,?,?,?,?,?,?,?,?,?)",
    params = list(rep(game_id, nrow(h)), h$week, h$role, h$is_ghost, h$player_id, h$demand,
                  h$received, h$shipped, h$stock, h$backlog, h$order_qty, h$cost))
  if (g$week >= sess$weeks) {
    write_positions(con, game_id, res$pos)
    exe(con, "UPDATE games SET status='done', finished_at=? WHERE id=?", now_utc(), game_id)
  } else {
    write_positions(con, game_id, start_week(res$pos, demand[g$week + 1L]))
    exe(con, "UPDATE games SET week = week + 1 WHERE id = ?", game_id)
  }
  TRUE
}

# Returns "ok", "stale" (week already moved on / already ordered) or "no".
submit_order <- function(con, game_id, player_id, week, qty) {
  qty <- suppressWarnings(as.integer(round(as.numeric(qty))))
  if (is.na(qty) || qty < 0 || qty > 9999) return("no")
  in_txn(con, {
    g <- qry(con, "SELECT status, week FROM games WHERE id = ?", game_id)
    if (!nrow(g) || g$status != "active" || g$week != week) "stale" else {
      n <- exe(con, "UPDATE positions SET pending_order = ? WHERE game_id = ? AND player_id = ?
                     AND pending_order IS NULL", qty, game_id, player_id)
      if (n == 0) "stale" else { advance_if_ready(con, game_id); "ok" }
    }
  })
}

# ---- instructor ------------------------------------------------------------

# Hand roles that have not ordered this week to the computer (teammate left).
ghost_takeover <- function(con, game_id) {
  in_txn(con, {
    exe(con, "UPDATE positions SET player_id = NULL WHERE game_id = ? AND pending_order IS NULL
              AND game_id IN (SELECT id FROM games WHERE status = 'active')", game_id)
    advance_if_ready(con, game_id)
  })
}

delete_game <- function(con, game_id) {
  in_txn(con, {
    exe(con, "DELETE FROM history WHERE game_id = ?", game_id)
    exe(con, "DELETE FROM positions WHERE game_id = ?", game_id)
    exe(con, "DELETE FROM games WHERE id = ?", game_id)
  })
}

solo_progress <- function(con, session_id) {
  qry(con, "SELECT pl.id AS player_id, pl.name, g.id AS game_id, g.status, g.week,
              p.total_cost, g.finished_at
            FROM players pl
            LEFT JOIN positions p ON p.player_id = pl.id
                 AND p.game_id IN (SELECT id FROM games WHERE mode = 'solo')
            LEFT JOIN games g ON g.id = p.game_id
            WHERE pl.session_id = ? ORDER BY pl.name_key", session_id)
}

team_progress <- function(con, session_id) {
  qry(con, "SELECT g.id AS game_id, g.team_name, g.status, g.week,
              SUM(p.total_cost) AS total_cost,
              GROUP_CONCAT(CASE WHEN pl.name IS NULL THEN NULL ELSE pl.name END, ', ') AS members,
              SUM(CASE WHEN p.player_id IS NOT NULL AND p.pending_order IS NULL THEN 1 ELSE 0 END) AS waiting_on
            FROM games g JOIN positions p ON p.game_id = g.id
            LEFT JOIN players pl ON pl.id = p.player_id
            WHERE g.session_id = ? AND g.mode = 'team' GROUP BY g.id ORDER BY g.id", session_id)
}

export_history <- function(con, session_id) {
  h <- qry(con, "SELECT s.code AS class_code, g.mode AS round, g.id AS game_id,
                   COALESCE(g.team_name, '') AS team, h.week, h.role,
                   CASE WHEN h.is_ghost = 1 THEN 'Computer' ELSE pl.name END AS player,
                   h.is_ghost AS computer_run, h.demand AS order_received, h.received AS units_received,
                   h.shipped AS units_shipped, h.stock, h.backlog, h.order_qty AS order_placed, h.cost
                 FROM history h JOIN games g ON g.id = h.game_id
                 JOIN sessions s ON s.id = g.session_id
                 LEFT JOIN players pl ON pl.id = h.player_id
                 WHERE g.session_id = ? ORDER BY g.mode, g.id, h.week, h.role", session_id)
  h$round <- ifelse(h$round == "solo", "1-individual", "2-team")
  h$role_name <- ROLE_NAMES[h$role]
  h[, c("class_code", "round", "game_id", "team", "week", "role", "role_name", "player",
        "computer_run", "order_received", "units_received", "units_shipped", "stock",
        "backlog", "order_placed", "cost")]
}

# One row per human player per round, with totals and bullwhip.
export_results <- function(con, session_id) {
  sess <- get_session_id(con, session_id)
  demand <- parse_demand(sess$demand_seq, sess$weeks)
  rows <- qry(con, "SELECT g.id AS game_id, g.mode, COALESCE(g.team_name, '') AS team, g.status,
                      g.week, g.started_at, g.finished_at, p.role, pl.name AS player, p.total_cost
                    FROM games g JOIN positions p ON p.game_id = g.id
                    JOIN players pl ON pl.id = p.player_id
                    WHERE g.session_id = ? ORDER BY g.mode, g.id, p.role", session_id)
  if (!nrow(rows)) return(data.frame())
  hist <- qry(con, "SELECT h.game_id, h.week, h.role, h.order_qty, h.cost, h.backlog FROM history h
                    JOIN games g ON g.id = h.game_id WHERE g.session_id = ?", session_id)
  chain <- tapply(hist$cost, hist$game_id, sum)
  rows$weeks_played <- vapply(seq_len(nrow(rows)), function(i)
    sum(hist$game_id == rows$game_id[i] & hist$role == rows$role[i]), integer(1))
  rows$bullwhip_ratio <- vapply(seq_len(nrow(rows)), function(i) {
    h <- hist[hist$game_id == rows$game_id[i], ]
    if (length(unique(h$week)) < sess$weeks) NA_real_ else bullwhip(h, demand)[rows$role[i]]
  }, numeric(1))
  rows$weeks_with_backlog <- vapply(seq_len(nrow(rows)), function(i)
    sum(hist$backlog[hist$game_id == rows$game_id[i] & hist$role == rows$role[i]] > 0), integer(1))
  rows$chain_total_cost <- as.numeric(chain[as.character(rows$game_id)])
  rows$chain_total_cost[is.na(rows$chain_total_cost)] <- 0
  # the number the prize is judged on: own cost in round 1, whole chain in round 2
  rows$score <- ifelse(rows$mode == "solo", rows$total_cost, rows$chain_total_cost)
  rows$rank <- NA_integer_
  for (m in unique(rows$mode)) {
    done <- rows$mode == m & rows$status == "done"
    rows$rank[done] <- rank(rows$score[done], ties.method = "min")
  }
  data.frame(class_code = sess$code,
             round = ifelse(rows$mode == "solo", "1-individual", "2-team"),
             team = rows$team, player = rows$player, role = ROLE_NAMES[rows$role],
             status = rows$status, weeks_played = rows$weeks_played,
             own_cost = rows$total_cost, chain_total_cost = rows$chain_total_cost,
             score = rows$score, rank = rows$rank, bullwhip_ratio = rows$bullwhip_ratio,
             weeks_with_backlog = rows$weeks_with_backlog,
             started_utc = rows$started_at, finished_utc = rows$finished_at,
             stringsAsFactors = FALSE)
}
