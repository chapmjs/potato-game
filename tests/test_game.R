# Run from the app folder:  Rscript tests/test_game.R
# Checks the rules and the database layer against a throwaway database.
source("R/engine.R"); source("R/db.R")
ok <- function(label, cond) { stopifnot(isTRUE(cond)); cat("ok  ", label, "\n") }

# --- rules ---
g <- simulate_game(demand = rep(4L, 20))
ok("flat demand keeps every role at stock 12, order 4",
   all(g$history$order_qty == 4) && all(g$history$stock == 12))
ok("flat demand costs $60 a week each", all(g$history$cost == 60))

g <- simulate_game()
h <- g$history
ok("no negative stock or backlog", all(h$stock >= 0) && all(h$backlog >= 0))
ok("never ship more than is owed", all(h$shipped <= h$demand + c(rep(0, 4), head(h$backlog, -4))))
# potatoes are conserved: everything the processor made is in stock, in transit, or sold
made <- sum(h$order_qty[h$role == 4])
sold <- sum(h$shipped[h$role == 1])
on_hand <- sum(g$pos$stock) + sum(g$pos$pipe1) + sum(g$pos$pipe2)
ok("units conserved across the chain", 4 * 12 + 4 * 8 + made == sold + on_hand)
bw <- bullwhip(h, parse_demand(default_demand(), 20))
ok("ghost chain shows bullwhip growing upstream", all(diff(bw) > 0) && bw[4] > 5)
ok("each role's order received = downstream order a week earlier",
   all(h$demand[h$role == 3 & h$week > 1] == h$order_qty[h$role == 2 & h$week < 20]))

# --- database: individual round ---
f <- tempfile(fileext = ".sqlite"); con <- db_connect(f)
s <- create_session(con, "test1", "Test")
ok("class code is upper-cased", s$code == "TEST1")
p <- join_player(con, s$id, "  Spud   McKenzie ")
ok("same name returns the same player", join_player(con, s$id, "spud mckenzie")$id == p$id)
gid <- solo_game_id(con, p)
ok("second call resumes the same game", solo_game_id(con, p) == gid)
v <- game_view(con, gid)
ok("week 1 opens with stock 12", v$game$week == 1 && v$pos$stock[4] == 12)
ok("order for the wrong week is refused", submit_order(con, gid, p$id, 2, 4) == "stale")
ok("negative order is refused", submit_order(con, gid, p$id, 1, -3) == "no")
ok("order accepted", submit_order(con, gid, p$id, 1, 4) == "ok")
ok("double click does not order twice", submit_order(con, gid, p$id, 1, 4) == "stale")
for (w in 2:20) submit_order(con, gid, p$id, w, game_view(con, gid)$pos$demand[4])
v <- game_view(con, gid)
ref <- simulate_game(human_role = 4, human_policy = function(p, w) if (w == 1) 4 else p$demand)
ok("game finishes after week 20", v$game$status == "done" && nrow(game_history(con, gid)) == 80)
ok("database game matches the engine", all(v$pos$total_cost == ref$pos$total_cost))
ok("no orders after the end", submit_order(con, gid, p$id, 21, 4) == "stale")

# --- database: team round ---
ps <- lapply(c("Ann A", "Bo B", "Cy C"), function(n) join_player(con, s$id, n))
tid <- create_team(con, ps[[1]], "Tater Tots")
ok("duplicate team name refused", inherits(try(create_team(con, ps[[2]], "tater tots"), silent = TRUE), "try-error"))
claim_role(con, ps[[1]], tid, 1); claim_role(con, ps[[2]], tid, 2)
ok("taken role refused", inherits(try(claim_role(con, ps[[3]], tid, 2), silent = TRUE), "try-error"))
claim_role(con, ps[[3]], tid, 4)
claim_role(con, ps[[3]], tid, 3)                      # change of mind
ok("switching roles frees the old one", is.na(game_view(con, tid)$pos$player_id[4]))
start_team(con, ps[[1]], tid)
ok("late joiner refused once started", inherits(try(claim_role(con, p, tid, 4), silent = TRUE), "try-error"))
submit_order(con, tid, ps[[1]]$id, 1, 4); submit_order(con, tid, ps[[2]]$id, 1, 4)
ok("week waits for the last teammate", game_view(con, tid)$game$week == 1)
submit_order(con, tid, ps[[3]]$id, 1, 4)
ok("week turns when everyone has ordered", game_view(con, tid)$game$week == 2)
submit_order(con, tid, ps[[1]]$id, 2, 4)
ghost_takeover(con, tid)
v <- game_view(con, tid)
ok("computer takes over idle roles", v$game$week == 3 && sum(!is.na(v$pos$player_id)) == 1)
for (w in 3:20) submit_order(con, tid, ps[[1]]$id, w, 4)
ok("team game finishes", game_view(con, tid)$game$status == "done")

r <- export_results(con, s$id); hx <- export_history(con, s$id)
ok("results export has a row per human role", nrow(r) == 2 && all(!is.na(r$rank)))
ok("history export has every week and role", nrow(hx) == 160)
ok("team score is the whole chain", r$score[r$round == "2-team"] == sum(game_view(con, tid)$pos$total_cost))
dbDisconnect(con); unlink(f)
cat("\nAll checks passed.\n")
