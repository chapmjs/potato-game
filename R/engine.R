# engine.R -- Potato Game rules. Pure functions, no database, no Shiny.
#
# The chain has four positions. Role 1 sells to shoppers; role 4 makes the
# product. Potatoes flow 4 -> 3 -> 2 -> 1; orders flow 1 -> 2 -> 3 -> 4.
#
# Timing (one row of `pos` per role):
#   * Anything shipped in week t reaches the buyer's stock at the start of t+2.
#   * An order placed in week t is seen by the supplier in week t+1.
#   * The Processor's "order" is a production run: started in week t, finished
#     goods reach its stock at the start of week t+2 (the two production steps).
#
# Each week:  start_week()  -> receive, see demand, ship what you can
#             (players and ghosts decide how much to order)
#             close_week()  -> charge costs, record history, lodge the orders

ROLE_NAMES  <- c("Grocery Store", "Wholesaler", "Distributor", "Potato Processor")
ROLE_SHORT  <- c("Grocery", "Wholesaler", "Distributor", "Processor")
ROLE_BUYS_FROM  <- c("Wholesaler", "Distributor", "Potato Processor", "your own production line")
ROLE_SELLS_TO   <- c("shoppers", "the Grocery Store", "the Wholesaler", "the Distributor")
N_ROLES <- 4L

# Ghost (computer) ordering: the anchoring-and-adjustment rule Sterman (1989)
# fitted to real beer-game players. Ghosts under-weight what is already on
# order, so they over-react the way people do.
GHOST <- list(theta = 0.36,  # how fast the demand forecast follows new orders
              alpha = 0.26,  # share of the stock gap corrected each week
              beta  = 0.34)  # weight given to potatoes already on the way

parse_demand <- function(txt, weeks) {
  x <- suppressWarnings(as.integer(strsplit(gsub("\\s", "", txt), ",")[[1]]))
  x <- x[!is.na(x) & x >= 0]
  if (!length(x)) x <- 4L
  if (length(x) < weeks) x <- c(x, rep(x[length(x)], weeks - length(x)))
  x[seq_len(weeks)]
}

default_demand <- function(weeks = 20L) {
  paste(c(rep(4L, min(4L, weeks)), rep(8L, max(0L, weeks - 4L))), collapse = ",")
}

new_positions <- function(init_stock = 12L, base_demand = 4L) {
  data.frame(
    role       = 1:N_ROLES,
    stock      = as.integer(init_stock),
    backlog    = 0L,
    pipe1      = as.integer(base_demand),  # arrives next week
    pipe2      = as.integer(base_demand),  # arrives the week after
    demand     = 0L,                       # order received this week
    received   = 0L,
    shipped    = 0L,
    last_order = as.integer(base_demand),  # order placed last week
    forecast   = as.numeric(base_demand),
    total_cost = 0,
    stringsAsFactors = FALSE
  )
}

# Opening moves of a week: receive, read the incoming order, ship.
start_week <- function(pos, customer_demand) {
  pos <- pos[order(pos$role), ]
  pos$received <- pos$pipe1
  pos$stock    <- pos$stock + pos$pipe1
  pos$pipe1    <- pos$pipe2
  pos$pipe2    <- 0L

  pos$demand   <- as.integer(c(customer_demand, pos$last_order[1:(N_ROLES - 1L)]))
  owed         <- pos$demand + pos$backlog
  pos$shipped  <- pmin(pos$stock, owed)
  pos$stock    <- pos$stock - pos$shipped
  pos$backlog  <- owed - pos$shipped

  # what role i ships lands in role i-1's pipeline
  pos$pipe2[1:(N_ROLES - 1L)] <- pos$shipped[2:N_ROLES]

  pos$forecast <- GHOST$theta * pos$demand + (1 - GHOST$theta) * pos$forecast
  pos
}

week_cost <- function(pos, hold_cost, backlog_cost) {
  pos$stock * hold_cost + pos$backlog * backlog_cost
}

# What a ghost would order this week (call after start_week).
ghost_orders <- function(pos, init_stock = 12L, base_demand = 4L) {
  supplier_backlog <- c(pos$backlog[2:N_ROLES], 0L)       # owed to me, not yet shipped
  supply_line <- pos$pipe1 + pos$pipe2 + supplier_backlog
  target <- init_stock + GHOST$beta * 2 * base_demand
  gap <- target - (pos$stock - pos$backlog) - GHOST$beta * supply_line
  as.integer(pmax(0, round(pos$forecast + GHOST$alpha * gap)))
}

# Closing moves: charge costs, write the history rows, lodge orders.
# Returns list(pos = <state ready for next start_week>, history = <4 rows>).
close_week <- function(pos, orders, week, hold_cost, backlog_cost, is_ghost) {
  orders <- as.integer(pmax(0L, orders))
  cost <- week_cost(pos, hold_cost, backlog_cost)
  pos$total_cost <- pos$total_cost + cost
  hist <- data.frame(
    week = as.integer(week), role = pos$role, is_ghost = as.integer(is_ghost),
    demand = pos$demand, received = pos$received, shipped = pos$shipped,
    stock = pos$stock, backlog = pos$backlog, order_qty = orders,
    cost = cost, stringsAsFactors = FALSE
  )
  pos$last_order <- orders
  pos$pipe2[N_ROLES] <- pos$pipe2[N_ROLES] + orders[N_ROLES]  # production run starts
  list(pos = pos, history = hist)
}

# Bullwhip ratio per role: variance of orders placed / variance of shopper
# demand. 1 = orders as steady as shoppers; bigger = amplified swings.
bullwhip <- function(history, demand_seq) {
  v0 <- stats::var(demand_seq)
  out <- vapply(1:N_ROLES, function(r) {
    o <- history$order_qty[history$role == r]
    if (length(o) < 2 || is.na(v0) || v0 == 0) return(NA_real_)
    stats::var(o) / v0
  }, numeric(1))
  round(out, 2)
}

# Play a whole game with a policy function (used by tests and benchmarks).
simulate_game <- function(weeks = 20L, demand = parse_demand(default_demand(weeks), weeks),
                          human_role = NULL, human_policy = NULL,
                          init_stock = 12L, hold_cost = 5, backlog_cost = 25) {
  pos <- new_positions(init_stock, demand[1])
  hist <- list()
  for (w in seq_len(weeks)) {
    pos <- start_week(pos, demand[w])
    ord <- ghost_orders(pos, init_stock, demand[1])
    ghost <- rep(TRUE, N_ROLES)
    if (!is.null(human_role)) {
      ord[human_role] <- human_policy(pos[human_role, ], w)
      ghost[human_role] <- FALSE
    }
    res <- close_week(pos, ord, w, hold_cost, backlog_cost, ghost)
    pos <- res$pos
    hist[[w]] <- res$history
  }
  list(pos = pos, history = do.call(rbind, hist))
}
