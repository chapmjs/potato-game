# charts.R -- results charts, drawn with base R graphics.
# Base graphics draw in a few milliseconds, which matters when 40 students
# reach the results screen in the same minute on one R process.
# Role colours are fixed to the role, never to rank, and were checked for
# colour-blind separation. Every chart also has a table beside it in the app.

ROLE_COLORS <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100")  # roles 1..4
INK <- "#2b2622"; INK_SOFT <- "#6f665e"; GRID <- "#e9e4dc"

week_ticks <- function(max_week, narrow = FALSE) {
  by <- if (max_week > 30) 5 else if (narrow) 4 else 2
  b <- seq(by, max_week, by = by)
  if (by == 4) c(1, b) else b
}

# Push end-of-line labels apart so they never sit on top of each other.
spread_labels <- function(y, gap) {
  o <- order(y); ys <- y[o]
  for (i in seq_along(ys)[-1]) if (ys[i] - ys[i - 1] < gap) ys[i] <- ys[i - 1] + gap
  y[o] <- ys
  y
}

chart_frame <- function(xlim, ylim, xticks, ylab, right_margin, legend_rows, title = NULL) {
  top <- 1.0 + 1.25 * legend_rows + if (is.null(title)) 0 else 1.8
  par(mar = c(3.6, 3.8, top, right_margin), bg = "white", fg = INK_SOFT, col.axis = INK_SOFT,
      col.lab = INK_SOFT, cex = 1, las = 1, mgp = c(2.5, 0.6, 0), tcl = 0, xaxs = "i", yaxs = "i")
  plot.new(); plot.window(xlim, ylim)
  yt <- pretty(ylim, 5); yt <- yt[yt >= ylim[1] & yt <= ylim[2]]
  abline(h = yt, col = GRID, lwd = 1)
  axis(1, at = xticks, lwd = 0); axis(2, at = yt, labels = abs(yt), lwd = 0)
  title(xlab = "Week", line = 2.2); title(ylab = ylab, line = 2.6)
  if (!is.null(title)) mtext(title, side = 3, line = top - 1.5, adj = 0, font = 2, col = INK, cex = 1.1)
  invisible(yt)
}

# y position (user units) of a legend row counted up from the plot's top edge
legend_y <- function(row) {
  usr <- par("usr"); per_line <- (usr[4] - usr[3]) / par("pin")[2] * par("csi")
  usr[4] + per_line * (0.2 + 1.25 * row)
}

# Orders placed by each role, week by week, against what shoppers bought.
# `highlight` (a role number) thickens that role's line.
orders_chart <- function(history, demand_seq, highlight = NULL, title = NULL, narrow = FALSE) {
  weeks <- sort(unique(history$week)); W <- max(weeks)
  ymax <- max(c(history$order_qty, demand_seq[weeks]), 1) * 1.08
  chart_frame(c(0.5, W + 0.5), c(0, ymax), week_ticks(W, narrow), "Units ordered",
              right_margin = 6.5, legend_rows = if (narrow) 3 else 1, title = title)
  lines(c(rbind(weeks - 0.5, weeks + 0.5)), rep(demand_seq[weeks], each = 2), col = INK, lwd = 1.6, lty = "22")
  ends <- numeric(N_ROLES)
  for (r in 1:N_ROLES) {
    h <- history[history$role == r, ]; h <- h[order(h$week), ]
    lines(h$week, h$order_qty, col = ROLE_COLORS[r], lend = "round", ljoin = "round",
          lwd = if (is.null(highlight)) 2.2 else if (r == highlight) 4 else 1.8)
    ends[r] <- h$order_qty[nrow(h)]
  }
  text(W + 0.7, spread_labels(ends, ymax * 0.075), ROLE_SHORT, adj = 0, col = INK, xpd = NA, cex = 0.92)
  x0 <- par("usr")[1]
  lg <- function(row, ...) legend(x0, legend_y(row), ..., yjust = 1, bty = "n", xpd = NA,
                                  text.col = INK, cex = 0.95, seg.len = 1.4, x.intersp = 0.6)
  if (narrow) {
    lg(3, ROLE_SHORT, col = ROLE_COLORS, lwd = 2.5, ncol = 2)
    lg(1, "Shopper demand", col = INK, lwd = 1.6, lty = "22")
  } else {
    lg(1, c(ROLE_SHORT, "Shopper demand"), col = c(ROLE_COLORS, INK), lwd = c(rep(2.5, 4), 1.6),
       lty = c(rep("solid", 4), "22"), horiz = TRUE)
  }
  invisible()
}

# One role's stock (above zero) and backlog (below zero) each week.
stock_chart <- function(history, role, title = NULL, narrow = FALSE) {
  h <- history[history$role == role, ]; W <- max(h$week)
  lo <- -max(h$backlog, 0) * 1.08; hi <- max(h$stock, 1) * 1.08
  chart_frame(c(0.5, W + 0.5), c(lo, hi), week_ticks(W, narrow), "Units",
              right_margin = 1, legend_rows = if (narrow) 2 else 1, title = title)
  cols <- c("#2a78d6", "#eb6834")
  rect(h$week - 0.36, 0, h$week + 0.36, h$stock, col = cols[1], border = "white")
  rect(h$week - 0.36, -h$backlog, h$week + 0.36, 0, col = cols[2], border = "white")
  abline(h = 0, col = INK_SOFT)
  legend(par("usr")[1], legend_y(if (narrow) 2 else 1), yjust = 1,
         c("Stock (costs to hold)", "Backlog (unfilled orders)"), fill = cols, border = NA,
         horiz = !narrow, bty = "n", xpd = NA, text.col = INK, cex = 0.95, x.intersp = 0.6)
  invisible()
}
