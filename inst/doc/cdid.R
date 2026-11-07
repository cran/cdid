## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  error = FALSE
)

## ----simulate-panel-----------------------------------------------------------
library(cdid)
set.seed(20261029)

n_units <- 900
n_periods <- 5

units <- data.frame(
  id = seq_len(n_units),
  x = rbinom(n_units, size = 1, prob = 0.5)
)
units$g <- ifelse(
  runif(n_units) < plogis(-0.2 + 0.4 * units$x),
  3,
  0
)
unit_alpha <- rnorm(n_units)
window_start <- sample.int(n_periods - 1L, size = n_units, replace = TRUE)

panel_data <- data.frame(
  id = rep(units$id, each = n_periods),
  time = rep(seq_len(n_periods), times = n_units),
  g = rep(units$g, each = n_periods),
  x = rep(units$x, each = n_periods)
)

treated_post <- panel_data$g == 3 & panel_data$time >= 3
true_effect <- ifelse(treated_post, 1 + 0.25 * (panel_data$time - 3), 0)
y_latent <- rep(unit_alpha, each = n_periods) +
  (0.4 + 0.2 * panel_data$x) * panel_data$time +
  true_effect +
  rnorm(nrow(panel_data), sd = 0.5)

in_window <- panel_data$time == rep(window_start, each = n_periods) |
  panel_data$time == rep(window_start + 1L, each = n_periods)
panel_data$y <- ifelse(in_window, y_latent, NA_real_)

## ----check-structure----------------------------------------------------------
stopifnot(!anyDuplicated(panel_data[c("id", "time")]))
obs_per_unit <- tabulate(
  match(panel_data$id[!is.na(panel_data$y)], units$id),
  nbins = nrow(units)
)
table(cohort = units$g, observed_dates = obs_per_unit)

## ----fit-att-gt---------------------------------------------------------------
fit <- cdid::att_gt(
  yname = "y",
  tname = "time",
  idname = "id",
  gname = "g",
  xformla = ~ x,
  data = panel_data,
  frame = units,
  control_group = "nevertreated",
  est_method = "dr",
  mode = "chained",
  selection = "none",
  pt_assumption = "post",
  bstrap = FALSE,
  cband = FALSE
)
summary(fit)

## ----inspect-tidy-glance------------------------------------------------------
cells <- cdid::tidy(fit)
cells[, c("target.id", "role", "estimate", "std.error",
          "conf.low", "conf.high", "available",
          "support.status", "support.reason")]
cdid::glance(fit)

## ----aggregate-and-plot, fig.width = 6, fig.height = 3.5----------------------
agg_simple <- cdid::aggte(fit, type = "simple")
agg_dynamic <- cdid::aggte(fit, type = "dynamic")

summary(agg_simple)
cdid::tidy(agg_dynamic)[, c("label", "estimate",
                            "std.error", "conf.low", "conf.high",
                            "available", "reason")]
cdid::ggdid(agg_dynamic)

