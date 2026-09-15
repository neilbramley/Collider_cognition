##################################################################
######## noSelect at FIXED parameters (no optimisation) ##########
##################################################################

library(tidyverse)
library(here)
source(here('Scripts', 'modelNames.R'))
source(here('Scripts', 'optimUtils4par.R'))

load(here('Data', 'modelData', 'goOptim.rda')) # loads mp and df

# Just for the plot for noSelect at tau1 = 1, to discuss impact of noSelect

# ---------------------------------------------------------------
# 1. Build the pars vector
# ---------------------------------------------------------------
# get_prediction() expects pars on the UNCONSTRAINED scale, because
# noSelect$transform() applies exp() to pars[1], pars[3], pars[4] and
# plogis() to pars[2]. This helper takes values on the natural scale
# and inverts those links, so you can think in tau1 / epsilon / tau2 / kappa.

make_pars <- function(tau1, epsilon, tau2, kappa) {
  c(log(tau1), qlogis(epsilon), log(tau2), log(kappa))
}

# Check the round trip before relying on it:
noSelect$transform(make_pars(tau1 = 1, epsilon = 0.1, tau2 = 1, kappa = 0.5))

# ---------------------------------------------------------------
# 2. Predictions at one fixed parameter set
# ---------------------------------------------------------------

pars_fixed <- make_pars(tau1 = 1, epsilon = 0.1, tau2 = 1, kappa = 0.5)

preds <- get_prediction(
  pars = pars_fixed,
  mp = mp,
  df = df,
  model = noSelect
)

# ---------------------------------------------------------------
# 3. Attach participant proportions
# ---------------------------------------------------------------
# df$n holds the participant counts per (trial_id, node3). The observed
# proportion is n divided by the total count within that trial, so the
# denominator is the number of participants who answered that trial.

obs <- df |>
  group_by(trial_id) |>
  mutate(obs_prop = n / sum(n)) |>
  ungroup() |>
  select(trial_id, node3, n, obs_prop)

# split() names the list elements with as.character(trial_id), so
# preds$trial_id comes back as character. Coerce before joining.
preds <- preds |>
  mutate(trial_id = as.character(trial_id))

obs <- obs |>
  mutate(trial_id = as.character(trial_id))

fitfixed <- left_join(preds, obs, by = c("trial_id", "node3"))

save(fitfixed, file = here('Data', 'modelData', 'fitfixed.rda'))

# ---------------------------------------------------------------
# 4. Plot predicted against observed
# ---------------------------------------------------------------

r_pearson <- cor(
  fitfixed$predicted_prob,
  fitfixed$obs_prop,
  use = "complete.obs"
)

rmse <- sqrt(mean(
  (fitfixed$predicted_prob - fitfixed$obs_prop)^2,
  na.rm = TRUE
))

p <- ggplot(fitfixed, aes(x = obs_prop, y = predicted_prob)) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    colour = "grey50"
  ) +
  geom_point(alpha = 0.6) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(
    x = "Participant proportion",
    y = "Predicted probability",
    title = "noSelect at tau1 = 1, epsilon = 0.1, tau2 = 1, kappa = 0.5",
    subtitle = sprintf("r = %.3f, RMSE = %.3f", r_pearson, rmse)
  ) +
  theme_minimal()

print(p)

# ---------------------------------------------------------------
# 5. Sweep tau1 while holding the other three parameters fixed
# ---------------------------------------------------------------
# Use this when you want to see how sensitive the predictions are to tau1,
# rather than committing to a single value.

tau1_grid <- c(0.25, 0.5, 1, 2, 4)

sweep <- map_dfr(tau1_grid, function(t1) {
  get_prediction(
    pars = make_pars(tau1 = t1, epsilon = 0.1, tau2 = 1, kappa = 0.5),
    mp = mp,
    df = df,
    model = noSelect
  ) |>
    mutate(tau1 = t1, trial_id = as.character(trial_id))
})

sweep <- left_join(sweep, obs, by = c("trial_id", "node3"))

p_sweep <- ggplot(sweep, aes(x = obs_prop, y = predicted_prob)) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    colour = "grey50"
  ) +
  geom_point(alpha = 0.5) +
  facet_wrap(~tau1, labeller = label_both) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(x = "Participant proportion", y = "Predicted probability") +
  theme_minimal()

print(p_sweep)

# ---------------------------------------------------------------
# 6. Optional: fix tau1 = 1 but still fit epsilon, tau2, kappa
# ---------------------------------------------------------------
# This is a different exercise from sections 2-5. Here optim() searches over
# three parameters while tau1 stays pinned at 1, so the fit is the best the
# model can do under that constraint, rather than the fit at values you chose.

noSelect_tau1fixed <- noSelect
noSelect_tau1fixed$name <- "noSelect_tau1fixed"
noSelect_tau1fixed$n_params <- 3L
noSelect_tau1fixed$transform <- function(pars) {
  list(
    tau1 = 1, # pinned, not read from pars
    epsilon = plogis(pars[1]),
    tau2 = exp(pars[2]),
    kappa = exp(pars[3])
  )
}

fit_constrained <- optim(
  par = rep(0, 3),
  fn = function(par) {
    get_likelihood(par, mp = mp, df = df, model = noSelect_tau1fixed)
  },
  method = "Nelder-Mead",
  control = list(maxit = 1000)
)

noSelect_tau1fixed$transform(fit_constrained$par) # fitted values
-fit_constrained$value # log likelihood

preds_constrained <- get_prediction(
  pars = fit_constrained$par,
  mp = mp,
  df = df,
  model = noSelect_tau1fixed
)
