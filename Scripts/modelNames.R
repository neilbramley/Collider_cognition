#=========================================================
#  Model names to go in the optimisation script
# =========================================================

# ----------- Models list --------- might obviate operatives

full <- list(
  name = "full",
  n_params = 4,
  n_BIC = 4,
  probdist = "posterior",
  base = "cesm",
  Actual = "Actual",
  transform = function(pars) {
    list(
      tau1 = exp(pars[1]),
      epsilon = plogis(pars[2]),
      tau2 = exp(pars[3]),
      kappa = exp(pars[4])
    )
  }
)

noAct <- list(
  name = "noAct",
  n_params = 4,
  n_BIC = 4,
  probdist = "posterior",
  base = "cesm",
  Actual = NULL,
  transform = function(pars) {
    list(
      tau1 = exp(pars[1]),
      epsilon = plogis(pars[2]),
      tau2 = exp(pars[3]),
      kappa = exp(pars[4])
    )
  }
)

noSelect <- list(
  name = "noSelect",
  n_params = 4,
  n_BIC = 3,
  probdist = "posterior",
  base = "noSelect",
  Actual = "Actual",
  transform = function(pars) {
    list(
      tau1 = exp(pars[1]),
      epsilon = plogis(pars[2]),
      tau2 = exp(pars[3]),
      kappa = exp(pars[4])
    )
  }
)

noActnoSelect <- list(
  name = "noActnoSelect",
  n_params = 4,
  n_BIC = 3,
  probdist = "posterior",
  base = "noSelect",
  Actual = NULL,
  transform = function(pars) {
    list(
      tau1 = exp(pars[1]),
      epsilon = plogis(pars[2]),
      tau2 = exp(pars[3]),
      kappa = exp(pars[4])
    )
  }
)

noKind <- list(
  name = "noKind",
  n_params = 3,
  n_BIC = 3,
  probdist = "posterior",
  base = "cesm",
  Actual = "Actual",
  transform = function(pars) {
    list(
      tau1 = exp(pars[1]),
      epsilon = plogis(pars[2]),
      tau2 = exp(pars[3]),
      kappa = NULL
    )
  }
)

noActnoKind <- list(
  name = "noActnoKind",
  n_params = 3,
  n_BIC = 3,
  probdist = "posterior",
  base = "cesm",
  Actual = NULL,
  transform = function(pars) {
    list(
      tau1 = exp(pars[1]),
      epsilon = plogis(pars[2]),
      tau2 = exp(pars[3]),
      kappa = NULL
    )
  }
)

noInfnoKind <- list(
  name = "noInfnoKind",
  n_params = 3,
  n_BIC = 3,
  probdist = "PrUn",
  base = "cesm",
  Actual = "Actual",
  transform = function(pars) {
    list(
      tau1 = exp(pars[1]),
      epsilon = plogis(pars[2]),
      tau2 = exp(pars[3]),
      kappa = NULL
    )
  }
)

noKindnoSelect <- list(
  name = "noKindnoSelect",
  n_params = 3,
  n_BIC = 2,
  probdist = "posterior",
  base = "noSelect",
  Actual = "Actual",
  transform = function(pars) {
    list(
      tau1 = exp(pars[1]),
      epsilon = plogis(pars[2]),
      tau2 = exp(pars[3]),
      kappa = NULL
    )
  }
)

noActnoInfnoKind <- list(
  name = "noActnoInfnoKind",
  n_params = 3,
  n_BIC = 3,
  probdist = "PrUn",
  base = "cesm",
  Actual = NULL,
  transform = function(pars) {
    list(
      tau1 = exp(pars[1]),
      epsilon = plogis(pars[2]),
      tau2 = exp(pars[3]),
      kappa = NULL
    )
  }
)

noActnoKindnoSelect <- list(
  name = "noActnoKindnoSelect",
  n_params = 3,
  n_BIC = 2,
  probdist = "posterior",
  base = "noSelect",
  Actual = NULL,
  transform = function(pars) {
    list(
      tau1 = exp(pars[1]),
      epsilon = plogis(pars[2]),
      tau2 = exp(pars[3]),
      kappa = NULL
    )
  }
)

noInfnoKindnoSelect <- list(
  name = "noInfnoKindnoSelect",
  n_params = 3,
  n_BIC = 2,
  probdist = "PrUn",
  base = "noSelect",
  Actual = "Actual",
  transform = function(pars) {
    list(
      tau1 = exp(pars[1]),
      epsilon = plogis(pars[2]),
      tau2 = exp(pars[3]),
      kappa = NULL
    )
  }
)

noActnoInfnoKindnoSelect <- list(
  name = "noActnoInfnoKindnoSelect",
  n_params = 3,
  n_BIC = 2,
  probdist = "PrUn",
  base = "noSelect",
  Actual = NULL,
  transform = function(pars) {
    list(
      tau1 = exp(pars[1]),
      epsilon = plogis(pars[2]),
      tau2 = exp(pars[3]),
      kappa = NULL
    )
  }
)
