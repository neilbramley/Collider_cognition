#=========================================================
#  Optimisation functions
# =========================================================

# Static functions used by the optimisation script

# -----------
# Later 4 par version - the first softmax is on S so all the inferenece and marginalisation is now inside likelihood
# ------------

# A small function to strip the trailing 0s for presentation. Needs as.character as input. Likely to use elsewhere?
trim_zeros <- function(x) sub("^(-?)0\\.", ".", sprintf("%s", as.numeric(x)))

models <- list(
  full = full,
  noAct = noAct,
  noSelect = noSelect,
  noActnoSelect = noActnoSelect,
  noKind = noKind,
  noActnoKind = noActnoKind,
  noInfnoKind = noInfnoKind,
  noKindnoSelect = noKindnoSelect,
  noActnoInfnoKind = noActnoInfnoKind,
  noActnoKindnoSelect = noActnoKindnoSelect,
  noInfnoKindnoSelect = noInfnoKindnoSelect,
  noActnoInfnoKindnoSelect = noActnoInfnoKindnoSelect
)

model_names <- names(models)

prepare_trials <- function(pars, mp, df, model, kind_type) {
  # transform parameters
  params <- model$transform(pars)

  # softmax over base (tau1)
  col <- model$base
  mp1 <- mp |>
    group_by(pgroup, trialtype, uAuB) |>
    mutate(
      logits = .data[[col]] / params$tau1,
      soft1 = exp(logits) / sum(exp(logits))
    ) |>
    ungroup()

  # aggregate using probdist column
  pred <- mp1 |>
    group_by(pgroup, trialtype, node3, .drop = FALSE) |>
    summarise(
      pred = sum(soft1 * .data[[model$probdist]]),
      .groups = "drop"
    )

  # merge with df
  newdf <- merge(df, pred, by = c("pgroup", "trialtype", "node3"))

  # add kappa * ig BEFORE Actual
  if (!is.null(params$kappa)) {
    newdf$pred <- newdf$pred + params$kappa * newdf[[kind_type]]
  }

  # apply Actual (if specified as a column name in df)
  if (!is.null(model$Actual)) {
    newdf$pred <- newdf$pred * newdf$Actual
  }

  newdf$pred[newdf$pred == 0] <- -Inf

  # split by trial
  trials <- split(newdf, newdf$trial_id)

  list(trials = trials, params = params)
}


# .data is dplyr pronoun for current data being process, to match the string in base

# ==========================
# MATHS CORE OF LIKELIHOOD AND PREDICTION
# ==========================

compute_mpred <- function(tr, params) {
  # tr$pred is the per-option latent value for this trial
  base_logits <- tr$pred

  logits <- base_logits / params$tau2
  soft <- exp(logits) / sum(exp(logits))

  # epsilon mixing
  params$epsilon * (1 / length(soft)) + (1 - params$epsilon) * soft
}


# =======================================
# LIKELIHOOD FUNCTION
# =======================================

get_likelihood <- function(pars, mp, df, model, kind_type) {
  prep <- prepare_trials(pars, mp, df, model, kind_type)
  trials <- prep$trials
  params <- prep$params

  nlls <- vapply(
    trials,
    function(tr) {
      # Skip trials with zero total choices
      if (sum(tr$n) == 0) {
        return(0)
      }
      mpred <- compute_mpred(tr, params)
      -sum(log(mpred) * tr$n)
    },
    numeric(1)
  )

  sum(nlls)
}


# =======================================
# PREDICTION FUNCTION - follows same format as likelihood
# =======================================

get_prediction <- function(pars, mp, df, model, kind_type) {
  prep <- prepare_trials(pars, mp, df, model, kind_type)
  trials <- prep$trials
  params <- prep$params

  preds <- lapply(
    names(trials),
    function(tr_id) {
      tr <- trials[[tr_id]]
      # Skip trials with zero total choices
      if (sum(tr$n) == 0) {
        return(data.frame(
          model = model$name,
          trial_id = tr_id,
          node3 = tr$node3,
          predicted_prob = NA_real_
        ))
      }
      mpred <- compute_mpred(tr, params)

      data.frame(
        model = model$name,
        trial_id = tr_id,
        node3 = tr$node3,
        predicted_prob = mpred
      )
    }
  )

  do.call(rbind, preds)
}


# =======================================
#  MODEL FIT
# =======================================

get_optimisation <- function(
  model_names,
  mp,
  df,
  kind_type,
  initial_values = NULL
) {
  # A. Optimize each model
  fits <- lapply(model_names, function(mname) {
    model <- models[[mname]]
    if (is.null(initial_values)) {
      init <- rep(1, model$n_params)
    } else {
      stopifnot(length(initial_values) == model$n_params)
      init <- initial_values
    }

    tryCatch(
      optim(
        par = init,
        fn = function(par) get_likelihood(par, mp = mp, df = df, model = model, kind_type = kind_type),
        method = "Nelder-Mead",
        control = list(maxit = 1000)
      ),
      error = function(e) {
        message("Error in optimization for model ", mname, ": ", e$message)
        list(par = rep(NA, model$n_params), value = NA)
      }
    )
  })
  names(fits) <- model_names

  # B. Build model_fits table
  get_par <- function(idx, link) {
    sapply(fits, function(x) {
      if (is.null(x$par[idx]) || is.na(x$par[idx])) {
        return(NA_real_)
      }
      if (link == "exp") {
        return(exp(x$par[idx]))
      }
      if (link == "plogis") {
        return(plogis(x$par[idx]))
      }
      x$par[idx]
    })
  }

  tau1 <- get_par(1, "exp")
  epsilon <- get_par(2, "plogis")
  tau2 <- get_par(3, "exp")
  logl <- -sapply(fits, `[[`, "value")

  model_fits <- data.frame(
    model = model_names,
    tau1 = tau1,
    epsilon = epsilon,
    tau2 = tau2,
    logl = logl,
    stringsAsFactors = FALSE
  )

  # Handle kappa model‑specifically
  n_par_vec <- sapply(model_names, function(mname) models[[mname]]$n_params)
  if (any(n_par_vec == 4L)) {
    kappa <- sapply(fits, function(x) {
      if (length(x$par) < 4 || is.na(x$par[4])) {
        return(NA_real_)
      }
      exp(x$par[4])
    })
    model_fits$kappa <- kappa
  }

  # For BIC, use per‑model n_par
  N <- sum(df$n)
  model_fits$BIC <- -2 * model_fits$logl + sapply(models, '[[', 'n_BIC') * log(N)

  # Format for printing; keep numeric version in attributes if needed
  model_fits_fmt <- model_fits |>
    dplyr::mutate(
      tau1 = format(tau1, digits = 3),
      epsilon = format(epsilon, digits = 3),
      tau2 = format(tau2, digits = 3),
      logl = format(logl, digits = 4),
      BIC = format(BIC, digits = 4),
      kappa = if ("kappa" %in% names(model_fits)) {
        format(kappa, digits = 3)
      } else {
        NA
      }
    )

  # C. Predictions table
  predictions <- do.call(
    rbind,
    lapply(model_names, function(mname) {
      model <- models[[mname]]
      fit <- fits[[mname]]
      pars <- fit$par
      if (!any(is.na(pars))) {
        get_prediction(
          pars = pars,
          mp = mp,
          df = df,
          model = model,
          kind_type
        )
      }
    })
  )

  list(
    model_fits = model_fits_fmt,
    predictions = predictions
  )
}
