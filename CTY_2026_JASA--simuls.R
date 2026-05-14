################################################################################################
# Estimation and Inference in Boundary Discontinuity Designs: Location-Based Methods
# Simulation Study
################################################################################################

rm(list = ls(all = TRUE))

# This script calibrates the SA-10.2 fuzzy bivariate Monte Carlo design from
# the SPP data, writes DGP targets and metadata, runs the simulations, and saves
# raw replication-level results. Final LaTeX tables are built by
# CTY_2026_JASA--simuls_tables.R.

get_script_dir <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) > 0) {
    return(normalizePath(dirname(sub("^--file=", "", file_arg[1])), winslash = "/"))
  }
  normalizePath(getwd(), winslash = "/")
}

script_dir <- get_script_dir()
setwd(script_dir)

library(rd2d)

output_dir <- Sys.getenv("RD2D_OUTPUT_DIR", unset = "output")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

bw_specs <- data.frame(
  bwparam = c("main", "itt"),
  suffix = c("bwmain", "bwitt"),
  stringsAsFactors = FALSE
)

estimand_specs <- data.frame(
  design = c("fuzzy", "fuzzy", "fuzzy", "sharp", "sharp"),
  output = c("main", "itt", "fs", "main", "main.0"),
  estimand = c("fuzzy", "itt", "fs", "sharp", "sharp0"),
  stringsAsFactors = FALSE
)
fuzzy_estimand_specs <- estimand_specs[estimand_specs$design == "fuzzy", ]
sharp_estimand_specs <- estimand_specs[estimand_specs$design == "sharp", ]

get_env_int <- function(name, default) {
  value <- Sys.getenv(name, unset = NA_character_)
  if (is.na(value) || identical(value, "")) return(default)

  parsed <- suppressWarnings(as.integer(value))
  if (is.na(parsed) || parsed <= 0) {
    stop(sprintf("%s must be a positive integer.", name), call. = FALSE)
  }
  parsed
}

rd2d_package_version <- function() {
  desc_path <- system.file("DESCRIPTION", package = "rd2d")
  if (!nzchar(desc_path)) {
    stop("Could not find installed rd2d DESCRIPTION file.", call. = FALSE)
  }
  as.character(read.dcf(desc_path, fields = "Version")[1, 1])
}

check_rd2d_package <- function() {
  version <- rd2d_package_version()
  if (utils::compareVersion(version, "0.1.0") < 0) {
    stop(
      paste(
        "The replication scripts require rd2d version 0.1.0 or newer.",
        sprintf("Installed rd2d version: %s.", version),
        "Install or update rd2d from CRAN before running this script."
      ),
      call. = FALSE
    )
  }

  missing_args <- setdiff(c("params.other", "params.cov", "bwparam"), names(formals(rd2d::rd2d)))
  if (length(missing_args) > 0) {
    stop(
      paste(
        "The installed rd2d package does not match the replication API.",
        "Install or update rd2d from CRAN before running this script.",
        "Missing rd2d() argument(s):",
        paste(missing_args, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  cat(sprintf("Using rd2d version: %s\n", version))
}

clean_output_dir <- function() {
  old_files <- list.files(output_dir, pattern = "^simuls_.*\\.csv$", full.names = TRUE)
  if (length(old_files) > 0) unlink(old_files)
}

write_output_csv <- function(x, file) {
  write.csv(x, file.path(output_dir, file), row.names = FALSE, na = "")
}

simulation_seed <- function(seed_base, replication, design, dgp) {
  design_offset <- if (identical(design, "sharp")) 500000L else 0L
  seed_base + replication * 1000L + design_offset + dgp
}

################################## Data ########################################

load_spp_data <- function(path = "spp.csv") {
  raw <- read.csv(path)
  expected <- c(
    "running_saber11", "running_sisben", "eligible_spp",
    "beneficiary_spp", "spadies_any"
  )
  missing_cols <- setdiff(expected, names(raw))
  if (length(missing_cols) > 0) {
    stop(sprintf("spp.csv is missing columns: %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
  }

  dat <- raw[, expected]
  names(dat) <- c("x.1", "x.2", "assignment", "fuzzy", "Y")
  dat <- dat[complete.cases(dat), ]

  expected_assignment <- as.integer(dat$x.1 >= 0 & dat$x.2 >= 0)
  if (!all(dat$assignment == expected_assignment)) {
    stop("eligible_spp does not match 1{x.1 >= 0 & x.2 >= 0}.", call. = FALSE)
  }

  dat$assignment <- as.numeric(dat$assignment)
  dat$fuzzy <- as.numeric(dat$fuzzy)
  dat$Y <- as.numeric(dat$Y)
  dat
}

make_eval_grid <- function() {
  # Start from the 40-point empirical grid and keep points 11--31. This gives
  # the 21-point simulation grid with the kink at the internal point b11.
  neval <- 40
  half <- ceiling(neval / 2)
  grid <- rbind(
    data.frame(
      x.1 = rep(0, half),
      x.2 = 40 - (seq_len(half) - 1) * 40 / half
    ),
    data.frame(
      x.1 = (seq_len(neval - half) - 1) * 56 / half,
      x.2 = rep(0, neval - half)
    )
  )
  grid[11:31, , drop = FALSE]
}

################################## DGP #########################################

design_matrix <- function(dat, s) {
  x1 <- dat$x.1
  x2 <- dat$x.2

  if (s == 1) {
    return(cbind("(Intercept)" = 1, "x.1" = x1, "x.2" = x2))
  }
  if (s == 2) {
    return(cbind(
      "(Intercept)" = 1,
      "x.1" = x1,
      "x.2" = x2,
      "I(x.1^2)" = x1^2,
      "I(x.1 * x.2)" = x1 * x2,
      "I(x.2^2)" = x2^2
    ))
  }
  if (s == 3) {
    return(cbind(
      "(Intercept)" = 1,
      "x.1" = x1,
      "x.2" = x2,
      "I(x.1^2)" = x1^2,
      "I(x.1 * x.2)" = x1 * x2,
      "I(x.2^2)" = x2^2,
      "I(x.1^3)" = x1^3,
      "I(x.1^2 * x.2)" = x1^2 * x2,
      "I(x.1 * x.2^2)" = x1 * x2^2,
      "I(x.2^3)" = x2^3
    ))
  }

  stop("Only s = 1, s = 2, and s = 3 are supported.", call. = FALSE)
}

calibrate_dgp <- function(dat, s, dgp, design = c("fuzzy", "sharp")) {
  design <- match.arg(design)
  Xs <- design_matrix(dat, s)
  out <- list(design = design, dgp = dgp, s = s)

  for (side in 0:1) {
    ind <- dat$assignment == side
    X_side <- Xs[ind, , drop = FALSE]

    y_fit <- lm.fit(X_side, dat$Y[ind])
    beta_y <- as.numeric(y_fit$coefficients)
    names(beta_y) <- colnames(X_side)

    fuzzy_fit <- suppressWarnings(glm.fit(X_side, dat$fuzzy[ind], family = binomial()))
    beta_fuzzy <- as.numeric(fuzzy_fit$coefficients)
    names(beta_fuzzy) <- colnames(X_side)

    if (any(!is.finite(beta_y)) || any(!is.finite(beta_fuzzy))) {
      stop(sprintf("Non-finite calibration coefficient for s=%d, assignment=%d.", s, side), call. = FALSE)
    }

    mu_y <- as.numeric(X_side %*% beta_y)
    mu_fuzzy <- plogis(as.numeric(X_side %*% beta_fuzzy))
    eps_y <- dat$Y[ind] - mu_y
    eps_fuzzy <- dat$fuzzy[ind] - mu_fuzzy

    denom <- sum(eps_fuzzy^2)
    if (!is.finite(denom) || denom <= 0) {
      stop(sprintf("Degenerate first-stage residual variance for s=%d, assignment=%d.", s, side), call. = FALSE)
    }

    lambda <- if (identical(design, "sharp")) 0 else sum(eps_y * eps_fuzzy) / denom
    sigma2 <- mean((eps_y - lambda * eps_fuzzy)^2)
    if (!is.finite(sigma2) || sigma2 <= 0) {
      stop(sprintf("Degenerate outcome residual variance for s=%d, assignment=%d.", s, side), call. = FALSE)
    }

    out[[paste0("beta_y_", side)]] <- beta_y
    out[[paste0("beta_fuzzy_", side)]] <- beta_fuzzy
    out[[paste0("lambda_", side)]] <- lambda
    out[[paste0("sigma_y_", side)]] <- sqrt(sigma2)
  }

  out
}

make_dgp_parameter_table <- function(calibrations) {
  rows <- lapply(calibrations, function(cal) {
    coef_rows <- do.call(rbind, lapply(0:1, function(side) {
      do.call(rbind, lapply(c("beta_y", "beta_fuzzy"), function(component) {
        values <- cal[[paste0(component, "_", side)]]
        data.frame(
          design = cal$design,
          dgp = cal$dgp,
          s = cal$s,
          side = side,
          component = component,
          term = names(values),
          value = as.numeric(values),
          stringsAsFactors = FALSE
        )
      }))
    }))

    shock_rows <- do.call(rbind, lapply(0:1, function(side) {
      data.frame(
        design = cal$design,
        dgp = cal$dgp,
        s = cal$s,
        side = side,
        component = c("lambda", "sigma_y"),
        term = "",
        value = c(cal[[paste0("lambda_", side)]], cal[[paste0("sigma_y_", side)]]),
        stringsAsFactors = FALSE
      )
    }))

    rbind(coef_rows, shock_rows)
  })

  do.call(rbind, rows)
}

make_targets <- function(eval, calibrations) {
  out <- lapply(calibrations, function(cal) {
    R <- design_matrix(eval, cal$s)
    mu_y0 <- as.numeric(R %*% cal$beta_y_0)
    mu_y1 <- as.numeric(R %*% cal$beta_y_1)
    mu_fuzzy0 <- plogis(as.numeric(R %*% cal$beta_fuzzy_0))
    mu_fuzzy1 <- plogis(as.numeric(R %*% cal$beta_fuzzy_1))

    point_targets <- data.frame(
      dgp = cal$dgp,
      s = cal$s,
      row_type = "point",
      index = seq_len(nrow(eval)),
      aggregate = "",
      b1 = eval$x.1,
      b2 = eval$x.2,
      tau_itt = mu_y1 - mu_y0,
      tau_fs = mu_fuzzy1 - mu_fuzzy0,
      zeta = (mu_y1 - mu_y0) / (mu_fuzzy1 - mu_fuzzy0),
      sharp = mu_y1 - mu_y0,
      sharp0 = mu_y0,
      stringsAsFactors = FALSE
    )

    specs <- estimand_specs[estimand_specs$design == cal$design, ]
    target_map <- list(
      fuzzy = point_targets$zeta,
      itt = point_targets$tau_itt,
      fs = point_targets$tau_fs,
      sharp = point_targets$sharp,
      sharp0 = point_targets$sharp0
    )

    point_long <- do.call(rbind, lapply(specs$estimand, function(estimand) {
      data.frame(
        design = cal$design,
        point_targets[, c("dgp", "s", "row_type", "index", "aggregate", "b1", "b2")],
        estimand = estimand,
        target = target_map[[estimand]],
        stringsAsFactors = FALSE
      )
    }))

    aggregate_long <- do.call(rbind, lapply(split(point_long, point_long$estimand), function(z) {
      data.frame(
        dgp = z$dgp[1],
        design = z$design[1],
        s = z$s[1],
        row_type = "aggregate",
        index = NA_integer_,
        aggregate = c("WBATE", "LBATE"),
        b1 = NA_real_,
        b2 = NA_real_,
        estimand = z$estimand[1],
        target = c(mean(z$target), max(z$target)),
        stringsAsFactors = FALSE
      )
    }))

    rbind(point_long, aggregate_long)
  })

  do.call(rbind, out)
}

################################ Simulation ####################################

summary_table <- function(fit, output) {
  point_table <- fit[[output]]
  invisible(utils::capture.output(
    summ <- summary(
      fit,
      output = output,
      cbands = output,
      WBATE = rep(1, nrow(point_table)),
      LBATE = TRUE
    )
  ))
  summ$tables[[output]]
}

table_col <- function(table, name) {
  if (name %in% names(table)) table[[name]] else rep(NA_real_, nrow(table))
}

estimand_name <- function(output, design) {
  if (identical(design, "fuzzy") && identical(output, "main")) return("fuzzy")
  if (identical(design, "sharp") && identical(output, "main")) return("sharp")
  if (identical(design, "sharp") && identical(output, "main.0")) return("sharp0")
  output
}

make_output_rows <- function(fit, output, design, rep_id, dgp_id, dgp_s,
                             bwparam) {
  table <- summary_table(fit, output)
  point_rows <- !(rownames(table) %in% c("WBATE", "LBATE"))
  points <- table[point_rows, , drop = FALSE]
  aggregates <- table[!point_rows, , drop = FALSE]
  estimand <- estimand_name(output, design)

  point_out <- data.frame(
    replication = rep_id,
    design = design,
    dgp = dgp_id,
    s = dgp_s,
    bwparam = bwparam,
    estimand = estimand,
    row_type = "point",
    index = seq_len(nrow(points)),
    aggregate = "",
    b1 = table_col(points, "b1"),
    b2 = table_col(points, "b2"),
    h01 = table_col(points, "h01"),
    h02 = table_col(points, "h02"),
    h11 = table_col(points, "h11"),
    h12 = table_col(points, "h12"),
    N.Co = table_col(points, "N.Co"),
    N.Tr = table_col(points, "N.Tr"),
    estimate.p = table_col(points, "estimate.p"),
    std.err.p = table_col(points, "std.err.p"),
    estimate.q = table_col(points, "estimate.q"),
    std.err.q = table_col(points, "std.err.q"),
    t.value = table_col(points, "t.value"),
    p.value = table_col(points, "p.value"),
    ci.lower = table_col(points, "ci.lower"),
    ci.upper = table_col(points, "ci.upper"),
    cb.lower = table_col(points, "cb.lower"),
    cb.upper = table_col(points, "cb.upper"),
    stringsAsFactors = FALSE
  )

  aggregate_out <- data.frame(
    replication = rep_id,
    design = design,
    dgp = dgp_id,
    s = dgp_s,
    bwparam = bwparam,
    estimand = estimand,
    row_type = "aggregate",
    index = NA_integer_,
    aggregate = rownames(aggregates),
    b1 = NA_real_,
    b2 = NA_real_,
    h01 = NA_real_,
    h02 = NA_real_,
    h11 = NA_real_,
    h12 = NA_real_,
    N.Co = NA_real_,
    N.Tr = NA_real_,
    estimate.p = table_col(aggregates, "estimate.p"),
    std.err.p = NA_real_,
    estimate.q = table_col(aggregates, "estimate.q"),
    std.err.q = table_col(aggregates, "std.err.q"),
    t.value = table_col(aggregates, "t.value"),
    p.value = table_col(aggregates, "p.value"),
    ci.lower = table_col(aggregates, "ci.lower"),
    ci.upper = table_col(aggregates, "ci.upper"),
    cb.lower = NA_real_,
    cb.upper = NA_real_,
    stringsAsFactors = FALSE
  )

  rbind(point_out, aggregate_out)
}

draw_scores <- function(n) {
  data.frame(
    x.1 = 100 * rbeta(n, 3, 4) - 25,
    x.2 = 100 * rbeta(n, 3, 4) - 25
  )
}

assignment_rule <- function(X) {
  as.numeric(X$x.1 >= 0 & X$x.2 >= 0)
}

simulate_sample <- function(cal, n) {
  X <- draw_scores(n)
  assignment <- assignment_rule(X)
  R <- design_matrix(X, cal$s)

  mu_y0 <- as.numeric(R %*% cal$beta_y_0)
  mu_y1 <- as.numeric(R %*% cal$beta_y_1)

  if (identical(cal$design, "fuzzy")) {
    mu_fuzzy0 <- plogis(as.numeric(R %*% cal$beta_fuzzy_0))
    mu_fuzzy1 <- plogis(as.numeric(R %*% cal$beta_fuzzy_1))
    fuzzy0 <- as.numeric(runif(n) <= mu_fuzzy0)
    fuzzy1 <- as.numeric(runif(n) <= mu_fuzzy1)
    Y0 <- mu_y0 + cal$lambda_0 * (fuzzy0 - mu_fuzzy0) + rnorm(n, sd = cal$sigma_y_0)
    Y1 <- mu_y1 + cal$lambda_1 * (fuzzy1 - mu_fuzzy1) + rnorm(n, sd = cal$sigma_y_1)
    fuzzy <- ifelse(assignment == 1, fuzzy1, fuzzy0)
  } else {
    Y0 <- mu_y0 + rnorm(n, sd = cal$sigma_y_0)
    Y1 <- mu_y1 + rnorm(n, sd = cal$sigma_y_1)
    fuzzy <- NULL
  }

  list(
    X = X,
    assignment = assignment,
    Y = ifelse(assignment == 1, Y1, Y0),
    fuzzy = fuzzy
  )
}

raw_result_file <- function(replication, dgp, design, s, suffix) {
  file.path(
    output_dir,
    sprintf("simuls_raw_rep%04d_dgp%02d_%s_s%d_%s.csv", replication, dgp, design, s, suffix)
  )
}

write_raw_result <- function(rows, replication, cal, suffix) {
  write.csv(
    rows,
    file = raw_result_file(replication, cal$dgp, cal$design, cal$s, suffix),
    row.names = FALSE,
    na = ""
  )
}

run_fuzzy_calibration <- function(j, cal, sample, eval, repp, bw_specs) {
  for (i in seq_len(nrow(bw_specs))) {
    bwparam <- bw_specs$bwparam[i]
    suffix <- bw_specs$suffix[i]

    fit <- rd2d::rd2d(
      Y = sample$Y,
      X = sample$X,
      assignment = sample$assignment,
      b = eval,
      fuzzy = sample$fuzzy,
      bwparam = bwparam,
      stdvars = FALSE,
      masspoints = "off",
      vce = "hc1",
      repp = repp,
      params.cov = c("main", "itt", "fs")
    )

    rows <- rbind(
      make_output_rows(fit, "itt", cal$design, j, cal$dgp, cal$s, bwparam),
      make_output_rows(fit, "fs", cal$design, j, cal$dgp, cal$s, bwparam),
      make_output_rows(fit, "main", cal$design, j, cal$dgp, cal$s, bwparam)
    )
    write_raw_result(rows, j, cal, suffix)
  }
}

run_sharp_calibration <- function(j, cal, sample, eval, repp) {
  bwparam <- "main"
  suffix <- "bwmain"

  fit <- rd2d::rd2d(
    Y = sample$Y,
    X = sample$X,
    assignment = sample$assignment,
    b = eval,
    stdvars = FALSE,
    masspoints = "off",
    vce = "hc1",
    repp = repp,
    params.other = "main.0",
    params.cov = c("main", "main.0")
  )

  rows <- rbind(
    make_output_rows(fit, "main", cal$design, j, cal$dgp, cal$s, bwparam),
    make_output_rows(fit, "main.0", cal$design, j, cal$dgp, cal$s, bwparam)
  )
  write_raw_result(rows, j, cal, suffix)
}

simulate_replication <- function(j, fuzzy_calibrations, sharp_calibrations,
                                 eval, n, repp, seed_base, bw_specs) {
  for (cal in fuzzy_calibrations) {
    set.seed(simulation_seed(seed_base, j, cal$design, cal$dgp))
    sample <- simulate_sample(cal, n)
    run_fuzzy_calibration(j, cal, sample, eval, repp, bw_specs)
  }

  for (cal in sharp_calibrations) {
    set.seed(simulation_seed(seed_base, j, cal$design, cal$dgp))
    sample <- simulate_sample(cal, n)
    run_sharp_calibration(j, cal, sample, eval, repp)
  }

  TRUE
}

run_simulations <- function(m, num_workers) {
  if (num_workers <= 1) {
    message("Running sequentially.")
    invisible(lapply(seq_len(m), function(j) {
      simulate_replication(
        j = j,
        fuzzy_calibrations = fuzzy_calibrations,
        sharp_calibrations = sharp_calibrations,
        eval = eval,
        n = n,
        repp = repp,
        seed_base = seed_base,
        bw_specs = bw_specs
      )
    }))
    return(invisible(TRUE))
  }

  cl <- parallel::makeCluster(num_workers)
  on.exit(parallel::stopCluster(cl), add = TRUE)

  parallel::clusterExport(cl, "script_dir", envir = globalenv())
  parallel::clusterEvalQ(cl, {
    setwd(script_dir)
    library(rd2d)
    NULL
  })

  message(sprintf("Running in parallel with %d worker(s).", num_workers))

  parallel::clusterExport(
    cl,
    c(
      "fuzzy_calibrations", "sharp_calibrations", "eval", "n", "repp",
      "seed_base", "output_dir", "bw_specs",
      "simulation_seed", "design_matrix", "draw_scores", "assignment_rule",
      "simulate_sample", "summary_table", "table_col", "estimand_name",
      "make_output_rows", "raw_result_file", "write_raw_result",
      "run_fuzzy_calibration", "run_sharp_calibration", "simulate_replication"
    ),
    envir = environment()
  )

  parallel::parLapplyLB(cl, seq_len(m), function(j) {
    simulate_replication(
      j = j,
      fuzzy_calibrations = fuzzy_calibrations,
      sharp_calibrations = sharp_calibrations,
      eval = eval,
      n = n,
      repp = repp,
      seed_base = seed_base,
      bw_specs = bw_specs
    )
  })

  invisible(TRUE)
}

################################### Run ########################################

check_rd2d_package()
set.seed(3)
clean_output_dir()

spp <- load_spp_data("spp.csv")
eval <- make_eval_grid()
fuzzy_calibrations <- list(
  calibrate_dgp(spp, s = 1, dgp = 1, design = "fuzzy"),
  calibrate_dgp(spp, s = 2, dgp = 2, design = "fuzzy"),
  calibrate_dgp(spp, s = 3, dgp = 3, design = "fuzzy")
)
sharp_calibrations <- list(
  calibrate_dgp(spp, s = 1, dgp = 1, design = "sharp"),
  calibrate_dgp(spp, s = 2, dgp = 2, design = "sharp"),
  calibrate_dgp(spp, s = 3, dgp = 3, design = "sharp")
)
calibrations <- c(fuzzy_calibrations, sharp_calibrations)

m <- get_env_int("RD2D_M", 5000)
n <- get_env_int("RD2D_N", 10000)
repp <- get_env_int("RD2D_REPP", 2000)
seed_base <- get_env_int("RD2D_SEED", 20260510)

num_cores <- parallel::detectCores()
if (is.na(num_cores)) num_cores <- 2
default_workers <- max(1, num_cores - 4)
num_workers <- min(m, get_env_int("RD2D_WORKERS", default_workers))

write_output_csv(make_dgp_parameter_table(calibrations), "simuls_dgp_parameters.csv")
write_output_csv(make_targets(eval, calibrations), "simuls_dgp_targets.csv")
write_output_csv(estimand_specs, "simuls_estimands.csv")
write_output_csv(
  data.frame(
    name = c(
      "rd2d.version", "m", "n", "repp", "seed_base", "workers",
      "bwparams", "generated_at"
    ),
    value = c(
      rd2d_package_version(), m, n, repp, seed_base, num_workers,
      paste(paste(bw_specs$bwparam, bw_specs$suffix, sep = "="), collapse = "; "),
      as.character(Sys.time())
    ),
    stringsAsFactors = FALSE
  ),
  "simuls_metadata.csv"
)

timing <- system.time(run_simulations(m, num_workers))
write_output_csv(
  data.frame(
    metric = names(timing),
    seconds = as.numeric(timing),
    stringsAsFactors = FALSE
  ),
  "simuls_runtime.csv"
)

expected_raw <- c(unlist(lapply(fuzzy_calibrations, function(cal) {
  unlist(lapply(bw_specs$suffix, function(suffix) {
    basename(raw_result_file(seq_len(m), cal$dgp, cal$design, cal$s, suffix))
  }), use.names = FALSE)
}), use.names = FALSE), unlist(lapply(sharp_calibrations, function(cal) {
  basename(raw_result_file(seq_len(m), cal$dgp, cal$design, cal$s, "bwmain"))
}), use.names = FALSE))
missing_raw <- expected_raw[!file.exists(file.path(output_dir, expected_raw))]
if (length(missing_raw) > 0) {
  stop(sprintf("Missing expected raw simulation file: %s", missing_raw[1]), call. = FALSE)
}

cat("Simulation output complete.\n")
cat(sprintf("Raw simulation files written to: %s\n", output_dir))
cat(sprintf("Generated %d raw CSV file(s).\n", length(expected_raw)))
