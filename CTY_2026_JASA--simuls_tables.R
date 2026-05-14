################################################################################################
# Estimation and Inference in Boundary Discontinuity Designs: Location-Based Methods
# Simulation Study Tables
################################################################################################

rm(list = ls(all = TRUE))

# This script reads raw simulation CSV files produced by
# CTY_2026_JASA--simuls.R and writes the final LaTeX tables.

get_script_dir <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) > 0) {
    return(normalizePath(dirname(sub("^--file=", "", file_arg[1])), winslash = "/"))
  }
  normalizePath(getwd(), winslash = "/")
}

setwd(get_script_dir())

output_dir <- Sys.getenv("RD2D_OUTPUT_DIR", unset = "output")
tables_dir <- Sys.getenv("RD2D_TABLES_DIR", unset = "tables")
dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)

estimand_specs <- data.frame(
  design = c("fuzzy", "fuzzy", "fuzzy", "sharp", "sharp"),
  output = c("main", "itt", "fs", "main", "main.0"),
  estimand = c("fuzzy", "itt", "fs", "sharp", "sharp0"),
  stringsAsFactors = FALSE
)
estimands <- estimand_specs$estimand
estimand_labels <- c(
  fuzzy = "Fuzzy",
  itt = "Intention-to-treat",
  fs = "First stage",
  sharp = "Sharp",
  sharp0 = "Sharp control side"
)

bw_specs <- data.frame(
  bwparam = c("main", "itt"),
  suffix = c("bwmain", "bwitt"),
  stringsAsFactors = FALSE
)

core_raw_cols <- c(
  "replication", "design", "dgp", "s", "bwparam", "estimand", "row_type",
  "index", "aggregate", "b1", "b2", "estimate.p", "std.err.p",
  "estimate.q", "std.err.q", "t.value", "p.value", "ci.lower", "ci.upper",
  "cb.lower", "cb.upper"
)
effective_raw_cols <- c("N.Co", "N.Tr")
bw_cols <- c("h", "h01", "h02", "h11", "h12")

################################## Input #######################################

clean_tables_dir <- function() {
  old_files <- list.files(tables_dir, pattern = "^simuls_.*\\.tex$", full.names = TRUE)
  if (length(old_files) > 0) unlink(old_files)
}

read_output_csv <- function(file) {
  path <- file.path(output_dir, file)
  if (!file.exists(path)) {
    stop(sprintf("Missing output file: %s. Run CTY_2026_JASA--simuls.R first.", path), call. = FALSE)
  }
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

metadata_value <- function(metadata, name) {
  value <- metadata$value[metadata$name == name]
  if (length(value) != 1) {
    stop(sprintf("Simulation metadata is missing value '%s'.", name), call. = FALSE)
  }
  value
}

read_raw_file <- function(file) {
  dat <- read.csv(file, stringsAsFactors = FALSE, check.names = FALSE)
  missing_cols <- setdiff(core_raw_cols, names(dat))
  if (length(missing_cols) > 0) {
    stop(
      sprintf("File %s is missing columns: %s", file, paste(missing_cols, collapse = ", ")),
      call. = FALSE
    )
  }

  bw_available <- intersect(bw_cols, names(dat))
  if (length(bw_available) == 0) {
    stop(sprintf("File %s is missing bandwidth columns.", file), call. = FALSE)
  }

  missing_effective <- setdiff(effective_raw_cols, names(dat))
  if (length(missing_effective) > 0) {
    dat[missing_effective] <- NA_real_
  }

  dat[, c(core_raw_cols, effective_raw_cols, bw_available)]
}

read_simulation_rows <- function(m, dgp, design, s, suffix, specs) {
  files <- file.path(
    output_dir,
    sprintf("simuls_raw_rep%04d_dgp%02d_%s_s%d_%s.csv", seq_len(m), dgp, design, s, suffix)
  )
  missing_files <- files[!file.exists(files)]
  if (length(missing_files) > 0) {
    stop(sprintf("Missing simulation file: %s", missing_files[1]), call. = FALSE)
  }

  rows <- do.call(rbind, lapply(files, read_raw_file))
  expected_rows <- nrow(specs) * (21 + 2)
  rows_by_file <- table(rows$replication)
  if (any(rows_by_file != expected_rows)) {
    bad <- names(rows_by_file)[rows_by_file != expected_rows][1]
    stop(sprintf("Replication %s has %d rows; expected %d.", bad, rows_by_file[[bad]], expected_rows), call. = FALSE)
  }
  if (!all(rows$design == design)) stop(sprintf("File %s contains the wrong design.", files[1]), call. = FALSE)
  if (!setequal(unique(rows$estimand), specs$estimand)) stop(sprintf("File %s contains the wrong estimands.", files[1]), call. = FALSE)

  rows
}

################################ Summaries #####################################

fmt_num <- function(x, digits = 3) {
  ifelse(is.na(x) | !is.finite(x), "", sprintf(paste0("%.", digits, "f"), x))
}

bandwidth_summary <- function(z) {
  if (all(c("h01", "h02", "h11", "h12") %in% names(z))) {
    h0 <- colMeans(z[, c("h01", "h02"), drop = FALSE], na.rm = TRUE)
    h1 <- colMeans(z[, c("h11", "h12"), drop = FALSE], na.rm = TRUE)
    if (all(is.finite(c(h0, h1))) && isTRUE(all.equal(h0, h1))) {
      return(c(h1 = h0[1], h2 = h0[2]))
    }
    if (all(is.finite(c(h0, h1)))) {
      return(c(h1 = mean(c(h0[1], h1[1])), h2 = mean(c(h0[2], h1[2]))))
    }
  }

  if ("h" %in% names(z)) {
    return(c(h1 = mean(z$h, na.rm = TRUE), h2 = NA_real_))
  }

  c(h1 = NA_real_, h2 = NA_real_)
}

effective_sample_summary <- function(z) {
  if (all(c("N.Co", "N.Tr") %in% names(z))) {
    return(c(N.Co = mean(z$N.Co, na.rm = TRUE), N.Tr = mean(z$N.Tr, na.rm = TRUE)))
  }
  c(N.Co = NA_real_, N.Tr = NA_real_)
}

format_boundary_label <- function(index, aggregate) {
  ifelse(
    is.na(index),
    sprintf("$\\mathtt{%s}$", aggregate),
    sprintf("$\\bb_{%d}$", index)
  )
}

coverage <- function(lower, target, upper) {
  is.finite(lower) & is.finite(upper) & lower <= target & target <= upper
}

summarize_point_rows <- function(rows, target) {
  split_rows <- split(rows, rows$index)

  point_summary <- do.call(rbind, lapply(split_rows, function(z) {
    idx <- z$index[1]
    truth <- target$target[target$index == idx][1]
    bw <- bandwidth_summary(z)
    ns <- effective_sample_summary(z)
    data.frame(
      index = idx,
      aggregate = "",
      h1 = bw["h1"],
      h2 = bw["h2"],
      N.Co = ns["N.Co"],
      N.Tr = ns["N.Tr"],
      Bias = mean(z$estimate.p, na.rm = TRUE) - truth,
      SD = stats::sd(z$estimate.p, na.rm = TRUE),
      RMSE = sqrt(mean((z$estimate.p - truth)^2, na.rm = TRUE)),
      EC = mean(coverage(z$ci.lower, truth, z$ci.upper), na.rm = TRUE),
      IL = mean(z$ci.upper - z$ci.lower, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))

  point_summary[order(point_summary$index), ]
}

summarize_uniform_row <- function(rows, target) {
  uniform_coverage <- vapply(split(rows, rows$replication), function(z) {
    z <- z[order(z$index), ]
    truth <- target$target[match(z$index, target$index)]
    all(coverage(z$cb.lower, truth, z$cb.upper))
  }, logical(1))

  data.frame(
    index = NA_integer_,
    aggregate = "Uniform",
    h1 = NA_real_,
    h2 = NA_real_,
    N.Co = NA_real_,
    N.Tr = NA_real_,
    Bias = NA_real_,
    SD = NA_real_,
    RMSE = NA_real_,
    EC = mean(uniform_coverage, na.rm = TRUE),
    IL = mean(rows$cb.upper - rows$cb.lower, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

summarize_aggregate_rows <- function(rows, target) {
  ordering <- c("WBATE", "LBATE")
  split_rows <- split(rows, rows$aggregate)

  do.call(rbind, lapply(ordering, function(name) {
    z <- split_rows[[name]]
    if (is.null(z)) {
      stop(sprintf("Missing aggregate row: %s.", name), call. = FALSE)
    }
    truth <- target$target[target$aggregate == name][1]
    data.frame(
      index = NA_integer_,
      aggregate = name,
      h1 = NA_real_,
      h2 = NA_real_,
      N.Co = NA_real_,
      N.Tr = NA_real_,
      Bias = mean(z$estimate.p, na.rm = TRUE) - truth,
      SD = stats::sd(z$estimate.p, na.rm = TRUE),
      RMSE = sqrt(mean((z$estimate.p - truth)^2, na.rm = TRUE)),
      EC = mean(coverage(z$ci.lower, truth, z$ci.upper), na.rm = TRUE),
      IL = mean(z$ci.upper - z$ci.lower, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
}

simulation_summary <- function(rows, targets, dgp, design, estimand) {
  target_now <- targets[targets$dgp == dgp & targets$design == design & targets$estimand == estimand, ]
  point_targets <- target_now[target_now$row_type == "point", ]
  aggregate_targets <- target_now[target_now$row_type == "aggregate", ]
  rows_now <- rows[rows$estimand == estimand, ]
  point_rows <- rows_now[rows_now$row_type == "point", ]

  rbind(
    summarize_point_rows(point_rows, point_targets),
    summarize_uniform_row(point_rows, point_targets),
    summarize_aggregate_rows(rows_now[rows_now$row_type == "aggregate", ], aggregate_targets)
  )
}

################################## Output ######################################

write_simulation_table <- function(summary_rows, file) {
  lines <- c(
    "\\begin{tabular}{lccccrrrrr}",
    "\\toprule\\toprule",
    " & $h_1$ & $h_2$ & $N_{\\mathrm{Co}}$ & $N_{\\mathrm{Tr}}$ & Bias & SD & RMSE & EC & IL \\\\",
    "\\midrule"
  )

  for (i in seq_len(nrow(summary_rows))) {
    row <- summary_rows[i, ]
    if (is.na(row$index) && nzchar(row$aggregate)) {
      lines <- c(lines, "\\midrule")
    }
    lines <- c(lines, paste0(
      format_boundary_label(row$index, row$aggregate), " & ",
      fmt_num(row$h1), " & ",
      fmt_num(row$h2), " & ",
      fmt_num(row$N.Co, 0), " & ",
      fmt_num(row$N.Tr, 0), " & ",
      fmt_num(row$Bias), " & ",
      fmt_num(row$SD), " & ",
      fmt_num(row$RMSE), " & ",
      fmt_num(row$EC), " & ",
      fmt_num(row$IL), " \\\\"
    ))
  }

  lines <- c(lines, "\\bottomrule\\bottomrule", "\\end{tabular}")
  writeLines(lines, file)
  invisible(lines)
}

write_estimand_tables <- function(rows, targets, dgp, design, s, suffix, specs) {
  for (estimand in specs$estimand) {
    summary_rows <- simulation_summary(rows, targets, dgp, design, estimand)

    cat("\n")
    cat(sprintf("DGP %d (s=%d, %s): %s\n", dgp, s, suffix, estimand_labels[[estimand]]))
    cat(sprintf("Pointwise coverage: %.4f\n", mean(summary_rows$EC[!is.na(summary_rows$index)], na.rm = TRUE)))
    cat(sprintf("Uniform coverage: %.4f\n", summary_rows$EC[summary_rows$aggregate == "Uniform"]))
    cat(sprintf("WBATE coverage: %.4f\n", summary_rows$EC[summary_rows$aggregate == "WBATE"]))
    cat(sprintf("LBATE coverage: %.4f\n", summary_rows$EC[summary_rows$aggregate == "LBATE"]))

    write_simulation_table(
      summary_rows,
      file.path(tables_dir, sprintf("simuls_%s_s%d_%s.tex", estimand, s, suffix))
    )
  }
}

check_generated_tables <- function() {
  expected <- c(
    file.path(
      tables_dir,
      unlist(lapply(bw_specs$suffix, function(suffix) {
        as.vector(outer(
          estimand_specs$estimand[estimand_specs$design == "fuzzy"],
          c(1, 2, 3),
          function(e, s) sprintf("simuls_%s_s%d_%s.tex", e, s, suffix)
        ))
      }), use.names = FALSE)
    ),
    file.path(
      tables_dir,
      as.vector(outer(
        estimand_specs$estimand[estimand_specs$design == "sharp"],
        c(1, 2, 3),
        function(e, s) sprintf("simuls_%s_s%d_bwmain.tex", e, s)
      ))
    )
  )
  missing <- expected[!file.exists(expected)]
  if (length(missing) > 0) {
    stop(sprintf("Missing expected simulation table: %s", missing[1]), call. = FALSE)
  }

  for (file in expected) {
    txt <- paste(readLines(file, warn = FALSE), collapse = "\n")
    if (!grepl("Uniform", txt, fixed = TRUE) ||
        !grepl("WBATE", txt, fixed = TRUE) ||
        !grepl("LBATE", txt, fixed = TRUE)) {
      stop(sprintf("Generated table is missing Uniform, WBATE, or LBATE: %s", file), call. = FALSE)
    }
    if (!grepl("$N_{\\mathrm{Co}}$", txt, fixed = TRUE) ||
        !grepl("$N_{\\mathrm{Tr}}$", txt, fixed = TRUE)) {
      stop(sprintf("Generated table is missing N.Co or N.Tr headers: %s", file), call. = FALSE)
    }
  }
  invisible(expected)
}

################################### Run ########################################

clean_tables_dir()

metadata <- read_output_csv("simuls_metadata.csv")
m <- as.integer(metadata_value(metadata, "m"))
targets <- read_output_csv("simuls_dgp_targets.csv")

for (design in unique(estimand_specs$design)) {
  specs <- estimand_specs[estimand_specs$design == design, ]
  suffixes <- if (identical(design, "sharp")) "bwmain" else bw_specs$suffix

  for (suffix in suffixes) {
    for (dgp in sort(unique(targets$dgp[targets$design == design]))) {
      s_now <- unique(targets$s[targets$design == design & targets$dgp == dgp])
      if (length(s_now) != 1) {
        stop(sprintf("Targets for %s DGP %d have inconsistent s values.", design, dgp), call. = FALSE)
      }

      rows <- read_simulation_rows(m = m, dgp = dgp, design = design, s = s_now, suffix = suffix, specs = specs)
      write_estimand_tables(rows, targets, dgp, design, s_now, suffix, specs)
    }
  }
}

expected <- check_generated_tables()

cat("\n")
cat("Simulation LaTeX tables complete.\n")
cat(sprintf("Tables written to: %s\n", tables_dir))
cat(sprintf("Generated %d table file(s).\n", length(expected)))
