################################################################################################
# Estimation and Inference in Boundary Discontinuity Designs: Location-Based Methods
# Empirical Application: SPP data
# Tables
################################################################################################

rm(list = ls(all = TRUE))

# This script converts the CSV files produced by CTY_2026_JASA--empapp.R into
# manuscript-ready LaTeX tabular fragments. The full table keeps every
# evaluation point; the small table keeps selected evaluation points plus the
# WBATE and LBATE rows.

get_script_dir <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) > 0) {
    return(normalizePath(dirname(sub("^--file=", "", file_arg[1])), winslash = "/"))
  }
  normalizePath(getwd(), winslash = "/")
}

setwd(get_script_dir())

output_dir <- "output"
tables_dir <- "tables"
small_rows <- c(1, 5, 10, 15, 20, 25, 30, 35, 40)
aggregate_labels <- c("WBATE", "LBATE")

dir.create(tables_dir, showWarnings = FALSE)

# One row per CSV output that should become a pair of LaTeX tables. File names
# are intentionally tied to the source CSV stems.
table_specs <- data.frame(
  id = c(
    "empapp_main_fuzzy",
    "empapp_main_itt",
    "empapp_main_fs",
    "empapp_main_itt0",
    "empapp_covbal_itt",
    "empapp_main_percent"
  ),
  kind = c("effect", "effect", "effect", "effect", "effect", "percent"),
  small = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
  stringsAsFactors = FALSE
)

bw_specs <- data.frame(
  bwparam = c("main", "itt"),
  suffix = c("bwmain", "bwitt"),
  stringsAsFactors = FALSE
)

################################## Input #######################################

clean_tables_dir <- function() {
  # Generated tables should be reproducible from output/*.csv, so remove stale
  # empirical fragments before writing the current set.
  old_files <- list.files(tables_dir, pattern = "^empapp_.*\\.tex$", full.names = TRUE)
  if (length(old_files) > 0) unlink(old_files)
}

read_result_table <- function(id, kind, suffix) {
  path <- file.path(output_dir, paste0(id, "_", suffix, ".csv"))
  if (!file.exists(path)) {
    stop(sprintf("Missing output file: %s. Run CTY_2026_JASA--empapp.R first.", path), call. = FALSE)
  }

  tab <- read.csv(path, check.names = FALSE)
  required <- c("row", "h01", "h02", "h11", "h12", "N.Co", "N.Tr")
  if (kind == "effect") {
    required <- c(required, "estimate.p", "p.value", "ci.lower", "ci.upper")
  } else if (kind == "percent") {
    required <- c(required, "itt.percent", "fuzzy.percent")
  } else {
    stop(sprintf("Unknown table kind: %s", kind), call. = FALSE)
  }
  missing <- setdiff(required, names(tab))
  if (length(missing) > 0) {
    stop(sprintf("%s is missing columns: %s", path, paste(missing, collapse = ", ")), call. = FALSE)
  }

  tab$row <- as.character(tab$row)
  tab
}

################################ Formatting ####################################

is_aggregate_row <- function(row_label) row_label %in% aggregate_labels

fmt_num <- function(x, digits = 3) {
  if (is.na(x) || !is.finite(x)) return("")
  sprintf(paste0("%.", digits, "f"), x)
}

fmt_pvalue <- function(x) {
  if (is.na(x) || !is.finite(x)) return("")
  if (x < 0.001) return("0.000")
  sprintf("%.3f", x)
}

fmt_interval <- function(lower, upper) {
  if (is.na(lower) || is.na(upper) || !is.finite(lower) || !is.finite(upper)) return("")
  sprintf("(%s, %s)", fmt_num(lower), fmt_num(upper))
}

fmt_row_label <- function(row_id) {
  if (row_id == "WBATE") return("$\\mathtt{WBATE}$")
  if (row_id == "LBATE") return("$\\mathtt{LBATE}$")
  sprintf("$\\bb_{%s}$", row_id)
}

bandwidth_values <- function(tab, i) {
  if (is_aggregate_row(tab$row[i])) return(c(h1 = NA_real_, h2 = NA_real_))

  h0 <- c(tab$h01[i], tab$h02[i])
  h1 <- c(tab$h11[i], tab$h12[i])
  h0_ok <- all(is.finite(h0))
  h1_ok <- all(is.finite(h1))

  if (h0_ok && h1_ok && isTRUE(all.equal(h0, h1))) {
    return(c(h1 = h0[1], h2 = h0[2]))
  }
  if (h0_ok) return(c(h1 = h0[1], h2 = h0[2]))
  if (h1_ok) return(c(h1 = h1[1], h2 = h1[2]))

  c(h1 = NA_real_, h2 = NA_real_)
}

effective_sample_values <- function(tab, i) {
  if (is_aggregate_row(tab$row[i])) return(c(N.Co = NA_real_, N.Tr = NA_real_))
  c(N.Co = tab$N.Co[i], N.Tr = tab$N.Tr[i])
}

interval_limits <- function(tab) {
  # The output CSVs contain pointwise CIs and, when requested, uniform CBs.
  # For display, use CBs wherever available; WBATE/LBATE keep their CI limits.
  has_cb <- all(c("cb.lower", "cb.upper") %in% names(tab))
  use_cb <- rep(FALSE, nrow(tab))
  if (has_cb) {
    use_cb <- is.finite(tab$cb.lower) & is.finite(tab$cb.upper)
  }

  data.frame(
    lower = ifelse(use_cb, tab$cb.lower, tab$ci.lower),
    upper = ifelse(use_cb, tab$cb.upper, tab$ci.upper)
  )
}

################################## Output ######################################

display_rows <- function(tab, keep_rows = NULL) {
  aggregate_rows <- tab[is_aggregate_row(tab$row), , drop = FALSE]
  point_rows <- tab[!is_aggregate_row(tab$row), , drop = FALSE]

  if (!is.null(keep_rows)) {
    point_rows <- point_rows[point_rows$row %in% as.character(keep_rows), , drop = FALSE]
  }

  out <- rbind(point_rows, aggregate_rows)
  ints <- interval_limits(out)
  bws <- t(vapply(seq_len(nrow(out)), function(i) bandwidth_values(out, i), numeric(2)))
  ns <- t(vapply(seq_len(nrow(out)), function(i) effective_sample_values(out, i), numeric(2)))

  data.frame(
    label = vapply(out$row, fmt_row_label, character(1)),
    h1 = bws[, "h1"],
    h2 = bws[, "h2"],
    N.Co = ns[, "N.Co"],
    N.Tr = ns[, "N.Tr"],
    estimate = out$estimate.p,
    pvalue = out$p.value,
    lower = ints$lower,
    upper = ints$upper,
    stringsAsFactors = FALSE
  )
}

display_percent_rows <- function(tab) {
  aggregate_rows <- tab[is_aggregate_row(tab$row), , drop = FALSE]
  point_rows <- tab[!is_aggregate_row(tab$row), , drop = FALSE]
  out <- rbind(point_rows, aggregate_rows)
  bws <- t(vapply(seq_len(nrow(out)), function(i) bandwidth_values(out, i), numeric(2)))
  ns <- t(vapply(seq_len(nrow(out)), function(i) effective_sample_values(out, i), numeric(2)))

  data.frame(
    label = vapply(out$row, fmt_row_label, character(1)),
    h1 = bws[, "h1"],
    h2 = bws[, "h2"],
    N.Co = ns[, "N.Co"],
    N.Tr = ns[, "N.Tr"],
    itt = out$itt.percent,
    fuzzy = out$fuzzy.percent,
    stringsAsFactors = FALSE
  )
}

latex_body <- function(rows) {
  lines <- character(0)
  for (i in seq_len(nrow(rows))) {
    if (grepl("WBATE|LBATE", rows$label[i])) {
      lines <- c(lines, "  \\midrule")
    }
    lines <- c(lines, paste0(
      "   ", rows$label[i], " & ",
      fmt_num(rows$h1[i], 1), " & ",
      fmt_num(rows$h2[i], 1), " & ",
      fmt_num(rows$N.Co[i], 0), " & ",
      fmt_num(rows$N.Tr[i], 0), " & ",
      fmt_num(rows$estimate[i]), " & ",
      fmt_pvalue(rows$pvalue[i]), " & ",
      fmt_interval(rows$lower[i], rows$upper[i]), "\\\\"
    ))
  }
  lines
}

latex_percent_body <- function(rows) {
  lines <- character(0)
  for (i in seq_len(nrow(rows))) {
    if (grepl("WBATE|LBATE", rows$label[i])) {
      lines <- c(lines, "  \\midrule")
    }
    lines <- c(lines, paste0(
      "   ", rows$label[i], " & ",
      fmt_num(rows$h1[i], 1), " & ",
      fmt_num(rows$h2[i], 1), " & ",
      fmt_num(rows$N.Co[i], 0), " & ",
      fmt_num(rows$N.Tr[i], 0), " & ",
      fmt_num(rows$itt[i]), " & ",
      fmt_num(rows$fuzzy[i]), "\\\\"
    ))
  }
  lines
}

write_tabular <- function(rows, file) {
  lines <- c(
    "\\begin{tabular}{lccccccc}",
    "  \\toprule\\toprule",
    "   & $h_1$ & $h_2$ & $N_{\\mathrm{Co}}$ & $N_{\\mathrm{Tr}}$ & Estimate & $p$-value & 95\\% CI \\\\",
    "  \\midrule",
    latex_body(rows),
    "  \\bottomrule\\bottomrule",
    "\\end{tabular}"
  )
  writeLines(lines, file)
  invisible(lines)
}

write_percent_tabular <- function(rows, file) {
  lines <- c(
    "\\begin{tabular}{lcccccc}",
    "  \\toprule\\toprule",
    "   & $h_1$ & $h_2$ & $N_{\\mathrm{Co}}$ & $N_{\\mathrm{Tr}}$ & ITT / ITT.0 (\\%) & Fuzzy / ITT.0 (\\%) \\\\",
    "  \\midrule",
    latex_percent_body(rows),
    "  \\bottomrule\\bottomrule",
    "\\end{tabular}"
  )
  writeLines(lines, file)
  invisible(lines)
}

table_file <- function(id, suffix, small = FALSE) {
  paste0(id, if (isTRUE(small)) "_small" else "", "_", suffix, ".tex")
}

write_table_set <- function(id, kind, small, suffix) {
  tab <- read_result_table(id, kind, suffix)

  if (kind == "percent") {
    write_percent_tabular(display_percent_rows(tab), file.path(tables_dir, table_file(id, suffix)))
    return(invisible(TRUE))
  }

  write_tabular(
    display_rows(tab),
    file.path(tables_dir, table_file(id, suffix))
  )
  if (isTRUE(small)) {
    write_tabular(
      display_rows(tab, keep_rows = small_rows),
      file.path(tables_dir, table_file(id, suffix, small = TRUE))
    )
  }
}

################################ Sanity Checks #################################

check_generated_tables <- function(expected_tables) {
  missing_tables <- expected_tables[!file.exists(file.path(tables_dir, expected_tables))]
  if (length(missing_tables) > 0) {
    stop(sprintf("Missing expected tables: %s", paste(missing_tables, collapse = ", ")), call. = FALSE)
  }

  main_tables <- vapply(
    bw_specs$suffix,
    function(suffix) {
      paste(readLines(file.path(tables_dir, table_file("empapp_main_fuzzy", suffix))), collapse = "\n")
    },
    character(1)
  )
  main_table <- paste(main_tables, collapse = "\n")
  forbidden <- c("Z value", "t-statistic", "P>|t|", "P>|z|", "CI/CB", "$<$0.001")
  if (any(vapply(forbidden, grepl, logical(1), x = main_table, fixed = TRUE))) {
    stop("Generated empirical table contains an obsolete label.", call. = FALSE)
  }
  if (!grepl("WBATE", main_table) || !grepl("LBATE", main_table)) {
    stop("Generated empirical table is missing WBATE or LBATE.", call. = FALSE)
  }
  if (!grepl("$N_{\\mathrm{Co}}$", main_table, fixed = TRUE) || !grepl("$N_{\\mathrm{Tr}}$", main_table, fixed = TRUE)) {
    stop("Generated empirical table is missing N.Co or N.Tr headers.", call. = FALSE)
  }
}

################################### Run ########################################

clean_tables_dir()

for (j in seq_len(nrow(bw_specs))) {
  for (i in seq_len(nrow(table_specs))) {
    write_table_set(
      table_specs$id[i],
      table_specs$kind[i],
      table_specs$small[i],
      bw_specs$suffix[j]
    )
  }
}

expected_tables <- unlist(
  lapply(bw_specs$suffix, function(suffix) {
    c(
      vapply(table_specs$id, table_file, character(1), suffix = suffix),
      vapply(table_specs$id[table_specs$small], table_file, character(1), suffix = suffix, small = TRUE)
    )
  }),
  use.names = FALSE
)
check_generated_tables(expected_tables)

cat("Empirical application LaTeX tables complete.\n")
cat(sprintf("Tables written to: %s\n", tables_dir))
