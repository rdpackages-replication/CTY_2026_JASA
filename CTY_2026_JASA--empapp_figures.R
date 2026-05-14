################################################################################################
# Estimation and Inference in Boundary Discontinuity Designs: Location-Based Methods
# Empirical Application: SPP data
# Figures
################################################################################################

rm(list = ls(all = TRUE))

# This script converts the CSV files produced by CTY_2026_JASA--empapp.R into
# empirical-application figures. File names are tied to the source CSV stems so
# the output/tables/figures pipelines stay aligned.

get_script_dir <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) > 0) {
    return(normalizePath(dirname(sub("^--file=", "", file_arg[1])), winslash = "/"))
  }
  normalizePath(getwd(), winslash = "/")
}

setwd(get_script_dir())

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package ggplot2 is required to generate figures.", call. = FALSE)
}

output_dir <- "output"
figures_dir <- "figures"
dir.create(figures_dir, showWarnings = FALSE)

# One row per result CSV. Each result produces point-estimate, inference,
# effect-heatmap, and p-value heatmap figures.
figure_specs <- data.frame(
  id = c(
    "empapp_main_fuzzy",
    "empapp_main_itt",
    "empapp_main_fs",
    "empapp_main_itt0",
    "empapp_covbal_itt"
  ),
  ylab = c(
    "Treatment effect",
    "Intention-to-treat effect",
    "First stage",
    "Control-side baseline outcome",
    "Covariate balance ITT"
  ),
  heat_label = c("Fuzzy", "ITT", "FS", "Control baseline", "Cov. Bal."),
  stringsAsFactors = FALSE
)

bw_specs <- data.frame(
  bwparam = c("main", "itt"),
  suffix = c("bwmain", "bwitt"),
  stringsAsFactors = FALSE
)

################################## Input #######################################

clean_figures_dir <- function() {
  # Generated figures should be reproducible from output/*.csv, so remove stale
  # PNGs before writing the current set.
  old_files <- list.files(figures_dir, pattern = "\\.png$", full.names = TRUE)
  if (length(old_files) > 0) unlink(old_files)
}

read_output_csv <- function(file) {
  path <- file.path(output_dir, file)
  if (!file.exists(path)) {
    stop(sprintf("Missing output file: %s. Run CTY_2026_JASA--empapp.R first.", path), call. = FALSE)
  }
  read.csv(path, check.names = FALSE)
}

read_result_table <- function(id, suffix) {
  tab <- read_output_csv(paste0(id, "_", suffix, ".csv"))
  required <- c(
    "row", "b1", "b2", "estimate.p", "estimate.q", "p.value",
    "ci.lower", "ci.upper", "cb.lower", "cb.upper"
  )
  missing <- setdiff(required, names(tab))
  if (length(missing) > 0) {
    stop(sprintf("%s.csv is missing columns: %s", id, paste(missing, collapse = ", ")), call. = FALSE)
  }

  tab$row <- as.character(tab$row)
  tab
}

read_percent_table <- function(suffix) {
  file <- paste0("empapp_main_percent_", suffix, ".csv")
  tab <- read_output_csv(file)
  required <- c("row", "itt.percent", "fuzzy.percent")
  missing <- setdiff(required, names(tab))
  if (length(missing) > 0) {
    stop(sprintf("%s is missing columns: %s", file, paste(missing, collapse = ", ")), call. = FALSE)
  }

  tab$row <- as.character(tab$row)
  tab
}

point_rows <- function(tab) tab[!(tab$row %in% c("WBATE", "LBATE")), , drop = FALSE]

aggregate_value <- function(tab, name) {
  row <- tab[tab$row == name, , drop = FALSE]
  if (nrow(row) == 0 || !is.finite(row$estimate.p[1])) return(NA_real_)
  row$estimate.p[1]
}

################################ Formatting ####################################

eval_axis_breaks <- c(1, 5, 10, 15, 21, 25, 30, 35, 40)
eval_axis_labels <- paste0("b", eval_axis_breaks)

theme_rd2d <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 13),
      axis.text = ggplot2::element_text(size = 10),
      legend.position = c(0.78, 0.9),
      legend.justification = c(0, 1),
      legend.background = ggplot2::element_rect(fill = "white", colour = NA),
      legend.title = ggplot2::element_text(size = 11),
      legend.text = ggplot2::element_text(size = 10),
      legend.key.width = grid::unit(0.55, "cm"),
      legend.key.height = grid::unit(0.45, "cm"),
      legend.margin = ggplot2::margin(3, 4, 3, 4),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
}

plot_ylim <- function(...) {
  vals <- unlist(list(...), use.names = FALSE)
  vals <- vals[is.finite(vals)]
  lim <- range(vals)
  pad <- max(0.02, diff(lim) * 0.08)
  c(lim[1] - pad, lim[2] + pad)
}

b21_line <- function() {
  ggplot2::geom_vline(xintercept = 21, color = "grey90", linewidth = 0.45)
}

point_legend_theme <- function(id) {
  if (id %in% c("empapp_main_fuzzy", "empapp_main_itt")) {
    return(ggplot2::theme(
      legend.position = c(0.05, 0.05),
      legend.justification = c(0, 0)
    ))
  }
  if (id == "empapp_main_fs") {
    return(ggplot2::theme(
      legend.position = c(0.05, 0.62),
      legend.justification = c(0, 0.5)
    ))
  }
  if (id == "empapp_main_itt0") {
    return(ggplot2::theme(
      legend.position = c(0.05, 0.52),
      legend.justification = c(0, 0.5)
    ))
  }
  if (id == "empapp_covbal_itt") {
    return(ggplot2::theme(
      legend.position = c(0.72, 0.08),
      legend.justification = c(0, 0)
    ))
  }
  ggplot2::theme(legend.position = "right", legend.justification = "center")
}

inference_legend_theme <- function(id) {
  if (id == "empapp_main_fuzzy") {
    return(ggplot2::theme(
      legend.position = c(0.05, 0.05),
      legend.justification = c(0, 0)
    ))
  }
  if (id == "empapp_main_itt") {
    return(ggplot2::theme(
      legend.position = c(0.05, 0.05),
      legend.justification = c(0, 0)
    ))
  }
  if (id == "empapp_main_itt0") {
    return(ggplot2::theme(
      legend.position = c(0.05, 0.55),
      legend.justification = c(0, 0.5)
    ))
  }
  if (id == "empapp_main_fs") {
    return(ggplot2::theme(
      legend.position = c(0.05, 0.95),
      legend.justification = c(0, 1)
    ))
  }
  if (id == "empapp_covbal_itt") {
    return(ggplot2::theme(
      legend.position = c(0.05, 0.95),
      legend.justification = c(0, 1)
    ))
  }
  ggplot2::theme()
}

heatmap_legend_theme <- function() {
  ggplot2::theme(
    legend.position = c(0.64, 0.86),
    legend.justification = c(0, 1)
  )
}

save_png <- function(plot, file, width = 6, height = 5) {
  device <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png"
  ggplot2::ggsave(
    filename = file.path(figures_dir, file),
    plot = plot,
    width = width,
    height = height,
    dpi = 300,
    device = device
  )
  invisible(plot)
}

figure_file <- function(id, kind, suffix) {
  paste0(id, "_", kind, "_", suffix, ".png")
}

################################ Scatter Plot ##################################

save_scatter_plot <- function(dat, eval, suffix) {
  eval_labeled <- data.frame(
    eval,
    indx = seq_len(nrow(eval)),
    label = sprintf("b%d", seq_len(nrow(eval)))
  )
  eval_labeled <- eval_labeled[eval_labeled$indx %in% eval_axis_breaks, , drop = FALSE]
  eval_labeled$label_x <- ifelse(eval_labeled$x.1 == 0, -3.2, eval_labeled$x.1)
  eval_labeled$label_y <- ifelse(eval_labeled$x.1 == 0, eval_labeled$x.2, -2.4)
  eval_labeled$label_hjust <- ifelse(eval_labeled$x.1 == 0, 1, 0.5)
  eval_labeled$label_vjust <- ifelse(eval_labeled$x.1 == 0, 0.5, 1)
  boundary_segments <- data.frame(
    x = c(0, 0),
    xend = c(0, 80),
    y = c(0, 0),
    yend = c(55, 0),
    group = "Boundary",
    stringsAsFactors = FALSE
  )

  p <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data = dat,
      ggplot2::aes(x = x.1, y = x.2, color = factor(assignment), shape = factor(assignment)),
      alpha = 0.18,
      size = 0.35
    ) +
    ggplot2::geom_segment(
      data = boundary_segments,
      ggplot2::aes(x = x, xend = xend, y = y, yend = yend, color = group),
      linewidth = 1.0,
      linetype = "solid",
      lineend = "butt"
    ) +
    ggplot2::geom_point(
      data = eval,
      ggplot2::aes(x = x.1, y = x.2),
      size = 0.65
    ) +
    ggplot2::geom_text(
      data = eval_labeled,
      ggplot2::aes(
        x = label_x,
        y = label_y,
        label = label,
        hjust = label_hjust,
        vjust = label_vjust
      ),
      color = "black",
      size = 2.7
    ) +
    ggplot2::scale_color_manual(
      values = c("0" = "indianred2", "1" = "dodgerblue4", "Boundary" = "grey45"),
      breaks = c("0", "1", "Boundary"),
      name = NULL,
      labels = c("Control", "Treatment", "Boundary")
    ) +
    ggplot2::scale_shape_manual(
      values = c("0" = 15, "1" = 16),
      name = NULL,
      labels = c("Control", "Treatment")
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        override.aes = list(
          alpha = c(1, 1, 1),
          size = c(2, 2, 0),
          shape = c(15, 16, NA),
          linetype = c("blank", "blank", "solid"),
          linewidth = c(0, 0, 1)
        )
      ),
      shape = "none"
    ) +
    ggplot2::coord_cartesian(xlim = c(-80, 100), ylim = c(-40, 60)) +
    ggplot2::labs(x = "Saber 11", y = "Sisben") +
    theme_rd2d() +
    ggplot2::theme(
      legend.position = c(0.23, 0.88),
      legend.justification = c(0.5, 1),
      legend.background = ggplot2::element_rect(fill = "white", colour = NA)
    )

  save_png(p, paste0("empapp_scatter_", suffix, ".png"))
}

############################### Result Figures ################################

save_point_plot <- function(tab, id, ylab, suffix) {
  pts <- point_rows(tab)
  pts$indx <- seq_len(nrow(pts))
  wbate <- aggregate_value(tab, "WBATE")
  lbate <- aggregate_value(tab, "LBATE")
  line_df <- data.frame(
    label = c("WBATE", "LBATE"),
    y = c(wbate, lbate),
    stringsAsFactors = FALSE
  )
  line_df <- line_df[is.finite(line_df$y), , drop = FALSE]

  p <- ggplot2::ggplot(pts, ggplot2::aes(x = indx, y = estimate.p)) +
    b21_line() +
    ggplot2::geom_point(ggplot2::aes(color = "Estimate"), size = 1.35) +
    ggplot2::geom_hline(
      data = line_df,
      ggplot2::aes(yintercept = y, color = label, linetype = label),
      linewidth = 0.6,
      show.legend = TRUE
    ) +
    ggplot2::scale_color_manual(
      values = c("Estimate" = "black", "WBATE" = "dodgerblue4", "LBATE" = "firebrick"),
      name = NULL,
      breaks = c("Estimate", "WBATE", "LBATE")
    ) +
    ggplot2::scale_linetype_manual(
      values = c("Estimate" = "blank", "WBATE" = "dotted", "LBATE" = "longdash"),
      name = NULL,
      breaks = c("Estimate", "WBATE", "LBATE")
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        override.aes = list(
          shape = c(16, NA, NA),
          linetype = c("blank", "dotted", "longdash"),
          linewidth = c(0, 0.6, 0.6)
        )
      ),
      linetype = "none"
    ) +
    ggplot2::scale_x_continuous(breaks = eval_axis_breaks, labels = eval_axis_labels) +
    ggplot2::coord_cartesian(xlim = c(1, 40), ylim = plot_ylim(pts$estimate.p, wbate, lbate)) +
    ggplot2::labs(x = "Cutoffs on the boundary", y = ylab) +
    theme_rd2d() +
    point_legend_theme(id) +
    ggplot2::theme(
      legend.background = ggplot2::element_rect(fill = "white", colour = NA)
    )

  save_png(p, figure_file(id, "pointest", suffix))
}

save_percent_point_plot <- function(tab, value_col, ylab, file) {
  pts <- point_rows(tab)
  pts$indx <- seq_len(nrow(pts))
  pts$value <- pts[[value_col]]

  aggregates <- tab[tab$row %in% c("WBATE", "LBATE"), , drop = FALSE]
  line_df <- data.frame(
    label = aggregates$row,
    value = aggregates[[value_col]],
    stringsAsFactors = FALSE
  )
  line_df <- line_df[is.finite(line_df$value), , drop = FALSE]

  p <- ggplot2::ggplot(pts, ggplot2::aes(x = indx, y = value)) +
    b21_line() +
    ggplot2::geom_point(ggplot2::aes(color = "Estimate"), size = 1.35) +
    ggplot2::geom_hline(
      data = line_df,
      ggplot2::aes(yintercept = value, color = label, linetype = label),
      linewidth = 0.6,
      show.legend = TRUE
    ) +
    ggplot2::scale_color_manual(
      values = c("Estimate" = "black", "WBATE" = "dodgerblue4", "LBATE" = "firebrick"),
      name = NULL,
      breaks = c("Estimate", "WBATE", "LBATE")
    ) +
    ggplot2::scale_linetype_manual(
      values = c("Estimate" = "blank", "WBATE" = "dotted", "LBATE" = "longdash"),
      name = NULL,
      breaks = c("Estimate", "WBATE", "LBATE")
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        override.aes = list(
          shape = c(16, NA, NA),
          linetype = c("blank", "dotted", "longdash"),
          linewidth = c(0, 0.6, 0.6)
        )
      ),
      linetype = "none"
    ) +
    ggplot2::scale_x_continuous(breaks = eval_axis_breaks, labels = eval_axis_labels) +
    ggplot2::coord_cartesian(
      xlim = c(1, 40),
      ylim = plot_ylim(pts$value, line_df$value)
    ) +
    ggplot2::labs(x = "Cutoffs on the boundary", y = ylab) +
    theme_rd2d() +
    ggplot2::theme(
      legend.position = c(0.95, 0.95),
      legend.justification = c(1, 1),
      legend.background = ggplot2::element_rect(fill = "white", colour = NA)
    )

  save_png(p, file)
}

save_inference_plot <- function(tab, id, ylab, suffix) {
  pts <- point_rows(tab)
  pts$indx <- seq_len(nrow(pts))
  pts$band.lower <- ifelse(is.finite(pts$cb.lower), pts$cb.lower, pts$ci.lower)
  pts$band.upper <- ifelse(is.finite(pts$cb.upper), pts$cb.upper, pts$ci.upper)

  p <- ggplot2::ggplot(pts, ggplot2::aes(x = indx)) +
    b21_line() +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = band.lower, ymax = band.upper, fill = "95% CB", color = "95% CB"),
      alpha = 0.14,
      linewidth = 0
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = ci.lower, ymax = ci.upper, color = "95% CI"),
      width = 0.12,
      linewidth = 0.35
    ) +
    ggplot2::geom_point(ggplot2::aes(y = estimate.p, color = "Estimate"), size = 1.2) +
    ggplot2::scale_color_manual(
      values = c("Estimate" = "black", "95% CI" = "black", "95% CB" = "dodgerblue4"),
      name = NULL,
      breaks = c("Estimate", "95% CI", "95% CB")
    ) +
    ggplot2::scale_fill_manual(values = c("95% CB" = "dodgerblue4"), name = NULL) +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        override.aes = list(
          shape = c(16, NA, 22),
          linetype = c("blank", "solid", "blank"),
          fill = c(NA, NA, "dodgerblue4"),
          alpha = c(1, 1, 0.14),
          linewidth = c(0, 0.35, 0)
        )
      ),
      fill = "none"
    ) +
    ggplot2::scale_x_continuous(breaks = eval_axis_breaks, labels = eval_axis_labels) +
    ggplot2::coord_cartesian(
      xlim = c(1, 40),
      ylim = plot_ylim(pts$estimate.p, pts$ci.lower, pts$ci.upper, pts$band.lower, pts$band.upper)
    ) +
    ggplot2::labs(x = "Cutoffs on the boundary", y = ylab) +
    theme_rd2d() +
    inference_legend_theme(id)

  save_png(p, figure_file(id, "inference", suffix))
}

save_effect_heatmap <- function(tab, eval, id, label, suffix) {
  pts <- point_rows(tab)
  data_plot <- data.frame(eval, estimate.p = pts$estimate.p)
  data_plot$label <- sprintf("%02d", seq_len(nrow(data_plot)))
  mid <- stats::median(data_plot$estimate.p, na.rm = TRUE)

  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = x.1, y = x.2)) +
    ggplot2::geom_tile(
      ggplot2::aes(fill = estimate.p),
      color = "white",
      linewidth = 0.35,
      show.legend = TRUE
    ) +
    ggplot2::scale_fill_gradient2(
      low = "dodgerblue4",
      mid = "white",
      high = "firebrick",
      midpoint = mid,
      name = label
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = label),
      color = "black",
      size = 2.35
    ) +
    ggplot2::coord_fixed(ratio = 56 / 40, xlim = c(-10, 64), ylim = c(-10, 42)) +
    ggplot2::labs(x = "Saber 11", y = "Sisben") +
    theme_rd2d() +
    heatmap_legend_theme()

  save_png(p, figure_file(id, "heatmap", suffix))
}

save_pvalue_heatmap <- function(tab, eval, id, label, suffix) {
  pts <- point_rows(tab)
  data_plot <- data.frame(eval, p.value = pts$p.value)
  data_plot$p.sig <- cut(
    data_plot$p.value,
    breaks = c(0, 0.001, 0.01, 0.05, 0.1, 1),
    labels = c("0.000", "[0.001, 0.010)", "[0.010, 0.050)", "[0.050, 0.100)", ">= 0.100"),
    include.lowest = TRUE
  )
  sig_colors <- c(
    "0.000" = "#b2182b",
    "[0.001, 0.010)" = "#ef8a62",
    "[0.010, 0.050)" = "#fddbc7",
    "[0.050, 0.100)" = "#d1e5f0",
    ">= 0.100" = "#67a9cf"
  )

  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = x.1, y = x.2, fill = p.sig)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.35, show.legend = TRUE) +
    ggplot2::scale_fill_manual(values = sig_colors, name = label, drop = FALSE) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%02d", seq_len(nrow(data_plot)))),
      color = "black",
      size = 2.35
    ) +
    ggplot2::coord_fixed(ratio = 56 / 40, xlim = c(-10, 64), ylim = c(-10, 42)) +
    ggplot2::labs(x = "Saber 11", y = "Sisben") +
    theme_rd2d() +
    heatmap_legend_theme()

  save_png(p, figure_file(id, "heatmap_pval", suffix))
}

write_result_figures <- function(id, ylab, heat_label, eval, suffix) {
  tab <- read_result_table(id, suffix)
  save_point_plot(tab, id, ylab, suffix)
  save_inference_plot(tab, id, ylab, suffix)
  save_effect_heatmap(tab, eval, id, heat_label, suffix)
  save_pvalue_heatmap(tab, eval, id, heat_label, suffix)
}

################################ Sanity Checks #################################

check_generated_figures <- function(expected_figures) {
  missing_figures <- expected_figures[!file.exists(file.path(figures_dir, expected_figures))]
  if (length(missing_figures) > 0) {
    stop(sprintf("Missing expected figures: %s", paste(missing_figures, collapse = ", ")), call. = FALSE)
  }

  too_small <- expected_figures[file.info(file.path(figures_dir, expected_figures))$size < 1000]
  if (length(too_small) > 0) {
    stop(sprintf("Generated figure file(s) look empty: %s", paste(too_small, collapse = ", ")), call. = FALSE)
  }
}

################################### Run ########################################

clean_figures_dir()

eval <- read_output_csv("empapp_eval.csv")
data <- read_output_csv("empapp_data.csv")

for (j in seq_len(nrow(bw_specs))) {
  suffix <- bw_specs$suffix[j]
  save_scatter_plot(data, eval, suffix)

  for (i in seq_len(nrow(figure_specs))) {
    write_result_figures(
      id = figure_specs$id[i],
      ylab = figure_specs$ylab[i],
      heat_label = figure_specs$heat_label[i],
      eval = eval,
      suffix = suffix
    )
  }

  percent_table <- read_percent_table(suffix)
  save_percent_point_plot(
    percent_table,
    value_col = "itt.percent",
    ylab = "ITT / ITT.0 (%)",
    file = figure_file("empapp_main_percent_itt", "pointest", suffix)
  )
  save_percent_point_plot(
    percent_table,
    value_col = "fuzzy.percent",
    ylab = "Fuzzy / ITT.0 (%)",
    file = figure_file("empapp_main_percent_fuzzy", "pointest", suffix)
  )
}

expected_figures <- unlist(
  lapply(bw_specs$suffix, function(suffix) {
    c(
      paste0("empapp_scatter_", suffix, ".png"),
      as.vector(outer(
        figure_specs$id,
        c("pointest", "inference", "heatmap", "heatmap_pval"),
        Vectorize(function(id, kind) figure_file(id, kind, suffix))
      )),
      figure_file("empapp_main_percent_itt", "pointest", suffix),
      figure_file("empapp_main_percent_fuzzy", "pointest", suffix)
    )
  }),
  use.names = FALSE
)
check_generated_figures(expected_figures)

cat("Empirical application figures complete.\n")
cat(sprintf("Figures written to: %s\n", figures_dir))
