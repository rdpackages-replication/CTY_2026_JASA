
# rd2d: illustration file
# Authors: M. D. Cattaneo, R. Titiunik, R. R. Yu

rm(list=ls(all=TRUE))

library(MASS)
library(ggplot2)
library(rdrobust)
library(latex2exp)
library(tidyr)
library(dplyr)
library(haven)
library(xtable)
library(expm)
library(scales)
library(rd2d)

################################## Load Data ###################################

data <- read.csv("spp.csv")
data$X <- NULL
colnames(data) <- c("x.1", "x.2","y","d")
na.ok <- complete.cases(data$x.1) & complete.cases(data$x.2)
data <- data[na.ok,]

neval <- 40
eval <- matrix(nrow = neval, ncol = 2)
for (i in 1: ceiling(neval * 0.5)){
  eval[i,] <- c(0, 40 - (i-1) * 40 / ceiling(neval * 0.5))
}
for (i in (ceiling(neval * 0.5)+1): neval){
  eval[i,] <- c((i - ceiling(neval * 0.5) - 1) *56 / (ceiling(neval * 0.5)),0)
}
eval <- data.frame(eval)
colnames(eval) <- c("x.1", "x.2")

####################### Scatter Plot and Boundary ############################## 

bound <- 40
eval_labeled <- eval %>%
  mutate(
    index = row_number(),
    label = as.list(if_else(
      index %in% c(1, 5, 10,15, 21,  25, 30, 35, 40),
      paste0("$\\textbf{b}_{", index, "}$"),  # e.g. "$x_{10}$"
      NA_character_
    ))
  )

eval_labeled_bound <- eval_labeled[eval_labeled$index <= bound,]

# Remove label for index 31
eval_labeled_bound_no_21 <- eval_labeled_bound %>%
  filter(index != 21)
annotation_data <- eval_labeled_bound_no_21 %>%
  filter(!is.na(label))

# Extract the coordinates of point 31
point_21 <- eval_labeled_bound %>% filter(index == 21)
x_21 <- point_21$x.1
y_21 <- point_21$x.2

p1.1 <- ggplot() +
  geom_point(
    data = data  %>% sample_frac(0.3),
    aes(x = x.1, y = x.2, color = factor(d)),
    alpha = 0.5, size = 0.5
  ) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 55),
               linetype = "solid", color = "grey", alpha = 1, size = 3) +
  geom_segment(aes(x = 0, xend = 80, y = 0, yend = 0),
               linetype = "solid", color = "grey", alpha = 1, size = 3) +
  
  # Plot evaluation points excluding index = 21
  geom_point(
    data = eval_labeled_bound_no_21,
    aes(x = x.1, y = x.2),  # Map color to factor(d)
    alpha = 1,
    size = 0.5
  ) +
  
  # Use scale_color_manual to define your own colors for d=0 and d=1
  scale_color_manual(
    name = NULL,  # Legend title (optional)
    values = c("0" = "#619CFF",  # Color for d=0
               "1" = "#F8766D"), # Color for d=1
    labels = c("0" = "Control", "1" = "Treatment")
  )

annotation_data <- eval_labeled_bound_no_21 %>%
  filter(!is.na(label))

# Loop through each row and add an annotation using TeX() for LaTeX parsing
for(i in seq_len(nrow(annotation_data))) {
  if (i <= 4){
    hjust <- -0.15
    vjust <- 0.1
  } else {
    hjust <- 0.1
    vjust <- -0.15
  }
  p1.1 <- p1.1 + annotate("text",
                          x = annotation_data$x.1[i],
                          y = annotation_data$x.2[i],
                          label = TeX(as.character(annotation_data$label[i])),
                          hjust = hjust,
                          vjust = vjust,
                          size = 6,
                          color = "black",
                          fontface = "bold")}

# Arrow pointing to point 21
p1.1 <- p1.1 +  geom_segment(
  aes(x = x_21 - 8, y = y_21 - 8, xend = x_21, yend = y_21),
  arrow = arrow(length = unit(0.2, "cm")),
  color = "black",
  size = 1
) +
  
  # Label next to the arrow
  annotate(
    "text",
    x = x_21 - 10,
    y = y_21 - 12,
    label = TeX("$\\textbf{b}_{21}$"),
    color = "black",
    size = 6,
    fontface = "bold"
  ) +
  
  annotate(
    "text",
    x = eval$x.1[40] + 10,
    y = eval$x.2[40] - 5,
    label = "Boundary",
    color = "black",
    size = 6,
    fontface = "bold"
  ) +
  
  labs(x = "Saber 11 Score", y = "Sisben Score") +
  coord_cartesian(xlim = c(-80, 100), ylim = c(-40, 60)) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    plot.title   = element_text(size = 20, hjust = 0.5),
    text = element_text(family="Times New Roman", face="bold"),
    axis.text.x  = element_text(face = "bold", size = 12),
    axis.text.y  = element_text(face = "bold", size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.2, 0.8),
    legend.background = element_rect(fill = "white", color = "black", linetype = "solid")
  ) +
  xlab("Saber 11") +
  ylab("Sisben")

print(p1.1)

ggsave("Results/fig1a.png", p1.1, width = 6, height = 5)

############################# SPP: Using Bivariate Method ######################

Y <- data$y
X <- cbind(data$x.1, data$x.2)
t <- data$d
b <- eval
result.rd2d <- rd2d(Y, X, t, b)
summary(result.rd2d, CBuniform = TRUE, subset = c(1,5,10,15,21,25,30,35,40))
tau.hat <- result.rd2d$results$Est.p
tau.hat.biv <- tau.hat
CI.lower.biv <- result.rd2d$results$CI.lower
CI.upper.biv <- result.rd2d$results$CI.upper
CB.lower.biv <- result.rd2d$results$CB.lower
CB.upper.biv <- result.rd2d$results$CB.upper
  
############################# BATE #############################################

summary(result.rd2d, WBATE = rep(1, neval))
tau.WBATE <- rep(0.2806, neval) # read from summary output

################################## LBATE #########################################

summary(result.rd2d, LBATE = TRUE)
tau.extreme <- max(tau.hat)
tau.extreme <- rep(tau.extreme, neval)
eval.max <- which.max(tau.hat)
print(eval.max)

################################## Table #######################################

summary(result.rd2d, CBuniform = TRUE, subset = c(1,5,10,15,20,25,30,35,40), LBATE = TRUE, WBATE = rep(1,neval))

rd2d_summary_to_latex <- function(result_rd2d,
                                  subset = c(1,5,10,15,20,25,30,35,40),
                                  CBu = TRUE, LBATE = TRUE, WBATE = rep(1, length(result_rd2d$opt$h01)),
                                  caption = "Aggregated Treatment Effect Analysis Along the Boundary.",
                                  label = "tab:emp") {
  # Run the same summary call you used and capture its lines
  s <- capture.output(summary(result_rd2d,
                              CBu = CBu, CBuuniform = CBu, # tolerate either spelling
                              CBuniform = CBu,
                              subset = subset,
                              LBATE = LBATE,
                              WBATE = WBATE))
  
  # helper: trim
  trim <- function(x) sub("^\\s+|\\s+$", "", x)
  
  # parse rows 
  row_re <- "^\\s*(\\d+)\\s+(-?\\d+\\.?\\d*|NA)\\s+(-?\\d+\\.?\\d*|NA)\\s+(-?\\d+\\.?\\d*|NA)\\s+(-?\\d+\\.?\\d*|NA)\\s+(-?\\d+\\.?\\d*|NA)\\s+\\[\\s*(-?\\d+\\.?\\d*|NA)\\s*,\\s*(-?\\d+\\.?\\d*|NA)\\s*\\]"
  rows_idx <- grep(row_re, s)
  if (length(rows_idx) == 0) stop("Could not find detail rows in the summary output.")
  
  mat <- do.call(rbind, lapply(s[rows_idx], function(line) {
    m <- regexec(row_re, line)
    as.vector(regmatches(line, m)[[1]])[-1]
  }))
  colnames(mat) <- c("ID","b1","b2","Est","z","p","CIlo","CIhi")
  df <- as.data.frame(mat, stringsAsFactors = FALSE)
  num_cols <- c("ID","b1","b2","Est","z","p","CIlo","CIhi")
  df[num_cols] <- lapply(df[num_cols], function(v) as.numeric(v))
  
  # parse WBATE (if present) 
  WBATE_re <- "^\\s*WBATE\\s+(-?\\d+\\.?\\d*|NA)\\s+(-?\\d+\\.?\\d*|NA)\\s+(-?\\d+\\.?\\d*|NA)\\s+\\[\\s*(-?\\d+\\.?\\d*|NA)\\s*,\\s*(-?\\d+\\.?\\d*|NA)\\s*\\]"
  WB <- NULL
  if (any(grepl(WBATE_re, s))) {
    wline <- s[grep(WBATE_re, s)[1]]
    m <- regexec(WBATE_re, wline)
    WB <- as.numeric(regmatches(wline, m)[[1]][-1])
    names(WB) <- c("est","z","p","CIlo","CIhi")
  }
  
  # parse LBATE (if present) 
  LBATE_re <- "^\\s*LBATE\\s+(-?\\d+\\.?\\d*|NA)\\s+\\[\\s*(-?\\d+\\.?\\d*|NA)\\s*,\\s*(-?\\d+\\.?\\d*|NA)\\s*\\]"
  L <- NULL
  if (any(grepl(LBATE_re, s))) {
    lline <- s[grep(LBATE_re, s)[1]]
    m <- regexec(LBATE_re, lline)
    L <- as.numeric(regmatches(lline, m)[[1]][-1])
    names(L) <- c("est","CIlo","CIhi")
  }
  
  # build h_MSE strings from the object
  hfmt <- function(i) {
    h01 <- result_rd2d$opt$h01[i]; h02 <- result_rd2d$opt$h02[i]
    h11 <- result_rd2d$opt$h11[i]; h12 <- result_rd2d$opt$h12[i]
    if (any(is.na(c(h01,h02,h11,h12)))) return("")
    sprintf("(%.1f,%.1f)/(%.1f,%.1f)", h01, h02, h11, h12)
  }
  df$hMSE <- vapply(df$ID, hfmt, character(1))
  
  # format LaTeX lines 
  fmt <- function(x, d) ifelse(is.na(x), "", sprintf(paste0("%.", d, "f"), x))
  lines <- paste0(
    "   $\\widehat{\\tau}(\\bb_{", df$ID, "})$ & ",
    ifelse(df$hMSE == "", " ", df$hMSE), " & ",
    fmt(df$Est, 4), " & ",
    fmt(df$z, 4), " & ",
    fmt(df$p, 4), " & (",
    fmt(df$CIlo, 4), ", ", fmt(df$CIhi, 4), ")\\\\"
  )
  
  # WBATE / LBATE LaTeX 
  wb_line <- if (!is.null(WB)) {
    paste0("   $\\widehat{\\tau}_{\\mathsf{B}}$ &  & ",
           fmt(WB['est'], 4), " & ",
           fmt(WB['z'], 4), " & ",
           fmt(WB['p'], 4), " & (",
           fmt(WB['CIlo'], 4), ", ", fmt(WB['CIhi'], 4), ")\\\\")
  } else ""
  
  lte_line <- if (!is.null(L)) {
    paste0("   $\\widehat{\\tau}_{\\max}$ &  & ",
           fmt(L['est'], 4), " &  &  & (",
           fmt(L['CIlo'], 4), ", ", fmt(L['CIhi'], 4), ")\\\\")
  } else ""
  
  # assemble full table 
  tab <- c(
    "\\begin{table}[ht]",
    "\\centering",
    "\\begin{tabular}{lccccc}",
    "  \\toprule",
    "  Method & $h_\\mathtt{MSE}$ & Estimate & Z value & $p$-value & CI \\\\ ",
    "  \\midrule",
    "  \\midrule",
    lines,
    "  \\midrule",
    wb_line,
    "  \\midrule",
    lte_line,
    "  \\bottomrule",
    "\\end{tabular}",
    sprintf("\\caption{%s}", caption),
    sprintf("\\label{%s}", label),
    "\\end{table}"
  )
  
  cat(paste(tab, collapse = "\n"))
  invisible(tab)
}

rd2d_summary_to_latex(result.rd2d)

############################# SPP: Point Estimation ############################

indx <- c(1:neval)
df <- data.frame(
  indx = rep(indx, 3),
  y = c(tau.hat, tau.WBATE, tau.extreme),
  label = rep(c("BATEC","WBATE", "LBATE"), each = length(indx))
)

#### old code

temp_plot <- ggplot() + theme_bw()
temp_plot <- temp_plot + geom_point(data = df[df$label == "BATEC", ], aes(x = indx, y = y, color = label, shape = label, linetype = label))
temp_plot <- temp_plot + geom_line(data = df[df$label == "WBATE", ],
                                   aes(x = indx, y = y, color = label, shape = label, linetype = label),
                                   size = 0.5, show.legend = TRUE)
temp_plot <- temp_plot + geom_line(data = df[df$label == "LBATE", ],
                                   aes(x = indx, y = y, color = label, shape = label, linetype = label),
                                   size = 0.5, show.legend = TRUE)
temp_plot <- temp_plot + theme_minimal() +
  theme(
    axis.title.x = element_text(size = 15,face = "bold"),  # X-axis label size
    axis.title.y = element_text(size = 15,face = "bold"),  # Y-axis label size
    plot.title = element_text(size = 20, hjust = 0.5),  # Title size and centering
    text=element_text(family="Times New Roman", face="bold"),
    axis.text.x = element_text(face = "bold", size = 15),
    axis.text.y = element_text(face = "bold", size = 12),
    legend.position = c(0.8, 1),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", colour = NA),  # White background, no border
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

legend_order <- c("BATEC","WBATE", "LBATE")

temp_plot <- temp_plot + scale_color_manual(
  values = c("BATEC" = "black", "WBATE" = "blue", "LBATE" = "red"),
  name = NULL, breaks = legend_order
) + scale_shape_manual(
  values = c("BATEC" = 16, "WBATE" = NA, "LBATE" = NA),  # 16 = filled circle, 17 = triangle
  name = NULL, breaks = legend_order
) + scale_linetype_manual(
  values = c("BATEC" = 0, "WBATE" = 3, "LBATE" = 6),
  name = NULL, breaks = legend_order) +
  guides(
    color = guide_legend(order = 1),  # Ensure everything is in one group
    shape = guide_legend(order = 1),   # Align shapes with color
    linetype = guide_legend(order = 1)
  )

temp_plot <- temp_plot + geom_vline(xintercept = c(21), color = "lightgrey", size = 1, linetype = "dotted")

temp_plot <- temp_plot +
  annotate(
    "text",
    x = 19,                # Same x as the vertical line
    y = 0.15,               # Adjust this so it appears where you want
    label = "kink",
    color = "dimgrey",
    size = 4,              # Text size
    vjust = -0.5,          # Vertical justification (pulls text below this y-value)
    fontface = "bold"
  )

temp_plot <- temp_plot + xlab("Cutoffs on the Boundary") + ylab("Treatment Effect")

# temp_plot <- temp_plot + geom_vline(xintercept = c(11), color = "red", size = 1, linetype = "dotted")

temp_plot <- temp_plot + scale_x_continuous(
  breaks = c(1,5,10,15,21,25,30,35,40),
  labels = c(TeX("$\\textbf{b}_{1}$"), TeX("$\\textbf{b}_{5}$"), TeX("$\\textbf{b}_{10}$"), 
             TeX("$\\textbf{b}_{15}$"),TeX("$\\textbf{b}_{21}$"), TeX("$\\textbf{b}_{25}$"), 
             TeX("$\\textbf{b}_{30}$"), TeX("$\\textbf{b}_{35}$"),TeX("$\\textbf{b}_{40}$"))
)

temp_plot <- temp_plot + coord_cartesian(xlim = c(1, 40),ylim = c(0.15,0.4)) # ylim depends on confidence bands

# custom y-ticks for WBATE and max
wb <- tau.WBATE[1]; mx <- tau.extreme[1]; tol <- 1e-10
base_breaks <- seq(0.15, 0.40, by = 0.05)
y_breaks <- sort(unique(c(base_breaks, wb, mx)))

temp_plot <- temp_plot +
  scale_y_continuous(
    breaks = y_breaks,
    labels = function(x) {
      labs <- sapply(x, function(v) {
        if (abs(v - wb) < tol) {
          "bold(tau[WBATE])"
        } else if (abs(v - mx) < tol) {
          "bold(tau[max])"
        } else {
          sprintf("bold('%.2f')", v)  # bold plain text for numbers
        }
      })
      parse(text = labs)
    }
  ) 
# +
#   theme(
#     axis.text.y  = element_text(face = "bold", size = 12, colour = "black"),
#     axis.ticks.y = element_line(colour = "black")
#   )


# Print the plot
print(temp_plot)

ggsave("Results/fig1b.png", temp_plot, width = 6, height = 5)

############################# SPP: Confidence Bands ############################

indx <- c(1:neval)

# Create a data frame for plotting
bound <- 40

df <- data.frame( indx = indx, y = tau.hat.biv, label = rep(c("BATEC"), each = length(indx)))

# Build the plot
temp_plot <- ggplot() + theme_bw()
df <- df[df$indx <= bound,]

# Scatter plot for "BATEC" and "Distance"
temp_plot <- temp_plot + geom_point(data = df,
                                    aes(x = indx, y = y, color = label, shape = label, fill = label, linetype = label))

df_ribbon <- data.frame(
  indx = indx[c(1:bound)],
  ymin = CB.lower.biv[c(1:bound)],
  ymax = CB.upper.biv[c(1:bound)],
  label = "CB" 
)

temp_plot <- temp_plot + geom_ribbon(data = df_ribbon, aes(x = indx, ymin = ymin, ymax = ymax,
                                                           color = label, shape = label, fill = label, linetype = label), alpha = 0.1)

df_errorbar <- data.frame(
  indx = indx[c(1:bound)],
  ymin = CI.lower.biv[c(1:bound)],
  ymax = CI.upper.biv[c(1:bound)],
  label = "CI" 
)

temp_plot <- temp_plot + geom_errorbar(data = df_errorbar, aes(x = indx, ymin = ymin, ymax = ymax,
                                                               color = label, shape = label, fill = label, linetype = label))

temp_plot <- temp_plot + xlab("Cutoffs on the Boundary") + ylab("Treatment Effect")

legend_order <- c("BATEC", "CI", "CB")

temp_plot <- temp_plot + scale_color_manual(
  values = c("BATEC" = "black", "CI" = "black","CB" = "dodgerblue4"),
  name = NULL, breaks = legend_order
) + scale_shape_manual(
  values = c("BATEC" = 16, "CI" = 124,"CB" = 0),  # 16 = filled circle, 17 = triangle
  name = NULL, breaks = legend_order
) + scale_fill_manual(
  values = c("BATEC" = NA, "CI" = NA, "CB" = "dodgerblue4"),
  name = NULL, breaks = legend_order
) + scale_linetype_manual(
  values = c("BATEC" = 0, "CI" = 5,"CB" = 0),
  name = NULL, breaks = legend_order) +
  guides(
    fill = guide_legend(order = 1),  # Keep "CB" first
    color = guide_legend(order = 1),  # Ensure everything is in one group
    shape = guide_legend(order = 1),   # Align shapes with color
    linetype = guide_legend(order = 1)
  )

temp_plot <- temp_plot + geom_vline(xintercept = c(21), color = "lightgrey", size = 1, linetype = "dotted")

temp_plot <- temp_plot +
  annotate(
    "text",
    x = 19,                # Same x as the vertical line
    y = 0.05,               # Adjust this so it appears where you want
    label = "kink",
    color = "dimgrey",
    size = 4,              # Text size
    vjust = -0.5,          # Vertical justification (pulls text below this y-value)
    fontface = "bold"
  )

# Place legend inside and adjust text sizes
temp_plot <- temp_plot + theme_minimal() +
  theme(
    axis.title.x = element_text(size = 15,face = "bold"),  # X-axis label size
    axis.title.y = element_text(size = 15,face = "bold"),  # Y-axis label size
    plot.title = element_text(size = 20, hjust = 0.5),  # Title size and centering
    text=element_text(family="Times New Roman", face="bold"),
    axis.text.x = element_text(face = "bold",
                               size = 15),
    axis.text.y = element_text(face = "bold",
                               size = 12),
    legend.position = c(0.8, 1),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", colour = NA),  # White background, no border
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

temp_plot <- temp_plot + scale_x_continuous(
  breaks = c(1,5,10,15,21,25,30,35,40),
  labels = c(TeX("$\\textbf{b}_{1}$"), TeX("$\\textbf{b}_{5}$"), TeX("$\\textbf{b}_{10}$"), 
             TeX("$\\textbf{b}_{15}$"),TeX("$\\textbf{b}_{21}$"), TeX("$\\textbf{b}_{25}$"), 
             TeX("$\\textbf{b}_{30}$"), TeX("$\\textbf{b}_{35}$"), TeX("$\\textbf{b}_{40}$"))
)

temp_plot <- temp_plot + coord_cartesian(xlim = c(1, 40), ylim = c(0.05, 0.55))

# Print the plot
print(temp_plot)

ggsave("Results/fig2a.png", temp_plot, width = 6, height = 5)

############################ SPP: heatmap ######################################

# heat map for treatment effect

data.plot <- cbind(eval, tau.hat.biv)
colnames(data.plot) <- c("x.1", "x.2", "tau.hat")

# Function to interpolate points and color values between two consecutive points
interpolate_points <- function(df, n=ninter){
  do.call(rbind, lapply(1:(nrow(df)-1), function(i){
    xseq <- seq(df$x.1[i], df$x.1[i+1], length.out = n+2)[2:(n+1)]
    yseq <- seq(df$x.2[i], df$x.2[i+1], length.out = n+2)[2:(n+1)]
    colorseq <- seq(df$tau.hat[i], df$tau.hat[i+1], length.out = n+2)[2:(n+1)]
    data.frame(x.1 = xseq, x.2 = yseq, tau.hat = colorseq)
  }))
}

# Generating interpolated points
ninter <- 10
interpolated_data <- interpolate_points(data.plot)

# Plotting

heat_wd <- 6.5
heatcol_low <- "blue"
heatcol_mid <- "white"
heatcol_high <- "red"


heat_lab <- TeX("BATEC")
heat_title <- NULL
xlabel <- "Saber11"
ylabel <- "Sisben"

augmented_data <-rbind(data.plot, interpolated_data)
ord <- order(augmented_data[,1], augmented_data[,2])
augmented_data <- augmented_data[ord,]


vmax <- max(augmented_data$tau.hat, na.rm = TRUE)
vmin <- -0.15
mid  <- min(augmented_data$tau.hat, na.rm = TRUE)          

if (is.null(heat_title)) heat_title <- "Heat Map"
plot_heat <- ggplot(data.plot, aes(x=x.1, y=x.2)) +
  geom_segment(data = augmented_data,
               aes(xend = lead(x.1, order_by=x.1),
                   yend = lead(x.2, order_by=x.2),
                   color = tau.hat),
               size = heat_wd, lineend = "round") +
  scale_color_gradient2(
    low=heatcol_low, 
    mid = heatcol_mid,
    high=heatcol_high,
    midpoint = mid,
    limits = c(vmin, vmax)) +
  labs(color = heat_lab) +
  xlab(xlabel) +
  ylab(ylabel)

plot_heat <- plot_heat + geom_text(data=data.plot, aes(x=x.1, y=x.2, label=sprintf("%02d", 1:nrow(data.plot))), color="black", size=3)

plot_heat <- plot_heat + theme_minimal() +
  theme(
    axis.title.x = element_text(size = 15,face = "bold"),  # X-axis label size
    axis.title.y = element_text(size = 15,face = "bold"),  # Y-axis label size
    plot.title = element_text(size = 20, hjust = 0.5),  # Title size and centering
    text=element_text(family="Times New Roman", face="bold"),
    axis.text.x = element_text(face = "bold",
                               size = 15),
    axis.text.y = element_text(face = "bold",
                               size = 12),
    legend.position = c(0.8, 1),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", colour = NA),  # White background, no border
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) + coord_fixed(ratio = 56/40, xlim = c(-10, 64), ylim = c(-10,42))

print(plot_heat)

ggsave("Results/fig2b.png", plot_heat, width = 6, height = 5)

# heatmap for p-value

data.plot$p.value <- result.rd2d$results$`P>|z|`
data.plot$p.sig <- cut(data.plot$p.value,
                       breaks = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       labels = c("p < 0.001", "0.001 ≤ p < 0.01", "0.01 ≤ p < 0.05", "0.05 ≤ p < 0.1", "p ≥ 0.1"))
sig_colors <- c("p < 0.001" = "#d73027",       # red
                "0.001 ≤ p < 0.01" = "#fc8d59", # orange
                "0.01 ≤ p < 0.05" = "#fee08b",  # yellow
                "0.05 ≤ p < 0.1" = "#d9ef8b",   # light green
                "p ≥ 0.1" = "#91cf60")          # green
library(ggplot2)

plot_heat_pvalue <- ggplot(data.plot, aes(x = x.1, y = x.2, fill = p.sig)) +
  geom_tile(color = "white",show.legend = TRUE) +
  scale_fill_manual(values = sig_colors, name = "P-value",drop = FALSE) +
  labs(x = "Saber11", y = "Sisben") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 15, face = "bold"),
    axis.title.y = element_text(size = 15, face = "bold"),
    plot.title = element_text(size = 20, hjust = 0.5),
    text = element_text(family = "Times New Roman", face = "bold"),
    axis.text.x = element_text(face = "bold", size = 15),
    axis.text.y = element_text(face = "bold", size = 12),
    legend.position = c(0.8, 1),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) 

plot_heat_pvalue <- plot_heat_pvalue +  coord_fixed(ratio = 56/40, xlim = c(-10, 64), ylim = c(-10,42))
plot_heat_pvalue <- plot_heat_pvalue + geom_text(data=data.plot, aes(x=x.1, y=x.2, label=sprintf("%02d", 1:nrow(data.plot))), color="black", size=3)

print(plot_heat_pvalue)

ggsave("Results/fig2c.png", plot_heat_pvalue, width = 6, height = 5)

############################# Placebo: Using Bivariate Method ##################

data_placebo <- read_dta("spp.dta")
covariate <- "icfes_educm1"
data_placebo <- data_placebo[c("running_saber11","running_sisben",covariate)] 
colnames(data_placebo) <- c("x.1", "x.2","y")
na.ok <- complete.cases(data_placebo)
data_placebo <- data_placebo[na.ok,]
data_placebo <- as.data.frame(data_placebo)
data_placebo$d <- as.integer(data_placebo$x.1 >= 0 & data_placebo$x.2 >= 0)

Y <- data_placebo$y
X <- cbind(data_placebo$x.1, data_placebo$x.2)
t <- data_placebo$d
b <- eval
placebo.rd2d <- rd2d(Y, X, t, b)
summary(placebo.rd2d)

tau.hat.biv <- placebo.rd2d$results$Est.p
CI.lower.biv <- placebo.rd2d$results$CI.lower
CI.upper.biv <- placebo.rd2d$results$CI.upper
CB.lower.biv <- placebo.rd2d$results$CB.lower
CB.upper.biv <- placebo.rd2d$results$CB.upper

############################# Placebo: Confidence Bands ############################

indx <- c(1:neval)

# Create a data frame for plotting
bound <- 40

df <- data.frame( indx = indx, y = tau.hat.biv, label = rep(c("BATEC"), each = length(indx)))

# Build the plot
temp_plot <- ggplot() + theme_bw()
df <- df[df$indx <= bound,]

# Scatter plot for "BATEC" and "Distance"
temp_plot <- temp_plot + geom_point(data = df,
                                    aes(x = indx, y = y, color = label, shape = label, fill = label, linetype = label))

df_ribbon <- data.frame(
  indx = indx[c(1:bound)],
  ymin = CB.lower.biv[c(1:bound)],
  ymax = CB.upper.biv[c(1:bound)],
  label = "CB" 
)

temp_plot <- temp_plot + geom_ribbon(data = df_ribbon, aes(x = indx, ymin = ymin, ymax = ymax,
                                                           color = label, shape = label, fill = label, linetype = label), alpha = 0.1)

df_errorbar <- data.frame(
  indx = indx[c(1:bound)],
  ymin = CI.lower.biv[c(1:bound)],
  ymax = CI.upper.biv[c(1:bound)],
  label = "CI" 
)

temp_plot <- temp_plot + geom_errorbar(data = df_errorbar, aes(x = indx, ymin = ymin, ymax = ymax,
                                                               color = label, shape = label, fill = label, linetype = label))

temp_plot <- temp_plot + xlab("Cutoffs on the Boundary") + ylab("Treatment Effect")

legend_order <- c("BATEC", "CI", "CB")

temp_plot <- temp_plot + scale_color_manual(
  values = c("BATEC" = "black", "CI" = "black","CB" = "dodgerblue4"),
  name = NULL, breaks = legend_order
) + scale_shape_manual(
  values = c("BATEC" = 16, "CI" = 124,"CB" = 0),  # 16 = filled circle, 17 = triangle
  name = NULL, breaks = legend_order
) + scale_fill_manual(
  values = c("BATEC" = NA, "CI" = NA, "CB" = "dodgerblue4"),
  name = NULL, breaks = legend_order
) + scale_linetype_manual(
  values = c("BATEC" = 0, "CI" = 5,"CB" = 0),
  name = NULL, breaks = legend_order) +
  guides(
    fill = guide_legend(order = 1),  # Keep "CB" first
    color = guide_legend(order = 1),  # Ensure everything is in one group
    shape = guide_legend(order = 1),   # Align shapes with color
    linetype = guide_legend(order = 1)
  )

temp_plot <- temp_plot + geom_vline(xintercept = c(21), color = "lightgrey", size = 1, linetype = "dotted")

temp_plot <- temp_plot +
  annotate(
    "text",
    x = 19,                # Same x as the vertical line
    y = -0.16,               # Adjust this so it appears where you want
    label = "kink",
    color = "dimgrey",
    size = 4,              # Text size
    vjust = -0.5,          # Vertical justification (pulls text below this y-value)
    fontface = "bold"
  )

# Place legend inside and adjust text sizes
temp_plot <- temp_plot + theme_minimal() +
  theme(
    axis.title.x = element_text(size = 15,face = "bold"),  # X-axis label size
    axis.title.y = element_text(size = 15,face = "bold"),  # Y-axis label size
    plot.title = element_text(size = 20, hjust = 0.5),  # Title size and centering
    text=element_text(family="Times New Roman", face="bold"),
    axis.text.x = element_text(face = "bold",
                               size = 15),
    axis.text.y = element_text(face = "bold",
                               size = 12),
    legend.position = c(0.8, 1),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", colour = NA),  # White background, no border
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

temp_plot <- temp_plot + scale_x_continuous(
  breaks = c(1,5,10,15,21,25,30,35,40),
  labels = c(TeX("$\\textbf{b}_{1}$"), TeX("$\\textbf{b}_{5}$"), TeX("$\\textbf{b}_{10}$"), 
             TeX("$\\textbf{b}_{15}$"),TeX("$\\textbf{b}_{21}$"), TeX("$\\textbf{b}_{25}$"), 
             TeX("$\\textbf{b}_{30}$"), TeX("$\\textbf{b}_{35}$"), TeX("$\\textbf{b}_{40}$"))
)

temp_plot <- temp_plot + coord_cartesian(xlim = c(1, 40), ylim = c(-0.24, 0.24))

# Print the plot
print(temp_plot)

ggsave("Results/fig3a.png", temp_plot, width = 6, height = 5)

######################### Placebo: heatmap #####################################

# heat map for treatment effect

data.plot <- cbind(eval, tau.hat.biv)
colnames(data.plot) <- c("x.1", "x.2", "tau.hat")

# Function to interpolate points and color values between two consecutive points
interpolate_points <- function(df, n=ninter){
  do.call(rbind, lapply(1:(nrow(df)-1), function(i){
    xseq <- seq(df$x.1[i], df$x.1[i+1], length.out = n+2)[2:(n+1)]
    yseq <- seq(df$x.2[i], df$x.2[i+1], length.out = n+2)[2:(n+1)]
    colorseq <- seq(df$tau.hat[i], df$tau.hat[i+1], length.out = n+2)[2:(n+1)]
    data.frame(x.1 = xseq, x.2 = yseq, tau.hat = colorseq)
  }))
}

# Generating interpolated points
ninter <- 10
interpolated_data <- interpolate_points(data.plot)

# Plotting

heat_wd <- 6.5
heatcol_low <- "blue"
heatcol_mid <- "white"
heatcol_high <- "red"
heat_lab <- TeX("BATEC")
heat_title <- NULL
xlabel <- "Saber11"
ylabel <- "Sisben"

augmented_data <-rbind(data.plot, interpolated_data)
ord <- order(augmented_data[,1], augmented_data[,2])
augmented_data <- augmented_data[ord,]

vmax <- 0.3361467
vmin <- -0.15
mid  <- 0.1833615 

if (is.null(heat_title)) heat_title <- "Heat Map"
plot_heat <- ggplot(data.plot, aes(x=x.1, y=x.2)) +
  geom_segment(data = augmented_data,
               aes(xend = lead(x.1, order_by=x.1),
                   yend = lead(x.2, order_by=x.2),
                   color = tau.hat),
               size = heat_wd, lineend = "round") +
  scale_color_gradient2(
    low=heatcol_low, 
    mid = heatcol_mid,
    high=heatcol_high,
    midpoint = mid,
    limits = c(vmin, vmax)) +
  labs(color = heat_lab) +
  xlab(xlabel) +
  ylab(ylabel)

plot_heat <- plot_heat + geom_text(data=data.plot, aes(x=x.1, y=x.2, label=sprintf("%02d", 1:nrow(data.plot))), color="black", size=3)

plot_heat <- plot_heat + theme_minimal() +
  theme(
    axis.title.x = element_text(size = 15,face = "bold"),  # X-axis label size
    axis.title.y = element_text(size = 15,face = "bold"),  # Y-axis label size
    plot.title = element_text(size = 20, hjust = 0.5),  # Title size and centering
    text=element_text(family="Times New Roman", face="bold"),
    axis.text.x = element_text(face = "bold",
                               size = 15),
    axis.text.y = element_text(face = "bold",
                               size = 12),
    legend.position = c(0.8, 1),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", colour = NA),  # White background, no border
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) + coord_fixed(ratio = 56/40, xlim = c(-10, 64), ylim = c(-10,42))

print(plot_heat)

ggsave("Results/fig3b.png", plot_heat, width = 6, height = 5)

# heatmap for p-value

data.plot$p.value <- placebo.rd2d$results$`P>|z|`
data.plot$p.sig <- cut(data.plot$p.value,
                       breaks = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       labels = c("p < 0.001", "0.001 ≤ p < 0.01", "0.01 ≤ p < 0.05", "0.05 ≤ p < 0.1", "p ≥ 0.1"))
sig_colors <- c("p < 0.001" = "#d73027",       # red
                "0.001 ≤ p < 0.01" = "#fc8d59", # orange
                "0.01 ≤ p < 0.05" = "#fee08b",  # yellow
                "0.05 ≤ p < 0.1" = "#d9ef8b",   # light green
                "p ≥ 0.1" = "#91cf60")          # green
library(ggplot2)

plot_heat_pvalue <- ggplot(data.plot, aes(x = x.1, y = x.2, fill = p.sig)) +
  geom_tile(color = "white", show.legend = TRUE) +
  scale_fill_manual(values = sig_colors, name = "P-value", drop = FALSE) +
  labs(x = "Saber11", y = "Sisben") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 15, face = "bold"),
    axis.title.y = element_text(size = 15, face = "bold"),
    plot.title = element_text(size = 20, hjust = 0.5),
    text = element_text(family = "Times New Roman", face = "bold"),
    axis.text.x = element_text(face = "bold", size = 15),
    axis.text.y = element_text(face = "bold", size = 12),
    legend.position = c(0.8, 1),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

plot_heat_pvalue <- plot_heat_pvalue +  coord_fixed(ratio = 56/40, xlim = c(-10, 64), ylim = c(-10,42))
plot_heat_pvalue <- plot_heat_pvalue + geom_text(data=data.plot, aes(x=x.1, y=x.2, label=sprintf("%02d", 1:nrow(data.plot))), color="black", size=3)

print(plot_heat_pvalue)

ggsave("Results/fig3c.png", plot_heat_pvalue, width = 6, height = 5)



