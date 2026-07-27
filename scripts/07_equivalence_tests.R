# =============================================================================
# SCRIPT: 07_equivalence_tests.R
# PURPOSE: Produce Figure S2 — equivalence tests for baseline vs. bonus round
#          rule choices using the TOSTER package (Caldwell 2022).
#
# USAGE:  Run from replication_package/ after the Stata pipeline has generated
#         processed/analysis_long.dta.
#         Rscript scripts/07_equivalence_tests.R
#
# OUTPUT: results/figures/figureS2_equivalence_tests.png
# =============================================================================

# --- Setup -------------------------------------------------------------------
.req <- c("TOSTER", "haven", "dplyr", "ggplot2", "tidyr")
library(haven)
library(dplyr)
library(tidyr)
library(TOSTER)
library(ggplot2)

# Detect project root — walks up from getwd() or uses script location
find_root <- function() {
  target <- file.path("processed", "analysis_long.dta")

  # 1. Try script's own directory (works with Rscript --file=)
  file_arg <- grep("--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg) > 0) {
    d <- normalizePath(file.path(dirname(sub("--file=", "", file_arg[1])), ".."))
    if (file.exists(file.path(d, target))) return(d)
  }

  # 2. Walk up from getwd() (handles R GUI with any subdirectory open)
  d <- normalizePath(getwd())
  for (i in 1:6) {
    if (file.exists(file.path(d, target))) return(d)
    if (file.exists(file.path(d, "replication_package", target)))
      return(normalizePath(file.path(d, "replication_package")))
    parent <- dirname(d)
    if (parent == d) break
    d <- parent
  }

  stop("Cannot find processed/analysis_long.dta.\n",
       "  Set working directory first: setwd('.../replication_package')")
}
root <- find_root()
cat("Project root:", root, "\n")

# --- Color palette (matching run.do grstyle) ---------------------------------
# 1=Steel Blue (Democratic), 2=Warm Orange (Pseudo-dem), 3=Muted Red (Autocratic)
col_palette <- c(
  "Democratic"        = rgb( 57, 106, 177, maxColorValue = 255),  # #3966B1
  "Pseudo-Democratic" = rgb(218, 124,  48, maxColorValue = 255),  # #DA7C30
  "Autocratic"        = rgb(204,  37,  41, maxColorValue = 255)   # #CC2529
)

# --- Load data ---------------------------------------------------------------
df <- haven::read_dta(file.path(root, "processed", "analysis_long.dta"))

# Keep only leaders (id <= 64) and relevant decisions (1=baseline, 5/6=bonus)
df <- df %>%
  filter(id <= 64, decision %in% c(1, 5, 6))

# --- Compute paired differences ----------------------------------------------
# For each leader: baseline choice (decision 1) is 0 or 1 for each category.
# Bonus choice is the average across decisions 5 and 6 (gives 0, 0.5, or 1).
# Note: dem, fake, dic are 0/100 at this point (scaled in 03_analysis.do), but
# in the .dta saved by 02_generate.do they are 0/1. Check and handle both.

# Detect if values are 0/1 or 0/100
max_dem <- max(df$dem, na.rm = TRUE)
if (max_dem > 1) {
  df <- df %>%
    mutate(
      dem  = dem / 100,
      fake = fake / 100,
      dic  = dic / 100
    )
}

baseline <- df %>%
  filter(decision == 1) %>%
  select(id, dem_base = dem, fake_base = fake, dic_base = dic)

bonus <- df %>%
  filter(decision > 1) %>%
  group_by(id) %>%
  summarise(
    dem_bonus  = mean(dem, na.rm = TRUE),
    fake_bonus = mean(fake, na.rm = TRUE),
    dic_bonus  = mean(dic, na.rm = TRUE),
    .groups = "drop"
  )

paired <- inner_join(baseline, bonus, by = "id") %>%
  mutate(
    diff_dem  = dem_bonus  - dem_base,
    diff_fake = fake_bonus - fake_base,
    diff_dic  = dic_bonus  - dic_base
  )

# --- Equivalence bounds ------------------------------------------------------
# MDES from power analysis (Figure S1): 0.18 proportion points.
# This is conservative: it uses only the baseline-vs-first-bonus correlation
# (rho = 0.39). Pooling both bonus rounds raises rho to 0.45, which would
# lower MDES to ~0.177 — a negligible gain that does not change the bounds.
eqbound <- 0.18

# --- Run TOST on paired differences ------------------------------------------
tost_dem  <- t_TOST(x = paired$diff_dem,  eqb = eqbound, paired = FALSE,
                    hypothesis = "EQU", alpha = 0.05)
tost_fake <- t_TOST(x = paired$diff_fake, eqb = eqbound, paired = FALSE,
                    hypothesis = "EQU", alpha = 0.05)
tost_dic  <- t_TOST(x = paired$diff_dic,  eqb = eqbound, paired = FALSE,
                    hypothesis = "EQU", alpha = 0.05)

# --- Extract results for plotting -------------------------------------------
extract_tost <- function(tost_obj, label) {
  ci <- tost_obj$effsize
  # TOST$p.value has 3 values: [1] NHST, [2] lower bound, [3] upper bound.
  # The TOST decision uses the max of the two one-sided tests (indices 2:3).
  tost_pvals <- tost_obj$TOST$p.value
  data.frame(
    outcome    = label,
    estimate   = ci$estimate[1],
    ci_lower   = ci$lower.ci[1],
    ci_upper   = ci$upper.ci[1],
    p_tost     = max(tost_pvals[2], tost_pvals[3]),
    stringsAsFactors = FALSE
  )
}

results <- bind_rows(
  extract_tost(tost_dem,  "Democratic"),
  extract_tost(tost_fake, "Pseudo-Democratic"),
  extract_tost(tost_dic,  "Autocratic")
)

# Order factor levels for plotting
results$outcome <- factor(results$outcome,
                          levels = c("Autocratic", "Pseudo-Democratic", "Democratic"))

# --- Print results to console ------------------------------------------------
cat("\n=== Equivalence Test Results (bounds = +/-", eqbound, ") ===\n\n")
for (i in seq_len(nrow(results))) {
  cat(sprintf("%-20s: diff = %6.3f  [%6.3f, %6.3f]  p(TOST) = %.4f  %s\n",
              results$outcome[i],
              results$estimate[i],
              results$ci_lower[i],
              results$ci_upper[i],
              results$p_tost[i],
              ifelse(results$p_tost[i] < 0.05, "=> EQUIVALENT", "=> NOT equivalent")))
}
cat("\n")

# --- Figure S2: Equivalence test plot ----------------------------------------
p <- ggplot(results, aes(x = estimate, y = outcome, color = outcome)) +
  # Equivalence region
  annotate("rect",
           xmin = -eqbound, xmax = eqbound,
           ymin = -Inf, ymax = Inf,
           fill = "grey90", alpha = 0.5) +
  # Zero line
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60", linewidth = 0.5) +
  # Equivalence bounds
  geom_vline(xintercept = c(-eqbound, eqbound),
             linetype = "dotted", color = "grey50", linewidth = 0.4) +
  # 90% CI (standard for equivalence tests)
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper),
                 height = 0.2, linewidth = 1) +
  # Point estimates
  geom_point(size = 4, shape = 18) +
  scale_color_manual(values = col_palette) +
  scale_x_continuous(
    limits = c(-0.3, 0.3),
    breaks = seq(-0.3, 0.3, 0.1),
    labels = function(x) paste0(round(x * 100), "%")
  ) +
  labs(
    x = "Mean difference in percentage points (Bonus - Baseline)",
    y = NULL,
    title = NULL
  ) +
  annotate("text", x = 0, y = 0.55, label = "Equivalence region (\u00b118 pp)",
           size = 3.5, color = "grey40", fontface = "italic") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position    = "none",
    panel.background   = element_rect(fill = "white", color = NA),
    plot.background    = element_rect(fill = "white", color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(color = "grey92", linewidth = 0.3),
    axis.text.y  = element_text(size = 12),
    axis.text.x  = element_text(size = 10),
    axis.title.x = element_text(size = 11),
    plot.margin  = margin(10, 15, 10, 10)
  )

# --- Export as PNG ------------------------------------------------------------
outpath <- file.path(root, "results", "figures", "figureS2_equivalence_tests.png")
dir.create(dirname(outpath), showWarnings = FALSE, recursive = TRUE)

ggsave(outpath, plot = p, width = 7, height = 3.5, units = "in", dpi = 300,
       bg = "white")

cat("Figure saved to:", outpath, "\n")

# --- Recorded environment -----------------------------------------------------
# The published figure was generated with R 4.3.1 and:
#   TOSTER       0.8.6
#   haven        2.5.4
#   ggplot2      4.0.2
#   ggalluvial   0.12.6
#   dplyr        1.1.4
#   tidyr        1.3.1
# Print the versions actually in use so any divergence is visible in the log.
cat("
R:", R.version.string, "
")
for (.p in .req) cat(sprintf("  %-12s %s
", .p, as.character(packageVersion(.p))))
