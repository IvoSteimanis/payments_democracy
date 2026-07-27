# ==============================================================================
# SCRIPT: 08_transition_sankey.R
# PURPOSE: Alluvial (Sankey) diagram showing leader rule choice transitions
#          from baseline (Round 1, no payment) to first bonus round.
# REQUIRES: haven, ggalluvial, ggplot2, dplyr, tidyr
# OUTPUT: results/figures/figureS4_transition_sankey.png
# ==============================================================================

# --- Packages -----------------------------------------------------------------
# Required packages are checked, never installed: a replication script must not
# silently modify the replicator's R library. See README for versions.
.req <- c("haven", "ggalluvial", "dplyr", "ggplot2", "tidyr")
.missing <- .req[!vapply(.req, requireNamespace, logical(1), quietly = TRUE)]
if (length(.missing)) {
  stop("Missing R packages: ", paste(.missing, collapse = ", "),
       "
Install them with: install.packages(c(",
       paste(sprintf('"%s"', .missing), collapse = ", "), "))")
}

library(haven)
library(dplyr)
library(ggplot2)
library(ggalluvial)
library(tidyr)

# --- Paths --------------------------------------------------------------------
# Walk up directory tree to find project root (identified by run.do)
find_root <- function() {
  dir <- getwd()
  while (dir != dirname(dir)) {
    if (file.exists(file.path(dir, "run.do"))) return(dir)
    dir <- dirname(dir)
  }
  stop("Could not find project root (looking for run.do). ",
       "Set working directory to the replication_package folder.")
}
base_path <- find_root()
data_path <- file.path(base_path, "processed", "analysis_wide.dta")
out_path  <- file.path(base_path, "results", "figures", "figureS4_transition_sankey.png")

# --- Load data ----------------------------------------------------------------
df <- read_dta(data_path)
leaders <- df %>% filter(id <= 64)
stopifnot(nrow(leaders) == 64)

# --- Construct transition data ------------------------------------------------
rule_labels <- c("Democratic", "Pseudo-democratic", "Autocratic")

# Which bonus round to compare against the baseline.
#
# The SOM caption for Figure S4 says "first bonus round". Read literally that would be
# round 5, which is chronologically first for every leader; the order randomisation only
# determines whether round 5 carried the small or the large bonus. The published figure
# is in fact built on the SMALL bonus round: rule5 for leaders randomised to small-first
# (bonus_order == 1) and rule6 for leaders randomised to large-first (bonus_order == 0).
# That is the definition reproduced here, because the package must reproduce the accepted
# figure. Using round 5 for everyone instead yields 5 (not 6) autocrats switching to
# democracy and 8 (not 9) democrats switching to pseudo-democracy.
leaders <- leaders %>%
  mutate(rule_bonus_first = if_else(bonus_order == 1, rule5, rule6))

transitions <- leaders %>%
  mutate(
    Baseline    = factor(rule_base, levels = 1:3, labels = rule_labels),
    First_Bonus = factor(rule_bonus_first, levels = 1:3, labels = rule_labels)
  ) %>%
  count(Baseline, First_Bonus, name = "Freq", .drop = FALSE)

stopifnot(sum(transitions$Freq) == 64)

cat("\n=== Transition Matrix ===\n")
mat <- pivot_wider(transitions, names_from = First_Bonus,
                   values_from = Freq, values_fill = 0)
print(mat)

# --- Stratum totals -----------------------------------------------------------
bl_totals <- transitions %>% group_by(Baseline) %>% summarise(N = sum(Freq))
fb_totals <- transitions %>% group_by(First_Bonus) %>% summarise(N = sum(Freq))

# --- Stacking positions (from ggplot_build: bottom to top = Auto, Pseudo, Dem) -
# ggalluvial reverses factor order: last level at bottom, first at top
bl_stack <- bl_totals %>%
  arrange(desc(match(Baseline, rule_labels))) %>%
  mutate(ymax = cumsum(N), ymin = ymax - N, ymid = (ymin + ymax) / 2)

fb_stack <- fb_totals %>%
  arrange(desc(match(First_Bonus, rule_labels))) %>%
  mutate(ymax = cumsum(N), ymin = ymax - N, ymid = (ymin + ymax) / 2)

# --- Color scheme -------------------------------------------------------------
bar_colors <- c(
  "Democratic"        = "#2166AC",
  "Pseudo-democratic" = "#E08214",
  "Autocratic"        = "#B2182B"
)

# --- Build base plot ----------------------------------------------------------
p <- ggplot(transitions %>% filter(Freq > 0),
            aes(y = Freq, axis1 = Baseline, axis2 = First_Bonus)) +
  geom_alluvium(aes(fill = Baseline), width = 1/5, alpha = 0.5,
                curve_type = "sigmoid") +
  geom_stratum(aes(fill = after_stat(stratum)), width = 1/5,
               color = "white", linewidth = 0.4) +
  # Category labels + N INSIDE the colored bars
  geom_text(stat = "stratum",
            aes(label = ifelse(
              after_stat(count) >= 9,
              paste0(c("Democratic" = "Democratic",
                        "Pseudo-democratic" = "Pseudo-\ndemocratic",
                        "Autocratic" = "Autocratic")[as.character(after_stat(stratum))],
                     "\nN=", after_stat(count)),
              "")),
            size = 3.0, color = "white", fontface = "bold",
            lineheight = 0.85) +
  scale_fill_manual(values = bar_colors, guide = "none") +
  scale_x_discrete(limits = c("Baseline\n(no payment)",
                               "First bonus\n(with payment)"),
                   expand = c(0.35, 0.05)) +
  scale_y_continuous(expand = c(0.02, 0)) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y      = element_blank(),
    axis.ticks       = element_blank(),
    axis.title       = element_blank(),
    panel.grid       = element_blank(),
    plot.title       = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle    = element_text(size = 10, hjust = 0.5, color = "grey40"),
    axis.text.x      = element_text(size = 11, face = "bold"),
    plot.margin      = margin(10, 60, 10, 60)
  ) +
  labs(
    title    = "Transition of leaders' rule choices",
    subtitle = "Baseline (Round 1) to first bonus round (N = 64 leaders)"
  )

# --- Small-bar label outside (Autocratic N=2 too small for text inside) -------
p <- p +
  annotate("text", x = 2.18,
           y = fb_stack$ymid[fb_stack$First_Bonus == "Autocratic"],
           label = "Autocratic\nN=2",
           size = 2.4, hjust = 0, color = "grey30", fontface = "plain",
           lineheight = 0.85, vjust = -0.5)

# --- Flow labels: positions extracted from ggplot_build() ---------------------
# Actual stacking (bottom to top): Autocratic, Pseudo-dem, Democratic
#
# From ggplot_build, each flow's REAL start/end y-bands:
#   Dem->Dem(31):       start [33,64]  end [33,64]  → start_mid=48.5, end_mid=48.5
#   Dem->Pseudo(9):     start [24,33]  end [15,24]  → start_mid=28.5, end_mid=19.5
#   Pseudo->Dem(3):     start [20,23]  end [30,33]  → start_mid=21.5, end_mid=31.5
#   Pseudo->Pseudo(10): start [10,20]  end [5,15]   → start_mid=15.0, end_mid=10.0
#   Auto->Dem(6):       start [3,9]    end [24,30]  → start_mid=6.0,  end_mid=27.0
#   Auto->Pseudo(3):    start [0,3]    end [2,5]    → start_mid=1.5,  end_mid=3.5
#
# Sigmoid position at fraction t: y = start_mid + sigmoid(t) * (end_mid - start_mid)
# Labels placed at different x to spread crossing flows:

# The x/y coordinates below are hand-tuned layout, but the LABELS are looked up from
# `transitions` so they can never drift from the data. The six labelled flows are the
# ones large enough to annotate; the two single-leader flows are left unlabelled.
flow_of <- function(from, to) {
  v <- transitions$Freq[transitions$Baseline == from & transitions$First_Bonus == to]
  if (length(v) == 0) 0L else v
}

flow_label_data <- data.frame(
  x     = c(1.50, 1.35, 1.65, 1.60, 1.60, 1.40),
  y     = c(48.5, 27.0, 30.5, 12.5, 23.0,  2.5),
  label = as.character(c(
    flow_of("Democratic",        "Democratic"),
    flow_of("Democratic",        "Pseudo-democratic"),
    flow_of("Pseudo-democratic", "Democratic"),
    flow_of("Pseudo-democratic", "Pseudo-democratic"),
    flow_of("Autocratic",        "Democratic"),
    flow_of("Autocratic",        "Pseudo-democratic")
  )),
  stringsAsFactors = FALSE
)

p <- p +
  geom_text(data = flow_label_data,
            aes(x = x, y = y, label = label),
            inherit.aes = FALSE,
            size = 3.2, fontface = "bold",
            color = "grey10")

# --- Save as PNG (using png()/dev.off() for reliable file closure) ------------
png(out_path, width = 9, height = 6, units = "in", res = 300, bg = "white")
print(p)
dev.off()
cat("\nFigure saved to:", out_path, "\n")

# --- Print table for manuscript -----------------------------------------------
cat("\n=== For manuscript table ===\n")
cat(sprintf("%-20s | %-12s | %-18s | %-12s | Total\n",
            "", "Democratic", "Pseudo-democratic", "Autocratic"))
cat(paste(rep("-", 75), collapse = ""), "\n")
for (i in 1:nrow(mat)) {
  cat(sprintf("%-20s | %12d | %18d | %12d | %5d\n",
              mat$Baseline[i],
              mat$Democratic[i],
              mat$`Pseudo-democratic`[i],
              mat$Autocratic[i],
              mat$Democratic[i] + mat$`Pseudo-democratic`[i] + mat$Autocratic[i]))
}
cat(paste(rep("-", 75), collapse = ""), "\n")
cat(sprintf("%-20s | %12d | %18d | %12d | %5d\n",
            "Total",
            sum(mat$Democratic), sum(mat$`Pseudo-democratic`), sum(mat$Autocratic), 64))

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
