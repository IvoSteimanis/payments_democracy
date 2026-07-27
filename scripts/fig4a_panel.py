"""Manuscript Figure 4 - Allocation outcomes by procedural rule.

Produces the single-panel dumbbell figure used as Figure 4 in the manuscript:
- Group majority (baseline villager majority), drawn as a horizontal reference line.
  The value is computed from the data, not hardcoded.
- Leader's own vote share by rule (orange diamonds)
- Implemented outcome share by rule (blue circles)

Under the democratic rule the outcome equals the group majority by construction (the
small positive gap comes from selection). Under the pseudo-democratic and autocratic
rules the outcome equals the leader's own vote exactly, because the leader overrides.

USAGE:   python scripts/fig4a_panel.py
         Must be run AFTER the Stata pipeline, which writes processed/analysis_long.dta.
READS:   processed/analysis_long.dta
WRITES:  results/figures/figure4_allocation_outcomes.png
REQUIRES: pandas, matplotlib (see requirements.txt for tested versions)

NOTE: 06_allocation_analysis.do produces an all-Stata two-panel alternative,
      results/figures/fig_stage2_combined_stata.png. That file is NOT the manuscript
      figure; the two used to share a filename and silently overwrite each other.
"""
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
import os

root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

# ===== Load data =====
long = pd.read_stata(os.path.join(root, "processed", "analysis_long.dta"))

baseline = long[long["round"] == 1].copy()
baseline["majority_egal"] = baseline["majority"].astype(str).str.contains("B", na=False).astype(int)
group_majority_egal = baseline["majority_egal"].mean() * 100
n_groups_egal = baseline["majority_egal"].sum()
n_groups = len(baseline)
print(f"Group majority preferring egalitarian (baseline): {group_majority_egal:.1f}% ({n_groups_egal}/{n_groups} groups)")

lv = long.copy()
lv["voted_egal"] = lv["vote"].astype(str).str.contains("B", na=False).astype(float)
lv["rule_str"] = lv["rule"].astype(str)
leader_pref = lv.groupby("rule_str")["voted_egal"].agg(["mean", "count"])
leader_pref["mean"] *= 100

lc = long.copy()
lc["payoff_egal"] = lc["payoff"].astype(str).str.contains("B", na=False).astype(float)
lc["rule_str"] = lc["rule"].astype(str)
outcome = lc.groupby("rule_str")["payoff_egal"].agg(["mean", "count"])
outcome["mean"] *= 100

print(f"\nLeader vote share (egalitarian) by rule:\n{leader_pref}")
print(f"\nOutcome share (egalitarian) by rule:\n{outcome}")

# ===== Build plot data =====
rules = ["Democratic", "Pseudo-democratic", "Autocratic"]
labels_with_n = [f"{r}\n(n={int(outcome.loc[r,'count'])} cells)" for r in rules]

majority_ref = group_majority_egal
leader_vals = [leader_pref.loc[r, "mean"] for r in rules]
outcome_vals = [outcome.loc[r, "mean"] for r in rules]

# ===== Plot =====
fig, ax = plt.subplots(figsize=(7.5, 3.8))

y_pos = [2, 1, 0]  # Democratic at top

# Reference line: group majority
ax.axvline(majority_ref, linestyle="--", color="#3E9651", linewidth=1.3, alpha=0.85, zorder=1)

for i, y in enumerate(y_pos):
    lv_val = leader_vals[i]
    out_val = outcome_vals[i]
    same_value = abs(lv_val - out_val) < 1

    if same_value:
        # Overlay: diamond BEHIND (larger), circle IN FRONT (smaller). Both at same y.
        ax.scatter(lv_val, y, color="#DA7C30", s=180, zorder=3, marker="D",
                   edgecolors="white", linewidths=1.0)
        ax.scatter(out_val, y, color="#396AB1", s=70, zorder=4,
                   edgecolors="white", linewidths=1.0)
        # SINGLE % label centered above
        ax.annotate(f"{out_val:.0f}%", (out_val, y), textcoords="offset points",
                    xytext=(0, 14), ha="center", va="center", fontsize=9,
                    color="#396AB1", fontweight="bold")
    else:
        # Separate markers — Democratic row
        ax.scatter(lv_val, y, color="#DA7C30", s=85, zorder=3, marker="D",
                   edgecolors="white", linewidths=0.8)
        ax.scatter(out_val, y, color="#396AB1", s=100, zorder=3,
                   edgecolors="white", linewidths=0.8)
        ax.annotate(f"{lv_val:.0f}%", (lv_val, y), textcoords="offset points",
                    xytext=(-10, 0), ha="right", va="center", fontsize=8.5,
                    color="#DA7C30", fontweight="bold")
        ax.annotate(f"{out_val:.0f}%", (out_val, y), textcoords="offset points",
                    xytext=(10, 0), ha="left", va="center", fontsize=8.5,
                    color="#396AB1", fontweight="bold")

    # Override arrow + gap label (skip if gap is small)
    gap = out_val - majority_ref
    if abs(gap) > 5:
        ax.annotate("", xy=(out_val, y), xytext=(majority_ref, y),
                    arrowprops=dict(arrowstyle="->", color="#CC2529", lw=1.5,
                                    shrinkA=2, shrinkB=10, alpha=0.7),
                    zorder=2)
        midpoint = (majority_ref + out_val) / 2
        ax.annotate(f"{gap:+.0f} pp", (midpoint, y), textcoords="offset points",
                    xytext=(0, 8), ha="center", fontsize=8.5,
                    color="#CC2529", fontweight="bold",
                    bbox=dict(boxstyle="round,pad=0.25", facecolor="white",
                              edgecolor="#CC2529", alpha=0.95, linewidth=0.5))

# Axes
ax.set_yticks(y_pos)
ax.set_yticklabels(labels_with_n, fontsize=9)
ax.set_ylim(-0.7, 2.9)
ax.set_xlabel("Share of cells implementing the egalitarian allocation (%)", fontsize=8.5)
ax.set_xlim(0, 100)
ax.set_xticks(range(0, 101, 20))
ax.tick_params(axis="x", labelsize=8)
ax.spines["top"].set_visible(False)
ax.spines["right"].set_visible(False)
ax.grid(axis="x", alpha=0.2)

# Group majority text label above the plot
ax.annotate(f"Villager majority\nfor egalitarian ({majority_ref:.0f}%)",
            xy=(majority_ref, 2.95), xytext=(majority_ref, 3.15),
            ha="center", va="bottom", fontsize=8, color="#3E9651",
            fontweight="bold", annotation_clip=False)

# Legend below
legend_elements = [
    Line2D([0], [0], color="#3E9651", linestyle="--", linewidth=1.3,
           label="Villager majority preference"),
    Line2D([0], [0], marker="D", color="w", markerfacecolor="#DA7C30",
           markersize=9, label="Leader's own vote"),
    Line2D([0], [0], marker="o", color="w", markerfacecolor="#396AB1",
           markersize=10, label="Implemented outcome"),
]
ax.legend(handles=legend_elements, loc="lower center", fontsize=8,
          framealpha=0.95, ncol=3, bbox_to_anchor=(0.5, -0.35))

plt.tight_layout()

outpath = os.path.join(root, "results", "figures", "figure4_allocation_outcomes.png")
fig.savefig(outpath, dpi=300, bbox_inches="tight")
print(f"\nSaved: {outpath}")

plt.close()
