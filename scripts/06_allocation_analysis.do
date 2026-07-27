*-------------------------------------------------------------------------------------------------------
* SCRIPT: 06_allocation_analysis.do
* PURPOSE: Stage 2 allocation analysis
*          Responds to R1.3: "does not look at resulting allocations"
*          Also feeds R2.4: democracy != equality
* READS:  processed/analysis_long.dta, processed/analysis_wide.dta
* OUTPUT:
*   tableS14_allocation_by_rule_type.tex  SOM Table S14
*   stage2_descriptive.tex                supporting descriptive (not a numbered exhibit)
*   stage2_allocation.tex                 supporting marginal effects (not a numbered exhibit)
*   fig_stage2_coefplot.png               standalone coefplot (Panel B of manuscript Figure 4)
*   fig_stage2_combined_stata.png         all-Stata two-panel version; the manuscript
*                                         Figure 4 is produced by fig4a_panel.py
*
* NOTE: SOM Table S15 (likelihood of egalitarian allocation being implemented) is
*       produced by 04_supplementary.do, not here.
*-------------------------------------------------------------------------------------------------------

use "$working_ANALYSIS/processed/analysis_long.dta", clear

*-------------------------------------------------------------------------------------------------------
* Generate analysis variables
*-------------------------------------------------------------------------------------------------------
* payoff is already 0/1 (0=A leader-favoring, 1=B egalitarian)
label var payoff "Egalitarian distribution implemented (=1)"

gen majority_B = (majority == "B")
label var majority_B "Majority prefers egalitarian (=1)"

gen match_majority = (payoff == majority_B)
label var match_majority "Matches majority preference (=1)"

gen is_bonus = inlist(decision, 5, 6)
label var is_bonus "Bonus round (=1)"


*-------------------------------------------------------------------------------------------------------
* Descriptive table: egalitarian outcomes by rule type
*-------------------------------------------------------------------------------------------------------
di _n "{hline 70}"
di "STAGE 2 ALLOCATION OUTCOMES BY RULE TYPE"
di "{hline 70}"

tabstat payoff match_majority, by(rule) stat(mean n) format(%4.3f) nototal

tab rule payoff, chi2


*-------------------------------------------------------------------------------------------------------
* By treatment (baseline vs bonus)
*-------------------------------------------------------------------------------------------------------
di _n "{hline 70}"
di "EGALITARIAN OUTCOMES BY RULE x TREATMENT"
di "{hline 70}"

bysort rule: tabstat payoff, by(is_bonus) stat(mean n) format(%4.3f) nototal


*-------------------------------------------------------------------------------------------------------
* Democracy != equality patterns
*-------------------------------------------------------------------------------------------------------
di _n "{hline 70}"
di "DEMOCRACY != EQUALITY PATTERNS"
di "{hline 70}"

count if dem == 1 & payoff == 0
local dem_ineq = r(N)
count if dem == 1
local dem_total = r(N)
di "Democratic but leader-favoring: `dem_ineq'/`dem_total' = " %4.1f `dem_ineq'/`dem_total'*100 "%"

count if dic == 1 & payoff == 1
local auto_eq = r(N)
count if dic == 1
local auto_total = r(N)
di "Autocratic but egalitarian: `auto_eq'/`auto_total' = " %4.1f `auto_eq'/`auto_total'*100 "%"

sum payoff if is_bonus == 0
di "Baseline egalitarian rate: " %4.1f r(mean)*100 "%"
sum payoff if is_bonus == 1
di "Bonus egalitarian rate: " %4.1f r(mean)*100 "%"


*-------------------------------------------------------------------------------------------------------
* Transition analysis: use rule_base (3-category: Democratic, Pseudo, Autocratic)
*-------------------------------------------------------------------------------------------------------
di _n "{hline 70}"
di "TRANSITION ANALYSIS"
di "{hline 70}"

tab rule_base rule if is_bonus == 1, cell

di _n "Egalitarian rate by transition (baseline -> bonus rule):"
levelsof rule_base, local(base_levels)
levelsof rule, local(rule_levels)
foreach b of local base_levels {
	foreach r of local rule_levels {
		quietly count if rule_base == `b' & rule == `r' & is_bonus == 1
		local n = r(N)
		if `n' > 0 {
			quietly sum payoff if rule_base == `b' & rule == `r' & is_bonus == 1
			local lbl_b : label (rule_base) `b'
			local lbl_r : label (rule) `r'
			di "`lbl_b' -> `lbl_r': " %4.0f r(mean)*100 "% egalitarian (n=`n')"
		}
	}
}


*-------------------------------------------------------------------------------------------------------
* Panel probit: egalitarian outcome on contemporaneous rule
*-------------------------------------------------------------------------------------------------------
di _n "{hline 70}"
di "PANEL PROBIT: EGALITARIAN OUTCOME"
di "{hline 70}"

eststo clear

* Model 1: Contemporaneous rule only
eststo m1: xtprobit payoff fake dic, re i(id) vce(robust)
quietly margins, dydx(fake dic) post
est store m1_me

* Model 2: Rule + bonus
eststo m2: xtprobit payoff fake dic is_bonus, re i(id) vce(robust)
quietly margins, dydx(fake dic is_bonus) post
est store m2_me

esttab m1_me m2_me using "$working_ANALYSIS/results/tables/stage2_allocation.tex", ///
	b(%4.3f) se(%4.3f) ///
	mtitles("(1)" "(2)") ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	stats(N, labels("Observations") fmt(%4.0f)) ///
	varlabels(fake "Pseudo-democratic" dic "Autocratic" is_bonus "Bonus round") ///
	booktabs ///
	title("Marginal effects on egalitarian distribution (panel probit)") ///
	addnotes("Average marginal effects from random-effects panel probit." ///
	"Robust standard errors in parentheses. Base: democratic rule.") ///
	replace


*-------------------------------------------------------------------------------------------------------
* SOM Table S14 - Egalitarian allocation outcomes by governance rule and leader type
*-------------------------------------------------------------------------------------------------------
/* Share of decisions in which the egalitarian allocation was implemented, split by
   governance rule and leader type (TL = traditional authority, DEL = elected water
   point committee chairperson), with counts in parentheses. */
capture file close texS14
file open texS14 using "$working_ANALYSIS/results/tables/tableS14_allocation_by_rule_type.tex", write replace
file write texS14 "\begin{table}[htbp]\centering" _n
file write texS14 "\caption{Egalitarian allocation outcomes by governance rule and leader type}" _n
file write texS14 "\begin{tabular}{lccc}" _n
file write texS14 "\toprule" _n
file write texS14 "Governance rule & Traditional Leaders & Democratic Leaders & Total \\" _n
file write texS14 "\midrule" _n

local r = 1
foreach rule_name in "Democratic" "Pseudo-democratic" "Autocratic" {
	file write texS14 "`rule_name'"
	* columns: TL==1, TL==0, then both pooled
	foreach cond in "& TL==1" "& TL==0" "" {
		quietly count if rule==`r' & !missing(payoff) `cond'
		local den = r(N)
		quietly count if rule==`r' & payoff==1 `cond'
		local num = r(N)
		if `den' > 0 {
			local pct = 100 * `num' / `den'
			file write texS14 " & " %4.1f (`pct') "\% (`num'/`den')"
		}
		else {
			file write texS14 " & --"
		}
	}
	file write texS14 " \\" _n
	local r = `r' + 1
}

file write texS14 "\bottomrule" _n
file write texS14 "\multicolumn{4}{p{0.9\textwidth}}{\footnotesize Notes: Share of decisions in which the egalitarian allocation was implemented, by governance rule and leader type (Traditional Leaders vs. Development Committee Leaders). Counts in parentheses. Autocratic cell sizes are small; interpret with caution.} \\" _n
file write texS14 "\end{tabular}" _n
file write texS14 "\end{table}" _n
file close texS14


*-------------------------------------------------------------------------------------------------------
* Supporting descriptive: egalitarian outcome and villager-majority match rate by rule
* Not a numbered SOM exhibit; the match rate underpins the override discussion for Figure 4.
*-------------------------------------------------------------------------------------------------------
preserve

collapse (mean) egal_rate=payoff match_rate=match_majority (count) n=payoff, by(rule)

* Format as percentages
replace egal_rate = egal_rate * 100
replace match_rate = match_rate * 100

* Write manual .tex table
capture file close texfile
file open texfile using "$working_ANALYSIS/results/tables/stage2_descriptive.tex", write replace
file write texfile "\begin{table}[htbp]\centering" _n
file write texfile "\caption{Stage 2: Distributive outcomes by governance rule}" _n
file write texfile "\begin{tabular}{lccc}" _n
file write texfile "\toprule" _n
file write texfile "Governance rule & Egalitarian outcome (\%) & Match rate (\%) & N \\" _n
file write texfile "\midrule" _n

local i = 1
foreach rule_name in "Democratic" "Pseudo-democratic" "Autocratic" {
	local egal = egal_rate[`i']
	local match = match_rate[`i']
	local obs = n[`i']
	file write texfile "`rule_name' & " %4.1f (`egal') " & " %4.1f (`match') " & " %4.0f (`obs') " \\" _n
	local i = `i' + 1
}

file write texfile "\bottomrule" _n
file write texfile "\multicolumn{4}{l}{\footnotesize Egalitarian outcome: proportion where distribution B (equal split) was implemented.} \\" _n
file write texfile "\multicolumn{4}{l}{\footnotesize Match rate: proportion where implemented allocation matches the majority villager preference.} \\" _n
file write texfile "\multicolumn{4}{l}{\footnotesize Under the democratic rule, the match rate is 100\% by construction (binding vote).} \\" _n
file write texfile "\end{tabular}" _n
file write texfile "\end{table}" _n
file close texfile

restore


*-------------------------------------------------------------------------------------------------------
* Figure 4: Two-panel Stage 2 figure
* NOTE: The combined two-panel Figure 4 used in the manuscript is produced by
*       fig4a_panel.py (Python/matplotlib). The Stata code below generates
*       standalone components and reference values.
*-------------------------------------------------------------------------------------------------------

* --- Panel A: Actual egalitarian outcome by rule, with preference reference lines ---

* Step 1: Compute overall villager preference for B
preserve
use "$working_ANALYSIS/processed/analysis_wide.dta", clear
keep if id > 64
gen v1 = (vote1 == 2) if vote1 != .
gen v5 = (vote5 == 2) if vote5 != .
gen v6 = (vote6 == 2) if vote6 != .
egen vill_mean = rowmean(v1 v5 v6)
qui sum vill_mean
local vp = round(r(mean) * 100, 1)
di as text "Overall villager preference for egalitarian: `vp'%"
restore

* Step 2: Compute overall leader preference for B (individual vote, not outcome)
preserve
gen leader_egal = (vote == 2)
qui sum leader_egal
local lp = round(r(mean) * 100, 1)
di as text "Overall leader preference for egalitarian: `lp'%"
restore

* Step 3: Actual outcome by rule
preserve
collapse (mean) egal_outcome=payoff, by(rule)
replace egal_outcome = egal_outcome * 100
label define rulelab 1 "Democratic" 2 "Pseudo-democratic" 3 "Autocratic", replace
label values rule rulelab

graph hbar egal_outcome, over(rule, label(labsize(7pt))) ///
	bar(1, color("57 106 177")) ///
	blabel(bar, size(7pt) format(%3.0f) position(outside)) ///
	ylabel(0(20)100, nogrid labsize(6pt)) ///
	yline(`vp', lpattern(dash) lcolor("62 150 81") lwidth(medthick)) ///
	yline(`lp', lpattern(shortdash) lcolor("218 124 48") lwidth(medthick)) ///
	legend(off) ///
	title("{bf:A } Egalitarian outcomes by governance rule", size(8pt)) ///
	ytitle("% Egalitarian", size(6pt)) ///
	note("Green dashed = villager preference (`vp'%)     Orange dashed = leader preference (`lp'%)", size(5pt)) ///
	xsize(2.5) ysize(2)
gr save "$working_ANALYSIS/results/intermediate/fig4a_preference_gap.gph", replace

restore

* --- Panel B: Coefplot (marginal effects, scaled to pp) ---
quietly xtprobit payoff fake dic is_bonus, re i(id) vce(robust)
quietly margins, dydx(fake dic is_bonus) post
est store plot_me_pp

/* Marginal effects are on the probability scale; the plot reports percentage points.
   Rescaling is done by coefplot's rescale() rather than by post-multiplying e(b) and
   e(V) and calling `ereturn repost`: repost is only legal inside an eclass program, so
   issuing it from a do-file aborts with r(152). rescale(100) scales the point estimates
   and both confidence intervals identically, leaving p-values untouched. */
coefplot (plot_me_pp, msymbol(d) rescale(100)), ///
	keep(fake dic is_bonus) ///
	coeflabels(fake = "Pseudo-democratic" dic = "Autocratic" is_bonus = "Bonus round") ///
	xline(0, lpattern(dash) lcolor(gs6)) ///
	xtitle("Marginal effect on egalitarian outcome (pp)", size(6pt)) ///
	xla(-60(10)20, nogrid labsize(6pt)) ///
	grid(none) ///
	levels(95 90) ///
	ciopts(lwidth(0.8 2) lcolor(*1 *.3) recast(rcap)) ///
	mlabel(string(@b,"%3.0f") + " pp" + cond(@pval<.01, "***", cond(@pval<.05, "**", cond(@pval<.1, "*", ""))) + " (p=" + string(@pval,"%4.2f") + ")") ///
	mlabsize(6pt) mlabposition(12) mlabgap(1) msymbol(D) msize(3pt) ///
	title("{bf:B } Marginal effects on egalitarian distribution", size(8pt)) ///
	xsize(2.5) ysize(2)
gr save "$working_ANALYSIS/results/intermediate/fig4b_coefplot.gph", replace

* --- Combine ---
/* This is the all-Stata version of the two-panel figure. Manuscript Figure 4 is the
   Python version written by fig4a_panel.py; the two used to share the filename
   figure4_allocation_outcomes.png, so whichever ran last silently won. They now write
   distinct files. See README, "Output mapping". */
gr combine "$working_ANALYSIS/results/intermediate/fig4a_preference_gap" ///
	"$working_ANALYSIS/results/intermediate/fig4b_coefplot", ///
	cols(2) xsize(5) ysize(2) imargin(small)
gr export "$working_ANALYSIS/results/figures/fig_stage2_combined_stata.png", width(4000) replace

* Also keep standalone coefplot for reference
gr use "$working_ANALYSIS/results/intermediate/fig4b_coefplot.gph"
gr export "$working_ANALYSIS/results/figures/fig_stage2_coefplot.png", width(2100) replace


** EOF
