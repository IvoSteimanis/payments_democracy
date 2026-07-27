*--------------------------------------------------------------------
* SCRIPT: 04_supplementary.do
* PURPOSE: replicates all supplementary online materials (SOM) tables
*          and figures
*--------------------------------------------------------------------

*--------------------------------------------------
* STRUCTURE OF THE DO-FILE
/*
	Supplementary online materials:
	   - Figure S1 (power analysis): figureS1_MDES.png
	   - Figure S2 (equivalence tests): produced by scripts/07_equivalence_tests.R
	   - Figure S3 (order effects): figureS3_order_effects.png
	   - Figure S4 (transition Sankey): produced by scripts/08_transition_sankey.R
	   - Figure S5 (authentic leadership subsample): figureS5_subgroups_robustness.png
	   - Figure S6 (stage 2 distributions): figureS6_preferred_allocation.png
	   - Figure S7 (payoff/distribution B): figureS7_egalitarian_likelihood.png
	   - Tables S2-S8, S10-S12
*/
*--------------------------------------------------


* Load cleaned dataset (long-format)
clear
use "$working_ANALYSIS/processed/analysis_long.dta"

*set panel
xtset id decision


*--------------------------------------------------------------
* Analysis for supplementary online materials (SOM)
*--------------------------------------------------------------
*Table S1.	Time schedule of field implementation
*created in word

* Figure S1.	Minimal detectable effect size: Within-subject design
*power analysis: MDES with 80% power
power pairedproportions 0.64, corr(0.39) diff(0(0.01)0.2) alpha(0.05 0.1) n(64)
*MDES alpha 0.05 --> d=0.18, alpha 0.1 --> d=0.16

power pairedproportions 0.64, corr(0.39) diff(0(0.01)0.2) n(64) table(alpha power beta N delta:"Difference in proportion to select democratic rule") graph(ytitle(Effect size) xline(0.8, lcolor(538b) lpattern(dash)) yline(0.178, lpattern(dash)) title("") legend(ring(1) rows(1) pos(6)) xdimension(power) xla(0(0.1)1, nogrid) yla(0(0.02)0.2, nogrid) ydimension(diff))
gr save  "$working_ANALYSIS/results/intermediate/figureS1_MDES.gph", replace
gr export "$working_ANALYSIS/results/figures/figureS1_MDES.png", replace width(4000)


*Figure S2.	Equivalence tests
* Produced by the R script: scripts/07_equivalence_tests.R
* Uses the TOSTER package (Caldwell 2022) with equivalence bounds of +/-0.18 (MDES).


preserve
clear all
use "$working_ANALYSIS/processed/analysis_wide.dta"


* Table S2.	Participants characteristics
* Motivation: leaders with empty f6 (no stated reason) coded as extrinsic (=0).
global controls2 male age education rootedness pca_wealth d3a d3c
iebaltab $controls2, grpvar(id1) rowvarlabels format(%9.2f)  ftest tblnonote savexlsx("$working_ANALYSIS/results/tables/tableS4_participants_characteristics.xlsx") replace
prtest male, by(villager)
foreach x of varlist age education rootedness pca_wealth d3a d3c {
	ranksum `x', by(villager)
	}
reg id1 $controls2 if id1 !=0 // chiefs vs villagers
reg id1 $controls2 if id1 !=1 // DEL vs villagers
* Leaders only
global controls1 male age education rootedness pca_wealth d3a d3c  married experience motivation helpauthentic
iebaltab $controls1, grpvar(TL) rowvarlabels format(%9.2f)  ftest  tblnonote savexlsx("$working_ANALYSIS/results/tables/tableS4_leaders_characteristics.xlsx") replace
reg TL $controls1


*Table S3.	Summary statistics of  main outcomes for leaders
* Stage 1 rule choices by leaders
estpost tabulate rule1
esttab using "$working_ANALYSIS/results/tables/tableS5_outcome_variables.tex", cell("b pct(fmt(0)) cumpct(fmt(0))") unstack noobs booktabs replace
estpost tabulate rule5
esttab using "$working_ANALYSIS/results/tables/tableS5_outcome_variables.tex", cell("b pct(fmt(0)) cumpct(fmt(0))") unstack noobs booktabs append
estpost tabulate rule6
esttab using "$working_ANALYSIS/results/tables/tableS5_outcome_variables.tex", cell("b pct(fmt(0)) cumpct(fmt(0))") unstack noobs booktabs append

* Stage 2 distribution decisions by leaders
estpost tabulate  vote1  if id <65 & rule1==1
bysort rule1: tab vote1 if id <65
bysort rule5: tab vote5 if id <65
bysort rule6: tab vote6 if id <65

restore


*Figure S3.	Order effects on rule choice in bonus rounds
bysort bonus_size bonus_order: tab rule
catplot rule bonus_order if decision > 1, over(bonus_size) percent(bonus_order bonus_size ) asyvar recast(bar) yla(0(20)100) blabel(bar, format(%9.0f) pos(center)  gap(0.1)) b1title("") legend(ring(0) pos(12) rows(1))
gr export "$working_ANALYSIS/results/figures/figureS3_order_effects.png", replace width(5800)

tab rule bonus_order if decision > 1, chi2 exact
*no significant differences between the two randomization (small or large bonus round played first)


*Table S4.	Sample splits by order of conditional payments
*Order 1: Base-Large-Small
eststo reg_time1: xtreg dem i.round if bonus_order==0, vce(robust)
testparm 2.round 3.round, equal
estadd scalar joint_test =r(p)
eststo reg_time2: xtreg fake i.round if bonus_order==0, vce(robust)
testparm 2.round 3.round, equal
estadd scalar joint_test =r(p)
eststo reg_time3: xtreg dic i.round if bonus_order==0, vce(robust)
testparm 2.round 3.round, equal
estadd scalar joint_test =r(p)

*Order 2: Base-Small-Large
eststo reg_time4: xtreg dem i.round if bonus_order==1, vce(robust)
testparm 2.round 3.round, equal
estadd scalar joint_test =r(p)
eststo reg_time5: xtreg fake i.round if bonus_order==1, vce(robust)
testparm 2.round 3.round, equal
estadd scalar joint_test =r(p)
eststo reg_time6: xtreg dic i.round if bonus_order==1, vce(robust)
testparm 2.round 3.round, equal
estadd scalar joint_test =r(p)

esttab reg_time1 reg_time2 reg_time3 reg_time4 reg_time5 reg_time6 using "$working_ANALYSIS/results/tables/tableS6_order_effects.tex", keep(2.round 3.round _cons) label se(%4.3f)  transform(ln*: exp(@) exp(@)) mgroups("Order 1: Base-Large-Small" "Order 2: Base-Small-Large", pattern(1 0 0 1 0 0)) mtitles("Democratic" "Pseudo-democratic" "Dictator" "Democratic" "Pseudo-democratic" "Dictator") b(%4.3f) stats(N N_clust r2_o joint_test, fmt(0 0 3 3) labels("Observations" "Cluster" "Overall R-squared" "Test: Round 2 = Round 3 (chi2)" )) star(* 0.10 ** 0.05 *** 0.01) booktabs nonotes addnotes("Notes: Robust standard errors in brackets: * p < 0.10, ** p < 0.05, *** p < 0.01.") replace


*Figure S4.	Transition of leaders' rule choices
* Produced by the R script: scripts/08_transition_sankey.R



*Table S5. Main treatment effects
*stored before in 03_main_paper.do

*Table S6.	Treatment effects seperately for chiefs and DELs
*DEL only
eststo reg_del1: xtprobit dem bonus bonus_order if TL==0, vce(robust)
eststo reg_del2: xtprobit fake bonus bonus_order if TL==0, vce(robust)
eststo reg_del3: xtprobit dic bonus bonus_order if TL==0, vce(robust)
*TL only
eststo reg_tl1: xtprobit dem bonus bonus_order if TL==1, vce(robust)
eststo reg_tl2: xtprobit fake bonus bonus_order if TL==1, vce(robust)
eststo reg_tl3: xtprobit dic bonus bonus_order if TL==1, vce(robust)

esttab reg_del1 reg_del2 reg_del3 reg_tl1 reg_tl2 reg_tl3 using "$working_ANALYSIS/results/tables/tableS8_chiefs_DEL.tex",  ci transform(ln*: exp(@) exp(@))mtitles("Democratic" "Pseudo-democratic" "Dictator" "Democratic" "Pseudo-democratic" "Dictator") b(%4.2f) eqlabels("" "lnsig2u", none) stats(N N_g sigma_u rho chi2 p, labels("N" "Individuals" "sigma_u" "rho" "Wald Chi2" "p" ) fmt(%4.0f %4.0f %4.2f)) star(* 0.10 ** 0.05 *** 0.01) booktabs replace


*Table S7.	Robustness check – Mixed effects regression
* Treating participant as a random factor or use a simple linear model
eststo reg_rb1: xtmixed dem bonus_2 bonus_3 bonus_order || id:, reml
matrix N_g = e(N_g)
local individuals = N_g[1, 1]
estadd local individuals `individuals'
eststo reg_rb2: xtmixed fake bonus_2 bonus_3 bonus_order || id:,  reml
matrix N_g = e(N_g)
local individuals = N_g[1, 1]
estadd local individuals `individuals'
eststo reg_rb3: xtmixed dic bonus_2 bonus_3 bonus_order || id:,  reml
matrix N_g = e(N_g)
local individuals = N_g[1, 1]
estadd local individuals `individuals'

esttab reg_rb1 reg_rb2 reg_rb3 using "$working_ANALYSIS/results/tables/tableS9_multilevel_panel.tex",  ci transform(ln*: exp(@) exp(@))mtitles("Democratic" "Pseudo-democratic" "Dictator") b(%4.2f) eqlabels("" "sd(_cons)" "sd(Residual)", none) stats(N individuals chi2 p, labels("N" "Individuals" "Wald Chi2" "p" ) fmt(%4.0f %4.0f %4.2f)) star(* 0.10 ** 0.05 *** 0.01) booktabs replace


*Table S8.	Effects of bonus payments – Multinomial probit
mprobit rule bonus_2 bonus_3 bonus_order, cluster(id)
margins, dydx(bonus_2 bonus_3) post
est store bonus1
mprobit rule bonus bonus_order, cluster(id) level(90)
margins, dydx(bonus) post
est store bonus2

esttab bonus1 bonus2 using "$working_ANALYSIS/results/tables/tableS10_multinomial_probit.tex", ///
	b(%4.3f) se(%4.3f) ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	stats(N, labels("Observations") fmt(%4.0f)) ///
	booktabs replace



*Table S9.	Subsample treatment effects on democratic rule choices
*stored before in 03_main_paper.do


*Table S10.	Robustness check: Authentic leadership score median split
gen low_authentic = 1-authentic_leader1
eststo dem1: xtmixed dem100 i.bonus##i.low_authentic bonus_order || id: if rule_base==1, vce(robust)
matrix N_g = e(N_g)
estadd local individuals = N_g[1,1]
eststo dem2: xtmixed dem100 i.bonus##i.low_authentic bonus_order || id: if rule_base>1, vce(robust)
matrix N_g = e(N_g)
estadd local individuals = N_g[1,1]

eststo fake1: xtmixed fake100 i.bonus##i.low_authentic bonus_order || id: if rule_base==1, vce(robust)
matrix N_g = e(N_g)
estadd local individuals = N_g[1,1]
eststo fake2: xtmixed fake100 i.bonus##i.low_authentic bonus_order || id: if rule_base>1, vce(robust)
matrix N_g = e(N_g)
estadd local individuals = N_g[1,1]

eststo dic1: xtmixed dic100 i.bonus##i.low_authentic bonus_order || id: if rule_base==1, vce(robust)
matrix N_g = e(N_g)
estadd local individuals = N_g[1,1]
eststo dic2: xtmixed dic100 i.bonus##i.low_authentic bonus_order || id: if rule_base>1, vce(robust)
matrix N_g = e(N_g)
estadd local individuals = N_g[1,1]

*Figure S5: Heterogeneous responses — authentic leadership (colors from global palette)
preserve
coefplot(dem1, label(Baseline: Democratic) mcolor("57 106 177") ciopts(lcolor("57 106 177*1" "57 106 177*.3"))) (dem2, label(Baseline: Pseudo or Autocratic) mcolor("128 100 162") ciopts(lcolor("128 100 162*1" "128 100 162*.3"))),  bylabel(Democratic Rule) || (fake1, mcolor("57 106 177") ciopts(lcolor("57 106 177*1" "57 106 177*.3"))) (fake2, mcolor("128 100 162") ciopts(lcolor("128 100 162*1" "128 100 162*.3"))),bylabel(Pseudo-Democratic Rule) || (dic1, mcolor("57 106 177") ciopts(lcolor("57 106 177*1" "57 106 177*.3"))) (dic2, mcolor("128 100 162") ciopts(lcolor("128 100 162*1" "128 100 162*.3"))),  bylabel(Autocratic Rule) ||, xla(,labsize(6pt)) byopts(compact  imargin(*1.1) rows(1))  keep(1.bonus 1.low_authentic 1.bonus#1.low_authentic) coeflabels(1.bonus = "Payment: Combined" 1.low_authentic = "Motivation: Low authentic" 1.bonus#1.low_authentic = "Payment*Low authentic", labsize(6pt))  xline(0, lpattern(dash) lcolor(gs3)) xtitle("Regression estimated impact relative to baseline in %-points", size(6pt)) grid(none) levels(95 90)mlabel(cond(@pval<.01, "***", cond(@pval<.05, "**", cond(@pval<.1, "*", "")))) msize(3pt) msymbol(D) mlabsize(10pt) mlabposition(12) mlabgap(-1.2)  subtitle(, size(9pt) lstyle(none) margin(medium) nobox justification(center) alignment(top) bmargin(top))  xsize(5) ysize(2) ciopts(lwidth(0.8 2) recast(rcap)) legend(size(6pt))
gr save  "$working_ANALYSIS/results/intermediate/figureS5_subgroups_robustness.gph", replace
gr export  "$working_ANALYSIS/results/figures/figureS5_subgroups_robustness.png", replace width(4000)
restore


*Table S10.	Heterogeneous treatment effects with authentic leadership median split
esttab dem1 fake1 dic1 dem2 fake2 dic2 using "$working_ANALYSIS/results/tables/tableS12_authentic_leadership.tex",  keep(1.bonus 1.low_authentic 1.bonus#1.low_authentic)  label se(%4.3f)  transform(ln*: exp(@) exp(@)) mgroups("Baseline: Democratic rule" "Baseline: Pseudo-democratic or autocratic rule", pattern(1 0 0 1 0 0))   mtitles("Democratic" "Pseudo-democratic" "Autocratic" "Democratic" "Pseudo-democratic" "Autocratic") b(%4.3f) stats(N individuals chi2 p, labels("N" "Individuals" "Wald Chi2" "p" ) fmt(%4.0f %4.0f %4.2f)) star(* 0.10 ** 0.05 *** 0.01) booktabs nonotes addnotes("Notes: Estimates are from multilevel panel regressions that account for the grouping structure of the data at the individual level (random intercept). Standard errors with stars indicating the following significant levels: * p < 0.10, ** p < 0.05, *** p < 0.01") replace

*Table S11.	Villager satisfaction with different leader types
*stored before in 03_main_paper.do



preserve
clear all
use "$working_ANALYSIS/processed/analysis_wide.dta"
* Figure S6.	Preferred distribution by leaders and villagers in the baseline
*PANEL A: Leaders
catplot vote1 , over(rule_base) asyvar recast(bar) yla(0(20)100, nogrid) percent(rule_base) bargap(10) blabel(bar, format(%9.0f) pos(outside)  gap(0.1)) title("{bf:A} Leaders")  b1title("") legend(ring(1) pos(6) rows(1))
gr save  "$working_ANALYSIS/results/intermediate/figureS6a_panel.gph", replace

*PANEL B: Villagers
catplot vote1 if id>64, asyvar recast(bar) yla(0(20)100, nogrid) percent bargap(10)  blabel(bar, format(%9.0f)pos(outside)  gap(0.1)) title("{bf:B} Villagers")  b1title("") legend(ring(1) pos(6) rows(1))
gr save  "$working_ANALYSIS/results/intermediate/figureS6b_panel.gph", replace

grc1leg  "$working_ANALYSIS/results/intermediate/figureS6a_panel.gph" "$working_ANALYSIS/results/intermediate/figureS6b_panel.gph", rows(1) pos(6) ring(1)
gr save "$working_ANALYSIS/results/intermediate/figureS6_preferred_allocation.gph", replace
gr export "$working_ANALYSIS/results/figures/figureS6_preferred_allocation.png", replace width(4000)

*Tests
gen prefer_B=1 if vote1==2
replace prefer_B=0 if vote1==1

tab vote1 villager, chi2 exact
ttest prefer_B, by(villager)
* villagers significantly more likely to prefer distribution B than leaders (20 pp difference)

tab vote1 game1_group if villager==1, chi2 exact
ttest prefer_B if id>64, by(game1_group)
* distribution B is more often (7pp, p=0.13) preferred by villagers in groups governed by traditional authorities
reg prefer_B game1_group if id>64, vce(robust)
tab vote1 rule_base if rule_base <3, chi2 exact

tab leader_type
* Regression analysis
probit prefer_B villager , vce(robust)
margins, dydx(villager)


restore


*Table S12.	Likelihood of distribution B being implemented in stage 2
eststo reg_stage2: xtprobit payoff bonus baseline_2 baseline_3 bonus_pseudo bonus_dictator, vce(robust)
margins, dydx(*) post
est store payoff1

esttab reg_stage2 using "$working_ANALYSIS/results/tables/tableS15_egalitarian_allocation.tex",  ci transform(ln*: exp(@) exp(@))mtitles("Distribution B (=1)") b(%4.2f) eqlabels("" "lnsig2u", none) stats(N N_g sigma_u rho chi2 p, labels("N" "Individuals" "sigma_u" "rho" "Wald Chi2" "p" ) fmt(%4.0f %4.0f %4.2f)) star(* 0.10 ** 0.05 *** 0.01) booktabs replace


*Figure S7.	Likelihood of distribution B being implemented
coefplot (payoff1, msymbol(d)), keep(bonus baseline_2 baseline_3 bonus_pseudo bonus_dictator) coeflabels(bonus= "Pooled Bonus" baseline_2 = "Base: Pseudo-democrat" baseline_3 ="Base: Autocrat" bonus_pseudo = "Interaction:Bonus*Pseudo" bonus_dictator = "Interaction: Bonus*Autocrat") xline(0, lpattern(dash) lcolor(gs3))  xtitle("Regression estimated impact relative to baseline democrats in pp") msymbol(d) xla(-1(0.25)1, nogrid) grid(none) levels(95 90) ciopts(lwidth(0.3 1) lcolor(*.8 *.2)  recast(rcap)) mlabel(cond(@pval<.01, "***", cond(@pval<.05, "**", cond(@pval<.1, "*", "")))) mlabsize(8pt) mlabposition(12) mlabgap(-0.1)
gr save "$working_ANALYSIS/results/intermediate/figureS7_egalitarian_likelihood.gph", replace
gr export "$working_ANALYSIS/results/figures/figureS7_egalitarian_likelihood.png", replace width(3465)




** EOF
