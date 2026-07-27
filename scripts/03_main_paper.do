*--------------------------------------------------------------------
* SCRIPT: 03_main_paper.do
* PURPOSE: replicates the main paper tables and figures
*--------------------------------------------------------------------

*--------------------------------------------------
* STRUCTURE OF THE DO-FILE
/*
	Main paper outputs:
	   - Figure 2 (4-panel combined): figure2_democratic_preferences.png
	   - Figure 3 (main treatment effects): figure3_treatment_effects.png
	   - Figure 4 (heterogeneous effects): figure5_heterogeneous_responses.png
	   - Table S5: tableS7_main_effects.tex
	   - Table S9: tableS11_subsample_effects.tex
	   - Table S11: tableS13_villager_satisfaction.tex
*/
*--------------------------------------------------

*Figure 1.	Procedural fairness task
*created in powerpoint.


*--------------------------------------------------
* (1) Analysis for main paper
*--------------------------------------------------
*Figure 2.	Democratic preferences

*Panel A. Preference in study region (Afrobarometer)
clear
use "$working_ANALYSIS/data/afro_r6.dta"
keep if COUNTRY==22
rename *, lower


gen ohangwena= 0
replace ohangwena = 1 if region == 588
lab def ohang 0 "Namibia" 1 "Study region", replace
lab val ohangwena ohang
*SUPPORT FOR DEMOCRACY

foreach x of varlist q30 q52b q52j q52k q68d q71a {
	replace `x'=. if `x'==9
}
encode location_level_1, gen(constituency)
gen demo_pref= 1 if q30==3
replace demo_pref = 0 if q30<3

ttest q52b, by(ohangwena)
ttest q52j, by(ohangwena)
ttest q52k, by(ohangwena)
ttest q68d, by(ohangwena)
ttest q71a, by(ohangwena)

gen com_member = 0
replace com_member = 1 if q19b==3 | q19b==2


*Democracy is preferable to any other kind of government.
tab q30, nolab
replace q30 = 4-q30
lab def democracy 3 "Accept non-democracy" 2 "Indifferent" 1 "Pro-democracy", replace
lab val q30 democracy

gen study_sites = 0
replace study_sites = 1 if ohangwena==1
replace study_sites = 2 if constituency==9 | constituency==43 | constituency==71
lab def studies 0 "Namibia (n=1058)" 1 "Study region (n=76)" 2 "Study sites (n=38)", replace
lab val study_sites studies
mylabels 0(20)100, myscale(@) local(pctlabel) suffix("%")
catplot q30, over(study_sites) asyvars stack horizontal recast(bar) yla(`pctlabel', nogrid) percent(study_sites) bargap(10) blabel(bar, size(6pt) format(%9.0f) pos(center)  gap(0.1)) title("{bf:A }Afrobarometer: Preferences in Namibia", size(8pt)) ytitle("") b1title("", size(6pt)) legend(ring(1) pos(6) rows(1) size(6pt)) xsize(3.465) ysize(2)
gr save  "$working_ANALYSIS/results/intermediate/figure2a.gph", replace
ranksum q30 if study_sites!=1, by(study_sites)
ranksum q30 if study_sites!=0, by(study_sites)

reg q52b i.study_sites, vce(hc3)
ranksum q52b if study_sites!=1, by(study_sites)
ranksum q71a if study_sites!=1, by(study_sites)




*Panel B.	Importance of democratic principles
clear all
use "$working_ANALYSIS/processed/analysis_wide.dta"
* Please indicate for each of the statement whether you disagree strongly, disagree a little, neither agree nor disagree, agree a little, or strongly agree
 * Democratic norm items (c1-c12): recode missing → 0 → neutral midpoint (3).
 * This treats non-response as neither agreeing nor disagreeing.
 global dem_norm c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12
 foreach var of varlist $dem_norm {
	replace `var'=0 if `var'==.
	}

foreach var of varlist $dem_norm {
	gen new_`var'=1 if `var'==1
	replace new_`var'=2 if `var'==2
	replace new_`var'=3 if `var'==0
	replace new_`var'=4 if `var'==3
	replace new_`var'=5 if `var'==4
	}
lab var new_c1 "For important decisions members of a group should be allowed to vote."
lab var new_c2 "Most people can learn to be leaders, it is not a matter of birth."
lab var new_c3 "Democratic elections in this village ensure that the elected authorities act in the interest of their people."
lab var new_c4 "In most cases, failures in this village are traceable to bad cicumstances and not bad leadership."
lab var new_c5 "Schools and parents schould teach children to obey authority."
lab var new_c6 "Favouring friends, relatives or supporters over others in the course of ones duties as a leader is sometimes justified."
lab var new_c7 "On the whole, men make better political leaders than women do."
lab var new_c8 "A leader hast to serve all people including those who did not vote for him/her or are not friends."
lab var new_c9 "Accepting a bribe in the course of ones duties is sometimes justified."
lab var new_c10 "Most people in this village can be trusted."
lab var new_c11 "On the whole, more educated people make better political leaders than the less educated do."
lab var new_c12 "On the whole, elders make better political leaders than the youth do."

gen new_c6r = 6- new_c6
gen new_c7r = 6- new_c7
gen new_c9r = 6- new_c9
alpha new_c1 new_c3 new_c8 new_c9r

gen types = 0 if villager==1
replace types = 1 if extrinsic_leader==0
replace types = 2 if extrinsic_leader==1
lab def villy1 0 "Villagers" 1 "Intrinsic" 2 "Extrinsic", replace
lab val types villy1

*graphical presentation
preserve
rename new_c1 norm1
rename new_c3 norm2
rename new_c8 norm3

reshape long norm, i(id) j(pr_id)
label define pr1 1 "Voting" 2 "Dem. accountability" 3 "Inclusive leadership"
label values pr_id pr1

gen agree = (norm >= 4) * 100 if norm != .

* Simple grouped bar chart (% Agree)
graph hbar (mean) agree, over(types, label(labsize(6pt))) over(pr_id, label(labsize(6pt)) gap(80)) asyvars bar(1, color(gs9)) bar(2, color(forest_green)) bar(3, color(purple)) blabel(bar, size(5pt) format(%3.0f) position(outside)) ylabel(0(20)100, nogrid labsize(6pt)) legend(ring(1) pos(6) rows(1) size(6pt)) title("{bf:B } Survey: Importance of democratic principles", size(8pt)) ytitle("% Agree or strongly agree", size(6pt)) xsize(3.465) ysize(2)
gr save "$working_ANALYSIS/results/intermediate/figure2b.gph", replace

* Probit AMEs for text reporting (not plotted)
replace agree = agree / 100
di as text _newline "=== PROBIT AMEs: Agree ~ types, by survey item ==="
forval j = 1/3 {
	local lbl: label pr1 `j'
	di as text _newline "--- `lbl' ---"
	probit agree i.types if pr_id == `j', vce(robust)
	margins, dydx(types) post
	est store ame`j'
}
esttab ame1 ame2 ame3, b(%4.3f) se(%4.3f) mtitles("Voting" "Dem. accountability" "Inclusive leadership") star(* 0.10 ** 0.05 *** 0.01)

restore

*villagers vs. leaders
ttest new_c1, by(villager)
ttest new_c3, by(villager)
ttest new_c8, by(villager)

*intrinsic vs. extrinsic leaders
ttest new_c1, by(extrinsic_leader)
ttest new_c3, by(extrinsic_leader)
ttest new_c8, by(extrinsic_leader)


*normative expecation related to group voting by leader baseline choice
reg new_c1 i.rule_base3 if villager==1, vce(hc3)


*Panel C: Leaders' baseline rule choices
lab def choices 1 "Democratic" 2 "Pseudo-democratic" 3 "Autocratic", replace
lab val rule_base choices
mylabels 0(20)100, myscale(@) local(pctlabel) suffix("%")
catplot rule_base if types>0,  over(types) asyvars stack horizontal recast(bar) yla(`pctlabel', nogrid) percent(types) bargap(10) blabel(bar, size(6pt) format(%9.0f) pos(center) gap(0.1)) title("{bf:C }Experiment: Leaders' baseline preferences", size(8pt)) ytitle("") b1title("", size(6pt)) legend(ring(1) pos(6) rows(1) size(6pt)) xsize(3.465) ysize(2)
gr save  "$working_ANALYSIS/results/intermediate/figure2c.gph", replace

prtest rule_base1 if types>0, by(types)
prtest rule_base2 if types>0, by(types)
prtest rule_base3 if types>0, by(types)



*Panel D: Villagers' satisfaction with their leader's performance outside the lab
preserve
clear all
use "$working_ANALYSIS/processed/analysis_wide.dta"

bysort group_leader_type: sum satisfaction_leader
eststo reg_satisfaction: reg satisfaction_leader rule_base2 rule_base3 i.village, vce(robust)


coefplot ///
  (reg_satisfaction, keep(rule_base2) levels(95 90) msize(4pt) ///
    mlabel(string(@b,"%4.2f") + cond(@pval<.01, "***", cond(@pval<.05, "**", cond(@pval<.1, "*", ""))) + " (p=" + string(@pval,"%4.2f") + ")") ///
    msymbol(D) mlabsize(6pt) mlabposition(12) mlabgap(2) ///
    mlabcolor("218 124 48") mcolor("218 124 48") ///
    ciopts(lwidth(0.8 2) lcolor("218 124 48*1" "218 124 48*.3") recast(rcap))) ///
  (reg_satisfaction, keep(rule_base3) levels(95 90) msize(4pt) ///
    mlabel(string(@b,"%4.2f") + cond(@pval<.01, "***", cond(@pval<.05, "**", cond(@pval<.1, "*", ""))) + " (p=" + string(@pval,"%4.2f") + ")") ///
    msymbol(D) mlabsize(6pt) mlabposition(12) mlabgap(2) ///
    mlabcolor("204 37 41") mcolor("204 37 41") ///
    ciopts(lwidth(0.8 2) lcolor("204 37 41*1" "204 37 41*.3") recast(rcap))), ///
  title("{bf:D } Survey: Villagers' satisfaction with leaders'", size(8pt)) ///
  keep(rule_base2 rule_base3) ///
  coeflabels(rule_base2 = "Baseline: Pseudo-democratic" rule_base3 = "Baseline: Autocratic") ///
  xline(0, lpattern(dash) lcolor(gs6)) ///
  xtitle("Regression estimated effect relative to 'Baseline: Democratic'", size(6pt)) ///
  xla(-1(0.25)0.25, nogrid labsize(6pt)) grid(none) legend(off) ///
  xsize(4) ysize(3) aspect(0.6)
gr save "$working_ANALYSIS/results/intermediate/figure2d.gph", replace


*Table S11.	Villager satisfaction with different leader types
esttab reg_satisfaction using "$working_ANALYSIS/results/tables/tableS13_villager_satisfaction.tex", keep(rule_base2 rule_base3) ci transform(ln*: exp(@) exp(@))mtitles("Satisfaction") b(%4.2f) eqlabels("" "lnsig2u", none) stats(N N_g r2 r2_a , labels("N" "Individuals" "R-squared" "Adjusted R-squared") fmt(%4.0f %4.0f %4.2f)) star(* 0.10 ** 0.05 *** 0.01) booktabs replace
restore


*COMBINE ALL FOUR PANELS
gr combine "$working_ANALYSIS/results/intermediate/figure2a" "$working_ANALYSIS/results/intermediate/figure2b" "$working_ANALYSIS/results/intermediate/figure2c" "$working_ANALYSIS/results/intermediate/figure2d.gph" , xsize(5) ysize(3) cols(2) 
gr save  "$working_ANALYSIS/results/intermediate/figure2_democratic_preferences.gph", replace
gr export "$working_ANALYSIS/results/figures/figure2_democratic_preferences.png", replace width(4000)



* Load cleaned dataset (long-format)
clear
use "$working_ANALYSIS/processed/analysis_long.dta"

*set panel
xtset id decision


*Figure 4.	Average treatment effects of payment
*Panel A: Main treatment effects
tab rule if decision<2
bys TL: tab rule if decision<2
prtest dem1 if decision<2, by(TL)


* Small and Large bonus separately
* pr1-pr3: marginal effects from separate-bonus probits (Figure 3, left bars)
* pr4-pr6: marginal effects from pooled-bonus probits (Figure 3, right bars)
eststo reg_dem1: xtprobit dem bonus_2 bonus_3 bonus_order, vce(robust)
margins, dydx(*) post
est store pr1
eststo reg_fake1: xtprobit fake bonus_2 bonus_3 bonus_order, vce(robust)
margins, dydx(*) post
est store pr2
eststo reg_dic1: xtprobit dic bonus_2 bonus_3 bonus_order, vce(robust)
margins, dydx(*) post
est store pr3

*test small versus large bonus
xtprobit dem bonus_1 bonus_3 bonus_order, vce(robust)
margins, dydx(*) post
xtprobit fake bonus_1 bonus_3 bonus_order, vce(robust)
margins, dydx(*) post
xtprobit dic bonus_1 bonus_3 bonus_order, vce(robust)
margins, dydx(*) post

*pooled bonus
eststo reg_dem2: xtprobit dem bonus bonus_order, vce(robust)
margins, dydx(*) post
est store pr4
eststo reg_fake2: xtprobit fake bonus bonus_order, vce(robust)
margins, dydx(*) post
est store pr5
eststo reg_dic2: xtprobit dic bonus bonus_order, vce(robust)
margins, dydx(*) post
est store pr6


*plot marginal effects for bonus treatment

*Figure 3: Main treatment effects
* Note: mlabel shows coefficient in pp + significance stars + p-value
coefplot(pr1 pr4), bylabel(Democratic Rule) || (pr2 pr5),bylabel(Pseudo-Democratic Rule) || (pr3 pr6),  bylabel(Autocratic Rule) ||, xla(,labsize(6pt) nogrid) byopts(compact  imargin(*1.1) rows(1) legend(off))  keep(bonus_2 bonus_3 bonus) coeflabels(bonus_2 = "Payment: N$10" bonus_3 = " Payment: N$100" bonus = "Payment: Combined", labsize(6pt))  xline(0, lpattern(dash) lcolor(gs6)) xtitle("Regression estimated impact relative to baseline in %-points", size(6pt)) grid(none) levels(95 90) mlabel(string(@b*100,"%3.1f") + cond(@pval<.01, "***", cond(@pval<.05, "**", cond(@pval<.1, "*", ""))) + " (p=" + string(@pval,"%4.2f") + ")") msize(3pt) msymbol(D) mlabsize(6pt) mlabposition(12) mlabgap(1)  subtitle(, size(9pt) lstyle(none) margin(medium) nobox justification(center) alignment(top) bmargin(top))  xla(-0.2 "-20" -0.1 "-10" 0 "0" 0.1 "10" 0.2 "20")  xsize(5) ysize(2) ciopts(lwidth(0.8 2) lcolor(*1 *0.3)  recast(rcap)) norecycle aspectratio(0.7)
gr save  "$working_ANALYSIS/results/intermediate/figure3_treatment_effects.gph", replace
gr export  "$working_ANALYSIS/results/figures/figure3_treatment_effects.png", replace width(4000)



*Table S5.	Main effects of bonus payment
esttab reg_dem1 reg_fake1 reg_dic1 reg_dem2 reg_fake2 reg_dic2 using "$working_ANALYSIS/results/tables/tableS7_main_effects.tex",  ci transform(ln*: exp(@) exp(@))mtitles("Democratic" "Pseudo-democratic" "Dictator" "Democratic" "Pseudo-democratic" "Dictator") b(%4.2f) eqlabels("" "lnsig2u", none) stats(N N_g sigma_u rho chi2 p, labels("N" "Individuals" "sigma_u" "rho" "Wald Chi2" "p" ) fmt(%4.0f %4.0f %4.2f)) star(* 0.10 ** 0.05 *** 0.01) booktabs replace




*Table 2.	Observed crowding effects of conditional monetary incentives in the experiment.
*created in word, descriptives are calculated here

tab rule type if round==2, column

*Figure 5.	Likelihood to choose democratic rule across subgroups
xtmixed dem100 i.bonus##i.type bonus_order  || id: ,  reml

eststo dem1: xtmixed dem100 i.bonus##i.extrinsic_leader bonus_order || id: if rule_base==1, vce(robust)
matrix N_g = e(N_g)
estadd local individuals = N_g[1,1]
eststo dem2: xtmixed dem100 i.bonus##i.extrinsic_leader bonus_order || id: if rule_base>1, vce(robust)
matrix N_g = e(N_g)
estadd local individuals = N_g[1,1]

eststo fake1: xtmixed fake100 i.bonus##i.extrinsic_leader bonus_order || id: if rule_base==1, vce(robust)
matrix N_g = e(N_g)
estadd local individuals = N_g[1,1]
eststo fake2: xtmixed fake100 i.bonus##i.extrinsic_leader bonus_order || id: if rule_base>1, vce(robust)
matrix N_g = e(N_g)
estadd local individuals = N_g[1,1]

eststo dic1: xtmixed dic100 i.bonus##i.extrinsic_leader bonus_order || id: if rule_base==1, vce(robust)
matrix N_g = e(N_g)
estadd local individuals = N_g[1,1]
eststo dic2: xtmixed dic100 i.bonus##i.extrinsic_leader bonus_order || id: if rule_base>1, vce(robust)
matrix N_g = e(N_g)
estadd local individuals = N_g[1,1]

*Figure 4: Heterogeneous responses (colors from global palette)
* Note: mlabel shows coefficient in pp + stars + p-value; notes include subgroup N
preserve

coefplot(dem1, label(Baseline: Democratic (N=41)) mcolor("57 106 177") ciopts(lwidth(0.8 2) lcolor("57 106 177*1" "57 106 177*.3") recast(rcap))) (dem2, label(Baseline: Pseudo or Autocratic (N=23)) mcolor("128 100 162") ciopts(lwidth(0.8 2) lcolor("128 100 162*1" "128 100 162*.3") recast(rcap))),  bylabel(Democratic Rule) || (fake1, mcolor("57 106 177") ciopts(lwidth(0.8 2) lcolor("57 106 177*1" "57 106 177*.3") recast(rcap))) (fake2, mcolor("128 100 162") ciopts(lwidth(0.8 2) lcolor("128 100 162*1" "128 100 162*.3") recast(rcap))),bylabel(Pseudo-Democratic Rule) || (dic1, mcolor("57 106 177") ciopts(lwidth(0.8 2) lcolor("57 106 177*1" "57 106 177*.3") recast(rcap))) (dic2, mcolor("128 100 162") ciopts(lwidth(0.8 2) lcolor("128 100 162*1" "128 100 162*.3") recast(rcap))),  bylabel(Autocratic Rule) ||, xla(,labsize(6pt) nogrid) byopts(compact  imargin(*1.1) rows(1))  keep(1.bonus 1.extrinsic_leader 1.bonus#1.extrinsic_leader) coeflabels( 1.bonus = "Payment: Combined" 1.extrinsic_leader = "Motivation: Extrinsic" 1.bonus#1.extrinsic_leader = "Payment*Extrinsic", labsize(6pt))  xline(0, lpattern(dash) lcolor(gs10)) xtitle("Regression estimated impact relative to baseline in %-points", size(6pt)) grid(none) levels(95 90) mlabel(cond(@pval<.1, string(@b,"%3.1f") + cond(@pval<.01, "***", cond(@pval<.05, "**", cond(@pval<.1, "*", ""))) + " (p=" + string(@pval,"%4.2f") + ")", "")) msize(3pt) msymbol(D) mlabsize(5.5pt) mlabposition(12) mlabgap(1.2)  subtitle(, size(9pt) lstyle(none) margin(medium) nobox justification(center) alignment(top) bmargin(top))  xsize(5) ysize(2) ciopts(lwidth(0.8 2) recast(rcap)) legend(size(6pt)) aspectratio(0.7) p1(mlabcolor("57 106 177")) p2(mlabcolor("128 100 162"))
gr save  "$working_ANALYSIS/results/intermediate/figure5_heterogeneous_responses.gph", replace
gr export  "$working_ANALYSIS/results/figures/figure5_heterogeneous_responses.png", replace width(4000)
restore


*Table S9.	Subsample treatment effects on democratic rule choices
esttab dem1 fake1 dic1 dem2 fake2 dic2 using "$working_ANALYSIS/results/tables/tableS11_subsample_effects.tex",  keep(1.bonus 1.extrinsic_leader 1.bonus#1.extrinsic_leader)  label se(%4.3f)  transform(ln*: exp(@) exp(@)) mgroups("Baseline: Democratic rule" "Baseline: Pseudo-democratic or autocratic rule", pattern(1 0 0 1 0 0))   mtitles("Democratic" "Pseudo-democratic" "Autocratic" "Democratic" "Pseudo-democratic" "Autocratic") b(%4.3f) stats(N individuals chi2 p, labels("N" "Individuals" "Wald Chi2" "p" ) fmt(%4.0f %4.0f %4.2f)) star(* 0.10 ** 0.05 *** 0.01) booktabs nonotes addnotes("Notes: Estimates are from multilevel panel regressions that account for the grouping structure of the data at the individual level (random intercept). Standard errors with stars indicating the following significant levels: * p < 0.10, ** p < 0.05, *** p < 0.01") replace


** EOF
