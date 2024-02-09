*--------------------------------------------------------------------
* SCRIPT: 03_analysis.do
* PURPOSE: replicates the tables and figures and saves the results
*--------------------------------------------------------------------

*--------------------------------------------------
* STRUCTURE OF THE DO-FILE
/*

	1) Analysis for main paper
	2) Analysis for supplementary online materials
*/
*--------------------------------------------------


*--------------------------------------------------
* (1) Analysis for main paper
*--------------------------------------------------

*Figure 1.	Preference for democratic governance in study region
clear
use "$working_ANALYSIS/data/afro_r6.dta"
keep if COUNTRY==22
rename *, lower


gen ohangwena= 0
replace ohangwena = 1 if region == 588
lab def ohang 0 "Rest of Namibia" 1 "Ohangwena", replace
lab val ohangwena ohang
*SUPPORT FOR DEMOCRACY
tab q30, nolab
lab def democracy 1 "Doesn't matter for someone like me" 2 "Non-democratic sometimes preferable" 3 "Democracy always preferable", replace 
lab val q30 democracy
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
gen study_sites = 0
replace study_sites = 1 if ohangwena==1
replace study_sites = 2 if constituency==9 | constituency==43 | constituency==71
lab def studies 0 "Rest of Namibia (n=1058)" 1 "Rest of Ohangwena (n=76)" 2 "Study sites (n=38)", replace
lab val study_sites studies
mylabels 0(20)100, myscale(@) local(pctlabel) suffix("%") 
catplot q30, over(study_sites) asyvars stack horizontal recast(bar) yla(`pctlabel', nogrid) percent(study_sites) bargap(10) blabel(bar, format(%9.0f) pos(center)  gap(0.1)) title("Which of these three statements is closest to your own opinion?")  b1title("") legend(ring(1) pos(6) rows(3)) xsize(3.465) ysize(2) scale(1.3)
gr save  "$working_ANALYSIS/results/intermediate/figure1_afro.gph", replace
gr export "$working_ANALYSIS/results/figures/figure1_afro.tif", replace width(4000)
ranksum q30 if study_sites!=1, by(study_sites)
ranksum q30 if study_sites!=0, by(study_sites)

reg q52b i.study_sites, vce(hc3)
ranksum q52b if study_sites!=1, by(study_sites)
ranksum q71a if study_sites!=1, by(study_sites)


*Figure 2.	Procedural fairness task
*created in powerpoint.


*Figure 3.	Normative expectations regarding group decision-making
clear all
use "$working_ANALYSIS/processed/analysis_wide.dta"
* Please indicate for each of the statement whether you disagree strongly, disagree a little, neither agree nor disagree, agree a little, or strongly agree
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
lab def villy1 0 "Villager" 1 "Intrinsic leader" 2 "Extrinsic leader", replace
lab val types villy1

*graphical presentation
preserve
rename new_c1 norm1
rename new_c3 norm2
rename new_c8 norm3


reshape long norm ,i(id) j(pr_id)
label define pr1 1 "Group voting" 2 "Elections" 3 "Inclusive"
label values pr_id pr1

cibar norm, over1(types) over2(pr_id) bargap(0) gap(80) barlabel(on) blpos(11) blgap(0.01) blfmt(%9.1f) graphopts(legend() xsize(4) ysize(2) yla(1(1)5)  scale(1.2)  ytitle("Mean") note("{bf: Group voting}='For important decisions members of a group should be allowed to vote.'" "{bf: Elections}='Democratic elections in this village ensure that the elected authorities act in the interest of their people.'" "{bf: Inclusive}='A leader hast to serve all people including those who did not vote for him/her or are not friends.'")) ciopts(lpattern(dash) lcolor(black))
gr save "$working_ANALYSIS/results/intermediate/figure3_normative_expectations.gph", replace
gr export "$working_ANALYSIS/results/figures/figure3_normative_expectations.tif", replace width(3465)
restore

ttest new_c1 if types!= 2, by(types)
ranksum new_c1 if types!= 1, by(types)
ranksum new_c1 if types!= 0, by(types)
reg new_c1 i.types
testparm 1.types 2.types, equal
reg new_c3 i.types, vce(hc3)
testparm 1.types 2.types, equal
reg new_c8 i.types, vce(hc3)
testparm 1.types 2.types, equal


*normative expecation related to group voting by leader baseline choice
reg new_c1 i.rule_base3 if villager==1, vce(hc3)







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


* Small and Large bonus seperately
eststo reg_dem1: xtprobit dem bonus_2 bonus_3 , vce(robust)
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
coefplot (pr1, offset(0.2) msymbol(D)) (pr2, offset(0) msymbol(S)) (pr3, offset(-0.2) msymbol(O)) (pr4, offset(0.2) msymbol(D) mcolor("100 143 255") ciopts(lwidth(0.3 1) lcolor("100 143 255*.8" "100 143 255*.2")  recast(rcap))) (pr5, offset(0) msymbol(S) mcolor("120 94 240") ciopts(lwidth(0.3 1) color("120 94 240*.8" "120 94 240*.2")  recast(rcap)))  (pr6, offset(-0.2) msymbol(O) mcolor("220 38 127") ciopts(lwidth(0.3 1) color("220 38 127*.8" "220 38 127*.2")  recast(rcap))), keep(bonus_2 bonus_3 bonus) coeflabels(bonus = "Pooled Bonus" bonus_2 = "Small bonus" bonus_3="Large bonus") xline(0, lpattern(dash) lcolor(gs3)) xtitle("Regression estimated impact relative to baseline in pp") msize(3pt) xla(-0.3(0.1)0.3, nogrid) grid(none) levels(95 90) ciopts(lwidth(0.3 1) lcolor(*.8 *.2) recast(rcap)) mlabel(cond(@pval<.01, "***", cond(@pval<.05, "**", cond(@pval<.1, "*", "") )))  mlabsize(7pt)  mlabposition(12) mlabgap(-0.1)  legend(order(3 "Democratic" 6 "Pseudo-democratic" 9 "Autocratic") pos(6) ring(1) rows(1) bmargin(small)) graphregion(margin(tiny)) xsize(3) ysize(2) aspect(0.5)
gr save "$working_ANALYSIS/results/intermediate/figure4_treatment_effects.gph", replace 
gr export "$working_ANALYSIS/results/figures/figure4_treatment_effects.tif", replace width(4000)

*Table S5.	Main effects of bonus payment 
esttab reg_dem1 reg_fake1 reg_dic1 reg_dem2 reg_fake2 reg_dic2 using "$working_ANALYSIS\results\Tables\tableS5_main_effects.rtf",  ci transform(ln*: exp(@) exp(@))mtitles("Democratic" "Pseudo-democratic" "Dictator" "Democratic" "Pseudo-democratic" "Dictator") b(%4.2f) eqlabels("" "lnsig2u", none) stats(N N_g sigma_u rho chi2 p, labels("N" "Individuals" "sigma_u" "rho" "Wald Chi2" "p" ) fmt(%4.0f %4.0f %4.2f)) star(* 0.10 ** 0.05 *** 0.01) varlabels(,elist(weight:_cons "{break}{hline @width}")) replace 




*Table 2.	Observed crowding effects of conditional monetary incentives in the experiment.
*created in word, descriptives are calculated here

tab rule type if round==2, column


*Figure 5.	Likelihood to choose democratic rule across subgroups
eststo sub1: xtmixed dem100 i.bonus bonus_order  || id: if type==1,  reml
matrix N_g = e(N_g)
local individuals = N_g[1, 1]
estadd local individuals `individuals'
eststo sub2: xtmixed dem100 i.bonus bonus_order  || id: if type==2,  reml
matrix N_g = e(N_g)
local individuals = N_g[1, 1]
estadd local individuals `individuals' 
eststo sub3: xtmixed dem100 i.bonus bonus_order  || id: if type==3,  reml
matrix N_g = e(N_g)
local individuals = N_g[1, 1]
estadd local individuals `individuals'
eststo sub4: xtmixed dem100 i.bonus bonus_order  || id: if type==4,  reml
matrix N_g = e(N_g)
local individuals = N_g[1, 1]
estadd local individuals `individuals'


coefplot (sub1, label (Box I)) (sub2, label (Box III))  (sub3, label (Box II))  (sub4, label (Box IV)) , keep(1.bonus) coeflabels(1.bonus = "Pooled Bonus")  byopts(xrescale) xline(0, lpattern(dash) lcolor(gs3)) xtitle("Regression estimated impact relative to baseline in pp") grid(none) levels(95 90) ciopts(lwidth(0.8 2)  lcolor(*1 *.3) recast(rcap)) mlabel(cond(@pval<.01, "***", cond(@pval<.05, "**", cond(@pval<.1, "*", "")))) msize(3pt) msymbol(D) mlabsize(8pt) mlabposition(1) mlabgap(-0.4) subtitle(, size(6pt) lstyle(none) margin(medium) nobox justification(left)  bmargin(top))  xla(-50(25)75, nogrid)  xsize(3) ysize(2) scale(1.2) aspect(0.5)
gr_edit style.editstyle margin(vsmall) editcopy
gr save  "$working_ANALYSIS\results\intermediate\figure5_boxes_subgroups.gph", replace
gr export "$working_ANALYSIS\results\figures\figure5_boxes_subgroups.tif", replace width(4000) 


*Table S9.	Subsample treatment effects on democratic rule choices
esttab sub1 sub2 sub3 sub4 using "$working_ANALYSIS\results\tables\tableS9_social_norms_boxes_democratic.rtf",  label se(%4.3f)  transform(ln*: exp(@) exp(@)) mgroups("Democratic Rule (=1)", pattern(1 0 0))   mtitles("Box 1" "Box 2" "Box 3" "Box 4") b(%4.3f) stats(N individuals chi2 p, labels("N" "Individuals" "Wald Chi2" "p" ) fmt(%4.0f %4.0f %4.2f)) star(* 0.10 ** 0.05 *** 0.01) varlabels(,elist(weight:_cons "{break}{hline @width}")) nonotes addnotes("Notes: Estimates are from multilevel panel regressions that account for the grouping structure of the data at the individual level (random intercept). Standard errors with stars indicating the following significant levels: * p < 0.10, ** p < 0.05, *** p < 0.01") replace 


*Figure 6.	Villagers' satisfaction with their leader’s performance outside the lab
preserve
clear all
use "$working_ANALYSIS/processed/analysis_wide.dta"

bysort group_leader_type: sum satisfaction_leader
eststo reg_satisfaction: reg satisfaction_leader rule_base2 rule_base3 i.village, vce(robust)


coefplot (reg_satisfaction), keep(rule_base2 rule_base3) coeflabels(rule_base2 = "Baseline: Pseudo-democratic rule" rule_base3 = "Baseline: Autocratic rule") xline(0, lpattern(dash) lcolor(gs3))  xtitle("Regression estimated imppact relative to 'Baseline: Democratic rule'") xla(-1(0.25)0.25, nogrid) grid(none) levels(95 90) ciopts(lwidth(0.3 1) lcolor(*.8 *.2)   recast(rcap)) mlabel(cond(@pval<.01, "***", cond(@pval<.05, "**", cond(@pval<.1, "*", "")))) mlabsize(9pt) mlabposition(12) mlabgap(-0.1)  xsize(3) ysize(2) scale(1.2) aspect(0.6)
gr save "$working_ANALYSIS/results/intermediate/figure6_satisfaction_villagers.gph", replace
gr export "$working_ANALYSIS/results/figures/figure6_satisfaction_villagers.tif", replace width(3465)

*Table S11.	Villager satisfaction with different leader types
esttab reg_satisfaction using "$working_ANALYSIS\results\Tables\tableS11.rtf", keep(rule_base2 rule_base3) ci transform(ln*: exp(@) exp(@))mtitles("Satisfaction") b(%4.2f) eqlabels("" "lnsig2u", none) stats(N N_g r2 r2_a , labels("N" "Individuals" "R-squared" "Adjusted R-squared") fmt(%4.0f %4.0f %4.2f)) star(* 0.10 ** 0.05 *** 0.01) varlabels(,elist(weight:_cons "{break}{hline @width}")) replace 


restore





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
gr export "$working_ANALYSIS/results/figures/figureS1_MDES.tif", replace width(4000)


*Figure S2.	Equivalence tests
*created in R with the openly accessible  package ‘TOSTER’ by Caldwell (2022).


preserve
clear all
use "$working_ANALYSIS/processed/analysis_wide.dta"


* Table S2.	Participants characteristics
global controls2 male age education rootedness pca_wealth d3a d3c 
iebaltab $controls2, grpvar(id1) rowvarlabels format(%9.2f) stdev ftest fmissok tblnonote save("$working_ANALYSIS/results/tables/tableS2_sample_characteristics.xlsx") replace
prtest male, by(villager)
foreach x of varlist age education rootedness pca_wealth d3a d3c {
	ranksum `x', by(villager)
	}
reg id1 $controls2 if id1 !=0 // chiefs vs villagers
reg id1 $controls2 if id1 !=1 // DEL vs villagers
* Leaders only
global controls1 male age education rootedness pca_wealth d3a d3c  married experience motivation helpauthentic
iebaltab $controls1, grpvar(TL) rowvarlabels format(%9.2f) stdev ftest fmissok tblnonote save("$working_ANALYSIS/results/tables/tableS2_leaders_characteristics.xlsx") replace
reg TL $controls1


*Table S3.	Summary statistics of  main outcomes for leaders
* Stage 1 rule choices by leaders
estpost tabulate rule1 
esttab using "$working_ANALYSIS/results/tables/tableS3_outcome-variables.rtf", cell("b pct(fmt(0)) cumpct(fmt(0))") unstack noobs replace
estpost tabulate rule5
esttab using "$working_ANALYSIS/results/tables/tableS3_outcome-variables.rtf", cell("b pct(fmt(0)) cumpct(fmt(0))") unstack noobs append
estpost tabulate rule6
esttab using "$working_ANALYSIS/results/tables/tableS3_outcome-variables.rtf", cell("b pct(fmt(0)) cumpct(fmt(0))") unstack noobs append

* Stage 2 distribution decisions by leaders
estpost tabulate  vote1  if id <65 & rule1==1
bysort rule1: tab vote1 if id <65
bysort rule5: tab vote5 if id <65
bysort rule6: tab vote6 if id <65

restore


*Figure S3.	Order effects on rule choice in bonus rounds
bysort bonus_size bonus_order: tab rule
catplot rule bonus_order if decision > 1, over(bonus_size) percent(bonus_order bonus_size ) asyvar recast(bar) yla(0(20)100) blabel(bar, format(%9.0f) pos(center)  gap(0.1)) b1title("") legend(ring(0) pos(12) rows(1))
gr export "$working_ANALYSIS/results/figures/figureS3_order_effects.tif", replace width(5800)

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

esttab reg_time1 reg_time2 reg_time3 reg_time4 reg_time5 reg_time6 using "$working_ANALYSIS\results\Tables\tableS4_order_effects.rtf", keep(2.round 3.round _cons) label se(%4.3f)  transform(ln*: exp(@) exp(@)) mgroups("Order 1: Base-Large-Small" "Order 2: Base-Small-Large", pattern(1 0 0 1 0 0)) mtitles("Democratic" "Pseudo-democratic" "Dictator" "Democratic" "Pseudo-democratic" "Dictator") b(%4.3f) stats(N N_clust r2_o joint_test, fmt(0 0 3 3) labels("Observations" "Cluster" "Overall R-squared" "Test: Round 2 = Round 3 (chi2)" )) star(* 0.10 ** 0.05 *** 0.01) varlabels(,elist(weight:_cons "{break}{hline @width}")) nonotes addnotes("Notes: Robust standard errors in brackets: * p < 0.10, ** p < 0.05, *** p < 0.01.") replace 


*Table S5. Main treatment effects
*stored before in main analysis

*Table S6.	Treatment effects seperately for chiefs and DELs
*DEL only
eststo reg_del1: xtprobit dem bonus bonus_order if TL==0, vce(robust)
eststo reg_del2: xtprobit fake bonus bonus_order if TL==0, vce(robust)
eststo reg_del3: xtprobit dic bonus bonus_order if TL==0, vce(robust)
*TL only
eststo reg_tl1: xtprobit dem bonus bonus_order if TL==1, vce(robust)
eststo reg_tl2: xtprobit fake bonus bonus_order if TL==1, vce(robust)
eststo reg_tl3: xtprobit dic bonus bonus_order if TL==1, vce(robust)

esttab reg_del1 reg_del2 reg_del3 reg_tl1 reg_tl2 reg_tl3 using "$working_ANALYSIS\results\Tables\tableS6_chiefs_DEL.rtf",  ci transform(ln*: exp(@) exp(@))mtitles("Democratic" "Pseudo-democratic" "Dictator" "Democratic" "Pseudo-democratic" "Dictator") b(%4.2f) eqlabels("" "lnsig2u", none) stats(N N_g sigma_u rho chi2 p, labels("N" "Individuals" "sigma_u" "rho" "Wald Chi2" "p" ) fmt(%4.0f %4.0f %4.2f)) star(* 0.10 ** 0.05 *** 0.01) varlabels(,elist(weight:_cons "{break}{hline @width}")) replace 


*Table S7.	Robustness check – Mixed effects regression
* Treating participant as a random factor or use a simple linear model
eststo reg_rb1: xtmixed dem bonus_2 bonus_3 bonus_order || id:, reml
matrix N_g = e(N_g)
local individuals = N_g[1, 1]
estadd local individuals `individuals'´
eststo reg_rb2: xtmixed fake bonus_2 bonus_3 bonus_order || id:,  reml
matrix N_g = e(N_g)
local individuals = N_g[1, 1]
estadd local individuals `individuals'´
eststo reg_rb3: xtmixed dic bonus_2 bonus_3 bonus_order || id:,  reml 
matrix N_g = e(N_g)
local individuals = N_g[1, 1]
estadd local individuals `individuals'´

esttab reg_rb1 reg_rb2 reg_rb3 using "$working_ANALYSIS\results\Tables\tableS7_rc_xtmixed.rtf",  ci transform(ln*: exp(@) exp(@))mtitles("Democratic" "Pseudo-democratic" "Dictator") b(%4.2f) eqlabels("" "sd(_cons)" "sd(Residual)", none) stats(N individuals chi2 p, labels("N" "Individuals" "Wald Chi2" "p" ) fmt(%4.0f %4.0f %4.2f)) star(* 0.10 ** 0.05 *** 0.01) varlabels(,elist(weight:_cons "{break}{hline @width}"))  replace 


*Table S8.	Effects of bonus payments – Multinomial probit
mprobit rule bonus_2 bonus_3 bonus_order, cluster(id)
margins, dydx(bonus_2 bonus_3) post
est store bonus1
outreg2 using  "$working_ANALYSIS/results/tables/tableS8_multi_probit", drop() addstat(Pseudo R-squared, e(r2_p))  adec(3) dec(3) word  replace
mprobit rule bonus bonus_order, cluster(id) level(90)
margins, dydx(bonus) post
est store bonus2
outreg2 using "$working_ANALYSIS/results/tables/tableS8_multi_probit", drop() addstat(Pseudo R-squared, e(r2_p))  adec(3) dec(3) word  append



*Table S9.	Subsample treatment effects on democratic rule choices
*stored before under main analysis


*Table S10.	Robustness check: Authentic leadership score median split

eststo sub1_rc: xtmixed dem100 i.bonus bonus_order  || id: if type_AL==1,  reml
matrix N_g = e(N_g)
local individuals = N_g[1, 1]
estadd local individuals `individuals'
eststo sub2_rc: xtmixed dem100 i.bonus bonus_order  || id: if type_AL==2,  reml
matrix N_g = e(N_g)
local individuals = N_g[1, 1]
estadd local individuals `individuals' 
eststo sub3_rc: xtmixed dem100 i.bonus bonus_order  || id: if type_AL==3,  reml
matrix N_g = e(N_g)
local individuals = N_g[1, 1]
estadd local individuals `individuals'
eststo sub4_rc: xtmixed dem100 i.bonus bonus_order  || id: if type_AL==4,  reml
matrix N_g = e(N_g)
local individuals = N_g[1, 1]
estadd local individuals `individuals'


esttab sub1_rc sub2_rc sub3_rc sub4_rc using "$working_ANALYSIS\results\tables\tableS10_social_norms_boxes_democratic_rc.rtf",  label se(%4.3f)  transform(ln*: exp(@) exp(@)) mgroups("Democratic Rule (=1)", pattern(1 0 0))   mtitles("Box 1" "Box 2" "Box 3" "Box 4") b(%4.3f) stats(N individuals chi2 p, labels("N" "Individuals" "Wald Chi2" "p" ) fmt(%4.0f %4.0f %4.2f)) star(* 0.10 ** 0.05 *** 0.01) varlabels(,elist(weight:_cons "{break}{hline @width}")) nonotes addnotes("Notes: Estimates are from multilevel panel regressions that account for the grouping structure of the data at the individual level (random intercept). Standard errors with stars indicating the following significant levels: * p < 0.10, ** p < 0.05, *** p < 0.01") replace 


*Figure S4.	Subsample treatment effects: Robustness check with authentic leadership
coefplot (sub1_rc, label (Box I)) (sub2_rc, label (Box III))  (sub3_rc, label (Box II))  (sub4_rc, label (Box IV)) , keep(1.bonus) coeflabels(1.bonus = "Pooled Bonus")  byopts(xrescale) xline(0, lpattern(dash) lcolor(gs3)) xtitle("Regression estimated impact relative to baseline in pp") grid(none) levels(95 90) ciopts(lwidth(0.8 2)  lcolor(*1 *.3) recast(rcap)) mlabel(cond(@pval<.01, "***", cond(@pval<.05, "**", cond(@pval<.1, "*", "")))) msize(3pt) msymbol(D) mlabsize(8pt) mlabposition(1) mlabgap(-0.4) subtitle(, size(6pt) lstyle(none) margin(medium) nobox justification(left)  bmargin(top))  xla(-50(25)75, nogrid)  xsize(3.465) ysize(2) scale(1.2)
gr_edit style.editstyle margin(vsmall) editcopy
gr save  "$working_ANALYSIS\results\intermediate\figureS4_subsample_democratic.gph", replace
gr export "$working_ANALYSIS\results\figures\figureS4_subsample_democratic.tif", replace width(4000) 


*Table S11.	Villager satisfaction with different leader types
*stored before in main analysis



preserve
clear all
use "$working_ANALYSIS/processed/analysis_wide.dta"
* Figure S5.	Preferred distribution by leaders and villagers in the baseline
*PANEL A: Leaders
catplot vote1 , over(rule_base) asyvar recast(bar) yla(0(20)100, nogrid) percent(rule_base) bargap(10) blabel(bar, format(%9.0f) pos(outside)  gap(0.1)) title("{bf:A} Leaders")  b1title("") legend(ring(1) pos(6) rows(1))
gr save  "$working_ANALYSIS/results/intermediate/figureS5a.gph", replace

*PANEL B: Villagers
catplot vote1 if id>64, asyvar recast(bar) yla(0(20)100, nogrid) percent bargap(10)  blabel(bar, format(%9.0f)pos(outside)  gap(0.1)) title("{bf:B} Villagers")  b1title("") legend(ring(1) pos(6) rows(1))
gr save  "$working_ANALYSIS/results/intermediate/figureS5b.gph", replace

grc1leg  "$working_ANALYSIS/results/intermediate/figureS5a.gph" "$working_ANALYSIS/results/intermediate/figureS5b.gph", rows(1) pos(6) ring(1) 
gr save "$working_ANALYSIS/results/intermediate/figureS5_stage2.gph", replace
gr export "$working_ANALYSIS/results/figures/figureS5_stage2.tif", replace width(4000)

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
margin, dydx(*) post
est store payoff1

esttab reg_stage2 using "$working_ANALYSIS\results\Tables\tableS9.rtf",  ci transform(ln*: exp(@) exp(@))mtitles("Distribution B (=1)") b(%4.2f) eqlabels("" "lnsig2u", none) stats(N N_g sigma_u rho chi2 p, labels("N" "Individuals" "sigma_u" "rho" "Wald Chi2" "p" ) fmt(%4.0f %4.0f %4.2f)) star(* 0.10 ** 0.05 *** 0.01) varlabels(,elist(weight:_cons "{break}{hline @width}")) replace 


*Figure S6.	Likelihood of distribution B being implemented
coefplot (payoff1, msymbol(d)), keep(bonus baseline_2 baseline_3 bonus_pseudo bonus_dictator) coeflabels(bonus= "Pooled Bonus" baseline_2 = "Base: Pseudo-democrat" baseline_3 ="Base: Autocrat" bonus_pseudo = "Interaction:Bonus*Pseudo" bonus_dictator = "Interaction: Bonus*Autocrat") xline(0, lpattern(dash) lcolor(gs3))  xtitle("Regression estimated impact relative to baseline democrats in pp") msymbol(d) xla(-1(0.25)1, nogrid) grid(none) levels(95 90) ciopts(lwidth(0.3 1) lcolor(*.8 *.2)  recast(rcap)) mlabel(cond(@pval<.01, "***", cond(@pval<.05, "**", cond(@pval<.1, "*", "")))) mlabsize(8pt) mlabposition(12) mlabgap(-0.1) 
gr save "$working_ANALYSIS/results/intermediate/figureS7.gph", replace
gr export "$working_ANALYSIS/results/figures/figureS7.tif", replace width(3465)




** EOF