*--------------------------------------------------------------------
* SCRIPT: 05_robustness_motivation.do
* PURPOSE: Robustness check for R&R (Ecological Economics)
*          Addressing Reviewer 2 comment on "run for office" wording
*          not applying to chiefs (Traditional Leaders, TL).
*          Replicates motivation heterogeneity analysis (Figure 5 /
*          Table S9) restricted to DELs only, then chiefs only,
*          then a triple-interaction test.
* REVIEWER: R2.2a/R2.2b — "Does this wording make sense for the chiefs?"
* OUTPUT:
*   tableRC2_boxes_DEL_only.tex           (boxes analysis, DEL only)
*   tableRC3_boxes_chiefs_only.tex        (boxes analysis, chiefs only)
*   tableRC4_triple_interaction.tex       (bonus x extrinsic x TL)
*--------------------------------------------------------------------

* Assumes $working_ANALYSIS is set by run.do

*--------------------------------------------------
* Setup
*--------------------------------------------------
clear all
set more off
cap mkdir "$working_ANALYSIS/results/tables"

use "$working_ANALYSIS/processed/analysis_long.dta"
xtset id decision

* Generate scaled outcome (0-100) to match main analysis convention
cap drop dem100
gen dem100 = dem * 100

*==================================================================
* SECTION 0: Verify key variables exist and check coding
*==================================================================

* Confirm TL variable (0=DEL, 1=Traditional Leader/chief)
assert !missing(TL)
tab TL, m
* Expected: 32 DELs (TL=0), 32 chiefs (TL=1)

* Confirm motivation variable — may be named extrinsic_leader or extrinsic
* In analysis_long.dta the variable surviving the reshape should be extrinsic_leader
cap confirm var extrinsic_leader
if _rc != 0 {
    cap confirm var extrinsic
    if _rc != 0 {
        di as error "ERROR: Neither extrinsic_leader nor extrinsic found."
        error 111
    }
    else {
        gen extrinsic_leader = extrinsic
        di as text "Note: using extrinsic as extrinsic_leader"
    }
}

* Confirm type variable (4 boxes)
cap confirm var type
if _rc != 0 {
    di as error "ERROR: type variable not found — run 02_generate.do first."
    error 111
}

di as text "Variable check passed."


*==================================================================
* SECTION 1: Descriptives — Motivation by leader type
*==================================================================

di as text _newline "=== SECTION 1: MOTIVATION DISTRIBUTION BY LEADER TYPE ==="

* Overall motivation distribution
di as text _newline "-- Full sample --"
tab extrinsic_leader if decision==1, m

* By leader type (baseline round only, one obs per leader)
di as text _newline "-- DEL only (TL==0) --"
tab extrinsic_leader if TL==0 & decision==1, m

di as text _newline "-- Chiefs only (TL==1) --"
tab extrinsic_leader if TL==1 & decision==1, m

* Chi-square test: is motivation distribution different between DELs and chiefs?
di as text _newline "-- Chi-square test: motivation independent of leader type? --"
tab extrinsic_leader TL if decision==1, chi2 exact

* Box distribution by leader type
di as text _newline "-- Box (type) distribution by leader type --"
tab type TL if decision==1, m

* Note N per box within each leader type
di as text _newline "-- N per box: DEL only --"
tab type if TL==0 & decision==1

di as text _newline "-- N per box: Chiefs only --"
tab type if TL==1 & decision==1

* Baseline rule choice by leader type
di as text _newline "-- Baseline rule choice by leader type --"
tab rule_base TL if decision==1, chi2 exact


*==================================================================
* SECTION 2: Replication of Table S9 / Figure 5 — DEL only (TL==0)
*==================================================================

di as text _newline "=== SECTION 2: BOXES ANALYSIS — DEL ONLY (N=32) ==="

* Box I: Non-democratic baseline, intrinsic motivation, DEL only
cap eststo del_sub1: xtmixed dem100 i.bonus bonus_order || id: ///
    if type==1 & TL==0, reml
if _rc==0 {
    matrix N_g = e(N_g)
    local individuals = N_g[1,1]
    estadd local individuals `individuals'
    di as text "Box I (non-dem, intrinsic) DEL: N_leaders=" `individuals'
}
else {
    di as text "Box I DEL: too few obs for xtmixed"
}

* Box III: Non-democratic baseline, extrinsic motivation, DEL only  [KEY FINDING BOX]
cap eststo del_sub2: xtmixed dem100 i.bonus bonus_order || id: ///
    if type==2 & TL==0, reml
if _rc==0 {
    matrix N_g = e(N_g)
    local individuals = N_g[1,1]
    estadd local individuals `individuals'
    di as text "Box III (non-dem, extrinsic) DEL: N_leaders=" `individuals'
}
else {
    di as text "Box III DEL: too few obs for xtmixed"
}

* Box II: Democratic baseline, intrinsic motivation, DEL only
cap eststo del_sub3: xtmixed dem100 i.bonus bonus_order || id: ///
    if type==3 & TL==0, reml
if _rc==0 {
    matrix N_g = e(N_g)
    local individuals = N_g[1,1]
    estadd local individuals `individuals'
    di as text "Box II (dem, intrinsic) DEL: N_leaders=" `individuals'
}
else {
    di as text "Box II DEL: too few obs for xtmixed"
}

* Box IV: Democratic baseline, extrinsic motivation, DEL only
cap eststo del_sub4: xtmixed dem100 i.bonus bonus_order || id: ///
    if type==4 & TL==0, reml
if _rc==0 {
    matrix N_g = e(N_g)
    local individuals = N_g[1,1]
    estadd local individuals `individuals'
    di as text "Box IV (dem, extrinsic) DEL: N_leaders=" `individuals'
}
else {
    di as text "Box IV DEL: too few obs for xtmixed"
}

cap esttab del_sub1 del_sub2 del_sub3 del_sub4 ///
    using "$working_ANALYSIS/results/tables/tableRC2_boxes_DEL_only.tex", ///
    b(%4.3f) se(%4.3f) ///
    mtitles("Box I" "Box III" "Box II" "Box IV") ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    stats(N individuals, labels("Observations" "Leaders") fmt(%4.0f %4.0f)) ///
    booktabs ///
    title("Heterogeneous treatment effects by motivation type, DEL subsample (N=32)") ///
    addnotes("Multilevel panel regressions (random intercept at individual level)." ///
    "DEL subsample only. Standard errors in parentheses." ///
    "* p<0.10, ** p<0.05, *** p<0.01.") ///
    replace

di as text "Table RC2 saved."


*==================================================================
* SECTION 3: Replication of Table S9 / Figure 5 — Chiefs only (TL==1)
*==================================================================

di as text _newline "=== SECTION 3: BOXES ANALYSIS — CHIEFS ONLY (N=32) ==="

cap eststo tl_sub1: xtmixed dem100 i.bonus bonus_order || id: ///
    if type==1 & TL==1, reml
if _rc==0 {
    matrix N_g = e(N_g)
    local individuals = N_g[1,1]
    estadd local individuals `individuals'
    di as text "Box I (non-dem, intrinsic) Chiefs: N_leaders=" `individuals'
}
else {
    di as text "Box I Chiefs: too few obs for xtmixed"
}

cap eststo tl_sub2: xtmixed dem100 i.bonus bonus_order || id: ///
    if type==2 & TL==1, reml
if _rc==0 {
    matrix N_g = e(N_g)
    local individuals = N_g[1,1]
    estadd local individuals `individuals'
    di as text "Box III (non-dem, extrinsic) Chiefs: N_leaders=" `individuals'
}
else {
    di as text "Box III Chiefs: too few obs for xtmixed"
}

cap eststo tl_sub3: xtmixed dem100 i.bonus bonus_order || id: ///
    if type==3 & TL==1, reml
if _rc==0 {
    matrix N_g = e(N_g)
    local individuals = N_g[1,1]
    estadd local individuals `individuals'
    di as text "Box II (dem, intrinsic) Chiefs: N_leaders=" `individuals'
}
else {
    di as text "Box II Chiefs: too few obs for xtmixed"
}

cap eststo tl_sub4: xtmixed dem100 i.bonus bonus_order || id: ///
    if type==4 & TL==1, reml
if _rc==0 {
    matrix N_g = e(N_g)
    local individuals = N_g[1,1]
    estadd local individuals `individuals'
    di as text "Box IV (dem, extrinsic) Chiefs: N_leaders=" `individuals'
}
else {
    di as text "Box IV Chiefs: too few obs for xtmixed"
}

cap esttab tl_sub1 tl_sub2 tl_sub3 tl_sub4 ///
    using "$working_ANALYSIS/results/tables/tableRC3_boxes_chiefs_only.tex", ///
    b(%4.3f) se(%4.3f) ///
    mtitles("Box I" "Box III" "Box II" "Box IV") ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    stats(N individuals, labels("Observations" "Leaders") fmt(%4.0f %4.0f)) ///
    booktabs ///
    title("Heterogeneous treatment effects by motivation type, chiefs subsample (N=32)") ///
    addnotes("Multilevel panel regressions (random intercept at individual level)." ///
    "Chiefs subsample only. Standard errors in parentheses." ///
    "* p<0.10, ** p<0.05, *** p<0.01.") ///
    replace

di as text "Table RC3 saved."


*==================================================================
* SECTION 4: Pseudo-democratic rule choice by box and leader type
* (Secondary outcome — the key finding is pseudo-democracy, not dem)
*==================================================================

di as text _newline "=== SECTION 4: PSEUDO-DEMOCRATIC CHOICE (FAKE) — DEL vs CHIEFS ==="

* Replicate for pseudo-democratic outcome — DEL (binary, use xtprobit)
cap eststo del_fake1: xtprobit fake i.bonus bonus_order ///
    if type==2 & TL==0, re
if _rc != 0 {
    cap eststo del_fake1: reg fake i.bonus bonus_order ///
        if type==2 & TL==0, vce(cluster id)
}

cap eststo tl_fake1: xtprobit fake i.bonus bonus_order ///
    if type==2 & TL==1, re
if _rc != 0 {
    cap eststo tl_fake1: reg fake i.bonus bonus_order ///
        if type==2 & TL==1, vce(cluster id)
}


*==================================================================
* SECTION 5: Triple interaction — bonus × extrinsic × TL
* (More efficient than subsample splits; uses full N=64)
* Tests whether motivation heterogeneity is moderated by leader type
*==================================================================

di as text _newline "=== SECTION 5: TRIPLE INTERACTION — bonus × extrinsic × TL ==="

* Restrict to non-democratic baseline leaders (main finding group)
* H0: the extrinsic × bonus interaction is the same for DELs and chiefs

* Democratic outcome (LPM scaled 0-100, random intercept)
cap eststo trip_dem: xtmixed dem100 ///
    c.bonus##i.extrinsic_leader##i.TL bonus_order ///
    || id:, reml
if _rc==0 {
    matrix N_g = e(N_g)
    local individuals = N_g[1,1]
    estadd local individuals `individuals'
    di as text "trip_dem converged. N_leaders=" `individuals'
}
else {
    di as text "trip_dem xtmixed failed (rc=" _rc "), trying xtreg re"
    cap eststo trip_dem: xtreg dem100 ///
        c.bonus##i.extrinsic_leader##i.TL bonus_order, re vce(cluster id)
    if _rc != 0 di as text "trip_dem xtreg re also failed"
}

* Pseudo-democratic outcome (binary — use xtprobit RE directly)
cap eststo trip_fake: xtprobit fake ///
    c.bonus##i.extrinsic_leader##i.TL bonus_order, re
if _rc != 0 {
    di as text "trip_fake xtprobit failed, trying LPM"
    cap eststo trip_fake: xtreg fake ///
        c.bonus##i.extrinsic_leader##i.TL bonus_order, re vce(cluster id)
}

* Autocratic outcome (binary — use xtprobit RE directly)
cap eststo trip_dic: xtprobit dic ///
    c.bonus##i.extrinsic_leader##i.TL bonus_order, re
if _rc != 0 {
    di as text "trip_dic xtprobit failed, trying LPM"
    cap eststo trip_dic: xtreg dic ///
        c.bonus##i.extrinsic_leader##i.TL bonus_order, re vce(cluster id)
}

* Test the triple interaction terms
di as text _newline "-- Test of triple interaction (bonus × extrinsic × TL) --"
cap testparm 1.bonus#1.extrinsic_leader#1.TL


* Report key marginal effects
di as text _newline "-- Marginal effects of bonus by extrinsic × TL --"
cap margins, dydx(bonus) over(extrinsic_leader TL) post

* Save triple interaction table
cap esttab trip_dem trip_fake trip_dic ///
    using "$working_ANALYSIS/results/tables/tableRC4_triple_interaction.tex", ///
    b(%4.3f) se(%4.3f) ///
    mtitles("Democratic" "Pseudo-democratic" "Autocratic") ///
    keep(1.bonus 1.extrinsic_leader 1.TL ///
         1.bonus#1.extrinsic_leader 1.bonus#1.TL ///
         1.extrinsic_leader#1.TL 1.bonus#1.extrinsic_leader#1.TL) ///
    order(1.bonus 1.extrinsic_leader 1.TL ///
          1.bonus#1.extrinsic_leader 1.bonus#1.TL ///
          1.extrinsic_leader#1.TL 1.bonus#1.extrinsic_leader#1.TL) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    stats(N individuals, labels("Observations" "Leaders") fmt(%4.0f %4.0f)) ///
    booktabs ///
    title("Triple interaction: Bonus x Extrinsic motivation x Leader type") ///
    addnotes("Multilevel panel regressions (random intercept at individual level)." ///
    "Full sample (N=64). Triple interaction tests whether the motivation" ///
    "heterogeneity differs between DELs and chiefs." ///
    "Standard errors in parentheses. * p<0.10, ** p<0.05, *** p<0.01.") ///
    replace

di as text "Table RC4 saved."


*==================================================================
* SECTION 6: Descriptive summary table
*==================================================================

di as text _newline "=== SECTION 6: DESCRIPTIVE SUMMARY ==="

preserve
keep if decision==1  // one row per leader

* Motivation coding tabulation
di as text "Motivation by leader type (N=64):"
tab extrinsic_leader TL, m

* Baseline rule choice by motivation × leader type
di as text "Baseline rule choice by motivation and leader type:"
tab rule_base extrinsic_leader if TL==0, m
di as text "  (Chiefs):"
tab rule_base extrinsic_leader if TL==1, m

* Box N by leader type
di as text "Box distribution by leader type:"
tab type TL, m

restore


*==================================================================
* EOF
*==================================================================
di as text _newline "=== Robustness check complete ==="
di as text "Output tables saved to: $working_ANALYSIS/results/tables/"
di as text "  tableRC2_boxes_DEL_only.tex"
di as text "  tableRC3_boxes_chiefs_only.tex"
di as text "  tableRC4_triple_interaction.tex"
