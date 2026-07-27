*-------------------------------------------------------------------------------------------------------
* OVERVIEW
*-------------------------------------------------------------------------------------------------------
*   This script generates tables and figures reported in the manuscript and SOM of the paper:
*   "The Appearance of Democracy: How Conditional Payments Reshape Behavior Among Local Leaders"
*   Authors: Ivo Steimanis, Esther Blanco, Björn Vollan
*   All raw datafiles are stored in /data
*   All figures reported in the main manuscript and SOM are outputted to /results/figures
*   All tables reported in the main manuscript and SOM are outputted to /results/tables
* TO PERFORM A CLEAN RUN, DELETE THE FOLLOWING TWO FOLDERS:
*   /processed
*   /results
*-------------------------------------------------------------------------------------------------------


*--------------------------------------------------
* Set global Working Directory
*--------------------------------------------------
* The package root is resolved from this script's own location, so run.do works from
* any working directory. `c(filename)` holds the full path of the do-file being run.
local thisfile "`c(filename)'"
if `"`thisfile'"' == "" {
    * c(filename) is empty when commands are pasted interactively; fall back to pwd
    global working_ANALYSIS : pwd
    di as text "Could not self-locate; falling back to current directory."
}
else {
    local slash = max(strrpos(`"`thisfile'"', "/"), strrpos(`"`thisfile'"', "\\"))
    global working_ANALYSIS = substr(`"`thisfile'"', 1, `slash' - 1)
}
di "Package root: $working_ANALYSIS"

* Verify this is the correct directory by checking for expected files
capture confirm file "$working_ANALYSIS/data/leader_raw.dta"
if _rc != 0 {
    di as error " "
    di as error "ERROR: Cannot find expected data files."
    di as error "Expected to find data/leader_raw.dta under the package root."
    di as error "Resolved package root: $working_ANALYSIS"
    exit 601
}

di as text "Directory validated successfully."

*--------------------------------------------------
* Program Setup
*--------------------------------------------------
* Initialize log and record system parameters
clear
set more off
cap mkdir "$working_ANALYSIS/scripts/logs"
cap log close _all
local datetime : di %tcCCYY.NN.DD!-HH.MM.SS `=clock("$S_DATE $S_TIME", "DMYhms")'
local logfile "$working_ANALYSIS/scripts/logs/`datetime'.log.txt"
log using "`logfile'", text

di "Begin date and time: $S_DATE $S_TIME"
di "Stata version: `c(stata_version)'"
di "Updated as of: `c(born_date)'"
di "Variant:       `=cond( c(MP),"MP",cond(c(SE),"SE",c(flavor)) )'"
di "Processors:    `c(processors)'"
di "OS:            `c(os)' `c(osdtl)'"
di "Machine type:  `c(machine_type)'"

*   Analyses were run on Windows using Stata version 19.5
version 19.5              // Set Version number for backward compatibility

* All required Stata packages are available in the /libraries/stata folder
tokenize `"$S_ADO"', parse(";")
while `"`1'"' != "" {
  if `"`1'"'!="BASE" cap adopath - `"`1'"'
  macro shift
}
adopath ++ "$working_ANALYSIS/scripts/libraries/stata"
mata: mata mlib index
sysdir set PERSONAL "$working_ANALYSIS/scripts/libraries/stata"

* Create directories for output files
cap mkdir "$working_ANALYSIS/processed"
cap mkdir "$working_ANALYSIS/results"
cap mkdir "$working_ANALYSIS/results/intermediate"
cap mkdir "$working_ANALYSIS/results/tables"
cap mkdir "$working_ANALYSIS/results/figures"

* Set general graph style

set scheme s2color
grstyle init
{
	* White background everywhere
	grstyle set color white: background plotregion graphregion legend box textbox

	* Main color palette (ColorBrewer-inspired, colorblind-safe)
	* 1=Steel Blue (Democratic), 2=Warm Orange (Pseudo-dem), 3=Muted Red (Autocratic)
	* 4=Teal (Intrinsic/Subgroup 1), 5=Purple (Extrinsic/Subgroup 2), 6=Gray (Neutral)
	grstyle set color  "57 106 177" "218 124 48" "204 37 41" "62 150 81" "128 100 162" "148 148 148" ///
		: p# p#line p#lineplot p#bar p#area p#arealine p#pie histogram

	* Font sizes (standardized for journal figures)
	grstyle set size 10pt: heading
	grstyle set size 8pt: subheading axis_title
	grstyle set size 7pt: p#label p#boxlabel body small_body text_option axis_label tick_label minortick_label key_label
}
grstyle set compact


*--------------------------------------------------
* Run processing and analysis scripts
*--------------------------------------------------
do "$working_ANALYSIS/scripts/01_clean.do"
do "$working_ANALYSIS/scripts/02_generate.do"
do "$working_ANALYSIS/scripts/03_main_paper.do"
do "$working_ANALYSIS/scripts/04_supplementary.do"
do "$working_ANALYSIS/scripts/05_robustness_motivation.do"
do "$working_ANALYSIS/scripts/06_allocation_analysis.do"

* Python and R scripts (run separately after Stata pipeline completes):
*   python scripts/fig4a_panel.py             → Figure 4 (combined two-panel)
*   Rscript scripts/07_equivalence_tests.R    → Figure S2
*   Rscript scripts/08_transition_sankey.R    → Figure S4

* End log
di "End date and time: $S_DATE $S_TIME"
* Completion marker. Must be printed BEFORE log close, otherwise it never reaches the
* log and any script polling for it will wait forever.
di "PIPELINE_COMPLETE"
log close
di "PIPELINE_COMPLETE"
 
 
 
** EOF