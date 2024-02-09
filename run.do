*-------------------------------------------------------------------------------------------------------
* OVERVIEW
*-------------------------------------------------------------------------------------------------------
*   This script generates tables and figures reported in the manuscript and SOM of the paper:
*   "The perils of conditional payments for democracy: Experimental evidence from local leaders in rural Namibia"
*	Authors: Esther Blanco, Björn Vollan, Ivo Steimanis
*   All raw datafiles are stored in /data
*   All figures reported in the main manuscript and SOM are outputted to /results/figures
*   All tables areported in the main manuscript and SOM are outputted to /results/tables
* TO PERFORM A CLEAN RUN, DELETE THE FOLLOWING TWO FOLDERS:
*   /processed
*   /results
*-------------------------------------------------------------------------------------------------------


*--------------------------------------------------
* Set global Working Directory
*--------------------------------------------------
* Define this global macro to point where the replication folder is saved locally that includes this run.do script
global working_ANALYSIS "YourPath"


*--------------------------------------------------
* Program Setup
*--------------------------------------------------
* Initialize log and record system parameters
clear
set more off
cap mkdir "$working_ANALYSIS/scripts/logs"
cap log close
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

*   Analyses were run on Windows using Stata version 16
version 16              // Set Version number for backward compatibility

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
set scheme swift_red //select one scheme as reference scheme to work with
grstyle init 
{
*Background color
grstyle set color white: background plotregion graphregion legend box textbox //

*Main colors (note: swift_red only defines 8 colors. Multiplying the color, that is "xx yy zz*0.5" reduces/increases intensity and "xx yy zz%50" reduces transparency)
grstyle set color 	"100 143 255" "120 94 240" "220 38 127" "254 97 0" "255 176 0" /// 5 main colors
					"100 143 255*0.4" "120 94 240*0.4" "220 38 127*0.4" "254 97 0*0.4" "255 176 0*0.4" ///
					"100 143 255*1.7" "120 94 240*1.7" "220 38 127*1.7" "254 97 0*1.7" "255 176 0*1.7" ///
					: p# p#line p#lineplot p#bar p#area p#arealine p#pie histogram 

*Font size
grstyle set size 10pt: heading //titles
grstyle set size 8pt: subheading axis_title //axis titles
grstyle set size 8pt: p#label p#boxlabel body small_body text_option axis_label tick_label minortick_label key_label //all other text
}
* -------------------------------------------------


*--------------------------------------------------
* Run processing and analysis scripts
*--------------------------------------------------
//TODO: update with finale cleaning / analysis files
do "$working_ANALYSIS/scripts/01_clean.do"
do "$working_ANALYSIS/scripts/02_generate.do"
do "$working_ANALYSIS/scripts/03_analysis.do"

* End log
di "End date and time: $S_DATE $S_TIME"
log close
 
 
 
** EOF