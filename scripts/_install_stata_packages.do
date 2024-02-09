*--------------------------------------------------
* This script installs all necessary Stata packages into /libraries/stata
* To do a fresh install of all Stata packages, delete the entire /libraries/stata folder
* Note: this script has been provided for pedagogical purposes only. It should NOT be included as part of your replication materials, since these add-ons are already available in /libraries/stata
*--------------------------------------------------

*******

* Create and define a local installation directory for the packages
cap mkdir "$working_ANALYSIS/scripts/libraries"
cap mkdir "$working_ANALYSIS/scripts/libraries/stata"
net set ado "$working_ANALYSIS/scripts/libraries/stata"


* install grc1leg - combine graphs into one graph with a common legend
net install grc1leg, from("http://www.stata.com/users/vwiggins") replace
net install tost, from("https://alexisdinno.com/stata") replace

* install package directly from Github
net install ietoolkit , from("https://raw.githubusercontent.com/worldbank/ietoolkit/master/src") replace
net install palettes, replace from("https://raw.githubusercontent.com/benjann/palettes/master/")
net install colrspace, replace from("https://raw.githubusercontent.com/benjann/colrspace/master/")

* Install packages from SSC
foreach p in catplot cibar coefplot combomarginsplot distplot egenmore estout geo2xy grstyle mif2dta moss mylabels outreg2 schemepack shp2dta spmap stripplot winsor2 {
	local ltr = substr(`"`p'"',1,1)
	qui net from "http://fmwww.bc.edu/repec/bocode/`ltr'"
	net install `p', replace
}


** EOF


