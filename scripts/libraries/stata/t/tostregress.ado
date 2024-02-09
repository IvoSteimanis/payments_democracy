*! version 3.1.3  15may2021 by alexis dot dinno at pdx dot edu
*! perform two one-sided t tests for equivalence of regression coefficients

********************************************************************************
* Syntax:  tostregress depvar [indepvars] [if exp] [in range] [weight] 
*          [, eqvtype(type) eqvlevel(#) uppereqvlevel(#) level(#) relevance 
*          svy(svy_options) regression_options]

program define tostregress, byable(recall) sortpreserve
  local version : di "version " string(_caller()) ", missing :"
  preserve
  marksample touse
  qui: keep if `touse'
  if int(_caller())<8 {
    di in r "tostregress- does not support this version of Stata." _newline
    di as txt "Requests for a v7 compatible version will be relatively easy to honor." 
    di as txt "Requests for a v6 compatible version may be less easy." 
    di as txt "Requests for a version compatible with versions of STATA earlier than v6 are "
    di as txt "untenable since I do not have access to the software." _newline 
    di as txt "All requests are welcome and will be considered."
    exit
    }
   else {
    if int(_caller())<14 {
      tostregress11 `0'
      }
     else {
      tostregress14 `0'
      }
    }
  restore
end

program define tostregress14, eclass byable(recall) sortpreserve
  version 14, missing

  syntax [anything] [if] [in] [aw fw iw pw] [, EQVType(string) /*
  */      EQVLevel(numlist missingokay) UPPEReqvlevel(numlist missingokay) /*
  */      RELevance Level(cilevel) * ]

  * quietly {
  * Get the canonical cmdline for ereturn
  * Prep weightexp
  if ("`weight'" == "") {
    local weightexp = ""
    }
   else {
    local weightexp = "`weight'`exp'"
    }
  * Prep upperexp
  if ("`uppereqvlevel'"=="" | "`uppereqvlevel'"==".") {
    local upperexp = ""
    }
   else {
    local upperexp = "uppereqvlevel(`uppereqvlevel')"
    }
  * Prep levelexp
  if ("`cilevel'"!="" | "`cilevel'"!= ".") {
    local levelexp = ""
    }
   else {
    local levelexp = "level(`cilevel')"
    }
    
  local cmdline = "tostregress `anything' `if' `in' `weightexp', eqvtype(`eqvtype') eqvlevel(`eqvlevel') `upperexp' `relevance' `levelexp' `options'"
  local cmdline = ustrregexra("`cmdline'"," +"," ")
  local cmdline = ustrregexra("`cmdline'"," +,",",")

  * Tidy options:

  *  Ensure there is a space after a comma
  local options = ustrregexra("`options'", ",", ", ")

  *  Ensure there is not a space before a comma
  local options = ustrregexra("`options'", " +,", ",")

  *  Ensure there is a space after a closing parenthesis
  local options = ustrregexra("`options'", "\)", "\) ")
  
  *  Ensure there is not a space after an opening parenthesis...
  local options = ustrregexra("`options'", "\( +", "\(")
  
  *  Ensure there is no more than one space before a non-space character
  local options = ustrregexra("`options'", " {1,}", " ")
  
  *  Ensure there is not a space before a closing parenthesis...
  local options = ustrregexra("`options'", " \)", "\)")
  
  *  ... unless that closing parenthesis is preceeded by a closing parenthesis
  local options = ustrregexra("`options'", "\)\)", "\) \)")
  
  *  ... unless that opening parenthesis is preceeded by an opening parenthesis
  local options = ustrregexra("`options'", "\(\(", "\( \(")
  
  * And there must be at least one space between opening and closing parentheses
  local options = ustrregexra("`options'", "\(\)", "\( \)")
  
  * Check for noconstant
  local numoptions: word count `options'
  local nocons = 0
  if (`numoptions' > 0) {
    forvalues i=1/`numoptions' {
      local wordi: word `i' of `options'
      if (ustrlower("`wordi'") == "noconstant" | ustrlower("`wordi'") == "noconstant") {
        local nocons = 1
        }
      }
    }
      
  * Check for implicit vce types, and translate to explicit vce option if
  * necessary.
  local numoptions: word count `options'
  local vceexplicit = ""
  if (`numoptions' > 0) {
    local i = 1
    local tempopt = ""
    while `i' <=`numoptions' {
      local wordi: word `i' of `options'
      * Provide for robust option
      if (ustrlower("`wordi'")=="robust") {
        local tempopt = "`temopt' " + "vce(robust)" 
        }
       else {
        * Provide for hc2 option
        if (ustrlower("`wordi'")=="hc2") {
          local tempopt = "`temopt' " + "vce(hc2)" 
          }
         else {
          * Provide for hc3 option
          if (ustrlower("`wordi'")=="hc3") {
            local tempopt = "`temopt' " + "vce(hc3)" 
            }
           else {
            * Provide for cluster option
            if (usubstr("`wordi'",1,7)=="cluster") {
              local tempopt = "`temopt' " + ustrregexra("`wordi'", "cluster\(([A-Za-z_\\p{L}][A-Za-z0-9_\\p{L}]+)\)", "vce\(cluster $1\)") 
              }
             else {
              * And simply pass other options along...
              local tempopt = "`tempopt' " + "`wordi'"
              }
            }
          }
        }
      local i = `i'+1
      }
    * Replace options with only explicit vce types and remaining options.
    local options = "`tempopt'"
    }

  * Check for vce, set vceflag as needed, and prepare vceopt for output
  local numoptions: word count `options'
  local vceflag = 0
  if (`numoptions' > 0) {
    local i = 1
    while `i' <=`numoptions' {
      local wordi: word `i' of `options'
      local lenwordi = ustrlen("`wordi'")
      * For option vce(cluster clustervar)
      local vceoption = ""
      if ("`wordi'"=="vce(cluster") {
        local i = `i' + 1
        local next: word `i' of `options'
        local wordi = "`wordi' "+"`next'"
        * Take vce option out of options if using cluster
        local vceoption = "`wordi'"
        local vceoptargs = usubstr("`vceoption'",5,ustrlen("`vceoption'")-5)
        local options = ustrregexra("`options'", "(.*)vce\(`vceoptargs'\)(.*)", "$1$2")
        }

        * For option vce(jackknife or vce(bootstrap, and it's suboptions)
        if ("`wordi'"=="vce(jackknife," | "`wordi'"=="vce(jack," | "`wordi'"=="vce(bootstrap," | "`wordi'"=="vce(boot,") {
          local Nrp = 0
          local Nlp = 1
          while `Nrp' != `Nlp' {
            local i = `i' + 1
            local next: word `i' of `options'
            if (ustrpos("`next'","(")!=0) {
              local Nlp = `Nlp' + 1
              }
            if (ustrpos("`next'",")")!=0) {
              local Nrp = `Nrp' + 1
              }
            local wordi = "`wordi' "+"`next'"
              }
            }

        * Format SE labels based on vce()
        if (ustrpos("`wordi'","vce") != 0) {
          local vceflag = 1
          local lenword = ustrlen("`wordi'")
          local vceopt = ustrlower(usubstr("`wordi'",5,`lenword'-5))
          local replications = 0
          if ("`vceopt'"=="robust") {
            local vceopt = "Robust"
            }
          if ("`vceopt'"=="hc2") {
            local vceopt = "Robust HC2"
            }
          if ("`vceopt'"=="hc3") {
            local vceopt = "Robust HC3"
            }
          if (usubstr("`vceopt'",1,7)=="cluster") {
            local vceopt = "Cluster"
            }
          if (usubstr("`vceopt'",1,4)=="jack") {
            if (ustrpos("`vceopt'","mse")==0) {
              local vceopt = "Jackknife"
              }
             else {
              local vceopt = "{help jackknife_mse##intro_svy:Jknife *}"
              }
            if (ustrpos("`wordi'","cluster")!=0) {
              local replications = 1
              local repopt = usubstr("`wordi'",9,ustrlen("`wordi'")-1)
              local repopt = ustrregexra("`repopt'","(.*)\)","$1")
              }
            }
          if (usubstr("`vceopt'",1,4)=="boot") {
            if (ustrpos("`vceopt'","mse")==0) {
              local vceopt = "Bootstrap"
              }
             else {
              local vceopt = "{help bootstrap_mse:Bstrap *}"
              }
            if (ustrpos("`wordi'","cluster")!=0) {
              local replications = 1
              local repopt = usubstr("`wordi'",9,ustrlen("`wordi'")-1)
              local repopt = ustrregexra("`repopt'","(.*)\)","$1")
              }
            }
          if ("`vceopt'"=="ols") {
            local vceopt = ""
            }
          }
        local i = `i'+1
        }
      }
        
  **********
  * Address prefix command workaround

  * Check for svy() prefix
  * If there are any options...
  local svyflag = 0
  if (`numoptions' > 0) {
    qui: _svy_opts14 "`options'"
    local options = r(nonsvy_options)
    local svy_command = r(svy_command)
    if ("`svy_command'" != ", :") {
      local svyflag = 1
      }
     else {
      local svy_command = ""
      }
    }

* Replace missing options as an empty string
  if ("`options'"==".") {
    local options = ""
    }
      
  * Put vce option back in
  if ("`vceoption'"!="") {
    local options = "`options' `vceoption'"
    }

  * Get the number of independent variables, and number of coefficients from the 
  * regression
  if ("`weight'" == "") {
    qui: `svy_command' regress `anything' `if' `in', `options' level(`level')
    }
   else {
    qui: `svy_command' regress `anything' `if' `in' [`weight' `exp'], `options' level(`level')
    }
      
  * vceopt
  if (`svyflag'==1) {
    local vceopt = e(vcetype)
    }
  
  * Create the local macro of coefficient names and factor names (if any)

  * First get the column names from the coefficient matrix */ 
  local vnames : colfullnames e(b)

  * Next drop omitted categories in e(b) from vnames and put results into vlist
  local numcoefficients = 0
  foreach var of local vnames {
    _ms_parse_parts `var'
    if !`r(omit)' {
      local vlist `vlist' `var'
      local numcoefficients = `numcoefficients'+1
      }   
    }

  * Use _vlformat14 to get variables and factors as appropriate
  _vlformat14 "`vlist'"
  local coefficientnames = r(variables)
  local factors = r(factors)

*    if (`nocons'==1) {
*    local numindepvars = `numcoefficients' - 1
*        }
*      else {
  local numindepvars = `numcoefficients'
*        }
                
  * Drop various suboptions that should not be used twice after the above 
  * call of regress...
  *  ...including keep 
  local options = ustrregexra("`options'", "keep", "")
  *  ...including idcluster(newvar)
  local options = ustrregexra("`options'", "(.*)idcluster\(.*?\)(.*)", "$1$2")
  *  ...including the every(#) part of saving(filename, suboptions)
  local options = ustrregexra("`options'", "(.*)every\(.*?\)(.*)", "$1$2")
  *  ...including saving()
  local options = ustrregexra("`options'", "(.*)saving\(.*?\)(.*)", "$1$2")
    
  *  And reinsure there is no more than one space before a non-space character
  local options = ustrregexra("`options'", " {1,}", " ")

    **********
  * Validate eqvtype

  * If eqvtype is empty or does not exist, then supply the default (delta)
  if ustrlower("`eqvtype'") == "" {
    local temp = ""
    forvalues i = 1/`numindepvars' {
      local temp = "`temp'" + " delta"
      }
    local eqvtype = "`temp'"
    }


  local numeqvtypes: word count `eqvtype'

  * Validate correct number of eqvtype() entries
  if !(`numeqvtypes' == 1 | `numeqvtypes' == `numindepvars') {
    noi: di as err "option eqvtype() incorrectly specified:" _newline "option eqvtype must contain either 1 entry, or exactly as many entries as" _newline "independent variables plus 1 for the constant term (unless specifying the" _newline "noconstant option, in which case just the exact number of independent variables)"
    exit 198
    }
   else {
    * Expand a single entry for eqvtype() to a list containing the same entry 
    * once for each coefficient
    if (`numeqvtypes' == 1) {
      local temp = ""
      forvalues i = 1/`numindepvars' {
        local temp = "`temp'" + " `eqvtype'"
        }
      local eqvtype = "`temp'"
      }
    }

  * Validate that each eqvtype is either "delta" or "epsilon"
  forvalues i = 1(1)`numeqvtypes' {
    local eqvtypei: word `i' of `eqvtype'
    if !(ustrlower("`eqvtypei'") == "delta" | lower("`eqvtypei'") == "epsilon") {
      noi: di as err "option eqvtype() incorrectly specified:" _newline "each entry of eqvtype must be either delta or epsilon"
      exit 198
      }
    }

  **********
  * Validate eqvlevel

  * If eqvlevel is empty, contains a single missing value, or does not exist, 
  * then supply the appropriate default values corresponding to the eqvtype for 
  * that coefficient
  if ("`eqvlevel'"=="" | "`eqvlevel'"==".") {
    local temp = ""
    forvalues i = 1/`numindepvars' {
      local eqvtypei: word `i' of `eqvtype'
        if (ustrlower("`eqvtypei'")=="delta") {
          local temp = "`temp'" + " 1"
          }
        if (ustrlower("`eqvtypei'")=="epsilon") {
          local temp = "`temp'" + " 2"a
          }
        }
    numlist "`temp'"
    local eqvlevel = r(numlist)
    }

  local numeqvlevels: word count `eqvlevel'
      
  * Validate correct number of eqvlevel() values
  if !(`numeqvlevels' == 1 | `numeqvlevels' == `numindepvars') {
    noi: di as err "option eqvlevel() incorrectly specified:" _newline "option eqvlevel must contain either 1 value, or exactly as many values as" _newline "independent variables plus 1 for the constant term (unless specifying the" _newline "noconstant option, in which case just the exact number of independent variables)"
    exit 198
    }
   else {
    * Expand a single value for eqvlevel() to a list containing the same value 
    * once for each coefficient
    if (`numeqvlevels' == 1) {
      local temp = ""
      forvalues i = 1/`numindepvars' {
        local temp = "`temp'" + " `eqvlevel'"
        }
      numlist "`temp'"
      local eqvlevel = r(numlist)
      }
    }
    
    * Replace any missing eqvlevel values
    forvalues i = 1/`numindepvars' {
      local eqvleveli: word `i' of `eqvlevel'
      if (`eqvleveli' == .) {
        local eqvtypei: word `i' of `eqvtype'
        if (ustrlower("`eqvtypei'")=="delta") {
          local temp = "`temp'" + " 1"
          }
        if (ustrlower("`eqvtypei'")=="epsilon") {
          local temp = "`temp'" + " 2"a
          }
        }
       else {
        local temp = "`temp'" + " `eqvleveli'"
        }
      }
    numlist "`temp'"
    local eqvlevel = r(numlist)

  * Check whether uppereqvlevel exists, if not provide default value
  if ("`uppereqvlevel'"=="" | "`uppereqvlevel'"==".") {
    local uppereqvlevel = 0
    local numuppereqvlevels = 1
    }

  local numuppereqvlevels: word count `uppereqvlevel'
        
  * Validate correct number of uppereqvlevel() values
  if !(`numuppereqvlevels' == 0 | `numuppereqvlevels' == 1 | `numuppereqvlevels' == `numindepvars') {
    noi: di as err "option uppereqvlevel() incorrectly specified:" _newline "option uppereqvlevel must be empty, contain 1 value, or exactly as many values" _newline "as independent variables plus 1 for the constant term (unless specifying the " _newline "noconstant option, in which case just the exact number of independent " _newline "variables)"
    exit 198
    }
   else {
    * Expand a single value for uppereqvlevel() to a list containing the same  
    * value once for each coefficient
    if (`numuppereqvlevels' == 1) {
      local temp = ""
      forvalues i = 1/`numindepvars' {
        local temp = "`temp'" + " `uppereqvlevel'"
        }
      numlist "`temp'"
      local uppereqvlevel = r(numlist)
      }
    }

  * Validate eqvlevel default for eqvtype epsilon
  local temp = ""
  forvalues i = 1/`numindepvars' {
    local eqvleveli: word `i' of `eqvlevel'
    local uppereqvleveli: word `i' of `uppereqvlevel'
    local eqvtypei: word `i' of `eqvtype'
    if (lower("`eqvtypei'") == "epsilon") & (`eqvleveli' == 1 & (`uppereqvleveli'==. | `uppereqvleveli'==0) ) {
      local temp = "`temp'" + " 2"
      }
     else {
      local temp = "`temp'" + " `eqvleveli'"
      }
    }
  numlist "`temp'"
  local eqvlevel = r(numlist)

  * Set upper and lower with upper default values
  local tempupper = ""
  local templower = ""
  forvalues i = 1(1)`numindepvars' {
    local eqvleveli: word `i' of `eqvlevel'
    local uppereqvleveli: word `i' of `uppereqvlevel'
    if (`uppereqvleveli'==0 | `uppereqvleveli'==. | `uppereqvleveli' == abs(`eqvleveli')) {
      local tempupper = "`tempupper' " + string( abs(`eqvleveli') )
      local templower = "`templower' " + string( abs(`eqvleveli') )
      }
     else {
      local tempupper = "`tempupper'" + " `uppereqvleveli'"
      local templower = "`templower'" + " `eqvleveli'"
      }
    }
  numlist "`tempupper'"
  local upper = r(numlist)
  numlist "`templower'"
  local lower = r(numlist)

  * Set upper and lower with non-default values
  local tempupper = ""
  local templower = ""
  forvalues i = 1(1)`numindepvars' {
    local eqvleveli: word `i' of `eqvlevel'
    local uppereqvleveli: word `i' of `uppereqvlevel'
    local upperi: word `i' of `upper'
    local loweri: word `i' of `lower'
    if (`uppereqvleveli'>0 & `uppereqvleveli' != .) {
      local tempupper = "`tempupper' " + string( abs(`uppereqvleveli') )
      local templower = "`templower' " + string( abs(`eqvleveli') )
      }
     else {
      local tempupper = "`tempupper' " + "`upperi'"
      local templower = "`templower' " + "`loweri'"
      }
    }
  numlist "`tempupper'"
  local upper = r(numlist)
  numlist "`templower'"
  local lower = r(numlist)

  
  * Define and format alpha
  local alpha = (1 - `level'/100)
  
  * Format alpha to remove trailing zeros
  if (mod(`alpha'*1000, 1) == 0.0) {
    local alpha: di %6.3f `alpha'
    }
  if (mod(`alpha'*100, 1) == 0.0) {
    local alpha: di %5.2f `alpha'
    }
  if (mod(`alpha'*10, 1) == 0.0) {
    local alpha: di %4.1f `alpha'
    }
  if (mod(`alpha', 1) == 0.0) {
    local alpha: di %4.0f `alpha'
    }

*******************************************************************************
* The business starts here                                                    *
*******************************************************************************
  if ("`weight'" == "") {
    local regresscmd = "`svy_command' regress `anything' `if' `in', `options' level(`level')"
    if ("`relevance'" != "") {
      di as txt _newline "Regression tests for difference"
      if (`vceflag' == 0 & "`svy_command'"=="") {
        di as txt "{hline 13}{c TT}{hline 34}" _continue
        if (ustrpos("`vceopt'","J")!=0 | ustrpos("`vceopt'","B")!=0) {
          }
        }
       else {
        if ("`svy_command'"!="") {
          di as txt "{hline 47}"
          }
         else {
          di as txt "{hline 47}" _continue
          if (ustrpos("`vceopt'","J")!=0 | ustrpos("`vceopt'","B")!=0) {
            di 
            }
          }
        }
      `regresscmd'
      }
     else {
      qui: `regresscmd'
      }
    }
   else {
    local regresscmd = "`svy_command'regress `anything' `if' `in' [`weight' `exp'], `options' level(`level')"
    if ("`relevance'" != "") {
      di as txt _newline "Regression tests for difference"
      di as txt "{hline 48}"
      `regresscmd'
      }
     else {
      qui: `regresscmd'
      }
    }

  * Get the name of the dependent variable for subsequent output in tables
  local depvar = e(depvar)

  * Set degrees of freedom for t test statistics
  local df = e(df_r)
    
  * Get the coefficients, standard errors, t test statistics, and p-values from 
  * the regression
  matrix coefficients = e(b)
  
  * Retain only those coefficients that are not zero (i.e. not dropped from 
  * the estimation)
  matrix tempc = J(1,`numindepvars',0)
  local Neb = colsof(matrix(coefficients))
  if (`Neb' > `numindepvars') {
    forvalues j = 1/`numindepvars' {
      forvalues i = 1/`Neb' {
        if (matrix(coefficients[1,`i']) != 0) {
          matrix tempc[1,`j'] = coefficients[1,`i']
          matrix coefficients[1,`i'] = 0
          continue, break
          }
        }
      }
      matrix coefficients = tempc
    }

  * Retain only those variances (varcov diagonal elements) that are not zero 
  * (i.e. not dropped from the estimation)
  matrix varcov       = e(V)
  matrix tempVC       = J(1,`numindepvars',0)
  local NVC = colsof(matrix(varcov))
  if (`NVC' > `numindepvars') {
    forvalues j = 1/`numindepvars' {
      forvalues i = 1/`NVC' {
        if (matrix(varcov[`i',`i']) != 0) {
          matrix tempVC[1,`j'] = varcov[`i',`i']
          matrix varcov[`i',`i'] = 0
          continue, break
          }
        }
      }
    }
   else {
    forvalues i = 1/`numindepvars' {
      matrix tempVC[1,`i'] = varcov[`i',`i']
      }
    }
  matrix varcov = tempVC

  * Set up a variety of vectors of the correct size to receive various statistics
  matrix stderrs      = coefficients
  matrix T            = coefficients
  matrix T1           = coefficients
  matrix T2           = coefficients
  matrix P            = coefficients
  matrix P1           = coefficients
  matrix P2           = coefficients
  
  * Prepare for positivist and negativist decisions
  local PositivistDecision = ""
  local NegativistDecision = ""
  
  forvalues i = 1/`numindepvars' {
    matrix stderrs[1,`i'] = sqrt(varcov[1,`i'])
    local eqvtypei: word `i' of `eqvtype'
    local upperi: word `i' of `upper'
    local loweri: word `i' of `lower'
    local coefficienti = coefficients[1,`i']
    local stderri = stderrs[1,`i']
    matrix T[1,`i'] = `coefficienti'/`stderri'
          * If not using bootstrap
          if (ustrpos("`vceopt'","B")==0) {
      matrix P[1,`i'] = ttail(`df', abs(T[1,`i']))
              }
            * Otherwise if using bootstrap
            else {
      matrix P[1,`i'] = 1-normal(abs(T[1,`i']))
              }
    
    *Determine positivist rejection decision
    if (matrix(P[1,`i']) <= `alpha'/2) {
      local PositivistDecision = "`PositivistDecision'" + " Reject"
      }
     else {
      local PositivistDecision = "`PositivistDecision'" + " NotReject"
      }

    *Calculate T1, T2, P1, and P2
    if (ustrlower("`eqvtypei'") == "delta") {
      matrix T1[1,`i'] = (`upperi' - `coefficienti')/`stderri'
      matrix T2[1,`i'] = (`coefficienti' + `loweri')/`stderri'
      }
     else {
      matrix T1[1,`i'] = `upperi' - T[1,`i']
      matrix T2[1,`i'] = T[1,`i'] + `loweri'
      }
          * If not using bootstrap
          if (ustrpos("`vceopt'","B")==0) {
      matrix P1[1,`i'] = ttail(`df', T1[1,`i'])
      matrix P2[1,`i'] = ttail(`df', T2[1,`i'])
              }
            * Otherwise if using bootstrap
            else {
      matrix P1[1,`i'] = 1-normal(T1[1,`i'])
      matrix P2[1,`i'] = 1-normal(T2[1,`i'])
              }
    *Determine negativist rejection decision
    if (P1[1,`i'] <= `alpha' & P2[1,`i'] <= `alpha') {
      local NegativistDecision = "`NegativistDecision'" + " Reject"
      }
     else {
      local NegativistDecision = "`NegativistDecision'" + " NotReject"
      }
    }
        
  * Regression test for equivalence table header
  local symmetric = 1
  forvalues i = 1/`numindepvars' {
    local loweri: word `i' of `lower'
    local upperi: word `i' of `upper'
    if (`loweri' != `upperi') {
      local symmetric = 0
      }
    }

  * Determine if there are any factors, and set factorflag=1 if there are
  local factorflag = 0
  forvalues i = 1/`numindepvars' {
    local factori: word `i' of `factors'
    if ("`factori'"!=".") {
      local factorflag=1
      }
    }

  _tostregressheader14 "`depvar'" `factorflag' `symmetric' "`vceopt'" "`repopt'"

  * Regression test for equivalence table entries
  forvalues i = 1/`numindepvars' {
    * independent variable or constant name
    local indepvari: word `i' of `coefficientnames'
    * current factor name for this variable
    local factori: word `i' of `factors'
    *previous factor name for previous variable
    if (`i' == 1) {
      local previousfactori = "0"
      }
      else {
       local previous = `i' - 1
       local previousfactori: word `previous' of `factors'
       }
    
    * The particulars for that variable or constant
    local coefficienti = coefficients[1,`i']
    local stderri = stderrs[1,`i']
    local t1 = T1[1,`i']
    local p1 = P1[1,`i']
    local t2 = T2[1,`i']
    local p2 = P2[1,`i']
    local eqvtypei: word `i' of `eqvtype'
    local upperi: word `i' of `upper'
    local loweri: word `i' of `lower'
    _tostregressentry14 `indepvari' `factori' `previousfactori' `coefficienti' `stderri' `t1' `p1' `t2' `p2' `eqvtypei' `upperi' `loweri'
    }

  * Close the equivalence test table
  if (`symmetric') {
    di as txt "{hline 13}{c BT}{hline 80}"
    }
   else {
    di as txt "{hline 13}{c BT}{hline 86}"
    }
  
  * Relevance test table
  if ("`relevance'"!="") {
            
    * Relevance table header
    _relevanceregheader14 "`depvar'" `factorflag' `alpha' `symmetric'
        
    * Relevance table entries
    forvalues i = 1/`numindepvars' {
      * independent variable or constant name
      local indepvari: word `i' of `coefficientnames'
      * current factor name for this variable
      local factori: word `i' of `factors'
      *previous factor name for previous variable
      if (`i' == 1) {
        local previousfactori = "0"
        }
       else {
        local previous = `i' - 1
        local previousfactori: word `previous' of `factors'
        }

        * The particulars for that variable or constant
        local eqvtypei: word `i' of `eqvtype'
        local upperi: word `i' of `upper'
        local loweri: word `i' of `lower'
        local positivistdecisioni: word `i' of `PositivistDecision'
        local negativistdecisioni: word `i' of `NegativistDecision'
        _relevanceregentry14 `indepvari' `factori' `previousfactori' `eqvtypei' `upperi' `loweri' `positivistdecisioni' `negativistdecisioni' 
        }

    * Close the table
  
    if (`symmetric') {
      di as txt "{hline 13}{c BT}{hline 67}"
      }
     else {
      di as txt "{hline 13}{c BT}{hline 73}"
      }
    }

*******************************************************************************
* Program end. Close up shop and return things.                               *
*******************************************************************************

  * Prep a few things pertaining to svy and relevance options
  if (`svyflag'==1) {
    local svypref = "Survey: "
    }
   else {
    local svypref = ""
    }
  if ("`relevance'"=="") {
    local title = "`svypref'Regression tests for equivalence"
    }
   else {
    local title = "`svypref'Regression relevance tests"
    }
       
  * Create the local string of relevance conclusions
  local conclusions = ""
  forvalues i = 1/`numindepvars' {
    local positivistdecisioni: word `i' of `PositivistDecision'
    local negativistdecisioni: word `i' of `NegativistDecision'
    _relevanceconclusion `positivistdecisioni' `negativistdecisioni'
    local conclusion = r(conclusion)
    local conclusions = "`conclusions' `conclusion'"
    }

  ereturn scalar alpha = `alpha'
  if ("`relevance'"!="") {
    ereturn local rel_conclusions `conclusions'
    }
  ereturn local lowereqvlevel `lower'
  ereturn local uppereqvlevel `upper'
  ereturn local eqvtype `eqvtype'
  ereturn local title `title'
  ereturn local cmdline `cmdline'
  ereturn local cmd "tostregress"
  ereturn matrix P2 P2
  ereturn matrix P1 P1
  ereturn matrix T2 T2
  ereturn matrix T1 T1

  end


program define tostregress11, eclass byable(recall) sortpreserve
  version 11, missing

  syntax [anything] [if] [in] [aw fw iw pw] [, EQVType(string) /*
  */      EQVLevel(numlist missingokay) UPPEReqvlevel(numlist missingokay) /*
  */      RELevance Level(cilevel) * ]

  * quietly {
  * Get the canonical cmdline for ereturn
  * Prep weightexp
  if ("`weight'" == "") {
    local weightexp = ""
    }
   else {
    local weightexp = "`weight'`exp'"
    }
  * Prep upperexp
  if ("`uppereqvlevel'"=="" | "`uppereqvlevel'"==".") {
    local upperexp = ""
    }
   else {
    local upperexp = "uppereqvlevel(`uppereqvlevel')"
    }
  * Prep levelexp
  if ("`cilevel'"!="" | "`cilevel'"!= ".") {
    local levelexp = ""
    }
   else {
    local levelexp = "level(`cilevel')"
    }
  
  * Insure options does not contain just a '.'
  if (trim("`options'")==".") {
    local options = ""
    }
    
  local cmdline = "tostregress `anything' `if' `in' `weightexp', eqvtype(`eqvtype') eqvlevel(`eqvlevel') `upperexp' `relevance' `levelexp' `options'"
  local cmdline = subinstr("`cmdline'", " +", " ",.)
  local cmdline = subinstr("`cmdline'", " +,", ",",.)


  * Tidy options:

  *  Ensure there is a space after a comma
  local options = subinstr("`options'", ",", ", ",.)

  *  Ensure there is not a space before a comma
  local options = subinstr("`options'"," +,",",",.)

  *  Ensure there is a space after a closing parenthesis
  local options = subinstr("`options'",")",") ",.)
  
  *  Ensure there is not a space after an opening parenthesis...
  local options = subinstr("`options'","  "," ",.)
  local options = subinstr("`options'","( ","(",.)
  
  *  Rensure there is no more than one space before a non-space character
  local options = subinstr("`options'","  "," ",.)
  
  *  Ensure there is not a space before a closing parenthesis...
  local options = subinstr("`options'"," )",")",.)
  
  *  ... unless that closing parenthesis is preceded by a closing parenthesis
  local options = subinstr("`options'","))",") )",.)
  
  *  ... and ensure spaces between opening parentheses
  local options = subinstr("`options'","((","( (",.)
  
  * And there must be at least one space between opening and closing parentheses
  local options = subinstr("`options'","()","( )",.)
  
  * Check for noconstant
  local numoptions: word count `options'
  local nocons = 0
  if (`numoptions' > 0) {
    forvalues i=1/`numoptions' {
      local wordi: word `i' of `options'
      if (lower("`wordi'") == "noconstant" | lower("`wordi'") == "noconstant") {
        local nocons = 1
        }
      }
    }
      
  * Check for implicit vce types, and translate to explicit vce option if
  * necessary.
  local numoptions: word count `options'
  local vceexplicit = ""
  if (`numoptions' > 0) {
    local i = 1
    local tempopt = ""
    while `i' <=`numoptions' {
      local wordi: word `i' of `options'
      * Provide for robust option
      if (lower("`wordi'")=="robust") {
        local tempopt = "`temopt' " + "vce(robust)" 
        }
       else {
        * Provide for hc2 option
        if (lower("`wordi'")=="hc2") {
          local tempopt = "`temopt' " + "vce(hc2)" 
          }
         else {
          * Provide for hc3 option
          if (lower("`wordi'")=="hc3") {
            local tempopt = "`temopt' " + "vce(hc3)" 
            }
           else {
            * Provide for cluster option
            if (substr("`wordi'",1,7)=="cluster") {
              local tempopt = "`temopt' " + regexr("`wordi'", "cluster\(([A-Za-z_\\p{L}][A-Za-z0-9_\\p{L}]+)\)", "vce\(cluster $1\)") 
              }
             else {
              * And simply pass other options along...
              local tempopt = "`tempopt' " + "`wordi'"
              }
            }
          }
        }
      local i = `i'+1
      }
    * Replace options with only explicit vce types and remaining options.
    local options = "`tempopt'"
    * Replace missing options as an empty string
    if (trim("`options'")==".") {
      local options = ""
      }
    }

  * Check for vce, set vceflag as needed, and prepare vceopt for output

  local numoptions: word count `options'
  local vceflag = 0
  if (`numoptions' > 0) {
    local i = 1
    while `i' <=`numoptions' {
      local wordi: word `i' of `options'
      local lenwordi = length("`wordi'")
      * For option vce(cluster clustervar)
      local vceoption = ""
      if ("`wordi'"=="vce(cluster") {
        local i = `i' + 1
        local next: word `i' of `options'
        local wordi = "`wordi' "+"`next'"
        * Take vce option out of options if using cluster
        local vceoption = "`wordi'"
        local vceoptargs = substr("`vceoption'",5,(length("`vceoption'")-5))
        if regexm("`options'","(.*)vce\(`vceoptargs'\)(.*)") {
          local match1 = regexs(1)
          local match2 = regexs(2)
          local options = "`match1' `match2'"
          }
         
        * Replace missing options as an empty string
        if (trim("`options'")==".") {
          local options = ""
          }
        }

        * For option vce(jackknife or vce(bootstrap, and it's suboptions)
        if ("`wordi'"=="vce(jackknife," | "`wordi'"=="vce(jack," | "`wordi'"=="vce(bootstrap," | "`wordi'"=="vce(boot,") {
          local Nrp = 0
          local Nlp = 1
          while `Nrp' != `Nlp' {
            local i = `i' + 1
            local next: word `i' of `options'
            if (strpos("`next'","(")!=0) {
              local Nlp = `Nlp' + 1
              }
            if (strpos("`next'",")")!=0) {
              local Nrp = `Nrp' + 1
              }
            local wordi = "`wordi' "+"`next'"
              }
            }

        * Format SE labels based on vce()
        if (strpos("`wordi'","vce") != 0) {
          local vceflag = 1
          local lenword = length("`wordi'")
          local vceopt = lower(substr("`wordi'",5,`lenword'-5))
          local replications = 0
          if ("`vceopt'"=="robust") {
            local vceopt = "Robust"
            }
          if ("`vceopt'"=="hc2") {
            local vceopt = "Robust HC2"
            }
          if ("`vceopt'"=="hc3") {
            local vceopt = "Robust HC3"
            }
          if (substr("`vceopt'",1,7)=="cluster") {
            local vceopt = "Cluster"
            }
          if (substr("`vceopt'",1,4)=="jack") {
            if (strpos("`vceopt'","mse")==0) {
              local vceopt = "Jackknife"
              }
             else {
              local vceopt = "{help jackknife_mse##intro_svy:Jknife *}"
              }
            if (strpos("`wordi'","cluster")!=0) {
              local replications = 1
              local repopt = substr("`wordi'",9,length("`wordi'")-1)
              if regexm("`repopt'","(.*)\)(.*)") {
                local match1 = regexs(1)
                local match2 = regexs(2)
                local repopt = "`match1' `match2'"
                }
              }
            }
          if (substr("`vceopt'",1,4)=="boot") {
            if (strpos("`vceopt'","mse")==0) {
              local vceopt = "Bootstrap"
              }
             else {
              local vceopt = "{help bootstrap_mse:Bstrap *}"
              }
            if (strpos("`wordi'","cluster")!=0) {
              local replications = 1
              local repopt = substr("`wordi'",9,length("`wordi'")-1)
              if regexm("`repopt'","(.*)\)(.*)") {
                local match1 = regexs(1)
                local match2 = regexs(2)
                local repopt = "`match1' `match2'"
                }
              }
            }
          if ("`vceopt'"=="ols") {
            local vceopt = ""
            }
          }
        local i = `i'+1
        }
      }
        
  **********
  * Address prefix command workaround

  * Check for svy() prefix
  * If there are any options...
  local svyflag = 0
  if (`numoptions' > 0) {
    qui: _svy_opts11 "`options'"
    local options = r(nonsvy_options)
    local svy_command = r(svy_command)
    if ("`svy_command'" != ", :") {
      local svyflag = 1
      }
     else {
      local svy_command = ""
      }
    }
  * Replace missing options as an empty string
  if (trim("`options'")==".") {
    local options = ""
    }
      
  * Put vce option back in
  if ("`vceoption'"!="") {
    local options = "`options' `vceoption'"
    }

  * Get the number of independent variables, and number of coefficients from the 
  * regression
  if ("`weight'" == "") {
    qui: `svy_command' regress `anything' `if' `in', `options' level(`level')
    }
   else {
    qui: `svy_command' regress `anything' `if' `in' [`weight' `exp'], `options' level(`level')
    }
      
  * vceopt
  if (`svyflag'==1) {
    local vceopt = e(vcetype)
    }
  
  * Create the local macro of coefficient names and factor names (if any)

  * First get the column names from the coefficient matrix */ 
  local vnames : colfullnames e(b)

  * Next drop omitted categories in e(b) from vnames and put results into vlist
  local numcoefficients = 0
  foreach var of local vnames {
    _ms_parse_parts `var'
    if !`r(omit)' {
      local vlist `vlist' `var'
      local numcoefficients = `numcoefficients'+1
      }   
    }

  * Use _vlformat11 to get variables and factors as appropriate
  _vlformat11 "`vlist'"
  local coefficientnames = r(variables)
  local factors = r(factors)

*    if (`nocons'==1) {
*    local numindepvars = `numcoefficients' - 1
*        }
*      else {
  local numindepvars = `numcoefficients'
*        }
                
  * Drop various suboptions that should not be used twice after the above 
  * call of regress...
  *  ...including keep 
  local options = subinstr("`options'","keep","",.)
  *  ...including idcluster(newvar)
  if regexm("`options'","(.*)idcluster\([a-zA-Z0-9]*\)(.*)") {
    local match1 = regexs(1)
    local match2 = regexs(2)
    local options = "`match1' `match2'"
    }
  *  ...including the every(#) part of saving(filename, suboptions)
  if regexm("`options'","(.*)every\([a-zA-Z0-9]*\)(.*)") {
    local match1 = regexs(1)
    local match2 = regexs(2)
    local options = "`match1' `match2'"
    }

  *  ...including saving()
  if regexm("`options'","(.*)saving\([a-zA-Z0-9]*\)(.*)") {
    local match1 = regexs(1)
    local match2 = regexs(2)
    local options = "`match1' `match2'"
    }
    
  *  And reinsure there is no more than one space before a non-space character
  local options = subinstr("`options'","  "," ",.)

  * Replace missing options as an empty string
  if (trim("`options'")==".") {
    local options = ""
    }

    **********
  * Validate eqvtype

  * If eqvtype is empty or does not exist, then supply the default (delta)
  if lower("`eqvtype'") == "" {
    local temp = ""
    forvalues i = 1/`numindepvars' {
      local temp = "`temp'" + " delta"
      }
    local eqvtype = "`temp'"
    }


  local numeqvtypes: word count `eqvtype'

  * Validate correct number of eqvtype() entries
  if !(`numeqvtypes' == 1 | `numeqvtypes' == `numindepvars') {
    noi: di as err "option eqvtype() incorrectly specified:" _newline "option eqvtype must contain either 1 entry, or exactly as many entries as" _newline "independent variables plus 1 for the constant term (unless specifying the" _newline "noconstant option, in which case just the exact number of independent variables)"
    exit 198
    }
   else {
    * Expand a single entry for eqvtype() to a list containing the same entry 
    * once for each coefficient
    if (`numeqvtypes' == 1) {
      local temp = ""
      forvalues i = 1/`numindepvars' {
        local temp = "`temp'" + " `eqvtype'"
        }
      local eqvtype = "`temp'"
      }
    }

  * Validate that each eqvtype is either "delta" or "epsilon"
  forvalues i = 1(1)`numeqvtypes' {
    local eqvtypei: word `i' of `eqvtype'
    if !(lower("`eqvtypei'") == "delta" | lower("`eqvtypei'") == "epsilon") {
      noi: di as err "option eqvtype() incorrectly specified:" _newline "each entry of eqvtype must be either delta or epsilon"
      exit 198
      }
    }

  **********
  * Validate eqvlevel

  * If eqvlevel is empty, contains a single missing value, or does not exist, 
  * then supply the appropriate default values corresponding to the eqvtype for 
  * that coefficient
  if ("`eqvlevel'"=="" | "`eqvlevel'"==".") {
    local temp = ""
    forvalues i = 1/`numindepvars' {
      local eqvtypei: word `i' of `eqvtype'
        if (lower("`eqvtypei'")=="delta") {
          local temp = "`temp'" + " 1"
          }
        if (lower("`eqvtypei'")=="epsilon") {
          local temp = "`temp'" + " 2"a
          }
        }
    numlist "`temp'"
    local eqvlevel = r(numlist)
    }

  local numeqvlevels: word count `eqvlevel'
      
  * Validate correct number of eqvlevel() values
  if !(`numeqvlevels' == 1 | `numeqvlevels' == `numindepvars') {
    noi: di as err "option eqvlevel() incorrectly specified:" _newline "option eqvlevel must contain either 1 value, or exactly as many values as" _newline "independent variables plus 1 for the constant term (unless specifying the" _newline "noconstant option, in which case just the exact number of independent variables)"
    exit 198
    }
   else {
    * Expand a single value for eqvlevel() to a list containing the same value 
    * once for each coefficient
    if (`numeqvlevels' == 1) {
      local temp = ""
      forvalues i = 1/`numindepvars' {
        local temp = "`temp'" + " `eqvlevel'"
        }
      numlist "`temp'"
      local eqvlevel = r(numlist)
      }
    }
    
    * Replace any missing eqvlevel values
    forvalues i = 1/`numindepvars' {
      local eqvleveli: word `i' of `eqvlevel'
      if (`eqvleveli' == .) {
        local eqvtypei: word `i' of `eqvtype'
        if (lower("`eqvtypei'")=="delta") {
          local temp = "`temp'" + " 1"
          }
        if (lower("`eqvtypei'")=="epsilon") {
          local temp = "`temp'" + " 2"a
          }
        }
       else {
        local temp = "`temp'" + " `eqvleveli'"
        }
      }
    numlist "`temp'"
    local eqvlevel = r(numlist)

  * Check whether uppereqvlevel exists, if not provide default value
  if ("`uppereqvlevel'"=="" | "`uppereqvlevel'"==".") {
    local uppereqvlevel = 0
    local numuppereqvlevels = 1
    }

  local numuppereqvlevels: word count `uppereqvlevel'
        
  * Validate correct number of uppereqvlevel() values
  if !(`numuppereqvlevels' == 0 | `numuppereqvlevels' == 1 | `numuppereqvlevels' == `numindepvars') {
    noi: di as err "option uppereqvlevel() incorrectly specified:" _newline "option uppereqvlevel must be empty, contain 1 value, or exactly as many values" _newline "as independent variables plus 1 for the constant term (unless specifying the " _newline "noconstant option, in which case just the exact number of independent " _newline "variables)"
    exit 198
    }
   else {
    * Expand a single value for uppereqvlevel() to a list containing the same  
    * value once for each coefficient
    if (`numuppereqvlevels' == 1) {
      local temp = ""
      forvalues i = 1/`numindepvars' {
        local temp = "`temp'" + " `uppereqvlevel'"
        }
      numlist "`temp'"
      local uppereqvlevel = r(numlist)
      }
    }

  * Validate eqvlevel default for eqvtype epsilon
  local temp = ""
  forvalues i = 1/`numindepvars' {
    local eqvleveli: word `i' of `eqvlevel'
    local uppereqvleveli: word `i' of `uppereqvlevel'
    local eqvtypei: word `i' of `eqvtype'
    if (lower("`eqvtypei'") == "epsilon") & (`eqvleveli' == 1 & (`uppereqvleveli'==. | `uppereqvleveli'==0) ) {
      local temp = "`temp'" + " 2"
      }
     else {
      local temp = "`temp'" + " `eqvleveli'"
      }
    }
  numlist "`temp'"
  local eqvlevel = r(numlist)

  * Set upper and lower with upper default values
  local tempupper = ""
  local templower = ""
  forvalues i = 1(1)`numindepvars' {
    local eqvleveli: word `i' of `eqvlevel'
    local uppereqvleveli: word `i' of `uppereqvlevel'
    if (`uppereqvleveli'==0 | `uppereqvleveli'==. | `uppereqvleveli' == abs(`eqvleveli')) {
      local tempupper = "`tempupper' " + string( abs(`eqvleveli') )
      local templower = "`templower' " + string( abs(`eqvleveli') )
      }
     else {
      local tempupper = "`tempupper'" + " `uppereqvleveli'"
      local templower = "`templower'" + " `eqvleveli'"
      }
    }
  numlist "`tempupper'"
  local upper = r(numlist)
  numlist "`templower'"
  local lower = r(numlist)

  * Set upper and lower with non-default values
  local tempupper = ""
  local templower = ""
  forvalues i = 1(1)`numindepvars' {
    local eqvleveli: word `i' of `eqvlevel'
    local uppereqvleveli: word `i' of `uppereqvlevel'
    local upperi: word `i' of `upper'
    local loweri: word `i' of `lower'
    if (`uppereqvleveli'>0 & `uppereqvleveli' != .) {
      local tempupper = "`tempupper' " + string( abs(`uppereqvleveli') )
      local templower = "`templower' " + string( abs(`eqvleveli') )
      }
     else {
      local tempupper = "`tempupper' " + "`upperi'"
      local templower = "`templower' " + "`loweri'"
      }
    }
  numlist "`tempupper'"
  local upper = r(numlist)
  numlist "`templower'"
  local lower = r(numlist)

  * Define and format alpha
  local alpha = (1 - `level'/100)
  
  * Format alpha to remove trailing zeros
  if (mod(`alpha'*1000, 1) == 0.0) {
    local alpha: di %6.3f `alpha'
    }
  if (mod(`alpha'*100, 1) == 0.0) {
    local alpha: di %5.2f `alpha'
    }
  if (mod(`alpha'*10, 1) == 0.0) {
    local alpha: di %4.1f `alpha'
    }
  if (mod(`alpha', 1) == 0.0) {
    local alpha: di %4.0f `alpha'
    }

*******************************************************************************
* The business starts here                                                    *
*******************************************************************************
  if ("`weight'" == "") {
    local regresscmd = "`svy_command' regress `anything' `if' `in', `options' level(`level')"
    if ("`relevance'" != "") {
      di as txt _newline "Regression tests for difference"
      if (`vceflag' == 0 & "`svy_command'"=="") {
        di as txt "{hline 13}{c TT}{hline 34}" _continue
        if (strpos("`vceopt'","J")!=0 | strpos("`vceopt'","B")!=0) {
          }
        }
       else {
        if ("`svy_command'"!="") {
          di as txt "{hline 47}"
          }
         else {
          di as txt "{hline 47}" _continue
          if (strpos("`vceopt'","J")!=0 | strpos("`vceopt'","B")!=0) {
            di 
            }
          }
        }
      `regresscmd'
      }
     else {
      qui: `regresscmd'
      }
    }
   else {
    local regresscmd = "`svy_command'regress `anything' `if' `in' [`weight' `exp'], `options' level(`level')"
    if ("`relevance'" != "") {
      di as txt _newline "Regression tests for difference"
      di as txt "{hline 48}"
      `regresscmd'
      }
     else {
      qui: `regresscmd'
      }
    }

  * Get the name of the dependent variable for subsequent output in tables
  local depvar = e(depvar)

  * Set degrees of freedom for t test statistics
  local df = e(df_r)
    
  * Get the coefficients, standard errors, t test statistics, and p-values from 
  * the regression
  matrix coefficients = e(b)
  
  * Retain only those coefficients that are not zero (i.e. not dropped from 
  * the estimation)
  matrix tempc = J(1,`numindepvars',0)
  local Neb = colsof(matrix(coefficients))
  if (`Neb' > `numindepvars') {
    forvalues j = 1/`numindepvars' {
      forvalues i = 1/`Neb' {
        if (matrix(coefficients[1,`i']) != 0) {
          matrix tempc[1,`j'] = coefficients[1,`i']
          matrix coefficients[1,`i'] = 0
          continue, break
          }
        }
      }
      matrix coefficients = tempc
    }

  * Retain only those variances (varcov diagonal elements) that are not zero 
  * (i.e. not dropped from the estimation)
  matrix varcov       = e(V)
  matrix tempVC       = J(1,`numindepvars',0)
  local NVC = colsof(matrix(varcov))
  if (`NVC' > `numindepvars') {
    forvalues j = 1/`numindepvars' {
      forvalues i = 1/`NVC' {
        if (matrix(varcov[`i',`i']) != 0) {
          matrix tempVC[1,`j'] = varcov[`i',`i']
          matrix varcov[`i',`i'] = 0
          continue, break
          }
        }
      }
    }
   else {
    forvalues i = 1/`numindepvars' {
      matrix tempVC[1,`i'] = varcov[`i',`i']
      }
    }
  matrix varcov = tempVC

  * Set up a variety of vectors of the correct size to receive various statistics
  matrix stderrs      = coefficients
  matrix T            = coefficients
  matrix T1           = coefficients
  matrix T2           = coefficients
  matrix P            = coefficients
  matrix P1           = coefficients
  matrix P2           = coefficients
  
  * Prepare for positivist and negativist decisions
  local PositivistDecision = ""
  local NegativistDecision = ""
  
  forvalues i = 1/`numindepvars' {
    matrix stderrs[1,`i'] = sqrt(varcov[1,`i'])
    local eqvtypei: word `i' of `eqvtype'
    local upperi: word `i' of `upper'
    local loweri: word `i' of `lower'
    local coefficienti = coefficients[1,`i']
    local stderri = stderrs[1,`i']
    matrix T[1,`i'] = `coefficienti'/`stderri'
          * If not using bootstrap
          if (strpos("`vceopt'","B")==0) {
      matrix P[1,`i'] = ttail(`df', abs(T[1,`i']))
              }
            * Otherwise if using bootstrap
            else {
      matrix P[1,`i'] = 1-normal(abs(T[1,`i']))
              }
    
    *Determine positivist rejection decision
    if (matrix(P[1,`i']) <= `alpha'/2) {
      local PositivistDecision = "`PositivistDecision'" + " Reject"
      }
     else {
      local PositivistDecision = "`PositivistDecision'" + " NotReject"
      }

    *Calculate T1, T2, P1, and P2
    if (lower("`eqvtypei'") == "delta") {
      matrix T1[1,`i'] = (`upperi' - `coefficienti')/`stderri'
      matrix T2[1,`i'] = (`coefficienti' + `loweri')/`stderri'
      }
     else {
      matrix T1[1,`i'] = `upperi' - T[1,`i']
      matrix T2[1,`i'] = T[1,`i'] + `loweri'
      }
          * If not using bootstrap
          if (strpos("`vceopt'","B")==0) {
      matrix P1[1,`i'] = ttail(`df', T1[1,`i'])
      matrix P2[1,`i'] = ttail(`df', T2[1,`i'])
              }
            * Otherwise if using bootstrap
            else {
      matrix P1[1,`i'] = 1-normal(T1[1,`i'])
      matrix P2[1,`i'] = 1-normal(T2[1,`i'])
              }
    *Determine negativist rejection decision
    if (P1[1,`i'] <= `alpha' & P2[1,`i'] <= `alpha') {
      local NegativistDecision = "`NegativistDecision'" + " Reject"
      }
     else {
      local NegativistDecision = "`NegativistDecision'" + " NotReject"
      }
    }
        
  * Regression test for equivalence table header
  local symmetric = 1
  forvalues i = 1/`numindepvars' {
    local loweri: word `i' of `lower'
    local upperi: word `i' of `upper'
    if (`loweri' != `upperi') {
      local symmetric = 0
      }
    }

  * Determine if there are any factors, and set factorflag=1 if there are
  local factorflag = 0
  forvalues i = 1/`numindepvars' {
    local factori: word `i' of `factors'
    if ("`factori'"!=".") {
      local factorflag=1
      }
    }

  _tostregressheader11 "`depvar'" `factorflag' `symmetric' "`vceopt'" "`repopt'"

  * Regression test for equivalence table entries
  forvalues i = 1/`numindepvars' {
    * independent variable or constant name
    local indepvari: word `i' of `coefficientnames'
    * current factor name for this variable
    local factori: word `i' of `factors'
    *previous factor name for previous variable
    if (`i' == 1) {
      local previousfactori = "0"
      }
      else {
       local previous = `i' - 1
       local previousfactori: word `previous' of `factors'
       }
    
    * The particulars for that variable or constant
    local coefficienti = coefficients[1,`i']
    local stderri = stderrs[1,`i']
    local t1 = T1[1,`i']
    local p1 = P1[1,`i']
    local t2 = T2[1,`i']
    local p2 = P2[1,`i']
    local eqvtypei: word `i' of `eqvtype'
    local upperi: word `i' of `upper'
    local loweri: word `i' of `lower'
    _tostregressentry11 `indepvari' `factori' `previousfactori' `coefficienti' `stderri' `t1' `p1' `t2' `p2' `eqvtypei' `upperi' `loweri'
    }

  * Close the equivalence test table
  if (`symmetric') {
    di as txt "{hline 13}{c BT}{hline 80}"
    }
   else {
    di as txt "{hline 13}{c BT}{hline 86}"
    }
  
  * Relevance test table
  if ("`relevance'"!="") {
            
      * Relevance table header
    _relevanceregheader11 "`depvar'" `factorflag' `alpha' `symmetric'
        
    * Relevance table entries
    forvalues i = 1/`numindepvars' {
      * independent variable or constant name
      local indepvari: word `i' of `coefficientnames'
      * current factor name for this variable
      local factori: word `i' of `factors'
      *previous factor name for previous variable
      if (`i' == 1) {
        local previousfactori = "0"
        }
       else {
        local previous = `i' - 1
        local previousfactori: word `previous' of `factors'
        }

        * The particulars for that variable or constant
        local eqvtypei: word `i' of `eqvtype'
        local upperi: word `i' of `upper'
        local loweri: word `i' of `lower'
        local positivistdecisioni: word `i' of `PositivistDecision'
        local negativistdecisioni: word `i' of `NegativistDecision'
        _relevanceregentry11 `indepvari' `factori' `previousfactori' `eqvtypei' `upperi' `loweri' `positivistdecisioni' `negativistdecisioni' 
        }

    * Close the table
  
    if (`symmetric') {
      di as txt "{hline 13}{c BT}{hline 67}"
      }
     else {
      di as txt "{hline 13}{c BT}{hline 73}"
      }
    }

*******************************************************************************
* Program end. Close up shop and return things.                               *
*******************************************************************************

  * Prep a few things pertaining to svy and relevance options
  if (`svyflag'==1) {
    local svypref = "Survey: "
    }
   else {
    local svypref = ""
    }
  if ("`relevance'"=="") {
    local title = "`svypref'Regression tests for equivalence"
    }
   else {
    local title = "`svypref'Regression relevance tests"
    }
       
  * Create the local string of relevance conclusions
  local conclusions = ""
  forvalues i = 1/`numindepvars' {
    local positivistdecisioni: word `i' of `PositivistDecision'
    local negativistdecisioni: word `i' of `NegativistDecision'
    _relevanceconclusion `positivistdecisioni' `negativistdecisioni'
    local conclusion = r(conclusion)
    local conclusions = "`conclusions' `conclusion'"
    }

  ereturn scalar alpha = `alpha'
  if ("`relevance'"!="") {
    ereturn local rel_conclusions `conclusions'
    }
  ereturn local lowereqvlevel `lower'
  ereturn local uppereqvlevel `upper'
  ereturn local eqvtype `eqvtype'
  ereturn local title `title'
  ereturn local cmdline `cmdline'
  ereturn local cmd "tostregress"
  ereturn matrix P2 P2
  ereturn matrix P1 P1
  ereturn matrix T2 T2
  ereturn matrix T1 T1

  end


program define _tostregressheader11

  args depvar factorflag symmetric vceopt

  local abbrevdepvar = abbrev("`depvar'",12)
  local padadv = 12 - length("`abbrevdepvar'")
  di as txt _newline "Regression tests for equivalence"
  if ("`vceopt'"=="Cluster") {
    local clusters = e(N_clust)
    local clusterfmt = "%"+string(ceil(log10(`clusters'))+3)+".0gc"
    local clusterfmt: display "`clusterfmt'"
    local clusters: display `clusterfmt' `clusters'
    local clusters = substr("`clusters'",2,.)
    local clusNlen = length("`clusters'")
    local clusvar = e(clustvar)
    local clusvarlen = length("`clusvar'")
    local symgap = 6*(1-`symmetric')
    local gap = 56+`symgap'-`clusNlen'-`clusvarlen'
    di as txt "{dup `gap': }(Std. Err. adjusted for " as res "`clusters'" as txt " clusters in " as res "`clusvar'" as txt ")"
    local vceopt = "Robust"
    }
  if ("`repopt'"!="") {
    local clusters = e(N_clust)
    local clusterfmt = "%"+string(ceil(log10(`clusters'))+3)+".0gc"
    local clusterfmt: display "`clusterfmt'"
    local clusters: display `clusterfmt' `clusters'
    local clusters = substr("`clusters'",2,.)
    local clusNlen = length("`clusters'")
    local clusvarlen = length("`repopt'")
    local symgap = 6*(1-`symmetric')
    local gap = 57+`symgap'-`clusNlen'-`clusvarlen'
    di as txt "{dup `gap': }(Replications based on " as res "`clusters'" as txt " clusters in " as res "`repopt'" as txt ")"
    }
  local jackpad = 0
  if (strpos("`vceopt'","J")!=0 | strpos("`vceopt'","B")!=0) {
    local jackpad = 1
    }
  if (strpos("`vceopt'","B")!=0) {
    local obscoef = "{dup 3: }Observed{dup 2: }"
    local teststat = "z"
    local TESTSTAT = "Z"
    }
   else {
    local obscoef = "{dup 13: }"
    local teststat = "t"
    local TESTSTAT = "T"
    }

  if (`symmetric') {
    di as txt "{hline 13}{c TT}{hline 80}"
        if ("`vceopt'"!="") {
      di as txt "{dup 12: } {c |}" "`obscoef'" "{dup `jackpad': }" %~10s "`vceopt'"
            }
    di as txt "{dup `padadv': }`abbrevdepvar' {c |}      Coef.   Std. Err.     `teststat'1   P(`TESTSTAT'>`teststat'1)     `teststat'2   P(`TESTSTAT'>`teststat'2)    eqvtype    eqvlevel"
    di as txt "{hline 13}{c +}{hline 80}"
    }
   else {
    di as txt "{hline 13}{c TT}{hline 86}"
        if ("`vceopt'"!="") {
      di as txt "{dup 12: } {c |}" "`obscoef'" "{dup `jackpad': }" %~10s "`vceopt'"
            }
    di as txt "{dup `padadv': }`abbrevdepvar' {c |}      Coef.   Std. Err.     `teststat'1   P(`TESTSTAT'>`teststat'1)     `teststat'2   P(`TESTSTAT'>`teststat'2)    eqvtype    lower    upper"
    di as txt "{hline 13}{c +}{hline 86}"
    }
  end

program define _tostregressentry11

  args indepvar factor previousfactor coefficient stderr t1 p1 t2 p2 eqvtype upper lower

  local factorflag = 0
  
  * Print out the factor header if there is one, and it is not the same as the 
  * previousfactor
  if ("`factor'"!="." & "`factor'"!="`previousfactor'") {
    local padfactor = 12 - length("`factor'")
    di as txt "{dup 13: }{c |}" _newline "{dup `padfactor': }`factor' {c |}"
    }

  * Set the factorflag if factor not missing
  if ("`factor'"!=".") {
    local factorflag = 1
    }

  * Print a breakline if previousfactor was not missing, and is different than
  * factor
  if ("`previousfactor'"!="." & "`previousfactor'"!="`factor'" & "`previousfactor'"!="0") {
    di as txt "{dup 13: }{c |}"
    }

  * Respaceify indepvar
  local indepvar = subinstr("`indepvar'","_QQQQQQ_"," ",.)

  * Produce abbreviated independent variable name
  local abbrevindepvar = abbrev("`indepvar'",12-`factorflag')
  local padaiv = 12 - length("`abbrevindepvar'") - `factorflag'
  
  * Format eqvtype
  local eqvtype = lower("`eqvtype'")
  if ("`eqvtype'"=="delta") {
    local eqvtype = "Delta"
    }
  
  * Adjust the format string for t1 and t2 if they are exceptionally large

  * Format for t1
  local f_t1 = "%6.2f"
  if (`t1'>=1000) {
    local f_t1 = "%6.1f"
    if (`t1'>=10000) {
      local f_t1 = "%6.0f"
      if (`t1'>=100000) {
        local f_t1 = "%6.0g"
        }
      }
    }

  * Format for t2
  local f_t2 = "%6.2f"
  if (`t2'>=1000) {
    local f_t2 = "%6.1f"
    if (`t2'>=10000) {
      local f_t2 = "%6.0f"
      if (`t2'>=100000) {
        local f_t2 = "%6.0g"
        }
      }
    }
    
  * Display the entry
  if (`upper' == `lower') {
    di as txt "{dup `padaiv': }`abbrevindepvar' {dup `factorflag': }{c |}  " as res %9.0g `coefficient' "  " %9.0g `stderr' "   " `f_t1' `t1' "   " %06.4f `p1' "  " `f_t2' `t2' "   " %06.4f `p2' "    " %~7s "`eqvtype'"  "     " %6.0g `lower'
 *   di as txt "{hline 13}{c +}{hline 81}"
    }
   else {
    di as txt "{dup `padaiv': }`abbrevindepvar' {dup `factorflag': }{c |}  " as res %9.0g `coefficient' "  " %9.0g `stderr' "   " `f_t1' `t1' "   " %06.4f `p1' "  " `f_t2' `t2' "   " %06.4f `p2' "    " %~7s "`eqvtype'" "   "    %6.0g -1*`lower' "   "   %6.0g `upper'
 *   di as txt "{hline 13}{c +}{hline 88}"
    }
  end

    
program define _relevanceregheader11

  args depvar factorflag alpha symmetric

  * Produce abbreviated dependent variable name
  local abbrevdepvar = abbrev("`depvar'",12)
  local padadv = 12 - length("`abbrevdepvar'")
  
  di as txt _newline "Regression relevance tests (alpha = " as res "0" `alpha' as txt ")"
  if (`symmetric' == 1) {
    di as txt "{hline 13}{c TT}{hline 67}"
    di as txt "{dup `padadv': }`abbrevdepvar' {c |}  eqvtypeout   eqvlevel       Ho+         Ho-           Conclude"
    di as txt "{hline 13}{c +}{hline 67}"
    }
   else {
    di as txt "{hline 13}{c TT}{hline 73}"
    di as txt "{dup `padadv': }`abbrevdepvar' {c |}  eqvtypeout   lower    upper       Ho+         Ho-           Conclude"
    di as txt "{hline 13}{c +}{hline 73}"
    }
  end

    
program define _relevanceregentry11

  args indepvar factor previousfactor eqvtype upper lower positivistdecision negativistdecision 

  local factorflag = 0
  
  * Print out the factor header if there is one, and it is not the same as the 
  * previousfactor
  if ("`factor'"!="." & "`factor'"!="`previousfactor'") {
    local padfactor = 12 - length("`factor'")
    di as txt "{dup 13: }{c |}" _newline "{dup `padfactor': }`factor' {c |}"
    }

  * Set the factorflag if factor not missing
  if ("`factor'"!=".") {
    local factorflag = 1
    }

  * Print a breakline if previousfactor was not missing, and is different than
  * factor
  if ("`previousfactor'"!="." & "`previousfactor'"!="`factor'" & "`previousfactor'"!="0") {
    di as txt "{dup 13: }{c |}"
    }
  
  * Respaceify indepvar
  local indepvar = subinstr("`indepvar'","_QQQQQQ_"," ",.)

  * Produce abbreviated independent variable name
  local abbrevindepvar = abbrev("`indepvar'",12-`factorflag')
  local padaiv = 12 - length("`abbrevindepvar'") - `factorflag'
  
  * Format eqvtype
  local eqvtype = lower("`eqvtype'")
  if ("`eqvtype'"=="delta") {
    local eqvtype = "Delta"
    }
        
  * Format Not Reject
  if ("`positivistdecision'" == "NotReject") {
    local positivistdecision = "Not Reject"
    }
  if ("`negativistdecision'" == "NotReject") {
    local negativistdecision = "Not Reject"
    }
  
  * Create conclusion
  if ("`positivistdecision'" == "Reject" & "`negativistdecision'" == "Not Reject") {
    local conclusion = "Relevant Difference"
    }
  if ("`positivistdecision'" == "Reject" & "`negativistdecision'" == "Reject") {
    local conclusion = "Trivial Difference"
    }
  if ("`positivistdecision'" == "Not Reject" & "`negativistdecision'" == "Not Reject") {
    local conclusion = "Inconclusive"
    }
  if ("`positivistdecision'" == "Not Reject" & "`negativistdecision'" == "Reject") {
    local conclusion = "Equivalence"
    }
  
  * Display the entry
  if (`upper' == `lower') {
    di as txt "{dup `padaiv': }`abbrevindepvar' {dup `factorflag': }{c |}  " as res %~7s "`eqvtype'" "    " %6.0g `lower' "    " %~10s "`positivistdecision'" "  " %~10s "`negativistdecision'" "   " %~19s "`conclusion'"
    }
   else {
    di as txt "{dup `padaiv': }`abbrevindepvar' {dup `factorflag': }{c |}  " as res %~7s "`eqvtype'" "  " %6.0g `lower' "   " %6.0g -1*`upper' "   " %~10s "`positivistdecision'" "  " %~10s "`negativistdecision'" "   " %~19s "`conclusion'"
    }
  end

    
* _vlformat11 returns a local containing suitably formatted variable names, 
* whether or not they are derived from names containing factor variable 
* operators, and a local containing the corresponding factor name, if any, for
* each variable, including the constant term, if any.
program define _vlformat11, rclass

  * varlist is obtained from the names associated with e(b) after calling 
  * regress and cleaning them of omitted vars.
  args varlist
  local N: word count `varlist'
  local factorlist = ""
  local variablelist = ""
  forvalues i = 1/`N' {
    local current: word `i' of `varlist'
    * Test whether current has factor variable operators if not "_cons"
    if ("`current'"!="_cons") {
      fvexpand `current'
      local fvtest = r(fvops)
      local tstest = r(tsops)
      * tostregress does not support variables with *both* factor operators 
      * *and* ts operators
      if ("`fvtest'"=="true" & "`tstest'"=="true") {
        noi: di as err "tostregress supports {help fvvarlist:factor operators} and {help tsvarlist:ts operators}, but not both on the same variable."
        exit 198
        }
      
      * If current does have factor variable operators, use _fvopname11 to get the
      * factor name, and factor value name for factorlist and variablelist
      if ("`fvtest'"=="true") {
        _fvopname11 `current'
        local variablelist = "`variablelist' " + r(factorvalue)
            
        * Get the current factorname.
        local factorlist = "`factorlist' " + r(factorname)
        }

       * Otherwise...
       else {
        * If current does have tsoperators, use _tsopname11 to get the
        * ts operator's canonical prefix, and variable name
        if ("`tstest'"=="true") {
          _tsopname11 `current'
          local variablelist = "`variablelist' " + r(tsprefix)

          * Get the current varname.
          local factorlist = "`factorlist' " + r(rootvar)
          }
      
         * Otherwise
         else {
          * Treat as a single continuous variable
          local factorlist = "`factorlist' " + "."
          local variablelist = "`variablelist' " + "`current'"
          }
        }
      }

     * But if current does equal "_cons"
     else {
      local factorlist = "`factorlist' " + "."
      local variablelist = "`variablelist' " + "_cons"
      }
    }
  
  return local factors `factorlist'
  return local variables `variablelist'
  end

* _fvopname11 returns a suitably formatted factor name, and a suitably 
* formatted factor value based on an input string s containing one or more 
* factor operators.
program define _fvopname11, rclass

  args s
  _nvars11 "`s'"
  local nvars = r(nvars)

  local cleaned = subinstr("`s'","#"," ",.)
  local cleaned = subinstr("`cleaned'","ibn.","",.)
*  local cleaned = subinstr("`cleaned'","ib[0-9\s\/]+\.","",)
  local cleaned = subinstr("`cleaned'","ib(first).","",.)
  local cleaned = subinstr("`cleaned'","ib(last).","",.)
  local cleaned = subinstr("`cleaned'","ib(freq).","",.)
  local cleaned = subinstr("`cleaned'","i.","",.)
  local cleaned = subinstr("`cleaned'","b.",".",.)
  local factorname = ""
  local factorvalue = ""

  forvalues i = 1/`nvars' {

    local current: word `i' of `cleaned'
    * Get the factor name and factor value, first for the case of a variable 
    * declared continuous
    if (substr("`current'",1,2)=="c.") {
      local factorname = "`factorname'" + ".#"
      local factorvalue = "`factorvalue'`current'#"
      }

    * Otherwise in the case of a factor variable
     else {
      _factorname11 `current'
      local FN = r(factorname)
      local factorname = "`factorname'"+"`FN'#"
    
      * Get the factor values
      _factorvalue11 `current'
      local FV = r(factorvalue)
      * use "_QQQQQQ_" as a placeholder for spaces in the FV
      local FV = subinstr("`FV'"," ","_QQQQQQ_",.)
      local factorvalue = "`factorvalue'"+"`FV'#"
      }
    }
  local collapsefactorname = subinstr("`factorname'",".","",.)
  local collapsefactorname = subinstr("`collapsefactorname'","#","",.)
  if ("`collapsefactorname'"=="") {
    local factorname = "."
    }
  local fnlen = length("`factorname'")
  if (substr("`factorname'",`fnlen',1)=="#") {
    local factorname = substr("`factorname'",1,`fnlen'-1)
    }
  local fvlen = length("`factorvalue'")
  if (substr("`factorvalue'",`fvlen',1)=="#") {
    local factorvalue = substr("`factorvalue'",1,`fvlen'-1)
    }
  return local factorname `factorname'
  return local factorvalue `factorvalue'
  end

    
* _nvars11 returns the number of strings separated by "#" or "##"
program define _nvars11, rclass

  args s1

  local lens1 = length("`s1'")
  local loops = `lens1' - 1
  local count = 1
  local i = 2

  if (substr("`s1'",1,1) == "#" | substr("`s1'",`lens1',1) == "#") {
  noi: di as err "invalid input: cannot begin or end with '#'"
  exit 198
      }
  
  while `i' <= `loops' {
    if (substr("`s1'",`i',3) == "###") {
    noi: di as err "invalid input: cannot contain '###'"
    exit 198
          }
    if (substr("`s1'",`i',2) == "##") {
      local count = `count' + 1
      local i = `i' + 2
      }
     else {
      if (substr("`s1'",`i',1) == "#") {
        local count = `count' + 1
        }
      local i = `i' + 1
      }
    }
  return local nvars `count'
  end
    
    
* _factorname11 returns the factor name component of s    
program define _factorname11, rclass

  * s is expected to take the form FV.FN, where:
  * FV is a numeric value
  * FN is the name of a factor variable
  args s
    
  if (substr("`s'",1,2)== "c.") {
    return local factorname .
    }
   else {
    local period = strpos("`s'",".")
    local factorname = substr("`s'",`period'+1,.)
    return local factorname `factorname'
    }
  end

    
* _factorvalue11 returns the label corresponding to the factor value component of
* s, or simply returns the factor value if the factor has no value label, or if
* the specific value does not have an entry in the value label.
program define _factorvalue11, rclass

  * s is expected to take the form FV.FN, where:
  * FV is a numeric value
  * FN is the name of a factor variable
  args s

  _factorname11 "`s'"
  local factor = r(factorname)
  local period = strpos("`s'",".")
  local factorvalue = substr("`s'",1,`period'-1)
  * Strip out factor value bas value operator stuff
  if regexm("`factorvalue'","[a-zA-Z]*([0-9]+)[a-zA-Z]*") {
    local factorvalue = real(regexs(1))
    }
  local factorvaluelabel: value label `factor'
  if ("`factorvaluelabel'"!="") {
    local temp: label `factorvaluelabel' `factorvalue' 
    if ("`factorvalue'"=="`temp'") {
      return local factorvalue `factorvalue'
      }
     else {
      return local factorvalue `temp'
      }
    }
   else {
    return local factorvalue `factorvalue'
    }
  end


* _tsopname11 returns the canonical form of any tsoperators prefixing s, and 
* returns and empty string if s has no tsoperators prefixed.
program define _tsopname11, rclass

  * s is expected to take the form TSO.VAR, where:
  * TSO is one or more time series operators of the form [DFLS][DFLS0-9]*\.
  *    NOTE: the character in the second bracket, if not a digit must match the 
  *    character in the first bracket.
  * VAR may include factor variable operators.
  args s
    
  * Get the root variable underlying s, first just s if s contains no '.'
  * local tsflag indicates whether s contains ts operators
  local tsflag = 0
  if (strpos("`s'",".")==0) {
    local rootvar = "`s'"
    }
   * Otherwise, just the stuff following the final '.'
   else {
    qui: fvexpand `s'
    local tsflag = r(tsops)
    if ("`tsflag'" == "true") {
      local tsflag = 1
      }
     else {
      local tsflag = 0
      }
    if regexm("`s'",".*\.([A-Za-z_\\p{L}][A-Za-z0-9_\\p{L}]+)") {
      local rootvar = regexs(1)
      }
    }
    
  * Locate and condense tsops
  if (`tsflag' == 1) {
    
    * Uppercase
    local s = upper("`s'")

    * capture and condense difference prefix, begin with no D ts operator prefix
    if regexm("`s'","([\w\.]*\.)?([D][D0-9]*)\..*") {
      local Dcap = regexs(2)
      }
    if ("`Dcap'"=="`s'") {
      local Dcap = ""
      }
     * Otherwise, condense Dcap
     else {
      local length = length("`Dcap'")
      local Dcount = 0
      local digitcount = 0
      local digitsum = 0
      local i = 1
      while `i' <= `length' {
        local character = substr("`Dcap'",`i',1)
        if ("`character'" == "D") { 
          local Dcount = `Dcount' + 1
          }
         else {
          * Check if 
          local nextcharacter = substr("`Dcap'",`i'+1,1)
          if ("`nextcharacter'" != "D") {
            local character = "`character'"+"`nextcharacter'"
            local i = `i' + 1
            }
          local digitcount = `digitcount' + 1
          local digitsum = `digitsum' + real("`character'")
          }
        local i = `i' + 1
        }
      local Ddigits = `Dcount' - `digitcount' + `digitsum'
      if (`Ddigits' == 1) {
        local Dcondensed = "D"
        }
       else {
        local Dcondensed = "D`Ddigits'"
        }
      }

    * capture and condense difference prefix, begin with no F ts operator prefix
    if regexm("`s'","([\w\.]*\.)?([F][F0-9]*)\..*") {
      local Fcap = regexs(2)
      }
    if ("`Fcap'"=="`s'") {
      local Fcap = ""
      }
     * Otherwise, condense Fcap
     else {
      local length = length("`Fcap'")
      local Fcount = 0
      local digitcount = 0
      local digitsum = 0
      local i = 1
      while `i' <= `length' {
        local character = substr("`Fcap'",`i',1)
        if ("`character'" == "F") { 
          local Fcount = `Fcount' + 1
          }
         else {
          local nextcharacter = substr("`Fcap'",`i'+1,1)
          if ("`nextcharacter'" != "F") {
            local character = "`character'"+"`nextcharacter'"
            local i = `i' + 1
            }
          local digitcount = `digitcount' + 1
          local digitsum = `digitsum' + real("`character'")
          }
          local i = `i' + 1
        }
      local Fdigits = `Fcount' - `digitcount' + `digitsum'
      if (`Fdigits' == 1) {
        local Fcondensed = "F"
        }
       else {
        local Fcondensed = "F`Fdigits'"
        }
      }

    * capture and condense difference prefix, begin with no L ts operator prefix
    if regexm("`s'","([\w\.]*\.)?([L][L0-9]*)\..*") {
      local Lcap = regexs(2)
      }
    if ("`Lcap'"=="`s'") {
      local Lcap = ""
      }
     * Otherwise, condense Lcap
     else {
      local length = length("`Lcap'")
      local Lcount = 0
      local digitcount = 0
      local digitsum = 0
      local i = 1
      while `i' <= `length' {
        local character = substr("`Lcap'",`i',1)
        if ("`character'" == "L") { 
          local Lcount = `Lcount' + 1
          }
         else {
          local nextcharacter = substr("`Lcap'",`i'+1,1)
          if ("`nextcharacter'" != "L") {
            local character = "`character'"+"`nextcharacter'"
            local i = `i' + 1
            }
          local digitcount = `digitcount' + 1
          local digitsum = `digitsum' + real("`character'")
          }
        local i = `i' + 1
        }
      local Ldigits = `Lcount' - `digitcount' + `digitsum'
      if (`Ldigits' == 1) {
        local Lcondensed = "L"
        }
       else {
        local Lcondensed = "L`Ldigits'"
        }
      }

  * capture and condense difference prefix, begin with no S ts operator prefix
  if regexm("`s'","([\w\.]*\.)?([S][S0-9]*)\..*") {
    local Scap = regexs(2)
    }
  if ("`Scap'"=="`s'") {
    local Scap = ""
    }
   * Otherwise, condense Scap
   else {
    local length = length("`Scap'")
    local Scount = 0
    local digitcount = 0
    local digitsum = 0
    local i = 1
    while `i' <= `length' {
      local character = substr("`Scap'",`i',1)
      if ("`character'" == "S") { 
        local Scount = `Scount' + 1
        }
       else {
        local nextcharacter = substr("`Scap'",`i'+1,1)
        if ("`nextcharacter'" != "S") {
          local character = "`character'"+"`nextcharacter'"
          local i = `i' + 1
          }
        local digitcount = `digitcount' + 1
        local digitsum = `digitsum' + real("`character'")
        }
      local i = `i' + 1
      }
    local Sdigits = `Scount' - `digitcount' + `digitsum'
    if (`Sdigits' == 1) {
      local Scondensed = "S"
      }
     else {
      local Scondensed = "S`Sdigits'"
      }
    }

  }

  if ("`Dcondensed'" != "") {
    local Dcondensed = "`Dcondensed'."
    }
  if ("`Fcondensed'" != "") {
    local Fcondensed = "`Fcondensed'."
    }
  if ("`Lcondensed'" != "") {
    local Lcondensed = "`Lcondensed'."
    }
  if ("`Scondensed'" != "") {
    local Scondensed = "`Scondensed'."
    }
  local tscanonical = "`Lcondensed'`Fcondensed'`Dcondensed'`Scondensed'`rootvar'"
  local tsprefix = "`Lcondensed'`Fcondensed'`Dcondensed'`Scondensed'"
  local tsprefix = substr("`tsprefix'",1,length("`tsprefix'")-1)
  return local tscanonical `tscanonical'
  return local tsprefix `tsprefix'
  return local rootvar `rootvar'
  end
        
        
* _svy_opts11 takes an options macro, and returns (a) the options macro stripped
* of svy()-related options and suboptions, (b) a properly structured svy_prefix, 
* and (c) any svy-options
program define _svy_opts11, rclass

  args opts

  * Output macros
  local nonsvy_options = ""
  local svy_prefix = ""
  local svy_options = ""

  * Number of left and right parentheses
  local Nlp = 0
  local Nrp = 0

  * svy parenthesis open
  local svyparen = 0

  * Insure no double spaces 
  local opts = subinstr("`opts'"," +"," ",.)
    
  * Insure no space before left parenthesis
  local opts = subinstr("`opts'"," (","(",.)

  * Insure space after left parenthesis
  local opts = subinstr("`opts'","(","( ",.)

  * Insure space before and after right parenthesis
  local opts = subinstr("`opts'",")"," ) ",.)

  * Reinsure no double spaces 
  local opts = subinstr("`opts'"," +"," ",.)    
    
  * Number of options
  local numoptions: word count `opts'
    
  * Manual while loop across all the options
  local i = 1
  while `i' <= `numoptions' {
    local wordi: word `i' of `opts'
    local i = `i' + 1
        
    * Check if svyparen opens
    if ("`wordi'"=="svy(") {
      local svyparen = 1
      local Nlp = `Nlp' + 1
      local svy_prefix = "svy"
      continue
      }
        
    * Check if svyparen is open
    if (`svyparen'==1) {
      * Left parenthesis?
      if (strpos("`wordi'","(")!=0) {
        local Nlp = `Nlp' + 1
        }
      * Right parenthesis?
      if (strpos("`wordi'",")")!=0) {
        local Nrp = `Nrp' + 1
        }
      * Close svyparen?
      if (`Nlp'==`Nrp') {
        local svyparen = 0
        *local svy_options = "`svy_options' `wordi'"
        }
       else {
        * Check for vcetype
        if (substr("`wordi'",1,4)=="jack" | substr("`wordi'",1,4)=="boot" | substr("`wordi'",1,4)=="line" | substr("`wordi'",1,3)=="brr" | substr("`wordi'",1,3)=="sdr") {
          local svy_prefix = "`svy_prefix' `wordi'"
          }
         * Otherwise if not vcetype, add to svy_options
         else {
          local svy_options = "`svy_options' `wordi'"
          }
        }
      }
     else {
      * Left parenthesis?
      if (strpos("`wordi'","(")!=0) {
        local Nlp = `Nlp' + 1
        }
      * Right parenthesis?
      if (strpos("`wordi'",")")!=0) {
        local Nrp = `Nrp' + 1
        }
      local nonsvy_options = "`nonsvy_options' `wordi'"
      }
    }

  local nonsvy_options = subinstr("`nonsvy_options'","( ","(",.)
  local nonsvy_options = subinstr("`nonsvy_options'"," )",")",.)
  if trim("`nonsvy_options'") == "." {
    local nonsvy_options = ""
    }
  local svy_options = subinstr("`svy_options'","( ","(",.)
  local svy_options = subinstr("`svy_options'"," )",")",.)
  if trim("`svy_options'") == "." {
    local svy_options = ""
    }
  local svy_command = rtrim("`svy_prefix'")+", "+rtrim(ltrim("`svy_options'"))+":"
    
  return local nonsvy_options `nonsvy_options'
  return local svy_command `svy_command'
  end


program define _tostregressheader14

  args depvar factorflag symmetric vceopt

  local abbrevdepvar = abbrev("`depvar'",12)
  local padadv = 12 - ustrlen("`abbrevdepvar'")
  di as txt _newline "Regression tests for equivalence"
  if ("`vceopt'"=="Cluster") {
    local clusters = e(N_clust)
    local clusterfmt = "%"+string(ceil(log10(`clusters'))+3)+".0gc"
    local clusterfmt: display "`clusterfmt'"
    local clusters: display `clusterfmt' `clusters'
    local clusters = usubstr("`clusters'",2,.)
    local clusNlen = ustrlen("`clusters'")
    local clusvar = e(clustvar)
    local clusvarlen = ustrlen("`clusvar'")
    local symgap = 6*(1-`symmetric')
    local gap = 56+`symgap'-`clusNlen'-`clusvarlen'
    di as txt "{dup `gap': }(Std. Err. adjusted for " as res "`clusters'" as txt " clusters in " as res "`clusvar'" as txt ")"
    local vceopt = "Robust"
    }
  if ("`repopt'"!="") {
    local clusters = e(N_clust)
    local clusterfmt = "%"+string(ceil(log10(`clusters'))+3)+".0gc"
    local clusterfmt: display "`clusterfmt'"
    local clusters: display `clusterfmt' `clusters'
    local clusters = usubstr("`clusters'",2,.)
    local clusNlen = ustrlen("`clusters'")
    local clusvarlen = ustrlen("`repopt'")
    local symgap = 6*(1-`symmetric')
    local gap = 57+`symgap'-`clusNlen'-`clusvarlen'
    di as txt "{dup `gap': }(Replications based on " as res "`clusters'" as txt " clusters in " as res "`repopt'" as txt ")"
    }
  local jackpad = 0
  if (ustrpos("`vceopt'","J")!=0 | ustrpos("`vceopt'","B")!=0) {
    local jackpad = 1
    }
  if (ustrpos("`vceopt'","B")!=0) {
    local obscoef = "{dup 3: }Observed{dup 2: }"
    local teststat = "z"
    local TESTSTAT = "Z"
    }
   else {
    local obscoef = "{dup 13: }"
    local teststat = "t"
    local TESTSTAT = "T"
    }

  if (`symmetric') {
    di as txt "{hline 13}{c TT}{hline 80}"
        if ("`vceopt'"!="") {
      di as txt "{dup 12: } {c |}" "`obscoef'" "{dup `jackpad': }" %~10s "`vceopt'"
            }
    di as txt "{dup `padadv': }`abbrevdepvar' {c |}      Coef.   Std. Err.     `teststat'1   P(`TESTSTAT'>`teststat'1)     `teststat'2   P(`TESTSTAT'>`teststat'2)    eqvtype    eqvlevel"
    di as txt "{hline 13}{c +}{hline 80}"
    }
   else {
    di as txt "{hline 13}{c TT}{hline 86}"
        if ("`vceopt'"!="") {
      di as txt "{dup 12: } {c |}" "`obscoef'" "{dup `jackpad': }" %~10s "`vceopt'"
            }
    di as txt "{dup `padadv': }`abbrevdepvar' {c |}      Coef.   Std. Err.     `teststat'1   P(`TESTSTAT'>`teststat'1)     `teststat'2   P(`TESTSTAT'>`teststat'2)    eqvtype    lower    upper"
    di as txt "{hline 13}{c +}{hline 86}"
    }
  end

program define _tostregressentry14

  args indepvar factor previousfactor coefficient stderr t1 p1 t2 p2 eqvtype upper lower alpha

  local factorflag = 0
  
  * Print out the factor header if there is one, and it is not the same as the 
  * previousfactor
  if ("`factor'"!="." & "`factor'"!="`previousfactor'") {
    local padfactor = 12 - ustrlen("`factor'")
    di as txt "{dup 13: }{c |}" _newline "{dup `padfactor': }`factor' {c |}"
    }

  * Set the factorflag if factor not missing
  if ("`factor'"!=".") {
    local factorflag = 1
    }

  * Print a breakline if previousfactor was not missing, and is different than
  * factor
  if ("`previousfactor'"!="." & "`previousfactor'"!="`factor'" & "`previousfactor'"!="0") {
    di as txt "{dup 13: }{c |}"
    }

  * Respaceify indepvar
  local indepvar = ustrregexra("`indepvar'","_QQQQQQ_"," ")
            
  * Produce abbreviated independent variable name
  local abbrevindepvar = abbrev("`indepvar'",12-`factorflag')
  local padaiv = 12 - ustrlen("`abbrevindepvar'") - `factorflag'
  
  * Format eqvtype
  local eqvtype = ustrlower("`eqvtype'")
  if ("`eqvtype'"=="delta") {
    local eqvtype = "Delta"
    local eqvtypeout = uchar(0916)
    }
   else {
    local eqvtypeout = uchar(0949)
    }
  
  * Adjust the format string for t1 and t2 if they are exceptionally large

  * Format for t1
  local f_t1 = "%6.2f"
  if (`t1'>=1000) {
    local f_t1 = "%6.1f"
    if (`t1'>=10000) {
      local f_t1 = "%6.0f"
      if (`t1'>=100000) {
        local f_t1 = "%6.0g"
        }
      }
    }

  * Format for t2
  local f_t2 = "%6.2f"
  if (`t2'>=1000) {
    local f_t2 = "%6.1f"
    if (`t2'>=10000) {
      local f_t2 = "%6.0f"
      if (`t2'>=100000) {
        local f_t2 = "%6.0g"
        }
      }
    }
    
  * Display the entry
  if (`upper' == `lower') {
    di as txt "{dup `padaiv': }`abbrevindepvar' {dup `factorflag': }{c |}  " as res %9.0g `coefficient' "  " %9.0g `stderr' "   " `f_t1' `t1' "   " %06.4f `p1' "  " `f_t2' `t2' "   " %06.4f `p2' "    " %~7s "`eqvtypeout'"  "     " %6.0g `lower'
 *   di as txt "{hline 13}{c +}{hline 81}"
    }
   else {
    di as txt "{dup `padaiv': }`abbrevindepvar' {dup `factorflag': }{c |}  " as res %9.0g `coefficient' "  " %9.0g `stderr' "   " `f_t1' `t1' "   " %06.4f `p1' "  " `f_t2' `t2' "   " %06.4f `p2' "    " %~7s "`eqvtypeout'" "   "    %6.0g -1*`lower' "   "   %6.0g `upper'
 *   di as txt "{hline 13}{c +}{hline 88}"
    }
  end

    
program define _relevanceregheader14

  args depvar factorflag alpha symmetric

  * Produce abbreviated dependent variable name
  local abbrevdepvar = abbrev("`depvar'",12)
  local padadv = 12 - ustrlen("`abbrevdepvar'")
  
  di as txt _newline "Regression relevance tests (" uchar(0945) " = " as res "0" `alpha' as txt ")"
  if (`symmetric' == 1) {
    di as txt "{hline 13}{c TT}{hline 67}"
    di as txt "{dup `padadv': }`abbrevdepvar' {c |}  eqvtype   eqvlevel       Ho+         Ho-           Conclude"
    di as txt "{hline 13}{c +}{hline 67}"
    }
   else {
    di as txt "{hline 13}{c TT}{hline 73}"
    di as txt "{dup `padadv': }`abbrevdepvar' {c |}  eqvtype   lower    upper       Ho+         Ho-           Conclude"
    di as txt "{hline 13}{c +}{hline 73}"
    }
  end

    
program define _relevanceregentry14

  args indepvar factor previousfactor eqvtype upper lower positivistdecision negativistdecision 

  local factorflag = 0
  
  * Print out the factor header if there is one, and it is not the same as the 
  * previousfactor
  if ("`factor'"!="." & "`factor'"!="`previousfactor'") {
    local padfactor = 12 - ustrlen("`factor'")
    di as txt "{dup 13: }{c |}" _newline "{dup `padfactor': }`factor' {c |}"
    }

  * Set the factorflag if factor not missing
  if ("`factor'"!=".") {
    local factorflag = 1
    }

  * Print a breakline if previousfactor was not missing, and is different than
  * factor
  if ("`previousfactor'"!="." & "`previousfactor'"!="`factor'" & "`previousfactor'"!="0") {
    di as txt "{dup 13: }{c |}"
    }
  
  * Respaceify indepvar
  local indepvar = ustrregexra("`indepvar'","_QQQQQQ_"," ")
            
  * Produce abbreviated independent variable name
  local abbrevindepvar = abbrev("`indepvar'",12-`factorflag')
  local padaiv = 12 - ustrlen("`abbrevindepvar'") - `factorflag'
  
  * Format eqvtype
  local eqvtype = ustrlower("`eqvtype'")
  if ("`eqvtype'"=="delta") {
    local eqvtype = "Delta"
    local eqvtypeout = uchar(0916)
    }
   else {
    local eqvtypeout = uchar(0949)
    }
        
  * Format Not Reject
  if ("`positivistdecision'" == "NotReject") {
    local positivistdecision = "Not Reject"
    }
  if ("`negativistdecision'" == "NotReject") {
    local negativistdecision = "Not Reject"
    }
  
  * Create conclusion
  if ("`positivistdecision'" == "Reject" & "`negativistdecision'" == "Not Reject") {
    local conclusion = "Relevant Difference"
    }
  if ("`positivistdecision'" == "Reject" & "`negativistdecision'" == "Reject") {
    local conclusion = "Trivial Difference"
    }
  if ("`positivistdecision'" == "Not Reject" & "`negativistdecision'" == "Not Reject") {
    local conclusion = "Inconclusive"
    }
  if ("`positivistdecision'" == "Not Reject" & "`negativistdecision'" == "Reject") {
    local conclusion = "Equivalence"
    }
  
  * Display the entry
  if (`upper' == `lower') {
    di as txt "{dup `padaiv': }`abbrevindepvar' {dup `factorflag': }{c |}  " as res %~7s "`eqvtypeout'" "    " %6.0g `lower' "    " %~10s "`positivistdecision'" "  " %~10s "`negativistdecision'" "   " %~19s "`conclusion'"
    }
   else {
    di as txt "{dup `padaiv': }`abbrevindepvar' {dup `factorflag': }{c |}  " as res %~7s "`eqvtypeout'" "  " %6.0g `lower' "   " %6.0g -1*`upper' "   " %~10s "`positivistdecision'" "  " %~10s "`negativistdecision'" "   " %~19s "`conclusion'"
    }
  end

    
program define _relevanceconclusion, rclass

  args positivistdecision negativistdecision 

  * Format Not Reject
  if ("`positivistdecision'" == "NotReject") {
    local positivistdecision = "Not Reject"
    }
  if ("`negativistdecision'" == "NotReject") {
    local negativistdecision = "Not Reject"
    }
  
  * Create conclusion
  if ("`positivistdecision'" == "Reject" & "`negativistdecision'" == "Not Reject") {
    local conclusion = "Relevant Difference"
    }
  if ("`positivistdecision'" == "Reject" & "`negativistdecision'" == "Reject") {
    local conclusion = "Trivial Difference"
    }
  if ("`positivistdecision'" == "Not Reject" & "`negativistdecision'" == "Not Reject") {
    local conclusion = "Inconclusive"
    }
  if ("`positivistdecision'" == "Not Reject" & "`negativistdecision'" == "Reject") {
    local conclusion = "Equivalence"
    }

  return local conclusion `conclusion' 
  end


* _vlformat14 returns a local containing suitably formatted variable names, 
* whether or not they are derived from names containing factor variable 
* operators, and a local containing the corresponding factor name, if any, for
* each variable, including the constant term, if any.
program define _vlformat14, rclass

  * varlist is obtained from the names associated with e(b) after calling 
  * regress and cleaning them of omitted vars.
  args varlist
  local N: word count `varlist'
  local factorlist = ""
  local variablelist = ""
  forvalues i = 1/`N' {
    local current: word `i' of `varlist'
    * Test whether current has factor variable operators if not "_cons"
    if ("`current'"!="_cons") {
      fvexpand `current'
      local fvtest = r(fvops)
      local tstest = r(tsops)
      * tostregress does not support variables with *both* factor operators 
      * *and* ts operators
      if ("`fvtest'"=="true" & "`tstest'"=="true") {
        noi: di as err "tostregress supports {help fvvarlist:factor operators} and {help tsvarlist:ts operators}, but not both on the same variable."
        exit 198
        }
      
      * If current does have factor variable operators, use _fvopname14 to get the
      * factor name, and factor value name for factorlist and variablelist
      if ("`fvtest'"=="true") {
        _fvopname14 `current'
        local variablelist = "`variablelist' " + r(factorvalue)
            
        * Get the current factorname.
        local factorlist = "`factorlist' " + r(factorname)
        }

       * Otherwise...
       else {
        * If current does have tsoperators, use _tsopname14 to get the
        * ts operator's canonical prefix, and variable name
        if ("`tstest'"=="true") {
          _tsopname14 `current'
          local variablelist = "`variablelist' " + r(tsprefix)

          * Get the current varname.
          local factorlist = "`factorlist' " + r(rootvar)
          }
      
         * Otherwise
         else {
          * Treat as a single continuous variable
          local factorlist = "`factorlist' " + "."
          local variablelist = "`variablelist' " + "`current'"
          }
        }
      }

     * But if current does equal "_cons"
     else {
      local factorlist = "`factorlist' " + "."
      local variablelist = "`variablelist' " + "_cons"
      }
    }
  
  return local factors `factorlist'
  return local variables `variablelist'
  end

* _fvopname14 returns a suitably formatted factor name, and a suitably 
* formatted factor value based on an input string s containing one or more 
* factor operators.
program define _fvopname14, rclass

  args s
  _nvars14 "`s'"
  local nvars = r(nvars)

  local cleaned = ustrregexra("`s'", "#", " ")
  local cleaned = ustrregexra("`cleaned'", "ibn\.", "")
*  local cleaned = ustrregexra("`cleaned'", "ib[0-9\s\/]+\.", "")
  local cleaned = ustrregexra("`cleaned'", "ib\(first\)\.", "")
  local cleaned = ustrregexra("`cleaned'", "ib\(last\)\.", "")
  local cleaned = ustrregexra("`cleaned'", "ib\(freq\)\.", "")
  local cleaned = ustrregexra("`cleaned'", "i\.", "")
  local cleaned = ustrregexra("`cleaned'", "b\.", "\.")
  local factorname = ""
  local factorvalue = ""

  forvalues i = 1/`nvars' {

    local current: word `i' of `cleaned'
    * Get the factor name and factor value, first for the case of a variable 
    * declared continuous
    if (usubstr("`current'",1,2)=="c.") {
      local factorname = "`factorname'" + ".#"
      local factorvalue = "`factorvalue'`current'#"
      }

    * Otherwise in the case of a factor variable
     else {
      _factorname14 `current'
      local FN = r(factorname)
      local factorname = "`factorname'"+"`FN'#"
    
      * Get the factor values
      _factorvalue14 `current'
      local FV = r(factorvalue)
      * use "_QQQQQQ_" as a placeholder for spaces in the FV
      local FV = ustrregexra("`FV'"," ","_QQQQQQ_")
      local factorvalue = "`factorvalue'"+"`FV'#"
      }
    }
  local collapsefactorname = ustrregexra("`factorname'", "\.", "")
  local collapsefactorname = ustrregexra("`collapsefactorname'", "#", "")
  if ("`collapsefactorname'"=="") {
    local factorname = "."
    }
  local fnlen = ustrlen("`factorname'")
  if (usubstr("`factorname'",`fnlen',1)=="#") {
    local factorname = usubstr("`factorname'",1,`fnlen'-1)
    }
  local fvlen = ustrlen("`factorvalue'")
  if (usubstr("`factorvalue'",`fvlen',1)=="#") {
    local factorvalue = usubstr("`factorvalue'",1,`fvlen'-1)
    }
  return local factorname `factorname'
  return local factorvalue `factorvalue'
  end

    
* _nvars14 returns the number of strings separated by "#" or "##"
program define _nvars14, rclass

  args s1

  local lens1 = ustrlen("`s1'")
  local loops = `lens1' - 1
  local count = 1
  local i = 2

  if (usubstr("`s1'",1,1) == "#" | usubstr("`s1'",`lens1',1) == "#") {
  noi: di as err "invalid input: cannot begin or end with '#'"
  exit 198
      }
  
  while `i' <= `loops' {
    if (usubstr("`s1'",`i',3) == "###") {
    noi: di as err "invalid input: cannot contain '###'"
    exit 198
          }
    if (usubstr("`s1'",`i',2) == "##") {
      local count = `count' + 1
      local i = `i' + 2
      }
     else {
      if (usubstr("`s1'",`i',1) == "#") {
        local count = `count' + 1
        }
      local i = `i' + 1
      }
    }
  return local nvars `count'
  end
    
    
* _factorname14 returns the factor name component of s    
program define _factorname14, rclass

  * s is expected to take the form FV.FN, where:
  * FV is a numeric value
  * FN is the name of a factor variable
  args s
    
  if (usubstr("`s'",1,2)== "c.") {
    return local factorname .
    }
   else {
    local period = ustrpos("`s'",".")
    local factorname = usubstr("`s'",`period'+1,.)
    return local factorname `factorname'
    }
  end

    
* _factorvalue14 returns the label corresponding to the factor value component of
* s, or simply returns the factor value if the factor has no value label, or if
* the specific value does not have an entry in the value label.
program define _factorvalue14, rclass

  * s is expected to take the form FV.FN, where:
  * FV is a numeric value
  * FN is the name of a factor variable
  args s

  _factorname14 "`s'"
  local factor = r(factorname)
  local period = ustrpos("`s'",".")
  local factorvalue = usubstr("`s'",1,`period'-1)
  * Strip out factor value bas value operator stuff
  local factorvalue = real(ustrregexra("`factorvalue'","[a-zA-Z]*([0-9]+)[a-zA-Z]*","$1"))
  local factorvaluelabel: value label `factor'
  if ("`factorvaluelabel'"!="") {
    local temp: label `factorvaluelabel' `factorvalue' 
    if ("`factorvalue'"=="`temp'") {
      return local factorvalue `factorvalue'
      }
     else {
      return local factorvalue `temp'
      }
    }
   else {
    return local factorvalue `factorvalue'
    }
  end


* _tsopname14 returns the canonical form of any tsoperators prefixing s, and 
* returns and empty string if s has no tsoperators prefixed.
program define _tsopname14, rclass

  * s is expected to take the form TSO.VAR, where:
  * TSO is one or more time series operators of the form [DFLS][DFLS0-9]*\.
  *    NOTE: the character in the second bracket, if not a digit must match the 
  *    character in the first bracket.
  * VAR may include factor variable operators.
  args s
    
  * Get the root variable underlying s, first just s if s contains no '.'
  * local tsflag indicates whether s contains ts operators
  local tsflag = 0
  if (ustrpos("`s'",".")==0) {
    local rootvar = "`s'"
    }
   * Otherwise, just the stuff following the final '.'
   else {
    qui: fvexpand `s'
    local tsflag = r(tsops)
    if ("`tsflag'" == "true") {
      local tsflag = 1
      }
     else {
      local tsflag = 0
      }
    local rootvar = ustrregexra("`s'",".*\.([A-Za-z_\\p{L}][A-Za-z0-9_\\p{L}]+)","$1")
    }
    
  * Locate and condense tsops
  if (`tsflag' == 1) {
    
    * Uppercase
    local s = ustrupper("`s'")

    * capture and condense difference prefix, begin with no D ts operator prefix
    local Dcap = ustrregexra("`s'","([\w\.]*\.)?([D][D0-9]*)\..*","$2")
    if ("`Dcap'"=="`s'") {
      local Dcap = ""
      }
     * Otherwise, condense Dcap
     else {
      local length = ustrlen("`Dcap'")
      local Dcount = 0
      local digitcount = 0
      local digitsum = 0
      local i = 1
      while `i' <= `length' {
        local character = usubstr("`Dcap'",`i',1)
        if ("`character'" == "D") { 
          local Dcount = `Dcount' + 1
          }
         else {
          * Check if 
          local nextcharacter = usubstr("`Dcap'",`i'+1,1)
          if ("`nextcharacter'" != "D") {
            local character = "`character'"+"`nextcharacter'"
            local i = `i' + 1
            }
          local digitcount = `digitcount' + 1
          local digitsum = `digitsum' + real("`character'")
          }
        local i = `i' + 1
        }
      local Ddigits = `Dcount' - `digitcount' + `digitsum'
      if (`Ddigits' == 1) {
        local Dcondensed = "D"
        }
       else {
        local Dcondensed = "D`Ddigits'"
        }
      }

      * capture and condense difference prefix, begin with no F ts operator prefix
    local Fcap = ustrregexra("`s'","([\w\.]*\.)?([F][F0-9]*)\..*","$2")
    if ("`Fcap'"=="`s'") {
      local Fcap = ""
      }
     * Otherwise, condense Fcap
     else {
      local length = ustrlen("`Fcap'")
      local Fcount = 0
      local digitcount = 0
      local digitsum = 0
      local i = 1
      while `i' <= `length' {
        local character = usubstr("`Fcap'",`i',1)
        if ("`character'" == "F") { 
          local Fcount = `Fcount' + 1
          }
         else {
          local nextcharacter = usubstr("`Fcap'",`i'+1,1)
          if ("`nextcharacter'" != "F") {
            local character = "`character'"+"`nextcharacter'"
            local i = `i' + 1
            }
          local digitcount = `digitcount' + 1
          local digitsum = `digitsum' + real("`character'")
          }
          local i = `i' + 1
        }
      local Fdigits = `Fcount' - `digitcount' + `digitsum'
      if (`Fdigits' == 1) {
        local Fcondensed = "F"
        }
       else {
        local Fcondensed = "F`Fdigits'"
        }
      }

    * capture and condense difference prefix, begin with no L ts operator prefix
    local Lcap = ustrregexra("`s'","([\w\.]*\.)?([L][L0-9]*)\..*","$2")
    if ("`Lcap'"=="`s'") {
      local Lcap = ""
      }
     * Otherwise, condense Lcap
     else {
      local length = ustrlen("`Lcap'")
      local Lcount = 0
      local digitcount = 0
      local digitsum = 0
      local i = 1
      while `i' <= `length' {
        local character = usubstr("`Lcap'",`i',1)
        if ("`character'" == "L") { 
          local Lcount = `Lcount' + 1
          }
         else {
          local nextcharacter = usubstr("`Lcap'",`i'+1,1)
          if ("`nextcharacter'" != "L") {
            local character = "`character'"+"`nextcharacter'"
            local i = `i' + 1
            }
          local digitcount = `digitcount' + 1
          local digitsum = `digitsum' + real("`character'")
          }
        local i = `i' + 1
        }
      local Ldigits = `Lcount' - `digitcount' + `digitsum'
      if (`Ldigits' == 1) {
        local Lcondensed = "L"
        }
       else {
        local Lcondensed = "L`Ldigits'"
        }
      }

  * capture and condense difference prefix, begin with no S ts operator prefix
  local Scap = ustrregexra("`s'","([\w\.]*\.)?([S][S0-9]*)\..*","$2")
  if ("`Scap'"=="`s'") {
    local Scap = ""
    }
   * Otherwise, condense Scap
   else {
    local length = ustrlen("`Scap'")
    local Scount = 0
    local digitcount = 0
    local digitsum = 0
    local i = 1
    while `i' <= `length' {
      local character = usubstr("`Scap'",`i',1)
      if ("`character'" == "S") { 
        local Scount = `Scount' + 1
        }
       else {
        local nextcharacter = usubstr("`Scap'",`i'+1,1)
        if ("`nextcharacter'" != "S") {
          local character = "`character'"+"`nextcharacter'"
          local i = `i' + 1
          }
        local digitcount = `digitcount' + 1
        local digitsum = `digitsum' + real("`character'")
        }
      local i = `i' + 1
      }
    local Sdigits = `Scount' - `digitcount' + `digitsum'
    if (`Sdigits' == 1) {
      local Scondensed = "S"
      }
     else {
      local Scondensed = "S`Sdigits'"
      }
    }

  }

  if ("`Dcondensed'" != "") {
    local Dcondensed = "`Dcondensed'."
    }
  if ("`Fcondensed'" != "") {
    local Fcondensed = "`Fcondensed'."
    }
  if ("`Lcondensed'" != "") {
    local Lcondensed = "`Lcondensed'."
    }
  if ("`Scondensed'" != "") {
    local Scondensed = "`Scondensed'."
    }
  local tscanonical = "`Lcondensed'`Fcondensed'`Dcondensed'`Scondensed'`rootvar'"
  local tsprefix = "`Lcondensed'`Fcondensed'`Dcondensed'`Scondensed'"
  local tsprefix = usubstr("`tsprefix'",1,ustrlen("`tsprefix'")-1)
  return local tscanonical `tscanonical'
  return local tsprefix `tsprefix'
  return local rootvar `rootvar'
  end
        
        
* _svy_opts14 takes an options macro, and returns (a) the options macro stripped
* of svy()-related options and suboptions, (b) a properly structured svy_prefix, 
* and (c) any svy-options
program define _svy_opts14, rclass

  args opts

  * Output macros
  local nonsvy_options = ""
  local svy_prefix = ""
  local svy_options = ""

  * Number of left and right parentheses
  local Nlp = 0
  local Nrp = 0

  * svy parenthesis open
  local svyparen = 0

  * Insure no double spaces 
    local opts = ustrregexra("`opts'", " +", " ")
    
    * Insure no space before left parenthesis
    local opts = ustrregexra("`opts'", " \(", "\(")

    * Insure space after left parenthesis
    local opts = ustrregexra("`opts'", "\(", "\( ")

    * Insure space before and after right parenthesis
    local opts = ustrregexra("`opts'", "\)", " \) ")

  * Reinsure no double spaces 
    local opts = ustrregexra("`opts'", " +", " ")    
    
  * Number of options
  local numoptions: word count `opts'
    
  * Manual while loop across all the options
  local i = 1
  while `i' <= `numoptions' {
    local wordi: word `i' of `opts'
    local i = `i' + 1
        
    * Check if svyparen opens
    if ("`wordi'"=="svy(") {
      local svyparen = 1
      local Nlp = `Nlp' + 1
      local svy_prefix = "svy"
      continue
      }
        
    * Check if svyparen is open
    if (`svyparen'==1) {
      * Left parenthesis?
      if (ustrpos("`wordi'","(")!=0) {
        local Nlp = `Nlp' + 1
        }
      * Right parenthesis?
      if (ustrpos("`wordi'",")")!=0) {
        local Nrp = `Nrp' + 1
        }
      * Close svyparen?
      if (`Nlp'==`Nrp') {
        local svyparen = 0
        *local svy_options = "`svy_options' `wordi'"
        }
       else {
        * Check for vcetype
        if (usubstr("`wordi'",1,4)=="jack" | usubstr("`wordi'",1,4)=="boot" | usubstr("`wordi'",1,4)=="line" | usubstr("`wordi'",1,3)=="brr" | usubstr("`wordi'",1,3)=="sdr") {
          local svy_prefix = "`svy_prefix' `wordi'"
          }
         * Otherwise if not vcetype, add to svy_options
         else {
          local svy_options = "`svy_options' `wordi'"
          }
        }
      }
     else {
      * Left parenthesis?
      if (ustrpos("`wordi'","(")!=0) {
        local Nlp = `Nlp' + 1
        }
      * Right parenthesis?
      if (ustrpos("`wordi'",")")!=0) {
        local Nrp = `Nrp' + 1
        }
      local nonsvy_options = "`nonsvy_options' `wordi'"
      }
    }

  local nonsvy_options = ustrregexra("`nonsvy_options'", "\( ", "\(")
  local nonsvy_options = ustrregexra("`nonsvy_options'", " \)", "\)")
  local svy_options = ustrregexra("`svy_options'", "\( ", "\(")
  local svy_options = ustrregexra("`svy_options'", " \)", "\)")
  local svy_command = ustrrtrim("`svy_prefix'")+", "+ustrrtrim(ustrltrim("`svy_options'"))+":"
    
  return local nonsvy_options `nonsvy_options'
  return local svy_command `svy_command'
  end
