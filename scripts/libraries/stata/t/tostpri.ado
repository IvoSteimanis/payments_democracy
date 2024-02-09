*! version 3.1.3  15may2021 by alexis dot dinno at pdx dot edu
*! perform two one-sided tests for mean proportion equivalence

* Syntax:  tostpri #obs1 #p1 #obs2 #p2 [, eqvtype(type) eqvlevel(#) 
*          uppereqvlevel(#) count yates ha alpha(#) relevance]


program define tostpri

  if int(_caller())<8 {
    di in r "tostpri- does not support this version of Stata." _newline
    di as txt "Requests for a v7 compatible version will be relatively easy to honor." 
    di as txt "Requests for a v6 compatible version may be less easy." 
    di as txt "Requests for a version compatible with versions of STATA earlier than v6 are "
    di as txt "untenable since I do not have access to the software." _newline 
    di as txt "All requests are welcome and will be considered."
    exit
  }
  if int(_caller())<14 {
    tostpri8 `0'
    }
   else { 
    tostpri14 `0'
    }
  end

program define tostpri8, rclass
  version 8.0, missing

  * a little parsing
  gettoken 1 0 : 0 , parse(" ,")
  gettoken 2 0 : 0 , parse(" ,")
  gettoken 3 0 : 0 , parse(" ,")
  gettoken 4 : 0 , parse(" ,")
  if "`4'"=="" | "`4'"=="," {
    local twosample = 0
    }
  if "`4'"!="" & "`4'"!="," {
    local twosample = 1
    gettoken 4 0 : 0 , parse(" ,")
    }

  syntax [, EQVType(string) EQVLevel(real 1) UPPEReqvlevel(real 0) Xname(string) /*
  */      Yname(string) Count YAtes ha Alpha(real 0.05) RELevance]

  quietly {

    * Validate eqvtype
    if lower("`eqvtype'") == "" {
      local eqvtype = "delta"
      }

    if !(lower("`eqvtype'") == "delta" | lower("`eqvtype'") == "epsilon") {
      noisily: di as err "option eqvtype() must be either delta or epsilon"
      exit 198
      }

    * Validate eqvlevel
    if (lower("`eqvtype'") == "delta") & (`eqvlevel' == 1 & `uppereqvlevel'==0) {
      local eqvlevel = 0.1
      }

    if (lower("`eqvtype'") == "epsilon") & (`eqvlevel' == 1 & `uppereqvlevel'==0) {
      local eqvlevel = 2
      }

    if (lower("`eqvtype'") == "delta" || lower("`eqvtype'") == "epsilon") & (`eqvlevel' <= 0 & `uppereqvlevel' != abs(`eqvlevel')) {
      noisily: di as err "option eqvlevel() incorrectly specified" _newline "the tolerance must be a positive real value"
      exit 198
      }

    if lower("`eqvtype'") == "delta" & (`eqvlevel' >= 1 | `uppereqvlevel' >= 1) {
      noisily: di as err "option eqvlevel() incorrectly specified" _newline "you are likely to find all proportions equivalent within an interval of plus or minus 1 or more"
      exit 198
      }

    * Validate uppereqvlevel
    if (`uppereqvlevel'<0) {
      noisily: di as err "option uppereqvlevel() must be a positive real value"
      exit 198
      }
 
    if (`uppereqvlevel'==0 | `uppereqvlevel' == abs(`eqvlevel')) {
      local upper = abs(`eqvlevel')
      local lower = abs(`eqvlevel')
      }

    if (`uppereqvlevel'>0) {
      local upper = abs(`uppereqvlevel')
      local lower = abs(`eqvlevel')
      }

    * Validate continuity correction option
    if ("`yates'" != "" & "`ha'" != "") {
      noisily di as err "continuity correction options must be either yates or ha, but not both"
      exit 198
      }
   
    if (("`yates'" != "" | "`ha'" != "") & `twosample'==0) {
      noisily di _newline as res "continuity correction options are not available for one-sample tests"
      local yates = ""
      local ha = "" 
      }

    local continuity = 0

    * Validate alpha
    if (`alpha' < 0 | `alpha' > 1) {
      noisily: di as err "option alpha() must be between 0 and 1 inclusive"
      exit 198
      }
   
*******************************************************************************
* The business starts here                                                    *
*******************************************************************************
    local PositivistConclusion = "Reject"
    local NegativistConclusion = "Reject"
    if "`relevance'" != "" {
      noi: di as txt "Relevance test of sample means"
      }
    if "`xname'" == "" {
      local xname ="x"
      }
    if "`yname'" == "" {
      local yname ="y"
      }

**********
* one-sample test of proportion equivalence
    if `twosample'==0 {
 
    * Validate and use count
    if "`count'"!="" {
      confirm integer number `1'
      confirm integer number `2'
      confirm number `3'
      local n1 = `1'
      if `2' <= `1' { 
        local m1 = `2'/`1'
        }
      local m2 = `3'
      if `m1' > 1 | `m1' < 0 {
        noisily di as err "`m1' not in [0,1]"
        exit 198
        }
      if `m2' > 1 | `m2' < 0 {
        noisily di as err "`m2' not in [0,1]"
        exit 198
        }
      }
    if "`count'"=="" {
      confirm integer number `1'
      confirm number `2'
      confirm number `3'
      local n1 = `1'
      local m1 = `2'
      local m2 = `3'
      if `m1' > 1 | `m1' < 0 {
        noisily di as err "`m1' not in [0,1]"
        exit 198
        }
      if `m2' > 1 | `m2' < 0 {
        noisily di as err "`m2' not in [0,1]"
        exit 198
        }
      }

     local level : di %3.2f (100*(1-`alpha'))
     if "`relevance'" != "" {
       noi: prtesti `n1' `m1' `m2' , level(`level')
       noi: di _newline
       }
     prtesti `n1' `m1' `m2' , level(`level')
     if (1-normal(abs( (r(P_1) - `m2')/sqrt(r(P_1)*(1-r(P_1))/(r(N_1))) ))) > (`alpha'/2) {
       local PositivistConclusion = "Fail to reject"
       }
     local n1 = r(N_1)
     local m1 = r(P_1)
     local se = sqrt(r(P_1)*(1-r(P_1))/(`n1'))
     local se_p = sqrt(`m2'*(1-`m2')/`n1')
     if lower("`eqvtype'") == "delta" {
       local z1 = (`upper' - (r(P_1) - `m2'))/`se_p'
       local z2 = ((r(P_1) - `m2')+`lower')/`se_p'
       }
     if lower("`eqvtype'") == "epsilon" {
       local z1 = `upper' - ( ((r(P_1) - `m2'))/`se_p' )
       local z2 = ( ((r(P_1) - `m2'))/`se_p' ) + `lower'
       }
     local p1 = 1 - normal(`z1')
     local p2 = 1 - normal(`z2')
     if (`p1' > `alpha' | `p2' > `alpha') {
       local NegativistConclusion = "Fail to reject"
       }
     local xname = trim(substr(trim(`"`xname'"'),1,12))
     local c1 = 53 - length(`"`xname'"')
     noisily: di
     noisily: di as text `"One-sample test of proportion equivalence"' /*
     */ _col(`c1') as res abbrev(`"`xname'"', 12) as text _col(53) /*
     */ `": Number of obs = "' as res %8.0g `n1'
     local alpha : di %6.4f `alpha'
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
     noisily: _prtest1 `"`xname'"' `n1' `m1' `se' `alpha' 
     if lower("`eqvtype'") == "delta" {
       noisily: di in smcl as text "{hline 13}{c +}{hline 64}"
       if (`upper' == `lower') {
         noisily: _prtest2 "Delta-diff" `n1' `upper'-(`m1'-`m2') `se' `alpha'
         noisily: _prtest2 "diff+Delta" `n1' (`m1'-`m2')+`lower' `se' `alpha'
         }
       if (`upper' != `lower') {
         noisily: _prtest2 "Du-diff" `n1' `upper'-(`m1'-`m2') `se' `alpha'
         noisily: _prtest2 "diff-Dl" `n1' (`m1'-`m2')+`lower' `se' `alpha'
         }
       }
     noisily: di in smcl as text "{hline 13}{c BT}{hline 64}"
     if lower("`eqvtype'") == "delta" {
       noisily: di as text "      diff = prop(" as res "`xname'" as text ") - " as res `m2' as text " = " as res `m1' - `m2'    
       if (`upper' == `lower') {
         noisily: di as text "     Delta = " as res %-8.4f `lower' as res "Delta " as text "expressed in same units as prop(" as res "`xname'" as text")"
         }
       if (`upper' != `lower') {
         noisily: di as text "Delta (Dl) = " as res %-8.4f -1*`lower' as res "Dl " as text "expressed in same units as prop(" as res "`xname'" as text ")"
         noisily: di as text "Delta (Du) = " as res %-8.4f `upper' as res "Du " as text "expressed in same units as prop(" as res "`xname'" as text ")"
         }
       local criticalvalue = `se_p'*invnormal(1-`alpha')
       if (`upper' == `lower' & `lower' <= `criticalvalue') {
         noisily: di _newline as res "Impossible to reject any Ho if Delta <= z-crit*s.e. ( " %-6.4g `criticalvalue' "). See{help tostpr##mineqvlevel: help tostpr}."
         }
       if (`upper' != `lower' & `lower' <= `criticalvalue') {
         noisily: di _newline as res "Impossible to reject any Ho if |Dl| <= t-crit*s.e. ( " %-6.4g `criticalvalue' "). See{help tostpr##mineqvlevel: help tostpr}."
         }
       if (`upper' != `lower' & `upper' <= `criticalvalue') {
         noisily: di _newline as res "Impossible to reject any Ho if Du <= t-crit*s.e. ( " %-6.4g `criticalvalue' "). See{help tostpr##mineqvlevel: help tostpr}."
         }
       if (`upper' == `lower') {
         noisily: di _newline as text "Ho: |diff| >= Delta:" _newline 
         noisily: di as text "        z1 = " as res %-8.4g `z1' as text _col(38) "z2 = " as res %-8.4g `z2' _newline
         noisily: di as text "   Ho1: Delta-diff <= 0" _col(33) "Ho2: diff+Delta <= 0"
         noisily: di as text "   Ha1: Delta-diff > 0"  _col(33) "Ha2: diff+Delta > 0"
         noisily: di as text "   Pr(Z > z1) = " as res %6.4f `p1' _col(32) as text " Pr(Z > z2) = " as res %6.4f `p2'
         }
       if (`upper' != `lower') {
         noisily: di _newline as text "Ho: diff <= Dl, or diff >= Du:" _newline 
         noisily: di as text "        z1 = " as res %-8.4g `z1' as text _col(38) "z2 = " as res %-8.4g `z2' _newline
         noisily: di as text "   Ho1: Du-diff <= 0" _col(33) "Ho2: diff-Dl <= 0"
         noisily: di as text "   Ha1: Du-diff > 0"  _col(33) "Ha2: diff-Dl > 0"
         noisily: di as text "   Pr(Z > z1) = " as res %6.4f `p1' _col(32) as text " Pr(Z > z2) = " as res %6.4f `p2'
         }
       }
     if lower("`eqvtype'") == "epsilon" {
       noisily: di as text "        diff = prop(" as res "`xname'" as text ") - " as res `m2' as text " = " as res `m1' - `m2'     
       if (`upper' == `lower') {
         noisily: di as text "     epsilon = " as res %-8.4f `lower' as res "epsilon " as text "expressed in units of the z distribution"
         }
       if (`upper' != `lower') {
         noisily: di as text "epsilon (el) = " as res %-8.4f -1*`lower' as text " " as res "el" as text " expressed in units of the z distribution"
         noisily: di as text "epsilon (eu) = " as res %-8.4f `upper' as text " " as res "eu" as text " expressed in units of the z distribution"
         }
       local criticalvalue = invnormal(1-`alpha')
       if (`upper' == `lower' & `lower' <= `criticalvalue') {
         noisily: di _newline as res "Impossible to reject any Ho if epsilon <= z-crit (" %-5.3f `criticalvalue' "). See{help tostpr##mineqvlevel: help tostpr}."
         }
       if (`upper' != `lower' & `lower' <= `criticalvalue') {
         noisily: di _newline as res "Impossible to reject any Ho if |el| <= z-crit (" %-5.3f `criticalvalue' "). See{help tostpr##mineqvlevel: help tostpr}."
         }
       if (`upper' != `lower' & `upper' <= `criticalvalue') {
         noisily: di _newline as res "Impossible to reject any Ho if eu <= z-crit (" %-5.3f `criticalvalue' "). See{help tostpr##mineqvlevel: help tostpr}."
         }
       if (`upper' == `lower') {
         noisily: di _newline as text "Ho: |Z| >= epsilon:" _newline 
         noisily: di as text "        z1 = " as res %-8.4g `z1' as text _col(38) "z2 = " as res %-8.4g `z2' _newline
         noisily: di as text "   Ho1: epsilon-Z <= 0" _col(33) "Ho2: Z+epsilon <= 0"
         noisily: di as text "   Ha1: epsilon-Z > 0"  _col(33) "Ha2: Z+epsilon > 0"
         noisily: di as text "   Pr(Z > z1) = " as res %6.4f `p1' _col(32) as text " Pr(Z > z2) = " as res %6.4f `p2'
         }
       if (`upper' != `lower') {
         noisily: di _newline as text "Ho: Z <= el, or Z >= eu:" _newline 
         noisily: di as text "        z1 = " as res %-8.4g `z1' as text _col(38) "z2 = " as res %-8.4g `z2' _newline
         noisily: di as text "   Ho1: eu-Z <= 0" _col(33) "Ho2: Z-el <= 0"
         noisily: di as text "   Ha1: eu-Z > 0"  _col(33) "Ha2: Z-el > 0"
         noisily: di as text "   Pr(Z > z1) = " as res %6.4f `p1' _col(32) as text " Pr(Z > z2) = " as res %6.4f `p2'
         }
       }
     }


**********
* two-sample z test for mean equivalence
    if `twosample'==1 {
      if "`count'"!="" {
        confirm integer number `1'
        confirm integer number `2'
        confirm integer number `3'
        confirm integer number `4'
        local n1 = `1'
        local n2 = `3'
        local m1 = `2'/`1'
        local m2 = `4'/`3'
	* Error if either proportion is not in [0,1]
        if `m1' > 1 | `m1' < 0 {
          noisily di as err %9.0g `m1' " not in [0,1]"
          exit 198
          }
        if `m2' > 1 | `m2' < 0 {
          noisily di as err %9.0g `m2' " not in [0,1]"
          exit 198
          }
        }
      if "`count'"=="" {
        confirm integer number `1'
        confirm number `2'
        confirm integer number `3'
        confirm number `4'
        local n1 = `1'
        local m1 = `2'
        local n2 = `3'
        local m2 = `4'
        if `m1' > 1 | `m1' < 0 {
          noisily di as err "`m1' not in [0,1]"
          exit 198
          }
        if `m2' > 1 | `m2' < 0 {
          noisily di as err "`m2' not in [0,1]"
          exit 198
          }
        }

      local level: di %3.2f (100*(1-`alpha'))
      if "`relevance'" != "" {
        noi: prtesti `1' `2' `3' `4' , level(`level') `count'
        noi: di _newline
        }
      quietly: prtesti `1' `2' `3' `4' , level(`level') `count'
      local n1 = r(N_1)
      local m1 = r(P_1)
      local se1= sqrt(r(P_1)*(1-r(P_1))/(`n1'))
      local n2 = r(N_2)
      local m2 = r(P_2)
      local se2= sqrt(r(P_2)*(1-r(P_2))/(`n2'))
      local N  = `n1'+`n2'
      local p  = (r(P_1)*r(N_1) +  r(P_2)*r(N_2))/(`N')
      local se_p = sqrt( `p'*(1-`p')*((1/`n1') + (1/`n2')) )
      if ("`yates'" != "") {
        local continuity = 0.5*((1/`n1') + (1/`n2'))
        }
      if ("`ha'" != "") {
        local continuity = 1/(2*min(`n1',`n2'))
        local se_p = sqrt( (((`m1'/`n1') * (1-(`m1'/`n1')))/(`n1'-1)) + (((`m2'/`n2') * (1-(`m2'/`n2')))/(`n2'-1)))
        }
      if (1-normal(abs(((r(P_1) - r(P_2))-`continuity')/`se_p'))) > (`alpha'/2) {
        local PositivistConclusion = "Fail to reject"
        }
      if lower("`eqvtype'") == "delta" {
        local z1 = (`upper' - (r(P_1) - r(P_2))+`continuity')/`se_p'
        local z2 = ((r(P_1) - r(P_2))+`lower'-`continuity')/`se_p'
        }
      if lower("`eqvtype'") == "epsilon" {
        local z1 = `upper' - ( ((r(P_1) - r(P_2))+`continuity')/`se_p' )
        local z2 = ( ((r(P_1) - r(P_2))-`continuity')/`se_p' ) + `lower'
        }
      local p1 = 1 - normal(`z1')
      local p2 = 1 - normal(`z2')
      if (`p1' > `alpha' | `p2' > `alpha') {
        local NegativistConclusion = "Fail to reject"
        }
      noisily: di
      local xname = trim(substr(trim(`"`xname'"'),1,12))
      local yname = trim(substr(trim(`"`yname'"'),1,12))
      local c1 = 53 - length(`"`xname'"')
      local c2 = 53 - length(`"`yname'"')
      noisily: di in gr "Two-sample test of proportion equivalence" /*
      */ _col(`c1') in ye abbrev(`"`xname'"', 12) in gr _col(53) /*
      */ `": Number of obs = "' in ye %8.0g `n1'
      noisily: di _col(`c2') as res (abbrev(`"`yname'"', 12)) as text _col(53) /*
      */ `": Number of obs = "' as res %8.0g `n2'			
      noisily: _prtest1 `xname' `n1' `m1' `se1' `alpha' 
      noisily: _prtest2 `yname' `n2' `m2' `se2' `alpha' 
      noisily: di in smcl as text "{hline 13}{c +}{hline 64}"
      if lower("`eqvtype'") == "delta" {
        if (`upper' == `lower') {
          noisily: _prtest2 "Delta-diff" `n1' `upper'-(`m1'-`m2') `se_p' `alpha'
          noisily: _prtest2 "diff+Delta" `n1' (`m1'-`m2')+`lower' `se_p' `alpha'
          }
        if (`upper' != `lower') {
          noisily: _prtest2 "Du-diff" `n1' `upper'-(`m1'-`m2') `se_p' `alpha'
          noisily: _prtest2 "diff-Dl" `n1' (`m1'-`m2')+`lower' `se_p' `alpha'
          }
        }
      if lower("`eqvtype'") == "epsilon" {
        noisily: _prtest2 "      diff" `N' `m1'-`m2' `se_p' `alpha'
        }
      noisily: di in smcl as text "{hline 13}{c BT}{hline 64}"
      if lower("`eqvtype'") == "delta" {
        noisily: di as text "      diff = prop(" as res "`xname'" as text ") - prop(" as res "`yname'" as text ") = " as res `m1' - `m2'    
        if (`upper' == `lower') {
          noisily: di as text "     Delta = " as res %-8.4f `lower' as res "Delta " as text "expressed in same units as prop(" as res "`xname'" as text")"
          }
        if (`upper' != `lower') {
          noisily: di as text "Delta (Dl) = " as res %-8.4f -1*`lower' as res "Dl " as text "expressed in same units as prop(" as res "`xname'" as text ")"
          noisily: di as text "Delta (Du) = " as res %-8.4f `upper' as res "Du " as text "expressed in same units as prop(" as res "`xname'" as text ")"
          }
        local criticalvalue = `se_p'*invnormal(`alpha'/100)
        if `eqvlevel' <= `criticalvalue' {
          noisily: di _newline as res "Impossible to reject any Ho if Delta <= z-crit*s.e. ( " %-6.4g `criticalvalue' "). See{help tostpr##mineqvlevel: help tostpr}." _newline
          }
        if (`upper' == `lower') {
          noisily: di _newline as text "Ho: |diff| >= Delta:" _newline 
          if ("`yates'" != "") {
            noisily: di as text "Using the Yates continuity correction" _newline
            } 
          if ("`ha'" != "") {
            noisily: di as text "Using the Hauck-Anderson continuity correction" _newline
            } 
          noisily: di as text "        z1 = " as res %-8.4g `z1' as text _col(38) "z2 = " as res %-8.4g `z2' _newline
          noisily: di as text "   Ho1: Delta-diff <= 0" _col(33) "Ho2: diff+Delta <= 0"
          noisily: di as text "   Ha1: Delta-diff > 0"  _col(33) "Ha2: diff+Delta > 0"   
          noisily: di as text "   Pr(Z > z1) = " as res %6.4f `p1' _col(32) as text " Pr(Z > z2) = " as res %6.4f `p2'
          }
        if (`upper' != `lower') {
          noisily: di _newline as text "Ho: diff <= Dl, or diff >= Du:" _newline 
          if ("`yates'" != "") {
            noisily: di as text "Using the Yates continuity correction" _newline
            } 
          if ("`ha'" != "") {
            noisily: di as text "Using the Hauck-Anderson continuity correction" _newline
            } 
          noisily: di as text "        z1 = " as res %-8.4g `z1' as text _col(38) "z2 = " as res %-8.4g `z2' _newline
          noisily: di as text "   Ho1: Du-diff <= 0" _col(33) "Ho2: diff-Dl <= 0"
          noisily: di as text "   Ha1: Du-diff > 0"  _col(33) "Ha2: diff-Dl > 0"
          noisily: di as text "   Pr(Z > z1) = " as res %6.4f `p1' _col(32) as text " Pr(Z > z2) = " as res %6.4f `p2'
          }
        }
      if lower("`eqvtype'") == "epsilon" {
        noisily: di as text "        diff = prop(" as res "`xname'" as text ") - prop(" as res "`yname'" as text ") = " as res `m1' - `m2'     
        if (`upper' == `lower') {
          noisily: di as text "     epsilon = " as res %-8.4f `lower' as res "epsilon " as text "expressed in units of the z distribution"
          }
        if (`upper' != `lower') {
          noisily: di as text "epsilon (el) = " as res %-8.4f -1*`lower' as text " " as res "el" as text " expressed in units of the z distribution"
          noisily: di as text "epsilon (eu) = " as res %-8.4f `upper' as text " " as res "eu" as text " expressed in units of the z distribution"
          }
        local criticalvalue = invnormal(1-`alpha')
        if (`upper' == `lower' & `lower' <= `criticalvalue') {
          noisily: di _newline as res "Impossible to reject any Ho if epsilon <= z-crit (" %-5.3f `criticalvalue' "). See{help tostpr##mineqvlevel: help tostpr}."
          }
        if (`upper' != `lower' & `lower' <= `criticalvalue') {
          noisily: di _newline as res "Impossible to reject any Ho if |el| <= z-crit (" %-5.3f `criticalvalue' "). See{help tostpr##mineqvlevel: help tostpr}."
          }
        if (`upper' != `lower' & `upper' <= `criticalvalue') {
          noisily: di _newline as res "Impossible to reject any Ho if eu <= z-crit (" %-5.3f `criticalvalue' "). See{help tostpr##mineqvlevel: help tostpr}."
          }
        if (`upper' == `lower') {
          noisily: di _newline as text "Ho: Z >= epsilon:" _newline 
          if ("`yates'" != "") {
            noisily: di as text "Using the Yates continuity correction" _newline
            } 
          if ("`ha'" != "") {
            noisily: di as text "Using the Hauck-Anderson continuity correction" _newline
            } 
          noisily: di as text "        z1 = " as res %-8.4g `z1' as text _col(38) "z2 = " as res %-8.4g `z2' _newline
          noisily: di as text "   Ho1: epsilon-Z <= 0" _col(33) "Ho2: Z+epsilon <= 0"
          noisily: di as text "   Ha1: epsilon-Z > 0"  _col(33) "Ha2: Z+epsilon > 0"
          noisily: di as text "   Pr(Z > z1) = " as res %6.4f `p1' _col(32) as text " Pr(Z > z2) = " as res %6.4f `p2'
          }
        if (`upper' != `lower') {
          noisily: di _newline as text "Ho: Z <= el, or Z >= eu:" _newline 
          if ("`yates'" != "") {
            noisily: di as text "Using the Yates continuity correction" _newline
            } 
          if ("`ha'" != "") {
            noisily: di as text "Using the Hauck-Anderson continuity correction" _newline
            } 
          noisily: di as text "        z1 = " as res %-8.4g `z1' as text _col(38) "z2 = " as res %-8.4g `z2' _newline
          noisily: di as text "   Ho1: eu-Z <= 0" _col(33) "Ho2: Z-el <= 0"
          noisily: di as text "   Ha1: eu-Z > 0"  _col(33) "Ha2: Z-el > 0"
          noisily: di as text "   Pr(Z > z1) = " as res %6.4f `p1' _col(32) as text " Pr(Z > z2) = " as res %6.4f `p2'
          }
        }
      }
   
    * Output combined tests results if relevance test is requested
    if "`relevance'" != "" {
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

    * Format Delta or epsilon to remove trailing zeros
    if (mod(`lower'*1000, 1) == 0.0) {
      local lower: di %6.3f `lower'
      }
    if (mod(`lower'*100, 1) == 0.0) {
      local lower: di %5.2f `lower'
      }
    if (mod(`lower'*10, 1) == 0.0) {
      local lower: di %4.1f `lower'
      }
    if (mod(`lower', 1) == 0.0) {
      local lower: di %4.0f `lower'
      }
    if (`upper' != `lower') {
      if (mod(`upper'*1000, 1) == 0.0) {
        local upper: di %6.3f `upper'
        }
      if (mod(`upper'*100, 1) == 0.0) {
        local upper: di %5.2f `upper'
        }
      if (mod(`upper'*10, 1) == 0.0) {
        local upper: di %4.1f `upper'
         }
      if (mod(`upper', 1) == 0.0) {
        local upper: di %4.0f `upper'
         }
      }
    if (`upper' == `lower') {
      if lower("`eqvtype'") == "delta" {
        if (`lower' < 1) {
          noi: di _newline _newline as txt "Relevance test conclusion for alpha = " as res "0" `alpha' as txt ", and Delta = " as res "0" `lower' as txt ":"
          }
         else {
          noi: di _newline _newline as txt "Relevance test conclusion for alpha = " as res "0" `alpha' as txt ", and Delta = " as res `lower' as txt ":"
          }
        }
      if lower("`eqvtype'") == "epsilon" {
        noi: di _newline _newline as txt "Relevance test conclusion for alpha = " as res "0" `alpha' as txt ", and epsilon = " as res `lower' as txt ":"
        }
      }
    if (`upper' != `lower') {
      if lower("`eqvtype'") == "delta" {
        if (`lower' < 1 & `upper' < 1) {
          noi: di _newline _newline as txt "Relevance test conclusion for alpha = " as res "0" `alpha' as txt ", Delta (Dl) = " as res "-0" `lower' as txt ", and Delta (Du) = " as res "0" `upper' as txt ":"
          }
        if (`lower' >= 1 & `upper' < 1) {
          noi: di _newline _newline as txt "Relevance test conclusion for alpha = " as res "0" `alpha' as txt ", Delta (Dl) = " as res -1*`lower' as txt  ", and Delta (Du) = " as res "0" `upper' as txt ":"
          }
        if (`lower' < 1 & `upper' >= 1) {
          noi: di _newline _newline as txt "Relevance test conclusion for alpha = " as res "0" `alpha' as txt ", Delta (Dl) = " as res "-0" `lower' as txt  ", and Delta (Du) = " as res `upper' as txt ":"
          }
         if (`lower' >= 1 & `upper' >= 1) {
          noi: di _newline _newline as txt "Relevance test conclusion for alpha = " as res "0" `alpha' as txt ", Delta (Dl) = " as res -1*`lower' as txt  ", and Delta (Du) = " as res `upper' as txt ":"
          }
        }
      if lower("`eqvtype'") == "epsilon" {
        noi: di _newline _newline as txt "Relevance test conclusion for alpha = " as res "0" `alpha' as txt ", epsilon (el) = " as res -1*`lower' as txt  ", and epsilon (eu) = " as res `upper' as txt ":"
        }
      }
    noi: di as txt "  Ho test for difference:  " as res "`PositivistConclusion'" 
    noi: di as txt "  Ho test for equivalence: " as res "`NegativistConclusion'" 
    if "`PositivistConclusion'" == "Reject" & "`NegativistConclusion'" == "Reject" {
      local RelevanceTestConclusion = "Trivial difference (overpowered test)"
      }
    if "`PositivistConclusion'" == "Reject" & "`NegativistConclusion'" == "Fail to reject" {
      local RelevanceTestConclusion = "Relevant difference"
      }
    if "`PositivistConclusion'" == "Fail to reject" & "`NegativistConclusion'" == "Reject" {
      local RelevanceTestConclusion = "Equivalence"
      }
    if "`PositivistConclusion'" == "Fail to reject" & "`NegativistConclusion'" == "Fail to reject" {
      local RelevanceTestConclusion = "Indeterminate (underpowered test)"
      }
    noi: di _newline as txt "Conclusion from combined tests: " as res "`RelevanceTestConclusion'" 
    }   
 

*******************************************************************************
* Program end. Close up shop and return things.                               *
*******************************************************************************

  }

  if ("`relevance'" != "") {
    return local relevance = "`RelevanceTestConclusion'"
    }
  if (`upper' == `lower') {
    if "`eqvtype'" == "delta" {
      return scalar Delta   = `eqvlevel'
      }
    if "`eqvtype'" == "epsilon" {
      return scalar epsilon   = `eqvlevel'
      }
    }
  if (`upper' != `lower') {
    if "`eqvtype'" == "delta" {
      return scalar Dl   = `lower'
      return scalar Du   = `upper'
      }
    if "`eqvtype'" == "epsilon" {
      return scalar el   = `lower' 
      return scalar eu   = `upper'
      }
    }
  return scalar N_1     = `n1'
  return scalar P_1     = `m1'
  if `twosample'==1 {
    return scalar N_2   = `n2'
    return scalar P_2   = `m2'
    }
  return scalar z1      = `z1'
  return scalar z2      = `z2'
  end

  
program define tostpri14, rclass
  version 14.0, missing

  * a little parsing
  gettoken 1 0 : 0 , parse(" ,")
  gettoken 2 0 : 0 , parse(" ,")
  gettoken 3 0 : 0 , parse(" ,")
  gettoken 4 : 0 , parse(" ,")
  if "`4'"=="" | "`4'"=="," {
    local twosample = 0
    }
  if "`4'"!="" & "`4'"!="," {
    local twosample = 1
    gettoken 4 0 : 0 , parse(" ,")
    }

  syntax [, EQVType(string) EQVLevel(real 1) UPPEReqvlevel(real 0) Xname(string) /*
  */      Yname(string) Count YAtes ha Alpha(real 0.05) RELevance]

  quietly {

    * Create theta, delta, and epsilon constants
    local theta = uchar(0952)
    local delta = uchar(0916)
    local epsilon = uchar(0949)

    * Validate eqvtype
    if lower("`eqvtype'") == "" {
      local eqvtype = "delta"
      }

    if !(lower("`eqvtype'") == "delta" | lower("`eqvtype'") == "epsilon") {
      noisily: di as err "option eqvtype() must be either delta or epsilon"
      exit 198
      }

    * Validate eqvlevel
    if (lower("`eqvtype'") == "delta") & (`eqvlevel' == 1 & `uppereqvlevel'==0) {
      local eqvlevel = 0.1
      }

    if (lower("`eqvtype'") == "epsilon") & (`eqvlevel' == 1 & `uppereqvlevel'==0) {
      local eqvlevel = 2
      }

    if (lower("`eqvtype'") == "delta" || lower("`eqvtype'") == "epsilon") & (`eqvlevel' <= 0 & `uppereqvlevel' != abs(`eqvlevel')) {
      noisily: di as err "option eqvlevel() incorrectly specified" _newline "the tolerance must be a positive real value"
      exit 198
      }

    if lower("`eqvtype'") == "delta" & (`eqvlevel' >= 1 | `uppereqvlevel' >= 1) {
      noisily: di as err "option eqvlevel() incorrectly specified" _newline "you are likely to find all proportions equivalent within an interval of plus or minus 1 or more"
      exit 198
      }

    * Validate uppereqvlevel
    if (`uppereqvlevel'<0) {
      noisily: di as err "option uppereqvlevel() must be a positive real value"
      exit 198
      }
 
    if (`uppereqvlevel'==0 | `uppereqvlevel' == abs(`eqvlevel')) {
      local upper = abs(`eqvlevel')
      local lower = abs(`eqvlevel')
      }

    if (`uppereqvlevel'>0) {
      local upper = abs(`uppereqvlevel')
      local lower = abs(`eqvlevel')
      }

    * Validate continuity correction option
    if ("`yates'" != "" & "`ha'" != "") {
      noisily di as err "continuity correction options must be either yates or ha, but not both"
      exit 198
      }
   
    if (("`yates'" != "" | "`ha'" != "") & `twosample'==0) {
      noisily di _newline as res "continuity correction options are not available for one-sample tests"
      local yates = ""
      local ha = "" 
      }

    local continuity = 0

    * Validate alpha
    if (`alpha' < 0 | `alpha' > 1) {
      noisily: di as err "option alpha() must be between 0 and 1 inclusive"
      exit 198
      }
   
*******************************************************************************
* The business starts here                                                    *
*******************************************************************************
    local PositivistConclusion = "Reject"
    local NegativistConclusion = "Reject"
    if "`relevance'" != "" {
      noi: di as txt "Relevance test of sample means"
      }
    if "`xname'" == "" {
      local xname ="x"
      }
    if "`yname'" == "" {
      local yname ="y"
      }

**********
* one-sample test of proportion equivalence
    if `twosample'==0 {
 
    * Validate and use count
    if "`count'"!="" {
      confirm integer number `1'
      confirm integer number `2'
      confirm number `3'
      local n1 = `1'
      if `2' <= `1' { 
        local m1 = `2'/`1'
        }
      local m2 = `3'
      if `m1' > 1 | `m1' < 0 {
        noisily di as err "`m1' not in [0,1]"
        exit 198
        }
      if `m2' > 1 | `m2' < 0 {
        noisily di as err "`m2' not in [0,1]"
        exit 198
        }
      }
    if "`count'"=="" {
      confirm integer number `1'
      confirm number `2'
      confirm number `3'
      local n1 = `1'
      local m1 = `2'
      local m2 = `3'
      if `m1' > 1 | `m1' < 0 {
        noisily di as err "`m1' not in [0,1]"
        exit 198
        }
      if `m2' > 1 | `m2' < 0 {
        noisily di as err "`m2' not in [0,1]"
        exit 198
        }
      }

     local level : di %3.2f (100*(1-`alpha'))
     if "`relevance'" != "" {
       noi: prtesti `n1' `m1' `m2' , level(`level')
       noi: di _newline
       }
     prtesti `n1' `m1' `m2' , level(`level')
     if (1-normal(abs( (r(P_1) - `m2')/sqrt(r(P_1)*(1-r(P_1))/(r(N_1))) ))) > (`alpha'/2) {
       local PositivistConclusion = "Fail to reject"
       }
     local n1 = r(N_1)
     local m1 = r(P_1)
     local se = sqrt(r(P_1)*(1-r(P_1))/(`n1'))
     local se_p = sqrt(`m2'*(1-`m2')/`n1')
     if lower("`eqvtype'") == "delta" {
       local z1 = (`upper' - (r(P_1) - `m2'))/`se_p'
       local z2 = ((r(P_1) - `m2')+`lower')/`se_p'
       }
     if lower("`eqvtype'") == "epsilon" {
       local z1 = `upper' - ( ((r(P_1) - `m2'))/`se_p' )
       local z2 = ( ((r(P_1) - `m2'))/`se_p' ) + `lower'
       }
     local p1 = 1 - normal(`z1')
     local p2 = 1 - normal(`z2')
     if (`p1' > `alpha' | `p2' > `alpha') {
       local NegativistConclusion = "Fail to reject"
       }
     local xname = trim(substr(trim(`"`xname'"'),1,12))
     local c1 = 53 - length(`"`xname'"')
     noisily: di
     noisily: di as text `"One-sample test of proportion equivalence"' /*
     */ _col(`c1') as res abbrev(`"`xname'"', 12) as text _col(53) /*
     */ `": Number of obs = "' as res %8.0g `n1'
     local alpha : di %6.4f `alpha'
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
     noisily: _prtest1 `"`xname'"' `n1' `m1' `se' `alpha' 
     if lower("`eqvtype'") == "delta" {
       noisily: di in smcl as text "{hline 13}{c +}{hline 64}"
       if (`upper' == `lower') {
         noisily: _prtest2 "`delta'-`theta'" `n1' `upper'-(`m1'-`m2') `se' `alpha'
         noisily: _prtest2 "`theta'+`delta'" `n1' (`m1'-`m2')+`lower' `se' `alpha'
         }
       if (`upper' != `lower') {
         noisily: _prtest2 "`delta'u-`theta'" `n1' `upper'-(`m1'-`m2') `se' `alpha'
         noisily: _prtest2 "`theta'-`delta'l" `n1' (`m1'-`m2')+`lower' `se' `alpha'
         }
       }
     noisily: di in smcl as text "{hline 13}{c BT}{hline 64}"
     if lower("`eqvtype'") == "delta" {
       noisily: di as text "         `theta' = prop(" as res "`xname'" as text ") - " as res `m2' as text " = " as res `m1' - `m2'    
       if (`upper' == `lower') {
         noisily: di as text "         `delta' = " as res %-8.4f `lower' as res "`delta' " as text "expressed in same units as prop(" as res "`xname'" as text")"
         }
       if (`upper' != `lower') {
         noisily: di as text "        `delta'l = " as res %-8.4f -1*`lower' as res "`delta'l " as text "expressed in same units as prop(" as res "`xname'" as text ")"
         noisily: di as text "        `delta'u = " as res %-8.4f `upper' as res "`delta'u " as text "expressed in same units as prop(" as res "`xname'" as text ")"
         }
       local criticalvalue = `se_p'*invnormal(1-`alpha')
       if (`upper' == `lower' & `lower' <= `criticalvalue') {
         noisily: di _newline as res "Impossible to reject any Ho if `delta' <= z-crit*s.e. ( " %-6.4g `criticalvalue' "). See{help tostpr##mineqvlevel: help tostpr}."
         }
       if (`upper' != `lower' & `lower' <= `criticalvalue') {
         noisily: di _newline as res "Impossible to reject any Ho if |`delta'l| <= t-crit*s.e. ( " %-6.4g `criticalvalue' "). See{help tostpr##mineqvlevel: help tostpr}."
         }
       if (`upper' != `lower' & `upper' <= `criticalvalue') {
         noisily: di _newline as res "Impossible to reject any Ho if `delta'u <= t-crit*s.e. ( " %-6.4g `criticalvalue' "). See{help tostpr##mineqvlevel: help tostpr}."
         }
       if (`upper' == `lower') {
         noisily: di _newline as text "Ho: |`theta'| >= `delta':" _newline 
         noisily: di as text "        z1 = " as res %-8.4g `z1' as text _col(38) "z2 = " as res %-8.4g `z2' _newline
         noisily: di as text "   Ho1: `delta'-`theta' <= 0" _col(33) "Ho2: `theta'+`delta' <= 0"
         noisily: di as text "   Ha1: `delta'-`theta' > 0"  _col(33) "Ha2: `theta'+`delta' > 0"
         noisily: di as text "   Pr(Z > z1) = " as res %6.4f `p1' _col(32) as text " Pr(Z > z2) = " as res %6.4f `p2'
         }
       if (`upper' != `lower') {
         noisily: di _newline as text "Ho: `theta' <= `delta'l, or `theta' >= `delta'u:" _newline 
         noisily: di as text "        z1 = " as res %-8.4g `z1' as text _col(38) "z2 = " as res %-8.4g `z2' _newline
         noisily: di as text "   Ho1: `delta'u-`theta' <= 0" _col(33) "Ho2: `theta'-`delta'l <= 0"
         noisily: di as text "   Ha1: `delta'u-`theta' > 0"  _col(33) "Ha2: `theta'-`delta'l > 0"
         noisily: di as text "   Pr(Z > z1) = " as res %6.4f `p1' _col(32) as text " Pr(Z > z2) = " as res %6.4f `p2'
         }
       }
     if lower("`eqvtype'") == "epsilon" {
       noisily: di as text "         `theta' = prop(" as res "`xname'" as text ") - " as res `m2' as text " = " as res `m1' - `m2'     
       if (`upper' == `lower') {
         noisily: di as text "         `epsilon' = " as res %-8.4f `lower' as res "`epsilon' " as text "expressed in units of the z distribution"
         }
       if (`upper' != `lower') {
         noisily: di as text "        `epsilon'l = " as res %-8.4f -1*`lower' as text " " as res "`epsilon'l" as text " expressed in units of the z distribution"
         noisily: di as text "        `epsilon'u = " as res %-8.4f `upper' as text " " as res "`epsilon'u" as text " expressed in units of the z distribution"
         }
       local criticalvalue = invnormal(1-`alpha')
       if (`upper' == `lower' & `lower' <= `criticalvalue') {
         noisily: di _newline as res "Impossible to reject any Ho if `epsilon' <= z-crit (" %-5.3f `criticalvalue' "). See{help tostpr##mineqvlevel: help tostpr}."
         }
       if (`upper' != `lower' & `lower' <= `criticalvalue') {
         noisily: di _newline as res "Impossible to reject any Ho if |`epsilon'l| <= z-crit (" %-5.3f `criticalvalue' "). See{help tostpr##mineqvlevel: help tostpr}."
         }
       if (`upper' != `lower' & `upper' <= `criticalvalue') {
         noisily: di _newline as res "Impossible to reject any Ho if `epsilon'u <= z-crit (" %-5.3f `criticalvalue' "). See{help tostpr##mineqvlevel: help tostpr}."
         }
       if (`upper' == `lower') {
         noisily: di _newline as text "Ho: |Z| >= `epsilon':" _newline 
         noisily: di as text "        z1 = " as res %-8.4g `z1' as text _col(38) "z2 = " as res %-8.4g `z2' _newline
         noisily: di as text "   Ho1: `epsilon'-Z <= 0" _col(33) "Ho2: Z+`epsilon' <= 0"
         noisily: di as text "   Ha1: `epsilon'-Z > 0"  _col(33) "Ha2: Z+`epsilon' > 0"
         noisily: di as text "   Pr(Z > z1) = " as res %6.4f `p1' _col(32) as text " Pr(Z > z2) = " as res %6.4f `p2'
         }
       if (`upper' != `lower') {
         noisily: di _newline as text "Ho: Z <= `epsilon'l, or Z >= `epsilon'u:" _newline 
         noisily: di as text "        z1 = " as res %-8.4g `z1' as text _col(38) "z2 = " as res %-8.4g `z2' _newline
         noisily: di as text "   Ho1: `epsilon'u-Z <= 0" _col(33) "Ho2: Z-`epsilon'l <= 0"
         noisily: di as text "   Ha1: `epsilon'u-Z > 0"  _col(33) "Ha2: Z-`epsilon'l > 0"
         noisily: di as text "   Pr(Z > z1) = " as res %6.4f `p1' _col(32) as text " Pr(Z > z2) = " as res %6.4f `p2'
         }
       }
     }


**********
* two-sample z test for mean equivalence
    if `twosample'==1 {
      if "`count'"!="" {
        confirm integer number `1'
        confirm integer number `2'
        confirm integer number `3'
        confirm integer number `4'
        local n1 = `1'
        local n2 = `3'
        local m1 = `2'/`1'
        local m2 = `4'/`3'
	* Error if either proportion is not in [0,1]
        if `m1' > 1 | `m1' < 0 {
          noisily di as err %9.0g `m1' " not in [0,1]"
          exit 198
          }
        if `m2' > 1 | `m2' < 0 {
          noisily di as err %9.0g `m2' " not in [0,1]"
          exit 198
          }
        }
      if "`count'"=="" {
        confirm integer number `1'
        confirm number `2'
        confirm integer number `3'
        confirm number `4'
        local n1 = `1'
        local m1 = `2'
        local n2 = `3'
        local m2 = `4'
        if `m1' > 1 | `m1' < 0 {
          noisily di as err "`m1' not in [0,1]"
          exit 198
          }
        if `m2' > 1 | `m2' < 0 {
          noisily di as err "`m2' not in [0,1]"
          exit 198
          }
        }

      local level: di %3.2f (100*(1-`alpha'))
      if "`relevance'" != "" {
        noi: prtesti `1' `2' `3' `4' , level(`level') `count'
        noi: di _newline
        }
      quietly: prtesti `1' `2' `3' `4' , level(`level') `count'
      local n1 = r(N_1)
      local m1 = r(P_1)
      local se1= sqrt(r(P_1)*(1-r(P_1))/(`n1'))
      local n2 = r(N_2)
      local m2 = r(P_2)
      local se2= sqrt(r(P_2)*(1-r(P_2))/(`n2'))
      local N  = `n1'+`n2'
      local p  = (r(P_1)*r(N_1) +  r(P_2)*r(N_2))/(`N')
      local se_p = sqrt( `p'*(1-`p')*((1/`n1') + (1/`n2')) )
      if ("`yates'" != "") {
        local continuity = 0.5*((1/`n1') + (1/`n2'))
        }
      if ("`ha'" != "") {
        local continuity = 1/(2*min(`n1',`n2'))
        local se_p = sqrt( (((`m1'/`n1') * (1-(`m1'/`n1')))/(`n1'-1)) + (((`m2'/`n2') * (1-(`m2'/`n2')))/(`n2'-1)))
        }
      if (1-normal(abs(((r(P_1) - r(P_2))-`continuity')/`se_p'))) > (`alpha'/2) {
        local PositivistConclusion = "Fail to reject"
        }
      if lower("`eqvtype'") == "delta" {
        local z1 = (`upper' - (r(P_1) - r(P_2))+`continuity')/`se_p'
        local z2 = ((r(P_1) - r(P_2))+`lower'-`continuity')/`se_p'
        }
      if lower("`eqvtype'") == "epsilon" {
        local z1 = `upper' - ( ((r(P_1) - r(P_2))+`continuity')/`se_p' )
        local z2 = ( ((r(P_1) - r(P_2))-`continuity')/`se_p' ) + `lower'
        }
      local p1 = 1 - normal(`z1')
      local p2 = 1 - normal(`z2')
      if (`p1' > `alpha' | `p2' > `alpha') {
        local NegativistConclusion = "Fail to reject"
        }
      noisily: di
      local xname = trim(substr(trim(`"`xname'"'),1,12))
      local yname = trim(substr(trim(`"`yname'"'),1,12))
      local c1 = 53 - length(`"`xname'"')
      local c2 = 53 - length(`"`yname'"')
      noisily: di in gr "Two-sample test of proportion equivalence" /*
      */ _col(`c1') in ye abbrev(`"`xname'"', 12) in gr _col(53) /*
      */ `": Number of obs = "' in ye %8.0g `n1'
      noisily: di _col(`c2') as res (abbrev(`"`yname'"', 12)) as text _col(53) /*
      */ `": Number of obs = "' as res %8.0g `n2'			
      noisily: _prtest1 `xname' `n1' `m1' `se1' `alpha' 
      noisily: _prtest2 `yname' `n2' `m2' `se2' `alpha' 
      noisily: di in smcl as text "{hline 13}{c +}{hline 64}"
      if lower("`eqvtype'") == "delta" {
        if (`upper' == `lower') {
          noisily: _prtest2 "`delta'-`theta'" `n1' `upper'-(`m1'-`m2') `se_p' `alpha'
          noisily: _prtest2 "`theta'+`delta'" `n1' (`m1'-`m2')+`lower' `se_p' `alpha'
          }
        if (`upper' != `lower') {
          noisily: _prtest2 "`delta'u-`theta'" `n1' `upper'-(`m1'-`m2') `se_p' `alpha'
          noisily: _prtest2 "`theta'-`delta'l" `n1' (`m1'-`m2')+`lower' `se_p' `alpha'
          }
        }
      if lower("`eqvtype'") == "epsilon" {
        noisily: _prtest2 "      `theta'" `N' `m1'-`m2' `se_p' `alpha'
        }
      noisily: di in smcl as text "{hline 13}{c BT}{hline 64}"
      if lower("`eqvtype'") == "delta" {
        noisily: di as text "         `theta' = prop(" as res "`xname'" as text ") - prop(" as res "`yname'" as text ") = " as res `m1' - `m2'    
        if (`upper' == `lower') {
          noisily: di as text "         `delta' = " as res %-8.4f `lower' as res "`delta' " as text "expressed in same units as prop(" as res "`xname'" as text")"
          }
        if (`upper' != `lower') {
          noisily: di as text "        `delta'l = " as res %-8.4f -1*`lower' as res "`delta'l " as text "expressed in same units as prop(" as res "`xname'" as text ")"
          noisily: di as text "        `delta'u = " as res %-8.4f `upper' as res "`delta'u " as text "expressed in same units as prop(" as res "`xname'" as text ")"
          }
        local criticalvalue = `se_p'*invnormal(`alpha'/100)
        if `eqvlevel' <= `criticalvalue' {
          noisily: di _newline as res "Impossible to reject any Ho if `delta' <= z-crit*s.e. ( " %-6.4g `criticalvalue' "). See{help tostpr##mineqvlevel: help tostpr}." _newline
          }
        if (`upper' == `lower') {
          noisily: di _newline as text "Ho: |`theta'| >= `delta':" _newline 
          if ("`yates'" != "") {
            noisily: di as text "Using the Yates continuity correction" _newline
            } 
          if ("`ha'" != "") {
            noisily: di as text "Using the Hauck-Anderson continuity correction" _newline
            } 
          noisily: di as text "        z1 = " as res %-8.4g `z1' as text _col(38) "z2 = " as res %-8.4g `z2' _newline
          noisily: di as text "   Ho1: `delta'-`theta' <= 0" _col(33) "Ho2: `theta'+`delta' <= 0"
          noisily: di as text "   Ha1: `delta'-`theta' > 0"  _col(33) "Ha2: `theta'+`delta' > 0"   
          noisily: di as text "   Pr(Z > z1) = " as res %6.4f `p1' _col(32) as text " Pr(Z > z2) = " as res %6.4f `p2'
          }
        if (`upper' != `lower') {
          noisily: di _newline as text "Ho: `theta' <= `delta'l, or `theta' >= `delta'u:" _newline 
          if ("`yates'" != "") {
            noisily: di as text "Using the Yates continuity correction" _newline
            } 
          if ("`ha'" != "") {
            noisily: di as text "Using the Hauck-Anderson continuity correction" _newline
            } 
          noisily: di as text "        z1 = " as res %-8.4g `z1' as text _col(38) "z2 = " as res %-8.4g `z2' _newline
          noisily: di as text "   Ho1: `delta'u-`theta' <= 0" _col(33) "Ho2: `theta'-`delta'l <= 0"
          noisily: di as text "   Ha1: `delta'u-`theta' > 0"  _col(33) "Ha2: `theta'-`delta'l > 0"
          noisily: di as text "   Pr(Z > z1) = " as res %6.4f `p1' _col(32) as text " Pr(Z > z2) = " as res %6.4f `p2'
          }
        }
      if lower("`eqvtype'") == "epsilon" {
        noisily: di as text "         `theta' = prop(" as res "`xname'" as text ") - prop(" as res "`yname'" as text ") = " as res `m1' - `m2'     
        if (`upper' == `lower') {
          noisily: di as text "         `epsilon' = " as res %-8.4f `lower' as res "`epsilon' " as text "expressed in units of the z distribution"
          }
        if (`upper' != `lower') {
          noisily: di as text "        `epsilon'l = " as res %-8.4f -1*`lower' as text " " as res "`epsilon'l" as text " expressed in units of the z distribution"
          noisily: di as text "        `epsilon'u = " as res %-8.4f `upper' as text " " as res "`epsilon'u" as text " expressed in units of the z distribution"
          }
        local criticalvalue = invnormal(1-`alpha')
        if (`upper' == `lower' & `lower' <= `criticalvalue') {
          noisily: di _newline as res "Impossible to reject any Ho if `epsilon' <= z-crit (" %-5.3f `criticalvalue' "). See{help tostpr##mineqvlevel: help tostpr}."
          }
        if (`upper' != `lower' & `lower' <= `criticalvalue') {
          noisily: di _newline as res "Impossible to reject any Ho if |`epsilon'l| <= z-crit (" %-5.3f `criticalvalue' "). See{help tostpr##mineqvlevel: help tostpr}."
          }
        if (`upper' != `lower' & `upper' <= `criticalvalue') {
          noisily: di _newline as res "Impossible to reject any Ho if `epsilon'u <= z-crit (" %-5.3f `criticalvalue' "). See{help tostpr##mineqvlevel: help tostpr}."
          }
        if (`upper' == `lower') {
          noisily: di _newline as text "Ho: Z >= `epsilon':" _newline 
          if ("`yates'" != "") {
            noisily: di as text "Using the Yates continuity correction" _newline
            } 
          if ("`ha'" != "") {
            noisily: di as text "Using the Hauck-Anderson continuity correction" _newline
            } 
          noisily: di as text "        z1 = " as res %-8.4g `z1' as text _col(38) "z2 = " as res %-8.4g `z2' _newline
          noisily: di as text "   Ho1: `epsilon'-Z <= 0" _col(33) "Ho2: Z+`epsilon' <= 0"
          noisily: di as text "   Ha1: `epsilon'-Z > 0"  _col(33) "Ha2: Z+`epsilon' > 0"
          noisily: di as text "   Pr(Z > z1) = " as res %6.4f `p1' _col(32) as text " Pr(Z > z2) = " as res %6.4f `p2'
          }
        if (`upper' != `lower') {
          noisily: di _newline as text "Ho: Z <= `epsilon'l, or Z >= `epsilon'u:" _newline 
          if ("`yates'" != "") {
            noisily: di as text "Using the Yates continuity correction" _newline
            } 
          if ("`ha'" != "") {
            noisily: di as text "Using the Hauck-Anderson continuity correction" _newline
            } 
          noisily: di as text "        z1 = " as res %-8.4g `z1' as text _col(38) "z2 = " as res %-8.4g `z2' _newline
          noisily: di as text "   Ho1: `epsilon'u-Z <= 0" _col(33) "Ho2: Z-`epsilon'l <= 0"
          noisily: di as text "   Ha1: `epsilon'u-Z > 0"  _col(33) "Ha2: Z-`epsilon'l > 0"
          noisily: di as text "   Pr(Z > z1) = " as res %6.4f `p1' _col(32) as text " Pr(Z > z2) = " as res %6.4f `p2'
          }
        }
      }
   
    * Output combined tests results if relevance test is requested
    if "`relevance'" != "" {
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

    * Format Delta or epsilon to remove trailing zeros
    if (mod(`lower'*1000, 1) == 0.0) {
      local lower: di %6.3f `lower'
      }
    if (mod(`lower'*100, 1) == 0.0) {
      local lower: di %5.2f `lower'
      }
    if (mod(`lower'*10, 1) == 0.0) {
      local lower: di %4.1f `lower'
      }
    if (mod(`lower', 1) == 0.0) {
      local lower: di %4.0f `lower'
      }
    if (`upper' != `lower') {
      if (mod(`upper'*1000, 1) == 0.0) {
        local upper: di %6.3f `upper'
        }
      if (mod(`upper'*100, 1) == 0.0) {
        local upper: di %5.2f `upper'
        }
      if (mod(`upper'*10, 1) == 0.0) {
        local upper: di %4.1f `upper'
         }
      if (mod(`upper', 1) == 0.0) {
        local upper: di %4.0f `upper'
         }
      }
    if (`upper' == `lower') {
      if lower("`eqvtype'") == "delta" {
        if (`lower' < 1) {
          noi: di _newline _newline as txt "Relevance test conclusion for= " as res "0" `alpha' as txt ", and `delta' = " as res "0" `lower' as txt ":"
          }
         else {
          noi: di _newline _newline as txt "Relevance test conclusion for= " as res "0" `alpha' as txt ", and `delta' = " as res `lower' as txt ":"
          }
        }
      if lower("`eqvtype'") == "epsilon" {
        noi: di _newline _newline as txt "Relevance test conclusion for= " as res "0" `alpha' as txt ", and `epsilon' = " as res `lower' as txt ":"
        }
      }
    if (`upper' != `lower') {
      if lower("`eqvtype'") == "delta" {
        if (`lower' < 1 & `upper' < 1) {
          noi: di _newline _newline as txt "Relevance test conclusion for= " as res "0" `alpha' as txt ", `delta'l = " as res "-0" `lower' as txt ", and `delta'u = " as res "0" `upper' as txt ":"
          }
        if (`lower' >= 1 & `upper' < 1) {
          noi: di _newline _newline as txt "Relevance test conclusion for= " as res "0" `alpha' as txt ", `delta'l = " as res -1*`lower' as txt  ", and `delta'u = " as res "0" `upper' as txt ":"
          }
        if (`lower' < 1 & `upper' >= 1) {
          noi: di _newline _newline as txt "Relevance test conclusion for= " as res "0" `alpha' as txt ", `delta'l = " as res "-0" `lower' as txt  ", and `delta'u = " as res `upper' as txt ":"
          }
         if (`lower' >= 1 & `upper' >= 1) {
          noi: di _newline _newline as txt "Relevance test conclusion for= " as res "0" `alpha' as txt ", `delta'l = " as res -1*`lower' as txt  ", and `delta'u = " as res `upper' as txt ":"
          }
        }
      if lower("`eqvtype'") == "epsilon" {
        noi: di _newline _newline as txt "Relevance test conclusion for= " as res "0" `alpha' as txt ", `epsilon'l = " as res -1*`lower' as txt  ", and `epsilon'u = " as res `upper' as txt ":"
        }
      }
    noi: di as txt "  Ho test for difference:  " as res "`PositivistConclusion'" 
    noi: di as txt "  Ho test for equivalence: " as res "`NegativistConclusion'" 
    if "`PositivistConclusion'" == "Reject" & "`NegativistConclusion'" == "Reject" {
      local RelevanceTestConclusion = "Trivial difference (overpowered test)"
      }
    if "`PositivistConclusion'" == "Reject" & "`NegativistConclusion'" == "Fail to reject" {
      local RelevanceTestConclusion = "Relevant difference"
      }
    if "`PositivistConclusion'" == "Fail to reject" & "`NegativistConclusion'" == "Reject" {
      local RelevanceTestConclusion = "Equivalence"
      }
    if "`PositivistConclusion'" == "Fail to reject" & "`NegativistConclusion'" == "Fail to reject" {
      local RelevanceTestConclusion = "Indeterminate (underpowered test)"
      }
    noi: di _newline as txt "Conclusion from combined tests: " as res "`RelevanceTestConclusion'" 
    }   
 

*******************************************************************************
* Program end. Close up shop and return things.                               *
*******************************************************************************

  }

  if ("`relevance'" != "") {
    return local relevance = "`RelevanceTestConclusion'"
    }
  if (`upper' == `lower') {
    if "`eqvtype'" == "delta" {
      return scalar Delta   = `eqvlevel'
      }
    if "`eqvtype'" == "epsilon" {
      return scalar epsilon   = `eqvlevel'
      }
    }
  if (`upper' != `lower') {
    if "`eqvtype'" == "delta" {
      return scalar Dl   = `lower'
      return scalar Du   = `upper'
      }
    if "`eqvtype'" == "epsilon" {
      return scalar el   = `lower' 
      return scalar eu   = `upper'
      }
    }
  return scalar N_1     = `n1'
  return scalar P_1     = `m1'
  if `twosample'==1 {
    return scalar N_2   = `n2'
    return scalar P_2   = `m2'
    }
  return scalar z1      = `z1'
  return scalar z2      = `z2'
  end

program define _prtest1
  local name = abbrev(`"`1'"', 12)
  local n "`2'"
  local mean "`3'"
  local se "`4'"
  local alpha "`5'"
  local show = "`6'" 
  if `n' == 1 | `n' >= . {
    local se = .
    }
  local beg = 13 - length(`"`name'"')
  if "`show'" != "" {
    local z z
  	 local zp P>|Z| 
    }
  local level : di %3.2f (100*(1-`alpha'))
  if (mod(`level'*10, 1) == 0.0) {
    local level: di %3.1f `level'
    }
  if (mod(`level', 1) == 0.0) {
    local level: di %2.0f `level'
    }
  local cil `=string(`level')'
  local cil `=length("`cil'")'
  noisily: di in smcl as text "{hline 13}{c TT}{hline 64}"
  noisily: di in smcl as text "    Variable {c |}" /*
  */ _col(22) "Mean" _col(29) /*
  */ "Std. Err." _col(44) "`z'" _col(49) /*
  */ "`zp'" _col(`=61-`cil'') `"[`=strsubdp("`level'")'% Conf. Interval]"'
  noisily: di in smcl as text "{hline 13}{c +}{hline 64}"
  local vval = (100-(100-`level')/2)/100
  noisily: di in smcl as text _col(`beg') `"`name'"' /*
  */ as text _col(14) "{c |}" as res /*
  */ _col(17) %9.0g  `mean'   /*
  */ _col(28) %9.0g  `se'     /*
  */ _col(58) %9.0g  `mean'-invnorm(`vval')*`se'   /*
  */ _col(70) %9.0g  `mean'+invnorm(`vval')*`se'
  end


program define _prtest2
  local name = abbrev(`"`1'"', 12)
  local n "`2'"
  local mean "`3'"
  local se "`4'"
  if `n' == 1 | `n' == . {
    local se = .
    }
  local alpha "`5'"
  local level : di %3.2f (100*(1-`alpha'))
  if (mod(`level'*10, 1) == 0.0) {
    local level: di %3.1f `level'
    }
  if (mod(`level', 1) == 0.0) {
    local level: di %2.0f `level'
    }
  local vval = (100 - (100-`level')/2)/100
  noisily: di in smcl as text %12s `"`name'"' " {c |}" as res /*
  */ _col(17) %9.0g  `mean'   /* 
  */ _col(28) %9.0g  `se'     /*
  */ _col(58) %9.0g  `mean'-invnorm(`vval')/*
  */ *`se'   /*
  */ _col(70) %9.0g  `mean'+invnorm(`vval')*`se'
  end
  
