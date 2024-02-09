*! version 3.1.3  15may2021 by alexis dot dinno at pdx dot edu
*! perform two one-sided t tests for mean equivalence

********************************************************************************
* Syntax:  tostt varname [=exp] [if exp] [in range] [, eqvtype(type) 
*          eqvlevel(#) uppereqvlevel(#) by(groupname) unpaired unequal welch 
*          alpha(#) relevance]

program define tostt

  if int(_caller())<8 {
    di in r "tostt- does not support this version of Stata." _newline
    di as txt "Requests for a v7 compatible version will be relatively easy to honor." 
    di as txt "Requests for a v6 compatible version may be less easy." 
    di as txt "Requests for a version compatible with versions of STATA earlier than v6 are "
    di as txt "untenable since I do not have access to the software." _newline 
    di as txt "All requests are welcome and will be considered."
    exit
  }
  if int(_caller())<14 {
    tostt8 `0'
    }
   else { 
    tostt14 `0'
    } 
end

program define tostt8, rclass byable(recall)
  version 8.0, missing

  /* turn "==" into "=" if needed before calling -syntax- */
  gettoken vn rest : 0, parse(" =")
  gettoken eq rest : rest, parse(" =")
  if "`eq'" == "==" {
    local 0 `vn' = `rest'
    }

********************************************************************************
  syntax varname [=/exp] [if] [in] [, EQVType(string) EQVLevel(real 1) /*
*/      UPPEReqvlevel(real 0) BY(varname) UNPaired UNEqual Welch /*
*/      Alpha(real 0.05) RELevance ]

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
    local eqvlevel = 1
    }

  if (lower("`eqvtype'") == "epsilon") & (`eqvlevel' == 1 & `uppereqvlevel'==0) {
    local eqvlevel = 2
    }

  if (lower("`eqvtype'") == "delta" || lower("`eqvtype'") == "epsilon") & (`eqvlevel' <= 0 & `uppereqvlevel' != abs(`eqvlevel')) {
    noisily: di as err "option eqvlevel() incorrectly specified" _newline "the tolerance must be a positive real value"
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

  * Validate unequal variances against Welch
  if "`welch'" != "" & ("`unpaired'" != "" || "`by'" != "") & "`unequal'" == "" {
    noisily: di _newline as res "welch option specified, proceeding by assuming unequal variances"
    local unequal = "unequal"
    } 
  if "`welch'" != "" & "`by'" == "" & "`unpaired'" == "" & "`unequal'" == "" {
    noisily: di as err "welch option invalid for one-sample or paired tests"
    exit 198
    } 

  * Invalidate options specified with one-sample test
  capture confirm number `exp'
  if _rc == 0 {
    if "`welch'" != "" {
      noisily: di as err "welch option invalid for one-sample tests"
      exit 198
      } 
    if "`unequal'" != "" {
      noisily: di as err "unequal option invalid for one-sample tests"
      exit 198
      } 
    if "`unpaired'" != "" {
      noisily: di as err "unpaired option invalid for one-sample tests"
      exit 198
      } 
    if "`by'" != "" {
      noisily di as err "may not combine = and option by()"
      exit 198
      }

   }


  * Validate alpha
  if (`alpha' < 0 | `alpha' > 1) {
    noisily: di as err "option alpha() must be between 0 and 1 inclusive"
    exit 198
    }
  local level : di %3.2f (100*(1-`alpha'))
 

*******************************************************************************
* The business starts here                                                    *
*******************************************************************************

**********
* conduct the positivist t test of difference
  local PositivistConclusion = "Reject"
  local NegativistConclusion = "Reject"
  if "`exp'" != "" & "`by'" == "" {
    local eexp = "= `exp'"
    }
  if "`relevance'" == "" {
    quietly: ttest `varlist' `eexp' `if' `in', `unpaired' `unequal' `welch' by("`by'") level(`level')
    }
   else {
    noi: di as txt "Relevance test of sample means"
    noi: ttest `varlist' `eexp' `if' `in', `unpaired' `unequal' `welch' by("`by'") level(`level')
    noi: di _newline
    }

  capture confirm number `exp'
  if _rc != 0 {
    local n1 = r(N_1)
    local m1 = r(mu_1)
    local s1 = r(sd_1)
    local n2 = r(N_2)
    local m2 = r(mu_2)
    local s2 = r(sd_2)
    local n = `n1'+`n2'
    local s = r(se)
    local df = r(df_t)
    if ttail(r(df_t),abs(((r(mu_1) - r(mu_2))/r(se)))) > (`alpha'/2) {
      local PositivistConclusion = "Fail to reject"
      }
    if lower("`eqvtype'") == "delta" {
      local t1 = (`upper' - (r(mu_1) - r(mu_2)))/r(se)
      local t2 = ((r(mu_1) - r(mu_2))+`lower')/r(se)
      }
    if lower("`eqvtype'") == "epsilon" {
      local t1 = `upper' - ( ( (r(mu_1) - r(mu_2)))/r(se) )
      local t2 = ( ((r(mu_1) - r(mu_2)))/r(se) ) + `lower'
      }
    local p1 = ttail(`df',`t1')
    local p2 = ttail(`df',`t2')
    if (`p1' > `alpha' | `p2' > `alpha') {
      local NegativistConclusion = "Fail to reject"
      }
    if "`by'" != "" {
      sum `by'
      local min = r(min)
      local max = r(max)
      }
    }


**********
* one-sample t test for mean equivalence
  if _rc == 0 {
    if ttail(r(df_t),abs(((r(mu_1) - `exp')/r(se)))) > (`alpha'/2) {
      local PositivistConclusion = "Fail to reject"
      }
    local n1 = r(N_1)
    local m1 = r(mu_1)
    local s1 = r(sd_1)
    local s = r(se)
    local n = r(N_1)
    local df = r(df_t)
    if lower("`eqvtype'") == "delta" {
      local t1 = (`upper' - (r(mu_1) - `exp'))/r(se)
      local t2 = ((r(mu_1) - `exp')+`lower')/r(se)
      }
    if lower("`eqvtype'") == "epsilon" {
      local t1 = `upper' - ( ((r(mu_1) - `exp'))/r(se) )
      local t2 = ( ((r(mu_1) - `exp'))/r(se) ) + `lower'
      }
    local p1 = ttail(`df',`t1')
    local p2 = ttail(`df',`t2')
    if (`p1' > `alpha' | `p2' > `alpha') {
      local NegativistConclusion = "Fail to reject"
      }
    noisily: di as text _newline "One-sample t test for mean equivalence"
    if (mod(`level'*10, 1) == 0.0) {
      local level: di %3.1f `level'
      }
    if (mod(`level', 1) == 0.0) {
      local level: di %3.0f `level'
      }
    noisily: _ttest header `level' `varlist'
    noisily: _ttest table `level' `varlist' `n1' `m1' `s1'
    if lower("`eqvtype'") == "delta" {
      noisily: _ttest divline
      if (`upper' == `lower') {
        noisily: _ttest dtable `level' "D-diff" `n' `upper'-(`m1'-`exp') `s' `df'
        noisily: _ttest dtable `level' "diff+D" `n' (`m1'-`exp')+`lower' `s' `df'
        }
      if (`upper' != `lower') {
        noisily: _ttest dtable `level' "Du-diff" `n' `upper'-(`m1'-`exp') `s' `df'
        noisily: _ttest dtable `level' "diff-Dl" `n' (`m1'-`exp')+`lower' `s' `df'
        }
      }
    noisily: _ttest botline
    if lower("`eqvtype'") == "delta" {
      noisily: di as text "      diff = mean(" as res "`varlist'" as text ") - " as res `exp'
      if (`upper' == `lower') {
        noisily: di as text " Delta (D) = " as res %-8.4f `lower' as res "Delta " as text "expressed in same units as " as res "`varlist'"
        }
      if (`upper' != `lower') {
        noisily: di as text "Delta (Dl) = " as res %-8.4f -1*`lower' as res "Dl " as text "expressed in same units as " as res "`varlist'"
        noisily: di as text "Delta (Du) = " as res %-8.4f `upper' as res "Du " as text "expressed in same units as " as res "`varlist'"
        }
      local criticalvalue = r(se)*invttail(`df',`alpha')
      if (`upper' == `lower' & `lower' <= `criticalvalue') {
        noisily: di _newline as res "Impossible to reject any Ho if Delta <= t-crit*s.e. ( " %-6.4g `criticalvalue' "). See{help tostt##mineqvlevel: help tostt}." _newline
        }
      if (`upper' != `lower' & `lower' <= `criticalvalue') {
        noisily: di _newline as res "Impossible to reject any Ho if |Dl| <= t-crit*s.e. ( " %-6.4g `criticalvalue' "). See{help tostt##mineqvlevel: help tostt}." _newline
        }
      if (`upper' != `lower' & `upper' <= `criticalvalue') {
        noisily: di _newline as res "Impossible to reject any Ho if Du <= t-crit*s.e. ( " %-6.4g `criticalvalue' "). See{help tostt##mineqvlevel: help tostt}." _newline
        }
      if (`upper' == `lower') {
        noisily: di as text "        df = " as res %-8.0g `df' as text " using `n' - 1"
        noisily: di _newline as text "Ho: |diff| >= Delta:" _newline 
        noisily: di as text "        t1 = " as res %-8.4g `t1' as text _col(38) "t2 = " as res %-8.4g `t2' _newline
        noisily: di as text "   Ho1: Delta-diff <= 0       " _col(33) "Ho2: diff+Delta <= 0"
        noisily: di as text "   Ha1: Delta-diff > 0" _col(33) "Ha2: diff+Delta > 0"
        noisily: di as text "   Pr(T > t1) = " as res %6.4f `p1' _col(32) as text " Pr(T > t2) = " as res %6.4f `p2'
        }
      if (`upper' != `lower') {
        noisily: di as text "        df = " as res %-8.0g `df' as text " using `n' - 1"
        noisily: di _newline as text "Ho: diff <= Dl, or diff >= Du:" _newline 
        noisily: di as text "        t1 = " as res %-8.4g `t1' as text _col(38) "t2 = " as res %-8.4g `t2' _newline
        noisily: di as text "   Ho1: Du-diff <= 0       " _col(33) "Ho2: diff-Dl <= 0"
        noisily: di as text "   Ha1: Du-diff > 0" _col(33) "Ha2: diff-Dl > 0"
        noisily: di as text "   Pr(T > t1) = " as res %6.4f `p1' _col(32) as text " Pr(T > t2) = " as res %6.4f `p2'
        }
      }
    if lower("`eqvtype'") == "epsilon" {
      noisily: di as text "        diff = mean(" as res "`varlist'" as text ") - " as res `exp'
      if (`upper' == `lower') {
        noisily: di as text "     epsilon = " as res %-8.4f `lower' as text " " as res "`eqvtype'" as text " expressed in units of the t distribution"
        }
      if (`upper' != `lower') {
        noisily: di as text "epsilon (el) = " as res %-8.4f -1*`lower' as text " " as res "el" as text " expressed in units of the t distribution"
        noisily: di as text "epsilon (eu) = " as res %-8.4f `upper' as text " " as res "eu" as text " expressed in units of the t distribution"
        }
      local criticalvalue = invttail(`df',`alpha')
      if (`upper' == `lower' & `lower' <= `criticalvalue') {
        noisily: di _newline as res "Impossible to reject any Ho if epsilon <= t-crit (" %-5.3f `criticalvalue' "). See{help tostt##mineqvlevel: help tostt}." _newline
        }
      if (`upper' != `lower' & `lower' <= `criticalvalue') {
        noisily: di _newline as res "Impossible to reject any Ho if |el| <= t-crit (" %-5.3f `criticalvalue' "). See{help tostt##mineqvlevel: help tostt}." _newline
        }
      if (`upper' != `lower' & `upper' <= `criticalvalue') {
        noisily: di _newline as res "Impossible to reject any Ho if eu <= t-crit (" %-5.3f `criticalvalue' "). See{help tostt##mineqvlevel: help tostt}." _newline
        }
      if (`upper' == `lower') {
        noisily: di as text "          df = " as res %-8.0g `df' as text " using `n' - 1"
        noisily: di _newline as text "Ho: |T| >= epsilon:" _newline 
        noisily: di as text "        t1 = " as res %-8.4g `t1' as text _col(38) "t2 = " as res %-8.4g `t2' _newline
        noisily: di as text "    Ho1: epsilon-T <= 0       " _col(34) "Ho2: T+epsilon <= 0"
        noisily: di as text "    Ha1: epsilon-T > 0" _col(34) "Ha2: T+epsilon > 0"
        noisily: di as text "    Pr(T > t1) = " as res %6.4f `p1' _col(33) as text " Pr(T > t2) = " as res %6.4f `p2'
        }
      if (`upper' != `lower') {
        noisily: di as text "          df = " as res %-8.0g `df' as text " using `n' - 1"
        noisily: di _newline as text "Ho: T <= el, or T >= eu:" _newline 
        noisily: di as text "        t1 = " as res %-8.4g `t1' as text _col(38) "t2 = " as res %-8.4g `t2' _newline
        noisily: di as text "    Ho1: eu-T <= 0       " _col(34) "Ho2: T-el <= 0"
        noisily: di as text "    Ha1: eu-T > 0" _col(34) "Ha2: T-el > 0"
        noisily: di as text "    Pr(T > t1) = " as res %6.4f `p1' _col(33) as text " Pr(T > t2) = " as res %6.4f `p2'
        }
      }
     }
 

**********
* two-sample unpaired t test for mean equivalence
  if "`by'" != "" || "`unpaired'" != "" & _rc != 0 {
    if "`unequal'" == "" {
      noisily: di as text _newline "Two-sample unpaired t test for mean equivalence with equal variances"
      }
    if "`unequal'" != "" {
      noisily: di as text _newline "Two-sample unpaired t test for mean equivalence with unequal variances"
      }
    if (mod(`level'*10, 1) == 0.0) {
      local level: di %3.1f `level'
      }
    if (mod(`level', 1) == 0.0) {
      local level: di %3.0f `level'
      }
    if "`by'" != "" {
      noisily: _ttest header `level' "Group"
      noisily: _ttest table `level' "`: label (`by')`min''" `n1' `m1' `s1'
      noisily: _ttest table `level' "`: label (`by')`max''"  `n2' `m2' `s2'
      }
    if "`by'" == "" {
      noisily: _ttest header `level' `varlist'
      noisily: _ttest table `level' `varlist' `n1' `m1' `s1'
      noisily: _ttest table `level' `exp'  `n2' `m2' `s2'
       }
    noisily: _ttest divline
    if lower("`eqvtype'") == "delta" {
      if (`upper' == `lower') {
         noisily: _ttest dtable `level' "D-diff" `n' `upper'-(`m1'-`m2') `s' `df'
        noisily: _ttest dtable `level' "diff+D" `n' (`m1'-`m2')+`lower' `s' `df'
        }
      if (`upper' != `lower') {
        noisily: _ttest dtable `level' "Du-diff" `n' `upper'-(`m1'-`m2') `s' `df'
        noisily: _ttest dtable `level' "diff-Dl" `n' (`m1'-`m2')+`lower' `s' `df'
        }
      }
    if lower("`eqvtype'") == "epsilon" {
      noisily: _ttest dtable `level' "diff" `n' `m1'-`m2' `s' `df'
      }
    if (`p1' > `alpha' | `p2' > `alpha') {
      local NegativistConclusion = "Fail to reject"
      }
    noisily: _ttest botline
    if "`by'" == "" {
      noisily: di as text "      diff = mean(" as res "`varlist'" as text ") - mean(" as res "`exp'" as text ")"     
      }
    if "`by'" != "" {
      noisily: di as text "      diff = mean(`varlist'|`by' = " as res "`: label (`by')`min''" as text ") - mean(`varlist'|`by' = " as res "`: label (`by')`max''" as text ")"     
      }
    if lower("`eqvtype'") == "delta" {
      if (`upper' == `lower') {
        noisily: di as text " Delta (D) = " as res %-8.4f `lower' as res "Delta " as text "expressed in same units as " as res "`varlist'"
        }
      if (`upper' != `lower') {
        noisily: di as text "Delta (Dl) = " as res %-8.4f -1*`lower' as res "Dl " as text "expressed in same units as " as res "`varlist'"
        noisily: di as text "Delta (Du) = " as res %-8.4f `upper' as res "Du " as text "expressed in same units as " as res "`varlist'"
        }
      local criticalvalue = `s'*invttail(`df',`alpha')
      if (`upper' == `lower' & `lower' <= `criticalvalue') {
        noisily: di _newline as res "Impossible to reject any Ho if Delta <= t-crit*s.e. ( " %-6.4g `criticalvalue' "). See{help tostt##mineqvlevel: help tostt}." _newline
        }
      if (`upper' != `lower' & `lower' <= `criticalvalue') {
        noisily: di _newline as res "Impossible to reject any Ho if |Dl| <= t-crit*s.e. ( " %-6.4g `criticalvalue' "). See{help tostt##mineqvlevel: help tostt}." _newline
        }
      if (`upper' != `lower' & `upper' <= `criticalvalue') {
        noisily: di _newline as res "Impossible to reject any Ho if Du <= t-crit*s.e. ( " %-6.4g `criticalvalue' "). See{help tostt##mineqvlevel: help tostt}." _newline
        }
      }
    if lower("`eqvtype'") == "epsilon" {
      if (`upper' == `lower') {
        noisily: di as text "     epsilon = " as res %-8.4f `lower' as text " " as res "`eqvtype'" as text " expressed in units of the t distribution"
        }
      if (`upper' != `lower') {
        noisily: di as text "epsilon (el) = " as res %-8.4f -1*`lower' as text " " as res "el" as text " expressed in units of the t distribution"
        noisily: di as text "epsilon (eu) = " as res %-8.4f `upper' as text " " as res "eu" as text " expressed in units of the t distribution"
        }
      local criticalvalue = invttail(`df',`alpha')
      if (`upper' == `lower' & `lower' <= `criticalvalue') {
        noisily: di _newline as res "Impossible to reject any Ho if epsilon <= t-crit (" %-5.3f `criticalvalue' "). See{help tostt##mineqvlevel: help tostt}." _newline
        }
      if (`upper' != `lower' & `lower' <= `criticalvalue') {
        noisily: di _newline as res "Impossible to reject any Ho if |el| <= t-crit (" %-5.3f `criticalvalue' "). See{help tostt##mineqvlevel: help tostt}." _newline
        }
      if (`upper' != `lower' & `upper' <= `criticalvalue') {
        noisily: di _newline as res "Impossible to reject any Ho if eu <= t-crit (" %-5.3f `criticalvalue' "). See{help tostt##mineqvlevel: help tostt}." _newline
        }
      }
    if "`welch'" == "" & "`unequal'" != "" {
      noisily: di as text "        df = " as res %-8.0g `df' as text " using Satterthwaite's formula"
      }
    if "`welch'" != "" & "`unequal'" != "" {
      noisily: di as text "        df = " as res %-8.0g `df' as text " using Welch's formula"
      }
    if  "`unequal'" == "" {
      noisily: di as text "        df = " as res %-8.0g `df' as text " using `n1' + `n2' - 2"
      }
    if lower("`eqvtype'") == "delta" {
      if (`upper' == `lower') {
        noisily: di _newline as text "Ho: |diff| >= Delta:" _newline 
        noisily: di as text "        t1 = " as res %-8.4g `t1' as text _col(38) "t2 = " as res %-8.4g `t2' _newline
        noisily: di as text "   Ho1: Delta-diff <= 0       " _col(33) "Ho2: diff+Delta <= 0"
        noisily: di as text "   Ha1: Delta-diff > 0" _col(33) "Ha2: diff+Delta > 0"
        noisily: di as text "   Pr(T > t1) = " as res %6.4f `p1' _col(32) as text " Pr(T > t2) = " as res %6.4f `p2'
        }
      if (`upper' != `lower') {
        noisily: di _newline as text "Ho: diff <= Dl, or diff >= Du:" _newline 
        noisily: di as text "        t1 = " as res %-8.4g `t1' as text _col(38) "t2 = " as res %-8.4g `t2' _newline
        noisily: di as text "   Ho1: Du-diff <= 0       " _col(33) "Ho2: diff-Dl <= 0"
        noisily: di as text "   Ha1: Du-diff > 0" _col(33) "Ha2: diff-Dl > 0"
        noisily: di as text "   Pr(T > t1) = " as res %6.4f `p1' _col(32) as text " Pr(T > t2) = " as res %6.4f `p2'
        }
      }
    if lower("`eqvtype'") == "epsilon" {
      if (`upper' == `lower') {
        noisily: di _newline as text "Ho: |T| >= epsilon:" _newline 
        noisily: di as text "        t1 = " as res %-8.4g `t1' as text _col(38) "t2 = " as res %-8.4g `t2' _newline
        noisily: di as text "    Ho1: epsilon-T <= 0       " _col(34) "Ho2: T+epsilon <= 0"
        noisily: di as text "    Ha1: epsilon-T > 0" _col(34) "Ha2: T+epsilon > 0"
        noisily: di as text "    Pr(T > t1) = " as res %6.4f `p1' _col(33) as text " Pr(T > t2) = " as res %6.4f `p2'
        }
      if (`upper' != `lower') {
        noisily: di _newline as text "Ho: T <= el, or T >= eu:" _newline 
        noisily: di as text "        t1 = " as res %-8.4g `t1' as text _col(38) "t2 = " as res %-8.4g `t2' _newline
        noisily: di as text "    Ho1: eu-T <= 0       " _col(34) "Ho2: T-el <= 0"
        noisily: di as text "    Ha1: eu-T > 0" _col(34) "Ha2: T-el > 0"
        noisily: di as text "    Pr(T > t1) = " as res %6.4f `p1' _col(33) as text " Pr(T > t2) = " as res %6.4f `p2'
        }
      }
    }


**********
* paired t test for mean equivalence
  if "`by'" == "" & "`unpaired'" == "" & _rc != 0 {
    noisily: di as text _newline "Paired t test for mean equivalence"
    if (mod(`level'*10, 1) == 0.0) {
      local level: di %3.1f `level'
      }
    if (mod(`level', 1) == 0.0) {
      local level: di %3.0f `level'
      }
    noisily: _ttest header `level' `varlist'
    noisily: _ttest table `level' `varlist' `n1' `m1' `s1'
    noisily: _ttest table `level' "`exp'"  `n2' `m2' `s2'
    noisily: _ttest divline
    if lower("`eqvtype'") == "delta" {
      if (`upper' == `lower') {
        noisily: _ttest dtable `level' "D-diff" `n' `upper'-(`m1'-`m2') `s' `df'
        noisily: _ttest dtable `level' "diff+D" `n' (`m1'-`m2')+`lower' `s' `df'
        }
      if (`upper' != `lower') {
        noisily: _ttest dtable `level' "Du-diff" `n' `upper'-(`m1'-`m2') `s' `df'
        noisily: _ttest dtable `level' "diff-Dl" `n' (`m1'-`m2')+`lower' `s' `df'
        }
      }
    if lower("`eqvtype'") == "epsilon" {
      noisily: _ttest table `level' "diff" `n'/2 `m1'-`m2' `s'*sqrt(`n'/2)
      }
    noisily: _ttest botline
    noisily: di as text "mean(diff) =  mean(" as res "`varlist'" as text " - " as res "`exp'" as text ")"
    if lower("`eqvtype'") == "delta" {
      if (`upper' == `lower') {
        noisily: di as text " Delta (D) = " as res %-8.4f `lower' as res "Delta " as text "expressed in same units as " as res "`varlist'"
        }
      if (`upper' != `lower') {
        noisily: di as text "Delta (Dl) = " as res %-8.4f -1*`lower' as res "Dl " as text "expressed in same units as " as res "`varlist'"
        noisily: di as text "Delta (Du) = " as res %-8.4f `upper' as res "Du " as text "expressed in same units as " as res "`varlist'"
        }
      local criticalvalue = r(se)*invttail(`df',`alpha')
      if (`upper' == `lower' & `lower' <= `criticalvalue') {
        noisily: di _newline as res "Impossible to reject any Ho if Delta <= t-crit*s.e. ( " %-6.4g `criticalvalue' "). See{help tostt##mineqvlevel: help tostt}." _newline
        }
      if (`upper' != `lower' & `lower' <= `criticalvalue') {
        noisily: di _newline as res "Impossible to reject any Ho if |Dl| <= t-crit*s.e. ( " %-6.4g `criticalvalue' "). See{help tostt##mineqvlevel: help tostt}." _newline
        }
      if (`upper' != `lower' & `upper' <= `criticalvalue') {
        noisily: di _newline as res "Impossible to reject any Ho if Du <= t-crit*s.e. ( " %-6.4g `criticalvalue' "). See{help tostt##mineqvlevel: help tostt}." _newline
        }
      }
    if lower("`eqvtype'") == "epsilon" {
      if (`upper' == `lower') {
        noisily: di as text "     epsilon = " as res %-8.4f `lower' as text " " as res "`eqvtype'" as text " expressed in units of the t distribution"
        }
      if (`upper' != `lower') {
        noisily: di as text "epsilon (el) = " as res %-8.4f -1*`lower' as text " " as res "el" as text " expressed in units of the t distribution"
        noisily: di as text "epsilon (eu) = " as res %-8.4f `upper' as text " " as res "eu" as text " expressed in units of the t distribution"
        }
      local criticalvalue = invttail(`df',`alpha')
      if (`upper' == `lower' & `lower' <= `criticalvalue') {
        noisily: di _newline as res "Impossible to reject any Ho if epsilon <= t-crit (" %-5.3f `criticalvalue' "). See{help tostt##mineqvlevel: help tostt}." _newline
        }
      if (`upper' != `lower' & `lower' <= `criticalvalue') {
        noisily: di _newline as res "Impossible to reject any Ho if |el| <= t-crit (" %-5.3f `criticalvalue' "). See{help tostt##mineqvlevel: help tostt}." _newline
        }
      if (`upper' != `lower' & `upper' <= `criticalvalue') {
        noisily: di _newline as res "Impossible to reject any Ho if eu <= t-crit (" %-5.3f `criticalvalue' "). See{help tostt##mineqvlevel: help tostt}." _newline
        }
      }
    noisily: di as text "        df = " as res %-8.0g `df'
    if lower("`eqvtype'") == "delta" {
      if (`upper' == `lower') {
        noisily: di _newline as text "Ho: |diff| >= Delta:" _newline 
        noisily: di as text "        t1 = " as res %-8.4g `t1' as text _col(38) "t2 = " as res %-8.4g `t2' _newline
        noisily: di as text "   Ho1: Delta-diff <= 0       " _col(33) "Ho2: diff+Delta <= 0"
        noisily: di as text "   Ha1: Delta-diff > 0" _col(33) "Ha2: diff+Delta > 0"
        noisily: di as text "   Pr(T > t1) = " as res %6.4f `p1' _col(32) as text " Pr(T > t2) = " as res %6.4f `p2'
        }
      if (`upper' != `lower') {
        noisily: di _newline as text "Ho: diff <= Dl, or diff >= Du:" _newline 
        noisily: di as text "        t1 = " as res %-8.4g `t1' as text _col(38) "t2 = " as res %-8.4g `t2' _newline
        noisily: di as text "   Ho1: Du-diff <= 0       " _col(33) "Ho2: diff-Dl <= 0"
        noisily: di as text "   Ha1: Du-diff > 0" _col(33) "Ha2: diff-Dl > 0"
        noisily: di as text "   Pr(T > t1) = " as res %6.4f `p1' _col(32) as text " Pr(T > t2) = " as res %6.4f `p2'
        }
      }
    if lower("`eqvtype'") == "epsilon" {
      if (`upper' == `lower') {
        noisily: di _newline as text "Ho: |T| >= epsilon:" _newline 
        noisily: di as text "        t1 = " as res %-8.4g `t1' as text _col(38) "t2 = " as res %-8.4g `t2' _newline
        noisily: di as text "    Ho1: epsilon-T <= 0       " _col(34) "Ho2: T+epsilon <= 0"
        noisily: di as text "    Ha1: epsilon-T > 0" _col(34) "Ha2: T+epsilon > 0"
        noisily: di as text "    Pr(T > t1) = " as res %6.4f `p1' _col(33) as text " Pr(T > t2) = " as res %6.4f `p2'
        }
      if (`upper' != `lower') {
        noisily: di _newline as text "Ho: T <= el, or T >= eu:" _newline 
        noisily: di as text "        t1 = " as res %-8.4g `t1' as text _col(38) "t2 = " as res %-8.4g `t2' _newline
        noisily: di as text "    Ho1: eu-T <= 0       " _col(34) "Ho2: T-el <= 0"
        noisily: di as text "    Ha1: eu-T > 0" _col(34) "Ha2: T-el > 0"
        noisily: di as text "    Pr(T > t1) = " as res %6.4f `p1' _col(33) as text " Pr(T > t2) = " as res %6.4f `p2'
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
  }


*******************************************************************************
* Program end. Close up shop and return things.                               *
*******************************************************************************

  if ("`relevance'" != "") {
    return local relevance = "`RelevanceTestConclusion'"
    }
  capture confirm number `exp'
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
  return scalar mu_1    = `m1'
  if _rc != 0 {
    return scalar N_2   = `n2'
    return scalar mu_2  = `m2'
    }
  return scalar df_t    = `df'
  return scalar t1      = `t1'
  return scalar t2      = `t2'
  return scalar p1      = `p1'
  return scalar p2      = `p2'
  return scalar se      = `s'
  return scalar sd_1    = `s1'
  if _rc != 0 {
    return scalar sd_2  = `s2'
    }
  end


program define tostt14, rclass byable(recall)
  version 14.0, missing

  /* turn "==" into "=" if needed before calling -syntax- */
  gettoken vn rest : 0, parse(" =")
  gettoken eq rest : rest, parse(" =")
  if "`eq'" == "==" {
    local 0 `vn' = `rest'
    }

********************************************************************************
  syntax varname [=/exp] [if] [in] [, EQVType(string) EQVLevel(real 1) /*
*/      UPPEReqvlevel(real 0) BY(varname) UNPaired UNEqual Welch /*
*/      Alpha(real 0.05) RELevance ]

  quietly {

  * Create theta constant
  local theta = uchar(0952)

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
    local eqvlevel = 1
    }

  if (lower("`eqvtype'") == "epsilon") & (`eqvlevel' == 1 & `uppereqvlevel'==0) {
    local eqvlevel = 2
    }

  if (lower("`eqvtype'") == "delta" || lower("`eqvtype'") == "epsilon") & (`eqvlevel' <= 0 & `uppereqvlevel' != abs(`eqvlevel')) {
    noisily: di as err "option eqvlevel() incorrectly specified" _newline "the tolerance must be a positive real value"
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

  * Validate unequal variances against Welch
  if "`welch'" != "" & ("`unpaired'" != "" || "`by'" != "") & "`unequal'" == "" {
    noisily: di _newline as res "welch option specified, proceeding by assuming unequal variances"
    local unequal = "unequal"
    } 
  if "`welch'" != "" & "`by'" == "" & "`unpaired'" == "" & "`unequal'" == "" {
    noisily: di as err "welch option invalid for one-sample or paired tests"
    exit 198
    } 

  * Invalidate options specified with one-sample test
  capture confirm number `exp'
  if _rc == 0 {
    if "`welch'" != "" {
      noisily: di as err "welch option invalid for one-sample tests"
      exit 198
      } 
    if "`unequal'" != "" {
      noisily: di as err "unequal option invalid for one-sample tests"
      exit 198
      } 
    if "`unpaired'" != "" {
      noisily: di as err "unpaired option invalid for one-sample tests"
      exit 198
      } 
    if "`by'" != "" {
      noisily di as err "may not combine = and option by()"
      exit 198
      }

   }


  * Validate alpha
  if (`alpha' < 0 | `alpha' > 1) {
    noisily: di as err "option alpha() must be between 0 and 1 inclusive"
    exit 198
    }
  local level : di %3.2f (100*(1-`alpha'))
 

*******************************************************************************
* The business starts here                                                    *
*******************************************************************************

**********
* conduct the positivist t test of difference
  local PositivistConclusion = "Reject"
  local NegativistConclusion = "Reject"
  if "`exp'" != "" & "`by'" == "" {
    local eexp = "= `exp'"
    }
  if "`relevance'" == "" {
    quietly: ttest `varlist' `eexp' `if' `in', `unpaired' `unequal' `welch' by("`by'") level(`level')
    }
   else {
    noi: di as txt "Relevance test of sample means"
    noi: ttest `varlist' `eexp' `if' `in', `unpaired' `unequal' `welch' by("`by'") level(`level')
    noi: di _newline
    }

  capture confirm number `exp'
  if _rc != 0 {
    local n1 = r(N_1)
    local m1 = r(mu_1)
    local s1 = r(sd_1)
    local n2 = r(N_2)
    local m2 = r(mu_2)
    local s2 = r(sd_2)
    local n = `n1'+`n2'
    local s = r(se)
    local df = r(df_t)
    if ttail(r(df_t),abs(((r(mu_1) - r(mu_2))/r(se)))) > (`alpha'/2) {
      local PositivistConclusion = "Fail to reject"
      }
    if lower("`eqvtype'") == "delta" {
      local t1 = (`upper' - (r(mu_1) - r(mu_2)))/r(se)
      local t2 = ((r(mu_1) - r(mu_2))+`lower')/r(se)
      }
    if lower("`eqvtype'") == "epsilon" {
      local t1 = `upper' - ( ( (r(mu_1) - r(mu_2)))/r(se) )
      local t2 = ( ((r(mu_1) - r(mu_2)))/r(se) ) + `lower'
      }
    local p1 = ttail(`df',`t1')
    local p2 = ttail(`df',`t2')
    if (`p1' > `alpha' | `p2' > `alpha') {
      local NegativistConclusion = "Fail to reject"
      }
    if "`by'" != "" {
      sum `by'
      local min = r(min)
      local max = r(max)
      }
    }


**********
* one-sample t test for mean equivalence
  if _rc == 0 {
    if ttail(r(df_t),abs(((r(mu_1) - `exp')/r(se)))) > (`alpha'/2) {
      local PositivistConclusion = "Fail to reject"
      }
    local n1 = r(N_1)
    local m1 = r(mu_1)
    local s1 = r(sd_1)
    local s = r(se)
    local n = r(N_1)
    local df = r(df_t)
    if lower("`eqvtype'") == "delta" {
      local t1 = (`upper' - (r(mu_1) - `exp'))/r(se)
      local t2 = ((r(mu_1) - `exp')+`lower')/r(se)
      }
    if lower("`eqvtype'") == "epsilon" {
      local t1 = `upper' - ( ((r(mu_1) - `exp'))/r(se) )
      local t2 = ( ((r(mu_1) - `exp'))/r(se) ) + `lower'
      }
    local p1 = ttail(`df',`t1')
    local p2 = ttail(`df',`t2')
    if (`p1' > `alpha' | `p2' > `alpha') {
      local NegativistConclusion = "Fail to reject"
      }
    noisily: di as text _newline "One-sample t test for mean equivalence"
    if (mod(`level'*10, 1) == 0.0) {
      local level: di %3.1f `level'
      }
    if (mod(`level', 1) == 0.0) {
      local level: di %3.0f `level'
      }
    noisily: _ttest header `level' `varlist'
    noisily: _ttest table `level' `varlist' `n1' `m1' `s1'
    if lower("`eqvtype'") == "delta" {
      local eqvtypeout = uchar(0916)
      noisily: _ttest divline
      if (`upper' == `lower') {
        noisily: _ttest dtable `level' "`eqvtypeout'-`theta'" `n' `upper'-(`m1'-`exp') `s' `df'
        noisily: _ttest dtable `level' "`theta'+`eqvtypeout'" `n' (`m1'-`exp')+`lower' `s' `df'
        }
      if (`upper' != `lower') {
        noisily: _ttest dtable `level' "`eqvtypeout'u-`theta'" `n' `upper'-(`m1'-`exp') `s' `df'
        noisily: _ttest dtable `level' "`theta'-`eqvtypeout'l" `n' (`m1'-`exp')+`lower' `s' `df'
        }
      }
    noisily: _ttest botline
    if lower("`eqvtype'") == "delta" {
      local eqvtypeout = uchar(0916)
      noisily: di as text "         `theta' = mean(" as res "`varlist'" as text ") - " as res `exp'
      if (`upper' == `lower') {
        noisily: di as text "         `eqvtypeout' = " as res %-8.4f `lower' as res "`eqvtypeout' " as text "expressed in same units as " as res "`varlist'"
        }
      if (`upper' != `lower') {
        noisily: di as text "        `eqvtypeout'l = " as res %-8.4f -1*`lower' as res "`eqvtypeout'l " as text "expressed in same units as " as res "`varlist'"
        noisily: di as text "        `eqvtypeout'u = " as res %-8.4f `upper' as res "`eqvtypeout'u " as text "expressed in same units as " as res "`varlist'"
        }
      local criticalvalue = r(se)*invttail(`df',`alpha')
      if (`upper' == `lower' & `lower' <= `criticalvalue') {
        noisily: di _newline as res "Impossible to reject any Ho if `eqvtypeout' <= t-crit*s.e. ( " %-6.4g `criticalvalue' "). See{help tostt##mineqvlevel: help tostt}." _newline
        }
      if (`upper' != `lower' & `lower' <= `criticalvalue') {
        noisily: di _newline as res "Impossible to reject any Ho if |`eqvtypeout'l| <= t-crit*s.e. ( " %-6.4g `criticalvalue' "). See{help tostt##mineqvlevel: help tostt}." _newline
        }
      if (`upper' != `lower' & `upper' <= `criticalvalue') {
        noisily: di _newline as res "Impossible to reject any Ho if `eqvtypeout'u <= t-crit*s.e. ( " %-6.4g `criticalvalue' "). See{help tostt##mineqvlevel: help tostt}." _newline
        }
      if (`upper' == `lower') {
        noisily: di as text "        df = " as res %-8.0g `df' as text " using `n' - 1"
        noisily: di _newline as text "Ho: |`theta'| >= `eqvtypeout':" _newline 
        noisily: di as text "        t1 = " as res %-8.4g `t1' as text _col(38) "t2 = " as res %-8.4g `t2' _newline
        noisily: di as text "   Ho1: `eqvtypeout'-`theta' <= 0       " _col(33) "Ho2: `theta'+`eqvtypeout' <= 0"
        noisily: di as text "   Ha1: `eqvtypeout'-`theta' > 0" _col(33) "Ha2: `theta'+`eqvtypeout' > 0"
        noisily: di as text "   Pr(T > t1) = " as res %6.4f `p1' _col(32) as text " Pr(T > t2) = " as res %6.4f `p2'
        }
      if (`upper' != `lower') {
        noisily: di as text "        df = " as res %-8.0g `df' as text " using `n' - 1"
        noisily: di _newline as text "Ho: `theta' <= `eqvtypeout'l, or `theta' >= `eqvtypeout'u:" _newline 
        noisily: di as text "        t1 = " as res %-8.4g `t1' as text _col(38) "t2 = " as res %-8.4g `t2' _newline
        noisily: di as text "   Ho1: `eqvtypeout'u-`theta' <= 0       " _col(33) "Ho2: `theta'-`eqvtypeout'l <= 0"
        noisily: di as text "   Ha1: `eqvtypeout'u-`theta' > 0" _col(33) "Ha2: `theta'-`eqvtypeout'l > 0"
        noisily: di as text "   Pr(T > t1) = " as res %6.4f `p1' _col(32) as text " Pr(T > t2) = " as res %6.4f `p2'
        }
      }
    if lower("`eqvtype'") == "epsilon" {
      local eqvtypeout = uchar(0949)
      noisily: di as text "         `theta' = mean(" as res "`varlist'" as text ") - " as res `exp'
      if (`upper' == `lower') {
        noisily: di as text "         `eqvtypeout' = " as res %-8.4f `lower' as text " " as res "`eqvtypeout'" as text " expressed in units of the t distribution"
        }
      if (`upper' != `lower') {
        noisily: di as text "        `eqvtypeout'l = " as res %-8.4f -1*`lower' as text " " as res "`eqvtypeout'l" as text " expressed in units of the t distribution"
        noisily: di as text "        `eqvtypeout'u = " as res %-8.4f `upper' as text " " as res "`eqvtypeout'u" as text " expressed in units of the t distribution"
        }
      local criticalvalue = invttail(`df',`alpha')
      if (`upper' == `lower' & `lower' <= `criticalvalue') {
        noisily: di _newline as res "Impossible to reject any Ho if `eqvtypeout' <= t-crit (" %-5.3f `criticalvalue' "). See{help tostt##mineqvlevel: help tostt}." _newline
        }
      if (`upper' != `lower' & `lower' <= `criticalvalue') {
        noisily: di _newline as res "Impossible to reject any Ho if |`eqvtypeout'l| <= t-crit (" %-5.3f `criticalvalue' "). See{help tostt##mineqvlevel: help tostt}." _newline
        }
      if (`upper' != `lower' & `upper' <= `criticalvalue') {
        noisily: di _newline as res "Impossible to reject any Ho if `eqvtypeout'u <= t-crit (" %-5.3f `criticalvalue' "). See{help tostt##mineqvlevel: help tostt}." _newline
        }
      if (`upper' == `lower') {
        noisily: di as text "        df = " as res %-8.0g `df' as text " using `n' - 1"
        noisily: di _newline as text "Ho: |T| >= `eqvtypeout':" _newline 
        noisily: di as text "        t1 = " as res %-8.4g `t1' as text _col(38) "t2 = " as res %-8.4g `t2' _newline
        noisily: di as text "    Ho1: `eqvtypeout'-T <= 0       " _col(34) "Ho2: T+`eqvtypeout' <= 0"
        noisily: di as text "    Ha1: `eqvtypeout'-T > 0" _col(34) "Ha2: T+`eqvtypeout' > 0"
        noisily: di as text "    Pr(T > t1) = " as res %6.4f `p1' _col(33) as text " Pr(T > t2) = " as res %6.4f `p2'
        }
      if (`upper' != `lower') {
        noisily: di as text "        df = " as res %-8.0g `df' as text " using `n' - 1"
        noisily: di _newline as text "Ho: T <= `eqvtypeout'l, or T >= `eqvtypeout'u:" _newline 
        noisily: di as text "        t1 = " as res %-8.4g `t1' as text _col(38) "t2 = " as res %-8.4g `t2' _newline
        noisily: di as text "    Ho1: `eqvtypeout'u-T <= 0       " _col(34) "Ho2: T-`eqvtypeout'l <= 0"
        noisily: di as text "    Ha1: `eqvtypeout'u-T > 0" _col(34) "Ha2: T-`eqvtypeout'l > 0"
        noisily: di as text "    Pr(T > t1) = " as res %6.4f `p1' _col(33) as text " Pr(T > t2) = " as res %6.4f `p2'
        }
      }
     }
 

**********
* two-sample unpaired t test for mean equivalence
  if "`by'" != "" || "`unpaired'" != "" & _rc != 0 {
    if "`unequal'" == "" {
      noisily: di as text _newline "Two-sample unpaired t test for mean equivalence with equal variances"
      }
    if "`unequal'" != "" {
      noisily: di as text _newline "Two-sample unpaired t test for mean equivalence with unequal variances"
      }
    if (mod(`level'*10, 1) == 0.0) {
      local level: di %3.1f `level'
      }
    if (mod(`level', 1) == 0.0) {
      local level: di %3.0f `level'
      }
    if "`by'" != "" {
      noisily: _ttest header `level' "Group"
      noisily: _ttest table `level' "`: label (`by')`min''" `n1' `m1' `s1'
      noisily: _ttest table `level' "`: label (`by')`max''"  `n2' `m2' `s2'
      }
    if "`by'" == "" {
      noisily: _ttest header `level' `varlist'
      noisily: _ttest table `level' `varlist' `n1' `m1' `s1'
      noisily: _ttest table `level' `exp'  `n2' `m2' `s2'
       }
    noisily: _ttest divline
    if lower("`eqvtype'") == "delta" {
      local eqvtypeout = uchar(0916)
      if (`upper' == `lower') {
        noisily: _ttest dtable `level' "`eqvtypeout'-`theta'" `n' `upper'-(`m1'-`m2') `s' `df'
        noisily: _ttest dtable `level' "`theta'+`eqvtypeout'" `n' (`m1'-`m2')+`lower' `s' `df'
        }
      if (`upper' != `lower') {
        noisily: _ttest dtable `level' "`eqvtypeout'u-`theta'" `n' `upper'-(`m1'-`m2') `s' `df'
        noisily: _ttest dtable `level' "`theta'-`eqvtypeout'l" `n' (`m1'-`m2')+`lower' `s' `df'
        }
      }
    if lower("`eqvtype'") == "epsilon" {
      noisily: _ttest dtable `level' "θ" `n' `m1'-`m2' `s' `df'
      }
    if (`p1' > `alpha' | `p2' > `alpha') {
      local NegativistConclusion = "Fail to reject"
      }
    noisily: _ttest botline
    if "`by'" == "" {
      noisily: di as text "         `theta' = mean(" as res "`varlist'" as text ") - mean(" as res "`exp'" as text ")"     
      }
    if "`by'" != "" {
      noisily: di as text "         `theta' = mean(`varlist'|`by' = " as res "`: label (`by')`min''" as text ") - mean(`varlist'|`by' = " as res "`: label (`by')`max''" as text ")"     
      }
    if lower("`eqvtype'") == "delta" {
      local eqvtypeout = uchar(0916)
      if (`upper' == `lower') {
        noisily: di as text "         `eqvtypeout' = " as res %-8.4f `lower' as res "`eqvtypeout' " as text "expressed in same units as " as res "`varlist'"
        }
      if (`upper' != `lower') {
        noisily: di as text "        `eqvtypeout'l = " as res %-8.4f -1*`lower' as res "`eqvtypeout'l " as text "expressed in same units as " as res "`varlist'"
        noisily: di as text "        `eqvtypeout'u = " as res %-8.4f `upper' as res "`eqvtypeout'u " as text "expressed in same units as " as res "`varlist'"
        }
      local criticalvalue = `s'*invttail(`df',`alpha')
      if (`upper' == `lower' & `lower' <= `criticalvalue') {
        noisily: di _newline as res "Impossible to reject any Ho if `eqvtypeout' <= t-crit*s.e. ( " %-6.4g `criticalvalue' "). See{help tostt##mineqvlevel: help tostt}." _newline
        }
      if (`upper' != `lower' & `lower' <= `criticalvalue') {
        noisily: di _newline as res "Impossible to reject any Ho if |`eqvtypeout'l| <= t-crit*s.e. ( " %-6.4g `criticalvalue' "). See{help tostt##mineqvlevel: help tostt}." _newline
        }
      if (`upper' != `lower' & `upper' <= `criticalvalue') {
        noisily: di _newline as res "Impossible to reject any Ho if `eqvtypeout'u <= t-crit*s.e. ( " %-6.4g `criticalvalue' "). See{help tostt##mineqvlevel: help tostt}." _newline
        }
      }
    if lower("`eqvtype'") == "epsilon" {
      local eqvtypeout = uchar(0949)
      if (`upper' == `lower') {
        noisily: di as text "         `eqvtypeout' = " as res %-8.4f `lower' as text " " as res "`eqvtypeout'" as text " expressed in units of the t distribution"
        }
      if (`upper' != `lower') {
        noisily: di as text "        `eqvtypeout'l = " as res %-8.4f -1*`lower' as text " " as res "`eqvtypeout'l" as text " expressed in units of the t distribution"
        noisily: di as text "        `eqvtypeout'u = " as res %-8.4f `upper' as text " " as res "`eqvtypeout'u" as text " expressed in units of the t distribution"
        }
      local criticalvalue = invttail(`df',`alpha')
      if (`upper' == `lower' & `lower' <= `criticalvalue') {
        noisily: di _newline as res "Impossible to reject any Ho if `eqvtypeout' <= t-crit (" %-5.3f `criticalvalue' "). See{help tostt##mineqvlevel: help tostt}." _newline
        }
      if (`upper' != `lower' & `lower' <= `criticalvalue') {
        noisily: di _newline as res "Impossible to reject any Ho if |`eqvtypeout'l| <= t-crit (" %-5.3f `criticalvalue' "). See{help tostt##mineqvlevel: help tostt}." _newline
        }
      if (`upper' != `lower' & `upper' <= `criticalvalue') {
        noisily: di _newline as res "Impossible to reject any Ho if `eqvtypeout'u <= t-crit (" %-5.3f `criticalvalue' "). See{help tostt##mineqvlevel: help tostt}." _newline
        }
      }
    if "`welch'" == "" & "`unequal'" != "" {
      noisily: di as text "        df = " as res %-8.0g `df' as text " using Satterthwaite's formula"
      }
    if "`welch'" != "" & "`unequal'" != "" {
      noisily: di as text "        df = " as res %-8.0g `df' as text " using Welch's formula"
      }
    if  "`unequal'" == "" {
      noisily: di as text "        df = " as res %-8.0g `df' as text " using `n1' + `n2' - 2"
      }
    if lower("`eqvtype'") == "delta" {
      local eqvtypeout = uchar(0916)
      if (`upper' == `lower') {
        noisily: di _newline as text "Ho: |`theta'| >= `eqvtypeout':" _newline 
        noisily: di as text "        t1 = " as res %-8.4g `t1' as text _col(38) "t2 = " as res %-8.4g `t2' _newline
        noisily: di as text "   Ho1: `eqvtypeout'-`theta' <= 0       " _col(33) "Ho2: `theta'+`eqvtypeout' <= 0"
        noisily: di as text "   Ha1: `eqvtypeout'-`theta' > 0" _col(33) "Ha2: `theta'+`eqvtypeout' > 0"
        noisily: di as text "   Pr(T > t1) = " as res %6.4f `p1' _col(32) as text " Pr(T > t2) = " as res %6.4f `p2'
        }
      if (`upper' != `lower') {
        noisily: di _newline as text "Ho: `theta' <= `eqvtypeout'l, or diff >= `eqvtypeout'u:" _newline 
        noisily: di as text "        t1 = " as res %-8.4g `t1' as text _col(38) "t2 = " as res %-8.4g `t2' _newline
        noisily: di as text "   Ho1: `eqvtypeout'u-diff <= 0       " _col(33) "Ho2: diff-`eqvtypeout'l <= 0"
        noisily: di as text "   Ha1: `eqvtypeout'u-diff > 0" _col(33) "Ha2: diff-`eqvtypeout'l > 0"
        noisily: di as text "   Pr(T > t1) = " as res %6.4f `p1' _col(32) as text " Pr(T > t2) = " as res %6.4f `p2'
        }
      }
    if lower("`eqvtype'") == "epsilon" {
      local eqvtypeout = uchar(0949)
      if (`upper' == `lower') {
        noisily: di _newline as text "Ho: |T| >= `eqvtypeout':" _newline 
        noisily: di as text "        t1 = " as res %-8.4g `t1' as text _col(38) "t2 = " as res %-8.4g `t2' _newline
        noisily: di as text "    Ho1: `eqvtypeout'-T <= 0       " _col(34) "Ho2: T+`eqvtypeout' <= 0"
        noisily: di as text "    Ha1: `eqvtypeout'-T > 0" _col(34) "Ha2: T+`eqvtypeout' > 0"
        noisily: di as text "    Pr(T > t1) = " as res %6.4f `p1' _col(33) as text " Pr(T > t2) = " as res %6.4f `p2'
        }
      if (`upper' != `lower') {
        noisily: di _newline as text "Ho: T <= `eqvtypeout'l, or T >= `eqvtypeout'u:" _newline 
        noisily: di as text "        t1 = " as res %-8.4g `t1' as text _col(38) "t2 = " as res %-8.4g `t2' _newline
        noisily: di as text "    Ho1: `eqvtypeout'u-T <= 0       " _col(34) "Ho2: T-`eqvtypeout'l <= 0"
        noisily: di as text "    Ha1: `eqvtypeout'u-T > 0" _col(34) "Ha2: T-`eqvtypeout'l > 0"
        noisily: di as text "    Pr(T > t1) = " as res %6.4f `p1' _col(33) as text " Pr(T > t2) = " as res %6.4f `p2'
        }
      }
    }


**********
* paired t test for mean equivalence
  if "`by'" == "" & "`unpaired'" == "" & _rc != 0 {
    noisily: di as text _newline "Paired t test for mean equivalence"
    if (mod(`level'*10, 1) == 0.0) {
      local level: di %3.1f `level'
      }
    if (mod(`level', 1) == 0.0) {
      local level: di %3.0f `level'
      }
    noisily: _ttest header `level' `varlist'
    noisily: _ttest table `level' `varlist' `n1' `m1' `s1'
    noisily: _ttest table `level' "`exp'"  `n2' `m2' `s2'
    noisily: _ttest divline
    if lower("`eqvtype'") == "delta" {
      local eqvtypeout = uchar(0916)
      if (`upper' == `lower') {
        noisily: _ttest dtable `level' "`eqvtypeout'-`theta'" `n' `upper'-(`m1'-`m2') `s' `df'
        noisily: _ttest dtable `level' "`theta'+`eqvtypeout'" `n' (`m1'-`m2')+`lower' `s' `df'
        }
      if (`upper' != `lower') {
        noisily: _ttest dtable `level' "`eqvtypeout'u-`theta'" `n' `upper'-(`m1'-`m2') `s' `df'
        noisily: _ttest dtable `level' "`theta'-`eqvtypeout'l" `n' (`m1'-`m2')+`lower' `s' `df'
        }
      }
    if lower("`eqvtype'") == "epsilon" {
      noisily: _ttest table `level' "θ" `n'/2 `m1'-`m2' `s'*sqrt(`n'/2)
      }
    noisily: _ttest botline
    noisily: di as text "   mean(`theta') = mean(" as res "`varlist'" as text " - " as res "`exp'" as text ")"
    if lower("`eqvtype'") == "delta" {
      local eqvtypeout = uchar(0916)
      if (`upper' == `lower') {
        noisily: di as text "         `eqvtypeout' = " as res %-8.4f `lower' as res "`eqvtypeout' " as text "expressed in same units as " as res "`varlist'"
        }
      if (`upper' != `lower') {
        noisily: di as text "        `eqvtypeout'l = " as res %-8.4f -1*`lower' as res "`eqvtypeout'l " as text "expressed in same units as " as res "`varlist'"
        noisily: di as text "        `eqvtypeout'u = " as res %-8.4f `upper' as res "`eqvtypeout'u " as text "expressed in same units as " as res "`varlist'"
        }
      local criticalvalue = r(se)*invttail(`df',`alpha')
      if (`upper' == `lower' & `lower' <= `criticalvalue') {
        noisily: di _newline as res "Impossible to reject any Ho if `eqvtypeout' <= t-crit*s.e. ( " %-6.4g `criticalvalue' "). See{help tostt##mineqvlevel: help tostt}." _newline
        }
      if (`upper' != `lower' & `lower' <= `criticalvalue') {
        noisily: di _newline as res "Impossible to reject any Ho if |`eqvtypeout'l| <= t-crit*s.e. ( " %-6.4g `criticalvalue' "). See{help tostt##mineqvlevel: help tostt}." _newline
        }
      if (`upper' != `lower' & `upper' <= `criticalvalue') {
        noisily: di _newline as res "Impossible to reject any Ho if `eqvtypeout'u <= t-crit*s.e. ( " %-6.4g `criticalvalue' "). See{help tostt##mineqvlevel: help tostt}." _newline
        }
      }
    if lower("`eqvtype'") == "epsilon" {
      local eqvtypeout = uchar(0949)
      if (`upper' == `lower') {
        noisily: di as text "         `eqvtypeout' = " as res %-8.4f `lower' as text " " as res "`eqvtypeout'" as text " expressed in units of the t distribution"
        }
      if (`upper' != `lower') {
        noisily: di as text "        `eqvtypeout'l = " as res %-8.4f -1*`lower' as text " " as res "`eqvtypeout'l" as text " expressed in units of the t distribution"
        noisily: di as text "        `eqvtypeout'u = " as res %-8.4f `upper' as text " " as res "`eqvtypeout'u" as text " expressed in units of the t distribution"
        }
      local criticalvalue = invttail(`df',`alpha')
      if (`upper' == `lower' & `lower' <= `criticalvalue') {
        noisily: di _newline as res "Impossible to reject any Ho if `eqvtypeout' <= t-crit (" %-5.3f `criticalvalue' "). See{help tostt##mineqvlevel: help tostt}." _newline
        }
      if (`upper' != `lower' & `lower' <= `criticalvalue') {
        noisily: di _newline as res "Impossible to reject any Ho if |`eqvtypeout'l| <= t-crit (" %-5.3f `criticalvalue' "). See{help tostt##mineqvlevel: help tostt}." _newline
        }
      if (`upper' != `lower' & `upper' <= `criticalvalue') {
        noisily: di _newline as res "Impossible to reject any Ho if eu <= t-crit (" %-5.3f `criticalvalue' "). See{help tostt##mineqvlevel: help tostt}." _newline
        }
      }
    noisily: di as text "        df = " as res %-8.0g `df'
    if lower("`eqvtype'") == "delta" {
      local eqvtypeout = uchar(0916)
      if (`upper' == `lower') {
        noisily: di _newline as text "Ho: |`theta'| >= `eqvtypeout':" _newline 
        noisily: di as text "        t1 = " as res %-8.4g `t1' as text _col(38) "t2 = " as res %-8.4g `t2' _newline
        noisily: di as text "   Ho1: `eqvtypeout'-`theta' <= 0       " _col(33) "Ho2: `theta'+`eqvtypeout' <= 0"
        noisily: di as text "   Ha1: `eqvtypeout'-`theta' > 0" _col(33) "Ha2: `theta'+`eqvtypeout' > 0"
        noisily: di as text "   Pr(T > t1) = " as res %6.4f `p1' _col(32) as text " Pr(T > t2) = " as res %6.4f `p2'
        }
      if (`upper' != `lower') {
        noisily: di _newline as text "Ho: `theta' <= `eqvtypeout'l, or `theta' >= `eqvtypeout'u:" _newline 
        noisily: di as text "        t1 = " as res %-8.4g `t1' as text _col(38) "t2 = " as res %-8.4g `t2' _newline
        noisily: di as text "   Ho1: `eqvtypeout'u-`theta' <= 0       " _col(33) "Ho2: `theta'-`eqvtypeout'l <= 0"
        noisily: di as text "   Ha1: `eqvtypeout'u-`theta' > 0" _col(33) "Ha2: `theta'-`eqvtypeout'l > 0"
        noisily: di as text "   Pr(T > t1) = " as res %6.4f `p1' _col(32) as text " Pr(T > t2) = " as res %6.4f `p2'
        }
      }
    if lower("`eqvtype'") == "epsilon" {
      local eqvtypeout = uchar(0949)
      if (`upper' == `lower') {
        noisily: di _newline as text "Ho: |T| >= `eqvtypeout':" _newline 
        noisily: di as text "        t1 = " as res %-8.4g `t1' as text _col(38) "t2 = " as res %-8.4g `t2' _newline
        noisily: di as text "    Ho1: `eqvtypeout'-T <= 0       " _col(34) "Ho2: T+`eqvtypeout' <= 0"
        noisily: di as text "    Ha1: `eqvtypeout'-T > 0" _col(34) "Ha2: T+`eqvtypeout' > 0"
        noisily: di as text "    Pr(T > t1) = " as res %6.4f `p1' _col(33) as text " Pr(T > t2) = " as res %6.4f `p2'
        }
      if (`upper' != `lower') {
        noisily: di _newline as text "Ho: T <= `eqvtypeout'l, or T >= `eqvtypeout'u:" _newline 
        noisily: di as text "        t1 = " as res %-8.4g `t1' as text _col(38) "t2 = " as res %-8.4g `t2' _newline
        noisily: di as text "    Ho1: `eqvtypeout'u-T <= 0       " _col(34) "Ho2: T-`eqvtypeout'l <= 0"
        noisily: di as text "    Ha1: `eqvtypeout'u-T > 0" _col(34) "Ha2: T-`eqvtypeout'l > 0"
        noisily: di as text "    Pr(T > t1) = " as res %6.4f `p1' _col(33) as text " Pr(T > t2) = " as res %6.4f `p2'
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
        local eqvtypeout= uchar(0916)
        if (`lower' < 1) {
          noi: di _newline _newline as txt "Relevance test conclusion for " uchar(0945) " = " as res "0" `alpha' as txt ", and `eqvtypeout' = " as res "0" `lower' as txt ":"
          }
         else {
          noi: di _newline _newline as txt "Relevance test conclusion for " uchar(0945) " = " as res "0" `alpha' as txt ", and `eqvtypeout' = " as res `lower' as txt ":"
          }
        }
      if lower("`eqvtype'") == "epsilon" {
        local eqvtypeout = uchar(0949)
        noi: di _newline _newline as txt "Relevance test conclusion for " uchar(0945) " = " as res "0" `alpha' as txt ", and `eqvtypeout' = " as res `lower' as txt ":"
        }
      }
    if (`upper' != `lower') {

      if lower("`eqvtype'") == "delta" {
        local eqvtypeout = uchar(0916)
        if (`lower' < 1 & `upper' < 1) {
          noi: di _newline _newline as txt "Relevance test conclusion for " uchar(0945) " = " as res "0" `alpha' as txt ", `eqvtypeout'l = " as res "-0" `lower' as txt ", and `eqvtypeout'u = " as res "0" `upper' as txt ":"
          }
        if (`lower' >= 1 & `upper' < 1) {
          noi: di _newline _newline as txt "Relevance test conclusion for " uchar(0945) " = " as res "0" `alpha' as txt ", `eqvtypeout'l = " as res -1*`lower' as txt  ", and `eqvtypeout'u = " as res "0" `upper' as txt ":"
          }
        if (`lower' < 1 & `upper' >= 1) {
          noi: di _newline _newline as txt "Relevance test conclusion for " uchar(0945) " = " as res "0" `alpha' as txt ", `eqvtypeout'l = " as res "-0" `lower' as txt  ", and `eqvtypeout'u = " as res `upper' as txt ":"
          }
        if (`lower' >= 1 & `upper' >= 1) {
          noi: di _newline _newline as txt "Relevance test conclusion for " uchar(0945) " = " as res "0" `alpha' as txt ", `eqvtypeout'l = " as res -1*`lower' as txt  ", and `eqvtypeout'u = " as res `upper' as txt ":"
          }
        }
      if lower("`eqvtype'") == "epsilon" {
        local eqvtypeout = uchar(0949)
        noi: di _newline _newline as txt "Relevance test conclusion for " uchar(0945) " = " as res "0" `alpha' as txt ", `eqvtypeout'l = " as res -1*`lower' as txt  ", and `eqvtypeout'u = " as res `upper' as txt ":"
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
  }


*******************************************************************************
* Program end. Close up shop and return things.                               *
*******************************************************************************

  if ("`relevance'" != "") {
    return local relevance = "`RelevanceTestConclusion'"
    }
  capture confirm number `exp'
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
  return scalar mu_1    = `m1'
  if _rc != 0 {
    return scalar N_2   = `n2'
    return scalar mu_2  = `m2'
    }
  return scalar df_t    = `df'
  return scalar t1      = `t1'
  return scalar t2      = `t2'
  return scalar p1      = `p1'
  return scalar p2      = `p2'
  return scalar se      = `s'
  return scalar sd_1    = `s1'
  if _rc != 0 {
    return scalar sd_2  = `s2'
    }
  end

