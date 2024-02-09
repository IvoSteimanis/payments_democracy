*! version 3.1.3  15may2021 by alexis dot dinno at pdx dot edu
*! perform two one-sided tests for  the distribution of paired or matched data 
*! being equivalent to one that is symmetrical & centered on zero

* Syntax:  tostsignrank varname [=exp] [if exp] [in range] [, eqvtype(type) 
*          eqvlevel(#) uppereqvlevel(#) ccontinuity alpha(#) relevance]

program define tostsignrank

  if int(_caller())<8 {
    di in r "tostsignrank- does not support this version of Stata." _newline
    di as txt "Requests for a version compatible with versions of STATA earlier than v8 are "
    di as txt "untenable since I do not have access to the software." _newline 
    exit
  }
  if int(_caller())<14 {
    tostsignrank8 `0'
    }
   else {
    tostsignrank14 `0'
    }
  end


program define tostsignrank8, rclass byable(recall)
  version 8.0, missing

  /* turn "==" into "=" if needed before calling -syntax- */
  gettoken vn rest : 0, parse(" =")
  gettoken eq rest : rest, parse(" =")
  if "`eq'" == "==" {
    local 0 `vn' = `rest'
    }

  syntax varname [=/exp] [if] [in] [, EQVType(string) EQVLevel(real 1) /*
*/      UPPEReqvlevel(real 0) CContinuity Alpha(real 0.05) RELevance] 

* Validate eqvtype
  if lower("`eqvtype'") == "" {
    local eqvtype = "epsilon"
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

* Validate alpha
  if (`alpha' < 0 | `alpha' > 1) {
    noisily: di as err "option alpha() must be between 0 and 1 inclusive"
    exit 198
    }


  tempname tp tn v unv z adj0
  tempvar touse diff ranks t

  quietly {
    local PositivistConclusion = "Reject"
    local NegativistConclusion = "Reject"
    if "`relevance'" != "" {
      noi: di as txt "Relevance signed-rank test" 
      noi: signrank `varlist' = `exp' `if' `in'
      noi: di _newline
      }
    mark `touse' `if' `in'
    gen double `diff' = `varlist'-(`exp') if `touse'
    markout `touse' `diff'
    egen double `ranks' = rank(abs(`diff')) if `touse'
    
    /* We do want to OMIT the ranks corresponding to `diff'==0 in the sums.  */
    gen double `t' = sum(cond(`diff'>0,`ranks',0))
    scalar `tp' = `t'[_N]
    replace `t' = sum(cond(`diff'<0,`ranks',0))
    scalar `tn' = `t'[_N]
    replace `t' = sum(cond(`diff'~=0,`ranks'*`ranks',0))
    scalar `v' = `t'[_N]/4
    local se = 2*sqrt(`v')
    if (lower("`ccontinuity'") == "") {
      local cc = 0
      }
    if (lower("`ccontinuity'") != "") {
      local cc = 0.5
      }
    if (1 - normal(abs( (sign(`tp'-`tn')*(abs(`tp'-`tn') - `cc'))/`se' )) > `alpha'/2) {
      local PositivistConclusion = "Fail to reject"
      }
    if lower("`eqvtype'") == "delta" {
        local z1 = (`upper' - (sign(`tp'-`tn')*(abs(`tp'-`tn') - `cc')))/`se'
        local z2 = ((sign(`tp'-`tn')*(abs(`tp'-`tn') - `cc')) + `lower')/`se'
        }
    if lower("`eqvtype'") == "epsilon" {
        local z1 = `upper' - ( (sign(`tp'-`tn')*(abs(`tp'-`tn') - `cc'))/`se' )
        local z2 = ( (sign(`tp'-`tn')*(abs(`tp'-`tn') - `cc'))/`se' ) + `lower'
        }        
    local p1 = 1 - normal(`z1')
    local p2 = 1 - normal(`z2')
    if (`p1' > `alpha' | `p2' > `alpha') {
      local NegativistConclusion = "Fail to reject"
      }
    count if `touse'
    local n = r(N)
    scalar `unv' = `n'*(`n'+1)*(2*`n'+1)/24
    count if `diff' == 0 & `touse'
    local n0 = r(N)
    scalar `adj0' = -`n0'*(`n0'+1)*(2*`n0'+1)/24
    count if `diff' > 0 & `touse'
    local np = r(N)
    local nn = `n' - `np' - `n0'
      }

  di _newline as txt `"Signed-rank test for the distribution of paired or matched data being"' _newline `"equivalent to one that is symmetrical & centered on zero"' _newline
  di in smcl as txt `"        sign {c |}      obs   sum ranks    expected"'
  di in smcl as txt "{hline 13}{c +}{hline 33}"
  ditablin positive `np' `tp' (`tp'+`tn')/2
  ditablin negative `nn' `tn' (`tp'+`tn')/2
  ditablin zero     `n0' `n0'*(`n0'+1)/2 `n0'*(`n0'+1)/2 
  di in smcl as txt "{hline 13}{c +}{hline 33}"
  ditablin all `n' `n'*(`n'+1)/2 `n'*(`n'+1)/2 

  if `unv' < 1e7 { 
    local vfmt `"%10.2f"' 
    }
   else
    local vfmt `"%10.0g"'

  di in smcl as txt _newline `"unadjusted variance"' _col(22) as res `vfmt' `unv'
  di as txt `"adjustment for ties"' _col(22) as res `vfmt' `v'-`unv'-`adj0'
  di as txt `"adjustment for zeros"' _col(22) as res `vfmt' `adj0'
  di as txt _col(22) "{hline 10}"
  di as txt `"adjusted variance"' _col(22) as res `vfmt' `v' _newline

  if (lower("`eqvtype'") == "delta") {
    if (`upper' == `lower') {
      noisily: di as text "Delta (D) = " as res %-8.0f `lower' as res "Delta " as text "expressed in units of signed ranks (T)"
      }
    if (`upper' != `lower') {
      noisily: di as text "Delta (Dl) = " as res %-8.0f -1*`lower' as res " Dl " as text "expressed in units of signed ranks (T)"
      noisily: di as text "Delta (Du) =  " as res %-8.0f `upper' as res "Du " as text "expressed in units of signed ranks (T)"
      }
    local criticalvalue = `se'*invnormal(1-`alpha')
    if (`upper' == `lower' & `lower' <= `criticalvalue') {
      noisily: di _newline as res "Impossible to reject any Ho if Delta <= z-crit*s.e. ( " %-6.4g `criticalvalue' "). See{help tostsignrank##mineqvlevel: help tostsignrank}."
      }
    if (`upper' != `lower' & `lower' <= `criticalvalue') {
      noisily: di _newline as res "Impossible to reject any Ho if |Dl| <= z-crit*s.e. ( " %-6.4g `criticalvalue' "). See{help tostsignrank##mineqvlevel: help tostsignrank}."
      }
    if (`upper' != `lower' & `upper' <= `criticalvalue') {
      noisily: di _newline as res "Impossible to reject any Ho if Du <= z-crit*s.e. ( " %-6.4g `criticalvalue' "). See{help tostsignrank##mineqvlevel: help tostsignrank}."
      }
    if (`upper' == `lower') {
      noisily: di _newline as text "Ho: |T-E(T)| >= Delta:" _newline 
      if ("`ccontinuity'" != "") {
        noisily: di as text "Using continuity correction" _newline
        } 
      noisily: di as text "        z1 = " as res %-8.4g `z1' as text _col(38) "z2 = " as res %-8.4g `z2' _newline
      noisily: di as text "   Ho1: D-[T-E(T)] <= 0" _col(33) "Ho2: [T-E(T)]+D <= 0"
      noisily: di as text "   Ha1: D-[T-E(T)] > 0"  _col(33) "Ha2: [T-E(T)]+D > 0"
      noisily: di as text "   Pr(Z > z1) = " as res %6.4f `p1' _col(32) as text " Pr(Z > z2) = " as res %6.4f `p2'
      }
    if (`upper' != `lower') {
      noisily: di _newline as text "Ho: [T-E(T)] <= Dl, or [T-E(T)] >= Du:" _newline 
      if ("`ccontinuity'" != "") {
        noisily: di as text "Using continuity correction" _newline
        } 
      noisily: di as text "        z1 = " as res %-8.4g `z1' as text _col(38) "z2 = " as res %-8.4g `z2' _newline
      noisily: di as text "   Ho1: Du-[T-E(T)] <= 0       " _col(33) "Ho2: [T-E(T)]-Dl <= 0"
      noisily: di as text "   Ha1: Du-[T-E(T)] > 0" _col(33) "Ha2: [T-E(T)]-Dl > 0"
      noisily: di as text "   Pr(Z > z1) = " as res %6.4f `p1' _col(32) as text " Pr(Z > z2) = " as res %6.4f `p2'
      }
    }
   if lower("`eqvtype'") == "epsilon" {
    if (`upper' == `lower') {
      noisily: di as text "epsilon = " as res %-8.4f `lower' as text " " as res "epsilon" as text " expressed in units of the z distribution"
      }
    if (`upper' != `lower') {
      noisily: di as text "epsilon (el) = " as res %-8.4f -1*`lower' as text " " as res " el" as text " expressed in units of the z distribution"
      noisily: di as text "epsilon (eu) =  " as res %-8.4f `upper' as text " " as res "eu" as text " expressed in units of the z distribution"
      }
    local criticalvalue = invnormal(1-`alpha')
    if (`upper' == `lower' & `lower' <= `criticalvalue') {
      noisily: di _newline as res "Impossible to reject any Ho if epsilon <= z-crit (" %-5.3f `criticalvalue' "). See{help tostsignrank##mineqvlevel: help tostsignrank}."
      }
    if (`upper' != `lower' & `lower' <= `criticalvalue') {
      noisily: di _newline as res "Impossible to reject any Ho if |el| <= z-crit (" %-5.3f `criticalvalue' "). See{help tostsignrank##mineqvlevel: help tostsignrank}."
      }
    if (`upper' != `lower' & `upper' <= `criticalvalue') {
      noisily: di _newline as res "Impossible to reject any Ho if eu <= z-crit (" %-5.3f `criticalvalue' "). See{help tostsignrank##mineqvlevel: help tostsignrank}."
      }
    if (`upper' == `lower') {
      noisily: di _newline as text "Ho: |Z| >= epsilon:" _newline 
      if ("`ccontinuity'" != "") {
        noisily: di as text "Using continuity correction" _newline
        } 
      noisily: di as text "        z1 = " as res %-8.4g `z1' as text _col(38) "z2 = " as res %-8.4g `z2' _newline
      noisily: di as text "    Ho1: epsilon-Z <= 0       " _col(34) "Ho2: Z+epsilon <= 0"
      noisily: di as text "    Ha1: epsilon-Z > 0" _col(34) "Ha2: Z+epsilon > 0"
      noisily: di as text "    Pr(Z > t1) = " as res %6.4f `p1' _col(33) as text " Pr(Z > t2) = " as res %6.4f `p2'
      }
    if (`upper' != `lower') {
      noisily: di _newline as text "Ho: Z <= el, or Z >= eu:" _newline 
      if ("`ccontinuity'" != "") {
        noisily: di as text "Using continuity correction" _newline
        } 
      noisily: di as text "        z1 = " as res %-8.4g `z1' as text _col(38) "z2 = " as res %-8.4g `z2' _newline
      noisily: di as text "    Ho1: eu-Z <= 0       " _col(34) "Ho2: Z-el <= 0"
      noisily: di as text "    Ha1: eu-Z > 0" _col(34) "Ha2: Z-el > 0"
      noisily: di as text "    Pr(Z > t1) = " as res %6.4f `p1' _col(33) as text " Pr(Z > t2) = " as res %6.4f `p2'
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
  if ("`relevance'" != "") {
    return local relevance = "`RelevanceTestConclusion'"
    }
  if (`upper' != `lower') {
    if "`eqvtype'" == "delta" {
      return scalar Du   = `upper'
      return scalar Dl   = `lower'
      }
    if "`eqvtype'" == "epsilon" {
      return scalar eu   = `upper'
      return scalar el   = `lower'
      }
    }

  if (`upper' == `lower') {
    if "`eqvtype'" == "delta" {
      return scalar Delta   = `eqvlevel'
      }
    if "`eqvtype'" == "epsilon" {
      return scalar epsilon   = `eqvlevel'
      }
    }

  ret scalar p2 = `p2'
  ret scalar p1 = `p1'
  ret scalar z2 = `z2'
  ret scalar z1 = `z1'
  ret scalar Var_a = `v'
  ret scalar sum_newlineeg = `tn'
  ret scalar sum_pos = `tp'
  ret scalar N_tie = `n0'
  ret scalar N_pos = `np'
  ret scalar N_newlineeg = `nn'
  end 


program define tostsignrank14, rclass byable(recall)
  version 14.0, missing

  /* turn "==" into "=" if needed before calling -syntax- */
  gettoken vn rest : 0, parse(" =")
  gettoken eq rest : rest, parse(" =")
  if "`eq'" == "==" {
    local 0 `vn' = `rest'
    }

  syntax varname [=/exp] [if] [in] [, EQVType(string) EQVLevel(real 1) /*
*/      UPPEReqvlevel(real 0) CContinuity Alpha(real 0.05) RELevance] 


  * Create delta, and epsilon constants
  local delta = uchar(0916)
  local epsilon = uchar(0949)
  
  * Validate eqvtype
  if lower("`eqvtype'") == "" {
    local eqvtype = "epsilon"
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

  * Validate alpha
  if (`alpha' < 0 | `alpha' > 1) {
    noisily: di as err "option alpha() must be between 0 and 1 inclusive"
    exit 198
    }


  tempname tp tn v unv z adj0
  tempvar touse diff ranks t

  quietly {
    local PositivistConclusion = "Reject"
    local NegativistConclusion = "Reject"
    if "`relevance'" != "" {
      noi: di as txt "Relevance signed-rank test" 
      noi: signrank `varlist' = `exp' `if' `in'
      noi: di _newline
      }
    mark `touse' `if' `in'
    gen double `diff' = `varlist'-(`exp') if `touse'
    markout `touse' `diff'
    egen double `ranks' = rank(abs(`diff')) if `touse'
    
    /* We do want to OMIT the ranks corresponding to `diff'==0 in the sums.  */
    gen double `t' = sum(cond(`diff'>0,`ranks',0))
    scalar `tp' = `t'[_N]
    replace `t' = sum(cond(`diff'<0,`ranks',0))
    scalar `tn' = `t'[_N]
    replace `t' = sum(cond(`diff'~=0,`ranks'*`ranks',0))
    scalar `v' = `t'[_N]/4
    local se = 2*sqrt(`v')
    if (lower("`ccontinuity'") == "") {
      local cc = 0
      }
    if (lower("`ccontinuity'") != "") {
      local cc = 0.5
      }
    if (1 - normal(abs( (sign(`tp'-`tn')*(abs(`tp'-`tn') - `cc'))/`se' )) > `alpha'/2) {
      local PositivistConclusion = "Fail to reject"
      }
    if lower("`eqvtype'") == "delta" {
        local z1 = (`upper' - (sign(`tp'-`tn')*(abs(`tp'-`tn') - `cc')))/`se'
        local z2 = ((sign(`tp'-`tn')*(abs(`tp'-`tn') - `cc')) + `lower')/`se'
        }
    if lower("`eqvtype'") == "epsilon" {
        local z1 = `upper' - ( (sign(`tp'-`tn')*(abs(`tp'-`tn') - `cc'))/`se' )
        local z2 = ( (sign(`tp'-`tn')*(abs(`tp'-`tn') - `cc'))/`se' ) + `lower'
        }        
    local p1 = 1 - normal(`z1')
    local p2 = 1 - normal(`z2')
    if (`p1' > `alpha' | `p2' > `alpha') {
      local NegativistConclusion = "Fail to reject"
      }
    count if `touse'
    local n = r(N)
    scalar `unv' = `n'*(`n'+1)*(2*`n'+1)/24
    count if `diff' == 0 & `touse'
    local n0 = r(N)
    scalar `adj0' = -`n0'*(`n0'+1)*(2*`n0'+1)/24
    count if `diff' > 0 & `touse'
    local np = r(N)
    local nn = `n' - `np' - `n0'
      }

  di _newline as txt `"Signed-rank test for the distribution of paired or matched data being"' _newline `"equivalent to one that is symmetrical & centered on zero"' _newline
  di in smcl as txt `"        sign {c |}      obs   sum ranks    expected"'
  di in smcl as txt "{hline 13}{c +}{hline 33}"
  ditablin positive `np' `tp' (`tp'+`tn')/2
  ditablin negative `nn' `tn' (`tp'+`tn')/2
  ditablin zero     `n0' `n0'*(`n0'+1)/2 `n0'*(`n0'+1)/2 
  di in smcl as txt "{hline 13}{c +}{hline 33}"
  ditablin all `n' `n'*(`n'+1)/2 `n'*(`n'+1)/2 

  if `unv' < 1e7 { 
    local vfmt `"%10.2f"' 
    }
   else
    local vfmt `"%10.0g"'

  di in smcl as txt _newline `"unadjusted variance"' _col(22) as res `vfmt' `unv'
  di as txt `"adjustment for ties"' _col(22) as res `vfmt' `v'-`unv'-`adj0'
  di as txt `"adjustment for zeros"' _col(22) as res `vfmt' `adj0'
  di as txt _col(22) "{hline 10}"
  di as txt `"adjusted variance"' _col(22) as res `vfmt' `v' _newline

  if (lower("`eqvtype'") == "delta") {
    if (`upper' == `lower') {
      noisily: di as text "         `delta' = " as res %-8.0f `lower' as res "`delta' " as text "expressed in units of signed ranks (T)"
      }
    if (`upper' != `lower') {
      noisily: di as text "        `delta'l = " as res %-8.0f -1*`lower' as res "`delta'l " as text "expressed in units of signed ranks (T)"
      noisily: di as text "        `delta'u = " as res %-8.0f `upper' as res "`delta'u " as text "expressed in units of signed ranks (T)"
      }
    local criticalvalue = `se'*invnormal(1-`alpha')
    if (`upper' == `lower' & `lower' <= `criticalvalue') {
      noisily: di _newline as res "Impossible to reject any Ho if `delta' <= z-crit*s.e. ( " %-6.4g `criticalvalue' "). See{help tostsignrank##mineqvlevel: help tostsignrank}."
      }
    if (`upper' != `lower' & `lower' <= `criticalvalue') {
      noisily: di _newline as res "Impossible to reject any Ho if |`delta'l| <= z-crit*s.e. ( " %-6.4g `criticalvalue' "). See{help tostsignrank##mineqvlevel: help tostsignrank}."
      }
    if (`upper' != `lower' & `upper' <= `criticalvalue') {
      noisily: di _newline as res "Impossible to reject any Ho if `delta'u <= z-crit*s.e. ( " %-6.4g `criticalvalue' "). See{help tostsignrank##mineqvlevel: help tostsignrank}."
      }
    if (`upper' == `lower') {
      noisily: di _newline as text "Ho: |T-E(T)| >= `delta':" _newline 
      if ("`ccontinuity'" != "") {
        noisily: di as text "Using continuity correction" _newline
        } 
      noisily: di as text "        z1 = " as res %-8.4g `z1' as text _col(38) "z2 = " as res %-8.4g `z2' _newline
      noisily: di as text "   Ho1: `delta'-[T-E(T)] <= 0" _col(33) "Ho2: [T-E(T)]+`delta' <= 0"
      noisily: di as text "   Ha1: `delta'-[T-E(T)] > 0"  _col(33) "Ha2: [T-E(T)]+`delta' > 0"
      noisily: di as text "   Pr(Z > z1) = " as res %6.4f `p1' _col(32) as text " Pr(Z > z2) = " as res %6.4f `p2'
      }
    if (`upper' != `lower') {
      noisily: di _newline as text "Ho: [T-E(T)] <= `delta'l, or [T-E(T)] >= `delta'u:" _newline 
      if ("`ccontinuity'" != "") {
        noisily: di as text "Using continuity correction" _newline
        } 
      noisily: di as text "        z1 = " as res %-8.4g `z1' as text _col(38) "z2 = " as res %-8.4g `z2' _newline
      noisily: di as text "   Ho1: `delta'u-[T-E(T)] <= 0       " _col(33) "Ho2: [T-E(T)]-`delta'l <= 0"
      noisily: di as text "   Ha1: `delta'u-[T-E(T)] > 0" _col(33) "Ha2: [T-E(T)]-`delta'l > 0"
      noisily: di as text "   Pr(Z > z1) = " as res %6.4f `p1' _col(32) as text " Pr(Z > z2) = " as res %6.4f `p2'
      }
    }
   if lower("`eqvtype'") == "epsilon" {
    if (`upper' == `lower') {
      noisily: di as text "         `epsilon' = " as res %-8.4f `lower' as text " " as res "`epsilon'" as text " expressed in units of the z distribution"
      }
    if (`upper' != `lower') {
      noisily: di as text "        `epsilon'l = " as res %-8.4f -1*`lower' as text " " as res "`epsilon'l" as text " expressed in units of the z distribution"
      noisily: di as text "        `epsilon'u = " as res %-8.4f `upper' as text " " as res "`epsilon'u" as text " expressed in units of the z distribution"
      }
    local criticalvalue = invnormal(1-`alpha')
    if (`upper' == `lower' & `lower' <= `criticalvalue') {
      noisily: di _newline as res "Impossible to reject any Ho if `epsilon' <= z-crit (" %-5.3f `criticalvalue' "). See{help tostsignrank##mineqvlevel: help tostsignrank}."
      }
    if (`upper' != `lower' & `lower' <= `criticalvalue') {
      noisily: di _newline as res "Impossible to reject any Ho if |`epsilon'l| <= z-crit (" %-5.3f `criticalvalue' "). See{help tostsignrank##mineqvlevel: help tostsignrank}."
      }
    if (`upper' != `lower' & `upper' <= `criticalvalue') {
      noisily: di _newline as res "Impossible to reject any Ho if `epsilon'u <= z-crit (" %-5.3f `criticalvalue' "). See{help tostsignrank##mineqvlevel: help tostsignrank}."
      }
    if (`upper' == `lower') {
      noisily: di _newline as text "Ho: |Z| >= `epsilon':" _newline 
      if ("`ccontinuity'" != "") {
        noisily: di as text "Using continuity correction" _newline
        } 
      noisily: di as text "        z1 = " as res %-8.4g `z1' as text _col(38) "z2 = " as res %-8.4g `z2' _newline
      noisily: di as text "    Ho1: `epsilon'-Z <= 0       " _col(34) "Ho2: Z+`epsilon' <= 0"
      noisily: di as text "    Ha1: `epsilon'-Z > 0" _col(34) "Ha2: Z+`epsilon' > 0"
      noisily: di as text "    Pr(Z > t1) = " as res %6.4f `p1' _col(33) as text " Pr(Z > t2) = " as res %6.4f `p2'
      }
    if (`upper' != `lower') {
      noisily: di _newline as text "Ho: Z <= `epsilon'l, or Z >= `epsilon'u:" _newline 
      if ("`ccontinuity'" != "") {
        noisily: di as text "Using continuity correction" _newline
        } 
      noisily: di as text "        z1 = " as res %-8.4g `z1' as text _col(38) "z2 = " as res %-8.4g `z2' _newline
      noisily: di as text "    Ho1: `epsilon'u-Z <= 0       " _col(34) "Ho2: Z-`epsilon'l <= 0"
      noisily: di as text "    Ha1: `epsilon'u-Z > 0" _col(34) "Ha2: Z-`epsilon'l > 0"
      noisily: di as text "    Pr(Z > t1) = " as res %6.4f `p1' _col(33) as text " Pr(Z > t2) = " as res %6.4f `p2'
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
         noi: di _newline _newline as txt "Relevance test conclusion for " uchar(0945) " = " as res "0" `alpha' as txt ", and `delta' = " as res "0" `lower' as txt ":"
         }
        else {
         noi: di _newline _newline as txt "Relevance test conclusion for " uchar(0945) " = " as res "0" `alpha' as txt ", and `delta' = " as res `lower' as txt ":"
         }
       }
     if lower("`eqvtype'") == "epsilon" {
       noi: di _newline _newline as txt "Relevance test conclusion for " uchar(0945) " = " as res "0" `alpha' as txt ", and `epsilon' = " as res `lower' as txt ":"
       }
     }
   if (`upper' != `lower') {

     if lower("`eqvtype'") == "delta" {
       if (`lower' < 1 & `upper' < 1) {
         noi: di _newline _newline as txt "Relevance test conclusion for " uchar(0945) " = " as res "0" `alpha' as txt ", `delta'l = " as res "-0" `lower' as txt ", and `delta'u = " as res "0" `upper' as txt ":"
         }
       if (`lower' >= 1 & `upper' < 1) {
         noi: di _newline _newline as txt "Relevance test conclusion for " uchar(0945) " = " as res "0" `alpha' as txt ", `delta'l = " as res -1*`lower' as txt  ", and `delta'u = " as res "0" `upper' as txt ":"
         }
       if (`lower' < 1 & `upper' >= 1) {
         noi: di _newline _newline as txt "Relevance test conclusion for alpha = " as res "0" `alpha' as txt ", `delta'l = " as res "-0" `lower' as txt  ", and `delta'u = " as res `upper' as txt ":"
         }
       if (`lower' >= 1 & `upper' >= 1) {
         noi: di _newline _newline as txt "Relevance test conclusion for " uchar(0945) " = " as res "0" `alpha' as txt ", `delta'l = " as res -1*`lower' as txt  ", and `delta'u = " as res `upper' as txt ":"
         }
       }
     if lower("`eqvtype'") == "epsilon" {
       noi: di _newline _newline as txt "Relevance test conclusion for " uchar(0945) " = " as res "0" `alpha' as txt ", `epsilon'l = " as res -1*`lower' as txt  ", and `epsilon'u = " as res `upper' as txt ":"
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
  if ("`relevance'" != "") {
    return local relevance = "`RelevanceTestConclusion'"
    }
  if (`upper' != `lower') {
    if "`eqvtype'" == "delta" {
      return scalar Du   = `upper'
      return scalar Dl   = `lower'
      }
    if "`eqvtype'" == "epsilon" {
      return scalar eu   = `upper'
      return scalar el   = `lower'
      }
    }

  if (`upper' == `lower') {
    if "`eqvtype'" == "delta" {
      return scalar Delta   = `eqvlevel'
      }
    if "`eqvtype'" == "epsilon" {
      return scalar epsilon   = `eqvlevel'
      }
    }

  ret scalar p2 = `p2'
  ret scalar p1 = `p1'
  ret scalar z2 = `z2'
  ret scalar z1 = `z1'
  ret scalar Var_a = `v'
  ret scalar sum_newlineeg = `tn'
  ret scalar sum_pos = `tp'
  ret scalar N_tie = `n0'
  ret scalar N_pos = `np'
  ret scalar N_newlineeg = `nn'
  end 


program define ditablin
  di in smcl as txt %12s `"`1'"' `" {c |}"' as res _col(17) %7.0g `2' _col(26) %10.0g `3' _col(38) %10.0g `4'
  end
  
