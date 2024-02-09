*! version 3.1.3  15may2021 by alexis dot dinno at pdx dot edu
*! perform two one-sided tests for stochastic equivalence in paired data

* Syntax:  tostmcci #a #b #c #d [if exp] [in range] [fweight exp], [ 
*          eqvtype(type) eqvlevel(#) uppereqvlevel(#) yates edwards alpha(#) 
*          relevance]

program define tostmcci

  if int(_caller())<8 {
    di in r "tostmcci- does not support this version of Stata." _newline
    di as txt "Requests for a version compatible with versions of STATA earlier than v8 are "
    di as txt "untenable since I do not have access to the software." _newline 
    exit
  }
  if int(_caller())<14 {
    tostmcci8 `0'
    }
   else { 
    tostmcci14 `0'
    }
end

program tostmcci8, rclass
  version 8.0, missing

  gettoken a 0 : 0, parse(" ,")
  gettoken b 0 : 0, parse(" ,")
  gettoken c 0 : 0, parse(" ,")
  gettoken d 0 : 0, parse(" ,")

  confirm integer number `a'
  confirm integer number `b'
  confirm integer number `c'
  confirm integer number `d'

  if `a'<0 | `b'<0 | `c'<0 | `d'<0 { 
    di in red "negative numbers invalid"
    exit 498
  }

  syntax  [, EQVType(string) EQVLevel(real 1) UPPEReqvlevel(real 0) YAtes /*
*/        EDwards Alpha(real 0.05) RELevance]

* Validate eqvtype
  if lower("`eqvtype'") == "" {
    local eqvtype = "delta"
    }

  if !(lower("`eqvtype'") == "delta" | lower("`eqvtype'") == "epsilon") {
    noisily: di as err "option eqvtype() must be one of: delta, or epsilon"
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
  if ("`yates'" != "" & "`edwards'" != "") {
    noisily di as err "continuity correction options must be either yates or edwards, but not both"
    exit 198
    }

* Validate alpha
  if (`alpha' < 0 | `alpha' > 1) {
    noisily: di as err "option alpha() must be between 0 and 1 inclusive"
    exit 198
    }

  local PositivistConclusion = "Reject"
  local NegativistConclusion = "Reject"
  if ("`relevance'" != "") {
    di as txt _newline "Relevance test for paired binary data" _newline _newline
    local level = 100*(1 - `alpha')
    noi: di as txt "McNemar's test for difference in paired binary data"
    noi: mcci `a' `b' `c' `d', level(`level')
    di as txt _newline _newline "Test for equivalence in paired binary data"
    }
  if ("`relevance'" == "") {
    di as txt _newline "Test for equivalence in paired binary data"
    }
  di as txt "{hline 17}{c TT}{hline 24}{c TT}{hline 12}"

  _crc4fld `a' `b' `c' `d' /*
    */ Controls Cases Exposed Unexposed Exposed Unexposed
  di as txt "{hline 17}{c BT}{hline 24}{c BT}{hline 12}"
  local n = `a'+`b'+`c'+`d'
  local den = `b' + `c'
  local low = min(`b',`c')
  local diff = `b' - `c'
  local se = sqrt((`b'+`c') - (`n'*(((`b'/`n')-(`c'/`n'))^2)))
  
  local continuity = 0
  if ("`yates'" != "") {
    local continuity = 0.5
    }
  if ("`edwards'" != "") {
    local continuity = 1
    }
  if (1 - normal(abs( ((`diff') - `continuity') )/`se' ) > `alpha'/2) {
    local PositivistConclusion = "Fail to reject"
    }
  if (lower("`eqvtype'")=="delta") {
    local z1 = ( (`n'*`upper') - ((`diff') - `continuity') )/`se'
    local z2 = ( ((`diff') + `continuity') + (`n'*`lower') )/`se'
    }
  if (lower("`eqvtype'")=="epsilon") {
    local z1 = `upper' - (( (`diff') - `continuity' )/`se')
    local z2 = (( (`diff') + `continuity' )/`se') + `lower'
    }
  local p1 = 1 - normal(`z1')
  local p2 = 1 - normal(`z2')
  if (`p1' > `alpha' | `p2' > `alpha') {
    local NegativistConclusion = "Fail to reject"
    }

  if lower("`eqvtype'") == "delta" {
    noisily: di as txt "       diff = " as res `b' as txt " - " as res `c' as txt " = " `diff'   
    noisily: di as txt "  s.e. diff = " as res %-8.4f `se'    
    if (`upper' == `lower') {
      noisily: di as txt "      Delta = " as res %-8.4f `lower' 
      noisily: di as res "      Delta " as txt "expressed in units of probability"
      }
    if (`upper' != `lower') {
      noisily: di as txt " Delta (Dl) = " as res %-8.4f -1*`lower'
      noisily: di as txt " Delta (Du) = " as res %-8.4f `upper' 
      noisily: di as res  " Delta lower" as txt " and " as res "upper" as txt " expressed in units of probability"
      }
    local criticalvalue = (`se'*invnormal(1-`alpha'))/`n'
    if (`upper' == `lower' & `lower' <= `criticalvalue') {
      noisily: di _newline as res "Impossible to reject any Ho if Delta <= z-crit*s.e/n ( " %-6.4g `criticalvalue' "). See{help tostmcc##mineqvlevel: help tostmcc}."
      }
    if (`upper' != `lower' & `lower' <= `criticalvalue') {
      noisily: di _newline as res "Impossible to reject any Ho if |Dl| <= t-crit*s.e/n ( " %-6.4g `criticalvalue' "). See{help tostmcc##mineqvlevel: help tostmcc}."
      }
    if (`upper' != `lower' & `upper' <= `criticalvalue') {
      noisily: di _newline as res "Impossible to reject any Ho if Du <= t-crit*s.e/n ( " %-6.4g `criticalvalue' "). See{help tostmcc##mineqvlevel: help tostmcc}."
      }
    if (`upper' == `lower') {
      noisily: di _newline as txt "Ho: |diff| >= Delta:" _newline 
      if ("`yates'" != "") {
        noisily: di as txt "Using the Yates continuity correction" _newline
        } 
      if ("`edwards'" != "") {
        noisily: di as txt "Using the Edwards continuity correction" _newline
        } 
      noisily: di as txt "        z1 = " as res %-8.4g `z1' as txt _col(38) "z2 = " as res %-8.4g `z2' _newline
      noisily: di as txt "   Ho1: Delta-diff <= 0" _col(33) "Ho2: diff+Delta <= 0"
      noisily: di as txt "   Ha1: Delta-diff > 0"  _col(33) "Ha2: diff+Delta > 0"
      noisily: di as txt "   Pr(Z > z1) = " as res %6.4f `p1' _col(32) as txt " Pr(Z > z2) = " as res %6.4f `p2'
      }
    if (`upper' != `lower') {
      noisily: di _newline as txt "Ho: diff <= Dl, or diff >= Du:" _newline 
      if ("`yates'" != "") {
        noisily: di as txt "Using the Yates continuity correction" _newline
        } 
      if ("`edwards'" != "") {
        noisily: di as txt "Using the Edwards continuity correction" _newline
        } 
      noisily: di as txt "        z1 = " as res %-8.4g `z1' as txt _col(38) "z2 = " as res %-8.4g `z2' _newline
      noisily: di as txt "   Ho1: Du-diff <= 0" _col(33) "Ho2: diff-Dl <= 0"
      noisily: di as txt "   Ha1: Du-diff > 0"  _col(33) "Ha2: diff-Dl > 0"
      noisily: di as txt "   Pr(Z > z1) = " as res %6.4f `p1' _col(32) as txt " Pr(Z > z2) = " as res %6.4f `p2'
      }
    }
  if lower("`eqvtype'") == "epsilon" {
    noisily: di as txt "        diff = " as res `b' as txt " - " as res `c' as txt " = " as res `diff'    
    noisily: di as txt "   s.e. diff = " as res %-8.4f `se'    
    if (`upper' == `lower') {
      noisily: di as txt "     epsilon = " as res %-8.4f `lower' 
      noisily: di as res "     epsilon " as txt "expressed in units of the z distribution"
      }
    if (`upper' != `lower') {
      noisily: di as txt "epsilon (el) = " as res %-8.4f -1*`lower'
      noisily: di as txt "epsilon (eu) = " as res %-8.4f `upper'
      noisily: di as res "epsilon lower" as txt " and " as res "upper" as txt " expressed in units of the z distribution"
      }
    local criticalvalue = invnormal(1-`alpha')
    if (`upper' == `lower' & `lower' <= `criticalvalue') {
      noisily: di _newline as res "Impossible to reject any Ho if epsilon <= z-crit (" %-5.3f `criticalvalue' "). See{help tostmcc##mineqvlevel: help tostmcc}."
      }
    if (`upper' != `lower' & `lower' <= `criticalvalue') {
      noisily: di _newline as res "Impossible to reject any Ho if |el| <= z-crit (" %-5.3f `criticalvalue' "). See{help tostmcc##mineqvlevel: help tostmcc}."
      }
    if (`upper' != `lower' & `upper' <= `criticalvalue') {
      noisily: di _newline as res "Impossible to reject any Ho if eu <= z-crit (" %-5.3f `criticalvalue' "). See{help tostmcc##mineqvlevel: help tostmcc}."
      }
    if (`upper' == `lower') {
      noisily: di _newline as txt "Ho: |Z| >= epsilon:" _newline 
      if ("`yates'" != "") {
        noisily: di as txt "Using the Yates continuity correction" _newline
        } 
      if ("`edwards'" != "") {
        noisily: di as txt "Using the Edwards continuity correction" _newline
        } 
      noisily: di as txt "        z1 = " as res %-8.4g `z1' as txt _col(38) "z2 = " as res %-8.4g `z2' _newline
      noisily: di as txt "   Ho1: epsilon-Z <= 0" _col(33) "Ho2: Z+epsilon <= 0"
      noisily: di as txt "   Ha1: epsilon-Z > 0"  _col(33) "Ha2: Z+epsilon > 0"
      noisily: di as txt "   Pr(Z > z1) = " as res %6.4f `p1' _col(32) as txt " Pr(Z > z2) = " as res %6.4f `p2'
      }
    if (`upper' != `lower') {
      noisily: di _newline as txt "Ho: Z <= el, or Z >= eu:" _newline 
      if ("`yates'" != "") {
        noisily: di as txt "Using the Yates continuity correction" _newline
        } 
      if ("`edwards'" != "") {
        noisily: di as txt "Using the Edwards continuity correction" _newline
        } 
      noisily: di as txt "        z1 = " as res %-8.4g `z1' as txt _col(38) "z2 = " as res %-8.4g `z2' _newline
      noisily: di as txt "   Ho1: eu-Z <= 0" _col(33) "Ho2: Z-el <= 0"
      noisily: di as txt "   Ha1: eu-Z > 0"  _col(33) "Ha2: Z-el > 0"
      noisily: di as txt "   Pr(Z > z1) = " as res %6.4f `p1' _col(32) as txt " Pr(Z > z2) = " as res %6.4f `p2'
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
         noi: di _newline _newline as txt "Relevance test conclusion for alpha = " as res "0" `alpha' as txt ", Delta (Dl) = " as res "0" `lower' as txt ", and Delta (Du) = " as res "0" `upper' as txt ":"
         }
       if (`lower' >= 1 & `upper' < 1) {
         noi: di _newline _newline as txt "Relevance test conclusion for alpha = " as res "0" `alpha' as txt ", Delta (Dl) = " as res `lower' as txt  ", and Delta (Du) = " as res "0" `upper' as txt ":"
         }
       if (`lower' < 1 & `upper' >= 1) {
         noi: di _newline _newline as txt "Relevance test conclusion for alpha = " as res "0" `alpha' as txt ", Delta (Dl) = " as res "0" `lower' as txt  ", and Delta (Du) = " as res `upper' as txt ":"
         }
       if (`lower' >= 1 & `upper' >= 1) {
         noi: di _newline _newline as txt "Relevance test conclusion for alpha = " as res "0" `alpha' as txt ", Delta (Dl) = " as res `lower' as txt  ", and Delta (Du) = " as res `upper' as txt ":"
         }
       }
     if lower("`eqvtype'") == "epsilon" {
       noi: di _newline _newline as txt "Relevance test conclusion for alpha = " as res "0" `alpha' as txt ", epsilon (el) = " as res `lower' as txt  ", and epsilon (eu) = " as res `upper' as txt ":"
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
  return scalar  D_f  = (`b'/`n') - (`c'/`n')
  return scalar  p2   = `p2'
  return scalar  p1   = `p1' 
  return scalar  z2   = `z2'
  return scalar  z1   = `z1' 
  end


program tostmcci14, rclass
  version 14.0, missing

  gettoken a 0 : 0, parse(" ,")
  gettoken b 0 : 0, parse(" ,")
  gettoken c 0 : 0, parse(" ,")
  gettoken d 0 : 0, parse(" ,")

  confirm integer number `a'
  confirm integer number `b'
  confirm integer number `c'
  confirm integer number `d'

  if `a'<0 | `b'<0 | `c'<0 | `d'<0 { 
    di in red "negative numbers invalid"
    exit 498
  }

  syntax  [, EQVType(string) EQVLevel(real 1) UPPEReqvlevel(real 0) YAtes /*
*/        EDwards Alpha(real 0.05) RELevance]

  * Create theta, delta, and epsilon constants
  local theta = uchar(0952)
  local delta = uchar(0916)	
  local epsilon = uchar(0949)
  
  * Validate eqvtype
  if lower("`eqvtype'") == "" {
    local eqvtype = "delta"
    }

  if !(lower("`eqvtype'") == "delta" | lower("`eqvtype'") == "epsilon") {
    noisily: di as err "option eqvtype() must be one of: delta, or epsilon"
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
  if ("`yates'" != "" & "`edwards'" != "") {
    noisily di as err "continuity correction options must be either yates or edwards, but not both"
    exit 198
    }

  * Validate alpha
  if (`alpha' < 0 | `alpha' > 1) {
    noisily: di as err "option alpha() must be between 0 and 1 inclusive"
    exit 198
    }

  local PositivistConclusion = "Reject"
  local NegativistConclusion = "Reject"
  if ("`relevance'" != "") {
    di as txt _newline "Relevance test for paired binary data" _newline _newline
    local level = 100*(1 - `alpha')
    noi: di as txt "McNemar's test for difference in paired binary data"
    noi: mcci `a' `b' `c' `d', level(`level')
    di as txt _newline _newline "Test for equivalence in paired binary data"
    }
  if ("`relevance'" == "") {
    di as txt _newline "Test for equivalence in paired binary data"
    }
  di as txt "{hline 17}{c TT}{hline 24}{c TT}{hline 12}"

  _crc4fld `a' `b' `c' `d' /*
    */ Controls Cases Exposed Unexposed Exposed Unexposed
  di as txt "{hline 17}{c BT}{hline 24}{c BT}{hline 12}"
  local n = `a'+`b'+`c'+`d'
  local den = `b' + `c'
  local low = min(`b',`c')
  local diff = `b' - `c'
  local se = sqrt((`b'+`c') - (`n'*(((`b'/`n')-(`c'/`n'))^2)))
  
  local continuity = 0
  if ("`yates'" != "") {
    local continuity = 0.5
    }
  if ("`edwards'" != "") {
    local continuity = 1
    }
  if (1 - normal(abs( ((`diff') - `continuity') )/`se' ) > `alpha'/2) {
    local PositivistConclusion = "Fail to reject"
    }
  if (lower("`eqvtype'")=="delta") {
    local z1 = ( (`n'*`upper') - ((`diff') - `continuity') )/`se'
    local z2 = ( ((`diff') + `continuity') + (`n'*`lower') )/`se'
    }
  if (lower("`eqvtype'")=="epsilon") {
    local z1 = `upper' - (( (`diff') - `continuity' )/`se')
    local z2 = (( (`diff') + `continuity' )/`se') + `lower'
    }
  local p1 = 1 - normal(`z1')
  local p2 = 1 - normal(`z2')
  if (`p1' > `alpha' | `p2' > `alpha') {
    local NegativistConclusion = "Fail to reject"
    }

  if lower("`eqvtype'") == "delta" {
    noisily: di as txt "         `theta' = " as res `b' as txt " - " as res `c' as txt " = " `diff'   
    noisily: di as txt "    s.e. `theta' = " as res %-8.4f `se'    
    if (`upper' == `lower') {
      noisily: di as txt "         `delta' = " as res %-8.4f `lower' "`delta' " as txt "expressed in units of probability"
      }
    if (`upper' != `lower') {
      noisily: di as txt "        `delta'l = " as res %-8.4f -1*`lower' "`delta'l" as txt " expressed in units of probability"
      noisily: di as txt "        `delta'u = " as res %-8.4f `upper' "`delta'u" as txt " expressed in units of probability"
      }
    local criticalvalue = (`se'*invnormal(1-`alpha'))/`n'
    if (`upper' == `lower' & `lower' <= `criticalvalue') {
      noisily: di _newline as res "Impossible to reject any Ho if `delta' <= z-crit*s.e/n ( " %-6.4g `criticalvalue' "). See{help tostmcc##mineqvlevel: help tostmcc}."
      }
    if (`upper' != `lower' & `lower' <= `criticalvalue') {
      noisily: di _newline as res "Impossible to reject any Ho if |`delta'l| <= t-crit*s.e/n ( " %-6.4g `criticalvalue' "). See{help tostmcc##mineqvlevel: help tostmcc}."
      }
    if (`upper' != `lower' & `upper' <= `criticalvalue') {
      noisily: di _newline as res "Impossible to reject any Ho if `delta'u <= t-crit*s.e/n ( " %-6.4g `criticalvalue' "). See{help tostmcc##mineqvlevel: help tostmcc}."
      }
    if (`upper' == `lower') {
      noisily: di _newline as txt "Ho: |`theta'| >= `delta':" _newline 
      if ("`yates'" != "") {
        noisily: di as txt "Using the Yates continuity correction" _newline
        } 
      if ("`edwards'" != "") {
        noisily: di as txt "Using the Edwards continuity correction" _newline
        } 
      noisily: di as txt "        z1 = " as res %-8.4g `z1' as txt _col(38) "z2 = " as res %-8.4g `z2' _newline
      noisily: di as txt "   Ho1: `delta'-`theta' <= 0" _col(33) "Ho2: `theta'+`delta' <= 0"
      noisily: di as txt "   Ha1: `delta'-`theta' > 0"  _col(33) "Ha2: `theta'+`delta' > 0"
      noisily: di as txt "   Pr(Z > z1) = " as res %6.4f `p1' _col(32) as txt " Pr(Z > z2) = " as res %6.4f `p2'
      }
    if (`upper' != `lower') {
      noisily: di _newline as txt "Ho: `theta' <= `delta'l, or `theta' >= `delta'u:" _newline 
      if ("`yates'" != "") {
        noisily: di as txt "Using the Yates continuity correction" _newline
        } 
      if ("`edwards'" != "") {
        noisily: di as txt "Using the Edwards continuity correction" _newline
        } 
      noisily: di as txt "        z1 = " as res %-8.4g `z1' as txt _col(38) "z2 = " as res %-8.4g `z2' _newline
      noisily: di as txt "   Ho1: `delta'u-`theta' <= 0" _col(33) "Ho2: `theta'-`delta'l <= 0"
      noisily: di as txt "   Ha1: `delta'u-`theta' > 0"  _col(33) "Ha2: `theta'-`delta'l > 0"
      noisily: di as txt "   Pr(Z > z1) = " as res %6.4f `p1' _col(32) as txt " Pr(Z > z2) = " as res %6.4f `p2'
      }
    }
  if lower("`eqvtype'") == "epsilon" {
    noisily: di as txt "         `theta' = " as res `b' as txt " - " as res `c' as txt " = " as res `diff'    
    noisily: di as txt "    s.e. `theta' = " as res %-8.4f `se'    
    if (`upper' == `lower') {
      noisily: di as txt "         `epsilon' = " as res %-8.4f `lower' "`epsilon' " as txt "expressed in units of the z distribution"
      }
    if (`upper' != `lower') {
      noisily: di as txt "        `epsilon'l = " as res %-8.4f -1*`lower' "`epsilon'l" as txt " expressed in units of the z distribution"
      noisily: di as txt "        `epsilon'u = " as res %-8.4f `upper' "`epsilon'u" as txt " expressed in units of the z distribution"
      }
    local criticalvalue = invnormal(1-`alpha')
    if (`upper' == `lower' & `lower' <= `criticalvalue') {
      noisily: di _newline as res "Impossible to reject any Ho if `epsilon' <= z-crit (" %-5.3f `criticalvalue' "). See{help tostmcc##mineqvlevel: help tostmcc}."
      }
    if (`upper' != `lower' & `lower' <= `criticalvalue') {
      noisily: di _newline as res "Impossible to reject any Ho if |`epsilon'l| <= z-crit (" %-5.3f `criticalvalue' "). See{help tostmcc##mineqvlevel: help tostmcc}."
      }
    if (`upper' != `lower' & `upper' <= `criticalvalue') {
      noisily: di _newline as res "Impossible to reject any Ho if `epsilon'u <= z-crit (" %-5.3f `criticalvalue' "). See{help tostmcc##mineqvlevel: help tostmcc}."
      }
    if (`upper' == `lower') {
      noisily: di _newline as txt "Ho: |Z| >= `epsilon':" _newline 
      if ("`yates'" != "") {
        noisily: di as txt "Using the Yates continuity correction" _newline
        } 
      if ("`edwards'" != "") {
        noisily: di as txt "Using the Edwards continuity correction" _newline
        } 
      noisily: di as txt "        z1 = " as res %-8.4g `z1' as txt _col(38) "z2 = " as res %-8.4g `z2' _newline
      noisily: di as txt "   Ho1: `epsilon'-Z <= 0" _col(33) "Ho2: Z+`epsilon' <= 0"
      noisily: di as txt "   Ha1: `epsilon'-Z > 0"  _col(33) "Ha2: Z+`epsilon' > 0"
      noisily: di as txt "   Pr(Z > z1) = " as res %6.4f `p1' _col(32) as txt " Pr(Z > z2) = " as res %6.4f `p2'
      }
    if (`upper' != `lower') {
      noisily: di _newline as txt "Ho: Z <= `epsilon'l, or Z >= `epsilon'u:" _newline 
      if ("`yates'" != "") {
        noisily: di as txt "Using the Yates continuity correction" _newline
        } 
      if ("`edwards'" != "") {
        noisily: di as txt "Using the Edwards continuity correction" _newline
        } 
      noisily: di as txt "        z1 = " as res %-8.4g `z1' as txt _col(38) "z2 = " as res %-8.4g `z2' _newline
      noisily: di as txt "   Ho1: `epsilon'u-Z <= 0" _col(33) "Ho2: Z-`epsilon'l <= 0"
      noisily: di as txt "   Ha1: `epsilon'u-Z > 0"  _col(33) "Ha2: Z-`epsilon'l > 0"
      noisily: di as txt "   Pr(Z > z1) = " as res %6.4f `p1' _col(32) as txt " Pr(Z > z2) = " as res %6.4f `p2'
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
         noi: di _newline _newline as txt "Relevance test conclusion for " uchar(0945) " = " as res "0" `alpha' as txt ", `delta'l = " as res "-0" `lower' as txt  ", and `delta'u = " as res `upper' as txt ":"
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
  return scalar  D_f  = (`b'/`n') - (`c'/`n')
  return scalar  p2   = `p2'
  return scalar  p1   = `p1' 
  return scalar  z2   = `z2'
  return scalar  z1   = `z1' 
  end
