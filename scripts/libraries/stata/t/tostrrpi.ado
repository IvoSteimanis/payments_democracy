*! version 3.1.3  15may2021 by alexis dot dinno at pdx dot edu
*! perform two one-sided tests for equivalence of RR and unity in paired binary data

* Syntax:  tostrrpi #a #b #c #n #delta0, [ deltaupper(#) alpha(#) relevance 
* treatment1(string) treatment2(string) outcome(string) nooutcome(string)]

program define tostrrpi

  if int(_caller())<8 {
    di in r "tostrrpi- does not support this version of Stata." _newline
    di as txt "Requests for a version compatible with versions of STATA earlier than v8 are "
    di as txt "untenable since I do not have access to the software." _newline 
    exit
  }
  if int(_caller())<14 {
    tostrrpi8 `0'
    }
   else { 
    tostrrpi14 `0'
    } 
end

program tostrrpi8, rclass
  version 8, missing

  gettoken a 0 : 0, parse(" ,")
  gettoken b 0 : 0, parse(" ,")
  gettoken c 0 : 0, parse(" ,")
  gettoken n 0 : 0, parse(" ,")
  gettoken delta0 0 : 0, parse(" ,")

  confirm integer number `a'
  confirm integer number `b'
  confirm integer number `c'
  confirm integer number `n'
  confirm number `delta0'
  local d = `n' - `a' - `b' - `c'

  syntax [, DELTAUpper(real 0) Alpha(real 0.05) RELevance treatment1(string) /*
  */      treatment2(string) OUTcome(string) NOOutcome(string)]


* Validate delta0
  if (`delta0' < 0 | `delta0' > 1 ) {
    noisily: di as err "delta0 must be a positive real value that is less than or equal to 1"
    exit 198
    }

* Validate deltaupper
  if (`deltaupper' < 1 & `deltaupper' !=0) {
    noisily: di as err "deltaupper must be a positive real value that is greater than or equal to 1"
    exit 198
    }
   else {
    local upper = 1/`deltaupper'
	}
  if `deltaupper' == 0 {
    local upper = `delta0'
    }
  local lower = `delta0'

* Validate alpha
  if (`alpha' < 0 | `alpha' > 1) {
    noisily: di as err "option alpha() must be between 0 and 1 inclusive"
    exit 198
    }
   
* Set up treatment and outcome status names, as user requests
  if "`treatment1'"=="" {
    local treatname1 = "Treatment 1"
	}
   else {
    local treatname1 = "`treatment1'"
	}
  local treatname1 = substr("`treatname1'",1,22)

  if "`treatment2'"=="" {
    local treatname2 = "Treatment 2"
	}
   else {
    local treatname2 = "`treatment2'"
	}
  local treatname2 = substr("`treatname2'",1,16)

  if "`outcome'" =="" {
    local positive = "Positive"
    local rowpositive = "Positive"
    local colpositive = "Positive"
	}
   else {
    local positive = "`outcome'"
    local rowpositive = substr("`outcome'",1,16)
    local colpositive = substr("`outcome'",1,9)
	}
  if "`nooutcome'" =="" {
    local negative = "Negative"
    local rownegative = "Negative"
    local colnegative = "Negative"
	}
   else {
    local rownegative = substr("`nooutcome'",1,16)
    local colnegative = substr("`nooutcome'",1,9)
	}
 

*******************************************************************************
* The business starts here                                                    *
*******************************************************************************

**********
* conduct McNemar's test of proportion difference (because if OR != 1, RR != 1)
 local PositivistConclusion = "Reject"
 local NegativistConclusion = "Reject"
 if ("`relevance'" != "") {
   di as txt _newline "Relevance test for relative risk and unity in paired designs" _newline _newline
   local level = 100*(1 - `alpha')
   noi: di as txt "McNemar's test for difference in paired binary data"
   noi: di as txt "{hline 17}{c TT}{hline 24}{c TT}{hline 12}" _continue
   noi: mcci `a' `b' `c' `d', level(`level')
   local p = r(p_exact)
   if (`p' > `alpha'/2) {
     local PositivistConclusion = "Fail to reject"
     }
   di as txt _newline _newline "Test for equivalence of relative risk and unity in paired designs"
   }
 if ("`relevance'" == "") {
   di as txt _newline "Test for equivalence of relative risk and unity in paired designs"
   }
 di as txt "{hline 17}{c TT}{hline 24}{c TT}{hline 12}"

  
  if `a' == 0 & `b' == 0 & `c' == 0 {
    local RR = .
	local sdRR = .
    local z1 = .
	local z2 = .
	noi: di as res _newline "Note: neither treatment has any observed responses; relative risk undefined"
	}
   else if `a'> 0 & `b' == 0 & `c' == 0 {
    local RR = 1
	local riskprefix = ""
	local sdRR = sqrt((`a'+`b')*(`b'+`c')/(`a'+`c')^3)
    local z1 = sqrt(`a'*(1-`lower')/`lower')
    local z2 = sqrt(`a'*(1-`upper')/`upper')
    noi: di as res _newline "Note: treatments have complete concordance"
    }
   else {
    local RR = (`a'+`b')/(`a'+`c')
	local riskprefix = "0"
	local sdRR = sqrt((`a'+`b')*(`b'+`c')/(`a'+`c')^3)
   _TangTangChanZ `a' `b' `c' `n' `lower'
	local z1 = r(TTC)
    _TangTangChanZ `a' `c' `b' `n' `upper'
	local z2 = r(TTC)
    }

  local p1 = 1 - normal(`z1')
  local p2 = 1 - normal(`z2')	
  if (`p1' > `alpha' | `p2' > `alpha') {
    local NegativistConclusion = "Fail to reject"
    }
  _crc4fld `a' `b' `c' `d' "`treatname1'" "`treatname2'" "`colpositive'" "`colnegative'" "`rowpositive'" "`rownegative'"
  di as txt "{hline 17}{c BT}{hline 24}{c BT}{hline 12}"

  noisily: di as text "Relative risk of " as res "`positive'" as text " in " as res "`treatname2'" as text " vs. " as res "`treatname1'" as text ":" 
  noisily: di as text "         RR = " as res `a'+`b' as text " / " as res `a'+`c' as text " = "  as res "`riskprefix'" %-8.4g `RR'
  noisily: di as text "    s.e. RR = " as res "0" %-8.4g `sdRR'
  noisily: di as text "Delta l (Dl) = " as res  %-8.4f `lower'
  noisily: di as text "Delta u (Du) = " as res  %-8.4f 1/`upper'

  noisily: di _newline as txt "Ho: true RR <= dl, or true RR >= du:" _newline 
  noisily: di as txt "        z1 = " as res %-8.4g `z1' as txt _col(38) "z2 = " as res %-8.4g `z2' _newline
  noisily: di as txt "   Ho1: true RR <= Dl" _col(33) "Ho2: true RR >= Du"
  noisily: di as txt "   Ha1: true RR > Dl"  _col(33) "Ha2: true RR < Du"
  noisily: di as txt "   Pr(Z > z1) = " as res %6.4f `p1' _col(32) as txt " Pr(Z > z2) = " as res %6.4f `p2'


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
	 
   * Format delta0 to remove trailing zeros
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
     noi: di _newline _newline as txt "Relevance test conclusion for alpha = " as res "0" `alpha' as txt ", Delta =" as res %5.2f `lower' as txt ", and 1/Delta =" as res %5.2f 1/`lower' as txt ":"
     }
   if (`upper' != `lower') {
     noi: di _newline _newline as txt "Relevance test conclusion for alpha = " as res "0" `alpha' as txt ", Delta l = " as res "0" `lower' as txt  ", and Delta u = " as res 1/`upper' as txt ":"
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
  if (`upper' == `lower') {
    return scalar delta0   = `lower'
    }
  if (`upper' != `lower') {
    return scalar deltaupper   = 1/`upper'
    return scalar deltalower   = `lower'
    }
  return scalar   p2   = `p2'
  return scalar   p1   = `p1' 
  return scalar   z2   = `z2'
  return scalar   z1   = `z1' 
  return scalar sdRR   = `sdRR'
  return scalar   RR   = `RR'
	
  end


program tostrrpi14, rclass
  version 14.0, missing

  gettoken a 0 : 0, parse(" ,")
  gettoken b 0 : 0, parse(" ,")
  gettoken c 0 : 0, parse(" ,")
  gettoken n 0 : 0, parse(" ,")
  gettoken delta0 0 : 0, parse(" ,")

  confirm integer number `a'
  confirm integer number `b'
  confirm integer number `c'
  confirm integer number `n'
  confirm number `delta0'
  local d = `n' - `a' - `b' - `c'

  syntax [, DELTAUpper(real 0) Alpha(real 0.05) RELevance treatment1(string) /*
  */      treatment2(string) OUTcome(string) NOOutcome(string)]

  * Create delta constant
  local delta = uchar(0916)

* Validate delta0
  if (`delta0' < 0 | `delta0' > 1 ) {
    noisily: di as err "delta0 must be a positive real value that is less than or equal to 1"
    exit 198
    }

* Validate deltaupper
  if (`deltaupper' < 1 & `deltaupper' !=0) {
    noisily: di as err "deltaupper must be a positive real value that is greater than or equal to 1"
    exit 198
    }
   else {
    local upper = 1/`deltaupper'
	}
  if `deltaupper' == 0 {
    local upper = `delta0'
    }
  local lower = `delta0'

* Validate alpha
  if (`alpha' < 0 | `alpha' > 1) {
    noisily: di as err "option alpha() must be between 0 and 1 inclusive"
    exit 198
    }
   
* Set up treatment and outcome status names, as user requests
  if "`treatment1'"=="" {
    local treatname1 = "Treatment 1"
	}
   else {
    local treatname1 = "`treatment1'"
	}
  local treatname1 = substr("`treatname1'",1,22)

  if "`treatment2'"=="" {
    local treatname2 = "Treatment 2"
	}
   else {
    local treatname2 = "`treatment2'"
	}
  local treatname2 = substr("`treatname2'",1,16)

  if "`outcome'" =="" {
    local positive = "Positive"
    local rowpositive = "Positive"
    local colpositive = "Positive"
	}
   else {
    local positive = "`outcome'"
    local rowpositive = substr("`outcome'",1,16)
    local colpositive = substr("`outcome'",1,9)
	}
  if "`nooutcome'" =="" {
    local negative = "Negative"
    local rownegative = "Negative"
    local colnegative = "Negative"
	}
   else {
    local rownegative = substr("`nooutcome'",1,16)
    local colnegative = substr("`nooutcome'",1,9)
	}
 

*******************************************************************************
* The business starts here                                                    *
*******************************************************************************

**********
* conduct McNemar's test of proportion difference (because if OR != 1, RR != 1)
 local PositivistConclusion = "Reject"
 local NegativistConclusion = "Reject"
 if ("`relevance'" != "") {
   di as txt _newline "Relevance test for relative risk and unity in paired designs" _newline _newline
   local level = 100*(1 - `alpha')
   noi: di as txt "McNemar's test for difference in paired binary data"
   noi: di as txt "{hline 17}{c TT}{hline 24}{c TT}{hline 12}" _continue
   noi: mcci `a' `b' `c' `d', level(`level')
   local p = r(p_exact)
   if (`p' > `alpha'/2) {
     local PositivistConclusion = "Fail to reject"
     }
   di as txt _newline _newline "Test for equivalence of relative risk and unity in paired designs"
   }
 if ("`relevance'" == "") {
   di as txt _newline "Test for equivalence of relative risk and unity in paired designs"
   }
 di as txt "{hline 17}{c TT}{hline 24}{c TT}{hline 12}"

  
  if `a' == 0 & `b' == 0 & `c' == 0 {
    local RR = .
	local sdRR = .
    local z1 = .
	local z2 = .
	noi: di as res _newline "Note: neither treatment has any observed responses; relative risk undefined"
	}
   else if `a'> 0 & `b' == 0 & `c' == 0 {
    local RR = 1
	local riskprefix = ""
	local sdRR = sqrt((`a'+`b')*(`b'+`c')/(`a'+`c')^3)
    local z1 = sqrt(`a'*(1-`lower')/`lower')
    local z2 = sqrt(`a'*(1-`upper')/`upper')
    noi: di as res _newline "Note: treatments have complete concordance"
    }
   else {
    local RR = (`a'+`b')/(`a'+`c')
	local riskprefix = "0"
	local sdRR = sqrt((`a'+`b')*(`b'+`c')/(`a'+`c')^3)
   _TangTangChanZ `a' `b' `c' `n' `lower'
	local z1 = r(TTC)
    _TangTangChanZ `a' `c' `b' `n' `upper'
	local z2 = r(TTC)
    }

  local p1 = 1 - normal(`z1')
  local p2 = 1 - normal(`z2')	
  if (`p1' > `alpha' | `p2' > `alpha') {
    local NegativistConclusion = "Fail to reject"
    }
  _crc4fld `a' `b' `c' `d' "`treatname1'" "`treatname2'" "`colpositive'" "`colnegative'" "`rowpositive'" "`rownegative'"
  di as txt "{hline 17}{c BT}{hline 24}{c BT}{hline 12}"

  noisily: di as text "Relative risk of " as res "`positive'" as text " in " as res "`treatname2'" as text " vs. " as res "`treatname1'" as text ":" 
  noisily: di as text "        RR = " as res `a'+`b' as text " / " as res `a'+`c' as text " = "  as res "`riskprefix'" %-8.4g `RR'
  noisily: di as text "   s.e. RR = " as res "0" %-8.4g `sdRR'
  noisily: di as text "        `delta'l = " as res  %-8.4f `lower'
  noisily: di as text "        `delta'u = " as res  %-8.4f 1/`upper'

  noisily: di _newline as txt "Ho: true RR <= `delta'l, or true RR >= `delta'u:" _newline 
  noisily: di as txt "        z1 = " as res %-8.4g `z1' as txt _col(38) "z2 = " as res %-8.4g `z2' _newline
  noisily: di as txt "   Ho1: true RR <= `delta'l" _col(33) "Ho2: true RR >= `delta'u"
  noisily: di as txt "   Ha1: true RR > `delta'l"  _col(33) "Ha2: true RR < `delta'u"
  noisily: di as txt "   Pr(Z > z1) = " as res %6.4f `p1' _col(32) as txt " Pr(Z > z2) = " as res %6.4f `p2'


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
	 
   * Format delta0 to remove trailing zeros
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
     noi: di _newline _newline as txt "Relevance test conclusion for " uchar(0945) " = " as res "0" `alpha' as txt ", `delta'l =" as res %5.2f `lower' as txt ", and `delta'u =" as res %5.2f 1/`lower' as txt ":"
     }
   if (`upper' != `lower') {
     noi: di _newline _newline as txt "Relevance test conclusion for " uchar(0945) " = " as res "0" `alpha' as txt ", `delta'l = " as res "0" `lower' as txt  ", and `delta'u = " as res 1/`upper' as txt ":"
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
  if (`upper' == `lower') {
    return scalar delta0   = `lower'
    }
  if (`upper' != `lower') {
    return scalar deltaupper   = 1/`upper'
    return scalar deltalower   = `lower'
    }
  return scalar   p2   = `p2'
  return scalar   p1   = `p1' 
  return scalar   z2   = `z2'
  return scalar   z1   = `z1' 
  return scalar sdRR   = `sdRR'
  return scalar   RR   = `RR'
	
  end
  
  *
program define _q1, rclass sort
/*
    _q1 returns the primary root of the "q1" term of f(x) at the top of page
	1221 in Tang, Tang, and Chan (2003)

    Syntax:
            _q1 #a #b #c #n #delta0 
*/

  gettoken a 0 : 0, parse(" ,")
  gettoken b 0 : 0, parse(" ,")
  gettoken c 0 : 0, parse(" ,")
  gettoken n 0 : 0, parse(" ,")
  gettoken delta0 0 : 0, parse(" ,")

  confirm integer number `a'
  confirm integer number `b'
  confirm integer number `c'
  confirm integer number `n'
  confirm number `delta0'

  local A = `n'*(1+`delta0')
  local B = (`a'+`c')*(`delta0'^2)-(`a'+`b'+2*`c')
  local C = `c'*(1-`delta0')*(`a'+`b'+`c')/`n'
  local q1 = (sqrt(((`B')^2) - (4*`A'*`C')) - `B')/(2*`A')

  ret scalar q1 = `q1'

  end

  
program define _TangTangChanZ, rclass sort
  

  gettoken a 0 : 0, parse(" ,")
  gettoken b 0 : 0, parse(" ,")
  gettoken c 0 : 0, parse(" ,")
  gettoken n 0 : 0, parse(" ,")
  gettoken delta0 0 : 0, parse(" ,")

  confirm integer number `a'
  confirm integer number `b'
  confirm integer number `c'
  confirm integer number `n'
  confirm number `delta0'
  
  local numerator = `a' + `b' - (`a' + `c')*`delta0'
  _q1 `a' `b' `c' `n' `delta0'
  local q1 = r(q1)
  local denominator = sqrt(`n'*((1+`delta0')*`q1' + ((`a'+`b'+`c')*(`delta0'-1)/`n')))
  local TTC = `numerator'/`denominator'

  return scalar TTC = `TTC'
  
  end
  
  
  
