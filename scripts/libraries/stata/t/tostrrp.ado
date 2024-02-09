*! version 3.1.3  15may2021 by alexis dot dinno at pdx dot edu
*! perform two one-sided tests for equivalence of RR and unity in paired data

* Syntax:  tostrrp varname1 == varname2 [if exp] [in range] , delta0(#) [
*          deltaupper(#) alpha(#) relevance treatment1(string) 
*          treatment2(string) outcome(string) nooutcome(string)]

program define tostrrp

  if int(_caller())<8 {
    di in r "tostrrp- does not support this version of Stata." _newline
    di as txt "Requests for a v7 compatible version will be relatively easy to honor." 
    di as txt "Requests for a v6 compatible version may be less easy." 
    di as txt "Requests for a version compatible with versions of STATA earlier than v6 are "
    di as txt "untenable since I do not have access to the software." _newline 
    di as txt "All requests are welcome and will be considered."
    exit
    }
  if int(_caller())<14 {
    tostrrp8 `0'
    }
   else { 
    tostrrp14 `0'
    } 
  end

program define tostrrp8, rclass byable(recall)
  version 8.0, missing

  /* turn "==" into "=" if needed before calling -syntax- */
*  gettoken vn rest : 0, parse(" =")
*  gettoken eq rest : rest, parse(" =")
*  if "`eq'" == "==" {
*    local 0 `vn' = `rest'
*	}

  syntax varlist [if] [in] , delta0(real) [ alpha(real 0.05) RELevance /*
  */     deltaupper(real 0) treatment1(string) treatment2(string) /*
  */     OUTcome(string) NOOutcome(string)]

  tokenize `varlist'

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

* Set up treatment and outcome status names, as user requests
*  local outcome = ""
*  local nooutcome = ""
  if "`treatment1'" == "" {
    local treatment1 : variable label `1'
	}
  if "`treatment2'" == "" {
    local treatment2 : variable label `2'
	}
  if "`outcome'" == "" {
    local labelname : value label `1'
    if ("`labelname'" == "") {
      local labelname : value label `2'
      }
     else {
      mata: st_vlload("`labelname'", values = ., text = "")
      mata: st_local("outcome", text[2])
      }
    }
  if "`nooutcome'" == "" {
    local labelname : value label `1'
    if ("`labelname'" == "") {
      local labelname : value label `2'
      }
     else {
      mata: st_vlload("`labelname'", values = ., text = "")
      mata: st_local("nooutcome", text[1])
      }
    }

  if "`treatment1'"=="" {
    local treatname1 = "Treatment 1"
	}
   else {
    local treatname1 = "`treatment1'"
	}
  if "`treatment2'"=="" {
    local treatname2 = "Treatment 2"
	}
   else {
    local treatname2 = "`treatment2'"
	}
  if "`outcome'" =="" {
    local positive = "Positive"
	}
   else {
    local positive = "`outcome'"
	}
  if "`nooutcome'" =="" {
    local negative = "Negative"
	}
   else {
    local negative = "`nooutcome'"
	} 

*******************************************************************************
* The business starts here                                                    *
*******************************************************************************

  quietly {
    if "`if'" == "" {
      count if `1' == 1 & `2' == 1 `in'
      local a = r(N)
      count if `1' == 0 & `2' == 1 `in'
      local b = r(N)
      count if `1' == 1 & `2' == 0 `in'
      local c = r(N)
      count `in'
      local n = r(N)
      }
	 else {
	  count `if' & `1' == 1 & `2' == 1 `in'
      local a = r(N)
      count `if' & `1' == 0 & `2' == 1 `in'
      local b = r(N)
      count `if' & `1' == 1 & `2' == 0 `in'
      local c = r(N)
      count `if' `in'
      local n = r(N)
      }
    }
  if `upper' == `lower' { 
    tostrrpi `a' `b' `c' `n' `delta0', alpha(`alpha') `relevance' treatment1("`treatname1'") treatment2("`treatname2'") outcome("`positive'") nooutcome("`nooutcome'")
	}
   else { 
    tostrrpi `a' `b' `c' `n' `delta0', deltaupper(`deltaupper') alpha(`alpha') `relevance' treatment1("`treatname1'") treatment2("`treatname2'") outcome("`positive'") nooutcome("`nooutcome'")
	}

  end


program define tostrrp14, rclass byable(recall)
  version 14.0, missing

  /* turn "==" into "=" if needed before calling -syntax- */
*  gettoken vn rest : 0, parse(" =")
*  gettoken eq rest : rest, parse(" =")
*  if "`eq'" == "==" {
*    local 0 `vn' = `rest'
*	}

  syntax varlist [if] [in] , delta0(real) [ alpha(real 0.05) RELevance /*
  */     deltaupper(real 0) treatment1(string) treatment2(string) /*
  */     OUTcome(string) NOOutcome(string)]

  tokenize `varlist'

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

* Set up treatment and outcome status names, as user requests
*  local outcome = ""
*  local nooutcome = ""
  if "`treatment1'" == "" {
    local treatment1 : variable label `1'
	}
  if "`treatment2'" == "" {
    local treatment2 : variable label `2'
	}
  if "`outcome'" == "" {
    local labelname : value label `1'
    if ("`labelname'" == "") {
      local labelname : value label `2'
      }
     else {
      mata: st_vlload("`labelname'", values = ., text = "")
      mata: st_local("outcome", text[2])
      }
    }
  if "`nooutcome'" == "" {
    local labelname : value label `1'
    if ("`labelname'" == "") {
      local labelname : value label `2'
      }
     else {
      mata: st_vlload("`labelname'", values = ., text = "")
      mata: st_local("nooutcome", text[1])
      }
    }

  if "`treatment1'"=="" {
    local treatname1 = "Treatment 1"
	}
   else {
    local treatname1 = "`treatment1'"
	}
  if "`treatment2'"=="" {
    local treatname2 = "Treatment 2"
	}
   else {
    local treatname2 = "`treatment2'"
	}
  if "`outcome'" =="" {
    local positive = "Positive"
	}
   else {
    local positive = "`outcome'"
	}
  if "`nooutcome'" =="" {
    local negative = "Negative"
	}
   else {
    local negative = "`nooutcome'"
	} 

*******************************************************************************
* The business starts here                                                    *
*******************************************************************************

  quietly {
    if "`if'" == "" {
      count if `1' == 1 & `2' == 1 `in'
      local a = r(N)
      count if `1' == 0 & `2' == 1 `in'
      local b = r(N)
      count if `1' == 1 & `2' == 0 `in'
      local c = r(N)
      count `in'
      local n = r(N)
      }
	 else {
	  count `if' & `1' == 1 & `2' == 1 `in'
      local a = r(N)
      count `if' & `1' == 0 & `2' == 1 `in'
      local b = r(N)
      count `if' & `1' == 1 & `2' == 0 `in'
      local c = r(N)
      count `if' `in'
      local n = r(N)
      }
    }
  if `upper' == `lower' { 
    tostrrpi `a' `b' `c' `n' `delta0', alpha(`alpha') `relevance' treatment1("`treatname1'") treatment2("`treatname2'") outcome("`positive'") nooutcome("`nooutcome'")
	}
   else { 
    tostrrpi `a' `b' `c' `n' `delta0', deltaupper(`deltaupper') alpha(`alpha') `relevance' treatment1("`treatname1'") treatment2("`treatname2'") outcome("`positive'") nooutcome("`nooutcome'")
	}

  end
