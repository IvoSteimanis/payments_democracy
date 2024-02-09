{smcl}
{* *! version 3.1.3  15may2021}{...}
{cmd:help tostregress}
{hline}


{title:Title}

{p2colset 5 20 24 2}{...}
{p2col:{cmd:tostregress} {hline 2}}Linear regression tests for equivalence{p_end}
{p2colreset}{...}


{title:Syntax}

{p 8 14 2}
{cmd:tostregress} {depvar} [{indepvars}] {ifin} [{it:{help regress##weight:weight}}] 
        [{cmd:,} 
        {opt eqvt:ype(typelist)} 
								{opt eqvl:evel(numlist)} 
								{opt upper:eqvlevel(numlist )}
								{opt rel:evance}
								{opt l:evel(#)}
								{it:regress_options}]


{synoptset 28 tabbed}{...}
{synopthdr}
{synoptline}
{syntab:Options}
{synopt :{opt eqvt:ype(typelist)}}specify equivalence threshold with Delta or epsilon for each {it:indepvar}{p_end}
{synopt :{opt eqvl:evel(numlist)}}the level of tolerance defining the equivalence interval for each {it:indepvar}{p_end}
{synopt :{opt upper:eqvlevel(numlist)}}the upper value of an asymmetric equivalence interval for each {it:indepvar}{p_end}
{synopt :{opt l:evel(#)}}set nominal type I level; default is {opt l:evel(95)} coressponding to alpha=0.05{p_end}
{synopt :{opt rel:evance}}perform & report combined tests for difference and equivalence{p_end}
{synopt :{opt reg:ress} options}all the options allowed with {help regress:regress}{p_end}
{synopt :{opt svy(svy options)}}all the options allowed with the {helpb svy} {opt prefix}, including {opt vcetype}{p_end}
{synoptline}
{p 4 6 2}{it:indepvars} may contain factor variables; see {help fvvarlist}.{p_end}
{p 4 6 2}{it:depvar} and {it:indepvars} may contain time-series operators; see {help tsvarlist}.{p_end}
{p 4 6 2}time-series operators may not be combined with factor variable operators.{p_end}
{p 4 6 2}{cmd:by}, and {cmd:statsby} are allowed; see {help prefix:prefix}.{p_end}
{p 4 6 2}{cmd:svy} is allowed via the {opt svy()} option, but not as a {help prefix:prefix} command.{p_end}
{p 4 6 2}{cmd:bootstrap} and {cmd:jackknife} are allowed using {opt vce()}, or {helpb svy}{cmd:()}, but are not allowed as {help prefix} commands. See Remarks.{p_end}
{marker weight}{...}
{p 4 6 2}Weights are allowed with neither {cmd:	vce(}{helpb bootstrap}{opt )}, nor {opt svy(bootstrap)}.{p_end}
{p 4 6 2}{cmd:aweight}s are allowed with neither {cmd:vce(}{helpb jackknife}{opt )}, nor {opt svy(jackknife)}.{p_end}
{p 4 6 2}{opt hascons}, {opt tsscons}, {opt vce()}, {opt beta}, {opt noheader}, {opt notable}, {opt plus}, {opt depname()}, {opt mse1}, and weights are not allowed with {helpb svy}{cmd:()}.{p_end}
{p 4 6 2} {cmd:aweight}s, {cmd:fweight}s, {cmd:iweight}s, and {cmd:pweight}s are
allowed; see {help weight}.{p_end}
{p 4 6 2}See {manhelp regress_postestimation R:regress postestimation} for features
available after estimation.  {p_end}

{p2colreset}{...}
{p 4 6 2}


{title:Description}

{pstd}
{cmd:tostregress} tests for the equivalence of each regression coefficient and zero 
within separate symmetric equivalence intervals defined by {opt eqvt:ype} and 
{opt eqvl:evel} for using a two one-sided {it:t} tests approach 
({help tostt##Schuirmann1987:Schuirmann, 1987}).  Typically ('positivist') null 
hypotheses are framed from an assumption of a lack of difference between two 
quantities, and reject this assumption only with sufficient evidence.  When 
performing tests for equivalence, one frames a ('negativist') null hypothesis with the 
assumption that two quantities are different by at least as much as an 
equivalence interval defined by some chosen level of tolerance.{p_end}

{pstd}
An equivalence null hypothesis takes one of the following two forms depending on 
whether equivalence is defined in terms of Delta (equivalence expressed in the 
same units as the parameter for an independent variable), or in terms of epsilon 
(equivalence expressed in the units of the {it:T} distribution with the given 
degrees of freedom):
 
{p 8}
Ho: |_b_{it:x}| >= Delta, {p_end}
{p 8 8 4}where the equivalence interval ranges from _b_{it:x} - Delta to _b_{it:x} + Delta, 
and where _b_{it:x} is the parameter being estimated for {it:x}.  This translates 
directly into two one-sided null hypotheses: {p_end}

{p 12}
Ho1: Delta - _b_{it:x} <= 0; and{p_end}

{p 12}
Ho2: _b_{it:x} + Delta <= 0{p_end}

{p 8}
-OR-

{p 12}
Ho: |{it:T}| >= epsilon, {p_end}

{p 8 8 8}where the equivalence interval ranges from -epsilon to epsilon.  This also 
translates directly into two one-sided null hypotheses: {p_end}

{p 12}
Ho1: epsilon - {it:T} <= 0; and{p_end}

{p 12}
Ho2: {it:T} + epsilon <= 0{p_end}

{p 8 8 4}
When an asymmetric equivalence interval is defined using the {opt upper:eqvlevel} option 
the general negativist null hypothesis becomes:{p_end}

{p 12}
Ho: _b_{it:x} <= Delta_lower, or _b_{it:x} >= Delta_upper,{p_end}

{p 8 8 4}
where the equivalence interval ranges from _b_{it:x} + Delta_lower to _b_{it:x} 
+ Delta_upper.  This also translates directly into two one-sided null hypotheses:{p_end}

{p 12}
Ho1: Delta_upper - _b_{it:x} <= 0; and{p_end}

{p 12}
Ho2: _b_{it:x} - Delta_lower <= 0{p_end}

{p 8}
-OR-

{p 8}
Ho: {it:T} <= epsilon_lower, or {it:T} >= epsilon_upper,{p_end}

{p 12}
Ho1: epsilon_upper - {it:T} <= 0; and{p_end}

{p 12}
Ho2: {it:T} - epsilon_lower <= 0{p_end}

{p 8 8 4}
The two one-sided test statistics corresponding to Ho1 and Ho2, {it:t}1 and {it:t}2, 
are both constructed so that their p-values are upper (right) tail 
probabilities:{p_end}

{p 12}
p1 = P({it:T}>={it:t}1){p_end}

{p 12}
p2 = P({it:T}>={it:t}2){p_end}
 
{pstd}
NOTE: the appropriate level of alpha implied by {opt l:evel} is precisely the 
same as in the corresponding two-sided test for mean difference, so that, for 
example, if one wishes to make a type I error %5 of the time, one simply 
conducts both of the one-sided tests of Ho1 and Ho2 by comparing the resulting 
p-value to 0.05 ({help tostt##Tryon2008:Tryon and Lewis, 2008}).{p_end}


{title:Options}

{dlgtab:Main}

{phang}
{opth eqvt:ype(typelist)} defines whether the equivalence interval will be 
defined in terms of Delta or epsilon ({opt delta}, or {opt epsilon}).  These 
options change the way that {opt evql:evel} is interpreted: when {opt delta} is 
specified, the {opt evql:evel} is measured in the units of the variable being 
tested, and when {opt epsilon} is specified, the {opt evql:evel} is measured in 
units of the {it:T} distribution; put another way epsilon = 
Delta/standard error.  The default is {opt delta}.{p_end}

{p 8 8 4}{opt eqvt:ype} must be specified with no entries, one entry, or the same 
number of entries as parameters (coefficients) being estimated, including the 
intercept term, if any. The below examples are for a model with three parameter 
estimates -- one for weight, one for foreign, and one for _cons (the intercept term):{p_end}

{phang2}The first example for {opt eqvt:ype} gives the default equivalence type ({opt delta}) either omitting the option (as shown here) or by leaving it empty):{p_end}
{phang3}{cmd:. tostregress mpg weight foreign, eqvlevel(5)}{p_end}

{phang2}This example for {opt eqvt:ype} gives a single equivalence type to be
used for all three parameters:{p_end}
{phang3}{cmd:. tostregress mpg weight foreign, eqvtype(epsilon) eqvlevel(2.145)}{p_end}

{phang2}The last example for {opt eqvt:ype} gives a separate equivalence type for 
each parameter:{p_end}
{phang3}{cmd:. tostregress mpg weight foreign, eqvtype(delta delta epsilon) eqvlevel(5 5 2.145)}{p_end}

{marker mineqvlevel}{...}
{p 8 8 4}
Defining tolerance in terms of epsilon means that it is not possible to reject 
any test for mean equivalence Ho if epsilon <= the critical value of {it:t} for a 
given {opt l:evel} and degrees of freedom.  Because epsilon = Delta/standard error, we 
can see that for the same reason it is not possible to reject any Ho if Delta 
<= the product of the standard error and critical value of {it:t} for a given 
{opt l:evel} and degrees of freedom. {cmd: tostregress} reports when either of 
these conditions obtain.{p_end}

{phang}
{opth eqvl:evel(numlist)} defines the equivalence threshold for the tests depending on 
whether {opt eqvt:ype} is {opt delta} or {opt epsilon} (see above).  Researchers 
are responsible for choosing meaningful values of Delta or epsilon.  The default 
value for each coefficient is 1 when {opt delta} is the {opt eqvt:ype} for that 
coefficient, and 2 when {opt epsilon} is the {opt eqvt:ype} for that coefficient.{p_end}

{p 8 8 4}{opt eqvl:evel} must be specified with no entries, one entry, or the same 
number of entries as parameters (coefficients) being estimated, including the 
intercept term, if any.{p_end}

{phang}
{opt upper:eqvlevel(#)} defines the {it: upper} equivalence threshold for the test, 
and transforms the meaning of {opt eqvl:evel} to mean the {it: lower} equivalence 
threshold for the test.  Also, {opt eqvl:evel} is assumed to be a negative value.  
Taken together, these correspond to Schuirmann's ({help tostt##Schuirmann1987:1987}) 
asymmetric equivalence intervals.  If {opt upper:eqvlevel}==|{opt eqvl:evel}|, then 
{opt upper:eqvlevel} will be ignored.{p_end}

{p 8 8 4}{opt eqvt:level} must be specified with no entries, one entry, or the same 
number of entries as parameters (coefficients) being estimated, including the 
intercept term, if any. The below examples are for a model with three parameter 
estimates -- one for weight, one for foreign, and one for _cons (the intercept term):{p_end}

{phang}
{opt l:evel(#)} specifies the nominal type I error rate.  The default is {opt l:evel(95)}, 
which corresponds to alpha = 0.05.

{phang}
{opt rel:evance} reports results and inference for combined tests for difference 
and equivalence for specific {opt l:evel}, {opt eqvt:ype}, and {opt eqvl:evel} choices.  
See the Remarks section more details on inference from combined tests.

{phang}
{opt svy(svy options)} estimates the model as if with the {opt svy} prefix, and 
any {opt svy} options you include. You may also specify {opt vcetype} options for 
{opt svy} here. See Remarks about {opt jackknife} and {opt bootstrap} options for {opt svy}.

{title:Remarks}

{pstd}
As described by Tryon and Lewis ({help tostt##Tryon2008:2008}), when both tests 
for difference and equivalence are taken together, there are four possible 
interpretations:{p_end}

{p 4 8 4}
1.  One may reject the null hypothesis of no difference, but fail to reject the 
null hypothesis of difference, and conclude that there is a {it: relevant difference} 
between _b_{it:x} and zero as large as Delta or epsilon.{p_end}

{p 4 8 4}
2.  One may fail to reject the null hypothesis of no difference, but reject the 
null hypothesis of difference, and conclude that _b_{it:x} is {it: equivalent} 
to zero within the equivalence range (i.e. defined by Delta or epsilon).{p_end}

{p 4 8 4}
3.  One may reject {it:both} the null hypothesis of no difference and the null 
hypothesis of difference, and conclude that _b_{it:x} is {it: trivially different}, 
from zero within the equivalence range (i.e. defined by Delta or epsilon).{p_end}

{p 4 8 4}
4.  One may fail to reject {it:both} the null hypothesis of no difference and the 
null hypothesis of difference, and draw an {it: indeterminate} conclusion, because 
the data are underpowered to detect difference or equivalence for _b_{it:x} and zero.{p_end}

{pstd}
Caveat Emptor: {opt jackknife} and {opt bootstrap} options for {opt svy} estimation have 
been implemented only at a basic level.  If you run into problems with these 
options, especially if using suboptions for either estimator, please share 
your syntax and data with me so that I may improve {cmd:tostregress}.


{title:Examples}

{pstd}
These examples correspond to those written in the help file for 
{help regress:regress}:{p_end}

{pstd}Setup{p_end}
{phang2}{cmd:. sysuse auto}{p_end}

{pstd}Report equivalence tests for a linear regression{p_end}
{phang2}{cmd:. tostregress mpg weight foreign, eqvtype(epsilon) eqvlevel(2.145)}{p_end}

{pstd}Report equivalence tests for a linear regression, but add a relevance test report{p_end}
{phang2}{cmd:. tostregress mpg weight foreign, eqvtype(epsilon) eqvlevel(2.145) relevance}{p_end}

{pstd}Fit a better linear regression, from a physics standpoint, but add asymmetric intervals, and report equivalence and relevance tests{p_end}
{phang2}{cmd:. gen gp100m = 100/mpg}{p_end}
{phang2}{cmd:. tostregress gp100m weight foreign, eqvtype(epsilon) eqvlevel(2.145) upper(1.895) rel}{p_end}

{pstd}Obtaining beta coefficients requires fitting model{p_end}
{phang2}{cmd:. tostregress gp100m weight foreign, beta eqvtype(epsilon) eqvlevel(2.145) upper(1.895) rel}{p_end}

{pstd}Report equivalence tests when suppressing the intercept term{p_end}
{phang2}{cmd:. tostregress weight length, noconstant eqvtype(delta) eqvlevel(5)}{p_end}

{pstd}Report equivalence tests when the model already has constant{p_end}
{phang2}{cmd:. tostregress weight length bn.foreign, hascons eqvtype(delta epsilon epsilon) eqvlevel(5 2.145 2.145)}{p_end}


{title:Examples:  equivalence and relevance tests for regression with robust standard errors}

        {hline}
{phang2}{cmd:. sysuse auto, clear}{p_end}
{phang2}{cmd:. generate gpmw = ((1/mpg)/weight)*100*1000}{p_end}
{phang2}{cmd:. tostregress gpmw foreign, eqvtype(epsilon) eqvlevel(2.145) rel}{p_end}
{phang2}{cmd:. tostregress gpmw foreign, vce(robust) eqvtype(epsilon) eqvlevel(2.145) rel}{p_end}
{phang2}{cmd:. tostregress gpmw foreign, vce(hc2) eqvtype(epsilon) eqvlevel(2.145) rel}{p_end}
{phang2}{cmd:. tostregress gpmw foreign, vce(hc3) eqvtype(epsilon) eqvlevel(2.145) rel}{p_end}
        {hline}
{phang2}{cmd:. webuse regsmpl, clear}{p_end}
{phang2}{cmd:. tostregress ln_wage age c.age#c.age tenure, vce(cluster id) eqvt(epsilon) eqvl(2.145)}{p_end}
        {hline}


{title:Example:  equivalence tests for weighted regression}

{phang2}{cmd:. sysuse census}{p_end}
{phang2}{cmd:. tostregress death medage i.region [aw=pop], eqvtype(epsilon) eqvlevel(1.782) level(90)}{p_end}


{title:Examples:  equivalence tests for linear regression with survey data}

{pstd}Setup{p_end}
{phang2}{cmd:. webuse highschool}

{pstd}Perform linear regression using survey data{p_end}
{phang2}{cmd:. svy: regress weight height}

{pstd}Perform corresponding linear regression tests for equivalence using survey data{p_end}
{phang2}{cmd:. tostregress weight height, eqvt(epsilon) eqvl(2.145) rel svy()}

{pstd}Setup{p_end}
{phang2}{cmd:. generate male = sex == 1 if !missing(sex)}

{pstd}Perform linear regression using survey data for a subpopulation{p_end}
{phang2}{cmd:. svy, subpop(male): regress weight height}

{pstd}Perform corresponding linear regression tests for equivalence using survey data for a subpopulation{p_end}
{phang2}{cmd:. tostregress weight height, eqvt(epsilon) eqvl(2.145) rel svy(subpop(male))}

{pstd}Perform the above survey data estimate using the jackknife variance estimator{p_end}
{phang2}{cmd:. tostregress weight height, eqvt(epsilon) eqvl(2.145) rel svy(jackknife subpop(male))}


{title:Saved results}

{pstd}
In addition to the information saved by {cmd:regress}, {cmd:tostregress} saves the following in {cmd:e()}:

{synoptset 20 tabbed}{...}
{p2col 5 15 19 2: Scalars}{p_end}
{synopt:{cmd:e(alpha)}}The nominal Type I error rate based on the {opt level} option (multiple comparisons issues will arise in a multiple regression context).{p_end}

{p2col 5 15 19 2: Macros}{p_end}
{synopt:{cmd:e(cmd)}}{cmd:tostregress}{p_end}
{synopt:{cmd:e(cmdline)}}canonical form of command as typed{p_end}
{synopt:{cmd:e(title)}}title of estimation output reflecting the {opt svy} option{p_end}
{synopt:{cmd:e(eqvtype)}}the {opt eqvtype} for each coefficient/test{p_end}
{synopt:{cmd:e(lowereqvlevel)}}the lower {opt eqvlevel} for each coefficient/test{p_end}
{synopt:{cmd:e(uppereqvlevel)}}the {opt uppereqvlevel} for each coefficient/test{p_end}
{synopt:{cmd:e(rel_conclusions)}}relevance test conclusions if the {opt relevance} option is used{p_end}

{p2col 5 15 19 2: Matrices}{p_end}
{synopt:{cmd:e(T1)}}{it:t}1 test statistics{p_end}
{synopt:{cmd:e(T2)}}{it:t}2 test statistics{p_end}
{synopt:{cmd:e(P1)}}{it:p}-values corresponding to the test statistics in T1{p_end}
{synopt:{cmd:e(P2)}}{it:p}-values corresponding to the test statistics in T2{p_end}
{p2colreset}{...}


{title:Author}

{pstd}Alexis Dinno{p_end}
{pstd}Portland State University{p_end}
{pstd}alexis.dinno@pdx.edu{p_end}

{pstd}
Development of {net "describe tost, from(https://alexisdinno.com/stata/)":tost} is 
ongoing, please contact me with any questions, bug reports or suggestions for 
improvement. Fixing bugs will be facilitated by sending along:{p_end}

{p 8 8 4}(1) a copy of the data (de-labeled or anonymized is fine),{p_end}
{p 8 8 4}(2) a copy of the command used, and{p_end}
{p 8 8 4}(3) a copy of the exact output of the command.{p_end}


{title:Suggested citation}

{p 4 8}
Dinno A. 2017. {bf:tostregress}: Linear regression tests for equivalence.  Stata software package. 
URL: {view "https://www.alexisdinno.com/stata/tost.html"}{p_end}


{title:References}

{marker Schuirmann1987}{...}
{phang}
Schuirmann, D.  A.  1987.  {browse "https://pdfs.semanticscholar.org/053b/97e316fc43588e6235f88a1a7a4077342de7.pdf":A comparison of the two one-sided tests procedure and the power approach for assessing the equivalence of average bioavailability}.  
{it:Journal of Pharmacokinetics and Biopharmaceutics}.  15: 657-680

{marker Tryon2008}{...}
{phang}
Tryon, W.  W., and C.  Lewis.  2008.  
{browse "https://sci-hub.io":An inferential confidence interval method of establishing statistical equivalence that corrects Tryon’s (2001) reduction factor}.  {it:Psychological Methods}.  13: 272-277


{title:Also See}

{psee}
{space 2}Help: {help tost:tost}, {help pkequiv:pkequiv}, {help regress:regress}{p_end}

