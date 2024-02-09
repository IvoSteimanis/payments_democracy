{smcl}
{* *! version 3.1.3  15may2021}{...}
{cmd:help tostmcc}
{hline}


{title:Title}

{p2colset 5 16 18 2}{...}
{p2col:{cmd:tostmcc} {hline 2}}Paired {it:z} test for equivalence of marginal probabilities in binary data
{p_end}
{p2colreset}{...}


{marker syntax}{...}
{title:Syntax}

{p 8 14 2}{cmd:tostmcc} {it:var_exposed_case} {it:var_exposed_control} {ifin} 
{weight} [{cmd:,} {opt eqvt:ype(type)}
    {opt eqvl:evel(#)}
    {opt upper:eqvlevel(#)}
    {opt ya:tes}
    {opt ed:wards}
    {opt a:lpha(#)}
    {opt rel:evance}]

{p 8 14 2}{cmd:tostmcci} {it:#a #b #c #d}  
    [{cmd:,} {opt eqvt:ype(type)}
    {opt eqvl:evel(#)}
    {opt upper:eqvlevel(#)}
    {opt ya:tes}
    {opt ed:wards}
    {opt a:lpha(#)}
    {opt rel:evance}]

{synoptset 21 tabbed}{...}
{synopthdr:tostmcc options}
{synoptline}
{syntab:Main}
{synopt :{opt eqvt:ype(string)}}specify equivalence threshold with Delta or epsilon{p_end}
{synopt :{opt eqvl:evel(#)}}the level of tolerance defining the equivalence interval{p_end}
{synopt :{opt upper:eqvlevel(#)}}the upper value of an asymmetric equivalence interval{p_end}
{synopt :{opt ya:tes}}include a Yates continuity correction{p_end}
{synopt :{opt ed:wards}}include an Edwards continuity correction{p_end}
{synopt :{opt a:lpha(#)}}set nominal type I level; default is {opt a:lpha(0.05)}{p_end}
{synopt :{opt rel:evance}}perform & report combined tests for difference and equivalence{p_end}
{synoptline}
{p2colreset}{...}
{pstd}{opt fweight}s are allowed; see {help weight}.


{marker description}{...}
{title:Description}

{pstd}
{cmd:tostmcc} tests for equivalence of the marginal probabilities of exposure in matched 
case-control data.  It calculates a Wald-type asymptotic {it:z} test ({help tostmcc##Liu2002:Liu, et al., 2002}) 
in a two one-sided tests approach ({help tostmcc##Schuirmann1987:Schuirmann, 1987}).
{cmd:tostmcci} is the immediate form of {cmd:tostmcc}; see {help immed}.  Typically the 
null hypotheses of the corresponding McNemar's chi-square test 
({help tostmcc##McNemar1947:McNemar, 1947}) for difference in marginal probabilities 
are framed from an assumption of equality of marginal probability of exposure
between cases and controls (e.g.  Ho: P(exposure|case) = P(exposure|controls), 
rejecting this assumption only with sufficient evidence.  When performing tests 
for equivalence of marginal probabilities, the null hypothesis is framed as the 
difference in marginal probabilities is at least as much as the equivalence 
interval as defined by some chosen level of tolerance (as specified by {opt eqvt:ype} 
and {opt eqvl:evel}).{p_end}

{pstd}
With respect to a {it:z} test, a negativist null hypothesis takes one of the 
following two forms depending on whether tolerance is defined in terms of
Delta (equivalence expressed in the units of the probability of counts of 
discordant pairs) or in terms of epsilon (equivalence expressed in the units of 
the {it:z} distribution):
 
{p 8}
Ho: |b - c| >= Delta, {p_end}
{p 8 8}where the equivalence interval ranges from |b - c|-Delta to 
|b - c|+Delta, and where b is the count of pairs with cases exposed, but 
controls unexposed, and and c is the count of pairs with cases unexposed and 
controls exposed.  This null hypothesis translates directly into two one-sided 
null hypotheses: {p_end}

{p 12}
Ho1: Delta - ({it:b} - {it:c}) <= 0; and{p_end}

{p 12}
Ho2: ({it:b} - {it:c}) + Delta <= 0{p_end}

{p 8}
-OR-

{p 8}
Ho: |{it:Z}| >= epsilon, {p_end}
{p 8 8}where the equivalence interval ranges from -epsilon to epsilon.  This also 
translates directly into two one-sided null hypotheses: {p_end}

{p 12}
Ho1: epsilon - {it:Z} <= 0; and{p_end}

{p 12}
Ho2: {it:Z} + epsilon <= 0{p_end}

{p 8 8}
When an asymmetric equivalence interval is defined using the {opt upper:eqvlevel} option 
the general negativist null hypothesis becomes:{p_end}

{p 8}
Ho: (b - c) <= Delta_lower, or (b - c) >= Delta_upper,{p_end}
{p 8 8 }
where the equivalence interval ranges from (b - c) + Delta_lower to 
(b - c) + Delta_upper.  This also translates directly into two one-sided null 
hypotheses:{p_end}

{p 12}
Ho1: Delta_upper - ({it:b} - {it:c}) <= 0; and{p_end}

{p 12}
Ho2: ({it:b} - {it:c}) - Delta_lower <= 0{p_end}

{p 8}
-OR-

{p 8}
Ho: {it:Z} <= epsilon_lower, or {it:Z} >= epsilon_upper,{p_end}

{p 12}
Ho1: epsilon_upper - {it:Z} <= 0; and{p_end}

{p 12}
Ho2: {it:Z} - epsilon_lower <= 0{p_end}
 
{pstd}
NOTE: the appropriate level of {opt a:lpha} is precisely the same as in the 
corresponding McNemar's test for difference of maginal probabilities, so that, 
for example, if one wishes to make a type I error %5 of the time, one simply 
conducts both of the one-sided tests of Ho1 and Ho2 by comparing the resulting 
{it:p}-value to 0.05 ({help tostmcc##Wellek2010:Wellek, 2010}).{p_end}


{marker options}{...}
{title:Options for mcc and mcci}

{dlgtab:Main}

{phang}
{opth eqvt:ype(string)} defines whether the equivalence interval will be 
defined in terms of Delta or epsilon ({opt delta}, or {opt epsilon}).  These 
options change the way that {opt evql:evel} is interpreted: when {opt delta} is 
specified, the {opt evql:evel} is measured in the units of the variable being 
tested, and when {opt epsilon} is specified, the {opt evql:evel} is measured in 
multiples of the standard deviation of the {it:Z} distribution; put another way 
epsilon = Delta/standard error.  The default is {opt delta}.{p_end}

{marker mineqvlevel}{...}
{p 8 8}
Defining tolerance in terms of epsilon means that it is not possible to reject 
any test of mean equivalence Ho if epsilon <= the critical value of {it:Z} for a 
given {opt a:lpha}.  Because epsilon = {it:n}*Delta/standard error, we can see that it is not 
possible to reject any Ho if Delta <= the product of the standard error and 
critical value of {it:Z} over n for a given {opt a:lpha}.  {cmd: tostmcc} reports when either 
of these conditions obtain.  Tolerances should be specified using {opt delta} by 
considering the difference in P({it:b}) and P({it:c}).{p_end}

{phang}
{opth eqvl:evel(#)} defines the equivalence threshold for the tests depending on 
whether {opt eqvt:ype} is {opt delta} or {opt epsilon} (see above).  Researchers 
are responsible for choosing meaningful values of Delta or epsilon.  The default 
value is .1 when {opt delta} is the {opt eqvt:ype} and 2 when {opt epsilon} is 
the {opt eqvt:ype}.{p_end}

{phang}
{opt upper:eqvlevel(#)} defines the {it: upper} equivalence threshold for the test, 
and transforms the meaning of {opt eqvl:evel} to mean the {it: lower} equivalence 
threshold for the test.  Also, {opt eqvl:evel} is assumed to be a negative value.  
Taken together, these correspond to Schuirmann's ({help tostranksum##Schuirmann1987:1987}) 
asymmetric equivalence intervals.  If {opt upper:eqvlevel}==|{opt eqvl:evel}|, then 
{opt upper:eqvlevel} will be ignored.{p_end}

{phang}
{opt ya:tes} specifies that the test statistics incorporate a Yates continuity 
correction (help tostmcc##yates1934:Yates, 1934) using the term [(b - c)-0.5] 
for z1, and the term [(b - c)+0.5] for z2.  {opt ya:tes} is exclusive of {opt ed:wards}.  This
continuity correction is only applied to the two one-sided {it:z} test statistics, 
and not to any of the confidence intervals.{p_end}

{phang}
{opt ed:wards} specifies that the test statistics incorporate an Edwards continuity 
correction ({help tostmcc##edwards1947:Edwards, 1947}) using the term [(b - c)-1] 
for z1, and the term [(b - c)+1] for z2.  {opt ed:wards} is exclusive of {opt ya:tes}.  This
continuity correction is only applied to the two one-sided {it:z} test statistics, 
and not to any of the confidence intervals.{p_end}

{phang}
{opt a:lpha(#)} specifies the nominal type I error rate.  The default is {opt a:lpha(0.05)}.{p_end}

{phang}
{opt rel:evance} reports results and inference for combined tests for difference 
and equivalence of marginal probabilities (of exposure) for a specific 
{opt a:lpha}, {opt eqvt:ype}, and {opt eqvl:evel}.  See the end of the 
Discussion section in {help tost} for more details on inference from combined tests.


{marker examples}{...}
{title:Examples}

{pstd}Setup{p_end}
{phang2}{cmd:.  webuse mccxmpl}

{pstd}Relevance test in paired binary data{p_end}
{phang2}{cmd:.  tostmcc case control [fw=pop], eqvt(delta) eqvlevel(.2) rel}

{pstd}Same as above command, but using immediate form{p_end}
{phang2}{cmd:.  tostmcci 8 8 3 8, eqvt(delta) eqvlevel(.2) rel}

{pstd}With asymetric equivalence intervals specified with epsilon{p_end}
{phang2}{cmd:.  tostmcci 8 8 3 8, eqvt(epsilon) eqvlevel(2) upper(3) rel}


{marker saved_results}{...}
{title:Saved results}

{pstd}
{cmd:tostmcc} and {cmd:tostmcci} save the following in {cmd:r()}:

{synoptset 15 tabbed}{...}
{p2col 5 15 19 2: Scalars}{p_end}
{synopt:{cmd:r(z1)}}{it:z} test statistic for Ho1 (upper){p_end}
{synopt:{cmd:r(z2)}}{it:z} test statistic for Ho2 (lower){p_end}
{synopt:{cmd:r(p1)}}P({it:Z} >= {it:z}1){p_end}
{synopt:{cmd:r(p2)}}P({it:Z} >= {it:z}2){p_end}
{synopt:{cmd:r(D_f)}}difference in proportion with exposure{p_end}
{synopt:{cmd:r(Delta)}}Delta, tolerance level defining the equivalence interval; OR{p_end}
{synopt:{cmd:r(Du)}}Delta_upper, tolerance level defining the equivalence interval's upper side; AND{p_end}
{synopt:{cmd:r(Dl)}}Delta_lower, tolerance level defining the equivalence interval's lower side; OR{p_end}
{synopt:{cmd:r(epsilon)}}epsilon, tolerance level defining the equivalence interval{p_end}
{synopt:{cmd:r(eu)}}epsilon_upper, tolerance level defining the equivalence interval's upper side; AND{p_end}
{synopt:{cmd:r(el)}}epsilon_lower, tolerance level defining the equivalence interval's lower side{p_end}
{synopt:{cmd:r(relevance)}}Relevance test conclusion for given alpha and Delta/epsilon{p_end}
{p2colreset}{...}


{title:Author}

{pstd}Alexis Dinno{p_end}
{pstd}Portland State University{p_end}
{pstd}alexis.dinno@pdx.edu{p_end}

{pstd}
Development of {net "describe tost, from(https://alexisdinno.com/stata/)":tost} is 
ongoing, please contact me with any questions, bug reports or suggestions for 
improvement.  Fixing bugs will be facilitated by sending along:{p_end}

{p 8 8 4}(1) a copy of the data (de-labeled or anonymized is fine),{p_end}
{p 8 8 4}(2) a copy of the command used, and{p_end}
{p 8 8 4}(3) a copy of the exact output of the command.{p_end}


{title:Suggested citation}

{p 4 8}
Dinno A.  2017.  {bf:tostmcc}: Paired {it:z} test for equivalence in binary 
data.  Stata software package.  URL: {view "https://www.alexisdinno.com/stata/tost.html"}


{marker reference}{...}
{title:Reference}

{marker Edwards1948}{...}
{phang}
Edwards, A.  1948.  {browse "https://sci-hub.io":Note on the "correction for continuity" in testing the significance of the difference between correlated proportions}.  
{it:Psychometrika} 13: 185-187

{marker Liu2002}{...}
{phang}
Liu, J., et al., 2002.  {browse "https://sci-hub.io":Tests for equivalence or non-inferiority for paired binary data}.  
{it:Statistics In Medicine} 21: 231-245.

{marker McNemar1947}{...}
{phang}
McNemar, Q.  1947.  {browse "https://sci-hub.io":Note on the sampling error of the difference between correlated proportions or percentages}.  
{it:Psychometrika} 12: 153-157

{marker Schuirmann1987}{...}
{phang}
Schuirmann, D.  A.  1987.  {browse "https://pdfs.semanticscholar.org/053b/97e316fc43588e6235f88a1a7a4077342de7.pdf":A comparison of the two one-sided tests procedure and the power approach for assessing the equivalence of average bioavailability}.  
{it:Journal of Pharmacokinetics and Biopharmaceutics}.  15: 657-680

{marker Yates1934}{...}
{phang}
Yates, F.  1934.  {browse "https://www.jstor.org/stable/2983604":Contingency tables involving small numbers and the chi-squared test}.  
{it: Supplement to the Journal of the Royal Statistical Society}.  1: 217-235

{marker Wellek2010}{...}
{phang}
Wellek, S.  2010.  {browse "https://www.crcpress.com/product/isbn/9781439808184":{it:Testing Statistical Hypotheses of Equivalence and Noninferiority}}, 
second edition.  Chapman and Hall/CRC Press.  p.  31{p_end}


{title:Also See}

{psee}
{space 2}Help: {help tost:tost}, {help tostrrp:tostrrp}, {help pkequiv:pkequiv}, {help mcc:mcc}


