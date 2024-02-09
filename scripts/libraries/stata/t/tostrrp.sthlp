{smcl}
{* *! version 3.1.3  15may2021}{...}
{cmd:help tostrrp}
{hline}


{title:Title}

{p2colset 5 16 18 2}{...}
{p2col:{cmd:tostrrp} {hline 2}}Test for equivalence of relative risk and unity in paired binary data
{p_end}
{p2colreset}{...}


{marker syntax}{...}
{title:Syntax}


{p 8 14 2}{cmd:tostrrp} {it:treatment_1_outcome_variable} {it:treatment_2_outcome_variable} {ifin} 
, 
    {opt delta0(#)}
	[{cmd:,} {opt deltau:pper(#)}
    {opt a:lpha(#)}
    {opt rel:evance}
	{opt treatment1(treatment 1 name)}
    {opt treatment2(treatment 2 name)}
    {opt out:come(positive outcome label)}
    {opt noo:utcome(negative outcome label)}]

{p 8 14 2}{cmd:tostrrpi} {it:#a #b #c #n #delta0}  
    [{cmd:,} {opt deltau:pper(#)}
    {opt a:lpha(#)}
    {opt rel:evance}
    {opt treatment1(treatment 1 name)}
    {opt treatment2(treatment 2 name)}
    {opt out:come(positive outcome label)}
    {opt noo:utcome(negative outcome label)}]


{synoptset 21 tabbed}{...}
{synopthdr:tostrrp options}
{synoptline}
{syntab:Main}
{synopt :{opt delta0(#)}}the value defining an equivalence interval (the lower value, if {opt deltau:pper} is used){p_end}
{synoptline}
{p2colreset}{...}

{synoptset 21 tabbed}{...}
{synopthdr:tostrrp and tostrrpi options}
{synoptline}
{syntab:Main}
{synopt :{opt deltau:pper(#)}}the upper value of a geometrically asymmetric equivalence interval{p_end}
{synopt :{opt a:lpha(#)}}set nominal type I level; default is {opt a:lpha(0.05)}{p_end}
{synopt :{opt rel:evance}}perform & report combined tests for difference and equivalence{p_end}
{synopt :{opt treatment1(string)}}The name of the first treatment group{p_end}
{synopt :{opt treatment2(string)}}The name of the second treatment group{p_end}
{synopt :{opt out:come(string)}}The label for having a positive outcome{p_end}
{synopt :{opt noo:utcome(string)}}The label for having a negative outcome{p_end}
{synoptline}
{p2colreset}{...}


{marker description}{...}
{title:Description}

{pstd}
{cmd:tostrrp} test for equivalence of the relative risk of a positive outcome 
and unity in paired (or matched) randomized control trial or paired (or matched) 
cohort design data.  It calculates an asymptotic {it:z} test statistic based on a 
reparameterized multinomial model ({help tostrrp##Tang2003:Tang, et al., 2003}) 
in a two one-sided tests approach ({help tostrrp##Schuirmann1987:Schuirmann, 1987}).
{cmd:tostrrpi} is the immediate form of {cmd:tostrrp}; see {help immed}.  The equivalence 
interval for the test is defined by a chosen level of tolerance, as specified by 
{cmd: delta0}, or by {it:#delta0} in the immediate form of the command.{p_end}

{pstd}
The two one-sided null hypotheses take on the following form based on the 
relative risk (RR), and the threshold {opt delta0}:

{p 12}
Ho1: RR <= {opt delta0}, or{p_end}

{p 12}
Ho2: RR >= 1/{opt delta0},{p_end}

{p 8 8}where the equivalence interval ranges from {opt delta0} to 1/{opt delta0}.{p_end}

{pstd}
When a geometrically asymmetric equivalence interval is defined using the {opt deltau:pper} option 
the two one-sided null hypotheses become:{p_end}

 
{p 12}
Ho1: RR <= {opt delta0}, or{p_end}

{p 12}
Ho2: RR >= {opt deltaupper},{p_end}

{p 8}
where the equivalence interval ranges from {opt delta0} to 
{opt deltaupper}.{p_end}
 
{pstd}
The two {it:z} test statistics, {it:z}1 and {it:z}2, are both constructed with rejection 
probabilities in the upper tails. So {it:p}1 = P(Z >= {it:z}1), and {it:p}2 = P(Z >= {it:z}2).
 
{pstd}
NOTES: When delta0 = 1, the Tang, Tang and Chan test statistic reduces to 
McNemar's test statistic ({help tostrrp##McNemar1947:McNemar, 1947}).  When {it:a} = {it:b} = {it:c} = 0, 
there are no positve outcomes in either treatment group, and the RR and test 
statistics become undefined.  If {it:a} > 0, and {it:b} = {it:c} = 0, then there 
is complete concordance, and {it:z}1 = {it:z}2, so {it:p}1 = {it:p}2.  As is standard with two 
one-sided tests for equivalence, if one wishes to make a type I error %5 of the 
time, one simply conducts both of the one-sided tests of Ho1 and Ho2 by comparing 
the resulting {it:p}-value to 0.05 ({help tostrrp##Wellek2010:Wellek, 2010}).{p_end}


{marker options}{...}
{title:Options for tostrrp and tostrrpi}

{dlgtab:Main}

{phang}
{opth delta0(#)} defines the equivalence threshold for the tests, with the lower 
boundary of equivalence equal to {opt delta0}, and the upper boundary equal to 
1/{opt delta0}.  For example, {opt delta0} = 0.8 gives an equivalence interval 
of 0.8, 1.25 (because 1.25 = 1/0.8). Researchers are responsible for choosing 
meaningful values of {opt delta0}.{p_end}

{phang}
{opt deltau:pper(#)} defines the {it: upper} equivalence threshold for the test, 
and restricts the meaning of {opt delta0} to the {it: lower} equivalence threshold 
for the test.  Also, {opt delta0} must be a positive value less than or equal to 1, 
and {opt deltau:pper} must be a positive value that is greater than or equal to 1.  
Taken together, these correspond to geometrically asymmetric equivalence intervals.{p_end}

{phang}
{opt a:lpha(#)} specifies the nominal type I error rate.  The default is {opt a:lpha(0.05)}.{p_end}

{phang}
{opt rel:evance} reports results and inference for combined tests for difference 
and equivalence of marginal probabilities (of exposure) for a specific 
{opt a:lpha}, and {opt delta0}.  See the end of the Discussion section in {help tost} 
for more details on inference from combined tests.

{phang}
{opt treatment1(string)} labels the name of the first treatment group in the output table.
For {cmd:tostppr} this defaults to the variable label for {it:treatment_1_outcome_variable}.

{phang}
{opt treatment2(string)} labels the name of the second treatment group in the output table.
For {cmd:tostppr} this defaults to the variable label for {it:treatment_2_outcome_variable}.

{phang}
{opt out:come(string)} labels the value corresponding to positive for the outcome. 
For {cmd:tostppr} this defaults to the label for the value = 1 in {it:treatment_1_outcome_variable} 
(or in {it:treatment_2_outcome_variable} if {it:treatment_1_outcome_variable} is unlabeled).

{phang}
{opt noout:come(string)} labels the value corresponding to negative for the outcome.
For {cmd:tostppr} this defaults to the label for the value = 0 in {it:treatment_1_outcome_variable} 
(or in {it:treatment_2_outcome_variable} if {it:treatment_1_outcome_variable} is unlabeled).


{marker examples}{...}
{title:Examples}

{pstd}
Setup{p_end}
{phang2}{cmd:. use hivfluid} (requires that you {net "get tost, from(https://alexisdinno.com/stata/)":install hivfluid.dta})

{pstd}
Relevance test example from Tang, et al., 2003, Table II, based on data from {help tostrrp##Lachenbruch1998:Lachenbruch and Lynch, 1998}{p_end}
{phang2}{cmd:. tostrrp plasma alternate, delta0(0.95) rel}          

{pstd}
Same as above command, but using immediate form{p_end}
{phang2}{cmd:. tostrrpi 446 5 16 1157 0.95, rel treatment1("Plasma sample") treatment2("Alternative fluid") outcome("HIV Positive") nooutcome("HIV Negative")}

{pstd}
Example from Tang, et al., 2003, Table V, based on data from {help tostrrp##Tango1998:Tango, 1998}{p_end}
{phang2}{cmd:. tostrrpi 43 0 1 44 0.9, treatment1("Thermal") treatment2("Chemical") outcome("Effective") nooutcome("Ineffective")}


{marker saved_results}{...}
{title:Saved results}

{pstd}
{cmd:tostrrp} and {cmd:tostrrpi} save the following in {cmd:r()}:

{synoptset 15 tabbed}{...}
{p2col 5 15 19 2: Scalars}{p_end}
{synopt:{cmd:r(RR)}}relative risk (aka incidence rate ratio) of positive outcome for treatment 2 vs. treatment 1{p_end}
{synopt:{cmd:r(sdRR)}}standard deviation of relative risk based on the score statistic per ({help tostrrp##Tang2003:Tang, et al., 2003}){p_end}
{synopt:{cmd:r(z1)}}{it:z} test statistic for Ho1 (upper){p_end}
{synopt:{cmd:r(z2)}}{it:z} test statistic for Ho2 (lower){p_end}
{synopt:{cmd:r(p1)}}P({it:Z} >= {it:z}1){p_end}
{synopt:{cmd:r(p2)}}P({it:Z} >= {it:z}2){p_end}
{synopt:{cmd:r(delta0)}}delta0, tolerance level defining the equivalence interval; OR{p_end}
{synopt:{cmd:r(deltalower)}}delta_lower, tolerance level defining the equivalence interval's lower side; AND{p_end}
{synopt:{cmd:r(deltaupper)}}delta_upper, tolerance level defining the equivalence interval's upper side{p_end}
{synopt:{cmd:r(relevance)}}Relevance test conclusion for given alpha and delta0{p_end}
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
Dinno A.  2017.  {bf:tostrrp}: Test for equivalence of relative risk and unity in 
paired designs.  Stata software package.  URL: {view "https://www.alexisdinno.com/stata/tost.html"}


{marker reference}{...}
{title:Reference}

{marker Lachenbruch1998}{...}
{phang}
Lachenbruch, P. A. and Lynch, C. J.  1998.  {browse "https://sci-hub.io":Assessing screening tests: Extensions of McNemar’s test}.  
{it:Statistics In Medicine} 17: 2207-2217

{marker McNemar1947}{...}
{phang}
McNemar, Q.  1947.  {browse "https://sci-hub.io":Note on the sampling error of the difference between correlated proportions or percentages}.  
{it:Psychometrika} 12: 153-157

{marker Schuirmann1987}{...}
{phang}
Schuirmann, D.  A.  1987.  {browse "https://pdfs.semanticscholar.org/053b/97e316fc43588e6235f88a1a7a4077342de7.pdf":A comparison of the two one-sided tests procedure and the power approach for assessing the equivalence of average bioavailability}.  
{it:Journal of Pharmacokinetics and Biopharmaceutics}.  15: 657-680

{marker Tang2003}{...}
{phang}
Tang, N.-S., Tang, M.-L., and Chan, I. S. F.  2003.  {browse "https://sci-hub.io":On tests of equivalence via non-unity relative risk for matched-pair design}.  {it:Statistics In Medicine} 22: 1217-1233.

{marker Tango1998}{...}
{phang}
Tango, T.  1998.  {browse "https://sci-hub.io":Equivalence test and confidence interval for the difference in proportions for the paired-sample design}.  
{it:Statistics In Medicine}, 17: 891-908{p_end}

{marker Wellek2010}{...}
{phang}
Wellek, S.  2010.  {browse "https://www.crcpress.com/product/isbn/9781439808184":{it:Testing Statistical Hypotheses of Equivalence and Noninferiority}}, 
second edition.  Chapman and Hall/CRC Press.  p.  31{p_end}


{title:Also See}

{psee}
{space 2}Help: {help tost:tost}, {help mcc:mcc}, {help tostmcc:tostmcc}

