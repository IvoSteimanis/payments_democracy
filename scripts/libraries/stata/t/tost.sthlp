{smcl}
{* *! version 3.1.3  15may2021}{...}
{cmd:help tost}
{hline}


{title:Title}

{p2colset 5 13 15 2}{...}
{p2col:{cmd:tost} {hline 2}}Two one-sided tests for equivalence{p_end}
{p2colreset}{...}


{synoptset 28 tabbed}{...}
{synopthdr:tost commands}
{synoptline}
{syntab:Miscellaneous}
{synopt :{opt tostt}}Mean equivalence {it:t} tests{p_end}
{synopt :{opt tostti}}Immediate command for mean equivalence {it:t} tests{p_end}
{synopt :{opt tostpr}}{it:z} tests for proportion equivalence{p_end}
{synopt :{opt tostpri}}Immediate command for {it:z} tests for proportion equivalence{p_end}
{synopt :{opt tostsignrank}}Test for the distribution of paired or matched data being equivalent to one that is symmetrical & centered on zero{p_end}
{synopt :{opt tostranksum}}Two-sample rank-sum test for stochastic equivalence{p_end}
{synopt :{opt tostmcc}}Paired {it:z}-test for stochastic equivalence in binary data{p_end}
{synopt :{opt tostmcci}}Immediate command for paired {it:z}-test for stochastic equivalence in binary data{p_end}
{synopt :{opt tostregress}}Linear regression tests for equivalence{p_end}
{synopt :{opt tostrrp}}Test for equivalence of relative risks in paired designs{p_end}
{synopt :{opt tostrrpi}}Immediate command for test for equivalence of relative risks in paired designs{p_end}
{synoptline}
{p2colreset}{...}


{title:Description}

{pstd}
The {net "describe tost, from(https://alexisdinno.com/stata/)":tost} package provides a suite of commands to perform 
two one-sided tests for equivalence corresponding to the {help ttest} & 
{help ttesti}, {help prtest} & {help prtesti}, {help mcc} & {help mcci}, {help regress}, 
{help signrank}, and {help ranksum} tests for difference, thus addressing 
inference about equivalence for a number of paired and unpaired, parametric 
and nonparametric study designs and data types.  Each command tests a null 
hypothesis that samples were drawn from populations different by at least plus 
or minus some researcher-defined level of tolerance, which can be defined in 
terms of units of the data or rank units (Delta), or (except for {cmd:tostrrp} 
and {cmd:tostrrpi}) in units of the test statistic's distribution (epsilon).  
Enough evidence rejects this null hypothesis in favor of equivalence within the 
tolerance.  Equivalence intervals for all tests may be defined symmetrically or 
asymmetrically.

{pstd}
These tests are equivalence tests are all more or less based on the {it:t} and 
{it:z} tests following the logic laid out by Schuirmann ({help tost##Schuirmann1987:1987}),
but with variations as detailed in the help files for each command.  All the 
{help tost} commands are based on a Wald-type test, where some difference in 
sample statistics is divided by the standard deviation of that difference: 
theta/st.dev. theta.  A general test for equivalence null hypothesis takes one 
of the following two forms depending on whether equivalence is defined in terms 
of Delta, or epsilon:{p_end}

{p 8}
Ho: |theta| >= Delta, {p_end}
{p 8 8}where the equivalence interval ranges from theta-Delta to theta+Delta.  
This translates  directly into two one-sided null hypotheses: {p_end}

{p 12}
Ho1: Delta - theta <= 0; and{p_end}

{p 12}
Ho2: theta + Delta <= 0{p_end}

{p 8}
-OR-

{p 8}
Ho: |{it:Z}| >= epsilon, (may substitute {it:T} for {it:Z} in these expressions){p_end}
{p 8 8}where the equivalence interval ranges from -epsilon to epsilon.  This also 
translates directly into two one-sided null hypotheses: {p_end}

{p 12}
Ho1: epsilon - {it:Z} <= 0; and{p_end}

{p 12}
Ho2: {it:Z} + epsilon <= 0{p_end}

{p 8 8}
When an asymmetric equivalence interval is defined using the the general 
negativist null hypothesis becomes:{p_end}

{p 8}
Ho: theta <= Delta_lower, or theta >= Delta_upper,{p_end}
{p 8 8 }
where the equivalence interval ranges from theta+Delta_lower to 
theta+Delta_upper.  This also translates directly into two one-sided null 
hypotheses:{p_end}

{p 12}
Ho1: Delta_upper - theta <= 0; and{p_end}

{p 12}
Ho2: theta - Delta_lower <= 0{p_end}

{p 8}
-OR-

{p 8}
Ho: {it:Z} <= epsilon_lower, or {it:Z} >= epsilon_upper, and{p_end}

{p 12}
Ho1: epsilon_upper - {it:Z} <= 0; and{p_end}

{p 12}
Ho2: {it:Z} - epsilon_lower <= 0{p_end}

{pstd}
{it:Relevance testing} is a term used to describe inference based on the 
combined results of a test for difference, and a corresponding test for 
equivalence.  The {opt rel:evance()} option available to all commands except 
{cmd:tostppr} and {cmd:tostppri} provides conclusions from combined results for 
a specified {opt a:lpha()}, {opt eqvt:ype()}, and {opt eqvl:evel()}.  Relevance 
tests provide four possible conclusions:{p_end}

{p 8 8}
Reject Ho for test for difference, and not reject Ho for test for 
equivalence: conclude {inp:relevant difference}.{p_end}

{p 8 8}
Not reject Ho for test for difference, and reject Ho for test for 
equivalence: conclude {inp:equivalence}.{p_end}

{p 8 8}
Reject Ho for test for difference, and reject Ho for test for 
equivalence: conclude {inp:trivial difference} (i.e. there is evidence for a 
difference, but you have stated {it:a priori} that differences that small are 
not relevant).{p_end}

{p 8 8}
Not reject Ho for test for difference, and not reject Ho for test for 
equivalence: conclude {inp:indeterminate} (i.e. underpowered test for these data).{p_end}


{title:Author}

{pstd}Alexis Dinno{p_end}
{pstd}Portland State University{p_end}
{pstd}alexis.dinno@pdx.edu{p_end}

{pstd}
Development of {net "describe tost, from(https://alexisdinno.com/stata/)":tost} is 
ongoing. Please contact me with any questions, bug reports or suggestions for 
improvement.  Fixing bugs will be facilitated by sending along:{p_end}

{p 8 8 4}(1) a copy of the data (de-labeled or anonymized is fine),{p_end}
{p 8 8 4}(2) a copy of the command used, and{p_end}
{p 8 8 4}(3) a copy of the exact output of the command.{p_end}


{title:Suggested citation}

{p 4 8}
Dinno A.  2017.  {bf:tost}: Two one-sided tests for equivalence.  Stata software 
package.  URL: {view "https://www.alexisdinno.com/stata/tost.html"}{p_end}


{title:References}

{marker Schuirmann1987}{...}
{phang}
Schuirmann, D.  A.  1987.  {browse "https://pdfs.semanticscholar.org/053b/97e316fc43588e6235f88a1a7a4077342de7.pdf":A comparison of the two one-sided tests procedure and the power approach for assessing the equivalence of average bioavailability}.  
{it:Journal of Pharmacokinetics and Biopharmaceutics}.  15: 657-680

{title:Also See}

{psee}
{space 2}Help: {help tostmcc}, {help tostpr}, {help tostregress}, {help tostrrp}, {help tostranksum}, {help tostsignrank}, {help tostt}{p_end}
