{smcl}
{* *! version 3.1.3  15may2021}{...}
{cmd:help tostranksum}
{hline}


{title:Title}

{p2colset 5 20 18 2}{...}
{p2col:{cmd:tostranksum} {hline 2}}Two-sample rank sum test for stochastic equivalence{p_end}
{p2colreset}{...}


{marker syntax}{...}
{title:Syntax}

{phang}
Two-sample stochastic equivalence rank sum test

{p 8 19 2}
{cmd:ranksum} {varname} {ifin}{cmd:,}
    {cmd:by(}{it:{help varlist:groupvar}}{cmd:)} 
    [{cmd:,} {opt eqvt:ype(type)}
    {opt eqvl:evel(#)}
    {opt upper:eqvlevel(#)}
    {opt cc:ontinuity}
    {opt a:lpha(#)}
    {opt rel:evance}]


{synoptset 21 tabbed}{...}
{synopthdr:tostranksum options}
{synoptline}
{syntab:Main}
{p2coldent:* {opth by:(varlist:groupvar)}}grouping variable{p_end}
{synopt :{opt eqvt:ype(string)}}specify equivalence threshold with Delta or epsilon{p_end}
{synopt :{opt eqvl:evel(#)}}the level of tolerance defining the equivalence interval{p_end}
{synopt :{opt upper:eqvlevel(#)}}the upper value of an asymmetric equivalence interval{p_end}
{synopt :{opt cc:ontinuity}}include a continuity correction{p_end}
{synopt :{opt a:lpha(#)}}set nominal type I level; default is {opt a:lpha(0.05)}{p_end}
{synopt :{opt rel:evance}}perform & report combined tests for difference and equivalence{p_end}
{synoptline}
{p2colreset}{...}
{p 4 6 2}* {opt by(groupvar)} is required.{p_end}
{p 4 6 2}{opt by} is allowed with {cmd:tostranksum} see {manhelp by D}.{p_end}


{marker description}{...}
{title:Description}

{pstd}
{cmd:tostranksum} tests the stochastic dominance of two independent samples (that 
is, unpaired or unmatched data) by using the {it:z} approximation to the Wilcoxon 
rank-sum test, which was generalized to different sample sizes by the 
Mann-Whitney two-sample statistic ({help tostranksum##W1945:Wilcoxon 1945}; 
{help tostranksum##MW1947:Mann and Whitney 1947}) in a two one-sided tests 
approach ({help tostranksum##Schuirmann1987:Schuirmann, 1987}).  Typically 
rank sum null hypotheses are framed from an assumption of stochastic equality 
(or distributional sameness) between two populations (e.g. Ho: P({it:X} > {it:Y}) = 0.5), 
rejecting this assumption only with sufficient evidence.  When performing tests 
of stochastic equivalence, the null hypothesis is framed as one population 
stochastically dominates the other by at least as much as the equivalence 
interval defined by some chosen level of tolerance (as specified by {opt eqvt:ype} 
and {opt eqvl:evel}).{p_end}


{pstd}
With respect to the rank sum test, a negativist null hypothesis takes one of 
the following two forms depending on whether tolerance is defined in terms of
Delta (equivalence expressed in the same units as the summed ranks) or in terms 
of epsilon (equivalence expressed in the units of the {it:Z} distribution):
 
{p 8}
Ho: |{it:W} - E({it:W})| >= Delta, {p_end}
{p 8 8}where the equivalence interval ranges from ({it:W} - E({it:W}))-Delta to 
({it:W} - E({it:W}))+Delta, and where {it:W} is the rank-sum statistic and E({it:W}) is its mean 
if there is no stochastic dominance. This translates directly into two one-sided 
null hypotheses: {p_end}

{p 12}
Ho1: Delta - [{it:W} - E({it:W})] <= 0; and{p_end}

{p 12}
Ho2: [{it:W} - E({it:W})] + Delta <= 0{p_end}

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
Ho: [{it:W} - E({it:W})] <= Delta_lower, or [{it:W} - E({it:W})] >= Delta_upper,{p_end}
{p 8 8 }
where the equivalence interval ranges from [{it:W} - E({it:W})] + Delta_lower to 
[{it:W} - E({it:W})] + Delta_upper.  This also translates directly into two one-sided null 
hypotheses:{p_end}

{p 12}
Ho1: Delta_upper - [{it:W} - E({it:W})] <= 0; and{p_end}

{p 12}
Ho2: [{it:W} - E({it:W})] - Delta_lower <= 0{p_end}

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
corresponding two-sided test for stochastic dominance, so that, for example, if 
one wishes to make a type I error %5 of the time, one simply conducts both of 
the one-sided tests of Ho1 and Ho2 by comparing the resulting p-value to 0.05 
({help tostranksum##Wellek2010:Wellek, 2010}).{p_end}

{pstd}
{cmd:tostranksum} is for use with unpaired/unmatched data.  For
equivalence tests on paired/matched data, see {help tostsignrank:tostsignrank}.


{marker options_ranksum}{...}
{title:Options for ranksum}

{dlgtab:Main}

{phang}
{cmd:by(}{it:{help varlist:groupvar}}{cmd:)} is required.  It specifies the
name of the grouping variable.

{phang}
{opth eqvt:ype(string)} defines whether the equivalence interval will be 
defined in terms of Delta or epsilon ({opt delta}, or {opt epsilon}).  These 
options change the way that {opt evql:evel} is interpreted: when {opt delta} is 
specified, the {opt evql:evel} is measured in the units of the rank sums, and 
when {opt epsilon} is specified, the {opt evql:evel} is measured in 
multiples of the standard deviation of the {it:Z} distribution; put another way 
epsilon = Delta/standard error.  The default is {opt epsilon}.{p_end}

{marker mineqvlevel}{...}
{p 8 8}
Defining tolerance in terms of epsilon means that it is not possible to reject 
any test of mean equivalence Ho if epsilon <= the critical value of {it:z} for a 
given {opt a:lpha}.  Because epsilon = Delta/standard error, we can see that it is not 
possible to reject any Ho if Delta <= the product of the standard error and 
critical value of {it:z} for a given {opt a:lpha}.  {cmd: tostranksum} reports when either of 
these conditions obtain.  Given that the variance of rank sum distributions 
can be very large, tolerance should be specified using {opt delta} only with 
great care{p_end}

{phang}
{opth eqvl:evel(#)} defines the equivalence threshold for the tests depending on 
whether {opt eqvt:ype} is {opt delta} or {opt epsilon} (see above).  Researchers 
are responsible for choosing meaningful values of Delta or epsilon.  The default 
value is 1 (certain to be meaningless) when {opt delta} is the {opt eqvt:ype} and 
2 when {opt epsilon} is the {opt eqvt:ype}.{p_end}

{phang}
{opt upper:eqvlevel(#)} defines the {it: upper} equivalence threshold for the test, 
and transforms the meaning of {opt eqvl:evel} to mean the {it: lower} equivalence 
threshold for the test.  Also, {opt eqvl:evel} is assumed to be a negative value.  
Taken together, these correspond to Schuirmann's ({help tostranksum##Schuirmann1987:1987}) 
asymmetric equivalence intervals.  If {opt upper:eqvlevel}==|{opt eqvl:evel}|, then 
{opt upper:eqvlevel} will be ignored.{p_end}

{phang}
{opt cc:ontinuity} specifies that the test statistics incorporate a continuity 
correction using |W-E(W)|-0.5, but retaining the sign of the z-statistic after 
the correction has been applied (see {opt eqv:type} above).{p_end}

{phang}
{opt a:lpha(#)} specifies the nominal type I error rate.  The default is {opt a:lpha(0.05)}.

{phang}
{opt rel:evance} reports results and inference for combined tests for stochastic 
difference and stochastic equivalence for a specific {opt a:lpha}, {opt eqvt:ype}, 
and {opt eqvl:evel}.  See the end of the Discussion section in {help tost} for 
more details on inference from combined tests.


{marker examples}{...}
{title:Examples}

    {hline}
{pstd}Setup{p_end}
{phang2}{cmd:. webuse fuel2}{p_end}

{pstd}Perform rank-sum equivalence test on {cmd:mpg} by using the two groups 
defined by {cmd:treat}{p_end}
{phang2}{cmd:. tostranksum mpg, by(treat) eqvt(epsilon) eqvl(3)}{p_end}

{pstd}Perform asymmetric rank-sum equivalence test on {cmd:mpg} by using the 
two groups defined by {cmd:treat}, and add a continuity correction{p_end}
{phang2}{cmd:. tostranksum mpg, by(treat) eqvt(epsilon) eqvl(3) upper(2.5) cc}{p_end}
    {hline}

{marker saved_results}{...}
{title:Saved results}

{pstd}
{cmd:tostranksum} saves the following in {cmd:r()}:

{synoptset 15 tabbed}{...}
{p2col 5 15 19 2: Scalars}{p_end}
{synopt:{cmd:r(N_1)}}sample size n_1{p_end}
{synopt:{cmd:r(N_2)}}sample size n_2{p_end}
{synopt:{cmd:r(z1)}}{it:z} statistic for Ho1 (upper){p_end}
{synopt:{cmd:r(z2)}}{it:z} statistic for Ho2 (lower){p_end}
{synopt:{cmd:r(p1)}}P({it:Z} >= {it:z}1){p_end}
{synopt:{cmd:r(p2)}}P({it:Z} >= {it:z}2){p_end}
{synopt:{cmd:r(Var_a)}}adjusted variance{p_end}
{synopt:{cmd:r(group1)}}value of variable for first group{p_end}
{synopt:{cmd:r(sum_obs)}}actual sum of ranks for first group{p_end}
{synopt:{cmd:r(sum_exp)}}expected sum of ranks for first group{p_end}
{synopt:{cmd:r(Delta)}}Delta, tolerance level defining the equivalence interval; OR{p_end}
{synopt:{cmd:r(Du)}}Delta_upper, tolerance level defining the equivalence interval's upper side; AND{p_end}
{synopt:{cmd:r(Dl)}}Delta_lower, tolerance level defining the equivalence interval's lower side; OR{p_end}
{synopt:{cmd:r(epsilon)}}epsilon, tolerance level defining the equivalence interval{p_end}
{synopt:{cmd:r(eu)}}epsilon_upper, tolerance level defining the equivalence interval's upper side; AND{p_end}
{synopt:{cmd:r(el)}}epsilon_lower, tolerance level defining the equivalence interval's lower side{p_end}
{synopt:{cmd:r(relevance)}}Relevance test conclusion for given alpha and Delta/epsilon{p_end}


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
Dinno A.  2017. {bf:tostranksum}: Two-sample rank sum test for stochastic 
equivalence.  Stata software package.  URL: {view "https://www.alexisdinno.com/stata/tost.html"}{p_end}


{marker references}{...}
{title:References}

{marker MW1947}{...}
{phang}
Mann, H. B., and D. R. Whitney. 1947. {browse "https://www.jstor.org/stable/2236101":On a test whether one of two random variables is stochastically larger than the other}.
{it:Annals of Mathematical Statistics} 18: 50-60.

{marker Schuirmann1987}{...}
{phang}
Schuirmann, D.  A.  1987.  {browse "https://pdfs.semanticscholar.org/053b/97e316fc43588e6235f88a1a7a4077342de7.pdf":A comparison of the two one-sided tests procedure and the power approach for assessing the equivalence of average bioavailability}.  
{it:Journal of Pharmacokinetics and Biopharmaceutics}.  15: 657-680

{marker Wellek2010}{...}
{phang}
Wellek, S. 2010. {browse "https://www.crcpress.com/product/isbn/9781439808184":{it:Testing Statistical Hypotheses of Equivalence and Noninferiority}}, 
second edition.  Chapman and Hall/CRC Press. p. 31{p_end}

{marker W1945}{...}
{phang}
Wilcoxon, F.  1945.  {browse "http://www.jstor.org/stable/3001968":Individual comparisons by ranking methods}.
{it:Biometrics Bulletin} 1: 80-83.
{p_end}


{title:Also See}

{psee}
{space 2}Help: {help tost:tost}, {help pkequiv:pkequiv}, {help ranksum:ranksum}{p_end}

