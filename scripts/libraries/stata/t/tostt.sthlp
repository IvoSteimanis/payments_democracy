{smcl}
{* *! version 3.1.3  15may2021}{...}
{cmd:help tostt}
{hline}


{title:Title}

{p2colset 5 14 18 2}{...}
{p2col:{cmd:tostt} {hline 2}}Mean-equivalence {it:t} tests{p_end}
{p2colreset}{...}


{title:Syntax}

{pstd}
One-sample mean-equivalence {it:t} test

{p 8 14 2}
{cmd:tostt} {varname} {cmd:==} {it:#} {ifin}
        [{cmd:,} {opt eqvt:ype(type)}
        {opt eqvl:evel(#)}
        {opt upper:eqvlevel(#)}        
        {opt a:lpha(#)}
        {opt rel:evance}]


{pstd}
Two-sample unpaired mean-equivalence {it:t} test

{p 8 14 2}
{cmd:tostt} {varname:1} {cmd:==} {varname:2} {ifin}
        {cmd:, {ul on}unp{ul off}aired}
        [{opt eqvt:ype(type)}
        {opt eqvl:evel(#)}
        {opt upper:eqvlevel(#)}
        {opt une:qual}
        {opt w:elch}
        {opt a:lpha(#)}
        {opt rel:evance}]


{pstd}
Two-sample paired mean-equivalence {it:t} test

{p 8 14 2}
{cmd:tostt} {varname:1} {cmd:==} {varname:2} {ifin}
        [{cmd:,} {opt eqvt:ype(type)}
        {opt eqvl:evel(#)}
        {opt upper:eqvlevel(#)}
        {opt a:lpha(#)}
        {opt rel:evance}]


{pstd}
Two-group unpaired mean-equivalence {it:t} test

{p 8 14 2}
{cmd:tostt} {varname} {ifin}
        {cmd:, }{opth by:(varlist:groupvar)}
        [{opt eqvt:ype(type)}
        {opt eqvl:evel(#)}
        {opt upper:eqvlevel(#)}
        {opt une:qual}
        {opt w:elch}
        {opt a:lpha(#)}
        {opt rel:evance}]


{pstd}
Immediate form of one-sample mean-equivalence {it:t} test

{p 8 14 2}
{cmd:tostti}
        {it:#obs}
        {it:#mean}
        {it:#sd}
        {it:#val}
        [{cmd:,} {opt eqvt:ype(type)}
        {opt eqvl:evel(#)}
        {opt upper:eqvlevel(#)}
        {opt x:name(string)}
        {opt a:lpha(#)}
        {opt rel:evance}]


{pstd}
Immediate form of two-sample mean-equivalence {it:t} test

{p 8 14 2}
        {cmd:tostti}
        {it:#obs1}
        {it:#mean1}
        {it:#sd1}
        {it:#obs2}
        {it:#mean2}
        {it:#sd2}
        [{cmd:,} {opt eqvt:ype(type)}
        {opt eqvl:evel(#)}
        {opt upper:eqvlevel(#)}
        {opt une:qual}
        {opt w:elch}
        {opt x:name(string)}
        {opt y:name(string)}
        {opt a:lpha(#)}
        {opt rel:evance}]


{synoptset 28 tabbed}{...}
{synopthdr}
{synoptline}
{syntab:Miscellaneous}
{synopt :{opt eqvt:ype(string)}}specify equivalence threshold with Delta or epsilon{p_end}
{synopt :{opt eqvl:evel(#)}}the level of tolerance defining the equivalence interval{p_end}
{synopt :{opt upper:eqvlevel(#)}}the upper value of an asymmetric equivalence interval{p_end}
{synopt :{opt unp:aired}}the data are unpaired{p_end}
{synopt :{opth by:(varlist:groupvar)}}variable defining the two groups (implies {opt unp:aired}){p_end}
{synopt :{opt une:qual}}unpaired data have unequal variances{p_end}
{synopt :{opt w:elch}}use Welch's approximation (implies {opt une:qual}){p_end}
{synopt :{opt x:name(string)}}the name of the first variable{p_end}
{synopt :{opt y:name(string)}}the name of the second variable{p_end}
{synopt :{opt a:lpha(#)}}set nominal type I level; default is {opt a:lpha(0.05)}{p_end}
{synopt :{opt rel:evance}}perform & report combined tests for difference and equivalence{p_end}
{synoptline}
{p2colreset}{...}
{p 4 6 2}


{title:Description}

{pstd}
{cmd:tostt} tests for the equivalence of means within a symmetric equivalence 
interval defined by {opt eqvt:ype} and {opt eqvl:evel} using a two one-sided {it:t} 
tests approach ({help tostt##Schuirmann1987:Schuirmann, 1987}).  Typically null 
hypotheses are framed from an assumption of a lack of difference between two 
quantities, and reject this assumption only with sufficient evidence.  When 
performing tests for equivalence, one frames a null hypothesis with the 
assumption that two quantities are different within an equivalence interval 
defined by some chosen level of tolerance (as specified by {opt eqvt:ype} and 
{opt eqvl:evel}).{p_end}

{pstd}
With respect to an unpaired {it:t} test, an equivalence null hypothesis takes one of 
the following two forms depending on whether equivalence is defined in terms of
Delta (equivalence expressed in the same units as the {it:x} and {it:y}) or in terms of 
epsilon (equivalence expressed in the units of the {it:T} distribution with the 
given degrees of freedom):
 
{p 8}
Ho: |mean({it:x}) - mean({it:y})| >= Delta, {p_end}
{p 8 8}where the equivalence interval ranges from diff-Delta to diff+Delta, and 
where diff is either the mean difference or the difference in means depending on
whether the test is paired or unpaired.  This translates directly into two 
one-sided null hypotheses: {p_end}

{p 12}
Ho1: Delta - [mean({it:x}) - mean({it:y})] <= 0; and{p_end}

{p 12}
Ho2: [mean({it:x}) - mean({it:y})] + Delta <= 0{p_end}

{p 8}
-OR-

{p 8}
Ho: |{it:T}| >= epsilon, {p_end}
{p 8 8}where the equivalence interval ranges from -epsilon to epsilon.  This also 
translates directly into two one-sided null hypotheses: {p_end}

{p 12}
Ho1: epsilon - {it:T} <= 0; and{p_end}

{p 12}
Ho2: {it:T} + epsilon <= 0{p_end}

{p 8 8}
When an asymmetric equivalence interval is defined using the {opt upper:eqvlevel} option 
the general negativist null hypothesis becomes:{p_end}

{p 8}
Ho: [mean({it:x}) - mean({it:y})] <= Delta_lower, or [mean({it:x}) - mean({it:y})] >= Delta_upper,{p_end}
{p 8 8 }
where the equivalence interval ranges from [mean({it:x}) - mean({it:y})] + Delta_lower to 
[mean({it:x}) - mean({it:y})] + Delta_upper.  This also translates directly into two 
one-sided null hypotheses:{p_end}

{p 12}
Ho1: Delta_upper - [mean({it:x}) - mean({it:y})] <= 0; and{p_end}

{p 12}
Ho2: [mean({it:x}) - mean({it:y})] - Delta_lower <= 0{p_end}

{p 8}
-OR-

{p 8}
Ho: {it:T} <= epsilon_lower, or {it:T} >= epsilon_upper,{p_end}

{p 12}
Ho1: epsilon_upper - {it:T} <= 0; and{p_end}

{p 12}
Ho2: {it:T} - epsilon_lower <= 0{p_end}
 
{pstd}
NOTE: the appropriate level of {opt a:lpha} is precisely the same as in the 
corresponding two-sided test of mean difference, so that, for example, if one 
wishes to make a type I error %5 of the time, one simply conducts both of the 
one-sided tests of Ho1 and Ho2 by comparing the resulting p-value to 0.05 
({help tostt##Tryon2008:Tryon and Lewis, 2008}).{p_end}


{title:Options}

{dlgtab:Main}

{phang}
{opth eqvt:ype(string)} defines whether the equivalence interval will be 
defined in terms of Delta or epsilon ({opt delta}, or {opt epsilon}).  These 
options change the way that {opt evql:evel} is interpreted: when {opt delta} is 
specified, the {opt evql:evel} is measured in the units of the variable being 
tested, and when {opt epsilon} is specified, the {opt evql:evel} is measured in 
multiples of the standard deviation of the {it:T} distribution; put another way 
epsilon = Delta/standard error.  The default is {opt delta}.{p_end}

{marker mineqvlevel}{...}
{p 8 8}
Defining tolerance in terms of epsilon means that it is not possible to reject 
any test of mean equivalence Ho if epsilon <= the critical value of {it:t} for a 
given {opt a:lpha} and degrees of freedom.  Because epsilon = Delta/standard error, we 
can see that it is not possible to reject any Ho if Delta <= the product of the 
standard error and critical value of {it:t} for a given {opt a:lpha} and degrees of freedom.
{cmd: tostt} and {cmd: tostti} now report when either of these conditions obtain.{p_end}

{phang}
{opth eqvl:evel(#)} defines the equivalence threshold for the tests depending on 
whether {opt eqvt:ype} is {opt delta} or {opt epsilon} (see above).  Researchers 
are responsible for choosing meaningful values of Delta or epsilon.  The default 
value is 1 when {opt delta} is the {opt eqvt:ype} and 2 when {opt epsilon} is the {opt eqvt:ype}.{p_end}

{phang}
{opt upper:eqvlevel(#)} defines the {it: upper} equivalence threshold for the test, 
and transforms the meaning of {opt eqvl:evel} to mean the {it: lower} equivalence 
threshold for the test.  Also, {opt eqvl:evel} is assumed to be a negative value.  
Taken together, these correspond to Schuirmann's ({help tostt##Schuirmann1987:1987}) 
asymmetric equivalence intervals.  If {opt upper:eqvlevel}==|{opt eqvl:evel}|, then 
{opt upper:eqvlevel} will be ignored.{p_end}

{phang}
{opth by:(varlist:groupvar)} specifies the {it:groupvar} that defines the two
groups that {cmd:tostt} will use to test the hypothesis that their means are
different.  Specifying {opt by(groupvar)} implies an unpaired (two sample) {it:t} 
test.  Do not confuse the {opt by()} option with the {cmd:by} prefix; you can 
specify both.{p_end}

{phang}
{opt unp:aired} specifies that the data be treated as unpaired.  The 
{opt unp:aired} option is used when the two set of values to be compared are 
in different variables.{p_end}

{phang}
{opt une:qual} specifies that the unpaired data not be assumed to have equal 
variances.

{phang}
{opt w:elch} specifies that the approximate degrees of freedom for the test 
be obtained from Welch's formula ({help tostt##Welch1947:1947}) rather than 
Satterthwaite's approximation formula ({help ttest##Satterthwaite1946:1946}), 
which is the default when {opt une:qual} is specified.  Specifying {opt w:elch} 
implies {opt une:qual}.{p_end}

{phang}
{opt x:name(string)} specifies how the first variable will be labeled in the 
output.  The default value of {opt x:name} is {cmd:x}.

{phang}
{opt y:name(string)} specifies how the second variable will be labeled in the 
output.  The default value of {opt y:name} is {cmd:y}.

{phang}
{opt a:lpha(#)} specifies the nominal type I error rate.  The default is {opt a:lpha(0.05)}.

{phang}
{opt rel:evance} reports results and inference for combined tests for difference 
and equivalence for a specific {opt a:lpha}, {opt eqvt:ype}, and {opt eqvl:evel}.  
See the Remarks section more details on inference from combined tests.


{title:Remarks}

{pstd}
As described by Tryon and Lewis ({help tostt##Tryon2008:2008}), when both tests 
for difference and equivalence are taken together, there are four possible 
interpretations:{p_end}

{p 4 8 2}
1.  One may reject the null hypothesis of no difference, but fail to reject the 
null hypothesis of difference, and conclude that there is a {it: relevant difference} 
in means at least as large as Delta or epsilon.{p_end}

{p 4 8 2}
2.  One may fail to reject the null hypothesis of no difference, but reject the 
null hypothesis of difference, and conclude that the means are {it: equivalent} 
within the equivalence range (i.e.  defined by Delta or epsilon).{p_end}

{p 4 8 2}
3.  One may reject {it:both} the null hypothesis of no difference and the null 
hypothesis of difference, and conclude that the means are {it: trivially different}, 
within the equivalence range (i.e.  defined by Delta or epsilon).{p_end}

{p 4 8 2}
4.  One may fail to reject {it:both} the null hypothesis of no difference and the 
null hypothesis of difference, and draw an {it: indeterminate} conclusion, because 
the data are underpowered to detect difference or equivalence.{p_end}


{title:Examples}

{pstd}
These examples correspond to those written in the help file for 
{help ttest:ttest}:{p_end}

    {cmd:.  sysuse auto}                                   (setup)
    {inp:.  tostt mpg==20, eqvt(delta) eqvl(2.5) upper(3)} (one-sample mean-equivalence test)

    {cmd:.  webuse fuel}                                   (setup)
    {cmd:.  tostt mpg1==mpg2, eqvt(epsilon) eqvl(3) rel}   (two-sample paired mean relevance test)

    {cmd:.  webuse fuel3}                                  (setup)
    {cmd:.  tostt mpg, by(treated) eqvt(delta) eqvl(1.5)}    (two-group unpaired mean-comparison test)
                                                           (note warning about value of Delta!)
    
                                 (no setup required)
    {cmd:.  tostti 24 62.6 15.8 75, rel}                   (immediate form; n=24, m=62.6, sd=15.8;
                                                                test m=75)


{title:Saved results}

{pstd}
The one-sample form of {cmd:tostt} saves the following in 
{cmd:r()}:

{synoptset 15 tabbed}{...}
{p2col 5 15 19 2: Scalars}{p_end}
{synopt:{cmd:r(sd_1)}}standard deviation for the variable{p_end}
{synopt:{cmd:r(se)}}estimate of standard error{p_end}
{synopt:{cmd:r(p2)}}P({it:T} >= {it:t}2); upper one-sided p-value under Ho2{p_end}
{synopt:{cmd:r(p1)}}P({it:T} >= {it:t}1); upper one-sided p-value under Ho1{p_end}
{synopt:{cmd:r(t2)}}{it:t} statistic under Ho2{p_end}
{synopt:{cmd:r(t1)}}{it:t} statistic under Ho1{p_end}
{synopt:{cmd:r(df_t)}}degrees of freedom{p_end}
{synopt:{cmd:r(mu_1)}}{it:x}_1 bar, mean for the population{p_end}
{synopt:{cmd:r(N_1)}}sample size {it:n}_1{p_end}
{synopt:{cmd:r(Delta)}}Delta, tolerance level defining the equivalence interval; OR{p_end}
{synopt:{cmd:r(Du)}}Delta_upper, tolerance level defining the equivalence interval's upper side; AND{p_end}
{synopt:{cmd:r(Dl)}}Delta_lower, tolerance level defining the equivalence interval's lower side; OR{p_end}
{synopt:{cmd:r(epsilon)}}epsilon, tolerance level defining the equivalence interval{p_end}
{synopt:{cmd:r(eu)}}epsilon_upper, tolerance level defining the equivalence interval's upper side; AND{p_end}
{synopt:{cmd:r(el)}}epsilon_lower, tolerance level defining the equivalence interval's lower side{p_end}
{synopt:{cmd:r(relevance)}}Relevance test conclusion for given alpha and Delta/epsilon{p_end}
{p2colreset}{...}

{pstd}
The two-sample and two-group forms of {cmd:tostt} save the following in 
{cmd:r()}:

{synoptset 15 tabbed}{...}
{p2col 5 15 19 2: Scalars}{p_end}
{synopt:{cmd:r(sd_2)}}standard deviation for second variable{p_end}
{synopt:{cmd:r(sd_1)}}standard deviation for first variable{p_end}
{synopt:{cmd:r(se)}}estimate of standard error{p_end}
{synopt:{cmd:r(p2)}}P({it:T} >= {it:t}2); upper one-sided p-value under Ho2{p_end}
{synopt:{cmd:r(p1)}}P({it:T} >= {it:t}1); upper one-sided p-value under Ho1{p_end}
{synopt:{cmd:r(t2)}}{it:t} statistic under Ho2{p_end}
{synopt:{cmd:r(t1)}}{it:t} statistic under Ho1{p_end}
{synopt:{cmd:r(df_t)}}degrees of freedom{p_end}
{synopt:{cmd:r(mu_2)}}{it:x}_2 bar, mean for population 2{p_end}
{synopt:{cmd:r(N_2)}}sample size {it:n}_2{p_end}
{synopt:{cmd:r(mu_1)}}{it:x}_1 bar, mean for population 1{p_end}
{synopt:{cmd:r(N_1)}}sample size {it:n}_1{p_end}
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

{pstd}
I am endebted to my winter 2013 students for their inspiration.


{title:Suggested citation}

{p 4 8}
Dinno A.  2017.  {bf:tostt}: Mean-equivalence {it:t} tests.  Stata software 
package.  URL: {view "https://www.alexisdinno.com/stata/tost.html"}{p_end}


{title:References}

{marker Satterthwaite1946}{...}
{phang}
Satterthwaite, F.  E.  1946.  {browse "https://www.jstor.org/stable/3002019":An approximate distribution of estimates of variance components}.  {it:Biometrics Bulletin} 2: 110-114.

{marker Schuirmann1987}{...}
{phang}
Schuirmann, D.  A.  1987.  {browse "https://pdfs.semanticscholar.org/053b/97e316fc43588e6235f88a1a7a4077342de7.pdf":A comparison of the two one-sided tests procedure and the power approach for assessing the equivalence of average bioavailability}.  
{it:Journal of Pharmacokinetics and Biopharmaceutics}.  15: 657-680

{marker Tryon2008}{...}
{phang}
Tryon, W.  W., and C.  Lewis.  2008.  
{browse "https://sci-hub.io":An inferential confidence interval method of establishing statistical equivalence that corrects Tryon’s (2001) reduction factor}.  {it:Psychological Methods}.  13: 272-277

{marker Welch1947}{...}
{phang}
Welch, B.  L.  1947.  {browse "https://www.jstor.org/stable/2332510":The generalization of “Student’s” problem when several different population variances are involved}.  {it:Biometrika} 34: 28-35.


{title:Also See}

{psee}
{space 2}Help: {help tost:tost}, {help pkequiv:pkequiv}, {help ttest:ttest}{p_end}
