*--------------------------------------------------------------------------------------
* SCRIPT: 02_generate.do
* PURPOSE: generates additional variables used in the analysis
*--------------------------------------------------------------------------------------



clear
use "$working_ANALYSIS\processed\data_clean.dta"

// GAME 1: PROCEDURAL FAIRNESS

rename rule_1 rule1
rename rule_2 rule2
rename rule_3 rule3
rename rule_4 rule4
rename rule_bonus_1 rule5
rename rule_bonus_2 rule6

rename vote_1 vote1
rename vote_2 vote2
rename vote_3 vote3
rename vote_4 vote4
rename vote_bonus_1 vote5
rename vote_bonus2 vote6

rename majority_1 majority1
rename majority_2 majority2
rename majority_3 majority3
rename majority_4 majority4
rename majority_bonus1 majority5
rename majority_bonus2 majority6


// Create dummies for democratic and autocratic choices
*democratic
gen dem1=0
replace dem1=1 if rule1==1
gen dem2=0
replace dem2=1 if rule2==1
gen dem3=0
replace dem3=1 if rule3==1
gen dem4=0
replace dem4=1 if rule4==1
gen dem5=0
replace dem4=1 if rule5==1
gen dem6=0
replace dem4=1 if rule6==1
egen dem7 = rowmean(dem1 dem2 dem3 dem4)

*dictator
gen dic1=0
replace dic1=1 if rule1==2
gen dic2=0
replace dic2=1 if rule2==2
gen dic3=0
replace dic3=1 if rule3==2
gen dic4=0
replace dic4=1 if rule4==2
gen dic5=0
replace dic5=1 if rule5==2
gen dic6=0
replace dic6=1 if rule6==2

*pseudo-democratic
gen fake1=0
replace fake1=1 if rule1==3
gen fake2=0
replace fake2=1 if rule2==3
gen fake3=0
replace fake3=1 if rule3==3
gen fake4=0
replace fake4=1 if rule4==3
gen fake5=0
replace fake5=1 if rule5==3
gen fake6=0
replace fake6=1 if rule6==3

replace rule1=1 if dem1==1
replace rule1=2 if fake1==1
replace rule1=3 if dic1==1
replace rule2=1 if dem2==1
replace rule2=2 if fake2==1
replace rule2=3 if dic2==1
replace rule3=1 if dem3==1
replace rule3=2 if fake4==1
replace rule3=3 if dic4==1
replace rule4=1 if dem4==1
replace rule4=2 if fake4==1
replace rule4=3 if dic4==1
replace rule5=1 if dem5==1
replace rule5=2 if fake5==1
replace rule5=3 if dic5==1
replace rule6=1 if dem6==1
replace rule6=2 if fake6==1
replace rule6=3 if dic6==1

*subsample autocratic rule choices
gen pseudo1=0
replace pseudo1=1 if rule1==3
replace pseudo1=. if rule1==1
gen pseudo2=0
replace pseudo2=1 if rule2==3
replace pseudo2=. if rule2==1
gen pseudo3=0
replace pseudo3=1 if rule3==3
replace pseudo3=. if rule3==1
gen pseudo4=0
replace pseudo4=1 if rule4==3
replace pseudo4=. if rule4==1
egen pseudo_all = rowmean(pseudo1 pseudo2 pseudo3 pseudo4)

*same rule across all three rounds
gen stable_rule=0
replace stable_rule=1 if rule1==rule5==rule6

*game1_group
*devide each village into two groups of game 1: TL or DEL (13=TL, 14=DEL)?
gen game1_group = 13
replace game1_group =14 if session_id==1 | session_id==2 | session_id==5 | session_id==6 | session_id==9 | session_id==10 | session_id==14
order game1_group, before(vote1)

*villager_preference1
*now the villager preferences, i.e. majority without leader (6 villager per group, a draw is therefore possible)
* villager_preference1 ("A", "B" or "draw") gives the preferences of the six villager (without leader) in the first decision of game 1
egen count_A = total(vote1==1) if id>64, by(village game1_group)
order count_A, after(vote1)

replace count_A = count_A[_n-1] if session_id==13
replace count_A = count_A[_n-4] if session_id==14

gen villager_preference1="A"
replace villager_preference1 = "B" if count_A<3
replace villager_preference1 = "draw" if count_A==3
order villager_preference1, after(vote1)
label variable villager_preference "majority vote of 6 villagers over distribution A and B"


drop count_A

*Bonus order
gen bonus_order=. if id >64
replace bonus_order=1 if  randomization<7
replace bonus_order=0 if  randomization>6 
label def bonus_order 1 "Small first" 0 "Large first", replace
lab val bonus_order bonus_order

*Identifier for first bonus that was offered to leaders
gen rule_bonus_first = rule5 if bonus_order==1
replace rule_bonus_first = rule6 if bonus_order==0
gen rule_bonus_second = rule5 if bonus_order==0
replace rule_bonus_second = rule6 if bonus_order==1

*New variable to identify baseline rule choices
gen rule_base=rule1
lab def rulebasic 1 "Base: Democratic" 2 "Base: Pseudo" 3 "Base: Autocratic", replace
lab val rule_base rulebasic
bysort village game1_group: egen group_rule_base = mean(rule_base)
lab val group_rule_base rulebasic
tab group_rule_base, gen(rule_base)

* label rule choices consistently
lab def rule 1 "Democratic" 2 "Pseudo-democratic" 3 "Autocratic" , replace
foreach x of varlist rule1 rule5 rule6 rule_bonus_first rule_bonus_second {
	lab val `x' rule
	}

*payoff1, payoff5, payoff6
*what do these villagers get?
*Basic
gen payoff1 = 0 if id<65
replace payoff1 = 1 if (id<65) & ((rule1==1 & majority1=="B") | (rule1!=1 & vote1==2))
label variable payoff1 "distribution A or B that is paid to the group in decision 1"

*Small Bonus
gen payoff5 = 0 if id<65
replace payoff5 = 1 if (id<65) & ((rule5==1 & majority5=="B") | (rule5!=1 & vote5==2))
label variable payoff5 "distribution A or B that is paid to the group in small bonus"

*Large Bonus
gen payoff6 = 0 if id<65
replace payoff6 = 1 if (id<65) & ((rule6==1 & majority6=="B") | (rule6!=1 & vote6==2))
label variable payoff6 "distribution A or B that is paid to the group in large bonus"

lab def payoff_lab 0 "A" 1 "B"
foreach var of varlist payoff1 payoff5 payoff6 {
	lab val `var' payoff_lab
	}
	
order payoff1 payoff5 payoff6, after(villager_preference1)
gsort village game1_group -session_id

by village game1_group: replace payoff1 = payoff1[_n-1] if missing(payoff1)
by village game1_group: replace payoff5 = payoff5[_n-1] if missing(payoff5)
by village game1_group: replace payoff6 = payoff6[_n-1] if missing(payoff6)

sort village session_id



// Leadertypes based on baseline stage 1 & stage 2 decisions
gen true_democrat=0
replace true_democrat=1 if rule1==1 
gen pseudo_democrat=0
replace pseudo_democrat=1 if rule1==2 & vote1==1
gen paternalistic=0
replace paternalistic=1 if rule1==2 & vote1==2
gen dictator=0
replace dictator=1 if rule1==3 & vote1==1
gen benevolent_dictator=0
replace benevolent_dictator=1 if rule1==3 & vote1==2

gen leader_type=1 if true_democrat==1
replace leader_type=2 if pseudo_democrat==1
replace leader_type=3 if paternalistic==1
replace leader_type=4 if dictator==1
replace leader_type=5 if benevolent_dictator==1
lab def lab_types 1 "True-democrat" 2 "Pseudo-democrat" 3 "Paternalist" 4 "Autocrat" 5 "Benevolent-Autocrat"
lab val leader_type lab_types


bysort village game1_group: egen group_leader_type = mean(leader_type)
lab val group_leader_type lab_types

lab def vote_lab 1 "Distribution A" 2 "Distribution B"
lab val vote1 vote_lab
tab group_leader_type, gen(leader_type)



************************************
*** 1C: Preference game (Game 2)

gen social_type=6 
replace social_type = 1 if decision1==1 & decision2==1 & decision3==1
replace social_type = 2 if decision1==1 & decision2==1 & decision3==2
replace social_type = 3 if decision1==1 & decision2==2 & decision3==1
replace social_type = 4 if decision1==1 & decision2==2 & decision3==2
replace social_type = 5 if decision1==2 & decision2==1 & decision3==2

gen social_type2 =.
replace social_type2=1 if (social_type==1 | social_type==2)
replace social_type2=2 if (social_type==3 | social_type==4)
replace social_type2=3 if social_type==5
replace social_type2=4 if social_type2==. & social_type==6

# delimit;
label define social
1	"Strongly egalitarian"
2	"Weakly egalitarian"
3   "Strongly generous"
4	"Weakly generous"
5	"Spiteful"
6	"Ambiguous";
label define social2
1	"Egalitarian"
2	"Generous"
3   "Spiteful"
4	"Ambiguous";
# delimit cr
label values  social_type social
label values  social_type2 social2



// Gen dummy for no education
gen no_educ = 0 if education>0 & education!=.
replace no_educ =1 if education==0

// age-squared
gen age_sq = age*age

// intrinsic motivation - binary variable:
* Based on the definition of Frey & Oberholzer (1997) ->
* intrinsic(=1): one likes to do them or because the individual derives some satisfaction from doing  his or her duty
*extrinsic(=0): activated from the outside
* if there are both intrinsic and extrinsic motives mentioned, the variable takes the value 1 - there can be some intrinsic motives crowded out by the bonus paymen 

gen motivation=.
replace motivation=0 if f6=="accepted because people but trust in her"
replace motivation=0 if f6=="just accepted what the villagers voted."
replace motivation=1 if f6=="to help the community mantain water"
replace motivation=1 if f6=="because I believe that I was born with characteristic of a leader."
replace motivation=0 if f6=="i just accepted because the villagers appointed me"
replace motivation=1 if f6=="to lead the community. A calling from god. Care for the community"
replace motivation=1 if f6=="to train the committee members to be in goood communication / relationship with tap users"
replace motivation=1 if f6=="accepted the task attributed to him and help his community"
replace motivation=0 if f6=="just accepted the vote"
replace motivation=1 if f6=="Assist and meet the needs of the villagers"
replace motivation=1 if f6=="to care and protect the use of water"
replace motivation=0 if f6=="he was chosen and just accepted"
replace motivation=0 if f6=="accepted responsibility"
replace motivation=1 if f6=="to lead the community"
replace motivation=1 if f6=="volunteer to help bring water to the village, help the community"
replace motivation=1 if f6=="to lead the community"
replace motivation=1 if f6=="help his community"
replace motivation=0 if f6=="was obliged, to lead the village, to bring development to the village"
replace motivation=1 if f6=="guarantee availability of water"
replace motivation=0 if f6=="Accepted because it is a calling by God. Did not want to disappoint the father and villagers."
replace motivation=1 if f6=="to help the community, to keep or take care of the water pipes"
replace motivation=1 if f6=="to fight for the good of the community. To bring it up to standard."
replace motivation=0 if f6=="because there was a lack of people to be in the committee"
replace motivation=0 if f6=="accepted because there was no on else to care and protect the community and it's members"
replace motivation=1 if f6=="arrange meetings, have better / good communication with village, report to senior headman"
replace motivation=0 if f6=="I just accepted the position, because the villagers wanted me to lead them."
replace motivation=1 if f6=="to take care of the water tap"
replace motivation=1 if f6=="lead the community. Care for the community"
replace motivation=1 if f6=="keep water in the community"
replace motivation=0 if f6=="There was no election. Just accepted to be the headman as my father appointed me."
replace motivation=1 if f6=="to lead the community, to take of the water tap"
replace motivation=0 if f6=="just accepted a responsibility given by elders and villagers."
replace motivation=1 if f6=="serving the community, take care of water related needs"
replace motivation=1 if f6=="to lead the community as given by god."
replace motivation=1 if f6=="care and protect the tap, sustain water"
replace motivation=0 if f6=="Just accepted because my late father appointed me and also the villagers trusted/trust me to lead them"
replace motivation=0 if f6=="was appointed to be one of the candidates, just accepted the result of the vote"
replace motivation=0 if f6=="just accepted because people trusted me."
replace motivation=1 if f6=="was given a responsibility and accepted, wanted to help bring private taps to village"
replace motivation=0 if f6=="took over family duties. Was not willing to give the village away to a stranger or another family."
replace motivation=1 if f6=="to keep taps in good condition, to arrange meetings, wotk with community members"
replace motivation=0 if f6=="just accepted the responsibility, because the villagers accepted me as their headman."
replace motivation=0 if f6=="just accepted, because the community voted him"
replace motivation=1 if f6=="to lead the nation. To care for community members. Help provide for community members."
replace motivation=0 if f6=="Accepted villagers decision"
replace motivation=0 if f6=="I just accepted as a calling from God."
replace motivation=1 if f6=="to help develop the village"
replace motivation=0 if f6=="just accepted to lead the villagers, because they appointed/elected me."
replace motivation=1 if f6=="to assist the community, accepted a responsibility given to him by the villagers, help develop the community"
replace motivation=0 if f6=="i just accepted"
replace motivation=1 if f6=="to help develop the village, improve water supply"
replace motivation=0 if f6=="There was no motive, just accepted."
replace motivation=0 if f6=="just accepted the vote"
replace motivation=1 if f6=="to lead the community. To help develop the community. To help improve people's/community member's lives."
replace motivation=1 if f6=="help out the villagers without expectations"
replace motivation=0 if f6=="" /*leader gave no answer - we set his intrinsic-motivation to 0*/
replace motivation=0 if f6=="just accepted votes & didnt volunteer to be a candidate"
replace motivation=1 if f6=="A God given gift/role. Did not want to disappoint community members. Lead the community."
replace motivation=0 if f6=="just accepted, because the people voted me"
replace motivation=1 if f6=="God given responsibility as well as responsibility from family and community members. Lead and care for the community."
replace motivation=1 if f6=="to care for taps and the community"
replace motivation=0 if f6=="I just accepted the responsibility"
replace motivation=1 if f6=="wanted to bring taps to the village, especially for drinking water purposes, used to drink from wells"
replace motivation=1 if f6=="assist every community member"
replace motivation=. if id>64
lab def motivation 0 "extrinsic" 1 "intrinsic", replace
lab val motivation motivation
gen extrinsic_leader = 1 if motivation==0
replace extrinsic_leader = 0 if motivation==1
lab def extrensic 0 "Intrinsic" 1 "Extrinsic", replace
lab val extrinsic_leader extrensic
bysort village game1_group: egen group_extrinsic_leader = mean(extrinsic_leader)


// PERCEPTION OF LEADERS PERFORMANCE BY VILLAGERS
lab def agree1 1 "strongly disagree" 2 "disagree a little" 3 "neutral" 4 "agree a little" 5 "strongly agree"

* synchronize scales
foreach v of varlist b12satisfactionTL b10b {
	replace `v'=5 if `v'==.
	local i = `i' + 1
	gen e_`i'=1 if `v'==4
	replace e_`i'=2 if `v'==3
	replace e_`i'=3 if `v'==5
	replace e_`i'=4 if `v'==2
	replace e_`i'=5 if `v'==1
	replace e_`i'=. if villager==0
	lab val e_`i' agree1
	}

foreach v of varlist b11g b8b b11f b8c b11h b8d {
	replace `v'=5 if `v'==.
	local i = `i' + 1
	gen e_`i'=1 if `v'==1
	replace e_`i'=2 if `v'==2
	replace e_`i'=3 if `v'==5
	replace e_`i'=4 if `v'==3
	replace e_`i'=5 if `v'==4
	replace e_`i'=. if villager==0
	lab val e_`i' agree1
	}

*TL specific satisfaction items
foreach v of varlist b11b b11c b11d b11e b11f b11g b11h {
	replace `v'=5 if `v'==.
	local j = `j' + 1
	gen opinionTL_`j'=1 if `v'==1
	replace opinionTL_`j'=2 if `v'==2
	replace opinionTL_`j'=3 if `v'==5
	replace opinionTL_`j'=4 if `v'==3
	replace opinionTL_`j'=5 if `v'==4
	replace opinionTL_`j'=. if villager==0
	lab val opinionTL_`j' agree1
	}
*scale all items so that higher agreement indicates higher satisfaction
gen opinionTL_1reverse = 6-opinionTL_1
gen opinionTL_7reverse = 6-opinionTL_7

alpha e_1 opinionTL_1reverse opinionTL_2 opinionTL_3 opinionTL_4 opinionTL_5 opinionTL_6 opinionTL_7reverse 
* alpha=0.75 pretty reliable scale, can reduce dimensionality
*start with simple additative index, 
egen mean_satisfactionTL = rowmean (e_1 opinionTL_1reverse opinionTL_2 opinionTL_3 opinionTL_4 opinionTL_5 opinionTL_6 opinionTL_7)

*DEL specific satisfaction  items
foreach v of varlist b8a b8b b8c b8d {
	replace `v'=5 if `v'==.
	local k = `k' + 1
	gen opinionDEL_`k'=1 if `v'==1
	replace opinionDEL_`k'=2 if `v'==2
	replace opinionDEL_`k'=3 if `v'==5
	replace opinionDEL_`k'=4 if `v'==3
	replace opinionDEL_`k'=5 if `v'==4
	replace opinionDEL_`k'=. if villager==0
	lab val opinionDEL_`k' agree1
	}
*scale all items so that higher agreement indicates higher satisfaction
gen opinionTL_4reverse = 6-opinionTL_4

alpha e_2 opinionDEL_1 opinionDEL_2 opinionDEL_3 opinionTL_4reverse
* alpha=0.52 less reliable
*start with simple additative index, 
egen mean_satisfactionDEL = rowmean (e_2 opinionDEL_1 opinionDEL_2 opinionDEL_3 opinionTL_4reverse)


*generate satisfaction dependent on which leaders villagers were matched with in game 1
gen satisfaction_TL = mean_satisfactionTL
replace satisfaction_TL = . if game1_group!=13
gen satisfaction_DEL = mean_satisfactionDEL
replace satisfaction_DEL = . if game1_group!=14
egen satisfaction_leader = rowmean (satisfaction_TL satisfaction_DEL)
lab val satisfaction_leader agree1
lab var satisfaction_leader "Leader satisfaction index (decision-making, exploitation, etc.)"
egen z_satisfaction = std(satisfaction_leader)

*Satisfaction with performance of DEL / TL
gen performance_TL = e_1
replace performance_TL = . if game1_group!=13
gen performance_DEL = e_2
replace performance_DEL = . if game1_group!=14
egen performance_leader = rowmean (performance_TL performance_DEL)
lab val performance_leader agree1
lab var performance_leader "How satisfied are you with the peformance of the TL/DEL?"

*label variables
label var e_1 "Satisfied with the chief in general?"
label var e_2 "Satisfied with the DEL in general?"
label var e_3 "The Chief makes decisions in an understandable manner?"
label var e_4 "The DEL makes decisions in an understandable manner?"
label var e_5 "The Chief takes appropriate actions against people who disobey the rules?"
label var e_6 "The DEL takes appropriate actions against people who disobey the rules?"
label var e_7 "Does the chief exploit his or her position?"
label var e_8 "Does the DEL exploit his or her position?"

*Combined Index of leader quality
gen reverse_e7 = 6-e_7
alpha e_1 e_3 e_5 reverse_e7


***************************
**** BIG FIVE
/*
Rammstedt, B. & John, O. P. (2007). Measuring personality in one minute or less: A 10-item short version of the Big Five Inventory in English and German. 
Journal of Research in Personality, 41, 203-212. 

De Young et al. 2010 Psych. Science: Neuroticism covaried with volume of brain regions associated with threat, punishment, and negative affect. 
http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3049165/


Positively formualed items: c29, c25, c28, c32, c34
Negatively formulated items: c26, c27, c30, c31, c33
*/

*** 1. Step, gen new variables
foreach var of varlist c25-c34{
replace `var'=. if `var'==5
gen `var'new = `var'
}

*** 2. Step, recode negatively formulated items ***
foreach var of varlist c26new c27new c30new c31new c33new{
recode `var' (1=4) (2=3) (3=2) (4=1)
}
quietly tab c26 c26new
quietly tab c27 c27new
sum c25-c34	/* we have missings for c25, otherwise complete info*/

*** 3. Step: Build variables ***
/* Higher values mean more agreeable, extraverted etc. */  
gen extraversion=(c29new+c26new)/2 if leader!=. 
gen agreeableness=(c25new+c31new)/2 if leader!=. & c25!=.
replace agreeableness=c31new if leader!=. & c25==.
gen conscientiousness=(c32new+c27new)/2 if leader!=.
gen neuroticism=(c33new+c28new)/2 if leader!=.
gen openness=(c34new+c30new)/2 if leader!=.

*** 4. Step: test for scale reliability (Cronbach's alpha)
alpha c29new c26new		/*extraversion: 0.86*/
alpha c25new c31new, item	/*agreeableness: 0.81*/
alpha c32new c27new	/*Conscientiousness: 0.92 */
alpha c33new c28new	/*neuroticism: 0.76 */
alpha c34new c30new	/*openness: 0.49*/

*** 5. Step: Standardize BIG5 measures
foreach var of varlist extraversion-openness{
sum `var'
gen `var'_normal = (`var'-r(min))/(r(max)-r(min))
}
sum *_normal 

foreach var of varlist *_normal{
ttest `var', by(leader)
}
/* No sig differences between leader type*/

label variable extraversion "B5 extraversion, based on c29 and recoded c26new"
label variable agreeableness "B5 agreeableness, based on c25 and recoded c31new"
label variable neuroticism "B5 neuroticism, based on c28 and recoded c33new"
label variable conscientiousness "B5 conscientiousness, based on c32 and recoded c27new"
label variable openness "B5 openess, based on c34 and recoded c30new"

drop c25new-c34new


// Pairwise correlations
global big5 *_normal
foreach var of varlist $big5{
pwcorr extra*_normal `var', sig
}
/* THAT is Puzzling: Big 5s are strongly correlated among each other. Including them jointly in regressions will be problematic due to multicollinearity */
******************************************




*********C13-C24***************************
/*we used authentic leadership self-assessment questionnaire (pdf).
Literature uses 5-point scale, and than simply takes the sum of all 4 items. Values above 15 mean high AL, values of 15 and below mean low AL.
In our case this would mean value of >12 is high AL, value of 12 and below is low AL.
However, we have one missing for c13. This person would hence automatically get a low AL score. 
We could circumvent this problem by taking the average score. (NOT NOW)
*/
alpha c13 c16 c19 c22		/*moral perspective: 0.72*/
alpha c14 c17 c20 c23		/*balanced processing: 0.94*/
alpha c15 c18 c21 c24		/*relation transparency: 0.89*/
global AL c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24
pca $AL
estat loadings
predict AL1, score 
sum AL1

gen moral_perspective=c13+c16+c19+c22
gen balance_processing=c14+c17+c20+c23
gen relational_transparency=c15+c18+c21+c24
label variable moral_perspective "AL moral perspective, total score on c13 c16 c19 and c22"
label variable balance_processing "AL balance processing, total score on c14 c17 c20 and c23"
label variable relational_transparency "AL relational transparency, total score on c15 c18 c21 and c24"

pwcorr moral balance
pwcorr moral relational
pwcorr balance relationa
egen helpauthentic = rowmean(moral_pers balance_pro relational_trans)
gen authentic_leader1=0 if helpauthe!=.
replace authentic_leader1=1 if helpauthe>12 & helpauthe!=.
label variable authentic_leader1 "1=high authentic leadership, 0 otherwise"
lab def authentic 0 "Low" 1 "High", replace
lab val authentic_leader1 authentic

/* AGAIN, very strong correlation among AL variables. Including them jointly in regressions may be no good idea*/
gen strong_moral=0 if leader!=.
replace strong_moral=1 if moral_perspective>=13 & leader!=.
gen strong_balance=0 if leader!=.
replace strong_balance=1 if balance_processing>=13 & leader!=.
gen strong_relational=0 if leader!=.
replace strong_relational=1 if relational_transparency>=13 & leader!=.

foreach var of varlist  strong_moral strong_balance strong_relational{
tab2 `var' leader, chi2
}
/* No differences, will not change if we use Fisher exact test for values being very similar*/
label variable strong_moral"AL dummy for moral perspective"
label variable strong_balance "AL dummy for balance processing"
label variable strong_relational "AL dummy for relational transparency"

gen authentic_leader2=0 if leader!=.
replace authentic_leader2=1 if strong_moral==1 & strong_balance==1 & strong_relational==1
label variable authentic_leader2 "Measure of authentic leadership based on dummies"

* standardize authentic leadership scale (z-scores)
egen AL_score=rowmean(moral_perspective balance_processing relational_transparency)
egen z_AL_score=std(AL_score)

* Village level leader motivation
bysort village: egen aux_motivation=total(motivation)
gen intrinsic_DEL=0 if motivation==0 & TL==0
replace intrinsic_DEL=1 if motivation==1 & TL==0
gen intrinsic_TL=0 if motivation==0 & TL==1
replace intrinsic_TL=1 if motivation==1 & TL==1
gen sum_motivation=.
replace sum_motivation=0 if aux_motivation==0
replace sum_motivation=1 if aux_motivation==1 & intrinsic_DEL==0
replace sum_motivation=1 if aux_motivation==1 & intrinsic_TL==1
replace sum_motivation=2 if aux_motivation==1 & intrinsic_TL==0
replace sum_motivation=2 if aux_motivation==1 & intrinsic_DEL==1
replace sum_motivation=3 if aux_motivation==2
lab def motivation2 0 "both extrinsic" 1 "DEL extrinsic/ TL intrinsic" 2 "TL extrinsic/ DEL intrinsic"  3 "both intrinsic", replace
lab val sum_motivation motivation2

gen bonus_AL=bonus*authentic_leader1



/* NOTE:
- FWe drop d5j (Motorrad), cell phone and freezer as this has too little variation. Drop sewing machine as no obs for leader
- Include dummies for d6 and d7a, in case of d6 a dummy if main source is private tap (d6==1) und in case of d7 if d7a=02 (concrete)
*/

* Dummies for main income sources
gen i_sf=0
replace i_sf=1 if d1==3 | d1==4
lab var i_sf "Farming"

gen i_ob=0 
replace i_ob=1 if d1==5
label var i_ob "Own business"

gen i_em=0
replace i_em=1 if d1==1 | d1==2
label var i_em "Employment"

gen i_p=0
replace i_p=1 if d1==6
label var i_p "Pension"



gen privatetap = (d6==1) if d6!=.
gen robustfloor = (d7a==2 | d7a==4) if d7a!=.
label variable robustfloor "Dummy taking 1 if concrete or wooden housefloor, based on d7a"
label variable privatetap "Dummy if privat tap, based on d6"

// Normalize livestock assets to [0,1] range
sum d3a 
gen z_cattle = (d3a - r(min))/(r(max)-r(min))
sum d3b
gen z_sheeps = (d3b - r(min))/(r(max)-r(min))
sum d3c
gen z_goats = (d3c - r(min))/(r(max)-r(min))

bysort villager: sum z_cattle z_sheeps z_goats

global asset z_cattle z_sheeps z_goats d5a d5b d5c d5d d5e d5f d5g d5i d5j d5k d5l d5m d5n robustfloor privatetap
global asset2 d5a d5b d5d d5f d5g d5i d5k-d5n privatetap

pca $asset2, comp(1)
estat loadings
predict pc1, score
estat kmo // legitimate pca, kmo=0.79 > 0.7
sum pc1
gen pca_wealth=pc1
label var pca_wealth "Socio-economic status proxy based on first component of pca"

xtile quintile=pca_wealth, nq(5)
bysort leader: sum pca_wealth
label define quints 1 "Poorest" 2 "Poor" 3 "Medium" 4 "Wealthy" 5 "Wealthiest"
label values quintile quints

bysort quintile: sum $asset




*** Create Variable that informs about years living in village ***
/* Problem: for 50% of sample, a6 takes the value of "1". What does this mean? */

** lifetime spent in village
replace a6=2000 if id==380
*Assistant for got a zero in the year
gen years_in_village = .
gen a6_new = 2015-a6 if a6 > 1
label variable a6_new "Transformation of a6 in years"
replace years_in_village = age if a6==1
replace  years_in_village = a6_new if a6 > 1
gen rootedness=years_in_village/age
replace rootedness=1 if rootedness > 1
label variable rootedness "Fraction of lifetime spent in village"
*some are younger than the time spent in the village (they probably have been living there all their life)


*** experience in years
gen experience = 2015-f1
label variable experience "Years in office"


// Competition
gen f5new=f5
replace f5new=0 if f5new==. & leader!=.
rename f5new competitor

gen dcompetitor = (competitor>0 & competitor!=.)
label variable competitor "Number of competitors in previous election"
label variable dcompetitor "Dummy=1 if there were some other candidates in previous election"

/*
gen competitor2=competitor
replace competitor2=20 if competitor2>=20 & competitor2!=.
*/

// Term office
gen term=f2a
label variable term "Dummy=1 if person faces term office"

// Elected
gen elected =1 if f3b==1 | (f3==1 | f3==2)
replace elected=0 if elected ==. & leader!=.
label variable elected "Dummy=1 if person was elected rather than appointed"

// Secret elections
gen secretelec = 1 if f4c==1
replace secretelec = 0 if f4c>1 & f4c!=.
label var secretelec "Dummy =1 if elections were secretly held"


save "$working_ANALYSIS\processed\analysis_wide.dta", replace


/*
Reshape dataset into long-format for analysis of bonus payment rounds.
Generate additional variables for analysis in long-format.
*/
reshape long rule vote majority payoff, i(id) j(decision)

*Drop all decisions except 1, 5 and 6 (basic, bonus1, bonus2)
drop if decision>1 & decision<5
drop if id>64
order decision rule vote majority payoff, after(session_id)
order session_id, after(id)

*---------------------------------------------------------
* Generating additional variables for long-format analysis
*---------------------------------------------------------
* Binary rule choice
gen dem=0
replace dem=1 if rule==1

gen dic=0
replace dic=1 if rule==3

gen fake=0
replace fake=1 if rule==2
*bonus: 0=no bonus, 1=any bonus
gen bonus=0
replace bonus=1 if decision>4


*bonus_size: 0=no, 1=small, 2=large
gen bonus_size=0
replace bonus_size=1 if decision==5
replace bonus_size=2 if decision==6
lab def bonus_size1 0 "Baseline" 1 "Small Bonus" 2 "Large Bonus", replace
lab val bonus_size bonus_size1


*Generate identifier for the first bonus that was offered
gen bonus_first=0 if (decision==6 & randomization<7) | (decision==5 & randomization>6)
replace bonus_first=1 if (decision==5 & randomization<7) | (decision==6 & randomization>6)
label def first_lab 0 "3rd round" 1 "2nd round", replace
lab val bonus_first first_lab

replace rule=1 if dem==1
replace rule=2 if fake==1
replace rule=3 if dic==1

label def bonus 0 "Baseline (n=64)" 1 "Pooled Bonus (n=128)", replace
label def rule 1 "Democratic" 2 "Pseudo-democratic" 3 "Autocratic" , replace
label val bonus bonus
label val rule rule

order bonus bonus_size bonus_order randomization dem dic fake, after(majority)

*rule choice variable for basic round
gen rule_basic = rule if decision==1
replace rule_basic = rule_basic[_n-1] if missing(rule_basic)
order rule_basic, after(rule)
label def rule_basic 1 "Base: Democratic (n=41)" 2 "Base: Pseudo (n=14)" 3 "Base: Autocrat (n=9)", replace
label val rule_basic rule_basic


qui tab bonus_size, gen(bonus_)
qui tab bonus_order, gen(order_)
qui tab rule_basic, gen(baseline_)

*interactions 
gen bonus_democrat=bonus*baseline_1
gen bonus_pseudo=bonus*baseline_2
gen bonus_dictator=bonus*baseline_3

*round identifier
gen round= 1 if bonus_first==.
replace round = 2 if bonus_first==1
replace round = 3 if bonus_first==0

lab def order1 0 "Base-Large-Small" 1 "Base-Small-Large", replace
lab val bonus_order order1




*Types based on boxes as explained in section 2
*based on leaders' motivations from pre-workshop survey 
gen type=0
replace type=1 if rule_base!=1 & extrinsic==0
replace type=2 if rule_base!=1 & extrinsic==1
replace type=3 if rule_base==1 & extrinsic==0
replace type=4 if rule_base==1 & extrinsic==1
lab def typo 1 "Box I" 2 "Box III" 3 "Box II" 4 "Box IV", replace
lab val type typo

*based on authentic leadership measure
gen type_AL=1 if rule_base!=1 & authentic_leader1==1
replace type_AL=2 if rule_base!=1 & authentic_leader1==0
replace type_AL=3 if rule_base==1 & authentic_leader1==1
replace type_AL=4 if rule_base==1 & authentic_leader1==0
lab val type_AL typo

*democratic rule choice in percent
gen dem100=dem*100

// Globals
global order order_3
global socio education age pca_wealth male experience married rootedness

*interactions
gen bonus_extrinsic=bonus*extrinsic_leader
gen bonus_TL=bonus*TL


save "$working_ANALYSIS\processed\analysis_long.dta", replace




** EOF