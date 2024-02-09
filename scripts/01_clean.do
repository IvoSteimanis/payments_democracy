*--------------------------------------------------------------------------------------
* SCRIPT: 01_clean.do
* PURPOSE: cleans the raw experimental and survey data for leaders and villages and merges both datasets
*--------------------------------------------------------------------------------------




*---------------------------------------
* 1) Clean leader data
*---------------------------------------
clear
use "$working_ANALYSIS\data\leader_raw.dta"

*** Generate Leader dummies ***
gen leader=ID
/* ID-old, 13=TL, 14=DL*/
replace leader=1 if leader==13
replace leader=0 if leader==14
label define leader1 1 "Chiefs" 0 "DEL"
label values leader leader1 
gen DEL= (leader==0)
gen TL= (leader==1)
drop Position // redundant with "leader"
label variable DEL "democratic leader"
label variable TL "traditional authority"



*** Generate Village-Variable in byte-Format ***
/* Necessary if we want to control for village-fixed effects.
Note: need to make sure that we use the same codes in villager dataset*/
gen village = Village
replace village = "1" if village=="Edundja"
replace village = "2" if village=="Ehafo"
replace village = "3" if village=="Endola"
replace village = "4" if village=="Epatululo"
replace village = "5" if village=="Epuku"
replace village = "6" if village=="Etale"
replace village = "7" if village=="Etilashi"
replace village = "8" if village=="Etomba"
replace village = "9" if village=="Ohangwena"
replace village = "10" if village=="Oipapakane"
replace village = "11" if village=="Okadiwa"
replace village = "12" if village=="Okafitu"
replace village = "13" if village=="Okakwa"
replace village = "14" if village=="Okangudi"
replace village = "15" if village=="Okapundja"
replace village = "16" if village=="Okatale"
replace village = "17" if village=="Okelemba"
replace village = "18" if village=="Omahenge"
replace village = "19" if village=="Omakango"
replace village = "20" if village=="Omalyata"
replace village = "21" if village=="Omaputo"
replace village = "22" if village=="Omukuve"
replace village = "23" if village=="Omutwe Oshimbungu"
replace village = "24" if village=="Onangwhe"
replace village = "25" if village=="Onekwaya East"
replace village = "26" if village=="Onekwaya West"
replace village = "27" if village=="Onengali"
replace village = "28" if village=="Ongonga"
replace village = "29" if village=="Onheleiwa"
replace village = "30" if village=="Oshikwiyu 1"
replace village = "31" if village=="Oshikwiyu 2"
replace village = "32" if village=="Ouhongo"
destring village, replace

#delimit;
label define village 
1 "Edundja"
2 "Ehafo"
3 "Endola"
4 "Epatululo"
5 "Epuku"
6 "Etale"
7 "Etilashi"
8 "Etomba"
9 "Ohangwena"
10 "Oipapakane"
11 "Okadiwa"
12 "Okafitu"
13 "Okakwa"
14 "Okangudi"
15 "Okapundja"
16 "Okatale"
17 "Okelemba"
18 "Omahenge"
19 "Omakango"
20 "Omalyata"
21 "Omaputo"
22 "Omukuve"
23 "Omutwe Oshimbungu"
24 "Onangwhe"
25 "Onekwaya East"
26 "Onekwaya West"
27 "Onengali"
28 "Ongonga"
29 "Onheleiwa"
30 "Oshikwiyu 1"
31 "Oshikwiyu 2"
32 "Ouhongo";
label values village village;
#delimit cr
quietly tab village Village //ok, everything correct
drop Village // not necessary anymore
label variable village "village names"
tab village, gen(village_)


*** Generate unique Leader-ID ***
sort village leader
rename ID ID_old
gen id = _n
label variable id "unique identifier"
duplicates report id //ok
drop ID_old


*** Day and Month ***
/*Info on Day, Month and Interviewer only available at village level, not at individual level*/
sort village TL
xtset id village	//tell stata that village is time variable (necessary to make following commands)
gen BXnew = BX
replace BXnew=BXnew[_n+1] if TL==0 & BXnew==""
drop BX
rename BXnew BX
replace BV=BV[_n+1] if TL==0 & BV==.
replace BW=BW[_n+1] if TL==0 & BW==""
xtset, clear
tab BX village /*ok, consitent*/
label variable BX "village"

rename BV day_exp
label variable day_exp "Day of experiment conduct"
rename BW month_exp
label variable month_exp "Month of experiment conduct"
rename Day day_pre
label var day_pre "Day of pre-questionnaire conduct
rename Month month_pre
label var month_pre "Month of pre-questionnaire conduct"
replace month_exp="" if id==9 | id==10
destring month_exp, replace

gen testCB = day_exp-CB
tab testCB, miss
sort village id
/* edit village id day_exp day_pre CB testCB if testCB!=0 /* we assume that CB was a typo */ */
drop CB testCB
destring CC, replace
gen testCC = month_exp-CC
replace month_exp=8 if village==5
drop CC
quietly tab village CD /*ok*/
drop CD



*** Interviewer ***
sort village
/* edit village Assistant Interviewer BY CE */

replace Interviewer="1" if regexm(Interviewer, "Pa")
replace Interviewer="2" if regexm(Interviewer, "Iya")
replace Interviewer="3" if regexm(Interviewer, "Sa")
rename Inter interviewer_pre
label variable interviewer_pre "interviewer for leader, pre-game"
destring interviewer_pre, replace
label define interviewer 1"Parista" 2"Iyalo" 3"Sarah"
label values interviewer_pre interviewer

rename Assistant interviewer_exp
label var interviewer_exp "interviewer for leader in experiment and post questionnaire"
label values interviewer_exp interviewer

label var BY "Interviewer in experiment for TL"
label var CE "Interviewer in experiment for DEL"


************************************
*** RANDOMISATION VARIABLE GAME 3***
************************************
gen pos_pairRN = 0
replace pos_pairRN = 1 if randomization==1 | randomization==2 | randomization==7 | randomization==8
replace pos_pairRN = 2 if randomization==3 | randomization==4 | randomization==9 | randomization==10
replace pos_pairRN = 3 if randomization==5 | randomization==6 | randomization==11 | randomization==12

gen pos_pairNR = 0
replace pos_pairNR = 1 if randomization==3 | randomization==5 | randomization==9 | randomization==11
replace pos_pairNR = 2 if randomization==1 | randomization==6 | randomization==7 | randomization==12
replace pos_pairNR = 3 if randomization==2 | randomization==4 | randomization==8 | randomization==10

gen pos_pairNN = 0
replace pos_pairNN = 1 if randomization==4 | randomization==6 | randomization==10 | randomization==12
replace pos_pairNN = 2 if randomization==2 | randomization==5 | randomization==8 | randomization==11
replace pos_pairNN = 3 if randomization==1 | randomization==3 | randomization==7 | randomization==9


**** MODULE A ****

*** Check Name-Gender consistency, generate gender dummy ***
/* edit Name gender */
rename A1gender male
recode male (2=0)
label variable male "dummy =1 if male, 0 if women"


*** Rename important Socio-demos ***
rename A2marital maritalstatus
label variable maritalstatus maritalstatus
label define maritalstatus 1"married" 2"cohabiting" 3"Single" 4"Divorced" 5"Widowed"
label values maritalstatus maritalstatus

gen married = (maritalstatus==1 | maritalstatus==2) if leader!=.
label variable married "Dummy = 1 if married"

rename A3age age
label variable age "age in years"

rename A4a education
label var education "Highest grade attained at school"

recode A4bcol 2=0
rename A4bcol college

rename A5ho hhsize

rename A6 a6
label variable a6 "Since when living in village"
/* Later we may create a variable based on A6 and age, taking 1 if person has lived in village since birth */

rename A7com religion
label variable religion "religious affiliation"
replace religion="1" if regexm(religion, "lci")
replace religion="2" if regexm(religion, "angelica")
replace religion="3" if regexm(religion, "apos")
replace religion="3" if regexm(religion, "church")
destring religion, replace
label define religion 1"Elcin" 2"Angelican" 3"Apostolic" 4"Church of Christus"
label values religion religion

/* No variation in A8a and A8b, can be dropped for leaders */
drop A8* A7re


***************************
*** MODULE F - PRE-GAME ***
***************************
label define yes 1"yes" 0"no"

rename F1 f1old
gen f1 = f1
label variable f1 "In office since, corrected for DEL"
/* Keep original f1 variable, but modify "inoffice". Assume that DEL cannot be longer in office than 14 years */
replace f1=2000 if f1<2000 & DEL==1

rename F2ate f2a
label variable f2a "F2a. Is there a term office?"
replace f2a=1 if  f2a==.
/* Strange, only few people answer affirmatively - MISTAKE IN OSHIVAMBO TRANSL? Vielleicht haben alle die Headmen Frage bekomment*/

rename F2c f2c
/* All missing, I assume that everyone answered question with no*/
replace f2c=0 if f2c==.
drop F2d
label var f2c "F2c. Being in position before?"
replace f2c=F2cWP if leader==0
drop F2cWP

rename F2b f2b
quietly tab f2b if f2a==1	//ok
label variable f2b "F2b. Length of term office?"

rename F2e f2eold
label var f2eold "F2e. Will u run for another term, not corrected?"
gen f2e = f2eold
label var f2e "F2e. Will u run for another term?"
bysort f2a: tab f2e  /* People saying there is no office term restriction nevertheless answer they would run for another term*/


rename F3 f3
label variable f3 "F3. How did DEL come to power"
# delimit;
label define f3
1 "Directly elected as chairperson by user"
2 "Elected as member of the WPC and then elected as CP by WPC"
3 "Elected by someone else, specify"
4 "Appointed, specify"
5 "Inherited position"
6 "Other, specify";
# delimit cr 
label values f3 f3

rename F3co f3comment
tab f3 f3comment if f3comment!="."

rename F3b f3b
label variable f3b "F3b. How did TL come into office?"
# delimit;
label define f3b
1 "Elected by villagers"
2 "Eleceted by selected villagers"
3 "Elected someone else"
4 "Appointed"
5 "Inherited position"
6 "Other";
label values f3b f3b;
# delimit cr
list id f3b F3bcomm if leader==1 & F3bcomm!=""
tab F3bcom if f3b==3
replace f3b=4 if f3b==3
rename F3bcom f3bcomment

replace f3=1 if (f3comment=="Villagers" | f3comment=="villagers" | f3comment=="villagers voted")


global e ""a""b""c""
foreach e of global e{
rename F4`e' f4`e'
}
rename F4cc f4ccomment
label var f4a "F4a. Who can become TL"
label var f4b "F4b. Who can appoint TL"
label var f4c "F4c. Voting/appointment procedure"
# delimit:
label define f4c
1"Voting/appointment was anonymous "
2"Voting/appointment was publicy held"
3"Other, specify";
label values f4c f4c;
#delimit cr 

rename F5 f5

label var f5 "F5. No of people running position as leader"

rename F6 f6
label var f6 "F6. Motivation to become leader"
/* Need to identify meaningful categories for this variable */
 
rename F7_func f7
label var f7 "F7. Functions of Leader (self-reported)"

replace f5=F5a if leader==0
/*replace f5=20 if (f5==30 | f5==48)*/
drop F5a
rename F5b f5b
label var f5b "Number of received votes"

/* Recode all dummies and assign label*/
foreach var of varlist f2a f2c f2e {
recode `var' (2=0)
label values `var' yes
}


***************************
*** MODULE F - Post-GAME ***
***************************

tab F10a
rename F10a f10a
label variable f10a "F10a, Do you receive a fix payment"
recode f10a (2=0)
label values f10a yes

rename F10b f10b 
label variable f10b "F10b, What kind of payments/benefits do you receive"
gen f10b_new = f10b
replace f10b_new="1" if regexm(f10b_new, "othing")
replace f10b_new="2" if regexm(f10b_new, "start up")
replace f10b_new="2" if regexm(f10b_new, "usines")
replace f10b_new="2" if regexm(f10b_new, "shebe")
replace f10b_new="2" if regexm(f10b_new, "bar")
replace f10b_new="3" if regexm(f10b_new, "transport")
replace f10b_new="4" if regexm(f10b_new, "owner")
replace f10b_new="4" if regexm(f10b_new, "death")
replace f10b_new="4" if regexm(f10b_new, "deceased")
replace f10b_new="4" if regexm(f10b_new, "taking over")
replace f10b_new="5" if regexm(f10b_new, "land fees")
replace f10b_new="5" if regexm(f10b_new, "land")
replace f10b_new="6" if (f10b_new!="1" & f10b_new!="2" & f10b_new!="3" & f10b_new!="4" & f10b_new!="5")
destring f10b_new, replace
# delimit;
label define f10b_new
1"Nothing"
2"For starting business"
3"Transport allowance"
4"Change in house ownership"
5"Land fees"
6"Other";
#delimit cr
label values f10b_new f10b_new

rename F11 f11
label variable f11 "F11, How much time do you spend leader activities"
rename F12a f12a
recode f12a (2=0)
label values f12a yes
label variable f12a "F12a, Right to grant access to land"

rename F12b f12b
label variable f12b "F12b, Decision process of land allocation"
# delimit;
label define f12b
1"take decision alone"
2"take decision alone but seek other’s opinion"
3"take decision together with other people"
4"Other specify";
#delimit cr
label values f12b f12b

forvalues i=1/4{
rename F12c`i' f12c`i'
}
label variable f12c1 "Decision criterion applicant's Neediness"
label variable f12c2 "Decision criterion applicant's Social status"
label variable f12c3 "Decision criterion applicant's trustworthiness"
label variable f12c4 "Decision criterion relationship to applicant"
# delimit;
label define f12c
1"Highly relevant"
2"Somewhat relevant"
3"Rather irrelevant"
4"Not at all relevant";
# delimit cr
foreach var of varlist f12c*{
label values `var' f12c
}

rename F12d f12d
label variable f12d "Further criteria for land assignment"

rename F12e f12e
label var f12e "Do beneficiaries of land assignment have to pay"
recode f12e (2=0)
label values f12e yes

global e ""a""b""c""d""
foreach e of global e{
rename F13`e' f13`e'
}
label var f13a "Statement:leadership position helps me to improve my financial situation"
label var f13b "Statement:leadership position increases my popularity among villagers"
label var f13c "Statement:leadership position increases my status in the community"
label var f13d "Statement:leadership fosters my connections to people outside the villages"
foreach var of varlist f13*{
recode `var' (2=0)
label values `var' yes
}

global e ""a""b""c""d""
foreach e of global e{
rename F18`e' f18`e'
}
label var f18a "Source of water supply for village"
label define f18a 1"Pipeline" 2"Boreholes" 3"Other"

label var f18b "Frequency of wrong reports/refusals of water fee"
# delimit;
label define f18b
1"Almost all"
2"More than half"
3"Less than half, but still a sizeable fraction"
4"Only very few"
5"None";
# delimit cr
label values f18b f18b

label var f18c "Main reason for wrong reports/refusals"
# delimit;
label define f18c
1"Poverty"
2"Access to water is costless basic right"
3"Other";
#delimit cr
label values f18c f18c
replace f18c =. if F18ccomment=="N/A"

label var f18d "What do you do with people who refuse to pay?"
# delimit;
label define f18d
1"Report to police"
2"Report to Rural Water Supply"
3"Report to traditional authority"
4"Try to convince them to pay"
5"Nothing"
6"Refusing access"
7"Other"; 
label values f18d f18d;
# delimit cr
replace f18d =. if F18dcomment=="N/A"
replace f18d =2 if regexm(F18dcomment,"rural water")
replace f18d =6 if regexm(F18dcomment,"close")
replace f18d =6 if regexm(F18dcomment,"cut off")

rename Qa qa
label var qa "Who has final say wrt important water-related issue"
rename Qb qb
labe var qb "If decision is made by committee, according to what procedure"
# delimit;
label define qa
1"Chairperson"
2"WPC" 
3"WPA"
4"RWS"
5"Other";
label values qa qa;
label define qb
1"Majority vote"
2"Consensus"
3"Other";
label values qb qb;
# delimit cr 

replace qa=3 if Qaco=="community members" | Qaco=="village member"
replace qa=3 if Qaco=="all water tap users"
/* Inconsistency in qb: answered by 31 people though only 19 took WPC in qa */


/* Module F is cleaned */



******************
**** Module G ****
******************
/* G1b is missing !!!*/

global el ""a""c""d""e""f""g""h""
foreach e of global el{
rename G1`e' g1`e'
rename G1`e'_posit g1`e'_position
}
foreach var of varlist g1a g1c g1d g1e g1f g1g g1h{
recode `var' (2=0)
}
label var g1a "G1a. Council Member of traditional authority"  
label var g1c "G1c. Management committee of a communal conservancy"
label var g1d "G1d. Board of a political party"
label var g1e "G1e. Board of a religious organization"
label var g1f "G1f. Board of a sport or recreational organization"
label var g1g "G1g. Board of professional organization"
label var g1h "G1h. Board of other"

forva i=1/5{
rename G2_name`i' g2_name`i'
}
forv i=1/3{
rename G3_name`i' g3_name`i'
}
forv i=1/6{
rename G4b_name`i' g4b_name`i'
rename G4b_relationship`i' g4b_relation`i'
}
rename G4a g4a
recode g4a (2=0)
label variable g4a "G4a. Relatives in village?"

label define rela 1"close friend" 2"relative" 3"no friend or relative" 
global el ""a""c""d""e""
foreach e of global el{
rename G5`e' g5`e'
label values g5`e' rela
}
label variable g5c "G5c. Relationship to Healer"
label variable g5a "G5a. Relationship to TL/DEL"
label variable g5d "G5d. Relationship to Councilor"
label variable g5e "G5e. Relationship to priest"

rename G6_most g6
rename G6co g6comment

# delimit;
label define g6
1 "I myself"
2 "The traditional healer"
3 "The local councilor"
4 "The local priest/pastor" 
5 "TL / DEL"
6 "Other";
# delimit cr
label values g6 g6



****************
*** MODULE C ***
****************
forvalues i=1/12{
rename C`i' c`i'
}

/* NOTE: For the labels, we use the following prefix:
"BF" for Big Five item, "AL" for Authentic Leaderhsip item (Why did we not include self-awareness???? Fuck).

We further specify the respective components: 
Components of AL:
AL-IMP = Internalized Moral Perspective: C13, C16, C19, C22 
AL-BP = Balanced Processing: C14, C17, C20, C23
AL-RT = Relational Transparency: C15, C18, C21, C24

Components of 10-Item-BF:
BF-EV = Extraversion: C26, C29
BF-AN = Agreeableness: C25, C31
BF-CN = Conscientiousness: C27, C32
BF-NC = Neuroticism: C28, C33
BF-ON = Openness: C30, C34
*/

label var c1 "C1. For important decisions members of a group should be allowed to vote"
label var c2 "C2. Most people can learn to be leaders- it’s not a matter of birth"  
label var c3 "C3. Democratic elections in this village ensure that the elected authorities act in the interests of their people"
label var c4 "C4. In most cases, failures in this village are traceable to  bad leadership"
label var c5 "C5. Schools and parents should teach children to obey authority"
label var c6 "C6. Favouring friends, relatives or supporters over others in the course of ones duties as a leader is sometimes justified"
label var c7 "C7. On the whole, men make better political leaders than women do"
label var c8 "C8. A leader has to serve all his/her people including those who did not vote for him/her or are not friends with him/her" 
label var c9 "C9. Accepting a bribe in the course of ones duties is sometimes justified"
label var c10 "C10. Most people in this village can be trusted" 
label var c11 "C11. On the whole, more-educated people make better political leaders than the less-educated do"
label var c12 "C12. On the whole, elders make better political leaders than the youth do"
label var c13 "AL-IMP My actions reflect my core values"
label var c14 "AL-BP I seek others’ opinions before making up my own mind"
label var c15 "AL-RT I openly share my feelings with others"
label var c16 "AL-IMP I do allow group pressure to control me"
label var c17 "AL-BP I listen closely to the ideas of those who disagree with me"
label var c18 "AL-RT I let others know who I truly am as a person"
label var c19 "AL-IMP Other people know where I stand on controversial issues"
label var c20 "AL-BP I do not emphasize my own point of view at the expense of others"
label var c21 "AL-RT I rarely present a “false” front to others"
label var c22 "AL-IMP My morals guide what I do as a leader"
label var c23 "AL-BP I listen very carefully to the ideas of others before making decisions"
label var c24 "AL-RT I admit my mistakes to others"
label var c25 "BF-AN I see myself as someone who is generally trusting"
label var c26 "BF-EV I see myself as someone who is reserved"
label var c27 "BF-CN I see myself as someone who tends to be lazy"
label var c28 "BF-NC I see myself as someone who is relaxed, handles stress well"
label var c29 "BF-EV I see myself as someone who is outgoing, sociable"
label var c30 "BF-ON I see myself as someone who has few artistic interests"
label var c31 "BF-AN I see myself as someone who tends to find fault with others"
label var c32 "BF-CN I see myself as someone who does a thorough job"
label var c33 "BF-NC I see myself as someone who gets nervous easily"
label var c34 "BF-ON I see myself as someone who has an active imagination"

# delimit;
label define c
1"Strongly disagree"
2"Disagree a little"
3"Agree a little"
4"Strongly agree"
5"Don’t know";
# delimit cr
forvalues i=1/34{
label values c`i' c
}
/* NOTE: The answer category "dont know" has hardly been chosen. We can set it to missing, cannot work withit anyway*/
forvalues i=1/34{
replace c`i'=. if c`i'==5
}



label var c35a "I see myself as part of the Namibian nation"
label var c35b "I see myself as part of my local community"
label var c35c "I see myself as part of my ethnic group"   
label var c35d "I see myself as part of my kinship/clan"           
label var c35e "I see myself as a world citizen"
label var c35f "I see myself as an autonomous individual"
label define c35 1"Most applicable" 2"Second most applicable"
foreach var of varlist c35*{
label values `var' c35
}




****************
*** MODULE D ***
****************
*** Recode Asset variables, 0 instead of 2 for no ***
rename D1incomeso d1
label variable d1 "Income source"
# delimit;
label define is
1"Public sector employee"
2"Private sector employee"
3"Agriculture"
4"Livestock farming"
5"Own business (self-employment)"
6"Pension"
7"Transfers"
8"Other";
label values d1 is;
# delimit cr
/* one mistake in d1, income is 600, most probably pension. Change accordingly */
replace d1=6 if d1==9
replace d1=. if d1==0 | d1==100

rename D1b d1b
labe var d1b "Personal monthly income"

rename D2 d2
label var d2 "Household monthly income"
replace d2=600 if d1==6 & (d2==0 | d2==.)
/* We may not use d2, data are shit */

rename D3a d3a
rename D3b d3b
rename D3c d3c
label var d3a "Cattle"
label var d3b "Sheep"
label var d3c "Goats"

gen ssu = d3a*6+d3b+d3c
label var ssu "Livestock possession in small stock units, 1 cattle equivalent to 6 goat/sheep"


rename D4 d4
label variable d4 "Comparison to others"
label define d4 1"Better off" 2"Worse off" 3"Equal"
label values d4 d4

global a ""a""b""c""d""e""f""g""i""j""k""l""m""n""
foreach e of global a{
recode D5`e'* 2=0
}
foreach e of global a{
rename D5`e'* d5`e'
}
label var d5a "Radio"
label var d5b "Television"
label var d5c "Cell phone"
label var d5d "Refrigerator / Freezer"
label var d5e "Washing Machine"
label var d5f "Stove (electric, paraffin, kerosin"
label var d5g "Sewing machine"
label var d5i "Car"
label var d5j "Motorcycle / Scooter"
label var d5k "Bicycle"
label var d5l "Donkey-cart / Ox-cart"
label var d5m "Generator"
label var d5n "Plough"



rename D6wa d6
label var d6 "HH's main source of water"
# delimit;
label define d6
1"Private tap"
2"Public tap"
3"Water tank"
4"Well"
5"Other";
# delimit cr
label values d6 d6
rename D6c d6comment

rename D7afl d7a
replace d7a=5 if d7a==6
label var d7a "HH's material for floor"
# delimit;
label define d7a
1"Sand"
2"concrete"
3"mud, clay and/or cow dung"
4"wood"
5"other";
# delimit cr
label values d7a d7a
rename D7ac d7acomment



**************************************
*** Procedural-Fairness: Game 1 ***
**************************************

label var rule_1 "1=majority, 2=dictator, 3=Fake majority rule"
label var rule_2 "1=majority, 2=dictator, 3=Fake majority rule"
label var rule_3 "1=majority, 2=dictator, 3=Fake majority rule"
label var rule_4 "1=majority, 2=dictator, 3=Fake majority rule"
label var rule_bonus_1 "1=majority, 2=dictator, 3=Fake majority rule"
label var rule_bonus_2 "1=majority, 2=dictator, 3=Fake majority rule"

label var vote_1 "Benefit to leader at the expense of villagers" /* A=(100,10,10,10,10,10,10)	vs.	B=(40,20,20,20,20,20,20) */
label var vote_2 "Benefit to leader at no cost to villagers" /* A=(60,20,20,20,20,20,20) 	vs.	B=(40,20,20,20,20,20,20)*/
label var vote_3 "Unequal villagers" /* A=(40,5,23,23,23,23,23)	vs. 	B=(40,20,20,20,20,20,20)*/
label var vote_4 "Spiteful Leader" /* A=(40,15,15,15,15,15,15) 	vs.	B=(40,20,20,20,20,20,20)*/
label var vote_bonus_1 "Small bonus (+10) for choosing majority or fake majority" /* same as in decision 1: A=(100,10,10,10,10,10,10)	vs.	B=(40,20,20,20,20,20,20)*/
label var vote_bonus2 "Big bonus (+100) for choosing majority or fake majority" /* same as in decision 1: A=(100,10,10,10,10,10,10)	vs.	B=(40,20,20,20,20,20,20)*/

label var majority_1 "Majority-decision of group independent of implemented rule by leader"
label var majority_2 "Majority-decision of group independent of implemented rule by leader"
label var majority_3 "Majority-decision of group independent of implemented rule by leader"
label var majority_4 "Majority-decision of group independent of implemented rule by leader"
label var majority_bonus1 "Majority-decision of group independent of implemented rule by leader" /*only leaders voted / picked rule in bonus1, we took villagers decision from decision 1*/
label var majority_bonus2 "Majority-decision of group independent of implemented rule by leader" /*only leaders voted / picked rule in bonus2, we took villagers decision from decision 1*/

label define decision 1 "Chose A" 2 "Chose B"

foreach var of varlist vote_*{
replace `var'="1" if `var'=="a"
replace `var'="2" if `var'=="b"
destring `var', replace
label values `var' decision
}

**************************************
*** Social type, Preference-Game 2 ***
**************************************

label variable decision1 "Prosocial game trade-off 5,5 vs 5,0" /* A=[5,5] B=[5,0] -> first number player himself, second number is rdm player from participants*/
label variable decision2 "Envy game trade-off 5,5 vs 5,10" /* A=[5,5] B=[5,10] */
label variable decision3 "Sharing game trade-off 5,5 vs 10,0" /* A=[5,5] B=[10,0] */

foreach var of varlist decision*{
replace `var'="1" if `var'=="a"
replace `var'="2" if `var'=="b"
destring `var', replace
label values `var' decision
}

**************************************
*** Nepotism(TG-TTP)-Game 3 ***
**************************************

/*1) Bezeichnung der Paare im Nepotismus Game sollte unbenannt werden in pairRN, pairNR, pairNN
Game 3 (TG-TTP)
Pair 1	= RN, relative neutral
Pair 2 	= NR, neutral relative
Pair 3 	= NN, neutral neutral
r = right, i.e. defection
d = down, i.e. cooperation*/

label var punish_right_Pair1 "PSP in pairRN (Player A=relative,Player B=neutral)"
label var punish_down_Pair1 "ASP in pairRN (Player A=relative,Player B=neutral)"
label var punish_right_Pair2 "PSP in pairNR (Player A=neutral,Player B=relative)"
label var punish_down_Pair2 "ASP in pairNR (Player A=neutral,Player B=relative)"
label var punish_right_Pair3 "PSP in pairNN (Player A=neutral,Player B=neutral)"
label var punish_down_Pair3 "ASP in pairNN (Player A=neutral,Player B=neutral)"

rename punish_right_Pair1 PSP_RN
rename punish_right_Pair2 PSP_NR
rename punish_right_Pair3 PSP_NN
rename punish_down_Pair1 ASP_RN
rename punish_down_Pair2 ASP_NR
rename punish_down_Pair3 ASP_NN


save "$working_ANALYSIS\processed\leader_clean.dta", replace





*----------------------------
* 2) Clean villager data
*----------------------------
clear
use "$working_ANALYSIS\data\villagers_raw.dta"

*** Generate Village-Variable in byte-Format ***
/* Necessary if we want to control for village-fixed effects.
Note: need to make sure that we use the same codes in leader dataset*/

gen village = Village
replace village = "1" if village=="Edundja"
replace village = "2" if village=="Ehafo"
replace village = "3" if village=="Endola"
replace village = "4" if village=="Epatululo"
replace village = "5" if village=="Epuku"
replace village = "6" if village=="Etale"
replace village = "7" if village=="Etilashi"
replace village = "8" if village=="Etomba"
replace village = "9" if village=="Ohangwena"
replace village = "10" if village=="Oipapakane"
replace village = "11" if village=="Okadiwa"
replace village = "12" if village=="Okafitu"
replace village = "13" if village=="Okakwa"
replace village = "14" if village=="Okangudi"
replace village = "15" if village=="Okapundja"
replace village = "16" if village=="Okatale"
replace village = "17" if village=="Okelemba"
replace village = "18" if village=="Omahenge"
replace village = "19" if village=="Omakango"
replace village = "20" if village=="Omalyata"
replace village = "21" if village=="Omaputo"
replace village = "22" if village=="Omukuve"
replace village = "23" if village=="Omutwe Oshimbungu"
replace village = "24" if village=="Onangwhe"
replace village = "25" if village=="Onekwaya East"
replace village = "26" if village=="Onekwaya west"
replace village = "27" if village=="Onengali"
replace village = "28" if village=="Ongonga"
replace village = "29" if village=="Onheleiwa"
replace village = "30" if village=="Oshikwiyu A"
replace village = "31" if village=="Oshikwiyu B"
replace village = "32" if village=="Ouhongo"
destring village, replace

#delimit;
label define village 
1 "Edundja"
2 "Ehafo"
3 "Endola"
4 "Epatululo"
5 "Epuku"
6 "Etale"
7 "Etilashi"
8 "Etomba"
9 "Ohangwena"
10 "Oipapakane"
11 "Okadiwa"
12 "Okafitu"
13 "Okakwa"
14 "Okangudi"
15 "Okapundja"
16 "Okatale"
17 "Okelemba"
18 "Omahenge"
19 "Omakango"
20 "Omalyata"
21 "Omaputo"
22 "Omukuve"
23 "Omutwe Oshimbungu"
24 "Onangwhe"
25 "Onekwaya East"
26 "Onekwaya West"
27 "Onengali"
28 "Ongonga"
29 "Onheleiwa"
30 "Oshikwiyu 1"
31 "Oshikwiyu 2"
32 "Ouhongo";
label values village village;
#delimit cr
quietly tab village Village //ok, everything correct
drop Village // not necessary anymore
label variable village "village names"
tab village, gen(village_)

*** Generate unique Villager-ID starting from 64 onwards ***
sort village PNo
rename PNo session_id
gen id = _n+64
label variable id "unique identifier"
duplicates report id //ok

*** Day and Month ***
rename Day day_exp
label variable day_exp "Day of experiment conduct"
rename Month month_exp
label variable day_exp "Month of experiment conduct"

rename Relationshiptoleader relation
label var relation "session_id 1-2 related TL, 3-4 DEL, 5-12 neutral"
rename solvedallcontrolquestion solved_all
label var solved_all "Participant answered all control questions correctly"

rename Name name
label var name "Participant's name"


**** MODULE A: PERSONAL CHARACTERISTICS ****

rename A1gender male
recode male (2=0)
label variable male "dummy =1 if male, 0 if women"
list name id if male==.
replace male=1 if id==142
replace male=0 if id==150
replace male=0 if id==360
replace male=1 if id==364

*** Rename important Socio-demos ***

rename A3age age
label variable age "age in years"

rename A4a education
label var education "Highest grade attained at school"

rename A6 a6
label variable a6 "Since when living in village"
/* Later we may create a variable based on A6 and age, taking 1 if person has liv
> ed in village since birth */

rename A9ethnicity a9eth
label var a9eth "What ethnic group do you identify with?"
rename A9bclan a9bclan
label var a9bclan " If Ovambo, please specify the clan you identify with"
***MODULE A DONE***

**** MODULE B: VILLAGE LIFE AND LOCAL ORGANIZATIONS****

rename B1 b1
label var b1 "What would you say is the most influential village position?"
#delimit;
label define b1
1 "TL"
2 "DEL"
3 "trad. healer/witch-doc"
4 "local councilor"
5 "local priest/pastor"
6 "other, define";
label values b1 b1;
#delimit cr

rename B2a b2a
label var b2a "who is responsible for access to and use of the village’s water resources"
#delimit;
label define b2
1 "WPC committee"
2 "TL"
3 "local government"
4 "villagers"
5 "each individual him/herself"
6 "someone else";
label values b2a b2;
#delimit cr

rename B2b b2b
label var b2b "who responsible for access to and use of the village’s agricultural land"
label values b2b b2

rename B2c b2c
label var b2c "who responsible for access to and use of the village’s grazing land "
label values b2c b2

rename B3 b3
label var b3 "Name of WPC?"

rename B4 b4
recode b4 (2=0)
label var b4 "Member of WPC-committee?"

rename B5a b5a
recode b5a (2=0)
label var b5a "Vote in last wp-committee election?"

rename B5b b5b
recode b5b (2=0)
label var b5b " If yes, did you vote for the current WPC"

rename B6candidates b6candidates
label var b6candidates "How many candidates in the last election?"

global a ""a""b""c""d""e""f""
foreach e of global a{
rename B7`e' b7`e'
replace b7`e'=. if b7`e'==5
}
#delimit;
label define agree
1"Strongly disagree"
2"Disagree a little"
3"Agree a little"
4"Strongly agree"
5"Don’t know";
# delimit cr

label var b7a "election votes were counted by a group of trustworthy people"
label value b7a agree

label var b7b "elections held in privat"
label value b7b agree

label var b7c "intimidation of candidates running for office"
label value b7c agree

label var b7d "elected candidates made promises that were they didnt keep"
label value b7d agree

label var b7e "people were paid to vote for a certain candidate"
label value b7e agree

label var b7f "I felt pressure to vote for a ceratin candidate"
label value b7f agree


global b ""a""b""c""d""
foreach e of global b{
rename B8`e' b8`e'
replace b8`e'=. if b8`e'==5
}

label var b8a "Small repairs are directly undertaken by the WPC"
label values b8a agree

label var b8b "WPC takes decisions in a way that they are understandable for the people"
label value b8b agree

label var b8c "WPC takes action against people who wrongly report/refuse to pay the fees"
label value b8c agree

label var b8d "DEL exploits his/her position for own benefit"
label value b8d agree

rename B9 b9
label var b9 "How many people report false water consumption?"
#delimit;
label define b9
1 "Almost all"
2 "More than half"
3 "Less than half, but still a sizeable fraction"
4 "Only very few"
5 "None"
6 "Don’t know";
# delimit cr
label value b9 b9

global c ""a""b""c""
foreach e of global c{
rename B10`e' b10`e'
}
label var b10a "Overall satisfaction with WP-committee?"
#delimit;
label define satisfied
1 "very satisfied"
2 "somewhat satisfied"
3 "somewhat dissatisfied"
4 "very dissatisfied"
5 "don't know";
#delimit cr
label value b10a satisfied

label var b10b "Satisfaction with performance of DEL?"
label value b10b satisfied

label var b10c "Would you vote for the current chairperson(if tomorrow would be elections?"
recode b10c (2=0)

global d ""a""b""c""d""e""f""g""h""i""
foreach e of global d{
rename B11`e' b11`e'
replace b11`e'=. if b11`e'==5
}
label var b11a "everybody from the village could become TL?"
label value b11a agree

label var b11b "TL made promises he didn't keep?"
label value b11b agree

label var b11c "TL allocates access to farming/grazin land in a fair manner?"
label value b11c agree

label var b11d "In caes of Arguments/conflicts the TL tries to find fair solutions?"
label value b11d agree

label var b11e "TL treats people equally in the trad. court?"
label value b11e agree

label var b11f "TL takes actions against people who disobey rules?"
label value b11f agree

label var b11g "TL takes decisions so they are understandable?"
label value b11g agree

label var b11h "TL eploits his position to his own interest?"
label value b11h agree

label var b11i"Creation of WP-committee reduced power of TL?"
label value b11i agree

rename B12asatisfactionheadman b12satisfactionTL
label var b12satisfactionTL "How satisfied are you with the performance of the village headman?"
label value b12satisfactionTL satisfied

rename B12b b12b
recode b12b (2=0)
label var b12b "If there was an election for the TL, would you vote for the current TL?"
***MODULE B DONE***

**** MODULE C: OPINIONS****

forvalues e=1/12{
rename C`e' c`e'
replace c`e'=. if c`e'==5
}

label var c1 "C1. For important decisions members of a group should be allowed to vote"
label var c2 "C2. Most people can learn to be leaders- it’s not a matter of birth"  
label var c3 "C3. Democratic elections in this village ensure that the elected authorities act in the interests of their people"
label var c4 "C4. In most cases, failures in this village are traceable to  bad leadership"
label var c5 "C5. Schools and parents should teach children to obey authority"
label var c6 "C6. Favouring friends, relatives or supporters over others in the course of ones duties as a leader is sometimes justified"
label var c7 "C7. On the whole, men make better political leaders than women do"
label var c8 "C8. A leader has to serve all his/her people including those who did not vote for him/her or are not friends with him/her" 
label var c9 "C9. Accepting a bribe in the course of ones duties is sometimes justified"
label var c10 "C10. Most people in this village can be trusted" 
label var c11 "C11. On the whole, more-educated people make better political leaders than the less-educated do"
label var c12 "C12. On the whole, elders make better political leaders than the youth do"

forvalues e=1/12{
label values c`e' agree
}
***MODULE C DONE***

**** MODULE D: ECONOMIC SITUATION****

*** Recode Asset variables, 0 instead of 2 for no ***
rename D1incomeso d1
label variable d1 "Income source"
# delimit;
label define is
1"Public sector employee"
2"Private sector employee"
3"Agriculture"
4"Livestock farming"
5"Own business (self-employment)"
6"Pension"
7"Transfers"
8"Other";
label values d1 is;
# delimit cr
replace d1=6 if d1==9

rename D1b d1b
labe var d1b "Personal monthly income"

rename D2 d2
label var d2 "Household monthly income"
replace d2=600 if d1==6 & (d2==0 | d2==.)
/* We may not use d2, data are shit */

rename D3a d3a
rename D3b d3b
rename D3c d3c
label var d3a "Cattle"
label var d3b "Sheep"
label var d3c "Goats"

gen ssu = d3a*6+d3b+d3c
label var ssu "Livestock possession in small stock units, 1 cattle equivalent to 6 goat/sheep"


rename D4 d4
label variable d4 "Comparison to others"
label define d4 1"Better off" 2"Worse off" 3"Equal"
label values d4 d4

rename D5_etemba D5l_etemba

global f ""a""b""c""d""e""f""g""h""i""j""k""l""m""n""
foreach e of global f{
recode D5`e'* 2=0
}
foreach e of global f{
rename D5`e' d5`e'
}
label var d5a "Radio"
label var d5b "Television"
label var d5c "Cell phone"
label var d5d "Fridge, Refrigerator"
label var d5e "Freezer"
label var d5f "Washing machine"
label var d5g "Stove (electric, paraffin, kerosin"
label var d5h "Sewing machine"
label var d5i "Car"
label var d5j "Motorcycle / Scooter"
label var d5k "Bicycle"
label var d5l "Donkey-cart / Ox-cart"
label var d5m "Generator"
label var d5n "Plough"

rename D6watersource d6
label var d6 "HH's main source of water"
# delimit;
label define d6
1"Private tap"
2"Public tap"
3"Water tank"
4"Well"
5"Other";
# delimit cr
label values d6 d6

rename D7afl d7a
replace d7a=5 if d7a==6
label var d7a "HH's material for floor"
# delimit;
label define d7a
1"Sand"
2"concrete"
3"mud, clay and/or cow dung"
4"wood"
5"other";
# delimit cr
label values d7a d7a

rename D7bwa d7b
replace d7b=5 if d7b==6
label var d7b "HH's main material for walls"
#delimit;
label define d7b
1"cement blocks, bricks, stones"
2"corrugated iron, zinc"
3"sticks"
4"thatch, grass"
5"other";
#delimit cr
label values d7b d7b

***MODULE D DONE***

***************************************************
***MODULE E: Relationship to other participants***
***************************************************

rename e1afriends e1a
label var e1a "close friends"
rename e1brelatives e1b
label var e1b "relatives of you"
rename e1cfight e1c
label var e1c "argument / fight in the past with"

rename e2ahead e2a
label var e2a " How is your relationship to the TL"
#delimit;
label define e2
1 "close friend of mine"
2 "relative of mine"
3 "neither friend nor relative of mine";
#delimit cr
label values e2a e2

rename e2bchair e2b
label var e2b "How is your relationship the the DEL"
label values e2b e2
***MODULE E DONE***



**************************************
*** Procedural-Fairness: Game 1 ***
**************************************
rename expectation_vi expec_game1
label var expec_game1 "Villagers expectation if leader allows for a vote"

label var vote_1 "Benefit to leader at the expense of villagers" /* A=(100,10,10,10,10,10,10)	vs.	B=(40,20,20,20,20,20,20) */
label var vote_2 "Benefit to leader at no cost to villagers" /* A=(60,20,20,20,20,20,20) 	vs.	B=(40,20,20,20,20,20,20)*/
label var vote_3 "Unequal villagers" /* A=(40,5,23,23,23,23,23)	vs. 	B=(40,20,20,20,20,20,20)*/
label var vote_4 "Spiteful Leader" /* A=(40,15,15,15,15,15,15) 	vs.	B=(40,20,20,20,20,20,20)*/

label var majority_1 "Majority-decision of group independent of implemented rule by leader"
label var majority_2 "Majority-decision of group independent of implemented rule by leader"
label var majority_3 "Majority-decision of group independent of implemented rule by leader"
label var majority_4 "Majority-decision of group independent of implemented rule by leader"

foreach var of varlist vote_*{
replace `var'="1" if `var'=="a"
replace `var'="2" if `var'=="b"
destring `var', replace
label values `var' decision
}

**************************************
*** Social type, Preference-Game 2 ***
**************************************

label variable decision1 "Envy trade-off 5,5 vs 5,0" /* A=[5,5] B=[5,0] -> first number player himself, second number is rdm player from participants*/
label variable decision2 "Prosocial trade-off 5,5 vs 5,10" /* A=[5,5] B=[5,10] */
label variable decision3 "Selfish trade-off 5,5 vs 10,0" /* A=[5,5] B=[10,0] */
label define decision 1"Chose A" 2"Chose B"

foreach var of varlist decision*{
replace `var'="1" if `var'=="a"
replace `var'="2" if `var'=="b"
destring `var', replace
label values `var' decision
}

**************************************
*** Nepotism(TG-TTP)-Game 3 ***
**************************************
rename Role positionTG
label var positionTG "Indicates whether participant is Player A or B in Trust Game"

gen pair="."
label var pair "indicates in which pair the player is"
replace pair="RN_TL" if Pair=="FV_TL"
replace pair="NR_TL" if Pair=="VF_TL"
replace pair="NN_TL" if Pair=="VV_TL"
replace pair="RN_DEL" if Pair=="FV_DL"
replace pair="NR_DEL" if Pair=="VF_DL"
replace pair="NN_DEL" if Pair=="VV_DL"

rename Trust decision_A
label var decision_A "Dummy for decision of Player A, 1==coop 0==defection"

rename Trustwort decision_B
label var decision_B "Dummy for decision of Player B, 1==coop 0==defection"

rename expectation_pl expec_B
label var expec_B "Expectation of Player B whether leader will punish him"



save "$working_ANALYSIS\processed\villager_clean.dta", replace





*----------------------------
* 3) Append both datasets
*----------------------------
append using "$working_ANALYSIS\processed\leader_clean.dta"


duplicates report id /*ok, 448*/
sort id
replace name=Name if name==""
drop Name
label variable session_id "Player ID of villager in games"
order id name village


foreach var of varlist  pos_pairRN pos_pairNR pos_pairNN{
label var `var' "Order of pair in nepotism game"
}

drop BX /* Consistent with "village", do not need it anymore*/
drop testCC
drop Pair

*** Leader session_id ***
replace session_id=13 if TL==1
replace session_id=14 if DEL==1

*** variable to identify all 3 groups
gen villager = 0
replace villager = 1 if session_id<13
gen id1=0
replace id1=1 if TL==1
replace id1=2 if villager==1
label define setup 0 "DEL" 1 "Chiefs" 2 "Villagers"
label values id1 setup

save "$working_ANALYSIS\processed\data_clean.dta", replace




** EOF