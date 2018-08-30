
 
 
/********************** Version 2 (Analyze all States, Interaction Term) ********************************/ 
/*** Analyzing (freq_smoker) ***/
/** 1997 & 1999 **/
use "\\IASTATE.EDU\ECON\GRADSTUDENTS\OTHERDATA\sjcho\DESKTOP\Manski_Method.dta", clear
matrix pro_97_99 = J(55,2,.) // Make a empty matrix.
svy, subpop(if (year==1997|year==1999) &(age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy1999 NJ1999 i.stfips, nolog 

gen cc = _b[3b.age]*3b.age+_b[4.age]*4.age+_b[5.age]*5.age+_b[6.age]*6.age+_b[1b.sex]*1b.sex+_b[2.sex]*2.sex+_b[1b.race4]*1b.race4 ///
+_b[2.race4]*2.race4+_b[3.race4]*3.race4+_b[4.race4]*4.race4+_b[Average_Cost]*Average_Cost ///
+_b[adult_rate]*adult_rate+_b[dummy1999]*dummy1999+_b[NJ1999]*NJ1999+_b[4.stfips]*4.stfips+_b[6.stfips]*6.stfips ///
+_b[12.stfips]*12.stfips+_b[13.stfips]*13.stfips+_b[26.stfips]*26.stfips+_b[34.stfips]*34.stfips+_b[36.stfips]*36.stfips ///
+_b[48.stfips]*48.stfips+_b[55.stfips]*55.stfips+_b[_cons] // Include every coefficient.
gen phat=exp(cc)/(1+exp(cc))
svy, subpop(if stfips== 34 & year==1999 & age_r==1): mean phat // New Jersey, 1999
matrix pro_97_99[34,2] = e(b)

svy, subpop(if (year==1997|year==1999)& (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy1999 NJ1999 i.stfips, nolog 
gen cc_time = cc-_b[dummy1999]*dummy1999-_b[NJ1999]*NJ1999
gen phat_time=exp(cc_time)/(1+exp(cc_time))
svy, subpop(if stfips== 34 & year==1999 & age_r==1): mean phat_time 
matrix pro_97_99[34,1] = e(b) // New Jersey, 1997 
 
gen cc_interstate=.
gen phat_interstate=.

foreach i in 4 6 12 13 26 36 48 55 {
svy, subpop(if (year==1997|year==1999)& (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy1999 NJ1999 i.stfips, nolog 
replace cc_interstate=cc-_b[34.stfips]*34.stfips-_b[NJ1999]*NJ1999+_b[`i'.stfips]
replace phat_interstate=exp(cc_interstate)/(1+exp(cc_interstate))
svy, subpop(if stfips== 34 & year==1999 & age_r==1): mean phat_interstate
matrix pro_97_99[`i',2] = e(b) 
}

foreach i in 4 6 12 13 26 36 48 55 {
svy, subpop(if (year==1997|year==1999)& (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy1999 NJ1999 i.stfips, nolog 
replace cc_interstate=.
replace cc_interstate=cc-_b[dummy1999]*dummy1999-_b[NJ1999]*NJ1999-_b[34.stfips]*34.stfips+_b[`i'.stfips]
replace phat_interstate=exp(cc_interstate)/(1+exp(cc_interstate))
svy, subpop(if stfips== 34 & year==1999 & age_r==1): mean phat_interstate 
matrix pro_97_99[`i',1] = e(b) 
} 

matrix list pro_97_99

matrix predict_97_99= J(9,2,.) 

forvalues i = 1(1)2 {
matrix predict_97_99[1,`i']=pro_97_99[4,`i']
matrix predict_97_99[2,`i']=pro_97_99[6,`i']
matrix predict_97_99[3,`i']=pro_97_99[12,`i']
matrix predict_97_99[4,`i']=pro_97_99[13,`i']
matrix predict_97_99[5,`i']=pro_97_99[26,`i']
matrix predict_97_99[6,`i']=pro_97_99[36,`i']
matrix predict_97_99[7,`i']=pro_97_99[48,`i']
matrix predict_97_99[8,`i']=pro_97_99[55,`i']
matrix predict_97_99[9,`i']=pro_97_99[34,`i']
} // Rearrange Matrix

matrix colnames predict_97_99 = "1997" "1999" 
matrix rownames predict_97_99 = "Arizona" "California" "Florida" "Georgia" "Michigan" "New York" "Texas" "Wisconsin" "New Jersey"

matrix list predict_97_99

matrix TE = J(18,7,.)   //Treatment Effect
matrix colnames TE = "1997&1999" "1999&2001" "2001&2003" "2003&2005" "2005&2007" "2005&2009" "2005&2011" 
matrix rownames TE = "Time" "AZ State" "CA State" "FL State" "GA State" "MI State" "NY State"  ///
"TX State" "WI State" "AZ DID" "CA DID" "FL DID" "GA DID" "MI DID" "NY DID" "TX DID" "WI DID" "Puhani"

matrix TE[1,1]=predict_97_99[9,2]-predict_97_99[9,1] //Time

forvalues i= 1(1)8 {
matrix TE[`i'+1,1]=predict_97_99[9,2]-predict_97_99[`i',2]
} // Interstate

forvalues i= 1(1)8 {
matrix TE[`i'+9,1]=predict_97_99[9,2]-predict_97_99[9,1]-predict_97_99[`i',2]+predict_97_99[`i',1]
} // DID

svy, subpop(if (year==1997|year==1999)& (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy1999 NJ1999 i.stfips, nolog 
gen cc_DID = cc-_b[NJ1999]*NJ1999
gen phat_DID=exp(cc_DID)/(1+exp(cc_DID))
svy, subpop(if stfips== 34 & year==1999 & age_r==1): mean phat_DID // Puhani's method
matrix TE[18,1]=predict_97_99[9,2]-e(b)

matrix list TE


/*** Analyzing (freq_smoker) ***/
/** 1999 & 2001 **/
use "\\IASTATE.EDU\ECON\GRADSTUDENTS\OTHERDATA\sjcho\DESKTOP\Manski_Method.dta", clear
matrix pro_99_01 = J(55,2,.) // Make a empty matrix.
svy, subpop(if (year==1999|year==2001) &(age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2001 NJ2001 i.stfips, nolog 

gen cc = _b[3b.age]*3b.age+_b[4.age]*4.age+_b[5.age]*5.age+_b[6.age]*6.age+_b[1b.sex]*1b.sex+_b[2.sex]*2.sex+_b[1b.race4]*1b.race4 ///
+_b[2.race4]*2.race4+_b[3.race4]*3.race4+_b[4.race4]*4.race4+_b[Average_Cost]*Average_Cost ///
+_b[adult_rate]*adult_rate+_b[dummy2001]*dummy2001+_b[NJ2001]*NJ2001+_b[4.stfips]*4.stfips+_b[6.stfips]*6.stfips ///
+_b[12.stfips]*12.stfips+_b[13.stfips]*13.stfips+_b[26.stfips]*26.stfips+_b[34.stfips]*34.stfips+_b[36.stfips]*36.stfips ///
+_b[48.stfips]*48.stfips+_b[55.stfips]*55.stfips+_b[_cons] // Include every coefficient.
gen phat=exp(cc)/(1+exp(cc))
svy, subpop(if stfips== 34 & year==2001 & age_r==1): mean phat // New Jersey, 2001
matrix pro_99_01[34,2] = e(b)

svy, subpop(if (year==1999|year==2001)& (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2001 NJ2001 i.stfips, nolog 
gen cc_time = cc-_b[dummy2001]*dummy2001-_b[NJ2001]*NJ2001
gen phat_time=exp(cc_time)/(1+exp(cc_time))
svy, subpop(if stfips== 34 & year==2001 & age_r==1): mean phat_time 
matrix pro_99_01[34,1] = e(b) // New Jersey, 1999 
 
gen cc_interstate=.
gen phat_interstate=.

foreach i in 4 6 12 13 26 36 48 55 {
svy, subpop(if (year==1999|year==2001)& (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2001 NJ2001 i.stfips, nolog 
replace cc_interstate=cc-_b[34.stfips]*34.stfips-_b[NJ2001]*NJ2001+_b[`i'.stfips]
replace phat_interstate=exp(cc_interstate)/(1+exp(cc_interstate))
svy, subpop(if stfips== 34 & year==2001 & age_r==1): mean phat_interstate
matrix pro_99_01[`i',2] = e(b) 
}

foreach i in 4 6 12 13 26 36 48 55 {
svy, subpop(if (year==1999|year==2001)& (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2001 NJ2001 i.stfips, nolog 
replace cc_interstate=.
replace cc_interstate=cc-_b[dummy2001]*dummy2001-_b[NJ2001]*NJ2001-_b[34.stfips]*34.stfips+_b[`i'.stfips]
replace phat_interstate=exp(cc_interstate)/(1+exp(cc_interstate))
svy, subpop(if stfips== 34 & year==2001 & age_r==1): mean phat_interstate 
matrix pro_99_01[`i',1] = e(b) 
} 

matrix list pro_99_01

matrix predict_99_01= J(9,2,.) 

forvalues i = 1(1)2 {
matrix predict_99_01[1,`i']=pro_99_01[4,`i']
matrix predict_99_01[2,`i']=pro_99_01[6,`i']
matrix predict_99_01[3,`i']=pro_99_01[12,`i']
matrix predict_99_01[4,`i']=pro_99_01[13,`i']
matrix predict_99_01[5,`i']=pro_99_01[26,`i']
matrix predict_99_01[6,`i']=pro_99_01[36,`i']
matrix predict_99_01[7,`i']=pro_99_01[48,`i']
matrix predict_99_01[8,`i']=pro_99_01[55,`i']
matrix predict_99_01[9,`i']=pro_99_01[34,`i']
} // Rearrange Matrix

matrix colnames predict_99_01 = "1999" "2001" 
matrix rownames predict_99_01 = "Arizona" "California" "Florida" "Georgia" "Michigan" "New York" "Texas" "Wisconsin" "New Jersey"

matrix list predict_99_01

matrix TE[1,2]=predict_99_01[9,2]-predict_99_01[9,1] //Time

forvalues i= 1(1)8 {
matrix TE[`i'+1,2]=predict_99_01[9,2]-predict_99_01[`i',2]
} // Interstate

forvalues i= 1(1)8 {
matrix TE[`i'+9,2]=predict_99_01[9,2]-predict_99_01[9,1]-predict_99_01[`i',2]+predict_99_01[`i',1]
} // DID

svy, subpop(if (year==1999|year==2001)& (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2001 NJ2001 i.stfips, nolog 
gen cc_DID = cc-_b[NJ2001]*NJ2001
gen phat_DID=exp(cc_DID)/(1+exp(cc_DID))
svy, subpop(if stfips== 34 & year==2001 & age_r==1): mean phat_DID // Puhani's method
matrix TE[18,2]=predict_99_01[9,2]-e(b)

matrix list TE


/*** Analyzing (freq_smoker) ***/
/** 2001 & 2003 **/
use "\\IASTATE.EDU\ECON\GRADSTUDENTS\OTHERDATA\sjcho\DESKTOP\Manski_Method.dta", clear
matrix pro_01_03 = J(55,2,.) // Make a empty matrix.
svy, subpop(if (year==2001|year==2003) &(age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2003 NJ2003 i.stfips, nolog 

gen cc = _b[3b.age]*3b.age+_b[4.age]*4.age+_b[5.age]*5.age+_b[6.age]*6.age+_b[1b.sex]*1b.sex+_b[2.sex]*2.sex+_b[1b.race4]*1b.race4 ///
+_b[2.race4]*2.race4+_b[3.race4]*3.race4+_b[4.race4]*4.race4+_b[Average_Cost]*Average_Cost ///
+_b[adult_rate]*adult_rate+_b[dummy2003]*dummy2003+_b[NJ2003]*NJ2003+_b[4.stfips]*4.stfips+_b[6.stfips]*6.stfips ///
+_b[12.stfips]*12.stfips+_b[13.stfips]*13.stfips+_b[26.stfips]*26.stfips+_b[34.stfips]*34.stfips+_b[36.stfips]*36.stfips ///
+_b[48.stfips]*48.stfips+_b[55.stfips]*55.stfips+_b[_cons] // Include every coefficient.
gen phat=exp(cc)/(1+exp(cc))
svy, subpop(if stfips== 34 & year==2003 & age_r==1): mean phat // New Jersey, 2003
matrix pro_01_03[34,2] = e(b)

svy, subpop(if (year==2001|year==2003)& (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2003 NJ2003 i.stfips, nolog 
gen cc_time = cc-_b[dummy2003]*dummy2003-_b[NJ2003]*NJ2003
gen phat_time=exp(cc_time)/(1+exp(cc_time))
svy, subpop(if stfips== 34 & year==2003 & age_r==1): mean phat_time 
matrix pro_01_03[34,1] = e(b) // New Jersey, 2001 
 
gen cc_interstate=.
gen phat_interstate=.

foreach i in 4 6 12 13 26 36 48 55 {
svy, subpop(if (year==2001|year==2003)& (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2003 NJ2003 i.stfips, nolog 
replace cc_interstate=cc-_b[34.stfips]*34.stfips-_b[NJ2003]*NJ2003+_b[`i'.stfips]
replace phat_interstate=exp(cc_interstate)/(1+exp(cc_interstate))
svy, subpop(if stfips== 34 & year==2003 & age_r==1): mean phat_interstate
matrix pro_01_03[`i',2] = e(b) 
}

foreach i in 4 6 12 13 26 36 48 55 {
svy, subpop(if (year==2001|year==2003)& (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2003 NJ2003 i.stfips, nolog 
replace cc_interstate=.
replace cc_interstate=cc-_b[dummy2003]*dummy2003-_b[NJ2003]*NJ2003-_b[34.stfips]*34.stfips+_b[`i'.stfips]
replace phat_interstate=exp(cc_interstate)/(1+exp(cc_interstate))
svy, subpop(if stfips== 34 & year==2003 & age_r==1): mean phat_interstate 
matrix pro_01_03[`i',1] = e(b) 
} 

matrix list pro_01_03

matrix predict_01_03= J(9,2,.) 

forvalues i = 1(1)2 {
matrix predict_01_03[1,`i']=pro_01_03[4,`i']
matrix predict_01_03[2,`i']=pro_01_03[6,`i']
matrix predict_01_03[3,`i']=pro_01_03[12,`i']
matrix predict_01_03[4,`i']=pro_01_03[13,`i']
matrix predict_01_03[5,`i']=pro_01_03[26,`i']
matrix predict_01_03[6,`i']=pro_01_03[36,`i']
matrix predict_01_03[7,`i']=pro_01_03[48,`i']
matrix predict_01_03[8,`i']=pro_01_03[55,`i']
matrix predict_01_03[9,`i']=pro_01_03[34,`i']
} // Rearrange Matrix

matrix colnames predict_01_03 = "2001" "2003" 
matrix rownames predict_01_03 = "Arizona" "California" "Florida" "Georgia" "Michigan" "New York" "Texas" "Wisconsin" "New Jersey"

matrix list predict_01_03

matrix TE[1,3]=predict_01_03[9,2]-predict_01_03[9,1] //Time

forvalues i= 1(1)8 {
matrix TE[`i'+1,3]=predict_01_03[9,2]-predict_01_03[`i',2]
} // Interstate

forvalues i= 1(1)8 {
matrix TE[`i'+9,3]=predict_01_03[9,2]-predict_01_03[9,1]-predict_01_03[`i',2]+predict_01_03[`i',1]
} // DID

svy, subpop(if (year==2001|year==2003)& (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2003 NJ2003 i.stfips, nolog 
gen cc_DID = cc-_b[NJ2003]*NJ2003
gen phat_DID=exp(cc_DID)/(1+exp(cc_DID))
svy, subpop(if stfips== 34 & year==2003 & age_r==1): mean phat_DID // Puhani's method
matrix TE[18,3]=predict_01_03[9,2]-e(b)

matrix list TE

/*** Analyzing (freq_smoker) ***/
/** 2003 & 2005 **/
use "\\IASTATE.EDU\ECON\GRADSTUDENTS\OTHERDATA\sjcho\DESKTOP\Manski_Method.dta", clear
matrix pro_03_05 = J(55,2,.) // Make a empty matrix.
svy, subpop(if (year==2003|year==2005) &(age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2005 NJ2005 i.stfips, nolog 

gen cc = _b[3b.age]*3b.age+_b[4.age]*4.age+_b[5.age]*5.age+_b[6.age]*6.age+_b[1b.sex]*1b.sex+_b[2.sex]*2.sex+_b[1b.race4]*1b.race4 ///
+_b[2.race4]*2.race4+_b[3.race4]*3.race4+_b[4.race4]*4.race4+_b[Average_Cost]*Average_Cost ///
+_b[adult_rate]*adult_rate+_b[dummy2005]*dummy2005+_b[NJ2005]*NJ2005+_b[4.stfips]*4.stfips+_b[6.stfips]*6.stfips ///
+_b[12.stfips]*12.stfips+_b[13.stfips]*13.stfips+_b[26.stfips]*26.stfips+_b[34.stfips]*34.stfips+_b[36.stfips]*36.stfips ///
+_b[48.stfips]*48.stfips+_b[55.stfips]*55.stfips+_b[_cons] // Include every coefficient.
gen phat=exp(cc)/(1+exp(cc))
svy, subpop(if stfips== 34 & year==2005 & age_r==1): mean phat // New Jersey, 2005
matrix pro_03_05[34,2] = e(b)

svy, subpop(if (year==2003|year==2005)& (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2005 NJ2005 i.stfips, nolog 
gen cc_time = cc-_b[dummy2005]*dummy2005-_b[NJ2005]*NJ2005
gen phat_time=exp(cc_time)/(1+exp(cc_time))
svy, subpop(if stfips== 34 & year==2005 & age_r==1): mean phat_time 
matrix pro_03_05[34,1] = e(b) // New Jersey, 2003 
 
gen cc_interstate=.
gen phat_interstate=.

foreach i in 4 6 12 13 26 36 48 55 {
svy, subpop(if (year==2003|year==2005)& (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2005 NJ2005 i.stfips, nolog 
replace cc_interstate=cc-_b[34.stfips]*34.stfips-_b[NJ2005]*NJ2005+_b[`i'.stfips]
replace phat_interstate=exp(cc_interstate)/(1+exp(cc_interstate))
svy, subpop(if stfips== 34 & year==2005 & age_r==1): mean phat_interstate
matrix pro_03_05[`i',2] = e(b) 
}

foreach i in 4 6 12 13 26 36 48 55 {
svy, subpop(if (year==2003|year==2005)& (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2005 NJ2005 i.stfips, nolog 
replace cc_interstate=.
replace cc_interstate=cc-_b[dummy2005]*dummy2005-_b[NJ2005]*NJ2005-_b[34.stfips]*34.stfips+_b[`i'.stfips]
replace phat_interstate=exp(cc_interstate)/(1+exp(cc_interstate))
svy, subpop(if stfips== 34 & year==2005 & age_r==1): mean phat_interstate 
matrix pro_03_05[`i',1] = e(b) 
} 

matrix list pro_03_05

matrix predict_03_05= J(9,2,.) 

forvalues i = 1(1)2 {
matrix predict_03_05[1,`i']=pro_03_05[4,`i']
matrix predict_03_05[2,`i']=pro_03_05[6,`i']
matrix predict_03_05[3,`i']=pro_03_05[12,`i']
matrix predict_03_05[4,`i']=pro_03_05[13,`i']
matrix predict_03_05[5,`i']=pro_03_05[26,`i']
matrix predict_03_05[6,`i']=pro_03_05[36,`i']
matrix predict_03_05[7,`i']=pro_03_05[48,`i']
matrix predict_03_05[8,`i']=pro_03_05[55,`i']
matrix predict_03_05[9,`i']=pro_03_05[34,`i']
} // Rearrange Matrix

matrix colnames predict_03_05 = "2003" "2005" 
matrix rownames predict_03_05 = "Arizona" "California" "Florida" "Georgia" "Michigan" "New York" "Texas" "Wisconsin" "New Jersey"

matrix list predict_03_05

matrix TE[1,4]=predict_03_05[9,2]-predict_03_05[9,1] //Time

forvalues i= 1(1)8 {
matrix TE[`i'+1,4]=predict_03_05[9,2]-predict_03_05[`i',2]
} // Interstate

forvalues i= 1(1)8 {
matrix TE[`i'+9,4]=predict_03_05[9,2]-predict_03_05[9,1]-predict_03_05[`i',2]+predict_03_05[`i',1]
} // DID

svy, subpop(if (year==2003|year==2005)& (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2005 NJ2005 i.stfips, nolog 
gen cc_DID = cc-_b[NJ2005]*NJ2005
gen phat_DID=exp(cc_DID)/(1+exp(cc_DID))
svy, subpop(if stfips== 34 & year==2005 & age_r==1): mean phat_DID // Puhani's method
matrix TE[18,4]=predict_03_05[9,2]-e(b)

matrix list TE


/*** Analyzing (freq_smoker) ***/
/** 2005 & 2007 **/
use "\\IASTATE.EDU\ECON\GRADSTUDENTS\OTHERDATA\sjcho\DESKTOP\Manski_Method.dta", clear
matrix pro_05_07 = J(55,2,.) // Make a empty matrix.
svy, subpop(if (year==2005|year==2007) &(age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2007 NJ2007 i.stfips, nolog 

gen cc = _b[3b.age]*3b.age+_b[4.age]*4.age+_b[5.age]*5.age+_b[6.age]*6.age+_b[1b.sex]*1b.sex+_b[2.sex]*2.sex+_b[1b.race4]*1b.race4 ///
+_b[2.race4]*2.race4+_b[3.race4]*3.race4+_b[4.race4]*4.race4+_b[Average_Cost]*Average_Cost ///
+_b[adult_rate]*adult_rate+_b[dummy2007]*dummy2007+_b[NJ2007]*NJ2007+_b[4.stfips]*4.stfips+_b[6.stfips]*6.stfips ///
+_b[12.stfips]*12.stfips+_b[13.stfips]*13.stfips+_b[26.stfips]*26.stfips+_b[34.stfips]*34.stfips+_b[36.stfips]*36.stfips ///
+_b[48.stfips]*48.stfips+_b[55.stfips]*55.stfips+_b[_cons] // Include every coefficient.
gen phat=exp(cc)/(1+exp(cc))
svy, subpop(if stfips== 34 & year==2007 & age_r==1): mean phat // New Jersey, 2007
matrix pro_05_07[34,2] = e(b)

svy, subpop(if (year==2005|year==2007)& (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2007 NJ2007 i.stfips, nolog 
gen cc_time = cc-_b[dummy2007]*dummy2007-_b[NJ2007]*NJ2007
gen phat_time=exp(cc_time)/(1+exp(cc_time))
svy, subpop(if stfips== 34 & year==2007 & age_r==1): mean phat_time 
matrix pro_05_07[34,1] = e(b) // New Jersey, 2005 
 
gen cc_interstate=.
gen phat_interstate=.

foreach i in 4 6 12 13 26 36 48 55 {
svy, subpop(if (year==2005|year==2007)& (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2007 NJ2007 i.stfips, nolog 
replace cc_interstate=cc-_b[34.stfips]*34.stfips-_b[NJ2007]*NJ2007+_b[`i'.stfips]
replace phat_interstate=exp(cc_interstate)/(1+exp(cc_interstate))
svy, subpop(if stfips== 34 & year==2007 & age_r==1): mean phat_interstate
matrix pro_05_07[`i',2] = e(b) 
}

foreach i in 4 6 12 13 26 36 48 55 {
svy, subpop(if (year==2005|year==2007)& (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2007 NJ2007 i.stfips, nolog 
replace cc_interstate=.
replace cc_interstate=cc-_b[dummy2007]*dummy2007-_b[NJ2007]*NJ2007-_b[34.stfips]*34.stfips+_b[`i'.stfips]
replace phat_interstate=exp(cc_interstate)/(1+exp(cc_interstate))
svy, subpop(if stfips== 34 & year==2007 & age_r==1): mean phat_interstate 
matrix pro_05_07[`i',1] = e(b) 
} 

matrix list pro_05_07

matrix predict_05_07= J(9,2,.) 

forvalues i = 1(1)2 {
matrix predict_05_07[1,`i']=pro_05_07[4,`i']
matrix predict_05_07[2,`i']=pro_05_07[6,`i']
matrix predict_05_07[3,`i']=pro_05_07[12,`i']
matrix predict_05_07[4,`i']=pro_05_07[13,`i']
matrix predict_05_07[5,`i']=pro_05_07[26,`i']
matrix predict_05_07[6,`i']=pro_05_07[36,`i']
matrix predict_05_07[7,`i']=pro_05_07[48,`i']
matrix predict_05_07[8,`i']=pro_05_07[55,`i']
matrix predict_05_07[9,`i']=pro_05_07[34,`i']
} // Rearrange Matrix

matrix colnames predict_05_07 = "2005" "2007" 
matrix rownames predict_05_07 = "Arizona" "California" "Florida" "Georgia" "Michigan" "New York" "Texas" "Wisconsin" "New Jersey"

matrix list predict_05_07

matrix TE[1,5]=predict_05_07[9,2]-predict_05_07[9,1] //Time

forvalues i= 1(1)8 {
matrix TE[`i'+1,5]=predict_05_07[9,2]-predict_05_07[`i',2]
} // Interstate

forvalues i= 1(1)8 {
matrix TE[`i'+9,5]=predict_05_07[9,2]-predict_05_07[9,1]-predict_05_07[`i',2]+predict_05_07[`i',1]
} // DID

svy, subpop(if (year==2005|year==2007)& (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2007 NJ2007 i.stfips, nolog 
gen cc_DID = cc-_b[NJ2007]*NJ2007
gen phat_DID=exp(cc_DID)/(1+exp(cc_DID))
svy, subpop(if stfips== 34 & year==2007 & age_r==1): mean phat_DID // Puhani's method
matrix TE[18,5]=predict_05_07[9,2]-e(b)

matrix list TE


/*** Analyzing (freq_smoker) ***/
/** 2005 & 2009 **/
use "\\IASTATE.EDU\ECON\GRADSTUDENTS\OTHERDATA\sjcho\DESKTOP\Manski_Method.dta", clear
matrix pro_05_09 = J(55,2,.) // Make a empty matrix.
svy, subpop(if (year==2005|year==2009) &(age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2009 NJ2009 i.stfips, nolog 

gen cc = _b[3b.age]*3b.age+_b[4.age]*4.age+_b[5.age]*5.age+_b[6.age]*6.age+_b[1b.sex]*1b.sex+_b[2.sex]*2.sex+_b[1b.race4]*1b.race4 ///
+_b[2.race4]*2.race4+_b[3.race4]*3.race4+_b[4.race4]*4.race4+_b[Average_Cost]*Average_Cost ///
+_b[adult_rate]*adult_rate+_b[dummy2009]*dummy2009+_b[NJ2009]*NJ2009+_b[4.stfips]*4.stfips+_b[6.stfips]*6.stfips ///
+_b[12.stfips]*12.stfips+_b[13.stfips]*13.stfips+_b[26.stfips]*26.stfips+_b[34.stfips]*34.stfips+_b[36.stfips]*36.stfips ///
+_b[48.stfips]*48.stfips+_b[55.stfips]*55.stfips+_b[_cons] // Include every coefficient.
gen phat=exp(cc)/(1+exp(cc))
svy, subpop(if stfips== 34 & year==2009 & age_r==1): mean phat // New Jersey, 2009
matrix pro_05_09[34,2] = e(b)

svy, subpop(if (year==2005|year==2009)& (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2009 NJ2009 i.stfips, nolog 
gen cc_time = cc-_b[dummy2009]*dummy2009-_b[NJ2009]*NJ2009
gen phat_time=exp(cc_time)/(1+exp(cc_time))
svy, subpop(if stfips== 34 & year==2009 & age_r==1): mean phat_time 
matrix pro_05_09[34,1] = e(b) // New Jersey, 2005 
 
gen cc_interstate=.
gen phat_interstate=.

foreach i in 4 6 12 13 26 36 48 55 {
svy, subpop(if (year==2005|year==2009)& (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2009 NJ2009 i.stfips, nolog 
replace cc_interstate=cc-_b[34.stfips]*34.stfips-_b[NJ2009]*NJ2009+_b[`i'.stfips]
replace phat_interstate=exp(cc_interstate)/(1+exp(cc_interstate))
svy, subpop(if stfips== 34 & year==2009 & age_r==1): mean phat_interstate
matrix pro_05_09[`i',2] = e(b) 
}

foreach i in 4 6 12 13 26 36 48 55 {
svy, subpop(if (year==2005|year==2009)& (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2009 NJ2009 i.stfips, nolog 
replace cc_interstate=.
replace cc_interstate=cc-_b[dummy2009]*dummy2009-_b[NJ2009]*NJ2009-_b[34.stfips]*34.stfips+_b[`i'.stfips]
replace phat_interstate=exp(cc_interstate)/(1+exp(cc_interstate))
svy, subpop(if stfips== 34 & year==2009 & age_r==1): mean phat_interstate 
matrix pro_05_09[`i',1] = e(b) 
} 

matrix list pro_05_09

matrix predict_05_09= J(9,2,.) 

forvalues i = 1(1)2 {
matrix predict_05_09[1,`i']=pro_05_09[4,`i']
matrix predict_05_09[2,`i']=pro_05_09[6,`i']
matrix predict_05_09[3,`i']=pro_05_09[12,`i']
matrix predict_05_09[4,`i']=pro_05_09[13,`i']
matrix predict_05_09[5,`i']=pro_05_09[26,`i']
matrix predict_05_09[6,`i']=pro_05_09[36,`i']
matrix predict_05_09[7,`i']=pro_05_09[48,`i']
matrix predict_05_09[8,`i']=pro_05_09[55,`i']
matrix predict_05_09[9,`i']=pro_05_09[34,`i']
} // Rearrange Matrix

matrix colnames predict_05_09 = "2005" "2009" 
matrix rownames predict_05_09 = "Arizona" "California" "Florida" "Georgia" "Michigan" "New York" "Texas" "Wisconsin" "New Jersey"

matrix list predict_05_09

matrix TE[1,6]=predict_05_09[9,2]-predict_05_09[9,1] //Time

forvalues i= 1(1)8 {
matrix TE[`i'+1,6]=predict_05_09[9,2]-predict_05_09[`i',2]
} // Interstate

forvalues i= 1(1)8 {
matrix TE[`i'+9,6]=predict_05_09[9,2]-predict_05_09[9,1]-predict_05_09[`i',2]+predict_05_09[`i',1]
} // DID

svy, subpop(if (year==2005|year==2009)& (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2009 NJ2009 i.stfips, nolog 
gen cc_DID = cc-_b[NJ2009]*NJ2009
gen phat_DID=exp(cc_DID)/(1+exp(cc_DID))
svy, subpop(if stfips== 34 & year==2009 & age_r==1): mean phat_DID // Puhani's method
matrix TE[18,6]=predict_05_09[9,2]-e(b)

matrix list TE

/*** Analyzing (freq_smoker) ***/
/** 2005 & 2011 **/
use "\\IASTATE.EDU\ECON\GRADSTUDENTS\OTHERDATA\sjcho\DESKTOP\Manski_Method.dta", clear
matrix pro_05_11 = J(55,2,.) // Make a empty matrix.
svy, subpop(if (year==2005|year==2011) &(age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2011 NJ2011 i.stfips, nolog 

gen cc = _b[3b.age]*3b.age+_b[4.age]*4.age+_b[5.age]*5.age+_b[6.age]*6.age+_b[1b.sex]*1b.sex+_b[2.sex]*2.sex+_b[1b.race4]*1b.race4 ///
+_b[2.race4]*2.race4+_b[3.race4]*3.race4+_b[4.race4]*4.race4+_b[Average_Cost]*Average_Cost ///
+_b[adult_rate]*adult_rate+_b[dummy2011]*dummy2011+_b[NJ2011]*NJ2011+_b[4.stfips]*4.stfips+_b[6.stfips]*6.stfips ///
+_b[12.stfips]*12.stfips+_b[13.stfips]*13.stfips+_b[26.stfips]*26.stfips+_b[34.stfips]*34.stfips+_b[36.stfips]*36.stfips ///
+_b[48.stfips]*48.stfips+_b[55.stfips]*55.stfips+_b[_cons] // Include every coefficient.
gen phat=exp(cc)/(1+exp(cc))
svy, subpop(if stfips== 34 & year==2011 & age_r==1): mean phat // New Jersey, 2011
matrix pro_05_11[34,2] = e(b)

svy, subpop(if (year==2005|year==2011)& (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2011 NJ2011 i.stfips, nolog 
gen cc_time = cc-_b[dummy2011]*dummy2011-_b[NJ2011]*NJ2011
gen phat_time=exp(cc_time)/(1+exp(cc_time))
svy, subpop(if stfips== 34 & year==2011 & age_r==1): mean phat_time 
matrix pro_05_11[34,1] = e(b) // New Jersey, 2005 
 
gen cc_interstate=.
gen phat_interstate=.

foreach i in 4 6 12 13 26 36 48 55 {
svy, subpop(if (year==2005|year==2011)& (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2011 NJ2011 i.stfips, nolog 
replace cc_interstate=cc-_b[34.stfips]*34.stfips-_b[NJ2011]*NJ2011+_b[`i'.stfips]
replace phat_interstate=exp(cc_interstate)/(1+exp(cc_interstate))
svy, subpop(if stfips== 34 & year==2011 & age_r==1): mean phat_interstate
matrix pro_05_11[`i',2] = e(b) 
}

foreach i in 4 6 12 13 26 36 48 55 {
svy, subpop(if (year==2005|year==2011)& (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2011 NJ2011 i.stfips, nolog 
replace cc_interstate=.
replace cc_interstate=cc-_b[dummy2011]*dummy2011-_b[NJ2011]*NJ2011-_b[34.stfips]*34.stfips+_b[`i'.stfips]
replace phat_interstate=exp(cc_interstate)/(1+exp(cc_interstate))
svy, subpop(if stfips== 34 & year==2011 & age_r==1): mean phat_interstate 
matrix pro_05_11[`i',1] = e(b) 
} 

matrix list pro_05_11

matrix predict_05_11= J(9,2,.) 

forvalues i = 1(1)2 {
matrix predict_05_11[1,`i']=pro_05_11[4,`i']
matrix predict_05_11[2,`i']=pro_05_11[6,`i']
matrix predict_05_11[3,`i']=pro_05_11[12,`i']
matrix predict_05_11[4,`i']=pro_05_11[13,`i']
matrix predict_05_11[5,`i']=pro_05_11[26,`i']
matrix predict_05_11[6,`i']=pro_05_11[36,`i']
matrix predict_05_11[7,`i']=pro_05_11[48,`i']
matrix predict_05_11[8,`i']=pro_05_11[55,`i']
matrix predict_05_11[9,`i']=pro_05_11[34,`i']
} // Rearrange Matrix

matrix colnames predict_05_11 = "2005" "2011" 
matrix rownames predict_05_11 = "Arizona" "California" "Florida" "Georgia" "Michigan" "New York" "Texas" "Wisconsin" "New Jersey"

matrix list predict_05_11

matrix TE[1,7]=predict_05_11[9,2]-predict_05_11[9,1] //Time

forvalues i= 1(1)8 {
matrix TE[`i'+1,7]=predict_05_11[9,2]-predict_05_11[`i',2]
} // Interstate

forvalues i= 1(1)8 {
matrix TE[`i'+9,7]=predict_05_11[9,2]-predict_05_11[9,1]-predict_05_11[`i',2]+predict_05_11[`i',1]
} // DID

svy, subpop(if (year==2005|year==2011)& (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2011 NJ2011 i.stfips, nolog 
gen cc_DID = cc-_b[NJ2011]*NJ2011
gen phat_DID=exp(cc_DID)/(1+exp(cc_DID))
svy, subpop(if stfips== 34 & year==2011 & age_r==1): mean phat_DID // Puhani's method
matrix TE[18,7]=predict_05_11[9,2]-e(b)

matrix list TE



/****************** Version 3 (No interaction Term) ******************************/
 
 
/*** Analyzing (freq_smoker) ***/
/** 1997 & 1999 **/
use "\\IASTATE.EDU\ECON\GRADSTUDENTS\OTHERDATA\sjcho\DESKTOP\Manski_Method.dta", clear
matrix pro_97_99 = J(55,2,.) // Make a empty matrix.
svy, subpop(if (year==1997|year==1999) &(age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy1999 i.stfips, nolog 

gen cc = _b[3b.age]*3b.age+_b[4.age]*4.age+_b[5.age]*5.age+_b[6.age]*6.age+_b[1b.sex]*1b.sex+_b[2.sex]*2.sex+_b[1b.race4]*1b.race4 ///
+_b[2.race4]*2.race4+_b[3.race4]*3.race4+_b[4.race4]*4.race4+_b[Average_Cost]*Average_Cost ///
+_b[adult_rate]*adult_rate+_b[dummy1999]*dummy1999+_b[4.stfips]*4.stfips+_b[6.stfips]*6.stfips ///
+_b[12.stfips]*12.stfips+_b[13.stfips]*13.stfips+_b[26.stfips]*26.stfips+_b[34.stfips]*34.stfips+_b[36.stfips]*36.stfips ///
+_b[48.stfips]*48.stfips+_b[55.stfips]*55.stfips+_b[_cons] // Include every coefficient.
gen phat=exp(cc)/(1+exp(cc))
svy, subpop(if stfips== 34 & year==1999 & age_r==1): mean phat // New Jersey, 1999
matrix pro_97_99[34,2] = e(b)

svy, subpop(if (year==1997|year==1999) &(age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy1999 i.stfips, nolog 
gen cc_time = cc-_b[dummy1999]*dummy1999
gen phat_time=exp(cc_time)/(1+exp(cc_time))
svy, subpop(if stfips== 34 & year==1999 & age_r==1): mean phat_time 
matrix pro_97_99[34,1] = e(b) // New Jersey, 1997 
 
gen cc_interstate=.
gen phat_interstate=.

foreach i in 4 6 12 13 26 36 48 55 {
svy, subpop(if (year==1997|year==1999) & (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy1999 i.stfips, nolog 
replace cc_interstate=cc-_b[34.stfips]*34.stfips+_b[`i'.stfips]
replace phat_interstate=exp(cc_interstate)/(1+exp(cc_interstate))
svy, subpop(if stfips== 34 & year==1999 & age_r==1): mean phat_interstate
matrix pro_97_99[`i',2] = e(b) 
}

foreach i in 4 6 12 13 26 36 48 55 {
svy, subpop(if (year==1997|year==1999) &(age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy1999 i.stfips, nolog 
replace cc_interstate=.
replace cc_interstate=cc-_b[dummy1999]*dummy1999-_b[34.stfips]*34.stfips+_b[`i'.stfips]
replace phat_interstate=exp(cc_interstate)/(1+exp(cc_interstate))
svy, subpop(if stfips== 34 & year==1999 & age_r==1): mean phat_interstate 
matrix pro_97_99[`i',1] = e(b) 
} 

matrix list pro_97_99

matrix predict_97_99= J(9,2,.) 

forvalues i = 1(1)2 {
matrix predict_97_99[1,`i']=pro_97_99[4,`i']
matrix predict_97_99[2,`i']=pro_97_99[6,`i']
matrix predict_97_99[3,`i']=pro_97_99[12,`i']
matrix predict_97_99[4,`i']=pro_97_99[13,`i']
matrix predict_97_99[5,`i']=pro_97_99[26,`i']
matrix predict_97_99[6,`i']=pro_97_99[36,`i']
matrix predict_97_99[7,`i']=pro_97_99[48,`i']
matrix predict_97_99[8,`i']=pro_97_99[55,`i']
matrix predict_97_99[9,`i']=pro_97_99[34,`i']
} // Rearrange Matrix

matrix colnames predict_97_99 = "1997" "1999" 
matrix rownames predict_97_99 = "Arizona" "California" "Florida" "Georgia" "Michigan" "New York" "Texas" "Wisconsin" "New Jersey"

matrix list predict_97_99

matrix TE = J(18,7,.)   //Treatment Effect
matrix colnames TE = "1997&1999" "1999&2001" "2001&2003" "2003&2005" "2005&2007" "2005&2009" "2005&2011" 
matrix rownames TE = "Time" "AZ State" "CA State" "FL State" "GA State" "MI State" "NY State"  ///
"TX State" "WI State" "AZ DID" "CA DID" "FL DID" "GA DID" "MI DID" "NY DID" "TX DID" "WI DID" "Puhani"

matrix TE[1,1]=predict_97_99[9,2]-predict_97_99[9,1] //Time

forvalues i= 1(1)8 {
matrix TE[`i'+1,1]=predict_97_99[9,2]-predict_97_99[`i',2]
} // Interstate

forvalues i= 1(1)8 {
matrix TE[`i'+9,1]=predict_97_99[9,2]-predict_97_99[9,1]-predict_97_99[`i',2]+predict_97_99[`i',1]
} // DID

matrix list TE


/*** Analyzing (freq_smoker) ***/
/** 1999 & 2001 **/
use "\\IASTATE.EDU\ECON\GRADSTUDENTS\OTHERDATA\sjcho\DESKTOP\Manski_Method.dta", clear
matrix pro_99_01 = J(55,2,.) // Make a empty matrix.
svy, subpop(if (year==1999|year==2001) & (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2001 i.stfips, nolog 

gen cc = _b[3b.age]*3b.age+_b[4.age]*4.age+_b[5.age]*5.age+_b[6.age]*6.age+_b[1b.sex]*1b.sex+_b[2.sex]*2.sex+_b[1b.race4]*1b.race4 ///
+_b[2.race4]*2.race4+_b[3.race4]*3.race4+_b[4.race4]*4.race4+_b[Average_Cost]*Average_Cost ///
+_b[adult_rate]*adult_rate+_b[dummy2001]*dummy2001+_b[4.stfips]*4.stfips+_b[6.stfips]*6.stfips ///
+_b[12.stfips]*12.stfips+_b[13.stfips]*13.stfips+_b[26.stfips]*26.stfips+_b[34.stfips]*34.stfips+_b[36.stfips]*36.stfips ///
+_b[48.stfips]*48.stfips+_b[55.stfips]*55.stfips+_b[_cons] // Include every coefficient.
gen phat=exp(cc)/(1+exp(cc))
svy, subpop(if stfips== 34 & year==2001 & age_r==1): mean phat // New Jersey, 2001
matrix pro_99_01[34,2] = e(b)

svy, subpop(if (year==1999|year==2001) & (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2001 i.stfips, nolog 
gen cc_time = cc-_b[dummy2001]*dummy2001
gen phat_time=exp(cc_time)/(1+exp(cc_time))
svy, subpop(if stfips== 34 & year==2001 & age_r==1): mean phat_time 
matrix pro_99_01[34,1] = e(b) // New Jersey, 1999 
 
gen cc_interstate=.
gen phat_interstate=.

foreach i in 4 6 12 13 26 36 48 55 {
svy, subpop(if (year==1999|year==2001) & (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2001 i.stfips, nolog 
replace cc_interstate=cc-_b[34.stfips]*34.stfips+_b[`i'.stfips]
replace phat_interstate=exp(cc_interstate)/(1+exp(cc_interstate))
svy, subpop(if stfips== 34 & year==2001 & age_r==1): mean phat_interstate
matrix pro_99_01[`i',2] = e(b) 
}

foreach i in 4 6 12 13 26 36 48 55 {
svy, subpop(if (year==1999|year==2001) & (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2001 i.stfips, nolog 
replace cc_interstate=.
replace cc_interstate=cc-_b[dummy2001]*dummy2001-_b[34.stfips]*34.stfips+_b[`i'.stfips]
replace phat_interstate=exp(cc_interstate)/(1+exp(cc_interstate))
svy, subpop(if stfips== 34 & year==2001 & age_r==1): mean phat_interstate 
matrix pro_99_01[`i',1] = e(b) 
} 

matrix list pro_99_01

matrix predict_99_01= J(9,2,.) 

forvalues i = 1(1)2 {
matrix predict_99_01[1,`i']=pro_99_01[4,`i']
matrix predict_99_01[2,`i']=pro_99_01[6,`i']
matrix predict_99_01[3,`i']=pro_99_01[12,`i']
matrix predict_99_01[4,`i']=pro_99_01[13,`i']
matrix predict_99_01[5,`i']=pro_99_01[26,`i']
matrix predict_99_01[6,`i']=pro_99_01[36,`i']
matrix predict_99_01[7,`i']=pro_99_01[48,`i']
matrix predict_99_01[8,`i']=pro_99_01[55,`i']
matrix predict_99_01[9,`i']=pro_99_01[34,`i']
} // Rearrange Matrix

matrix colnames predict_99_01 = "1999" "2001" 
matrix rownames predict_99_01 = "Arizona" "California" "Florida" "Georgia" "Michigan" "New York" "Texas" "Wisconsin" "New Jersey"

matrix list predict_99_01

matrix TE[1,2]=predict_99_01[9,2]-predict_99_01[9,1] //Time

forvalues i= 1(1)8 {
matrix TE[`i'+1,2]=predict_99_01[9,2]-predict_99_01[`i',2]
} // Interstate

forvalues i= 1(1)8 {
matrix TE[`i'+9,2]=predict_99_01[9,2]-predict_99_01[9,1]-predict_99_01[`i',2]+predict_99_01[`i',1]
} // DID

matrix list TE


/*** Analyzing (freq_smoker) ***/
/** 2001 & 2003 **/
use "\\IASTATE.EDU\ECON\GRADSTUDENTS\OTHERDATA\sjcho\DESKTOP\Manski_Method.dta", clear
matrix pro_01_03 = J(55,2,.) // Make a empty matrix.
svy, subpop(if (year==2001|year==2003) & (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2003 i.stfips, nolog 

gen cc = _b[3b.age]*3b.age+_b[4.age]*4.age+_b[5.age]*5.age+_b[6.age]*6.age+_b[1b.sex]*1b.sex+_b[2.sex]*2.sex+_b[1b.race4]*1b.race4 ///
+_b[2.race4]*2.race4+_b[3.race4]*3.race4+_b[4.race4]*4.race4+_b[Average_Cost]*Average_Cost ///
+_b[adult_rate]*adult_rate+_b[dummy2003]*dummy2003+_b[4.stfips]*4.stfips+_b[6.stfips]*6.stfips ///
+_b[12.stfips]*12.stfips+_b[13.stfips]*13.stfips+_b[26.stfips]*26.stfips+_b[34.stfips]*34.stfips+_b[36.stfips]*36.stfips ///
+_b[48.stfips]*48.stfips+_b[55.stfips]*55.stfips+_b[_cons] // Include every coefficient.
gen phat=exp(cc)/(1+exp(cc))
svy, subpop(if stfips== 34 & year==2003 & age_r==1): mean phat // New Jersey, 2003
matrix pro_01_03[34,2] = e(b)

svy, subpop(if (year==2001|year==2003) & (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2003 i.stfips, nolog 
gen cc_time = cc-_b[dummy2003]*dummy2003
gen phat_time=exp(cc_time)/(1+exp(cc_time))
svy, subpop(if stfips== 34 & year==2003 & age_r==1): mean phat_time 
matrix pro_01_03[34,1] = e(b) // New Jersey, 2001 
 
gen cc_interstate=.
gen phat_interstate=.

foreach i in 4 6 12 13 26 36 48 55 {
svy, subpop(if (year==2001|year==2003) & (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2003 i.stfips, nolog 
replace cc_interstate=cc-_b[34.stfips]*34.stfips+_b[`i'.stfips]
replace phat_interstate=exp(cc_interstate)/(1+exp(cc_interstate))
svy, subpop(if stfips== 34 & year==2003 & age_r==1): mean phat_interstate
matrix pro_01_03[`i',2] = e(b) 
}

foreach i in 4 6 12 13 26 36 48 55 {
svy, subpop(if (year==2001|year==2003) & (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2003 i.stfips, nolog 
replace cc_interstate=.
replace cc_interstate=cc-_b[dummy2003]*dummy2003-_b[34.stfips]*34.stfips+_b[`i'.stfips]
replace phat_interstate=exp(cc_interstate)/(1+exp(cc_interstate))
svy, subpop(if stfips== 34 & year==2003 & age_r==1): mean phat_interstate 
matrix pro_01_03[`i',1] = e(b) 
} 

matrix list pro_01_03

matrix predict_01_03= J(9,2,.) 

forvalues i = 1(1)2 {
matrix predict_01_03[1,`i']=pro_01_03[4,`i']
matrix predict_01_03[2,`i']=pro_01_03[6,`i']
matrix predict_01_03[3,`i']=pro_01_03[12,`i']
matrix predict_01_03[4,`i']=pro_01_03[13,`i']
matrix predict_01_03[5,`i']=pro_01_03[26,`i']
matrix predict_01_03[6,`i']=pro_01_03[36,`i']
matrix predict_01_03[7,`i']=pro_01_03[48,`i']
matrix predict_01_03[8,`i']=pro_01_03[55,`i']
matrix predict_01_03[9,`i']=pro_01_03[34,`i']
} // Rearrange Matrix

matrix colnames predict_01_03 = "2001" "2003" 
matrix rownames predict_01_03 = "Arizona" "California" "Florida" "Georgia" "Michigan" "New York" "Texas" "Wisconsin" "New Jersey"

matrix list predict_01_03

matrix TE[1,3]=predict_01_03[9,2]-predict_01_03[9,1] //Time

forvalues i= 1(1)8 {
matrix TE[`i'+1,3]=predict_01_03[9,2]-predict_01_03[`i',2]
} // Interstate

forvalues i= 1(1)8 {
matrix TE[`i'+9,3]=predict_01_03[9,2]-predict_01_03[9,1]-predict_01_03[`i',2]+predict_01_03[`i',1]
} // DID

matrix list TE

 
/*** Analyzing (freq_smoker) ***/
/** 2003 & 2005 **/
use "\\IASTATE.EDU\ECON\GRADSTUDENTS\OTHERDATA\sjcho\DESKTOP\Manski_Method.dta", clear
matrix pro_03_05 = J(55,2,.) // Make a empty matrix.
svy, subpop(if (year==2003|year==2005) & (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2005 i.stfips, nolog 

gen cc = _b[3b.age]*3b.age+_b[4.age]*4.age+_b[5.age]*5.age+_b[6.age]*6.age+_b[1b.sex]*1b.sex+_b[2.sex]*2.sex+_b[1b.race4]*1b.race4 ///
+_b[2.race4]*2.race4+_b[3.race4]*3.race4+_b[4.race4]*4.race4+_b[Average_Cost]*Average_Cost ///
+_b[adult_rate]*adult_rate+_b[dummy2005]*dummy2005+_b[4.stfips]*4.stfips+_b[6.stfips]*6.stfips ///
+_b[12.stfips]*12.stfips+_b[13.stfips]*13.stfips+_b[26.stfips]*26.stfips+_b[34.stfips]*34.stfips+_b[36.stfips]*36.stfips ///
+_b[48.stfips]*48.stfips+_b[55.stfips]*55.stfips+_b[_cons] // Include every coefficient.
gen phat=exp(cc)/(1+exp(cc))
svy, subpop(if stfips== 34 & year==2005 & age_r==1): mean phat // New Jersey, 2005
matrix pro_03_05[34,2] = e(b)

svy, subpop(if (year==2003|year==2005) & (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2005 i.stfips, nolog 
gen cc_time = cc-_b[dummy2005]*dummy2005
gen phat_time=exp(cc_time)/(1+exp(cc_time))
svy, subpop(if stfips== 34 & year==2005 & age_r==1): mean phat_time 
matrix pro_03_05[34,1] = e(b) // New Jersey, 2003 
 
gen cc_interstate=.
gen phat_interstate=.

foreach i in 4 6 12 13 26 36 48 55 {
svy, subpop(if (year==2003|year==2005) & (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2005 i.stfips, nolog 
replace cc_interstate=cc-_b[34.stfips]*34.stfips+_b[`i'.stfips]
replace phat_interstate=exp(cc_interstate)/(1+exp(cc_interstate))
svy, subpop(if stfips== 34 & year==2005 & age_r==1): mean phat_interstate
matrix pro_03_05[`i',2] = e(b) 
}

foreach i in 4 6 12 13 26 36 48 55 {
svy, subpop(if (year==2003|year==2005) & (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2005 i.stfips, nolog 
replace cc_interstate=.
replace cc_interstate=cc-_b[dummy2005]*dummy2005-_b[34.stfips]*34.stfips+_b[`i'.stfips]
replace phat_interstate=exp(cc_interstate)/(1+exp(cc_interstate))
svy, subpop(if stfips== 34 & year==2005 & age_r==1): mean phat_interstate 
matrix pro_03_05[`i',1] = e(b) 
} 

matrix list pro_03_05

matrix predict_03_05= J(9,2,.) 

forvalues i = 1(1)2 {
matrix predict_03_05[1,`i']=pro_03_05[4,`i']
matrix predict_03_05[2,`i']=pro_03_05[6,`i']
matrix predict_03_05[3,`i']=pro_03_05[12,`i']
matrix predict_03_05[4,`i']=pro_03_05[13,`i']
matrix predict_03_05[5,`i']=pro_03_05[26,`i']
matrix predict_03_05[6,`i']=pro_03_05[36,`i']
matrix predict_03_05[7,`i']=pro_03_05[48,`i']
matrix predict_03_05[8,`i']=pro_03_05[55,`i']
matrix predict_03_05[9,`i']=pro_03_05[34,`i']
} // Rearrange Matrix

matrix colnames predict_03_05 = "2003" "2005" 
matrix rownames predict_03_05 = "Arizona" "California" "Florida" "Georgia" "Michigan" "New York" "Texas" "Wisconsin" "New Jersey"

matrix list predict_03_05

matrix TE[1,4]=predict_03_05[9,2]-predict_03_05[9,1] //Time

forvalues i= 1(1)8 {
matrix TE[`i'+1,4]=predict_03_05[9,2]-predict_03_05[`i',2]
} // Interstate

forvalues i= 1(1)8 {
matrix TE[`i'+9,4]=predict_03_05[9,2]-predict_03_05[9,1]-predict_03_05[`i',2]+predict_03_05[`i',1]
} // DID

matrix list TE

 /*** Analyzing (freq_smoker) ***/
/** 2005 & 2007 **/
use "\\IASTATE.EDU\ECON\GRADSTUDENTS\OTHERDATA\sjcho\DESKTOP\Manski_Method.dta", clear
matrix pro_05_07 = J(55,2,.) // Make a empty matrix.
svy, subpop(if (year==2005|year==2007) & (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2007 i.stfips, nolog 

gen cc = _b[3b.age]*3b.age+_b[4.age]*4.age+_b[5.age]*5.age+_b[6.age]*6.age+_b[1b.sex]*1b.sex+_b[2.sex]*2.sex+_b[1b.race4]*1b.race4 ///
+_b[2.race4]*2.race4+_b[3.race4]*3.race4+_b[4.race4]*4.race4+_b[Average_Cost]*Average_Cost ///
+_b[adult_rate]*adult_rate+_b[dummy2007]*dummy2007+_b[4.stfips]*4.stfips+_b[6.stfips]*6.stfips ///
+_b[12.stfips]*12.stfips+_b[13.stfips]*13.stfips+_b[26.stfips]*26.stfips+_b[34.stfips]*34.stfips+_b[36.stfips]*36.stfips ///
+_b[48.stfips]*48.stfips+_b[55.stfips]*55.stfips+_b[_cons] // Include every coefficient.
gen phat=exp(cc)/(1+exp(cc))
svy, subpop(if stfips== 34 & year==2007 & age_r==1): mean phat // New Jersey, 2007
matrix pro_05_07[34,2] = e(b)

svy, subpop(if (year==2005|year==2007) & (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2007 i.stfips, nolog 
gen cc_time = cc-_b[dummy2007]*dummy2007
gen phat_time=exp(cc_time)/(1+exp(cc_time))
svy, subpop(if stfips== 34 & year==2007 & age_r==1): mean phat_time 
matrix pro_05_07[34,1] = e(b) // New Jersey, 2005 
 
gen cc_interstate=.
gen phat_interstate=.

foreach i in 4 6 12 13 26 36 48 55 {
svy, subpop(if (year==2005|year==2007) & (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2007 i.stfips, nolog 
replace cc_interstate=cc-_b[34.stfips]*34.stfips+_b[`i'.stfips]
replace phat_interstate=exp(cc_interstate)/(1+exp(cc_interstate))
svy, subpop(if stfips== 34 & year==2007 & age_r==1): mean phat_interstate
matrix pro_05_07[`i',2] = e(b) 
}

foreach i in 4 6 12 13 26 36 48 55 {
svy, subpop(if (year==2005|year==2007) & (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2007 i.stfips, nolog 
replace cc_interstate=.
replace cc_interstate=cc-_b[dummy2007]*dummy2007-_b[34.stfips]*34.stfips+_b[`i'.stfips]
replace phat_interstate=exp(cc_interstate)/(1+exp(cc_interstate))
svy, subpop(if stfips== 34 & year==2007 & age_r==1): mean phat_interstate 
matrix pro_05_07[`i',1] = e(b) 
} 

matrix list pro_05_07

matrix predict_05_07= J(9,2,.) 

forvalues i = 1(1)2 {
matrix predict_05_07[1,`i']=pro_05_07[4,`i']
matrix predict_05_07[2,`i']=pro_05_07[6,`i']
matrix predict_05_07[3,`i']=pro_05_07[12,`i']
matrix predict_05_07[4,`i']=pro_05_07[13,`i']
matrix predict_05_07[5,`i']=pro_05_07[26,`i']
matrix predict_05_07[6,`i']=pro_05_07[36,`i']
matrix predict_05_07[7,`i']=pro_05_07[48,`i']
matrix predict_05_07[8,`i']=pro_05_07[55,`i']
matrix predict_05_07[9,`i']=pro_05_07[34,`i']
} // Rearrange Matrix

matrix colnames predict_05_07 = "2005" "2007" 
matrix rownames predict_05_07 = "Arizona" "California" "Florida" "Georgia" "Michigan" "New York" "Texas" "Wisconsin" "New Jersey"

matrix list predict_05_07

matrix TE[1,5]=predict_05_07[9,2]-predict_05_07[9,1] //Time

forvalues i= 1(1)8 {
matrix TE[`i'+1,5]=predict_05_07[9,2]-predict_05_07[`i',2]
} // Interstate

forvalues i= 1(1)8 {
matrix TE[`i'+9,5]=predict_05_07[9,2]-predict_05_07[9,1]-predict_05_07[`i',2]+predict_05_07[`i',1]
} // DID

matrix list TE


 /*** Analyzing (freq_smoker) ***/
/** 2005 & 2009 **/
use "\\IASTATE.EDU\ECON\GRADSTUDENTS\OTHERDATA\sjcho\DESKTOP\Manski_Method.dta", clear
matrix pro_05_09 = J(55,2,.) // Make a empty matrix.
svy, subpop(if (year==2005|year==2009) & (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2009 i.stfips, nolog 

gen cc = _b[3b.age]*3b.age+_b[4.age]*4.age+_b[5.age]*5.age+_b[6.age]*6.age+_b[1b.sex]*1b.sex+_b[2.sex]*2.sex+_b[1b.race4]*1b.race4 ///
+_b[2.race4]*2.race4+_b[3.race4]*3.race4+_b[4.race4]*4.race4+_b[Average_Cost]*Average_Cost ///
+_b[adult_rate]*adult_rate+_b[dummy2009]*dummy2009+_b[4.stfips]*4.stfips+_b[6.stfips]*6.stfips ///
+_b[12.stfips]*12.stfips+_b[13.stfips]*13.stfips+_b[26.stfips]*26.stfips+_b[34.stfips]*34.stfips+_b[36.stfips]*36.stfips ///
+_b[48.stfips]*48.stfips+_b[55.stfips]*55.stfips+_b[_cons] // Include every coefficient.
gen phat=exp(cc)/(1+exp(cc))
svy, subpop(if stfips== 34 & year==2009 & age_r==1): mean phat // New Jersey, 2009
matrix pro_05_09[34,2] = e(b)

svy, subpop(if (year==2005|year==2009) & (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2009 i.stfips, nolog 
gen cc_time = cc-_b[dummy2009]*dummy2009
gen phat_time=exp(cc_time)/(1+exp(cc_time))
svy, subpop(if stfips== 34 & year==2009 & age_r==1): mean phat_time 
matrix pro_05_09[34,1] = e(b) // New Jersey, 2005 
 
gen cc_interstate=.
gen phat_interstate=.

foreach i in 4 6 12 13 26 36 48 55 {
svy, subpop(if (year==2005|year==2009) & (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2009 i.stfips, nolog 
replace cc_interstate=cc-_b[34.stfips]*34.stfips+_b[`i'.stfips]
replace phat_interstate=exp(cc_interstate)/(1+exp(cc_interstate))
svy, subpop(if stfips== 34 & year==2009 & age_r==1): mean phat_interstate
matrix pro_05_09[`i',2] = e(b) 
}

foreach i in 4 6 12 13 26 36 48 55 {
svy, subpop(if (year==2005|year==2009) & (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2009 i.stfips, nolog 
replace cc_interstate=.
replace cc_interstate=cc-_b[dummy2009]*dummy2009-_b[34.stfips]*34.stfips+_b[`i'.stfips]
replace phat_interstate=exp(cc_interstate)/(1+exp(cc_interstate))
svy, subpop(if stfips== 34 & year==2009 & age_r==1): mean phat_interstate 
matrix pro_05_09[`i',1] = e(b) 
} 

matrix list pro_05_09

matrix predict_05_09= J(9,2,.) 

forvalues i = 1(1)2 {
matrix predict_05_09[1,`i']=pro_05_09[4,`i']
matrix predict_05_09[2,`i']=pro_05_09[6,`i']
matrix predict_05_09[3,`i']=pro_05_09[12,`i']
matrix predict_05_09[4,`i']=pro_05_09[13,`i']
matrix predict_05_09[5,`i']=pro_05_09[26,`i']
matrix predict_05_09[6,`i']=pro_05_09[36,`i']
matrix predict_05_09[7,`i']=pro_05_09[48,`i']
matrix predict_05_09[8,`i']=pro_05_09[55,`i']
matrix predict_05_09[9,`i']=pro_05_09[34,`i']
} // Rearrange Matrix

matrix colnames predict_05_09 = "2005" "2009" 
matrix rownames predict_05_09 = "Arizona" "California" "Florida" "Georgia" "Michigan" "New York" "Texas" "Wisconsin" "New Jersey"

matrix list predict_05_09

matrix TE[1,6]=predict_05_09[9,2]-predict_05_09[9,1] //Time

forvalues i= 1(1)8 {
matrix TE[`i'+1,6]=predict_05_09[9,2]-predict_05_09[`i',2]
} // Interstate

forvalues i= 1(1)8 {
matrix TE[`i'+9,6]=predict_05_09[9,2]-predict_05_09[9,1]-predict_05_09[`i',2]+predict_05_09[`i',1]
} // DID

matrix list TE


 /*** Analyzing (freq_smoker) ***/
/** 2005 & 2011 **/
use "\\IASTATE.EDU\ECON\GRADSTUDENTS\OTHERDATA\sjcho\DESKTOP\Manski_Method.dta", clear
matrix pro_05_11 = J(55,2,.) // Make a empty matrix.
svy, subpop(if (year==2005|year==2011) & (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2011 i.stfips, nolog 

gen cc = _b[3b.age]*3b.age+_b[4.age]*4.age+_b[5.age]*5.age+_b[6.age]*6.age+_b[1b.sex]*1b.sex+_b[2.sex]*2.sex+_b[1b.race4]*1b.race4 ///
+_b[2.race4]*2.race4+_b[3.race4]*3.race4+_b[4.race4]*4.race4+_b[Average_Cost]*Average_Cost ///
+_b[adult_rate]*adult_rate+_b[dummy2011]*dummy2011+_b[4.stfips]*4.stfips+_b[6.stfips]*6.stfips ///
+_b[12.stfips]*12.stfips+_b[13.stfips]*13.stfips+_b[26.stfips]*26.stfips+_b[34.stfips]*34.stfips+_b[36.stfips]*36.stfips ///
+_b[48.stfips]*48.stfips+_b[55.stfips]*55.stfips+_b[_cons] // Include every coefficient.
gen phat=exp(cc)/(1+exp(cc))
svy, subpop(if stfips== 34 & year==2011 & age_r==1): mean phat // New Jersey, 2011
matrix pro_05_11[34,2] = e(b)

svy, subpop(if (year==2005|year==2011) & (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2011 i.stfips, nolog 
gen cc_time = cc-_b[dummy2011]*dummy2011
gen phat_time=exp(cc_time)/(1+exp(cc_time))
svy, subpop(if stfips== 34 & year==2011 & age_r==1): mean phat_time 
matrix pro_05_11[34,1] = e(b) // New Jersey, 2005 
 
gen cc_interstate=.
gen phat_interstate=.

foreach i in 4 6 12 13 26 36 48 55 {
svy, subpop(if (year==2005|year==2011) & (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2011 i.stfips, nolog 
replace cc_interstate=cc-_b[34.stfips]*34.stfips+_b[`i'.stfips]
replace phat_interstate=exp(cc_interstate)/(1+exp(cc_interstate))
svy, subpop(if stfips== 34 & year==2011 & age_r==1): mean phat_interstate
matrix pro_05_11[`i',2] = e(b) 
}

foreach i in 4 6 12 13 26 36 48 55 {
svy, subpop(if (year==2005|year==2011) & (age==3|age==4|age==5|age==6)): logit freq_smoker i.age i.sex i.race4 Average_Cost adult_rate dummy2011 i.stfips, nolog 
replace cc_interstate=.
replace cc_interstate=cc-_b[dummy2011]*dummy2011-_b[34.stfips]*34.stfips+_b[`i'.stfips]
replace phat_interstate=exp(cc_interstate)/(1+exp(cc_interstate))
svy, subpop(if stfips== 34 & year==2011 & age_r==1): mean phat_interstate 
matrix pro_05_11[`i',1] = e(b) 
} 

matrix list pro_05_11

matrix predict_05_11= J(9,2,.) 

forvalues i = 1(1)2 {
matrix predict_05_11[1,`i']=pro_05_11[4,`i']
matrix predict_05_11[2,`i']=pro_05_11[6,`i']
matrix predict_05_11[3,`i']=pro_05_11[12,`i']
matrix predict_05_11[4,`i']=pro_05_11[13,`i']
matrix predict_05_11[5,`i']=pro_05_11[26,`i']
matrix predict_05_11[6,`i']=pro_05_11[36,`i']
matrix predict_05_11[7,`i']=pro_05_11[48,`i']
matrix predict_05_11[8,`i']=pro_05_11[55,`i']
matrix predict_05_11[9,`i']=pro_05_11[34,`i']
} // Rearrange Matrix

matrix colnames predict_05_11 = "2005" "2011" 
matrix rownames predict_05_11 = "Arizona" "California" "Florida" "Georgia" "Michigan" "New York" "Texas" "Wisconsin" "New Jersey"

matrix list predict_05_11

matrix TE[1,7]=predict_05_11[9,2]-predict_05_11[9,1] //Time

forvalues i= 1(1)8 {
matrix TE[`i'+1,7]=predict_05_11[9,2]-predict_05_11[`i',2]
} // Interstate

forvalues i= 1(1)8 {
matrix TE[`i'+9,7]=predict_05_11[9,2]-predict_05_11[9,1]-predict_05_11[`i',2]+predict_05_11[`i',1]
} // DID

matrix list TE







