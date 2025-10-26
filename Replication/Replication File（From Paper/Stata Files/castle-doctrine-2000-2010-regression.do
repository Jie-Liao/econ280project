******** regression code for "Does Strengthening Self-Defense Law Deter Crime or Escalate Violence?  Evidence from Castle Doctrine" ********
*** by Cheng Cheng and Mark Hoekstra
*** Department of Economics, Texas A&M University

* note: install "estout" and "outsum" first 
cd "C:\Users\faculty\Documents\Cheng - Castle Doctrine\Data for Replication"
use castle-doctrine-2000-2010.dta, clear

log using results_castle_doctrine.txt, text replace
set more off
xtset sid year

** generate region-by-year fixed effects
forvalues j=2000/2010{
	gen year`j'=(year==`j')
	gen r`j'1=year`j'*northeast
	gen r`j'2=year`j'*midwest
	gen r`j'3=year`j'*south
	gen r`j'4=year`j'*west
	drop year`j'
}		 

** generate state-specific linear trends
forvalues j=1/51{
	sort state year
	by state: gen trend_`j'=_n
	replace trend_`j'=0 if `j'~=sid
}
	
* generate log data		
gen l_murder=log(murder)
gen l_homicide=log(homicide)
gen l_homicide_c=log(homicide_c)
gen l_robbery=log(robbery)
gen l_assault=log(assault)
gen l_burglary=log(burglary)
gen l_larceny=log(larceny)
gen l_motor=log(motor)
gen l_robbery_gun=log(robbery_gun)
gen l_jhcitizen=log(jhcitizen)
gen l_jhpolice=log(jhpolice)
gen l_hc_felonywsus=log(hc_felonywsus)
gen l_robbery_gun_r=log(robbery_gun_r)

gen l_pop=log(population)
gen l_police=log(police)
gen l_income=log(income)
gen l_prisoner=log(prisoner)
gen l_lagprisoner=log(lagprisoner)
gen l_exp_subsidy=log(exp_subsidy)
gen l_exp_pubwelfare=log(exp_pubwelfare)


* define global macro
global crime1 jhcitizen_c jhpolice_c murder homicide  robbery assault burglary larceny motor robbery_gun_r 
global demo blackm_15_24 whitem_15_24 blackm_25_44 whitem_25_44 //demographics
global lintrend trend_1-trend_51 //state linear trend
global region r20001-r20104  //region-quarter fixed effects
global exocrime l_larceny l_motor // exogenous crime rates
global spending l_exp_subsidy l_exp_pubwelfare
global xvar l_police unemployrt poverty l_income l_prisoner l_lagprisoner $demo $spending

** generate state population weight
bys sid: egen popwt=mean(population) 

* treatment variable
global law cdl  
global prelaw pre2_cdl //leading indicator

** drop states **
drop if state=="District of Columbia"  //missing data

*************  Descriptive Statistics  *************
outsum $crime1 police unemployrt poverty income prisoner lagprisoner $demo exp_subsidy exp_pubwelfare using stats, replace
outsum $crime1 police unemployrt poverty income prisoner lagprisoner $demo exp_subsidy exp_pubwelfare using stats-w [aweight=popwt], replace
* If outsum isn't functioning properly on your computer, you might want to modify the "outsum" .do file as suggested by Stata by replacing "_all" with "*":
* First, type "which outsum" in the command window.
* Second, copy the directory in the output of that command and replace "copy the directory here" in the following command
* doedit "copy_the_directory_here"


					**********************************
					*                                *
					*            Regression          *
					*                                *
					**********************************

** Table 3: Falsification Tests **

* Larceny
xi: xtreg l_larceny $law i.year [aweight=popwt],fe vce(cluster sid)
eststo l1
xi: xtreg l_larceny $law i.year $region [aweight=popwt], fe vce(cluster sid)
eststo l2
xi: xtreg l_larceny $law i.year $region $xvar [aweight=popwt],fe vce(cluster sid)
eststo l3
xi: xtreg l_larceny $law i.year $region $xvar $prelaw [aweight=popwt], fe vce(cluster sid)
eststo l4
xi: xtreg l_larceny $law i.year $region $xvar l_motor [aweight=popwt], fe vce(cluster sid)
eststo l5
xi: xtreg l_larceny $law i.year $region $xvar $lintrend [aweight=popwt], fe vce(cluster sid)
eststo l6

xi: xtreg l_larceny $law i.year,fe vce(cluster sid)
eststo l7
xi: xtreg l_larceny $law i.year $region, fe vce(cluster sid)
eststo l8
xi: xtreg l_larceny $law i.year $region $xvar,fe vce(cluster sid)
eststo l9
xi: xtreg l_larceny $law i.year $region $xvar $prelaw, fe vce(cluster sid)
eststo l10
xi: xtreg l_larceny $law i.year $region $xvar l_motor, fe vce(cluster sid)
eststo l11
xi: xtreg l_larceny $law i.year $region $xvar $lintrend, fe vce(cluster sid)
eststo l12


esttab l1 l2 l3 l4 l5 l6 l7 l8 l9 l10 l11 l12 using Table3_larceny.csv, se star(* 0.1 ** 0.05 *** 0.01) scalars(F r2) replace

* Motor
xi: xtreg l_motor $law i.year [aweight=popwt],fe vce(cluster sid)
eststo mo1
xi: xtreg l_motor $law i.year $region [aweight=popwt], fe vce(cluster sid)
eststo mo2
xi: xtreg l_motor $law i.year $region $xvar [aweight=popwt],fe vce(cluster sid)
eststo mo3
xi: xtreg l_motor $law i.year $region $xvar $prelaw [aweight=popwt], fe vce(cluster sid)
eststo mo4
xi: xtreg l_motor $law i.year $region $xvar l_larceny [aweight=popwt], fe vce(cluster sid)
eststo mo5
xi: xtreg l_motor $law i.year $region $xvar $lintrend [aweight=popwt], fe vce(cluster sid)
eststo mo6

xi: xtreg l_motor $law i.year,fe vce(cluster sid)
eststo mo7
xi: xtreg l_motor $law i.year $region, fe vce(cluster sid)
eststo mo8
xi: xtreg l_motor $law i.year $region $xvar,fe vce(cluster sid)
eststo mo9
xi: xtreg l_motor $law i.year $region $xvar $prelaw, fe vce(cluster sid)
eststo mo10
xi: xtreg l_motor $law i.year $region $xvar l_larceny, fe vce(cluster sid)
eststo mo11
xi: xtreg l_motor $law i.year $region $xvar $lintrend, fe vce(cluster sid)
eststo mo12


esttab mo1 mo2 mo3 mo4 mo5 mo6 mo7 mo8 mo9 mo10 mo11 mo12 using Table3_motor.csv, se star(* 0.1 ** 0.05 *** 0.01) scalars(F r2) replace

** Table 4: Deterrence Effects **

* Burglary
xi: xtreg l_burglary $law i.year [aweight=popwt],fe vce(cluster sid)
eststo b1
xi: xtreg l_burglary $law i.year $region [aweight=popwt], fe vce(cluster sid)
eststo b2
xi: xtreg l_burglary $law i.year $region $xvar [aweight=popwt],fe vce(cluster sid)
eststo b3
xi: xtreg l_burglary $law i.year $region $xvar $prelaw [aweight=popwt], fe vce(cluster sid)
eststo b4
xi: xtreg l_burglary $law i.year $region $xvar $exocrime [aweight=popwt], fe vce(cluster sid)
eststo b5
xi: xtreg l_burglary $law i.year $region $xvar $lintrend [aweight=popwt], fe vce(cluster sid)
eststo b6

xi: xtreg l_burglary $law i.year,fe vce(cluster sid)
eststo b7
xi: xtreg l_burglary $law i.year $region, fe vce(cluster sid)
eststo b8
xi: xtreg l_burglary $law i.year $region $xvar,fe vce(cluster sid)
eststo b9
xi: xtreg l_burglary $law i.year $region $xvar $prelaw, fe vce(cluster sid)
eststo b10
xi: xtreg l_burglary $law i.year $region $xvar $exocrime, fe vce(cluster sid)
eststo b11
xi: xtreg l_burglary $law i.year $region $xvar $lintrend, fe vce(cluster sid)
eststo b12

esttab b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 using Table4_burglary.csv, se star(* 0.1 ** 0.05 *** 0.01) scalars(F r2) replace


* Robbery
xi: xtreg l_robbery $law i.year [aweight=popwt],fe vce(cluster sid)
eststo ro1
xi: xtreg l_robbery $law i.year $region [aweight=popwt], fe vce(cluster sid)
eststo ro2
xi: xtreg l_robbery $law i.year $region $xvar [aweight=popwt],fe vce(cluster sid)
eststo ro3
xi: xtreg l_robbery $law i.year $region $xvar $prelaw [aweight=popwt], fe vce(cluster sid)
eststo ro4
xi: xtreg l_robbery $law i.year $region $xvar $exocrime [aweight=popwt], fe vce(cluster sid)
eststo ro5
xi: xtreg l_robbery $law i.year $region $xvar $lintrend [aweight=popwt], fe vce(cluster sid)
eststo ro6

xi: xtreg l_robbery $law i.year,fe vce(cluster sid)
eststo ro7
xi: xtreg l_robbery $law i.year $region, fe vce(cluster sid)
eststo ro8
xi: xtreg l_robbery $law i.year $region $xvar,fe vce(cluster sid)
eststo ro9
xi: xtreg l_robbery $law i.year $region $xvar $prelaw, fe vce(cluster sid)
eststo ro10
xi: xtreg l_robbery $law i.year $region $xvar $exocrime, fe vce(cluster sid)
eststo ro11
xi: xtreg l_robbery $law i.year $region $xvar $lintrend, fe vce(cluster sid)
eststo ro12

esttab ro1 ro2 ro3 ro4 ro5 ro6 ro7 ro8 ro9 ro10 ro11 ro12 using Table4_robbery.csv, se star(* 0.1 ** 0.05 *** 0.01) scalars(F r2) replace

* Aggravated Assault
xi: xtreg l_assault $law i.year [aweight=popwt],fe vce(cluster sid)
eststo a1
xi: xtreg l_assault $law i.year $region [aweight=popwt], fe vce(cluster sid)
eststo a2
xi: xtreg l_assault $law i.year $region $xvar [aweight=popwt],fe vce(cluster sid)
eststo a3
xi: xtreg l_assault $law i.year $region $xvar $prelaw [aweight=popwt], fe vce(cluster sid)
eststo a4
xi: xtreg l_assault $law i.year $region $xvar $exocrime [aweight=popwt], fe vce(cluster sid)
eststo a5
xi: xtreg l_assault $law i.year $region $xvar $lintrend [aweight=popwt], fe vce(cluster sid)
eststo a6

xi: xtreg l_assault $law i.year,fe vce(cluster sid)
eststo a7
xi: xtreg l_assault $law i.year $region, fe vce(cluster sid)
eststo a8
xi: xtreg l_assault $law i.year $region $xvar,fe vce(cluster sid)
eststo a9
xi: xtreg l_assault $law i.year $region $xvar $prelaw, fe vce(cluster sid)
eststo a10
xi: xtreg l_assault $law i.year $region $xvar $exocrime, fe vce(cluster sid)
eststo a11
xi: xtreg l_assault $law i.year $region $xvar $lintrend, fe vce(cluster sid)
eststo a12

esttab a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 using Table4_assault.csv, se star(* 0.1 ** 0.05 *** 0.01) scalars(F r2) replace


** Table 5: Homicide **

* homicide (murder + non-negligent manslaughter)
xi: xtreg l_homicide $law i.year [aweight=popwt],fe vce(cluster sid)
eststo h1
xi: xtreg l_homicide $law i.year $region [aweight=popwt], fe vce(cluster sid)
eststo h2
xi: xtreg l_homicide $law i.year $region $xvar [aweight=popwt],fe vce(cluster sid)
eststo h3
xi: xtreg l_homicide $law i.year $region $xvar $prelaw [aweight=popwt], fe vce(cluster sid)
eststo h4
xi: xtreg l_homicide $law i.year $region $xvar $exocrime [aweight=popwt], fe vce(cluster sid)
eststo h5
xi: xtreg l_homicide $law i.year $region $xvar $lintrend [aweight=popwt], fe vce(cluster sid)
eststo h6

xi: xtreg l_homicide $law i.year,fe vce(cluster sid)
eststo h7
xi: xtreg l_homicide $law i.year $region, fe vce(cluster sid)
eststo h8
xi: xtreg l_homicide $law i.year $region $xvar,fe vce(cluster sid)
eststo h9
xi: xtreg l_homicide $law i.year $region $xvar $prelaw, fe vce(cluster sid)
eststo h10
xi: xtreg l_homicide $law i.year $region $xvar $exocrime, fe vce(cluster sid)
eststo h11
xi: xtreg l_homicide $law i.year $region $xvar $lintrend, fe vce(cluster sid)
eststo h12

esttab h1 h2 h3 h4 h5 h6 h7 h8 h9 h10 h11 h12 using Table5_homicide_ols.csv, se star(* 0.1 ** 0.05 *** 0.01) scalars(F r2) replace


xi:nbreg homicide_c $law i.year i.sid,exposure(population) vce(cluster sid)
eststo h13
xi:nbreg homicide_c $law i.year i.sid $region, exposure(population) vce(cluster sid)
eststo h14
xi:nbreg homicide_c $law i.year i.sid $region $xvar,exposure(population) vce(cluster sid)
eststo h15
xi:nbreg homicide_c $law i.year i.sid $region $xvar $prelaw, exposure(population) vce(cluster sid)
eststo h16
xi:nbreg homicide_c $law i.year i.sid $region $xvar $exocrime, exposure(population) vce(cluster sid)
eststo h17
xi:nbreg homicide_c $law i.year i.sid $region $xvar $lintrend, exposure(population) vce(cluster sid)
eststo h18


esttab h13 h14 h15 h16 h17 h18  using Table5_homicide_nb.csv, se star(* 0.1 ** 0.05 *** 0.01) scalars(F r2) replace


** Table 6 **

* Murder
xi: xtreg l_murder $law i.year [aweight=popwt],fe vce(cluster sid)
eststo m1
xi: xtreg l_murder $law i.year $region [aweight=popwt], fe vce(cluster sid)
eststo m2
xi: xtreg l_murder $law i.year $region $xvar [aweight=popwt],fe vce(cluster sid)
eststo m3
xi: xtreg l_murder $law i.year $region $xvar $prelaw [aweight=popwt], fe vce(cluster sid)
eststo m4
xi: xtreg l_murder $law i.year $region $xvar $exocrime [aweight=popwt], fe vce(cluster sid)
eststo m5
xi: xtreg l_murder $law i.year $region $xvar $lintrend [aweight=popwt], fe vce(cluster sid)
eststo m6

esttab m1 m2 m3 m4 m5 m6 using Table6_murder.csv, se star(* 0.1 ** 0.05 *** 0.01) scalars(F r2) replace

* felony/suspected felony type homicide
xi: xtreg l_hc_felonywsus $law i.year [aweight=popwt],fe vce(cluster sid)
eststo hcfs1
xi: xtreg l_hc_felonywsus $law i.year $region [aweight=popwt], fe vce(cluster sid)
eststo hcfs2
xi: xtreg l_hc_felonywsus $law i.year $region $xvar [aweight=popwt],fe vce(cluster sid)
eststo hcfs3
xi: xtreg l_hc_felonywsus $law i.year $region $xvar $prelaw [aweight=popwt], fe vce(cluster sid)
eststo hcfs4
xi: xtreg l_hc_felonywsus $law i.year $region $xvar $exocrime [aweight=popwt], fe vce(cluster sid)
eststo hcfs5
xi: xtreg l_hc_felonywsus $law i.year $region $xvar $lintrend [aweight=popwt], fe vce(cluster sid)
eststo hcfs6

esttab hcfs1 hcfs2 hcfs3 hcfs4 hcfs5 hcfs6 using Table6_homicide_felony.csv, se star(* 0.1 ** 0.05 *** 0.01) scalars(F r2) replace

* Robbery with a gun (ratio)

xi: xtreg robbery_gun_r $law i.year [aweight=popwt],fe vce(cluster sid)
eststo rog_r1
xi: xtreg robbery_gun_r $law i.year $region [aweight=popwt], fe vce(cluster sid)
eststo rog_r2
xi: xtreg robbery_gun_r $law i.year $region $xvar [aweight=popwt],fe vce(cluster sid)
eststo rog_r3
xi: xtreg robbery_gun_r $law i.year $region $xvar $prelaw [aweight=popwt], fe vce(cluster sid)
eststo rog_r4
xi: xtreg robbery_gun_r $law i.year $region $xvar $exocrime [aweight=popwt], fe vce(cluster sid)
eststo rog_r5
xi: xtreg robbery_gun_r $law i.year $region $xvar $lintrend [aweight=popwt], fe vce(cluster sid)
eststo rog_r6

esttab rog_r1 rog_r2 rog_r3 rog_r4 rog_r5 rog_r6 using Table6_robbery_gun_r.csv, se star(* 0.1 ** 0.05 *** 0.01) scalars(F r2) replace


* Justifiable Homicide by private citizen (ols count)
xi: xtreg jhcitizen_c $law i.year,fe vce(cluster sid)
eststo jhcct1
xi: xtreg jhcitizen_c $law i.year $region, fe vce(cluster sid)
eststo jhcct2
xi: xtreg jhcitizen_c $law i.year $region $xvar l_pop, fe vce(cluster sid)
eststo jhcct3
xi: xtreg jhcitizen_c $law i.year $region $xvar l_pop $prelaw, fe vce(cluster sid)
eststo jhcct4
xi: xtreg jhcitizen_c $law i.year $region $xvar l_pop $exocrime, fe vce(cluster sid)
eststo jhcct5
xi: xtreg jhcitizen_c $law i.year $region $xvar l_pop $lintrend, fe vce(cluster sid)
eststo jhcct6

esttab jhcct1 jhcct2 jhcct3 jhcct4 jhcct5 jhcct6  using Table6_jhcitizen_OLS_count.csv, se star(* 0.1 ** 0.05 *** 0.01) scalars(F r2) replace


* Justifiable Homicide by private citizen (negative binomial)
xi:nbreg jhcitizen_c $law i.year i.sid,exposure(population) vce(cluster sid)
eststo jhc1
xi:nbreg jhcitizen_c $law i.year i.sid $region, exposure(population) vce(cluster sid)
eststo jhc2
xi:nbreg jhcitizen_c $law i.year i.sid $region $xvar,exposure(population) vce(cluster sid)
eststo jhc3
xi:nbreg jhcitizen_c $law i.year i.sid $region $xvar $prelaw, exposure(population) vce(cluster sid)
eststo jhc4
xi:nbreg jhcitizen_c $law i.year i.sid $region $xvar $exocrime, exposure(population) vce(cluster sid)
eststo jhc5
/*
xi:nbreg jhcitizen_c $law i.year i.sid $region $xvar $lintrend, exposure(population) vce(cluster sid)
eststo jhc6
*/
esttab jhc1 jhc2 jhc3 jhc4 jhc5 using Table6_jhcitizen_nb.csv, se star(* 0.1 ** 0.05 *** 0.01) scalars(F r2) replace

** Table A1: Differential Effects **

* caselaw
gen strongcdl=cdl*(1-caselaw)
gen weakcdl=cdl-strongcdl

global law strongcdl weakcdl 

xi: xtreg l_burglary $law  $xvar i.year $region [aweight=popwt],fe vce(cluster sid)
eststo s1
xi: xtreg l_robbery $law  $xvar i.year $region [aweight=popwt],fe vce(cluster sid)
eststo s2
xi: xtreg l_assault $law  $xvar i.year $region [aweight=popwt],fe vce(cluster sid)
eststo s3
xi: xtreg l_homicide $law  $xvar i.year $region [aweight=popwt],fe vce(cluster sid)
eststo s4
xi: xtreg robbery_gun_r $law  $xvar i.year $region [aweight=popwt],fe vce(cluster sid)
eststo s5
xi:xtreg jhcitizen_c $law  $xvar l_pop i.year i.sid $region [aweight=popwt],fe vce(cluster sid)
eststo s6

esttab s1 s2 s3 s4 s5 s6 using TableA1_caselaw.csv, se star(* 0.1 ** 0.05 *** 0.01) scalars(F r2) replace

* anywhere
preserve
drop if state=="North Dakota"|state=="Alaska" | state=="Missouri" |state=="Ohio"

global law cdl  

xi: xtreg l_burglary $law  $xvar i.year $region [aweight=popwt],fe vce(cluster sid)
eststo s1
xi: xtreg l_robbery $law  $xvar i.year $region [aweight=popwt],fe vce(cluster sid)
eststo s2
xi: xtreg l_assault $law  $xvar i.year $region [aweight=popwt],fe vce(cluster sid)
eststo s3
xi: xtreg l_homicide $law  $xvar i.year $region [aweight=popwt],fe vce(cluster sid)
eststo s4
xi: xtreg robbery_gun_r $law  $xvar i.year $region [aweight=popwt],fe vce(cluster sid)
eststo s5
xi:xtreg jhcitizen_c $law  $xvar l_pop i.year i.sid $region [aweight=popwt],fe vce(cluster sid)
eststo s6

esttab s1 s2 s3 s4 s5 s6 using TableA1_anywhere.csv, se star(* 0.1 ** 0.05 *** 0.01) scalars(F r2) replace

restore

* presumption
replace strongcdl=cdl*assumption
replace weakcdl=cdl-strongcdl

global law strongcdl weakcdl 

xi: xtreg l_burglary $law  $xvar i.year $region [aweight=popwt],fe vce(cluster sid)
eststo s1
xi: xtreg l_robbery $law  $xvar i.year $region [aweight=popwt],fe vce(cluster sid)
eststo s2
xi: xtreg l_assault $law  $xvar i.year $region [aweight=popwt],fe vce(cluster sid)
eststo s3
xi: xtreg l_homicide $law  $xvar i.year $region [aweight=popwt],fe vce(cluster sid)
eststo s4
xi: xtreg robbery_gun_r $law  $xvar i.year $region [aweight=popwt],fe vce(cluster sid)
eststo s5
xi:xtreg jhcitizen_c $law  $xvar l_pop i.year i.sid $region [aweight=popwt],fe vce(cluster sid)
eststo s6

esttab s1 s2 s3 s4 s5 s6 using TableA1_presumption.csv, se star(* 0.1 ** 0.05 *** 0.01) scalars(F r2) replace

* civil liability
preserve
drop if state=="Montana" | state=="South Dakota" | state=="West Virginia"

global law cdl  

xi: xtreg l_burglary $law  $xvar i.year $region [aweight=popwt],fe vce(cluster sid)
eststo s1
xi: xtreg l_robbery $law  $xvar i.year $region [aweight=popwt],fe vce(cluster sid)
eststo s2
xi: xtreg l_assault $law  $xvar i.year $region [aweight=popwt],fe vce(cluster sid)
eststo s3
xi: xtreg l_homicide $law  $xvar i.year $region [aweight=popwt],fe vce(cluster sid)
eststo s4
xi: xtreg robbery_gun_r $law  $xvar i.year $region [aweight=popwt],fe vce(cluster sid)
eststo s5
xi:xtreg jhcitizen_c $law  $xvar l_pop i.year i.sid $region [aweight=popwt],fe vce(cluster sid)
eststo s6

esttab s1 s2 s3 s4 s5 s6 using TableA1_civil.csv, se star(* 0.1 ** 0.05 *** 0.01) scalars(F r2) replace

restore

** Table A2: Justifiable Homicide by Police **

xi: xtreg jhpolice_c $law i.year [aweight=popwt],fe vce(cluster sid)
eststo jhpct1
xi: xtreg jhpolice_c $law i.year $region [aweight=popwt], fe vce(cluster sid)
eststo jhpct2
xi: xtreg jhpolice_c $law i.year $region $xvar l_pop [aweight=popwt],fe vce(cluster sid)
eststo jhpct3
xi: xtreg jhpolice_c $law i.year $region $xvar l_pop $prelaw [aweight=popwt], fe vce(cluster sid)
eststo jhpct4
xi: xtreg jhpolice_c $law i.year $region $xvar l_pop $exocrime [aweight=popwt], fe vce(cluster sid)
eststo jhpct5
xi: xtreg jhpolice_c $law i.year $region $xvar l_pop $lintrend [aweight=popwt], fe vce(cluster sid)
eststo jhpct6

xi: xtreg jhpolice_c $law i.year,fe vce(cluster sid)
eststo jhpct7
xi: xtreg jhpolice_c $law i.year $region, fe vce(cluster sid)
eststo jhpct8
xi: xtreg jhpolice_c $law i.year $region $xvar l_pop, fe vce(cluster sid)
eststo jhpct9
xi: xtreg jhpolice_c $law i.year $region $xvar l_pop $prelaw, fe vce(cluster sid)
eststo jhpct10
xi: xtreg jhpolice_c $law i.year $region $xvar l_pop $exocrime, fe vce(cluster sid)
eststo jhpct11
xi: xtreg jhpolice_c $law i.year $region $xvar l_pop $lintrend, fe vce(cluster sid)
eststo jhpct12

esttab jhpct1 jhpct2 jhpct3 jhpct4 jhpct5 jhpct6 jhpct7 jhpct8 jhpct9 jhpct10 jhpct11 jhpct12 using TableA2_jhpolice_OLS_count.csv, se star(* 0.1 ** 0.05 *** 0.01) scalars(F r2) replace

xi:nbreg jhpolice_c $law i.year i.sid,exposure(population) vce(cluster sid)
eststo jhpnb1
xi:nbreg jhpolice_c $law i.year i.sid $region, exposure(population) vce(cluster sid)
eststo jhpnb2
xi:nbreg jhpolice_c $law i.year i.sid $region $xvar,exposure(population) vce(cluster sid)
eststo jhpnb3
xi:nbreg jhpolice_c $law i.year i.sid $region $xvar $prelaw, exposure(population) vce(cluster sid)
eststo jhpnb4
xi:nbreg jhpolice_c $law i.year i.sid $region $xvar $exocrime, exposure(population) vce(cluster sid)
eststo jhpnb5
xi:nbreg jhpolice_c $law i.year i.sid $region $xvar $lintrend, exposure(population) vce(cluster sid)
eststo jhpnb6

esttab jhpnb1 jhpnb2 jhpnb3 jhpnb4 jhpnb5 jhpnb6 using TableA2_jhpolice_nb.csv, se star(* 0.1 ** 0.05 *** 0.01) scalars(F r2) replace



log close
