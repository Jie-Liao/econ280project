******** graph code for "Does Strengthening Self-Defense Law Deter Crime or Escalate Violence?  Evidence from Castle Doctrine" ********
*** by Cheng Cheng and Mark Hoekstra
*** Department of Economics, Texas A&M University

* note: install "labmask" first, under "findit labutil"
cd "C:\Users\faculty\Documents\Cheng - Castle Doctrine\Data for Replication"
use castle-doctrine-2000-2010.dta, clear

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

* generate log data		
gen l_homicide=log(homicide)

gen l_pop=log(population)
gen l_police=log(police)
gen l_income=log(income)
gen l_prisoner=log(prisoner)
gen l_lagprisoner=log(lagprisoner)
gen l_exp_subsidy=log(exp_subsidy)
gen l_exp_pubwelfare=log(exp_pubwelfare)

** generate state population weight
bys sid: egen popwt=mean(population) 

** drop states **
drop if state=="District of Columbia"  //missing data

* treatment states
gen treatment=(effyear<.)

* define global macro
global demo blackm_15_24 whitem_15_24 blackm_25_44 whitem_25_44 
global spending l_exp_subsidy l_exp_pubwelfare
global region r20001-r20104 
global xvar l_police unemployrt poverty l_income l_prisoner l_lagprisoner $demo $spending

	

					**********************************
					*                                *
					*            Graph               *
					*                                *
					**********************************

** Figure 1
* a. treatment states: 2005 adopter (Florida)
preserve
keep if treatment==0 | (treatment==1 & effyear==2005)

bysort year treatment:egen homicideavg=mean(l_homicide)
bysort year: gen homicide_tr=homicideavg[_N] 
bysort year: gen homicide_co=homicideavg[1]

graph twoway (scatter homicide_tr year, connect(l) xline(2004.2)) ///
             (scatter homicide_co year, connect(l) clpattern(dash) ms(Oh)) ///
,xtitle("Year") xlabel(2000 (1) 2010) scheme(s1mono) play(black) ylabel(1 (0.1) 2, angle(0)) ///
 legend( order(1 "Treatment: Florida (law enacted in October 2005)" 2 "Control Group: States that did not enact a law 2000 - 2010") rows(2))
graph save homicide_05adopter, replace

restore

* b. treatment states: 2006 adopters (13 states)
preserve
keep if treatment==0 | (treatment==1 & effyear==2006)

bysort year treatment:egen homicideavg=mean(l_homicide)
bysort year: gen homicide_tr=homicideavg[_N] 
bysort year: gen homicide_co=homicideavg[1]

graph twoway (scatter homicide_tr year, connect(l) xline(2005.2)) ///
             (scatter homicide_co year, connect(l) clpattern(dash) ms(Oh)) ///
,xtitle("Year") xlabel(2000 (1) 2010) scheme(s1mono) play(black) ylabel(1 (0.1) 2, angle(0)) ///
 legend( order(1 "Treatment: States that enacted the law in 2006" 2 "Control Group: States that did not enact a law 2000 - 2010") rows(2))
graph save homicide_06adopter, replace

restore

* c. treatment states: 2007 adopters (3 states)
preserve
keep if treatment==0 | (treatment==1 & effyear==2007)

bysort year treatment:egen homicideavg=mean(l_homicide)
bysort year: gen homicide_tr=homicideavg[_N] 
bysort year: gen homicide_co=homicideavg[1]

graph twoway (scatter homicide_tr year, connect(l) xline(2006.2)) ///
             (scatter homicide_co year, connect(l) clpattern(dash) ms(Oh)) ///
,xtitle("Year") xlabel(2000 (1) 2010) scheme(s1mono) play(black) ylabel(1 (0.1) 2, angle(0)) ///
 legend( order(1 "Treatment: States that enacted the law in 2007" 2 "Control Group: States that did not enact a law 2000 - 2010") rows(2))
graph save homicide_07adopter, replace

restore

* d. treatment states: 2008 adopters (2 states)
preserve
keep if treatment==0 | (treatment==1 & effyear==2008)

bysort year treatment:egen homicideavg=mean(l_homicide)
bysort year: gen homicide_tr=homicideavg[_N] 
bysort year: gen homicide_co=homicideavg[1]

graph twoway (scatter homicide_tr year, connect(l) xline(2007.2)) ///
             (scatter homicide_co year, connect(l) clpattern(dash) ms(Oh)) ///
,xtitle("Year") xlabel(2000 (1) 2010) scheme(s1mono) play(black) ylabel(1 (0.1) 2, angle(0)) ///
 legend( order(1 "Treatment: States that enacted the law in 2008" 2 "Control Group: States that did not enact a law 2000 - 2010") rows(2))
graph save homicide_08adopter, replace

restore

* e. treatment states: 2009 adopters (Montana)
preserve
keep if treatment==0 | (treatment==1 & effyear==2009)

bysort year treatment:egen homicideavg=mean(l_homicide)
bysort year: gen homicide_tr=homicideavg[_N] 
bysort year: gen homicide_co=homicideavg[1]

graph twoway (scatter homicide_tr year, connect(l) xline(2008.2)) ///
             (scatter homicide_co year, connect(l) clpattern(dash) ms(Oh)) ///
,xtitle("Year") xlabel(2000 (1) 2010) scheme(s1mono) play(black) ylabel(0.5 (0.1) 1.5, angle(0)) ///
 legend( order(1 "Treatment: States that enacted the law in 2009" 2 "Control Group: States that did not enact a law 2000 - 2010") rows(2))
graph save homicide_09adopter, replace

restore


** Figure 2

gen t=year-effyear
gen t34_before=(t==-4 | t==-3)
gen t12_before=(t==-2 | t==-1)
gen t0=(t==0)
gen t12_after=(t==1 | t==2)
gen t34_after=(t>=3) 

* regression: weighted OLS
*controls are added with "$xvar" and region effects are added with $region
preserve

tempname sim
postfile `sim' b_34b b_12b b_0 b_12a b_34a using CDL_year_ols_w, replace

global CDL_year t34_before - t34_after

xi: xtreg l_homicide $CDL_year i.year $region $xvar   [aweight=popwt],fe vce(cluster sid)

	scalar b_34b=_b[t34_before]
	scalar b_12b=_b[t12_before]
	scalar b_0=_b[t0]
	scalar b_12a=_b[t12_after]
	scalar b_34a=_b[t34_after]
		
post `sim' (b_34b) (b_12b) (b_0) (b_12a) (b_34a) 
postclose `sim'

restore

* regression: unweighted OLS
preserve

tempname sim
postfile `sim' b_34b b_12b b_0 b_12a b_34a using CDL_year_ols_uw, replace

global CDL_year t34_before - t34_after

xi: xtreg l_homicide $CDL_year i.year $region $xvar ,fe vce(cluster sid)

	scalar b_34b=_b[t34_before]
	scalar b_12b=_b[t12_before]
	scalar b_0=_b[t0]
	scalar b_12a=_b[t12_after]
	scalar b_34a=_b[t34_after]
		
post `sim' (b_34b) (b_12b) (b_0) (b_12a) (b_34a) 
postclose `sim'

restore

* regression: negative binomial
preserve

tempname sim
postfile `sim' b_34b b_12b b_0 b_12a b_34a using CDL_year_nb, replace

global CDL_year t34_before - t34_after

xi:nbreg homicide_c $CDL_year i.year i.sid $region $xvar ,exposure(population) vce(cluster sid)

	scalar b_34b=_b[t34_before]
	scalar b_12b=_b[t12_before]
	scalar b_0=_b[t0]
	scalar b_12a=_b[t12_after]
	scalar b_34a=_b[t34_after]
		
post `sim' (b_34b) (b_12b) (b_0) (b_12a) (b_34a) 
postclose `sim'

restore

* graph: weighted OLS
use CDL_year_ols_w,clear 
keep b_34b b_12b b_0 b_12a b_34a
xpose, clear varname
drop _varname
gen t=_n
replace t=t-3

gen ta="-4 & -3"
replace ta="-2 & -1" if t==-1
replace ta="0" if t==0
replace ta="1 & 2" if t==1
replace ta="3 & 4" if t==2
labmask t, values(ta) 

graph twoway (scatter v1 t,connect(l)), xtitle("Year since the Adoption of Castle Doctrine Laws") ///
xlabel(-2(1)2, valuelabel) play(black2) ylabel(0(0.05)0.1) xline(-0.8) title("Weighted OLS")
graph save CDL_year_ols_w, replace

* graph: unweighted OLS
use CDL_year_ols_uw,clear 
keep b_34b b_12b b_0 b_12a b_34a
xpose, clear varname
drop _varname
gen t=_n
replace t=t-3

gen ta="-4 & -3"
replace ta="-2 & -1" if t==-1
replace ta="0" if t==0
replace ta="1 & 2" if t==1
replace ta="3 & 4" if t==2
labmask t, values(ta) 

graph twoway (scatter v1 t,connect(l)), xtitle("Year since the Adoption of Castle Doctrine Laws") ///
xlabel(-2(1)2, valuelabel) play(black2) ylabel(0(0.05)0.1) xline(-0.8) title("Unweighted OLS")
graph save CDL_year_ols_uw, replace

* graph: negative binomial
use CDL_year_nb,clear 
keep b_34b b_12b b_0 b_12a b_34a
xpose, clear varname
drop _varname
gen t=_n
replace t=t-3

gen ta="-4 & -3"
replace ta="-2 & -1" if t==-1
replace ta="0" if t==0
replace ta="1 & 2" if t==1
replace ta="3 & 4" if t==2
labmask t, values(ta) 

graph twoway (scatter v1 t,connect(l)), xtitle("Year since the Adoption of Castle Doctrine Laws") ///
xlabel(-2(1)2, valuelabel) play(black2) ylabel(0(0.05)0.1) xline(-0.8) title("Negative Binomial")
graph save CDL_year_ols_nb, replace
