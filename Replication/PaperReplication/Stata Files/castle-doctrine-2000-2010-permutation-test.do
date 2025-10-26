******** simulation code for "Does Strengthening Self-Defense Law Deter Crime or Escalate Violence?  Evidence from Castle Doctrine" ********

** 5 treatment years are consecutive to mimic the real world **
* 10 years moving window 
* 5 treatment years are consecutive to mimic the real world 
* but the last year is the 6th treatment year corresponding to 2010
* 1960-1993 *??
cd "C:\Users\faculty\Documents\Cheng - Castle Doctrine\Data for Replication"


					**********************************
					*                                *
					*    Permutation Tests 1 & 2     *
					*                                *
					**********************************

use homicide-1960-2010.dta, clear

xtset sid year
set more off

gen population=homicide_c*100000/homicide
gen l_pop=log(population)
by sid: egen popwt=mean(population) //population weight

					
					
** sample choice **
drop if state=="District of Columbia"
drop if year>2004 //1960-2004

gen l_homicide=log(homicide)

** generate region-year effects
forvalues j=1960/2010{
gen year`j'=(year==`j')
gen r`j'1=year`j'*northeast
gen r`j'2=year`j'*midwest
gen r`j'3=year`j'*south
gen r`j'4=year`j'*west
drop year`j'
}

tempname sim
postfile `sim' b2 se2 pv2 niter using result, replace // change "results" name later

** simulation setup **
global nobs 1000 // number of simulation
set seed 10101 

forvalues i=1/$nobs {

* randomly pick post treatment period (6 years) during 1965-2004
scalar y1=1965+int((2000-1965+1)*runiform()) // first treatment year
scalar y2=y1+1
scalar y3=y1+2
scalar y4=y1+3
scalar y5=y1+4
scalar y6=y1+5



* randomly pick 21 treatment states
sort year state
by year: gen rannum=runiform()

bys year rannum: gen firsty=. //first year under treatment, with randomization based on rannum
bys year rannum: gen firsty2=. //first year under treatment, using CDL2 definition

bys year: gen rid=_n //id based on rannum

gen pick=(year==1960 & rid<=21) //pick 10 treatment states based on randomization in 1960 (1st year of the overall sample)
gen pick_order=rid*pick

bys state: egen treatment=total(pick)  //generate treatment variable
sort state year
by state: egen treatment_order=total(pick_order*treatment) //generate variable indicating treatment order


replace firsty2=0.246575  if year==y1 & treatment_order==1 //1 state in y1 (2005)
replace firsty2=0.580822  if year==y2 & treatment_order==2 //13 states in y2 (2006)
	replace firsty2=0.29589  if year==y2 & treatment_order==3
	replace firsty2=0.684932  if year==y2 & treatment_order==4
	replace firsty2=0.5  if year==y2 & treatment_order==5
	replace firsty2=0.5  if year==y2 & treatment_order==6
	replace firsty2=0.6  if year==y2 & treatment_order==7
	replace firsty2=0.468493  if year==y2 & treatment_order==8
	replace firsty2=0.375342  if year==y2 & treatment_order==9
	replace firsty2=0.246575  if year==y2 & treatment_order==10
	replace firsty2=0.5  if year==y2 & treatment_order==11
	replace firsty2=0.161644  if year==y2 & treatment_order==12
	replace firsty2=0.558904  if year==y2 & treatment_order==13
	replace firsty2=0.5  if year==y2 & treatment_order==14
replace firsty2=0.339726  if year==y3 & treatment_order==15 //4 states in y3 (2007)
	replace firsty2=0.413699  if year==y3 & treatment_order==16
	replace firsty2=0.608219  if year==y3 & treatment_order==17
	replace firsty2=0.328767 if year==y3 & treatment_order==18
replace firsty2=0.306849  if year==y4 & treatment_order==19 //2 states in y4 (2008)
	replace firsty2=0.838356  if year==y4 & treatment_order==20 
replace firsty2=0.66667  if year==y5 & treatment_order==21 //1 state in y5 (2009)

// define CDL & treatment
replace firsty=1 if firsty2~=.
sort state year
bys state: gen cdl=sum(firsty) //running sum

gen cdl2=cdl
replace cdl2=firsty2 if firsty2~=.

local t1 = y1-4
local t10 = y6

disp "now iteration has run `i' times" // iteration count

* choose specification
// xi: xtreg l_homicide cdl2  i.year*midwest i.year*south i.year*west [aweight=popwt] if year<=y6 & year>=y1-5,fe vce(cluster sid)
 xi: xtreg l_homicide cdl2  i.year*midwest i.year*south i.year*west if year<=y6 & year>=y1-5,fe vce(cluster sid)
// xi: nbreg homicide_c cdl2 i.year*midwest i.year*south i.year*west  i.sid if year<=y6 & year>=y1-5,exposure(population) vce(cluster sid)

scalar b2=_b[cdl]
scalar se2=_se[cdl]
scalar t2=b2/se2
scalar df2=e(N)-e(df_m)-1
scalar pv2=2*ttail(df2,abs(t2)) //add 2 to avoid confusion? Cameron and Trivedi P151
scalar niter=`i'

post `sim' (b2) (se2) (pv2) (niter)

drop rannum firsty firsty2 rid cdl cdl2 pick pick_order treatment treatment_order

}
postclose `sim'


* graph: weighted OLS
use 1960-2004-ols-w, clear // change the name of the "result" file to "1960-2004-ols-w"
count if pv2<0.05
count if pv2<0.1
sort b2

graph twoway (kdensity b2,  xline(0.0946)), title("Weighted OLS") xtitle("estimate of CDL") 
graph save 1960-2004-ols-w, replace

* graph: unweighted OLS
use 1960-2004-ols-uw, clear // change the name of the "result" file to "1960-2004-ols-uw"
count if pv2<0.05
count if pv2<0.1
sort b2

graph twoway (kdensity b2,  xline(0.0811)), title("Unweighted OLS") xtitle("estimate of CDL") 
graph save 1960-2004-ols-uw, replace

* graph: NB
use 1960-2004-nb, clear // change the name of the "result" file to "1960-2004-nb"
count if pv2<0.05
count if pv2<0.1
sort b2

graph twoway (kdensity b2,  xline(0.0734)), title("Negative Binomial") xtitle("estimate of CDL") 
graph save 1960-2004-nb, replace


					
