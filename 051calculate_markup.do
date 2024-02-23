clear
clear matrix
set more off
local datadir1 = "output03-05.dta"
local newoutputd="alternative_output_deflator.dta"
local newinputd="alternative_input_deflator.dta"


local tempout:tempfile    
local tempin:tempfile     
local temp: tempfile

 ***********************
* get firm-level data *
***********************
use "`datadir1'", clear
egen wagebill =rsum(wage nonwage)
drop if wage==0 | employment==0 
drop _merge
gen  cic2=floor(cic_adj/100)
egen gcic2 =group(cic2)
save `temp', replace

*****************
* get PF coef.  *
*****************
use "betaA", clear

merge 1:1 gcic2 using "betaB"
drop if _merge==2
drop _merge
merge 1:1 gcic2 using "betaC"
drop if _merge==2
drop _merge
merge 1:m gcic2 using `temp'
drop if _merge==2
drop _merge
save `temp', replace

***************************
* Get tariff plus benchmark deflators *
***************************
merge m:1 year cic_adj using "industry_level_data.dta"
* Check merge results and drop observations that do not match
drop if _merge==2
drop _merge
sort cic_adj year
save `temp', replace

***************************
* get new deflators*
***************************
use "`newoutputd'", clear
reshape long output_d, i(cic_adj) j(year)
save `tempout'

use "`newinputd'", clear
reshape long input_d, i(cic_adj) j(year)
save `tempin'

merge n:1 year cic_adj using `tempin'
drop _merge

merge n:1 year cic_adj using `tempout'
drop _merge

* Implement some adjustments to the new deflators
rename  input_d deflator_input_new
rename output_d deflator_output_new
for var deflator*: egen MX=mean(X) if cic_adj==2010|cic_adj==2011|cic_adj==2012, by(year)
for var deflator*: replace X=MX if X==.
replace deflator_output=deflator_input if deflator_output==. & cic_adj==3352 /* 1 change */
replace deflator_output=deflator_input if deflator_output==0 & cic_adj==3352 /* 6 change */
drop if cic_adj==.
drop M*
sort cic_adj year
merge cic_adj year using `temp'
drop if _merge==1
drop _merge
sort cic_adj year
save `temp', replace


* calculations *
for var output input: gen Xr =X/deflator_X_4d
for var output input: gen Xr2=X/deflator_X_new
for var outputr outputr2 employment real_cap inputr inputr2 \ any q q2 l k m m2: gen Y=log(X)
gen expdummy = (export>0 & export~=.)



*************************
* calculate phi for all *
*************************
* eliminated interaction with investment
quietly forvalues ll = 0/4 {
	local kmax = 4 - `ll'
	forvalues kk = 0/`kmax' {
		local mmax = 4 - `ll' - `kk'
		forvalues mm = 0/`mmax' {
			gen NON`ll'`kk'`mm'`ii'=(l^`ll')*(k^`kk')*(m^`mm')
			gen EXP`ll'`kk'`mm'`ii'=(l^`ll')*(k^`kk')*(m^`mm')*expdummy
			gen OUTPUTTARIFF`ll'`kk'`mm'`ii'=(l^`ll')*(k^`kk')*(m^`mm')*tariff_output
			gen INPUTTARIFF`ll'`kk'`mm'`ii' =(l^`ll')*(k^`kk')*(m^`mm')*tariff_output
		}
	}	
}
local temp2: tempfile
save `temp2', replace

clear
gen year=.
local phifile: tempfile
save `phifile', replace


forvalues cc = 1/29 {
	use `temp2', clear
	keep if gcic2==`cc'
	quietly xi: areg q NON* EXP* OUTPUTTARIFF* INPUTTARIFF* i.ownership i.year i.province,absorb(cic_adj)
	predict phi2
	keep firm year phi2
	append using `phifile'
	sort firm year
	save `phifile', replace
	}

**********************************
* calculate tfp & markup for all *
**********************************
use `temp2', clear
sort  firm year
merge firm year using `phifile'

for var beta*:   egen MX        =mean(X)       , by(cic2)
* for any c l k m: egen M4beta_X_D=mean(beta_X_D), by(cic_adj)
gen w=log(wagebill/deflator_output_4d)
for any A: gen tfpX = q  - MbetaX1  -  MbetaX2*m   - MbetaX3*l  - MbetaX4*k
for any B: gen tfpX = q  - MbetaX1  -  MbetaX2*m   - MbetaX3*w  - MbetaX4*k
for any C: gen tfpX = q2 - MbetaX1  -  MbetaX2*m2  - MbetaX3*l  - MbetaX4*k
* for any D: gen tfpX = q  - M4betaX1 -  M4betaX2*m  - M4betaX3*l - M4betaX4*k
* for any E: gen tfpX = q  - MbetaX1 - MbetaX2*l  - MbetaX3*k

gen     ms1=(input/exp(phi2))*(outputr/output)
gen     ms2=(input/output)
gen     ws =(wagebill /exp(phi2))*(outputr/output)
replace ws =(wagebill /output) if ws==.
for var ms1 ms2 ws: replace X=1 if X>1
gen mum1 = log(MbetaA2/ms1)
gen mum2 = log(MbetaA2/ms2)
gen mul  = log(MbetaA3/ws)

for var tfp* mum*: egen P1X =pctile(X), by(cic2 year) p(1)
for var tfp* mum*: egen P99X=pctile(X), by(cic2 year) p(99)
* drop P1tfpD P99tfpD
* for any 1 99:      egen PXtfpD=pctile(tfpD), by(cic_adj year) p(X)
for var tfp* mum*: replace X=. if X<P1X | X>P99X

* tfp level not comparable if firm switches 2-digit sector
egen      sdcic2=sd(cic2), by(firm)
drop if   sdcic2~=0
drop sdcic2

* Drop intermediate variables
drop M* P* EXP* OUTPUT* INPUT* NON* v* beta* 其中* 行业* 主要业务* _merge
save output05-06, replace
