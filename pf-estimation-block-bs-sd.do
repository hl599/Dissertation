* *******************************************************
* Data Preparation for Production Function Estimation
* *******************************************************
local usedata=".../firm-level5.dta"  
* Path to firm-level data
local industrydata=".../industry-level-1995-2007-lag.dta"  
* Path to industry-level data

local newoutputd="alternative output deflators 101216.dta"  
* Path to output deflators data
local newinputd="alternative io input deflators 101216.dta"  
* Path to input deflators data

local savepath="..."  
* Path to save the results

* Temporary Files Setup
local estsample:tempfile  
* Temporary file for estimation sample
local tempapp:tempfile  
* Temporary file for appending data

* Bootstrapping Setup
local B=100  
* Number of bootstrap iterations

* *******************************************************
* Data Merging and Cleaning
* *******************************************************
use "`usedata'", clear  
* Load firm-level data and clear existing data in memory
merge m:1 year cic_adj using "`industrydata'"  
* Merge firm-level and industry-level data
tab _merge  
* Tabulate merge results
drop if _merge==2  
* Drop observations that do not match
drop _merge  
* Drop the merge indicator variable

drop if wage==0 | employment==0  
* Drop observations with zero wage or employment

* Variable Transformations
gen inputr =input /deflator_input_4d  
* Generate real input variable
gen outputr=output/deflator_output_4d  
* Generate real output variable
gen var    =output/deflator_output_4d - inputr  
* Generate variable for analysis
egen wagebill =rsum(wage nonwage)  
* Aggregate wage bill
gen wagebillr=wagebill/deflator_output_4d  
* Generate real wage bill variable

* Generate log of variables for regression analysis
for any q y k l m  \ var outputr var real_cap `labormeasure' inputr : gen  X=log(Y)

* Export dummy and variable preparation
gen expdummy = (export>0 & export~=.)  
* Generate export dummy variable
keep firm year q y k l m cic_adj expdummy ownership province wagebillr inputr input tariff* employment var outputr output deflator_*put_4d  
* Keep relevant variables for analysis
sort firm year  
* Sort data by firm and year
duplicates drop firm year,force  
* Drop duplicate observations

* Lagged and Interaction Variables
for var q l k m: bysort firm (year): gen X_lag = X[_n-1]  
* Generate lagged variables
for var l k m l_lag k_lag m_lag: gen X2=X^2  
* Generate squared variables
for any m l m_lag l_lag: gen Xk=X*k  
* Generate interaction variables
for any m l m_lag l_lag: gen Xk_lag=X*k_lag  
* Generate lagged interaction variables
for any m m_lag \ any l l_lag: gen XY=X*Y  
* Generate cross-product variables
gen mlk=l*m*k  
* Generate three-way interaction variable
gen m_lagl_lagk_lag=m_lag*l_lag*k_lag  
* Generate lagged three-way interaction variable
gen m_lagl_lagk=m_lag*l_lag*k  
* Generate mixed lagged and current interaction variable

* More complex variable generation using loops
quietly forvalues ll = 0/3 {
	local kmax = 3 - `ll'
	forvalues kk = 0/`kmax' {
		local mmax = 3 - `ll' - `kk'
		forvalues mm = 0/`mmax' {
			* Generate interaction terms with different powers for non-export, export, output tariff, and input tariff variables
			gen NON`ll'`kk'`mm'=(l^`ll')*(k^`kk')*(m^`mm')
			gen EXP`ll'`kk'`mm'=(l^`ll')*(k^`kk')*(m^`mm')*expdummy

			gen OUTPUTTARIFF`ll'`kk'`mm'=(l^`ll')*(k^`kk')*(m^`mm')*tariff_output_l1
			gen INPUTTARIFF`ll'`kk'`mm'=(l^`ll')*(k^`kk')*(m^`mm')*tariff_input_l1

		}
	}
}	

* *******************************************************
* Sample Preparation for Estimation
* *******************************************************
gen missing=y+k+l+m  
* Generate variable for missing data check
bysort firm (year): gen missing_lag=missing[_n-1]  
* Generate lagged missing data variable

gen cic2=floor(cic_adj/100)  
* Generate 2-digit industry codes
egen gcic2 =group(cic2)  
* Group by 2-digit industry codes
egen gcic4=group(cic_adj)  
* Group by 4-digit industry codes
egen SDcic=sd(cic2), by(firm)  
* Generate standard deviation of industry codes within firms

drop if missing==. | (missing_lag==. & year~=1998)  
* Drop observations with missing data
drop if SDcic~=0  
* Drop observations with non-zero standard deviation of industry codes

drop missing missing_lag SDcic  
* Drop auxiliary variables

tsset firm year  
* Declare panel data structure
save `estsample'  
* Save the prepared sample

* *******************************************************
* End of Data Preparation
* *******************************************************

* Begin Estimation by Sectors
sum gcic2  
* Summarize 2-digit industry codes

* Loop over each sector for estimation
forvalues cc = 1/`r(max)' {  
	use `estsample', clear  
	* Load estimation sample

	keep if gcic2==`cc'  
	* Keep data for current sector
	matrix beta$version`cc'=J(`B',4,0)  
	* Initialize matrix for bootstrap results
	
	forval b=1/`B' {  
		* Loop over bootstrap iterations
		* Bootstrap Analysis
		preserve  
		* Preserve current dataset state
		
			bsample,cluster(firm)  
			* Bootstrap sample with clustering by firm
	
			* Regression Analysis
			capture xi: areg q NON* EXP* OUTPUTTARIFF* INPUTTARIFF* i.ownership i.year i.province,absorb(cic_adj)  
			* Fixed effects regression
			if _rc==2001 continue  
			* Continue if regression fails
			qui predict phi  
			* Predict efficiency or productivity measure
			qui gsort firm + year  
			* Sort by firm and year
			
			bysort firm: gen phi_lag=L.phi  
			* Generate lagged productivity measure
			qui drop _I*  
			* Drop indicator variables
			qui gen const=1  
			* Generate constant term
			for var q l_lag k phi phi_lag: qui drop if X==.  
			* Drop observations with missing values
	
			* OLS Estimation
			qui xi: regress q m l k i.cic_adj i.ownership i.year i.province  
			* OLS regression
			for any m l k : qui gen OLSX=_b[X]  
			* Store OLS coefficients
			qui gen OLSConst=_b[_c]	
			* Store OLS constant
	
			* Set Initial Values for GMM
			for any m l k : qui gen initialX=OLSX  
			* Generate initial values for GMM
			qui gen initialConst=OLSConst	
			* Generate initial constant for GMM
	
			qui `specification'
			* Run specified estimation technique
			for num 1/4: matrix beta$version`cc'[`b',X]=beta_`specification'[1,X]	
			* Store bootstrap results
	
		restore  
		* Restore original dataset state
	}
}

* Compile and Save Bootstrap Results
forval cc=1(1)29 {
	clear  
	* Clear dataset
	set obs `B'  
	* Set number of observations to number of bootstrap iterations
	for any 0 m l k  \ num 1/4: gen CDX=beta$version`cc'[1,Y]  
	* Generate variables for bootstrap coefficients
	forval n=2(1)`B' {
		for any 0 m l k \ num 1/4: qui replace CDX=beta$version`cc'[`n',Y] in `n'  
		* Replace coefficients for each bootstrap iteration
	}
	gen cc=`cc'  
	* Generate sector identifier variable
	if `cc'~=1 append using `tempapp'  
	* Append results for each sector
	save `tempapp',replace  
	* Save appended results
}

save "`savepath'/pfcoef_BS.dta"  
* Save final bootstrap results
