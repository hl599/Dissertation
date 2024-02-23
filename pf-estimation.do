* *******************************************************
* Data Preparation for Production Function Estimation
* *******************************************************
* Define paths to data sources for firm-level and industry-level data
cd "C:/Users/liu/Desktop/Empirics/Raw data"

local usedata="output03-04.dta"
local industrydata="industry_level_data.dta"
local newoutputd="alternative_output_deflator.dta"
local newinputd="alternative_input_deflator.dta"

* 'wagebillr' for version B, and 'employment' for other versions
if "$version"=="B" local labormeasure="wagebillr"
if "$version"~="B" local labormeasure="employment"

local estsample:tempfile  
local tempout:tempfile    
local tempin:tempfile     
local specification "DLW_CD"

* *******************************************************
* Data Merging and Cleaning
* *******************************************************
use "`usedata'", clear
merge m:1 year cic_adj using "`industrydata'"

tab _merge
drop if _merge==2
drop _merge

drop if wage==0 | employment==0 

if "$version"=="C" {
    preserve  
    use "`newoutputd'", clear
    reshape long output_d, i(cic_adj) j(year)
    rename output_d newoutput_d
    save `tempout'

    use "`newinputd'", clear
    reshape long input_d, i(cic_adj) j(year)
    rename input_d newinput_d
    save `tempin'

    restore  

    merge n:1 year cic_adj using `tempout'
    drop _merge
    merge n:1 year cic_adj using `tempin'
    drop _merge

    gen inputr = input / newinput_d
    gen outputr = output / newoutput_d
    gen var = output / newoutput_d - inputr
    egen wagebill = rsum(wage nonwage)
    gen wagebillr = wagebill / newoutput_d
}


* For other versions, use standard deflators
if "$version"~="C" {
    gen inputr = input / deflator_input_4d
    gen outputr = output / deflator_output_4d
    gen var = output / deflator_output_4d - inputr
    egen wagebill = rsum(wage nonwage)
    gen wagebillr = wagebill / deflator_output_4d
}

for any q y k l m  \ var outputr var real_cap `labormeasure' inputr : gen  X = log(Y)
gen expdummy = (export > 0 & export ~= .)
keep firm year q y k l m cic_adj expdummy ownership province wagebillr inputr input tariff* employment var outputr output deflator_*put_4d
sort firm year
duplicates drop firm year, force

for var q l k m: bysort firm (year): gen X_lag = X[_n-1]
for var l k m l_lag k_lag m_lag: gen X2 = X^2
for any m l m_lag l_lag: gen Xk = X * k
for any m l m_lag l_lag: gen Xk_lag = X * k_lag
for any m m_lag \ any l l_lag: gen XY = X * Y
gen mlk = l * m * k
gen m_lagl_lagk_lag = m_lag * l_lag * k_lag
gen m_lagl_lagk = m_lag * l_lag * k
quietly forvalues ll = 0/3 {
    local kmax = 3 - `ll'
    forvalues kk = 0/`kmax' {
        local mmax = 3 - `ll' - `kk'
        forvalues mm = 0/`mmax' {
            gen NON`ll'`kk'`mm' = (l^`ll') * (k^`kk') * (m^`mm')
            gen EXP`ll'`kk'`mm' = (l^`ll') * (k^`kk') * (m^`mm') * expdummy
            gen OUTPUTTARIFF`ll'`kk'`mm' = (l^`ll') * (k^`kk') * (m^`mm') * tariff_output
            gen INPUTTARIFF`ll'`kk'`mm' = (l^`ll') * (k^`kk') * (m^`mm') * tariff_input
        }
    }
}   

* *******************************************************
* Sample Preparation for Estimation
* *******************************************************
gen missing = y + k + l + m
bysort firm (year): gen missing_lag = missing[_n-1]
gen cic2 = floor(cic_adj / 100)
egen gcic2 = group(cic2)
egen gcic4 = group(cic_adj)
if "$version"~="D" egen SDcic = sd(cic2), by(firm)
if "$version"=="D" egen SDcic = sd(cic_adj), by(firm)

drop if missing == . | (missing_lag == . & year ~= 1998)
drop if SDcic ~= 0

drop missing missing_lag SDcic
tsset firm year
save `estsample'

* *******************************************************
* End of Data Preparation
* *******************************************************

if "$version"~="D" {
	sum gcic2
	local `r(max)' = 29
}

if "$version"=="D" {
	sum gcic4
	local `r(max)' = 424
}

forvalues cc = 1/`r(max)' {  

	di "Iteration $version, `cc':"
    use `estsample', clear
    if "$version"~="D" keep if gcic2 == `cc'

    if "$version"=="D" keep if gcic4 == `cc'
	capture xi: areg q NON* EXP* OUTPUTTARIFF* INPUTTARIFF* i.ownership i.year i.province, absorb(cic_adj)
	* Perform fixed effects regression of q on various variables and fixed effects
	* 'capture' is used to prevent the script from stopping if the command fails
	* 'areg' absorbs the fixed effects associated with 'cic_adj'
	* 'q' is the dependent variable
    if _rc {
        di "Warning: An error occurred at $version, `cc' Continuing..."
		continue
    }

	qui predict phi
	qui gsort firm + year

	preserve
		gen phi$version = phi
		* Create a new variable phi$version (e.g., phiA, phiB, etc.) which copies the values of phi

		keep firm year phi$version cic_adj gcic*
		if `cc' ~= 1 append using "phi$version"
		save "phi$version", replace
	restore

	bysort firm: gen phi_lag = L.phi
	qui drop _I*
	qui gen const = 1
	for var q l_lag k phi phi_lag: drop if X==.

	if "$version"~="E" {
		qui xi: regress q m l k i.cic_adj i.ownership i.year i.province
		matrix beta$version`cc' = J(1, 4, .) 
		if _rc {
			di "Warning: An error occurred at $version, `cc' Continuing..."
			continue
		}
		for any m l k : qui gen OLSX = _b[X]  
		qui gen OLSConst = _b[_c]  
		for any m l k : qui gen initialX = OLSX  
		qui gen initialConst = OLSConst  
		qui `specification'
		for num 1/4: matrix beta$version`cc'[1,X] = beta_`specification'[1,X]  
	}

	if "$version"=="E" {
		qui regress q l k i.cic_adj i.ownership i.year i.province
		for any l k : qui gen OLSX = _b[X]  
		qui gen OLSConst = _b[_c]  
		for any l k : qui gen initialX = OLSX  
		qui gen initialConst = OLSConst  
		qui `specification'
		for num 1/3: matrix beta$version`cc'[`b',X] = beta_`specification'[1,X]  
	}

}