* *******************************************************
* Data Preparation for Production Function Estimation
* *******************************************************
* Define paths to data sources for firm-level and industry-level data
cd "C:/Users/liu/Desktop/Empirics/Raw data"

* Data sources needs to be changed
local usedata="output03-04.dta"

* Why do we need lagged data??
local industrydata="industry_level_data.dta"

* Define paths to datasets for alternative output and input deflators
local newoutputd="alternative_output_deflator.dta"
local newinputd="alternative_input_deflators.dta"

* Estimation specification: Set labor measure based on specified version
* 'wagebillr' for version B, and 'employment' for other versions
if "$version"=="B" local labormeasure="wagebillr"
if "$version"~="B" local labormeasure="employment"

* Define temporary files for storing intermediate results
local estsample:tempfile  
* Estimation sample
local tempout:tempfile    
* Output temporary file
local tempin:tempfile     
* Input temporary file

* *******************************************************
* Data Merging and Cleaning
* *******************************************************
* Load firm-level data and clear existing data in memory
use "`usedata'", clear

* Merge firm-level data with industry-level data on 'year' and 'cic_adj'
merge m:1 year cic_adj using "`industrydata'"

* Check merge results and drop observations that do not match
tab _merge
drop if _merge==2
drop _merge

* Drop observations with zero wage or employment
drop if wage==0 | employment==0 

* For version C, use new deflators
if "$version"=="C" {
    preserve  
    use "`newoutputd'", clear
    reshape long output_d, i(cic_adj) j(year)
    * Reshape output deflators for merging
    rename output_d newoutput_d
    * Rename reshaped output deflator
    save `tempout'
    * Save reshaped output deflators for later use

    use "`newinputd'", clear
    reshape long input_d, i(cic_adj) j(year)
    rename input_d newinput_d
    save `tempin'

    restore  
    * Restore original data state after saving deflators

    * Merge the reshaped output and input deflators with main data
    merge n:1 year cic_adj using `tempout'
    drop _merge
    merge n:1 year cic_adj using `tempin'
    drop _merge

    * Generate variables adjusted with new deflators
    gen inputr = input / newinput_d
    gen outputr = output / newoutput_d
    gen var = output / newoutput_d - inputr
    * Generate variable 'var' (stand for deflated value added) for analysis
    egen wagebill = rsum(wage nonwage)
    * Aggregate total wagebill (wage and nonwage components)
    gen wagebillr = wagebill / newoutput_d
    * Generate real wagebill variable using new output deflator
}


* For other versions, use standard deflators
if "$version"~="C" {
    gen inputr = input / deflator_input_4d
    gen outputr = output / deflator_output_4d
    gen var = output / deflator_output_4d - inputr
    egen wagebill = rsum(wage nonwage)
    gen wagebillr = wagebill / deflator_output_4d
}

* HIGH RISK Generate log-transformed variables for regression analysis. Applies to variables like output, capital, labor, materials, etc. This line is particularly confusing for me and requires further investigation. I deleted var in the middle bc I think there is something wrong
for any q y k l m  \ outputr var real_cap `labormeasure' inputr : gen  X = log(Y)

* Create a dummy variable 'expdummy' to indicate whether a firm is exporting
gen expdummy = (export > 0 & export ~= .)
* Keep only the specified variables in the dataset
keep firm year q y k l m cic_adj expdummy ownership province wagebillr inputr input tariff* employment var outputr output deflator_*put_4d
* 'tariff*' and 'deflator_*put_4d' keeps all variables that match the pattern, which could include different tariff measures

sort firm year
* Sorting is important for panel data analysis and to ensure that operations like lagging are done correctly

duplicates drop firm year, force
* Remove duplicate observations for the same firm and year combination. This step is crucial to ensure the uniqueness of each observation in panel data

* Generate lagged and squared variables for regression analysis
for var q l k m: bysort firm (year): gen X_lag = X[_n-1]
for var l k m l_lag k_lag m_lag: gen X2 = X^2

* Generate interaction terms between variables
for any m l m_lag l_lag: gen Xk = X * k
for any m l m_lag l_lag: gen Xk_lag = X * k_lag
for any m m_lag \ any l l_lag: gen XY = X * Y

* Generate more complex interaction terms (e.g., labor*material*capital)
gen mlk = l * m * k
gen m_lagl_lagk_lag = m_lag * l_lag * k_lag
gen m_lagl_lagk = m_lag * l_lag * k

* Generate variables with different powers for non-export, export, output tariff, and input tariff
quietly forvalues ll = 0/3 {
    local kmax = 3 - `ll'
    forvalues kk = 0/`kmax' {
        local mmax = 3 - `ll' - `kk'
        forvalues mm = 0/`mmax' {
            gen NON`ll'`kk'`mm' = (l^`ll') * (k^`kk') * (m^`mm')
            gen EXP`ll'`kk'`mm' = (l^`ll') * (k^`kk') * (m^`mm') * expdummy
            gen OUTPUTTARIFF`ll'`kk'`mm' = (l^`ll') * (k^`kk') * (m^`mm') * tariff_output_l1
            gen INPUTTARIFF`ll'`kk'`mm' = (l^`ll') * (k^`kk') * (m^`mm') * tariff_input_l1
        }
    }
}   

* *******************************************************
* Sample Preparation for Estimation
* *******************************************************
* Handle missing data and create industry codes for analysis

gen missing = y + k + l + m
* If any of y, k, l, or m are missing, 'missing' will be missing for that observation

bysort firm (year): gen missing_lag = missing[_n-1]
* Tracks whether data in previous year is missing within the same firm

* Generate 2-digit and 4-digit industry codes and standard deviation of industry codes
gen cic2 = floor(cic_adj / 100)
egen gcic2 = group(cic2)
egen gcic4 = group(cic_adj)
if "$version"~="D" egen SDcic = sd(cic2), by(firm)
if "$version"=="D" egen SDcic = sd(cic_adj), by(firm)

* Drop observations with missing data and inconsistent industry codes
drop if missing == . | (missing_lag == . & year ~= 1998)
drop if SDcic ~= 0

* Drop auxiliary variables and set time series structure for panel data
drop missing missing_lag SDcic
tsset firm year
save `estsample'

* *******************************************************
* End of Data Preparation
* *******************************************************

* Estimate production functions by sectors
* Summarize industry codes to determine number of sectors
if "$version"~="D" sum gcic2
if "$version"=="D" sum gcic4

* Loop over each sector for fixed effects regression and other analysis
forvalues cc = 1/`r(max)' {  
    * Loop through each sector, with 'cc' taking values from 1 to the maximum sector code

    use `estsample', clear
    * Load the dataset named 'estsample' into memory, clearing any existing data in memory
    if "$version"~="D" keep if gcic2 == `cc'

    if "$version"=="D" keep if gcic4 == `cc'
    * Keep only observations from the sector identified by the current value of `cc'

	* Conduct fixed effects regression and generate phi (productivity/efficiency measure)
	capture xi: areg q NON* EXP* OUTPUTTARIFF* INPUTTARIFF* i.ownership i.year i.province, absorb(cic_adj)
	* Perform fixed effects regression of q on various variables and fixed effects
	* 'capture' is used to prevent the script from stopping if the command fails
	* 'areg' absorbs the fixed effects associated with 'cic_adj'
	* 'q' is the dependent variable
	* 'NON*', 'EXP*', 'OUTPUTTARIFF*', 'INPUTTARIFF*' are independent variables (patterns match multiple variables)
	* 'i.ownership', 'i.year', 'i.province' are categorical variables (factor variables) included in the model

	if _rc == 2001 continue
	* Check if the previous command failed (return code 2001)
	* If it failed, skip the rest of the commands in this loop and continue with the next iteration

	qui predict phi
	* 'phi' is the predicted value from the fixed effects regression
	* 'qui' (quietly) suppresses the output of the command

	qui gsort firm + year

	* Generate lagged phi and set up for GMM estimation
	preserve
		gen phi$version = phi
		* Create a new variable phi$version (e.g., phiA, phiB, etc.) which copies the values of phi

		keep firm year phi$version cic_adj gcic*
		* Keep only the specified variables in the dataset
		* gcic* keeps all variables that match the pattern (e.g., gcic2, gcic4, etc.)

		if `cc' ~= 1 append using "phi$version"
		* If not the first sector (cc not equal to 1), then append the data to an existing "phi$version" file

		save "phi$version", replace
		* Save the modified dataset as "phi$version" (e.g., phiA, phiB, etc.), replacing any existing file

	restore
	* Restore the original dataset state

	bysort firm: gen phi_lag = L.phi
	* Generate lagged phi variable by firm
	* L.phi creates a lagged version of phi within each firm

	qui drop _I*
	* Quietly drop all variables that start with _I (typically generated after factor variable regression)

	qui gen const = 1
	* Quietly generate a constant variable named 'const' with a value of 1 for all observations

	for var q l_lag k phi phi_lag: qui drop if X == .
	* This step cleans the dataset by removing rows with missing values in these key variables

	* OLS estimation for initial GMM values
	if "$version"~="E" {
		* Perform OLS regression quietly
		qui xi: regress q m l k i.cic_adj i.ownership i.year i.province

		* Generate variables for OLS coefficients
		for any m l k : qui gen OLSX = _b[X]  
		qui gen OLSConst = _b[_c]  

		* Generate initial values for GMM estimation based on OLS results
		for any m l k : qui gen initialX = OLSX  
		qui gen initialConst = OLSConst  

		* Run specification command (placeholder for actual command)
		qui `specification'

		* Store coefficients in matrix
		for num 1/4: matrix beta$version`cc'[`b',X] = beta_`specification'[1,X]  
	}

	if "$version"=="E" {
		* This time regress without m (deflated input)
		qui xi: regress q l k i.cic_adj i.ownership i.year i.province
		for any l k : qui gen OLSX = _b[X]  
		qui gen OLSConst = _b[_c]  
		for any l k : qui gen initialX = OLSX  
		qui gen initialConst = OLSConst  
		qui `specification'
		for num 1/3: matrix beta$version`cc'[`b',X] = beta_`specification'[1,X]  
	}

}