* Clear the Stata environment before starting analysis
clear
clear matrix
clear mata

* Set Stata environment settings
set matsize 10000  
* Increase matrix size limit to handle large datasets
set trace off  
* Turn off command tracing
set more off  
* Disable pausing in output display
set mem 2g  
* Allocate 2 gigabytes of memory for Stata

* Description of estimation versions
* These specifications apply to all versions
* (1) Use De Loecker and Warzynski (DLW) method
* (2) Use OLS estimates as initial values for GMM estimation
* (3) Use materials to construct the inverse function
* (4) Do not trim the sample
* (5) Include 1998 observations in the first stage
* (6) Exclude observations of new entries in their first year after 1998

* Execute DLW-3input.do which sets up Mata functions for estimation
do DLW-3input.do

* Version A: Benchmark labor measure using employment
global version="A"
do "pf estimation"  

* Version B: Labor measure using deflated wagebill
global version="B"
do "pf estimation"  

* Version C: Use new deflators
global version="C"
do "pf estimation"  

* Version D: Use 4-digit CIC codes for more detailed industry classification
global version="D"
do "pf estimation" 

* Compile coefficient estimates for each version
foreach v in A B C D {
    matrix beta`v'=beta`v'1
    * Combine coefficient matrices across sectors
    if "`v'"~="D" {
        forval n=2/29 {
            matrix beta`v'=beta`v'\beta`v'`n'
        }
    }
    if "`v'"=="D" {
        forval n=2/424 {
            matrix beta`v'=beta`v'\beta`v'`n'
        }
    }
    
    * Save the compiled coefficients
    clear
    svmat beta`v'
    if "`v'"~="D" gen gcic2=_n
    if "`v'"=="D" gen gcic4=_n      
    save beta`v', replace  * Save coefficients for version `v`
}

* Bootstrap standard error calculation for the benchmark version
do DLW-3input.do
do "pf estimation block bs sd"  * Execute block bootstrap estimation

* Load bootstrap results and calculate means and standard deviations
use pfcoef_BS.dta, clear
* Collapse data to get the mean and standard deviation of coefficients by sector
collapse (mean) c_coef=CD0 mcoef=CDm lcoef=CDl kcoef=CDk (sd) c_se=CD0 mse=CDm lse=CDl kse=CDk, by(cc)

* Adjust CIC codes for presentation
gen cic2=cc+12
replace cic2=cic2+1 if cic2>=38
reshape long c_ m l k, i(cic2 cc bad) j(est) s  

* Convert numerical estimates to strings for formatting
tostring m l k c_, replace force
* Format standard errors and coefficients
for any m l k c_: replace X="("+substr(X,1,4)+")" if est=="se"
for any m l k c_: replace X=substr(X,1,4) if est=="coef"
for any m l k c_: replace X=subinstr(X,".","0.",.)

* Order variables for output presentation
order cic2 m l k
