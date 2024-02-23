* Set the working directory
cd "C:/Users/liu/Desktop/Empirics/Raw data"

*------------------------------------*
* industry concordance
*------------------------------------*

* Load the main dataset
use "output01-03", clear

* Load the concordance dataset for years before 2003
tempfile concordance_pre2003
use "CIC_ADJ-02.dta", clear
save `concordance_pre2003'

* Merge based on cic02 for years before 2003
use "output01-03", clear
tempfile pre2003
keep if year < 2003
rename cic cic02
merge m:1 cic02 using `concordance_pre2003', nogen
save `pre2003'

* Load the concordance dataset for years 2003 and onwards
tempfile concordance_post2002
use "CIC_ADJ-03.dta", clear
save `concordance_post2002'

* Merge based on cic03 for years 2003 and onwards
use "output01-03", clear
tempfile post2002
keep if year >= 2003
rename cic cic03
merge m:1 cic03 using `concordance_post2002', nogen
save `post2002'

* Combine the datasets
use `pre2003', clear
append using `post2002'
drop cic02 cic03
drop if missing(cic_adj)
* We would have to lose some data from this step bc industry > 44/<13 is dropped from the sample
tempfile tempdata
save `tempdata'


*------------------------------------*
* merge with real capital data
*------------------------------------*
use "output02-03", clear
keep firm real_cap*
reshape long real_cap, i(firm) j(year) string
destring year, replace
drop if missing(real_cap)
tempfile tempinput
save `tempinput'

* Merge with the main file
merge 1:m firm year using `tempdata'
drop if missing(real_cap)
drop if missing(dq)
* Lose some more data points here


*------------------------------------*
* rename variables accordingly
*------------------------------------*
rename firm firm_id
* Generate firm as a string
egen firm = group(firm_id)

rename 工业总产值_当年价格千元 output
rename 其中出口交货值千元 export
rename 全部从业人员年平均人数人 employment
replace employment = 年末从业人员合计人 if missing(employment)
rename 工业中间投入合计千元 input
rename 应付工资薪酬总额千元 wage
gen nonwage = 应付福利费总额千元 + 劳动失业保险费千元
rename dq province
for var output export employment input nonwage wage real_cap: drop if X < 0


save "output03-05.dta", replace

keep firm year output export employment input nonwage wage ownership province real_cap cic_adj 
save "output03-04.dta", replace