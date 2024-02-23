cd "C:\Users\liu\Desktop\Empirics\Raw data"

clear
set mem 900m
set more off
tempfile temp1 temp2 temp3 temp4 temp5

* use (1) output01-02; (2) 1993 firm data "1993.dta"
* depreciation rate set at 0.09



***********************************************************************************************************************
*                                                                                                                     *
* Stage 1: Calculate r = province + two-digit industry nominal capital stock annual growth rate between 1993 and 1998 *
*                                                                                                                     *
***********************************************************************************************************************


*calcualte overall growth rate of all province/industry; we will use it later when some province/industry growth rates are missing 

use 1993.dta, clear
describe
save `temp1', replace

use output01-02, clear
describe

forvalues i = 1998(1)2007 {
	use 1993.dta, clear
	collapse (sum) f_cap
	save `temp1', replace

	use output01-02, clear
	drop if real(bdat`i') >1993
	collapse (sum) fa_original`i'
	merge using `temp1'
	gen a = (fa/f_cap)^(1/(`i'-1993))-1
	scalar average`i' = a
}

forvalues i = 1998(1)2007 {

	*** calculate 1993 capital stock by province/industry ***

	use 1993.dta, clear
	gen province = substr(dq,1,2)
	gen industry = substr(cic,1,2)
	drop if real(industry) >43 | real(industry) < 13
	gen code = province + industry
	collapse (sum) f_cap, by(code)
	sort code
	save `temp2', replace

	*** calculate growth rate of province/industry for each year 1998-2007 ***

	use output01-02, clear
	drop if real(bdat`i') >1993
	gen province`i' = substr(dq`i',1,2)
	replace province`i' = "51" if province`i' == "50" /* Chongqing separated from Sichuan Province in 1997 */
	gen industry`i' = substr(cic`i',1,2)

	if `i' >= 2003 {   /* industry code changed in 2003 */

		replace industry="43" if industry=="42"
		replace industry="42" if industry=="41"
		replace industry="41" if industry=="40"
		replace industry="40" if industry=="39"

	}

	gen code = province`i' + industry`i'
	collapse (sum) fa_original`i', by(code)
	sort code
	merge code using `temp2'
	gen growth`i' = fa/f_cap 
	gen g`i' = growth`i'^(1/(`i'-1993))-1  /* g`i' = province/two-digit industry nominal capital stock annual growth rate between 1993 and and year `i; */ 
	replace g`i' = 0.5 if g`i'>0.5 & g`i'!=.
	replace g`i' = 0 if g`i' <0
	replace g`i' = average`i' if g`i' == . /* use overall growth rate when province/industry growth rates are missing */
	sort code
	keep code g`i'
	drop in 1
	save g.`i'.dta, replace

}


use output01-02, clear
keep firm fa_original* bdat* dq* cic*
sort firm
save `temp4', replace

use `temp4', clear

forvalues i = 1998(1)2007 {

	gen province`i' = substr(dq`i',1,2)
	replace province`i' = "51" if province`i' == "50" 
	gen industry`i' = substr(cic`i',1,2)

	if `i' >= 2003 {   /* industry code changed in 2003 */

		replace industry`i'="43" if industry`i'=="42"
		replace industry`i'="42" if industry`i'=="41"
		replace industry`i'="41" if industry`i'=="40"
		replace industry`i'="40" if industry`i'=="39"

	}

	gen code = province`i' + industry`i'
	sort code
	joinby code using g.`i'.dta, unmatched (master)
	drop code _merge
}

keep firm fa_original* bdat* g*

forvalues i = 1998(1)2007 {
	
	replace g`i' = average`i' if g`i' == . 

}

reshape long fa_original bdat g, i(firm) j(year)

save `temp3', replace



****************************************************************************************************
*                                                                                                  *
* Stage 2: Calculate birth year capital stock and move forward to 1998 (or first year in database) *
*                                                                                                  *                                              
****************************************************************************************************


use `temp3', clear
drop if fa == .
gen b = real(bdat)
drop bdat
replace b = 1978 if b  < 1978  /* if birth year is smaller than 1978, assuming 1978 will have little effect on 1998 capital stock */
by firm: keep if _n == 1
gen k0 = fa/((1+g)^(year-b))

forvalues i = 1978 (1) 2007 {
	gen nk`i' = .
	gen rk`i' = .
}


** use updated Brandt-Rawski deflator: see Loren's email June 1, 2008 **
** create a template to calcualte capital stock **

gen p1978	= 	16.8
gen p1979 	= 	17.7
gen p1980	=	18.6
gen p1981	=	20.3
gen p1982	=	21.2
gen p1983	=	23.0
gen p1984	=	25.3
gen p1985	=	27.9
gen p1986	=	30.6
gen p1987	=	34.7
gen p1988	=	38.8
gen p1989	=	47.2
gen p1990	=	52.6
gen p1991	=	55.7
gen p1992	=	63.9
gen p1993	=	81.8
gen p1994	=	90.1
gen p1995	=	94.7
gen p1996	=	98.7
gen p1997	=	100.3
gen p1998	=	100.0
gen p1999	=	99.5
gen p2000	=	100.6
gen p2001	=	100.8
gen p2002	=	100.7
gen p2003	=	102.9
gen p2004	=	108.8
gen p2005	=	110.1
gen p2006 	=	111.7
gen p2007   =   116.7

forvalues i = 1978 (1) 2007 {
	replace nk`i' = k0 if `i' == b
	replace rk`i' = k0*100/p`i' if `i' == b

}

forvalues i = 1979 (1) 2007 {
	local j = `i' - 1
	replace nk`i' = nk`j'*(1+g) if nk`j' != .
	replace rk`i' = rk`j'*0.91+ nk`j'*g*100/p`i' if rk`j' != .
}

forvalues i = 1998 (1) 2007 {

	replace nk`i' = . if `i'>year
	replace rk`i' = . if `i'>year
	replace nk`i' = . if `i'<year
	replace rk`i' = . if `i'<year

}

keep firm nk1998-nk2007 rk1998-rk2007 p1998-p2007
sort firm
save `temp5', replace



**************************************************************
*                                                            *                                     
* Stage 3: Move from 1998 (or first year in database)to 2007 *
*                                                            *                                                                                
**************************************************************


use `temp4', clear
merge firm using `temp5'
drop _merge

forvalues i = 1999 (1) 2007 {
	local j = `i' - 1
	replace nk`i' = fa_original`i' if fa_original`i' != .
	replace nk`i' = nk`j' if nk`i' < nk`j'& nk`j'!=.
}

*** need to solve the problem that some years in the middle are missing 
*** for example, one firm can have 2000-2001 and 2003-2004 
*** impute the nominal capital stock in the middle year to allow the procedure to go through

forvalues i = 2008(1)2012 {
	gen nk`i' = .
}

forvalues i = 1998 (1) 2004 {

	local i1 = `i' + 1
	local i2 = `i' + 2
	local i3 = `i' + 3
	local i4 = `i' + 4
	local i5 = `i' + 5
	local i6 = `i' + 6
	local i7 = `i' + 7
	local i8 = `i' + 8

	replace nk`i1' = nk`i'*(nk`i2'/nk`i')^(1/2) if nk`i1' == . & nk`i2' != . 
	replace nk`i1' = nk`i'*(nk`i3'/nk`i')^(1/3) if nk`i1' == . & nk`i3' != . 
	replace nk`i1' = nk`i'*(nk`i4'/nk`i')^(1/4) if nk`i1' == . & nk`i4' != . 
	replace nk`i1' = nk`i'*(nk`i5'/nk`i')^(1/5) if nk`i1' == . & nk`i5' != . 
	replace nk`i1' = nk`i'*(nk`i6'/nk`i')^(1/6) if nk`i1' == . & nk`i6' != . 
	replace nk`i1' = nk`i'*(nk`i7'/nk`i')^(1/7) if nk`i1' == . & nk`i7' != . 
	replace nk`i1' = nk`i'*(nk`i7'/nk`i')^(1/8) if nk`i1' == . & nk`i8' != . 

}

forvalues i = 2008(1)2012 {
	drop nk`i'
}

forvalues i = 1999 (1) 2007 {
	local j = `i' - 1
	replace rk`i' = rk`j'*0.91+ (nk`i' - nk`j')*100/p`i' if nk`i' != . & nk`j'!=.

}

forvalues i = 1998(1)2007 {
	replace nk`i' = . if fa_original`i' == .
	replace rk`i' = . if fa_original`i' == .
}

forvalues i = 1998(1)2007 {

rename rk`i' real_cap`i'

}

keep firm real* 
sort firm

save output02-03.dta, replace

*** merge real capital stock back to the original file

merge firm using output01-02
drop _merge

exit

