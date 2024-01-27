* Set the working directory
cd "C:/Users/liu/Desktop/Empirics/Raw data"
preserve
tempfile temp1
*------------------------------------*
* Convert province names into code id
*------------------------------------*

* Change this into ASIF_combined.dta
use "sample.dta", clear
use "province_concordance.dta", clear
merge 1:m 省自治区直辖市 using "sample.dta"
drop _merge
drop 省自治区直辖市
drop if missing(dq)
* It should not lose any data
save temp1

*------------------------------------*
* Convert type and equity value into ownership
*------------------------------------*
* Convert the chinese names into type code
use "ownership_concordance.dta", clear
merge 1:m 登记注册类型 using temp1
drop _merge
drop 登记注册类型
drop if missing(type)

rename 国家资本千元 e_state
rename 集体资本千元 e_collective
rename 法人资本千元 e_legal_person
rename 个人资本千元 e_individual
rename 港澳台资本千元 e_HMT
rename 外商资本千元 e_foreign
gen ownership = type

    * Recoding 'ownership' variable to categorize into 5 types: 1=state, 2=collective, 3=private, 4=foreign, 5=Hong Kong, Macau, and Taiwan
    recode ownership`i' 110 141 143 151=1 120 130 142 149=2 171 172 173 174 190=3 210 220 230 240=4 200 310 320 330 340=5 
	
	* Handling negative values in equity structure variables by setting them to 0 for each year
    for any HMT collective foreign state individual legal_person: replace e_X=0 if e_X<0
	
	egen e_total=rsum(e_*)
    replace e_state = e_state + e_legal_person
	
	for any state collective individual \ num 1/3: replace ownership=Y if (e_X>=e_state&e_X>=e_collective&e_X>=e_individual)&(ownership==159|ownership==160)

	drop e_*
	drop type
	
*------------------------------------*
* Rename variables
*------------------------------------*
rename 统计时间 year
rename 工业企业标识码 firm
rename 行业小类代码 cic
rename 开业成立时间年 bdat
rename 固定资产合计千元 fa_original

save "output01-03.dta", replace


keep year firm cic bdat fa_original ownership dq

tostring cic, replace
tostring dq, replace
tostring bdat, replace
reshape wide cic bdat fa_original ownership dq, i(firm) j(year)

save "output01-02.dta", replace
restore