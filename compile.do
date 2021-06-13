* compile slosh data from centroids sampling tool
* data cleaning output: c1-c5, mean and high

clear
set more off
global root = "F:\GitHub\hmda_slosh\centroids_slosh"

foreach j in ap3 cd2 em2 ep3 jx3 ms8 ok3 pa2 sf1 sv4 tp3 {
clear
import delimited "$root\\`j'.csv"
foreach i of varlist c*_* {
rename `i' `j'_`i'
}
save "$root\\`j'.dta", replace
}

use "$root\\ap3.dta", clear
foreach j in cd2 em2 ep3 jx3 ms8 pa2 sf1 sv4 tp3 {
merge 1:1 county tract using "$root\\`j'.dta"
drop _merge
}

/*** data cleaning ***/
foreach i of varlist *c*_* {
replace `i' = . if `i' > 99.8 & `i' < 100
}
*drop c0: not available in some areas
drop *c0*
*generate mean and high for c1 to c5
foreach j of numlist 1/5 {
foreach i in mean high {
egen float c`j'_`i' = rowmean(*c`j'*`i') 
gen c`j'_`i'0 = c`j'_`i'
label variable c`j'_`i'0 "c`j'_`i', missing value set as zero"
replace c`j'_`i'0 = 0 if c`j'_`i' == .
}
}

*11-digit fsid code for census tract
gen ctn = tract

keep county tract ctn c*
drop cd2*

save "$root\\centroids_slosh.dta", replace

/* merge HMDA with slosh */
use "F:\GitHub\hmda_slosh\hmda.dta", clear
drop fsid
gen county=county_code 
gen ctn = 100 * census_tract_number
replace ctn = round(ctn)
gen fsid = 12*1000*1000*1000 + county_code * 1000*1000 + ctn 
merge m:1 county ctn using "$root\\centroids_slosh.dta"
drop if _merge!=3
drop _merge
save "F:\GitHub\hmda_slosh\hmda_slosh.dta", replace