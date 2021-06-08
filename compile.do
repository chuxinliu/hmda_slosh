* compile slosh data from centroids sampling tool
* data cleaning output: c1-c5, mean and high

clear
set more off
global root = "C:\Users\chuxi\OneDrive\Documents\GitHub\hmda_slosh\centroids_slosh"

foreach j in ap3 cd2 em2 ep3 jx3 ms8 ok3 pa2 sf1 sv4 tp3 {
clear
import delimited "$root\\`j'.csv"
foreach i of varlist c* {
rename `i' `j'_`i'
}
save "$root\\`j'.dta", replace
}

use "$root\\ap3.dta", clear
foreach j in cd2 em2 ep3 jx3 ms8 ok3 pa2 sf1 sv4 tp3 {
merge 1:1 geo_id using "$root\\`j'.dta"
drop _merge
}

/*** data cleaning ***/
*11-digit fsid code for census tract
gen FSID = substr(geo_id, 10, 20)
destring FSID, gen(fsid)
drop topography
foreach i of varlist *c* {
replace `i' = . if `i' > 99.8 & `i' < 100
}
*drop ok3 data: I don't know how to incorporate this different data definition
drop *ok*
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

save "$root\\centroids_slosh.dta", replace
