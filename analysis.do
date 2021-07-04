* analysis HMDA&SLOSH data
* data cleaning and selection
* summary statistics
* GWR function takes too long, will convert this to R for spatial analysis

clear
set more off
global root = "F:\Github\hmda_slosh"

**********************************
use "$root\hmda_slosh.dta",clear
rename as_of_year year
keep if property_type == 1
keep if owner_occupancy == 1
gen ln_loan_amount = ln( loan_amount_000s )
gen loan_ed = (action_taken == 1 | action_taken == 1) 
gen ln_applicant_income = ln( applicant_income_000s )
gen pc_occupied_units = number_of_owner_occupied_units/ population
gen pc_1to4_units = number_of_1_to_4_family_units / population
foreach i of numlist 1/9 {
gen denial_`i' = 0
replace denial_`i' = 1 if denial_reason_1 == `i'
replace denial_`i' = 1 if denial_reason_2 == `i'
replace denial_`i' = 1 if denial_reason_3 == `i'
}
gen denied = 0
replace denied = 1 if !mi(denial_reason_1)
replace denied = 1 if !mi(denial_reason_2)
replace denied = 1 if !mi(denial_reason_3)

*****************************************************************************
/* summary statistics */
su loan_ed *000s ln* c*high0
su loan_ed *000s ln* c*high0 if c5_high0 != 0

*****************************************************************************
/* non-spatial regression analysis */
*-----------------------------------
*Y1: loan_ed 
*baseline
est clear
local control = "i.year#i.county i.agency_code i.preapproval i.loan_type i.applicant_ethnicity i.co_applicant_ethnicity i.applicant_race_1 i.co_applicant_race_1 i.applicant_sex i.co_applicant_sex i.hoepa_status population minority_population hud_median_family_income tract_to_msamd_income pc_occupied_units pc_1to4_units"
foreach i of varlist c*high0 {
qui reg loan_ed ln* `i' `control', robust
est store `i'wz
qui reg loan_ed ln* `i' `control' if c5_high0 !=0, robust
est store `i'nz
}
esttab *wz *nz, keep(ln* *high0*) r2

*subsample: loan_purpose
foreach j of numlist 1/3 {
est clear
local control = "i.year#i.county i.agency_code i.loan_type i.preapproval i.applicant_ethnicity i.co_applicant_ethnicity i.applicant_race_1 i.co_applicant_race_1 i.applicant_sex i.co_applicant_sex i.hoepa_status population minority_population hud_median_family_income tract_to_msamd_income pc_occupied_units pc_1to4_units"
foreach i of varlist c*high0 {
qui reg loan_ed ln* `i' `control' if loan_purpose == `j', robust
est store `i'wz 
qui reg loan_ed ln* `i' `control' if loan_purpose == `j' & `i' !=0, robust
est store `i'nz
}
esttab *wz *nz, keep(ln* *high0*) r2
}

*------------------------------------------------------------------------------
*Y2: denial reason 
preserve
keep if denied == 1
*baseline
est clear
local control = "i.year#i.county i.agency_code i.preapproval i.loan_type i.applicant_ethnicity i.co_applicant_ethnicity i.applicant_race_1 i.co_applicant_race_1 i.applicant_sex i.co_applicant_sex i.hoepa_status population minority_population hud_median_family_income tract_to_msamd_income pc_occupied_units pc_1to4_units"
foreach j of numlist 1/9 {
foreach i of varlist c*high0 {
qui reg denial_`j' ln* `i' `control', robust
est store `i'wz
qui reg denial_`j' ln* `i' `control' if c5_high0 !=0, robust
est store `i'nz
}
esttab *wz *nz, keep(ln* *high0*) r2
}
local control = "i.year#i.county i.agency_code i.preapproval i.loan_type i.applicant_ethnicity i.co_applicant_ethnicity i.applicant_race_1 i.co_applicant_race_1 i.applicant_sex i.co_applicant_sex i.hoepa_status population minority_population hud_median_family_income tract_to_msamd_income pc_occupied_units pc_1to4_units"
foreach j of numlist 1/7 {
est clear
foreach i of varlist c*high0 {
qui logit denial_`j' ln* `i' `control', robust iterate(50)
est store `i'wz
qui logit denial_`j' ln* `i' `control' if c5_high0 !=0, robust iterate(50)
est store `i'nz
}
esttab *wz *nz, keep(ln* *high0*) r2
}
local control = "i.year i.county i.agency_code i.preapproval i.loan_type i.applicant_ethnicity i.co_applicant_ethnicity i.applicant_race_1 i.co_applicant_race_1 i.applicant_sex i.co_applicant_sex i.hoepa_status population minority_population hud_median_family_income tract_to_msamd_income pc_occupied_units pc_1to4_units"
foreach j of numlist 8 {
est clear
foreach i of varlist c*high0 {
qui logit denial_`j' ln* `i' `control', robust iterate(5)
est store `i'wz
qui logit denial_`j' ln* `i' `control' if c5_high0 !=0, robust iterate(5)
est store `i'nz
}
esttab *wz *nz, keep(ln* *high0*) r2
}
*subsample: loan_purpose
local control = "i.year i.county i.agency_code i.preapproval i.loan_type i.applicant_ethnicity i.co_applicant_ethnicity i.applicant_race_1 i.co_applicant_race_1 i.applicant_sex i.co_applicant_sex i.hoepa_status population minority_population hud_median_family_income tract_to_msamd_income pc_occupied_units pc_1to4_units"
foreach k of numlist 1/3 {
foreach j of numlist 1/8 {
est clear
foreach i of varlist c*high0 {
qui logit denial_`j' ln* `i' `control' if loan_purpose == `k', robust iterate(5)
est store `i'wz
qui logit denial_`j' ln* `i' `control' if c5_high0 !=0 & loan_purpose == `k', robust iterate(5)
est store `i'nz
}
esttab *wz *nz, keep(ln* *high0*) r2
}
}
restore 

*------------------------------------------------------------------------------
*Y3: rate_spread
preserve 
keep if rate_spread!=.
*baseline
est clear
local control = "i.year#i.county i.agency_code i.preapproval i.loan_type i.applicant_ethnicity i.co_applicant_ethnicity i.applicant_race_1 i.co_applicant_race_1 i.applicant_sex i.co_applicant_sex i.hoepa_status population minority_population hud_median_family_income tract_to_msamd_income pc_occupied_units pc_1to4_units"
foreach i of varlist c*high0 {
qui reg rate_spread ln* `i' `control', robust
est store `i'wz
qui reg rate_spread ln* `i' `control' if c5_high0 !=0, robust
est store `i'nz
}
esttab *wz *nz, keep(ln* *high0*) r2 

*subsample: loan_purpose
foreach j of numlist 1/3 {
est clear
local control = "i.year#i.county i.agency_code i.loan_type i.preapproval i.applicant_ethnicity i.co_applicant_ethnicity i.applicant_race_1 i.co_applicant_race_1 i.applicant_sex i.co_applicant_sex i.hoepa_status population minority_population hud_median_family_income tract_to_msamd_income pc_occupied_units pc_1to4_units"
foreach i of varlist c*high0 {
qui reg rate_spread ln* `i' `control' if loan_purpose == `j', robust
est store `i'wz 
qui reg rate_spread ln* `i' `control' if loan_purpose == `j' & `i' !=0, robust
est store `i'nz
}
esttab *wz *nz, keep(ln* *high0*) r2
}
restore 

