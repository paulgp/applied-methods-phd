set scheme paul, perm

use "~/Dropbox/GPW/data/ACS_microdata_full/ACS_microdata_full_55_75.dta", clear

keep if year > 2013
sample 1000000, count

gen has_insurance = hcovany == 2

binscatter has_insurance inctot [weight=hhwt], linetype(none) n(10) xtitle("Total Income", size(large)) ytitle("") title("Has Health Insurance", position(11))
graph export "~/Dropbox/Teaching/applied-methods-phd/lectures/binscatter_insurance1.pdf", replace
binscatter has_insurance inctot [weight=hhwt], linetype(none) n(20) xtitle("Total Income", size(large)) ytitle("") title("Has Health Insurance", position(11))
graph export "~/Dropbox/Teaching/applied-methods-phd/lectures/binscatter_insurance2.pdf", replace
binscatter has_insurance inctot [weight=hhwt], linetype(none) n(50) xtitle("Total Income", size(large)) ytitle("") title("Has Health Insurance", position(11)) 
graph export "~/Dropbox/Teaching/applied-methods-phd/lectures/binscatter_insurance3.pdf", replace


binscatter has_insurance inctot [weight=hhwt],  linetype(none) n(10) xtitle("Total Income", size(large)) ytitle("") title("Has Health Insurance", position(11)) savedata("~/Dropbox/Teaching/applied-methods-phd/lectures/binscatter_insurance") replace
local control_list i.sex age
binscatter has_insurance inctot [weight=hhwt], controls(`control_list' ) absorb(statefip) linetype(none) n(10) xtitle("Total Income", size(large)) ytitle("") title("Has Health Insurance", position(11)) savedata("~/Dropbox/Teaching/applied-methods-phd/lectures/binscatter_insurance_controls") replace


fastxtile inctot_bin = inctot [weight=hhwt], n(10)
preserve
collapse inctot [weight=hhwt], by(inctot_bin)
tempfile tmp_means
save `tmp_means'
restore

sum has_insurance if inctot_bin == 1
local base_mean_y = r(mean)
reg has_insurance i.inctot_bin `control_list', absorb(statefip)
preserve
regsave

tempfile tmp
save `tmp'

insheet using "~/Dropbox/Teaching/applied-methods-phd/lectures/binscatter_insurance.csv", comma clear
tempfile tmp_nocontrols
save `tmp_nocontrols'
insheet using "~/Dropbox/Teaching/applied-methods-phd/lectures/binscatter_insurance_controls.csv", comma clear
tempfile tmp_controls
save `tmp_controls'

use `tmp'
keep if regexm(var, "inctot_bin")

gen inctot_bin = _n

merge 1:1 inctot_bin using `tmp_means'
drop _merge
drop var
gen outcome_reg = coef + `base_mean_y'

rename outcome_reg has_insurance
gen case = "reg"
append using `tmp_controls'
replace case = "bin_controls" if case == ""
append using `tmp_nocontrols'
replace case = "bin_nocontrols" if case == ""

twoway (scatter  has_insurance inctot if case == "reg") ///
  (scatter  has_insurance inctot if case == "bin_controls") ///
  (scatter  has_insurance inctot if case == "bin_nocontrols"), ///
  xtitle("Total Income", size(large)) ytitle("") title("Has Health Insurance", position(11)) ///
  legend(label(1 "Regression") label(2 "Binscatter w/ controls") label(3 "Binscatter w/o controls") rows(3) ring(0) position(5))
graph export "~/Dropbox/Teaching/applied-methods-phd/lectures/binscatter_insurance_controls_comparison.pdf", replace
restore

binsreg has_insurance inctot if inctot < 200000 & inctot >= 0 , line(3 3) ci(3 3) cb(3 3) dots(3 3)   xtitle("Total Income", size(large)) ytitle("") title("Has Health Insurance", position(11))
graph export "~/Dropbox/Teaching/applied-methods-phd/lectures/binsreg_insurance_nocontrols.pdf", replace

binsreg has_insurance inctot i.sex age i.statefip if inctot < 200000 & inctot >= 0 , line(3 3) cb(3 3) ci(3 3) dots(3 3)   xtitle("Total Income", size(large)) ytitle("") title("Has Health Insurance", position(11))
graph export "~/Dropbox/Teaching/applied-methods-phd/lectures/binsreg_insurance_controls.pdf", replace

use  final_zip_level5080_clean.dta, clear
collapse q_coll_12 avg_riskscore pctui_young  [aw=population ] if age < 65, by(state)
drop if pctui_young == .
twoway (scatter avg_riskscore   pctui_young) (lfit avg_riskscore   pctui_young), xtitle("State Uninsurance Rate") ytitle("") title("State Average Credit Score", pos(11)) legend(off)
graph export "~/Dropbox/Teaching/applied-methods-phd/lectures/example_scatter1.pdf", replace

use  final_zip_level5080_clean.dta, clear
collapse q_coll_12 avg_riskscore pctui_young  [aw=population ] if age < 65, by(zip state)
 drop if pctui_young == . | avg_riskscore == 0
twoway (scatter avg_riskscore   pctui_young) (lfit avg_riskscore   pctui_young), xtitle("Local Uninsurance Rate") ytitle("") title("Average Credit Score", pos(11)) legend(off)
graph export "~/Dropbox/Teaching/applied-methods-phd/lectures/example_scatter2.pdf", replace

egen state_id = group(state)
reg avg_riskscore i.state_id
predict y_res, res

reg pctui_young i.state_id
predict x_res, res

twoway (scatter y_res   x_res) (lfit y_res   x_res), xtitle("Local Uninsurance Rate") ytitle("") title("Average Credit Score", pos(11)) legend(off)

graph export "~/Dropbox/Teaching/applied-methods-phd/lectures/example_scatter3.pdf", replace



sum avg_riskscore
gen y_res2 = y_res + r(mean)

sum pctui_young
gen x_res2 = x_res + r(mean)
twoway (scatter y_res2   x_res2) (lfit y_res2   x_res2), xtitle("Local Uninsurance Rate") ytitle("") title("Average Credit Score", pos(11)) legend(off)

graph export "~/Dropbox/Teaching/applied-methods-phd/lectures/example_scatter4.pdf", replace
