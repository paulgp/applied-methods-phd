

use "/Users/psg24/Dropbox/Papers/GGP_HoldingLength/data/holding_data.dta", clear

tsset property_id transaction_t


gen fym = F.ym
replace fym = ym(2017,9) if fym == .
gen duration  = fym - ym
keep if duration > 0

gen year = year(dofm(ym))
sample 1,  by(year)


save "/Users/psg24/Dropbox/Papers/GGP_HoldingLength/data/holding_data_sample100k.dta", replace
