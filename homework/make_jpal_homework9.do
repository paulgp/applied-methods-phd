

use analysis.dta, clear
global dubs_sampleJ   = "inlist(strata,1,2,3,4,21,22,23,24) & !inlist(round,100,200)"
keep if inlist(round,110,210) & ${dubs_sampleJ}
keep treatment ever_export id strata round ever_export_b log_weight_sidda_b qual_corners_b qual_waviness_b qual_weight_b qual_touch_b  qual_sidda_packed_b


save /Users/psg24/Dropbox/Teaching/applied-methods-phd/homework/data/jpal_analysis.dta, replace