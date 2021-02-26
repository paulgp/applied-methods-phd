insheet using ~/Dropbox/Teaching/applied-methods-phd/lectures/binscatter_insurance.csv.csv

twoway (scatter has_insurance inctot, mcolor(navy) lcolor(maroon)) , graphregion(fcolor(white))  xtitle(inctot) ytitle(has_insurance) legend(off order()) xtitle("Total Income", size(large)) ytitle("") title("Has Health Insurance", position(11))
