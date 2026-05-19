/**************************************************************************************************
* Location-Based Methods: empirical application figures from Stata CSV output
**************************************************************************************************/
clear all
set more off

capture confirm file "spp.csv"
if _rc {
    local dofile = subinstr("`c(filename)'", "\\", "/", .)
    local slash = strrpos("`dofile'", "/")
    if `slash' > 0 cd "`=substr("`dofile'", 1, `slash' - 1)'"
}

local outdir : environment RD2D_OUTPUT_DIR
if "`outdir'" == "" local outdir "output"
local figuresdir : environment RD2D_FIGURES_DIR
if "`figuresdir'" == "" local figuresdir "figures"
cap mkdir "`figuresdir'"

program define point_plot
    syntax using/, Saving(string) Yvar(name) Ytitle(string)
    preserve
    import delimited using "`using'", clear varnames(1)
    gen double idx = real(row)
    keep if idx < .
    twoway (rcap cilower ciupper idx, lcolor(gs10)) ///
           (scatter `yvar' idx, mcolor(black) msize(vsmall)), ///
           legend(off) xtitle("Boundary point") ytitle("`ytitle'") graphregion(color(white))
    graph export "`saving'", replace width(1800)
    restore
end

program define scatter_plot
    syntax using/, Saving(string) Yvar(name) Ytitle(string)
    preserve
    import delimited using "`using'", clear varnames(1)
    gen double idx = real(row)
    keep if idx < .
    twoway (scatter `yvar' idx, mcolor(black) msize(vsmall)), ///
           legend(off) xtitle("Boundary point") ytitle("`ytitle'") graphregion(color(white))
    graph export "`saving'", replace width(1800)
    restore
end

import delimited using "`outdir'/empapp_data.csv", clear varnames(1)
twoway (scatter x2 x1 if assignment==0, mcolor(red%20) msymbol(square) msize(tiny)) ///
       (scatter x2 x1 if assignment==1, mcolor(navy%20) msymbol(circle) msize(tiny)) ///
       (function y=0, range(-80 100) lcolor(gs8)) ///
       (function y=x*0, range(0 0) horizontal lcolor(gs8)), ///
       legend(order(1 "Control" 2 "Treatment")) xtitle("Saber 11") ytitle("Sisben") graphregion(color(white))
graph export "`figuresdir'/empapp_scatter.png", replace width(1800)

foreach suffix in bwmain bwitt {
    point_plot using "`outdir'/empapp_main_fuzzy_`suffix'.csv", saving("`figuresdir'/empapp_main_fuzzy_pointest_`suffix'.png") yvar(estimatep) ytitle("Fuzzy effect")
    point_plot using "`outdir'/empapp_main_itt_`suffix'.csv", saving("`figuresdir'/empapp_main_itt_pointest_`suffix'.png") yvar(estimatep) ytitle("ITT")
    point_plot using "`outdir'/empapp_main_fs_`suffix'.csv", saving("`figuresdir'/empapp_main_fs_pointest_`suffix'.png") yvar(estimatep) ytitle("First stage")
    point_plot using "`outdir'/empapp_covbal_itt_`suffix'.csv", saving("`figuresdir'/empapp_covbal_itt_pointest_`suffix'.png") yvar(estimatep) ytitle("Covariate balance ITT")
    scatter_plot using "`outdir'/empapp_main_percent_`suffix'.csv", saving("`figuresdir'/empapp_main_percent_itt_pointest_`suffix'.png") yvar(ittpercent) ytitle("ITT / ITT.0 (%)")
    scatter_plot using "`outdir'/empapp_main_percent_`suffix'.csv", saving("`figuresdir'/empapp_main_percent_fuzzy_pointest_`suffix'.png") yvar(fuzzypercent) ytitle("Fuzzy / ITT.0 (%)")
}

di as text "Stata empirical application figures written to: `figuresdir'"
