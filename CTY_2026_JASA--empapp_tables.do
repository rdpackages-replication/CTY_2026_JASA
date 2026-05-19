/**************************************************************************************************
* Location-Based Methods: empirical application tables from Stata CSV output
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
local tablesdir : environment RD2D_TABLES_DIR
if "`tablesdir'" == "" local tablesdir "tables"
cap mkdir "`tablesdir'"
local oldfiles : dir "`tablesdir'" files "empapp_*.tex"
foreach f of local oldfiles {
    erase "`tablesdir'/`f'"
}

program define write_effect_table
    syntax using/, Saving(string) Title(string)
    preserve
    import delimited using "`using'", clear varnames(1)
    gen double __rownum = real(row)
    keep if inlist(row,"WBATE","LBATE") | inlist(__rownum,1,5,10,15,20,25,30,35,40)
    file open tab using "`saving'", write replace text
    file write tab "\begin{tabular}{lrrrrrc}" _n
    file write tab "\toprule" _n
    file write tab "Boundary & h1 & h2 & N.Co & N.Tr & Estimate & p-value \\\" _n
    file write tab "\midrule" _n
    forvalues i=1/`=_N' {
        local label = row[`i']
        if inlist("`label'", "WBATE", "LBATE") file write tab "\midrule" _n
        local h1 = cond(missing(h01[`i']), "", string(h01[`i'], "%9.3f"))
        local h2 = cond(missing(h02[`i']), "", string(h02[`i'], "%9.3f"))
        local n0 = cond(missing(nco[`i']), "", string(nco[`i'], "%9.0f"))
        local n1 = cond(missing(ntr[`i']), "", string(ntr[`i'], "%9.0f"))
        local est = cond(missing(estimatep[`i']), "", string(estimatep[`i'], "%9.3f"))
        local pv = cond(missing(pvalue[`i']), "", string(pvalue[`i'], "%9.3f"))
        file write tab "`label' & `h1' & `h2' & `n0' & `n1' & `est' & `pv' \\\" _n
    }
    file write tab "\bottomrule" _n
    file write tab "\end{tabular}" _n
    file close tab
    restore
end

program define write_percent_table
    syntax using/, Saving(string)
    preserve
    import delimited using "`using'", clear varnames(1)
    gen double __rownum = real(row)
    keep if inlist(row,"WBATE","LBATE") | inlist(__rownum,1,5,10,15,20,25,30,35,40)
    file open tab using "`saving'", write replace text
    file write tab "\begin{tabular}{lrrrrrr}" _n
    file write tab "\toprule" _n
    file write tab "Boundary & h1 & h2 & N.Co & N.Tr & ITT / ITT.0 & Fuzzy / ITT.0 \\\" _n
    file write tab "\midrule" _n
    forvalues i=1/`=_N' {
        local label = row[`i']
        if inlist("`label'", "WBATE", "LBATE") file write tab "\midrule" _n
        local h1 = cond(missing(h01[`i']), "", string(h01[`i'], "%9.3f"))
        local h2 = cond(missing(h02[`i']), "", string(h02[`i'], "%9.3f"))
        local n0 = cond(missing(nco[`i']), "", string(nco[`i'], "%9.0f"))
        local n1 = cond(missing(ntr[`i']), "", string(ntr[`i'], "%9.0f"))
        local itt = cond(missing(ittpercent[`i']), "", string(ittpercent[`i'], "%9.3f"))
        local fuzzy = cond(missing(fuzzypercent[`i']), "", string(fuzzypercent[`i'], "%9.3f"))
        file write tab "`label' & `h1' & `h2' & `n0' & `n1' & `itt' & `fuzzy' \\\" _n
    }
    file write tab "\bottomrule" _n
    file write tab "\end{tabular}" _n
    file close tab
    restore
end

foreach suffix in bwmain bwitt {
    foreach stem in empapp_main_fuzzy empapp_main_itt empapp_main_fs empapp_main_itt0 empapp_covbal_itt {
        write_effect_table using "`outdir'/`stem'_`suffix'.csv", saving("`tablesdir'/`stem'_`suffix'.tex") title("`stem'")
    }
    write_percent_table using "`outdir'/empapp_main_percent_`suffix'.csv", saving("`tablesdir'/empapp_main_percent_`suffix'.tex")
}

di as text "Stata empirical application tables written to: `tablesdir'"
