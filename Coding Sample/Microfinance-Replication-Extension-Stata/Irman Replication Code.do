clear all
set more off

* Directories
global datadir   "E:\Study\Fall 25\Metrix\PS\Paper"
global outputdir "E:\Study\Fall 25\Metrix\PS\Paper"
cd "$outputdir"

* Controls
global area_controls "area_pop_base area_debt_total_base area_business_total_base area_exp_pc_mean_base area_literate_head_base area_literate_base"

* Required packages
cap which mat2txt
if _rc ssc install mat2txt

cap which estout
if _rc ssc install estout

cap which eret2
if _rc ssc install eret2

*******************************************************
* TABLE 1A: BASELINE SUMMARY STATISTICS
*******************************************************
use "$datadir/baseline.dta", clear

local hh_composition "hh_size adult children male_head head_age head_noeduc"
local credit_access  "spandana othermfi bank informal anyloan"
local loan_amt       "spandana_amt othermfi_amt bank_amt informal_amt anyloan_amt"
local self_emp_activ "total_biz female_biz female_biz_pct"
local businesses     "bizrev bizexpense bizinvestment bizemployees hours_weekbiz"
local consumption    "total_exp_mo nondurable_exp_mo durables_exp_mo home_durable_index"

foreach var in `businesses' {
    gen `var'_allHH = `var'
    replace `var'_allHH = 0 if total_biz==0
}

local businesses_allHH ""
foreach var in `businesses' {
    local businesses_allHH "`businesses_allHH' `var'_allHH"
}

local allvars "`hh_composition' `credit_access' `loan_amt' `self_emp_activ' `businesses' `businesses_allHH' `consumption'"

capture matrix drop Table1A

foreach var of varlist `allvars' {
    quietly summarize `var' if treatment==0
    scalar N_`var'  = r(N)
    scalar M_`var'  = r(mean)
    scalar SD_`var' = r(sd)

    quietly regress `var' treatment, cluster(areaid)
    scalar D_`var' = _b[treatment]
    test treatment=0
    scalar P_`var' = r(p)

    matrix row = (N_`var', M_`var', SD_`var', D_`var', P_`var')
    matrix Table1A = (nullmat(Table1A) \ row)
}

matrix rownames Table1A = `allvars'
matrix colnames Table1A = Obs Control_mean Control_sd Difference p_val

mat2txt, matrix(Table1A) saving("$outputdir/table1a.txt") replace
import delimited "$outputdir/table1a.txt", delim(tab) clear
export delimited "$outputdir/table1a.csv", replace

*******************************************************
* TABLE 1B: ENDLINE 1 SUMMARY STATISTICS
*******************************************************
use "$datadir/endline12.dta", clear

local allvars_1 ///
hhsize_1 adults_1 children_1 male_head_1 head_age_1 head_noeduc_1 ///
spandana_1 othermfi_1 anymfi_1 anybank_1 anyinformal_1 anyloan_1 ///
spandana_amt_1 othermfi_amt_1 anymfi_amt_1 bank_amt_1 informal_amt_1 anyloan_amt_1 ///
bizrev_1 bizexpense_1 bizinvestment_1 bizemployees_1 bizprofit_1 any_biz_1 total_biz_1 ///
hours_week_1 hours_week_biz_1 hours_week_outside_1 ///
total_exp_mo_pc_1 nondurable_exp_mo_pc_1 durables_exp_mo_pc_1 food_exp_mo_pc_1

capture matrix drop Table1B_EL1

foreach var of local allvars_1 {
    quietly summarize `var' if treatment==0
    matrix row = (r(N), r(mean), r(sd), ., .)

    quietly regress `var' treatment, cluster(areaid)
    matrix row[1,4] = _b[treatment]

    test treatment=0
    matrix row[1,5] = r(p)

    matrix Table1B_EL1 = (nullmat(Table1B_EL1) \ row)
}

matrix rownames Table1B_EL1 = `allvars_1'
matrix colnames Table1B_EL1 = Obs Control_mean Control_sd Difference p_val

mat2txt, matrix(Table1B_EL1) saving("$outputdir/table1b_el1.txt") replace
import delimited "$outputdir/table1b_el1.txt", delim(tab) clear
export delimited "$outputdir/table1b_el1.csv", replace

*******************************************************
* TABLE 2: CREDIT
*******************************************************
use "$datadir/endline12.dta", clear
est clear

foreach var in spandana_1 othermfi_1 anymfi_1 anybank_1 anyinformal_1 anyloan_1 everlate_1 mfi_loan_cycles_1 ///
               spandana_amt_1 othermfi_amt_1 anymfi_amt_1 bank_amt_1 informal_amt_1 anyloan_amt_1 credit_index_1 {
    reg `var' treatment $area_controls [pweight=w1], cluster(areaid)
    eret2 scalar pval=2*ttail(e(df_r),abs(_b[treatment]/_se[treatment]))
    sum `var' if treatment==0 & e(sample)
    eret2 scalar mn1=r(mean)
    eret2 scalar sd1=r(sd)
    est store `var'
}

estout * using "table2.txt", drop($area_controls _cons) title("Table 2: Credit, Endline 1") ///
    prehead("" @title) cells(b(fmt(a3) s) se(fmt(a3) par("="))) replace s(r2 mn1 sd1 N pval) ///
    starlevels(* .1 ** .05 *** .01) legend

est clear

foreach var in spandana_2 othermfi_2 anymfi_2 anybank_2 anyinformal_2 anyloan_2 everlate_2 mfi_loan_cycles_2 ///
               spandana_amt_2 othermfi_amt_2 anymfi_amt_2 bank_amt_2 informal_amt_2 anyloan_amt_2 credit_index_2 {
    reg `var' treatment $area_controls [pweight=w2], cluster(areaid)
    eret2 scalar pval=2*ttail(e(df_r),abs(_b[treatment]/_se[treatment]))
    sum `var' if treatment==0 & e(sample)
    eret2 scalar mn2=r(mean)
    eret2 scalar sd2=r(sd)
    est store `var'
}

estout * using "table2.txt", drop($area_controls _cons) title("Table 2: Credit, Endline 2") ///
    prehead("" @title) cells(b(fmt(a3) s) se(fmt(a3) par("="))) append s(r2 mn2 sd2 N pval) ///
    starlevels(* .1 ** .05 *** .01) legend

import delimited using "table2.txt", delim(tab) clear
export delimited using "$outputdir/table2.csv", replace

*******************************************************
* TABLE 3: SELF-EMPLOYMENT (ALL HOUSEHOLDS)
*******************************************************
use "$datadir/endline12.dta", clear
est clear

foreach var in bizassets_1 bizinvestment_1 bizrev_1 bizexpense_1 bizprofit_1 any_biz_1 ///
               total_biz_1 any_new_biz_1 biz_stop_1 newbiz_1 female_biz_new_1 biz_index_all_1 {
    reg `var' treatment $area_controls [pweight=w1], cluster(areaid)
    eret2 scalar pval=2*ttail(e(df_r),abs(_b[treatment]/_se[treatment]))
    sum `var' if treatment==0 & e(sample)
    eret2 scalar mn1=r(mean)
    eret2 scalar sd1=r(sd)
    est store `var'
}

estout * using "table3.txt", drop($area_controls _cons) title("Table 3: Self-employment, Endline 1") ///
    prehead("" @title) cells(b(fmt(a3) s) se(fmt(a3) par("="))) replace s(r2 mn1 sd1 N pval) ///
    starlevels(* .1 ** .05 *** .01) legend

import delimited using "$outputdir/table3.txt", delim(tab) clear
export delimited using "$outputdir/table3_endline1.csv", replace

est clear
use "$datadir/endline12.dta", clear

foreach var in bizassets_2 bizinvestment_2 bizrev_2 bizexpense_2 bizprofit_2 any_biz_2 ///
               total_biz_2 any_new_biz_2 biz_stop_2 newbiz_2 female_biz_new_2 biz_index_all_2 {
    reg `var' treatment $area_controls [pweight=w2], cluster(areaid)
    eret2 scalar pval=2*ttail(e(df_r),abs(_b[treatment]/_se[treatment]))
    sum `var' if treatment==0 & e(sample)
    eret2 scalar mn2=r(mean)
    eret2 scalar sd2=r(sd)
    est store `var'
}

estout * using "table3.txt", drop($area_controls _cons) title("Table 3: Self-employment, Endline 2") ///
    prehead("" @title) cells(b(fmt(a3) s) se(fmt(a3) par("="))) append s(r2 mn2 sd2 N pval) ///
    starlevels(* .1 ** .05 *** .01) legend

import delimited using "$outputdir/table3.txt", delim(tab) clear
export delimited using "$outputdir/table3_endline2.csv", replace

*******************************************************
* TABLE 3B: SELF-EMPLOYMENT (OLD BUSINESSES)
*******************************************************
use "$datadir/endline12.dta", clear
est clear

foreach var in bizassets_1 bizinvestment_1 bizrev_1 bizexpense_1 bizprofit_1 bizemployees_1 biz_index_old_1 {
    reg `var' treatment $area_controls if any_old_biz==1 [pweight=w1], cluster(areaid)
    eret2 scalar pval=2*ttail(e(df_r),abs(_b[treatment]/_se[treatment]))
    sum `var' if treatment==0 & any_old_biz==1
    eret2 scalar mn1=r(mean)
    eret2 scalar sd1=r(sd)
    est store `var'
}

estout * using "table3b.txt", drop($area_controls _cons) title("Table 3B: Old Businesses, Endline 1") ///
    prehead("" @title) cells(b(fmt(a3) s) se(fmt(a3) par("="))) replace s(r2 mn1 sd1 N pval) ///
    starlevels(* .1 ** .05 *** .01) legend

import delimited using "$outputdir/table3b.txt", delim(tab) clear
export delimited using "$outputdir/table3b_endline1.csv", replace

use "$datadir/endline12.dta", clear
est clear

foreach var in bizassets_2 bizinvestment_2 bizrev_2 bizexpense_2 bizprofit_2 bizemployees_2 biz_index_old_2 {
    reg `var' treatment $area_controls if old_biz==1 [pweight=w2], cluster(areaid)
    est store `var'
}

estout * using "$outputdir/table3b_endline2.txt", drop($area_controls _cons) title("Table 3B: Old Businesses, Endline 2") ///
    prehead("" @title) cells(b(fmt(a3) star) se(fmt(a3) par("="))) replace s(r2 N) ///
    starlevels(* .1 ** .05 *** .01) legend

import delimited using "$outputdir/table3b_endline2.txt", delim(tab) clear
export delimited using "$outputdir/table3b_endline2.csv", replace

*******************************************************
* TABLE 3C: SELF-EMPLOYMENT (NEW BUSINESSES EL1)
*******************************************************
use "$datadir/endline12.dta", clear
est clear

foreach var in bizassets_1 bizinvestment_1 bizrev_1 bizexpense_1 bizprofit_1 bizemployees_1 biz_index_new_1 {
    reg `var' treatment $area_controls if newbiz_1>0 & newbiz_1!=. [pweight=w1], cluster(areaid)
    eret2 scalar pval=2*ttail(e(df_r),abs(_b[treatment]/_se[treatment]))
    sum `var' if treatment==0 & newbiz_1>0 & newbiz_1!=.
    eret2 scalar mn1=r(mean)
    eret2 scalar sd1=r(sd)
    est store `var'
}

estout * using "table3c.txt", drop($area_controls _cons) title("Table 3C: New Businesses, Endline 1") ///
    prehead("" @title) cells(b(fmt(a3) s) se(fmt(a3) par("="))) replace s(r2 mn1 sd1 N pval) ///
    starlevels(* .1 ** .05 *** .01) legend

import delimited using "$outputdir/table3c.txt", delim(tab) clear
export delimited using "$outputdir/table3c_endline1.csv", replace

*******************************************************
* TABLE 4: INCOME
*******************************************************
use "$datadir/endline12.dta", clear
est clear

foreach var in bizprofit_1 wages_nonbiz_1 income_index_1 {
    reg `var' treatment $area_controls [pweight=w1], cluster(areaid)
    eret2 scalar pval=2*ttail(e(df_r),abs(_b[treatment]/_se[treatment]))
    sum `var' if treatment==0 & e(sample)
    eret2 scalar mn1=r(mean)
    eret2 scalar sd1=r(sd)
    est store `var'
}

estout * using "table4.txt", drop($area_controls _cons) title("Table 4: Income, Endline 1") ///
    prehead("" @title) cells(b(fmt(a3) s) se(fmt(a3) par("="))) replace s(r2 mn1 sd1 N pval) ///
    starlevels(* .1 ** .05 *** .01) legend

est clear

foreach var in bizprofit_2 wages_nonbiz_2 income_index_2 {
    reg `var' treatment $area_controls [pweight=w2], cluster(areaid)
    eret2 scalar pval=2*ttail(e(df_r),abs(_b[treatment]/_se[treatment]))
    sum `var' if treatment==0 & e(sample)
    eret2 scalar mn2=r(mean)
    eret2 scalar sd2=r(sd)
    est store `var'
}

estout * using "table4.txt", drop($area_controls _cons) title("Table 4: Income, Endline 2") ///
    prehead("" @title) cells(b(fmt(a3) s) se(fmt(a3) par("="))) append s(r2 mn2 sd2 N pval) ///
    starlevels(* .1 ** .05 *** .01) legend

import delimited using "table4.txt", delim(tab) clear
export delimited using "table4_combined.csv", replace

*******************************************************
* TABLE 5: HOUSEHOLD LABOR HOURS
*******************************************************
use "$datadir/endline12.dta", clear
est clear

foreach var in hours_week_1 hours_week_biz_1 hours_week_outside_1 ///
               hours_girl1620_week_1 hours_boy1620_week_1 ///
               hours_headspouse_week_1 hours_headspouse_biz_1 hours_headspouse_outside_1 labor_index_1 {
    reg `var' treatment $area_controls [pweight=w1], cluster(areaid)
    eret2 scalar pval=2*ttail(e(df_r),abs(_b[treatment]/_se[treatment]))
    sum `var' if treatment==0 & e(sample)
    eret2 scalar mn1=r(mean)
    eret2 scalar sd1=r(sd)
    est store `var'
}

estout * using "table5.txt", drop($area_controls _cons) title("Table 5: Labor, Endline 1") ///
    prehead("" @title) cells(b(fmt(a3) s) se(fmt(a3) par("="))) replace s(r2 mn1 sd1 N pval) ///
    starlevels(* .1 ** .05 *** .01) legend

est clear

foreach var in hours_week_2 hours_week_biz_2 hours_week_outside_2 ///
               hours_girl1620_week_2 hours_boy1620_week_2 ///
               hours_headspouse_week_2 hours_headspouse_biz_2 hours_headspouse_outside_2 labor_index_2 {
    reg `var' treatment $area_controls [pweight=w2], cluster(areaid)
    eret2 scalar pval=2*ttail(e(df_r),abs(_b[treatment]/_se[treatment]))
    sum `var' if treatment==0 & e(sample)
    eret2 scalar mn2=r(mean)
    eret2 scalar sd2=r(sd)
    est store `var'
}

estout * using "table5.txt", drop($area_controls _cons) title("Table 5: Labor, Endline 2") ///
    prehead("" @title) cells(b(fmt(a3) s) se(fmt(a3) par("="))) append s(r2 mn2 sd2 N pval) ///
    starlevels(* .1 ** .05 *** .01) legend

import delimited using "table5.txt", delim(tab) clear
export delimited using "table5.csv", replace

*******************************************************
* TABLE 6: CONSUMPTION
*******************************************************
use "$datadir/endline12.dta", clear
est clear

foreach var in total_exp_mo_pc_1 durables_exp_mo_pc_1 nondurable_exp_mo_pc_1 food_exp_mo_pc_1 health_exp_mo_pc_1 ///
               educ_exp_mo_pc_1 temptation_exp_mo_pc_1 festival_exp_mo_pc_1 home_durable_index_1 {
    reg `var' treatment $area_controls [pweight=w1], cluster(areaid)
    eret2 scalar pval=2*ttail(e(df_r),abs(_b[treatment]/_se[treatment]))
    sum `var' if treatment==0 & e(sample)
    eret2 scalar mn1=r(mean)
    eret2 scalar sd1=r(sd)
    est store `var'
}

estout * using "table6.txt", drop($area_controls _cons) title("Table 6: Consumption, Endline 1") ///
    prehead("" @title) cells(b(fmt(a3) s) se(fmt(a3) par("="))) replace s(r2 mn1 sd1 N pval) ///
    starlevels(* .1 ** .05 *** .01) legend

est clear

foreach var in total_exp_mo_pc_2 durables_exp_mo_pc_2 nondurable_exp_mo_pc_2 food_exp_mo_pc_2 health_exp_mo_pc_2 ///
               educ_exp_mo_pc_2 temptation_exp_mo_pc_2 festival_exp_mo_pc_2 home_durable_index_2 {
    reg `var' treatment $area_controls [pweight=w2], cluster(areaid)
    eret2 scalar pval=2*ttail(e(df_r),abs(_b[treatment]/_se[treatment]))
    sum `var' if treatment==0 & e(sample)
    eret2 scalar mn2=r(mean)
    eret2 scalar sd2=r(sd)
    est store `var'
}

estout * using "table6.txt", drop($area_controls _cons) title("Table 6: Consumption, Endline 2") ///
    prehead("" @title) cells(b(fmt(a3) s) se(fmt(a3) par("="))) append s(r2 mn2 sd2 N pval) ///
    starlevels(* .1 ** .05 *** .01) legend

import delimited using "table6.txt", delim(tab) clear
export delimited using "table6.csv", replace

*******************************************************
* TABLE 7: SOCIAL EFFECTS
*******************************************************
use "$datadir/endline12.dta", clear
est clear

foreach var in girl515_school_1 boy515_school_1 girl515_workhrs_pc_1 boy515_workhrs_pc_1 girl1620_school_1 boy1620_school_1 ///
               women_emp_index_1 female_biz_new_1 social_index_1 {
    reg `var' treatment $area_controls [pweight=w1], cluster(areaid)
    eret2 scalar pval=2*ttail(e(df_r),abs(_b[treatment]/_se[treatment]))
    sum `var' if treatment==0 & e(sample)
    eret2 scalar mn1=r(mean)
    eret2 scalar sd1=r(sd)
    est store `var'
}

estout * using "table7.txt", drop($area_controls _cons) title("Table 7: Social Effects, Endline 1") ///
    prehead("" @title) cells(b(fmt(a3) s) se(fmt(a3) par("="))) replace s(r2 mn1 sd1 N pval) ///
    starlevels(* .1 ** .05 *** .01) legend

est clear

foreach var in girl515_school_2 boy515_school_2 girl515_workhrs_pc_2 boy515_workhrs_pc_2 girl1620_school_2 boy1620_school_2 ///
               women_emp_index_2 female_biz_pct_2 female_biz_new_2 social_index_2 {
    reg `var' treatment $area_controls [pweight=w2], cluster(areaid)
    eret2 scalar pval=2*ttail(e(df_r),abs(_b[treatment]/_se[treatment]))
    sum `var' if treatment==0 & e(sample)
    eret2 scalar mn2=r(mean)
    eret2 scalar sd2=r(sd)
    est store `var'
}

estout * using "table7.txt", drop($area_controls _cons) title("Table 7: Social Effects, Endline 2") ///
    prehead("" @title) cells(b(fmt(a3) s) se(fmt(a3) par("="))) append s(r2 mn2 sd2 N pval) ///
    starlevels(* .1 ** .05 *** .01) legend

import delimited using "table7.txt", delim(tab) clear
export delimited using "table7.csv", replace

*******************************************************
* EXTENSION 1: PERSISTENCE OF TREATMENT EFFECTS
*******************************************************
use "$datadir/endline12.dta", clear

reg bizprofit_1 treatment $area_controls [pweight=w1], cluster(areaid)
est store profit_EL1
reg bizprofit_2 treatment $area_controls [pweight=w2], cluster(areaid)
est store profit_EL2

esttab profit_EL1 profit_EL2 using "$outputdir/business_profit_time.csv", ///
    se label replace varlabels(treatment "Treatment" _cons "Constant") ///
    stats(N, labels("Observations")) star(* 0.05 ** 0.01 *** 0.001) csv

reg bizinvestment_1 treatment $area_controls [pweight=w1], cluster(areaid)
est store invest_EL1
reg bizinvestment_2 treatment $area_controls [pweight=w2], cluster(areaid)
est store invest_EL2

esttab invest_EL1 invest_EL2 using "$outputdir/business_investment_time.csv", ///
    se label replace varlabels(treatment "Treatment" _cons "Constant") ///
    stats(N, labels("Observations")) star(* 0.05 ** 0.01 *** 0.001) csv

reg bizassets_1 treatment $area_controls [pweight=w1], cluster(areaid)
est store assets_EL1
reg bizassets_2 treatment $area_controls [pweight=w2], cluster(areaid)
est store assets_EL2

esttab assets_EL1 assets_EL2 using "$outputdir/business_assets_time.csv", ///
    se label replace varlabels(treatment "Treatment" _cons "Constant") ///
    stats(N, labels("Observations")) star(* 0.05 ** 0.01 *** 0.001) csv

reg bizrev_1 treatment $area_controls [pweight=w1], cluster(areaid)
est store rev_EL1
reg bizrev_2 treatment $area_controls [pweight=w2], cluster(areaid)
est store rev_EL2

esttab rev_EL1 rev_EL2 using "$outputdir/business_revenue_time.csv", ///
    se label replace varlabels(treatment "Treatment" _cons "Constant") ///
    stats(N, labels("Observations")) star(* 0.05 ** 0.01 *** 0.001) csv

reg hours_week_1 treatment $area_controls [pweight=w1], cluster(areaid)
est store labor_EL1
reg hours_week_2 treatment $area_controls [pweight=w2], cluster(areaid)
est store labor_EL2

esttab labor_EL1 labor_EL2 using "$outputdir/labor_time.csv", ///
    se label replace varlabels(treatment "Treatment" _cons "Constant") ///
    stats(N, labels("Observations")) star(* 0.05 ** 0.01 *** 0.001) csv

esttab profit_EL1 profit_EL2 invest_EL1 invest_EL2 assets_EL1 assets_EL2 rev_EL1 rev_EL2 labor_EL1 labor_EL2 ///
    using "$outputdir/allreg_vcols.csv", se label replace ///
    mtitle(V1 V2 V3 V4 V5 V6 V7 V8 V9 V10) varlabels(treatment "Treatment" _cons "Constant") ///
    stats(N, labels("Observations")) star(* 0.05 ** 0.01 *** 0.001) csv

*******************************************************
* EXTENSION 2: FORMAL VS INFORMAL BORROWING
*******************************************************
use "$datadir/endline12.dta", clear

capture confirm variable formal_borrow_1
if _rc==0 replace formal_borrow_1 = (anymfi_1==1 | anybank_1==1)
if _rc!=0 gen formal_borrow_1 = (anymfi_1==1 | anybank_1==1)

capture confirm variable formal_borrow_2
if _rc==0 replace formal_borrow_2 = (anymfi_2==1 | anybank_2==1)
if _rc!=0 gen formal_borrow_2 = (anymfi_2==1 | anybank_2==1)

capture confirm variable informal_borrow_1
if _rc==0 replace informal_borrow_1 = anyinformal_1
if _rc!=0 gen informal_borrow_1 = anyinformal_1

capture confirm variable informal_borrow_2
if _rc==0 replace informal_borrow_2 = anyinformal_2
if _rc!=0 gen informal_borrow_2 = anyinformal_2

capture confirm variable d_formal_borrow
if _rc==0 replace d_formal_borrow = formal_borrow_2 - formal_borrow_1
if _rc!=0 gen d_formal_borrow = formal_borrow_2 - formal_borrow_1

capture confirm variable d_informal_borrow
if _rc==0 replace d_informal_borrow = informal_borrow_2 - informal_borrow_1
if _rc!=0 gen d_informal_borrow = informal_borrow_2 - informal_borrow_1

reg d_formal_borrow treatment $area_controls, cluster(areaid)
est store dFormal_OLS

reg d_informal_borrow treatment $area_controls, cluster(areaid)
est store dInformal_OLS

ivregress 2sls formal_borrow_2 (anymfi_2 = treatment) $area_controls [pweight=w2], cluster(areaid)
est store IV_Formal

ivregress 2sls informal_borrow_2 (anymfi_2 = treatment) $area_controls [pweight=w2], cluster(areaid)
est store IV_Informal

esttab dFormal_OLS dInformal_OLS IV_Formal IV_Informal using "$outputdir/borrow_panel.csv", ///
    se label replace mtitle(V1 V2 V3 V4) varlabels(treatment "Treatment" _cons "Constant") ///
    stats(N, labels("Observations")) star(* 0.05 ** 0.01 *** 0.001) noobs nonumber plain csv
