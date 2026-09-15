clear
capture log close
set more off

* Set working directory and load dataset
cd "E:\Study\Fall 25\Metrix\PS\4"
use "mw2009_replication_file.dta", clear

** 4C

* Keep only relevant observations
keep if pres == 0
keep if bday < 22

* Generate eligibility indicator (1 if born on or after 20th)
gen age = 0 if bday < 20
replace age = 1 if bday >= 20

* Create Republican president dummy
gen reppres = 0
replace reppres = 1 if inlist(year, 1982, 1986, 1990)

* Generate state and year dummy variables
tab state, gen(stdum)
tab year, gen(ydum)

* Create interaction term: Eligibility × Party Alignment
gen age_party = age * party

* Create all *_p1 interaction terms for controls × president party
foreach var of varlist hsdiploma loginc work married urban northeast south northcen west male black hispanic asian native union homeowner {
    gen `var'_p1 = reppres * `var'
}

* Drop missing values for required controls
foreach var of varlist age party hsdiploma loginc work married urban male black hispanic asian native homeowner union {
    drop if `var' == .
}

* Run regression for Table 3 Top Panel
regress opinion stdum* ydum* ///
    hsdiploma hsdiploma_p1 loginc loginc_p1 work work_p1 married married_p1 ///
    urban urban_p1 northeast northeast_p1 south south_p1 northcen northcen_p1 ///
    west west_p1 male male_p1 black black_p1 hispanic hispanic_p1 asian asian_p1 ///
    native native_p1 union union_p1 homeowner homeowner_p1 ///
    age party age_party, robust

* Install outreg2
cap which outreg2
if _rc ssc install outreg2, replace	

* Rename
label variable opinion "Feelings toward President"
label variable age "Eligible to vote"
label variable party "Same party as president"
label variable age_party "Eligible * party"

	
* Export only the top panel variables to LaTeX table
outreg2 using "table3_top.tex", ///
    keep(age party age_party) ///
    title("Top Panel of Table 3") ///
    label tex bdec(4) se bracket nocons aster(3) replace
	
** 4D

* Heteroskedasticity Test
regress opinion age party age_party
hettest

** 4E

* Install estout

cap which esttab
if _rc ssc install estout, replace

* Reset estimates store
eststo clear

* Column 1: Original with robust (HC1) standard errors
regress opinion stdum* ydum* ///
    hsdiploma hsdiploma_p1 loginc loginc_p1 work work_p1 married married_p1 ///
    urban urban_p1 northeast northeast_p1 south south_p1 northcen northcen_p1 ///
    west west_p1 male male_p1 black black_p1 hispanic hispanic_p1 asian asian_p1 ///
    native native_p1 union union_p1 homeowner homeowner_p1 ///
    age party age_party, robust
eststo col1

* Column 2: HC3 standard errors using regress + vce(hc3)
regress opinion stdum* ydum* ///
    hsdiploma hsdiploma_p1 loginc loginc_p1 work work_p1 married married_p1 ///
    urban urban_p1 northeast northeast_p1 south south_p1 northcen northcen_p1 ///
    west west_p1 male male_p1 black black_p1 hispanic hispanic_p1 asian asian_p1 ///
    native native_p1 union union_p1 homeowner homeowner_p1 ///
    age party age_party, vce(hc3)
eststo col2

* Column 3: Clustered by state
regress opinion stdum* ydum* ///
    hsdiploma hsdiploma_p1 loginc loginc_p1 work work_p1 married married_p1 ///
    urban urban_p1 northeast northeast_p1 south south_p1 northcen northcen_p1 ///
    west west_p1 male male_p1 black black_p1 hispanic hispanic_p1 asian asian_p1 ///
    native native_p1 union union_p1 homeowner homeowner_p1 ///
    age party age_party, vce(cluster state)
eststo col3

* Column 4: Clustered by state-year (interaction)
capture drop state_year
egen state_year = group(state year)
regress opinion stdum* ydum* ///
    hsdiploma hsdiploma_p1 loginc loginc_p1 work work_p1 married married_p1 ///
    urban urban_p1 northeast northeast_p1 south south_p1 northcen northcen_p1 ///
    west west_p1 male male_p1 black black_p1 hispanic hispanic_p1 asian asian_p1 ///
    native native_p1 union union_p1 homeowner homeowner_p1 ///
    age party age_party, vce(cluster state_year)
eststo col4

* Export LaTeX table
esttab col1 col2 col3 col4 using "table3_robust.tex", ///
    keep(age party age_party) ///
    label tex se star(* 0.10 ** 0.05 *** 0.01) ///
    title("Alternative Standard Errors for Table 3 Top Panel") ///
    replace

clear
capture log close
set more off
cd "E:\Study\Fall 25\Metrix\PS\4"
use Florida.dta, clear

** 5Ci

* Apply sample restrictions
keep if forprofit == 1
keep if AAdeg == 1 | diploma == 1
keep if enrolled > 0 & tuition > 0 & length > 0
keep if tuition != . & chain != . & length != . & enrolled != .

* Apply paper-style labels
label variable title4 "Title IV"
label variable lnlength "ln(program length)"
label variable lnenrolled "ln(enrollment)"
label variable yrs_open "Years open"
label variable chain "Chain"

* Run regression (Table 7, Column 4)
xi: areg lntuit title4 lnlength lnenrolled yrs_open chain ///
    t405-t409 i.fy i.county, absorb(cipcode) cluster(schoolid)

* Store results
eststo col4

* Export clean LaTeX table
esttab col4 using t7c4.tex, replace se label ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    keep(title4 lnlength lnenrolled yrs_open chain) ///
    title("Replication of Table 7, Column 4") ///
    mtitles("Column 4") ///
    addnotes("Standard errors in parentheses, clustered by school.", ///
    "Sample includes for-profit AA and diploma programs in Florida.") ///
    compress

** 5Cii

* Define sample1 = programs with at least 900 hours
gen sample1 = (hours900 == 1)

* Run modified regression (Table 7, Column 4 specification restricted to sample1)
xi: areg lntuit title4 lnlength lnenrolled yrs_open chain ///
    t405-t409 i.fy i.county if sample1 == 1, absorb(cipcode) cluster(schoolid)

* Store modified model
eststo col4_sample1

* Export LaTeX table for sample1
esttab col4_sample1 using t7c4_sample1.tex, replace se label ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    keep(title4 lnlength lnenrolled yrs_open chain) ///
    title("Modified Table 7, Column 4 (Sample 1: 900+ hours)") ///
    mtitles("Sample 1") ///
    addnotes("Standard errors in parentheses, clustered by school.", ///
    "Sample includes for-profit AA and diploma programs in Florida with 900+ clock hours.") ///
    compress

** 5Ciii
* Encode string variables for use as factor variables
capture confirm variable fy_num
if _rc encode fy, gen(fy_num)

capture confirm variable county_num
if _rc encode county, gen(county_num)

* OLS regression (Table 7, Col 4 specification)
reg lntuit title4 lnlength lnenrolled yrs_open chain t405-t409 i.fy_num i.county_num
eststo ols_model

* Save residuals and squared residuals
predict e, resid
gen e2 = e^2
gen inv_enrolled = 1 / enrolled

* Breusch–Pagan regression: squared residuals on inverse enrolled
reg e2 inv_enrolled
eststo bp_model

* Mean residual by school
egen mean_resid_school = mean(e), by(schoolid)

* Between-school variance (σ²_c)
summarize mean_resid_school, meanonly
scalar overall_mean = r(mean)
gen var_cluster = (mean_resid_school - overall_mean)^2
summarize var_cluster, meanonly
scalar sigma2_c = r(mean)

* Within-school variance (σ²_u)
gen e_dev = e - mean_resid_school
gen var_within = e_dev^2
summarize var_within, meanonly
scalar sigma2_u = r(mean)

display "σ²_c = " sigma2_c
display "σ²_u = " sigma2_u

* Weighted Least Squares regression (weighted by enrolled)
reg lntuit title4 lnlength lnenrolled yrs_open chain t405-t409 i.fy_num i.county_num [aweight=enrolled]
eststo wls_model

* Export combined LaTeX table (σ² values excluded from table)
esttab ols_model bp_model wls_model using 5Ciii.tex, replace se label ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    keep(title4 lnlength lnenrolled yrs_open chain) ///
    stats(N r2, fmt(0 3) labels("Observations" "R-squared")) ///
    title("Table 5: Log Tuition Differences between Title IV and non–Title IV Institutions") ///
    mtitles("OLS Regression" "Breusch-Pagan Test" "WLS Regression") ///
    addnotes("Standard errors in parentheses.", "*** p<0.01, ** p<0.05, * p<0.1") ///
    compress

* Print σ²_c and σ²_u in log so you can report them in write-up
di as text "Note: σ²_c = " %9.3f sigma2_c "   σ²_u = " %9.3f sigma2_u


** 5CIV

xi: areg lntuit title4 lnlength lnenrolled yrs_open chain ///
    t405-t409 i.fy i.county, absorb(cipcode) cluster(cipcode)

eststo col4_cip
esttab col4_cip using t7c4_cip.tex, replace se label ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    keep(title4 lnlength lnenrolled yrs_open chain) ///
    title("Replication of Table 7, Column 4 (Clustered by CIP)") ///
    mtitles("CIP-level clustering") ///
    addnotes("Standard errors clustered by CIP code.", ///
    "Sample includes for-profit AA and diploma programs in Florida.") ///
    compress

clear
capture log close
set more off
cd "E:\Study\Fall 25\Metrix\PS\4"
use "Dizon-Ross-2019-replication.dta", clear

** 6Ai
keep if inlist(std, 2, 3)

* Confirm sample size = 2543
count
assert r(N) == 2543

* Rename perf_u_ave to score
gen score = perf_u_ave

* Create interaction term
gen treat_score = treat * score

* Control variables from Table 1, Panel B
local controls "tot_kids one_par educ_ave any_secondary"

* Run regressions
eststo clear

eststo default_se: reg u_ave score treat treat_score `controls'
eststo hc1_se: reg u_ave score treat treat_score `controls', vce(robust)
eststo cluster_se: reg u_ave score treat treat_score `controls', vce(cluster hhid)

* Export table in LaTeX
esttab default_se hc1_se cluster_se using "6i.tex", replace ///
    se star(* 0.10 ** 0.05 *** 0.01) label ///
    title("Information Treatment Effects on The Slope of Investments (Clustered by hhid)") ///
    align(D{.}{.}{-1}) ///
    b(3) se(3) ///
    compress ///
    keep(treat_score score treat _cons) ///
    collabels("Homoskedasticity" "HC1 SE" "Clustered SE") ///
    addnotes("Standard errors in parentheses", ///
             "* p < 0.10, ** p < 0.05, *** p < 0.01") ///
    nonum
	
** 6Aii

* Homoskedasticity
est restore default_se
scalar b_homo = _b[treat_score]
scalar se_homo = _se[treat_score]
scalar t_homo = b_homo / se_homo
di "T-stat (Homoskedasticity): " t_homo
assert abs(t_homo - _b[treat_score]/_se[treat_score]) < 1e-6

* HC1 robust
est restore hc1_se
scalar b_hc1 = _b[treat_score]
scalar se_hc1 = _se[treat_score]
scalar t_hc1 = b_hc1 / se_hc1
di "T-stat (HC1 robust): " t_hc1
assert abs(t_hc1 - _b[treat_score]/_se[treat_score]) < 1e-6

* Clustered SE by hhid
est restore cluster_se
scalar b_cl = _b[treat_score]
scalar se_cl = _se[treat_score]
scalar t_cl = b_cl / se_cl
di "T-stat (Clustered SE): " t_cl
assert abs(t_cl - _b[treat_score]/_se[treat_score]) < 1e-6

** 6Aiii
* Define critical value for 95% CI
scalar alpha = 0.05

* Default (homoskedasticity)
est restore default_se
scalar df_homo = e(df_r)  
scalar tcrit_homo = invttail(df_homo, alpha/2)
scalar lb_homo = _b[treat_score] - tcrit_homo * _se[treat_score]
scalar ub_homo = _b[treat_score] + tcrit_homo * _se[treat_score]
di "95% CI (Default SE): [" lb_homo ", " ub_homo "]"

* HC1
est restore hc1_se
scalar df_hc1 = e(df_r)  
scalar tcrit_hc1 = invttail(df_hc1, alpha/2)
scalar lb_hc1 = _b[treat_score] - tcrit_hc1 * _se[treat_score]
scalar ub_hc1 = _b[treat_score] + tcrit_hc1 * _se[treat_score]
di "95% CI (HC1 robust): [" lb_hc1 ", " ub_hc1 "]"

* Clustered SE
est restore cluster_se
gen cluster = hhid
quietly levelsof hhid, local(hhlist)
scalar df_cl = `: word count `hhlist'' - 1
scalar tcrit_cl = invttail(df_cl, alpha/2)
scalar lb_cl = _b[treat_score] - tcrit_cl * _se[treat_score]
scalar ub_cl = _b[treat_score] + tcrit_cl * _se[treat_score]
di "95% CI (Clustered SE): [" lb_cl ", " ub_cl "]"	

** 6Aiv

* Default (homoskedasticity)
est restore default_se
test treat_score = 0
di "Default SE p-value: " r(p)

* HC1 robust
est restore hc1_se
test treat_score = 0
di "HC1 robust p-value: " r(p)

* Clustered SE
est restore cluster_se
test treat_score = 0
di "Clustered SE p-value: " r(p)


** 6B
scalar H0val = 0.75
scalar alpha = 0.05

* Default SE
est restore default_se
scalar b_hat = _b[treat_score]
scalar se = _se[treat_score]
scalar df = e(df_r)
scalar t_stat = (b_hat - H0val) / se
scalar p_val = 2 * ttail(df, abs(t_stat))
di "Default SE:"
di "T = " t_stat
di "p = " p_val
test treat_score = 0.75

* HC1
est restore hc1_se
scalar b_hat = _b[treat_score]
scalar se = _se[treat_score]
scalar df = e(df_r)
scalar t_stat = (b_hat - H0val) / se
scalar p_val = 2 * ttail(df, abs(t_stat))
di "HC1 SE:"
di "T = " t_stat
di "p = " p_val
test treat_score = 0.75

* Clustered SE
est restore cluster_se

quietly levelsof hhid, local(hhlist)
local nclust : word count `hhlist'
scalar df = `nclust' - 1

scalar b_hat = _b[treat_score]
scalar se = _se[treat_score]
scalar t_stat = (b_hat - H0val) / se
scalar p_val = 2 * ttail(df, abs(t_stat))
di "Clustered SE:"
di "T = " t_stat
di "p = " p_val

test treat_score = 0.75

** 6Ci

* Reload fresh data
cd "E:\Study\Fall 25\Metrix\PS\4"
use "Dizon-Ross-2019-replication.dta", clear

* Filter grades 2–5
keep if inlist(std, 2, 3, 4, 5)

* Confirm correct sample size
count
assert r(N) == 4480

* Generate variables
gen score = perf_u_ave
gen treat_score = treat * score

* Controls from earlier
local controls "tot_kids one_par educ_ave any_secondary"

* Run regression
eststo clear
eststo c1: reg u_ave score treat treat_score `controls'

* Export LaTeX table
esttab c1 using "6Ci.tex", replace ///
    se star(* 0.10 ** 0.05 *** 0.01) label ///
    title("Regression for Grades 2--5 with Default Standard Errors") ///
    align(D{.}{.}{-1}) ///
    b(3) se(3) ///
    compress ///
    keep(treat_score score treat _cons) ///
    collabels("Grades 2--5") ///
    addnotes("Standard errors in parentheses", ///
             "* p < 0.10, ** p < 0.05, *** p < 0.01") ///
    nonum
	
** 6Cii
* Reload fresh data
cd "E:\Study\Fall 25\Metrix\PS\4"
use "Dizon-Ross-2019-replication.dta", clear

* Generate needed variables
gen score = perf_u_ave
gen treat_score = treat * score

* Define controls
local controls "tot_kids one_par educ_ave any_secondary"

* Grades 2–3 sample
preserve
keep if inlist(std, 2, 3)
eststo g23: reg u_ave score treat treat_score `controls'
restore

* Grades 4–5 sample
preserve
keep if inlist(std, 4, 5)
eststo g45: reg u_ave score treat treat_score `controls'
restore

* Export LaTeX table with Columns 3–4
esttab g23 g45 using "6Cii.tex", replace ///
    se star(* 0.10 ** 0.05 *** 0.01) label ///
    title("Regression by Grade Group (2--3 vs. 4--5)") ///
    align(D{.}{.}{-1}) ///
    b(3) se(3) ///
    compress ///
    keep(treat_score score treat _cons) ///
    collabels("Grades 2--3" "Grades 4--5") ///
    addnotes("Standard errors in parentheses", ///
             "* p < 0.10, ** p < 0.05, *** p < 0.01") ///
    nonum
	
** 6Ciii
cd "E:\Study\Fall 25\Metrix\PS\4"
use "Dizon-Ross-2019-replication.dta", clear

* Generate variables
gen score = perf_u_ave
gen treat_score = treat * score
local controls "tot_kids one_par educ_ave any_secondary"

* Estimate model for grades 2–3
gen group = .
replace group = 1 if inlist(std, 2, 3)
reg u_ave score treat treat_score `controls' if group == 1
estimates store g23

* Estimate model for grades 4–5
replace group = 2 if inlist(std, 4, 5)
reg u_ave score treat treat_score `controls' if group == 2
estimates store g45

* Seemingly unrelated estimation to allow cross-model test
suest g23 g45

* Test if treat_score coefficient differs
test [g23_mean]treat_score = [g45_mean]treat_score

** 6D
cd "E:\Study\Fall 25\Metrix\PS\4"
use "Dizon-Ross-2019-replication.dta", clear

* Keep grades 2 to 5
keep if inlist(std, 2, 3, 4, 5)

* Generate variables
gen score = perf_u_ave
gen treat_score = treat * score

* Define controls
local controls "tot_kids one_par educ_ave any_secondary"

* Run the 6Ci regression again
reg u_ave score treat treat_score tot_kids one_par educ_ave any_secondary

* Test individual coefficients
test tot_kids
test treat_score
test treat
test score

* Joint test of all controls and treatment variables
test treat_score treat score tot_kids one_par educ_ave any_secondary


















































