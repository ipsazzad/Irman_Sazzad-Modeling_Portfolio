clear all
set more off
capture log close
cd "E:\Study\Fall 25\Metrix\PS\5"

cap which ftools
if _rc ssc install ftools

cap which reghdfe
if _rc ssc install reghdfe

cap which esttab
if _rc ssc install estout

cap which ivreghdfe
if _rc ssc install ivreghdfe

cap which boottest
if _rc ssc install boottest

cap which ivreg2
if _rc ssc install ivreg2

cap which ranktest
if _rc ssc install ranktest

***************************************************************
***   QUESTION 1                                            ***
***************************************************************

********************************* Q1(b) *********************************

use "student_pres_data.dta", clear
merge m:1 pupilid using "student_test_data.dta", nogenerate keep(match)

reghdfe pres tracking bungoma girl bottomhalf, ///
    absorb(schoolid) vce(cluster lowstream)

esttab using "Q1b.tex", replace ///
    se label nonumber noobs ///
    title("Effect of Tracking on Student Presence")


********************************* Q1(c) *********************************

* Encode visit number if necessary
capture confirm variable visit_num
if _rc encode visit, gen(visit_num)

capture drop girl_bottomhalf
gen girl_bottomhalf = girl * bottomhalf

eststo clear

* Column 3
reghdfe pres tracking bungoma girl bottomhalf ///
        visit_num realdate, ///
        absorb(schoolid) vce(cluster lowstream)
eststo col3

* Column 4
reghdfe pres tracking bungoma girl bottomhalf std_mark ///
        visit_num realdate, ///
        absorb(schoolid) vce(cluster lowstream)
eststo col4

* Column 5
reghdfe pres tracking bungoma girl bottomhalf std_mark ///
        agetest visit_num realdate, ///
        absorb(schoolid) vce(cluster lowstream)
eststo col5

* Column 6
reghdfe pres tracking bungoma girl bottomhalf std_mark ///
        agetest girl_bottomhalf visit_num realdate, ///
        absorb(schoolid) vce(cluster lowstream)
eststo col6

esttab col3 col4 col5 col6 using "Q1c.tex", replace ///
    se label nonumber noobs ///
    b(3) star(* 0.10 ** 0.05 *** 0.01) ///
    title("Panel A: Reduced-form Regressions with Visit/Date Controls")


********************************* Q1(d) *********************************

use "student_test_data.dta", clear

keep if tracking == 0
replace agetest = r2_age - 1 if agetest == .

quietly summarize totalscore
scalar mean_tot = r(mean)
scalar sd_tot   = r(sd)

capture drop stdR_totalscore
generate stdR_totalscore = (totalscore - mean_tot) / sd_tot

eststo clear

* Panel A
reghdfe stdR_totalscore ///
    rMEANstream_std_baselinemark ///
    rSDstream_std_baselinemark ///
    std_mark girl agetest etpteacher, ///
    absorb(schoolid) vce(cluster schoolid)

eststo panelA
scalar RF_coef = _b[rMEANstream_std_baselinemark]

* Panel C (first stage)
reghdfe rMEANstream_std_total ///
    rMEANstream_std_baselinemark ///
    rSDstream_std_baselinemark ///
    std_mark girl agetest etpteacher, ///
    absorb(schoolid) vce(cluster schoolid)

eststo panelC
scalar FS_coef = _b[rMEANstream_std_baselinemark]
scalar FS_F    = e(F)

* Panel B (2SLS)
ivregress 2sls stdR_totalscore ///
    (rMEANstream_std_total = rMEANstream_std_baselinemark) ///
    rSDstream_std_baselinemark std_mark girl agetest etpteacher ///
    i.schoolid, cluster(schoolid)

eststo panelB
scalar IV_coef = _b[rMEANstream_std_total]

scalar ratio = RF_coef / FS_coef
display "IV coefficient     = " IV_coef
display "RF/FS coefficient  = " ratio

* Export
esttab panelA using "Q1dA.tex", replace ///
    se label noobs nonumber ///
    title("Panel A: Reduced-form Regression")

estadd scalar Fstat = FS_F
esttab panelC using "Q1dC.tex", replace ///
    se label noobs nonumber ///
    title("Panel C: First-stage Regression")

esttab panelB using "Q1dB.tex", replace ///
    se label noobs nonumber ///
    title("Panel B: IV Regression")


********************************* Q1(e) *********************************

capture drop iv_sample
gen byte iv_sample = e(sample)

reghdfe rMEANstream_std_total ///
    rMEANstream_std_baselinemark ///
    rSDstream_std_baselinemark std_mark girl agetest etpteacher ///
    if iv_sample, absorb(schoolid) vce(cluster schoolid) resid

capture drop vhat
predict vhat, resid

reghdfe stdR_totalscore ///
    rMEANstream_std_total vhat ///
    rSDstream_std_baselinemark std_mark girl agetest etpteacher ///
    if iv_sample, absorb(schoolid) vce(cluster schoolid)

scalar CF_coef = _b[rMEANstream_std_total]

display "2SLS IV coefficient (Panel B) = " IV_coef
display "OLS with residuals coef       = " CF_coef



***************************************************************
***   QUESTION 2                                            ***
***************************************************************

use original.dta, clear

foreach tt in 1 2 3 {
    capture drop m475t`tt'
    capture drop psu475t`tt'
    capture drop m475psut`tt'

    gen m475t`tt'    = (psut`tt'>=475) if psut`tt'!=.
    gen psu475t`tt'  = psut`tt' - 475 + 0.25
    gen m475psut`tt' = psu475t`tt' * m475t`tt'
}

capture drop psu475
gen psu475 = psut1 - 475

capture drop m475psu
gen m475psu = psu475 * m475t1

capture drop presel
gen presel = (1-(qqt1==. | qqt1==5))

capture drop m475presel
gen m475presel = presel * m475t1

capture drop psupresel
gen psupresel  = presel * psu475t1

capture drop m475psupresel
gen m475psupresel = presel * m475psut1

forvalues i =1(1)4 {
    capture drop psu475_p`i'
    capture drop m475psu_p`i'
    capture drop psupresel_p`i'
    capture drop m475psupresel_p`i'

    gen psu475_p`i'   = psu475t1^`i'
    gen m475psu_p`i'  = m475psut1^`i'
    gen psupresel_p`i'   = presel*psu475t1^`i'
    gen m475psupresel_p`i' = m475psupresel^`i'
}

capture drop everelig1
gen everelig1 = (psut1>=475 & qqt1<=4 & qqt1!=.) | ///
                (psut2>=475 & qqt2<=4 & qqt2!=.) | ///
                (psut3>=475 & qqt3<=4 & qqt3!=.)

capture drop yearcol1
gen yearcol1 = 0
foreach tt in 1 2 3 {
    replace yearcol1 = yearcol1 + 1 if enrolt`tt' == 1
}

capture drop everenroll1
gen everenroll1 = (yearcol1!=0)

capture drop ever2nd1
gen ever2nd1 = (yearcol1>=2) if year_pr!=2009

capture drop ever3rd1
gen ever3rd1 = (yearcol1==3) if year_pr==2007

foreach tt in 1 2 3 4 {
    capture drop takepsut`tt'
    gen takepsut`tt' = (psut`tt'!=.)
}

capture drop timespsu2
gen timespsu2 = 0
foreach t in 1 2 3 4 {
    replace timespsu2 = timespsu2 + 1 if psut`t' != .
}

capture drop everretakepsu2
gen everretakepsu2 = (timespsu2>1)

capture drop enrolfyt2
gen enrolfyt2 = (enrolt1==0 & enrolt2==1)

capture drop enrolfyt3
gen enrolfyt3 = (enrolt1==0 & enrolt2==0 & enrolt3==1)

save data_credit_access, replace


********************************* Q2(b) *********************************

use data_credit_access.dta, clear
eststo clear

reg enrolt1 m475t1 psu475 m475psu if qqt1<=4 & abs(psu475)<=44, r
eststo Enrolt1

reg enrolt1 m475t1 psu475 m475psu if presel==0 & abs(psu475)<=44, r
eststo Enrolt1_0

esttab using "table3.tex", replace ///
    b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
    stats(r2, fmt(2)) label ///
    title("Immediate College Enrollment") ///
    collabels(none) nonumbers nomtitles ///
    alignment(c) modelwidth(15) ///
    tex


********************************* Q2(c) *********************************

use data_credit_access.dta, clear
eststo clear

reg everelig1 m475t1 psu475 m475psu if qqt1<=4 & abs(psu475)<=44, r
eststo fs_ols

ivreg everenroll1 (everelig1 = m475t1) psu475 m475psu ///
    if qqt1<=4 & abs(psu475)<=44, r
eststo twosls

reg everenroll1 m475t1 psu475 m475psu ///
    if qqt1<=4 & abs(psu475)<=44, r
eststo reduced

esttab fs_ols twosls reduced using "Ques2(C).tex", replace ///
    b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
    stats(r2 N, fmt(2 0)) label ///
    title("Regression Results: FS OLS, 2SLS, and Reduced Models") ///
    collabels(none) nonumbers nomtitles ///
    alignment(c) modelwidth(15) ///
    tex


********************************* Q2(d) *********************************

tempname rdresults
postfile `rdresults' bw const beta se using rdresults.dta, replace

forvalues i = 2/80 {
    quietly reg enrolt1 m475t1 psu475 m475psu if qqt1<=4 & abs(psu475)<=`i', r
    post `rdresults' (`i') (`=_b[_cons]') (`=_b[m475t1]') (`=_se[m475t1]')
}

postclose `rdresults'

use rdresults.dta, clear

capture drop ci_hi
capture drop ci_lo

gen ci_hi = beta + 1.96 * se
gen ci_lo = beta - 1.96 * se

twoway (line ci_hi bw, lpattern(dash) lcolor(black)) ///
       (line ci_lo bw, lpattern(dash) lcolor(black)) ///
       (connect beta bw, msymbol(o) mcolor(black) msize(vsmall)) ///
       , xline(44, lpattern(solid) lcolor(black)) ///
         xscale(range(0 80)) xlabel(0(20)80) ///
         yscale(range(0 0.3)) ylabel(0(0.1)0.3) ///
         legend(order(3 1) rows(2) cols(1) ///
         label(3 "RD estimate") label(1 "95% CI")) ///
         scheme(s1mono) title("ITT results by bandwidth") ///
         xtitle("bandwidth") ytitle("estimated effect")

graph export GTitt_by_bandwidth.png, replace



***************************************************************
***   QUESTION 3                                            ***
***************************************************************

use "AJR2001.dta", clear
eststo clear

********************************* Q3(b) *********************************

reg loggdp risk
eststo model1
esttab model1 using "part1.tex", replace ///
    title("Part 1: OLS Regression of loggdp on risk")

quietly summarize risk
scalar meanrisk = r(mean)

quietly summarize loggdp
scalar meangdp = r(mean)

capture drop risk_demeaned
gen risk_demeaned   = risk - meanrisk

capture drop loggdp_demeaned
gen loggdp_demeaned = loggdp - meangdp

reg loggdp_demeaned risk_demeaned
eststo model2
esttab model2 using "part2.tex", replace ///
    title("Part 2: Demeaned Variables Regression")

correlate loggdp risk, covariance
scalar cov_yx = r(cov_12)
scalar var_x  = r(Var_2)
scalar beta_ratio = cov_yx / var_x

matrix results = (cov_yx \ var_x \ beta_ratio)
matrix rownames results = Covariance Variance Beta

esttab matrix(results) using "part3.tex", replace ///
    title("Part 3: Covariance and Variance Results") ///
    mtitles("Results")


********************************* Q3(c) *********************************

reg risk logmort0
eststo model3
esttab model3 using "part4.tex", replace ///
    title("Part 4: First Stage Regression: risk on logmort0")


********************************* Q3(d) *********************************

ivreg2 loggdp (risk = logmort0), first
scalar biv = _b[risk]

correlate loggdp logmort0, covariance
scalar cov_z_y = r(cov_12)

correlate risk logmort0, covariance
scalar cov_z_x = r(cov_12)

scalar ratio = cov_z_y / cov_z_x

display "IV coefficient from ivreg2:   " biv
display "Manual covariance ratio:      " ratio