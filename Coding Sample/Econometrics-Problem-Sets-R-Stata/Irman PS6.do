*******************************************************
* Problem Set 6 – Cleaned Author Code
*******************************************************

clear all
set more off
capture log close

cd "E:\Study\Fall 25\Metrix\PS\6"

cap which esttab
if _rc ssc install estout

cap which rwolf2
if _rc ssc install rwolf2

global path1 ".\Tables"
global path2 ".\Figures"

use loanmain, clear


*******************************************************
***   BASELINE BALANCE – TABLE 1                    ***
*******************************************************

*********** Panel A ***********
est clear
foreach x in firmage retail labor total_profit part5revenue {
    eststo: reg `x' T50 C50 T80 C80 if round==1, robust cluster(survey_town)
}

esttab, se
local models : coleq r(coefs)
matrix A = (2\3\4\5\1)
matrix B = r(coefs), A
mata st_matrix("C", sort(st_matrix("B"), 16))
matrix C = C[1...,1..15]
matrix rownames C = _cons T50 C50 T80 C80
matrix colnames C = `models'
eststo clear

local rnames : rownames C
local models : list uniq models
local i 0

foreach name of local rnames {
    local ++i
    local j 0
    capture matrix drop b
    capture matrix drop se
    foreach model of local models {
        local ++j
        matrix tmp = C[`i', 3*`j'-2]
        if tmp[1,1] < . {
            matrix colnames tmp = `model'
            matrix b = nullmat(b), tmp
            matrix tmp[1,1] = C[`i', 3*`j'-1]
            matrix se = nullmat(se), tmp
        }
    }
    ereturn post b
    quietly estadd matrix se
    eststo `name'
}

#delimit ;
esttab using "$path1\Table1.tex", replace f compress b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    nogaps booktabs nonote noobs nomtitles collabels(none) nonumbers noeqli nolines ///
    coeflabels(est1 "Firm age" est2 "Sector-Retail(\%)" est3 "Number of Employees" ///
               est4 "Profit(10,000 RMB)" est5 "Sales(10,000 RMB)") ///
    prehead("\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi} \begin{tabular*}{1.1\textwidth}{@{\extracolsep{\fill}}lccccc@{}} \hline \hline \\ \textit{Sample: All Baseline, 3,173 Firms} & \shortstack{Pure\\Control} & \shortstack{$\Delta$ Treated\\50\% Market} & \shortstack{$\Delta$ Untreated\\50\% Market} & \shortstack{$\Delta$ Treated\\80\% Market} & \shortstack{$\Delta$ Untreated\\80\% Market} \\ \hline \multicolumn{@span}{l}{\underline{\textit{\textbf{Panel A: Firm Characteristics}}}} \\")
;
#delimit cr


*********** Panel B ***********
est clear
foreach x in gender age education_college polconnection {
    eststo: reg `x' T50 C50 T80 C80 if round==1, robust cluster(survey_town)
}

esttab, se
local models : coleq r(coefs)
matrix A = (2\3\4\5\1)
matrix B = r(coefs), A
mata st_matrix("C", sort(st_matrix("B"), 13))
matrix C = C[1...,1..12]
matrix rownames C = _cons T50 C50 T80 C80
matrix colnames C = `models'
eststo clear

local rnames : rownames C
local models : list uniq models
local i 0

foreach name of local rnames {
    local ++i
    local j 0
    capture matrix drop b
    capture matrix drop se
    foreach model of local models {
        local ++j
        matrix tmp = C[`i', 3*`j'-2]
        if tmp[1,1] < . {
            matrix colnames tmp = `model'
            matrix b = nullmat(b), tmp
            matrix tmp[1,1] = C[`i', 3*`j'-1]
            matrix se = nullmat(se), tmp
        }
    }
    ereturn post b
    quietly estadd matrix se
    eststo `name'
}

#delimit ;
esttab using "$path1\Table1.tex", append f compress b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    nogaps booktabs nonote noobs nomtitles collabels(none) nonumbers noeqli nolines ///
    coeflabels(est1 "Gender (1=Male, 0=Female)" est2 "Age" est3 "Education-College" ///
               est4 "Political Connection (1=Yes, 0=No)") ///
    prehead("\multicolumn{@span}{l}{\underline{\textit{\textbf{Panel B: Managerial Characteristics}}}} \\")
;
#delimit cr


*********** Panel C ***********
est clear
eststo: reg bankloan T50 C50 T80 C80 if round==1, robust cluster(survey_town)
foreach x in bankloan_amount bankloan_interest {
    eststo: reg `x' T50 C50 T80 C80 if round==1 & bankloan==1, robust cluster(survey_town)
}

esttab, se
local models : coleq r(coefs)
matrix A = (2\3\4\5\1)
matrix B = r(coefs), A
mata st_matrix("C", sort(st_matrix("B"), 10))
matrix C = C[1...,1..9]
matrix rownames C = _cons T50 C50 T80 C80
matrix colnames C = `models'
eststo clear

local rnames : rownames C
local models : list uniq models
local i 0

foreach name of local rnames {
    local ++i
    local j 0
    capture matrix drop b
    capture matrix drop se
    foreach model of local models {
        local ++j
        matrix tmp = C[`i', 3*`j'-2]
        if tmp[1,1] < . {
            matrix colnames tmp = `model'
            matrix b = nullmat(b), tmp
            matrix tmp[1,1] = C[`i', 3*`j'-1]
            matrix se = nullmat(se), tmp
        }
    }
    ereturn post b
    quietly estadd matrix se
    eststo `name'
}

#delimit ;
esttab using "$path1\Table1.tex", append f compress b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    nogaps booktabs nonote noobs nomtitles collabels(none) nonumbers noeqli nolines ///
    coeflabels(est1 "Other Bank Loan (1=Yes, 0=No)" est2 "Loan Size (10,000 RMB)" ///
               est3 "Monthly Interest Rate($\permil$)") ///
    prehead("\multicolumn{@span}{l}{\underline{\textit{\textbf{Panel C: Borrowing}}}} \\")
;
#delimit cr


*********** Panel D ***********
est clear
foreach x in num_clients num_supplier {
    eststo: reg `x' T50 C50 T80 C80 if round==1, robust cluster(survey_town)
}

esttab, se
local models : coleq r(coefs)
matrix A = (2\3\4\5\1)
matrix B = r(coefs), A
mata st_matrix("C", sort(st_matrix("B"), 7))
matrix C = C[1...,1..6]
matrix rownames C = _cons T50 C50 T80 C80
matrix colnames C = `models'
eststo clear

local rnames : rownames C
local models : list uniq models
local i 0

foreach name of local rnames {
    local ++i
    local j 0
    capture matrix drop b
    capture matrix drop se
    foreach model of local models {
        local ++j
        matrix tmp = C[`i', 3*`j'-2]
        if tmp[1,1] < . {
            matrix colnames tmp = `model'
            matrix b = nullmat(b), tmp
            matrix tmp[1,1] = C[`i', 3*`j'-1]
            matrix se = nullmat(se), tmp
        }
    }
    ereturn post b
    quietly estadd matrix se
    eststo `name'
}

#delimit ;
esttab using "$path1\Table1.tex", append f compress b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    nogaps booktabs nonote noobs nomtitles collabels(none) nonumbers noeqli nolines ///
    coeflabels(est1 "Number of Clients" est2 "Number of Suppliers") ///
    prehead("\multicolumn{@span}{l}{\underline{\textit{\textbf{Panel D: Partnerships}}}} \\")
;
#delimit cr


*********** Panel E ***********
est clear
foreach x in flagatt shutdownall {
    eststo: reg `x' T50 C50 T80 C80 if round==1, robust cluster(survey_town)
}

esttab, se
local models : coleq r(coefs)
matrix A = (2\3\4\5\1)
matrix B = r(coefs), A
mata st_matrix("C", sort(st_matrix("B"), 7))
matrix C = C[1...,1..6]
matrix rownames C = _cons T50 C50 T80 C80
matrix colnames C = `models'
eststo clear

local rnames : rownames C
local models : list uniq models
local i 0

foreach name of local rnames {
    local ++i
    local j 0
    capture matrix drop b
    capture matrix drop se
    foreach model of local models {
        local ++j
        matrix tmp = C[`i', 3*`j'-2]
        if tmp[1,1] < . {
            matrix colnames tmp = `model'
            matrix b = nullmat(b), tmp
            matrix tmp[1,1] = C[`i', 3*`j'-1]
            matrix se = nullmat(se), tmp
        }
    }
    ereturn post b
    quietly estadd matrix se
    eststo `name'
}

#delimit ;
esttab using "$path1\Table1.tex", append f compress b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    nogaps booktabs nonote noobs nomtitles collabels(none) nonumbers noeqli nolines ///
    coeflabels(est1 "Attrition" est2 "Shutdown") ///
    prehead("\multicolumn{@span}{l}{\underline{\textit{\textbf{Panel E: Attrition and Shutdown (Endline)}}}} \\")
;
#delimit cr


*********** Bottom rows ***********
est clear
foreach x in T50 C50 T80 C80 {
    reg `x' firmage retail labor total_profit part5revenue gender age ///
        education_college polconnection bankloan num_clients num_supplier ///
        if round==1, robust cluster(survey_town)
    test firmage retail labor total_profit part5revenue gender age ///
         education_college polconnection bankloan num_clients num_supplier
    local pval_`x' = r(p)
}

matrix pval = [-999, `pval_T50', `pval_C50', `pval_T80', `pval_C80']
matrix rownames pval = P_val

#delimit ;
esttab m(pval, fmt(%9.3f)) using "$path1\Table1.tex", append f compress ///
    nogaps booktabs nonote noobs nomtitles collabels(none) nonumbers ///
    coeflabels(P_val "P-val of Joint Significance of Vars in Panels A-D") ///
    substitute("-999.000" " ")
;
#delimit cr

est clear
count if round==1 & T50==0 & C50==0 & T80==0 & C80==0
local obs_c = r(N)
foreach x in T50 C50 T80 C80 {
    count if `x'==1 & round==1
    local obs_`x' = r(N)
}
matrix obs = [`obs_c', `obs_T50', `obs_C50', `obs_T80', `obs_C80']
matrix rownames obs = N

#delimit ;
esttab m(obs, fmt(%9.0gc)) using "$path1\Table1.tex", append f compress ///
    nogaps booktabs nonote noobs nomtitles collabels(none) nonumbers nolines ///
    coeflabels(N "Observations") ///
    postfoot("\hline\hline \end{tabular*}")
;
#delimit cr


*******************************************************
***   APPENDIX TABLES A1, A2, TEXT DESCRIPTIONS     ***
*******************************************************

* Table A1
capture drop marketsize
bysort survey_town round: gen marketsize = _N

capture drop avgnumlabor
bysort survey_town: egen avgnumlabor = mean(labor) if round==1
tabstat avgnumlabor if round==1, by(marketcategory)

preserve
keep survey_town marketsize avgnumlabor marketcategory round
keep if round==1
duplicates drop
describe, short
bysort marketcategory: egen avgnumfirm = mean(marketsize)
tabstat avgnumfirm, by(marketcategory)
restore


* Table A2
tabstat part5revenue total_profit labor num_clients if shutdownall==0 & flagatt==0 & round==1 & survey_town_type==0
tabstat part5revenue total_profit labor num_clients if endatt==0 & round==1 & survey_town_type==0

reg part5revenue T50 C50 T80 C80 if shutdownall==0 & flagatt==0 & round==1, robust cluster(survey_town)
reg total_profit T50 C50 T80 C80 if shutdownall==0 & flagatt==0 & round==1, robust cluster(survey_town)
reg labor T50 C50 T80 C80 if shutdownall==0 & flagatt==0 & round==1, robust cluster(survey_town)
reg num_clients T50 C50 T80 C80 if shutdownall==0 & flagatt==0 & round==1, robust cluster(survey_town)

reg part5revenue T50 C50 T80 C80 if endatt==0 & round==1, robust cluster(survey_town)
reg total_profit T50 C50 T80 C80 if endatt==0 & round==1, robust cluster(survey_town)
reg labor T50 C50 T80 C80 if endatt==0 & round==1, robust cluster(survey_town)
reg num_clients T50 C50 T80 C80 if endatt==0 & round==1, robust cluster(survey_town)

* Table A2, joint significance 
reg T50 firmage retail labor total_profit part5revenue gender age education_college polconnection bankloan num_clients num_supplier if shutdownall==0 & flagatt==0 & round==1, robust cluster(survey_town)
test firmage retail labor total_profit part5revenue gender age education_college polconnection bankloan num_clients num_supplier

reg C50 firmage retail labor total_profit part5revenue gender age education_college polconnection bankloan num_clients num_supplier if shutdownall==0 & flagatt==0 & round==1, robust cluster(survey_town)
test firmage retail labor total_profit part5revenue gender age education_college polconnection bankloan num_clients num_supplier

reg T80 firmage retail labor total_profit part5revenue gender age education_college polconnection bankloan num_clients num_supplier if shutdownall==0 & flagatt==0 & round==1, robust cluster(survey_town)
test firmage retail labor total_profit part5revenue gender age education_college polconnection bankloan num_clients num_supplier

reg C80 firmage retail labor total_profit part5revenue gender age education_college polconnection bankloan num_clients num_supplier if shutdownall==0 & flagatt==0 & round==1, robust cluster(survey_town)
test firmage retail labor total_profit part5revenue gender age education_college polconnection bankloan num_clients num_supplier

reg T50 firmage retail labor total_profit part5revenue gender age education_college polconnection bankloan num_clients num_supplier if endatt==0 & round==1, robust cluster(survey_town)
test firmage retail labor total_profit part5revenue gender age education_college polconnection bankloan num_clients num_supplier

reg C50 firmage retail labor total_profit part5revenue gender age education_college polconnection bankloan num_clients num_supplier if endatt==0 & round==1, robust cluster(survey_town)
test firmage retail labor total_profit part5revenue gender age education_college polconnection bankloan num_clients num_supplier

reg T80 firmage retail labor total_profit part5revenue gender age education_college polconnection bankloan num_clients num_supplier if endatt==0 & round==1, robust cluster(survey_town)
test firmage retail labor total_profit part5revenue gender age education_college polconnection bankloan num_clients num_supplier

reg C80 firmage retail labor total_profit part5revenue gender age education_college polconnection bankloan num_clients num_supplier if endatt==0 & round==1, robust cluster(survey_town)
test firmage retail labor total_profit part5revenue gender age education_college polconnection bankloan num_clients num_supplier


* Table Text
capture drop townsize
bysort survey_town round: gen townsize = _N

capture drop indsize
bysort survey_town industry round: gen indsize = _N

capture drop indsalessum
bysort industry survey_town round: egen indsalessum = sum(part5revenue)

capture drop marketshare
gen marketshare = part5revenue / (2*indsalessum)

capture drop marketshare2
gen marketshare2 = marketshare^2

capture drop herf
bysort survey_town industry round: egen herf = sum(marketshare2)
replace herf = herf*2

capture drop invherf
gen invherf = 1/herf

ttest townsize if round==1, by(type)

preserve
keep survey_town industry townsize indsize invherf survey_town_type round
duplicates drop
describe, short

capture drop numind
bysort survey_town round: gen numind = _N

capture drop type
gen type = 0
replace type = 1 if survey_town_type ~= 0

summarize invherf, detail
ttest townsize if round==1, by(type)
ttest numind if round==1, by(type)
ttest indsize if round==1, by(type)
ttest invherf if round==1, by(type)
restore


*******************************************************
***   TABLE 2 – LOAN OUTCOMES                        ***
*******************************************************

capture drop sumtreated
bysort survey_town round: egen sumtreated = total(type)

capture drop treatratio_peer
gen treatratio_peer = (sumtreated - type) / (townsize - 1)

capture drop inter_untpeer
gen inter_untpeer = treatratio_peer * (1 - type)

tabstat newloan otherloan_end allloan_end if round==3 & survey_town_type==0

est clear
eststo: reg newloan type if round==3, robust cluster(survey_town)
quietly summarize newloan if round==3 & survey_town_type==0
estadd scalar Mean = r(mean)

eststo: reg newloan type inter_untpeer if round==3, robust cluster(survey_town)
quietly summarize newloan if round==3 & survey_town_type==0
estadd scalar Mean = r(mean)

eststo: reg newloan T50 T80 C50 C80 if round==3, robust cluster(survey_town)
quietly summarize newloan if round==3 & survey_town_type==0
estadd scalar Mean = r(mean)

eststo: reg otherloan_end type inter_untpeer if round==3, robust cluster(survey_town)
quietly summarize otherloan_end if round==3 & survey_town_type==0
estadd scalar Mean = r(mean)

eststo: reg otherloan_end T50 T80 C50 C80 if round==3, robust cluster(survey_town)
quietly summarize otherloan_end if round==3 & survey_town_type==0
estadd scalar Mean = r(mean)

eststo: reg allloan_end type inter_untpeer if round==3, robust cluster(survey_town)
quietly summarize allloan_end if round==3 & survey_town_type==0
estadd scalar Mean = r(mean)

eststo: reg allloan_end T50 T80 C50 C80 if round==3, robust cluster(survey_town)
quietly summarize allloan_end if round==3 & survey_town_type==0
estadd scalar Mean = r(mean)

#delimit ;
esttab using "$path1\Table2.tex", replace f ///
    compress keep(type inter_untpeer T50 T80 C50 C80 _cons) b(3) se(3) ///
    star(* 0.10 ** 0.05 *** 0.01) nogaps noeqli booktabs nonote noobs ///
    collabels(none) nomtitles ///
    coeflabels(type "Treated" ///
               inter_untpeer "\shortstack[l]{Untreated*Share of Peers\\Treated}" ///
               T50 "Treated*50\% Market" T80 "Treated*80\% Market" ///
               C50 "Untreated*50\% Market" C80 "Untreated*80\% Market" ///
               _cons "Constant") ///
    mgroups("{Borrow with New Loan Product}" "{\shortstack{Borrow from Other\\Sources}}" "{\shortstack{Borrow from Any\\Source}}", ///
            pattern(1 0 0 1 0 1 0) prefix(\multicolumn{@span}{c}) span ///
            erepeat(\cmidrule(lr){@span})) alignment(D{.}{.}{-1}) ///
    prehead("\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi} \begin{tabular}{p{0.3\textwidth}*{@span}{c}} \hline\hline \\ Dep. Var.: ") ///
    s(Mean N, fmt(%9.3f %9.0gc) labels("Mean of Pure Control" "Observations")) ///
    postfoot("\hline\hline \end{tabular}")
;
#delimit cr	


*******************************************************
***   APPENDIX TABLE A4                             ***
*******************************************************

tabstat RCC_amount otherloan_amount aallloan_end if round==3 & survey_town_type==0

reg RCC_amount type if round==3, robust cluster(survey_town)
reg RCC_amount type inter_untpeer if round==3, robust cluster(survey_town)
reg RCC_amount T50 T80 C50 C80 if round==3, robust cluster(survey_town)

reg otherloan_amount type inter_untpeer if round==3, robust cluster(survey_town)
reg otherloan_amount T50 T80 C50 C80 if round==3, robust cluster(survey_town)

reg aallloan_end type inter_untpeer if round==3, robust cluster(survey_town)
reg aallloan_end T50 T80 C50 C80 if round==3, robust cluster(survey_town)


*******************************************************
***   FIGURE 2                                     ***
*******************************************************

twoway (kdensity lnpart5revenue if type==1 & round==1, ///
           xtitle("log Sales") ytitle("Density") xlabel(,nogrid) ylabel(,nogrid) clpattern("solid")) ///
       (kdensity lnpart5revenue if type==0 & survey_town_type~=0 & round==1, clpattern("longdash")) ///
       (kdensity lnpart5revenue if type==0 & survey_town_type==0 & round==1, clpattern("shortdash")), ///
       legend(order(1 "Treated" 2 "Untreated in Treated Markets" 3 "Untreated in Control Markets") ///
              cols(2) pos(6) region(lcolor(black)))
graph save "$path2\Figure2_A.gph", replace

* Correct growth definition (lag within firm)
capture drop growthsales
bysort firmid (round): gen growthsales = lnpart5revenue - lnpart5revenue[_n-1] if round > 1

capture drop avggrowthsales
bysort firmid: egen avggrowthsales = mean(growthsales) if round > 1

twoway (kdensity avggrowthsales if type==1 & round>1, ///
           xtitle("log Sales Growth") ytitle("Density") xlabel(,nogrid) ylabel(,nogrid) clpattern("solid")) ///
       (kdensity avggrowthsales if type==0 & survey_town_type~=0 & round>1, clpattern("longdash")) ///
       (kdensity avggrowthsales if type==0 & survey_town_type==0 & round>1, clpattern("shortdash")), ///
       legend(order(1 "Treated" 2 "Untreated in Treated Markets" 3 "Untreated in Control Markets") ///
              cols(2) pos(6) region(lcolor(black)))
graph save "$path2\Figure2_B.gph", replace

graph combine "$path2\Figure2_A.gph" "$path2\Figure2_B.gph", row(1) ///
    plotregion(fcolor(white)) graphregion(fcolor(white)) ysize(5) xsize(12)
graph save "$path2\Figure2.gph", replace
graph export "$path2\Figure2.pdf", as(pdf) replace
capture erase "$path2\Figure2_A.gph"
capture erase "$path2\Figure2_B.gph"


*******************************************************
***   MAIN SPECIFICATION – TABLE 3                 ***
*******************************************************

capture drop interpost
gen interpost = post * type

capture drop inter4post
gen inter4post = post * treatratio_comp

capture drop interpost_comp1
gen interpost_comp1 = post * treatratio_comp * type

capture drop interpost_comp2
gen interpost_comp2 = post * treatratio_comp * (1 - type)

forvalues i = 1/8 {
    capture drop category`i'
    gen category`i' = 0
    replace category`i' = 1 if category == `i'
}
drop category

tabstat lnpart5revenue total_profit lnlabor lnwage_cost total_fixedassets lnmaterial_cost if round==1 & survey_town_type==0

est clear
foreach var in lnpart5revenue total_profit lnlabor lnwage_cost total_fixedassets lnmaterial_cost shutdown {
    eststo: xtreg `var' post interpost inter4post, fe robust cluster(survey_town) nonest
    quietly summarize `var' if round==1 & survey_town_type==0
    estadd scalar Mean = r(mean)
    estadd local FE "Yes"
}

#delimit ;
esttab using "$path1/Table3.tex", replace f ///
    compress keep(interpost inter4post) b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    nogaps noeqli booktabs nonote noobs collabels(none) nomtitles ///
    coeflabels(interpost "Post*Treated" ///
               inter4post "\shortstack[l]{Post*Share\\Competitors Treated}") ///
    prehead("\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi} \begin{tabular*}{1.1\textwidth}{@{\extracolsep{\fill}}lccccccc@{}}  \hline \hline \\ Dep. Var.:") ///
    s(FE Mean N, fmt(0 %9.3f %9.0gc) labels("Firm FE and Post" "Mean of Pure Control" "Observations")) ///
    mgroups("log Sales" "\shortstack{Profit(10,000\\RMB)}" ///
            "\shortstack{log Num of\\Employees}" ///
            "\shortstack{log Wage\\Bill}" ///
            "\shortstack{Fixed Assets\\(10,000 RMB)}" ///
            "\shortstack{log Material\\Cost}" "Shutdown", ///
            pattern(1 1 1 1 1 1 1)) ///
    substitute(_ \_ [1em])
;
#delimit cr


* Romano–Wolf p-values (clustered FE)
preserve

rwolf2 (xtreg lnpart5revenue    post interpost inter4post, fe robust cluster(survey_town) nonest) ///
       (xtreg total_profit      post interpost inter4post, fe robust cluster(survey_town) nonest) ///
       (xtreg lnlabor           post interpost inter4post, fe robust cluster(survey_town) nonest) ///
       (xtreg lnwage_cost       post interpost inter4post, fe robust cluster(survey_town) nonest) ///
       (xtreg total_fixedassets post interpost inter4post, fe robust cluster(survey_town) nonest) ///
       (xtreg lnmaterial_cost   post interpost inter4post, fe robust cluster(survey_town) nonest) ///
       (xtreg shutdown          post interpost inter4post, fe robust cluster(survey_town) nonest), ///
       indepvars(interpost inter4post, interpost inter4post, interpost inter4post, ///
                 interpost inter4post, interpost inter4post, interpost inter4post, ///
                 interpost inter4post) reps(1000) seed(123) cluster(survey_town) verbose usevalid

matrix RWp = e(RW)

forvalues i = 1/7 {
    capture matrix drop b
    capture matrix drop adjpval
    matrix b       = [ RWp[2*`i'-1, 3], RWp[2*`i', 3] ]
    matrix adjpval = [ RWp[2*`i'-1, 3], RWp[2*`i', 3] ]
    matrix colnames b       = "\hspace{0.25cm} Post*Treated" "\hspace{0.25cm} Post*Sh Comp Treated"
    matrix colnames adjpval = "\hspace{0.25cm} Post*Treated" "\hspace{0.25cm} Post*Sh Comp Treated"
    ereturn post b
    quietly estadd matrix adjpval
    eststo rwolf`i'
}

#delimit ;
esttab rwolf1 rwolf2 rwolf3 rwolf4 rwolf5 rwolf6 rwolf7 using "$path1\Table3.tex", ///
    cells(b(fmt(3) star pvalue(adjpval))) append f ///
    star(* 0.10 ** 0.05 *** 0.01) nogaps noeqli nomtitle nonumbers ///
    collabels(none) booktabs nolines noobs ///
    prehead("\hline \multicolumn{@span}{l}{Romano-Wolf P-values} \\") ///
    postfoot("\hline\hline \end{tabular*}")
;
#delimit cr

restore


*******************************************************
***   QUESTION 1C(ii): HOMOSKEDASTIC TABLE 3        ***
*******************************************************

display "=== PS6 C(ii): Generating Homoskedastic Table 3 ==="

est clear
foreach var in lnpart5revenue total_profit lnlabor lnwage_cost total_fixedassets lnmaterial_cost shutdown {
    eststo: xtreg `var' post interpost inter4post, fe
    quietly summarize `var' if round==1 & survey_town_type==0
    estadd scalar Mean = r(mean)
    estadd local FE "Yes"
}

#delimit ;
esttab using "$path1/Table3_homo.tex", replace f ///
    compress keep(interpost inter4post) b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    nogaps noeqli booktabs nonote noobs collabels(none) nomtitles ///
    coeflabels(interpost "Post*Treated" ///
               inter4post "\shortstack[l]{Post*Share\\Competitors Treated}") ///
    prehead("\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi} ///
             \begin{tabular*}{1.1\textwidth}{@{\extracolsep{\fill}}lccccccc@{}} ///
             \hline \hline \\ Dep. Var.:") ///
    s(FE Mean N, fmt(0 %9.3f %9.0gc) ///
       labels("Firm FE and Post" "Mean of Pure Control" "Observations")) ///
    mgroups("log Sales" "\shortstack{Profit(10,000\\RMB)}" ///
            "\shortstack{log Num of\\Employees}" ///
            "\shortstack{log Wage\\Bill}" ///
            "\shortstack{Fixed Assets\\(10,000 RMB)}" ///
            "\shortstack{log Material\\Cost}" "Shutdown", ///
            pattern(1 1 1 1 1 1 1)) ///
    substitute(_ \_ [1em])
;
#delimit cr


* Romano–Wolf p-values (homoskedastic)
preserve

rwolf2 (xtreg lnpart5revenue    post interpost inter4post, fe) ///
       (xtreg total_profit      post interpost inter4post, fe) ///
       (xtreg lnlabor           post interpost inter4post, fe) ///
       (xtreg lnwage_cost       post interpost inter4post, fe) ///
       (xtreg total_fixedassets post interpost inter4post, fe) ///
       (xtreg lnmaterial_cost   post interpost inter4post, fe) ///
       (xtreg shutdown          post interpost inter4post, fe), ///
       indepvars(interpost inter4post, interpost inter4post, interpost inter4post, ///
                 interpost inter4post, interpost inter4post, interpost inter4post, ///
                 interpost inter4post) reps(1000) seed(123) usevalid

matrix RW2 = e(RW)

forvalues i = 1/7 {
    capture matrix drop b
    capture matrix drop adjpval
    matrix b       = [ RW2[2*`i'-1, 3], RW2[2*`i', 3] ]
    matrix adjpval = [ RW2[2*`i'-1, 3], RW2[2*`i', 3] ]
    matrix colnames b       = "Post*Treated" "Post*Sh Comp Treated"
    matrix colnames adjpval = "Post*Treated" "Post*Sh Comp Treated"
    ereturn post b
    quietly estadd matrix adjpval
    eststo rwolf_h`i'
}

#delimit ;
esttab rwolf_h1 rwolf_h2 rwolf_h3 rwolf_h4 rwolf_h5 rwolf_h6 rwolf_h7 ///
    using "$path1/Table3_homo.tex", append f ///
    cells(b(fmt(3) star pvalue(adjpval))) ///
    star(* 0.10 ** 0.05 *** 0.01) nogaps noeqli nomtitle nonumbers ///
    collabels(none) booktabs nolines noobs ///
    prehead("\hline \multicolumn{@span}{l}{Romano-Wolf P-values} \\") ///
    postfoot("\hline\hline \end{tabular*}")
;
#delimit cr

restore


*******************************************************
***   HAUSMAN TESTS – C(ii)                        ***
*******************************************************

display "=== PS6 C(ii): Running Hausman FE vs RE Tests ==="

local outcomes lnpart5revenue total_profit lnlabor lnwage_cost total_fixedassets lnmaterial_cost
local K : word count `outcomes'

matrix H_chi2 = J(1, `K', .)
matrix H_p    = J(1, `K', .)

local j = 1
foreach var of local outcomes {
    quietly xtreg `var' post interpost inter4post, fe
    est store fe`j'

    quietly xtreg `var' post interpost inter4post type, re
    est store re`j'

    quietly hausman fe`j' re`j', sigmamore

    matrix H_chi2[1, `j'] = r(chi2)
    matrix H_p[1, `j']    = r(p)

    est drop fe`j' re`j'
    local ++j
}

matrix colnames H_chi2 = "Sales" "Profit" "Emp" "Wage" "FixedAssets" "MatCost"
matrix colnames H_p    = "Sales" "Profit" "Emp" "Wage" "FixedAssets" "MatCost"

display "=== Hausman chi2 statistics ==="
matrix list H_chi2

display "=== Hausman p-values ==="
matrix list H_p


***************************************************************
***   BONUS QUESTION – FIRST DIFFERENCES                    ***
***************************************************************

xtset firmid round

*--------------------------------------------------------------*
* Generate first–difference variables                         *
*--------------------------------------------------------------*

capture drop D_sales D_profit D_labor D_wage D_assets D_mat D_shut
capture drop D_interpost D_inter4post

gen D_sales   = D.lnpart5revenue
gen D_profit  = D.total_profit
gen D_labor   = D.lnlabor
gen D_wage    = D.lnwage_cost
gen D_assets  = D.total_fixedassets
gen D_mat     = D.lnmaterial_cost
gen D_shut    = D.shutdown

gen D_interpost  = D.interpost
gen D_inter4post = D.inter4post


*--------------------------------------------------------------*
* Regressions in first differences                            *
*--------------------------------------------------------------*

est clear
foreach var in D_sales D_profit D_labor D_wage D_assets D_mat D_shut {

    eststo: reg `var' D_interpost D_inter4post, robust cluster(survey_town)

    quietly summarize `var'
    estadd scalar Mean = r(mean)
    estadd local FE "No"
}

*--------------------------------------------------------------*
* Export FD Table 3                                           *
*--------------------------------------------------------------*

#delimit ;
esttab using "$path1/Table3_diff.tex", replace f ///
    compress keep(D_interpost D_inter4post) b(3) se(3) ///
    star(* 0.10 ** 0.05 *** 0.01) nogaps noeqli booktabs noobs nonotes nomtitles ///
    coeflabels(D_interpost "Post*Treated (FD)" ///
               D_inter4post "\shortstack[l]{Post*Share\\Competitors Treated (FD)}") ///
    prehead("\def\sym#1{\ifmmode^{#1}\else\(^ {#1}\)\fi}
             \begin{tabular*}{1.1\textwidth}{@{\extracolsep{\fill}}lccccccc@{}}
             \hline \hline \\ Dep. Var.:") ///
    s(FE Mean N, fmt(0 %9.3f %9.0gc)
      labels("No firm FE" "Mean of Pure Control" "Observations")) ///
    mgroups("log Sales" "\shortstack{Profit(10,000\\RMB)}"
            "\shortstack{log Num of\\Employees}"
            "\shortstack{log Wage\\Bill}"
            "\shortstack{Fixed Assets\\(10,000 RMB)}"
            "\shortstack{log Material\\Cost}" "Shutdown",
            pattern(1 1 1 1 1 1 1)) ///
    substitute(_ \_ [1em])
    ;
#delimit cr


*--------------------------------------------------------------*
* Romano–Wolf p-values for differenced specification          *
*--------------------------------------------------------------*

preserve

rwolf2                                                     ///
    (reg D_sales   D_interpost D_inter4post, robust cluster(survey_town)) ///
    (reg D_profit  D_interpost D_inter4post, robust cluster(survey_town)) ///
    (reg D_labor   D_interpost D_inter4post, robust cluster(survey_town)) ///
    (reg D_wage    D_interpost D_inter4post, robust cluster(survey_town)) ///
    (reg D_assets  D_interpost D_inter4post, robust cluster(survey_town)) ///
    (reg D_mat     D_interpost D_inter4post, robust cluster(survey_town)) ///
    (reg D_shut    D_interpost D_inter4post, robust cluster(survey_town)), ///
    indepvars( D_interpost D_inter4post, ///
               D_interpost D_inter4post, ///
               D_interpost D_inter4post, ///
               D_interpost D_inter4post, ///
               D_interpost D_inter4post, ///
               D_interpost D_inter4post, ///
               D_interpost D_inter4post ) ///
    reps(1000) seed(123) usevalid

matrix RWfd = e(RW)

forvalues i = 1/7 {

    capture matrix drop b
    capture matrix drop adjpval

    matrix b       = [ RWfd[2*`i'-1, 3], RWfd[2*`i', 3] ]
    matrix adjpval = [ RWfd[2*`i'-1, 3], RWfd[2*`i', 3] ]

    matrix colnames b       = Post_Treated_FD Post_ShComp_FD
    matrix colnames adjpval = Post_Treated_FD Post_ShComp_FD

    ereturn post b
    quietly estadd matrix adjpval
    eststo rwfd`i'
}

#delimit ;
esttab rwfd1 rwfd2 rwfd3 rwfd4 rwfd5 rwfd6 rwfd7 ///
    using "$path1/Table3_diff.tex", append f ///
    cells(b(fmt(3) star pvalue(adjpval))) ///
    star(* 0.10 ** 0.05 *** 0.01) nogaps noeqli nomtitle nonumbers ///
    booktabs nolines noobs ///
    prehead("\hline \multicolumn{@span}{l}{Romano–Wolf P-values (FD)} \\") ///
    postfoot("\hline\hline \end{tabular*}")
    ;
#delimit cr

restore
