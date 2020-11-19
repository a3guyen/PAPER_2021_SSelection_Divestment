clear
capture log close
global path "C:\Users\ngoca\Dropbox\Pubs_since_2018\2021_Divestment_Sample_Selection"
cd "$path\Data"

set matsize 10000 
*Install PPMLHDFE:
/*
ssc install ftools
ssc install reghdfe
ssc install ppmlhdfe
*/
*======================= Getting FDI flows data for 2012 ==================
use ".\DerivedData\Data2.dta", clear
keep source host CountryN* flow time
gen investment = flow if flow>0
gen divestment = abs(flow) if flow <0
export excel using ".\DerivedData\2000to2012data.xlsx", firstrow(variables)
keep time ==2012
export excel using ".\DerivedData\2012data.xlsx", firstrow(variables)

/*
bysort host: egen totalinv_h=total(investment)
bysort host: egen totaldiv_h = total(divestment)
bysort source: egen totalinv_s=total(investment)
bysort source: egen totaldiv_s = total(divestment)
corr totalinv_h totaldiv_h
corr totalinv_s totaldiv_s
*/

log using Log_1stSubmission, replace


use ".\DerivedData\Data1.dta", clear

*------------------------ 1. Create variables ---------------------------------------------------------
gen lgdp_h = ln(gdp_h/10^9)
gen lgdp_s = ln(gdp_s/10^9)
gen lgdppc_h = ln(gdppc_h/10^3)
gen lgdppc_s = ln(gdppc_s/10^3)
gen ldist = ln(dist/10^3)
gen lstartup_h = ln(startupcost_h)
gen startcost_h = 10-startupscore_h/10

replace religion = religion/10

rename comlang_ethno comlang


inspect rflow rflow_o
gen divestment = -rflow if rflow <0 & !missing(rflow)
replace divestment = 0 if rflow >=0 & !missing(rflow)
gen investment = rflow if rflow >=0
replace investment = 0 if rflow <0 & !missing(rflow)

gen divestment_o = -rflow_o if rflow_o <0 & !missing(rflow_o)
replace divestment_o = 0 if rflow_o >=0 & !missing(rflow_o)
gen investment_o = rflow_o if rflow_o >=0
replace investment_o = 0 if rflow_o <0 & !missing(rflow_o)


gen ldivestment = ln(divestment)
gen linvestment = ln(investment)


gen asinhdiv = asinh(divestment)
gen asinhinv = asinh(investment)
gen asinhdiv_o = asinh(divestment_o)
gen asinhinv_o = asinh(investment_o)
rename (bit comcur comlegal comlang contig  crex) (BIT Currency Legal Language Border Volatility)

*Generate non directional pair ids:
gen idp = cond(host <= source, host, source) + cond(host >= source, host, source)

global varlist lgdp_s lgdp_h patentshare_s patentshare_h remoteness_s remoteness_h startcost_h BIT Currency Legal religion Language colony  Border  ldist Volatility y*
reg asinhdiv $varlist  , cluster(idp) 
keep if e(sample)==1


 
gen x = Roeif - Roeof
// sum x
// local a = 100
// scatter x time, yline(100,lcolor(red)) yline(-100,lcolor(red)) 
drop if abs(x)>100 & !missing(x) // exclude 239 obs  with over 100 million USD difference in data reported by host and source countries


*Creat country & time dummies
tab host, gen(hostdummy)
tab source, gen(sourcedummy)
tab time, gen(y)
drop y // y is an idicator of invest or not

save Step3DataForRegression.dta, replace


*--------------------------- 2. Regression - No Country FEs ----------------------------
use Step3DataForRegression.dta, clear

global varlist lgdp_s lgdp_h patentshare_s patentshare_h remoteness_s remoteness_h startcost_h BIT Currency Legal religion Language colony  Border  ldist Volatility y*


*Testing for multicolinerity
/*
corr $varlist
collin $varlist
*/
eststo clear


*---------------Pooled OLS
reg asinhdiv $varlist  , cluster(idp) 
eststo polsdives
sum time divestment investment lgdp_s lgdp_h patentshare_s patentshare_h remoteness_s remoteness_h startcost_h BIT Currency Legal religion Language colony  Border  ldist Volatility if e(sample)==1
unique source if e(sample)==1 //126
unique host  if e(sample)==1 //126
unique time if e(sample)==1 //14

reg asinhinv $varlist  , cluster(idp) 
eststo polsinves

*---------------PPML
//poisson divestment  $varlist  , cluster(idp) 
ppml divestment  $varlist  , cluster(idp) 

eststo ppmldiv1
capture drop xb
capture drop fit2
capture drop fit
qui predict fit, xb
qui gen fit2 = fit^2
qui ppml divestment fit2  $varlist  , cluster(idp) 
test fit2=0 //pvalue = 0.4092


ppml investment  $varlist  , cluster(idp) 

eststo ppmlinv
capture drop xb
capture drop fit2
capture drop fit
qui predict fit, xb
qui gen fit2 = fit^2
qui ppml investment fit2  $varlist  , cluster(idp) 
test fit2=0 //pvalue = 0.1551




esttab using 1stsubmissionREsults.rtf, star(* 0.1 ** 0.05 *** 0.01) stats (ll N) ///
se(3) b(3) nodepvars drop (y* ) noomitted nogaps title(Table: Results without country FEs) compress replace



*--------------------------- 3. Regression - With Country FEs ----------------------------
use Step3DataForRegression.dta, clear
eststo clear
global varlist lgdp_s lgdp_h patentshare_s patentshare_h remoteness_s remoteness_h startcost_h BIT Currency Legal religion Language colony  Border  ldist Volatility y* hostd* sourced*




*---------------Pooled OLS
reg asinhdiv $varlist  , cluster(idp) 
eststo polsdives
reg asinhinv $varlist , cluster(idp) 
eststo polsinves


*---------------PPML
ppml divestment  $varlist  , cluster(idp) 

eststo ppmldiv1
capture drop xb
capture drop fit2
capture drop fit
qui predict fit, xb
qui gen fit2 = fit^2
qui ppml divestment fit2  $varlist  , cluster(idp) 
test fit2=0 //pvalue = 0.000


ppml investment  $varlist  , cluster(idp) 

eststo ppmlinv
capture drop xb
capture drop fit2
capture drop fit
qui predict fit, xb
qui gen fit2 = fit^2
qui ppml investment fit2  $varlist  , cluster(idp) 
test fit2=0 //pvalue = 0.5021




esttab using 1stsubmissionREsults.rtf, star(* 0.1 ** 0.05 *** 0.01) stats (ll N) ///
se(3) b(3) nodepvars drop (y* hostd* sourced*) noomitted nogaps title(Table: Results With country FEs) compress append


*--------------------------- 4. Regression - With PPML-HDFE (high dimensional fixed effects)----------------------------
/* See: 
https://www.statalist.org/forums/forum/general-stata-discussion/general/1487938-ppmlhdfe-is-now-on-ssc-poisson-regressions-with-fixed-effects
http://scorreia.com/help/ppmlhdfe.html
https://www.statalist.org/forums/forum/general-stata-discussion/general/1562011-reference-category-for-ppml-hdfe
*/

use Step3DataForRegression.dta, clear
eststo clear
global varlist lgdp_s lgdp_h patentshare_s patentshare_h remoteness_s remoteness_h startcost_h BIT Currency Legal religion Language colony  Border  ldist Volatility 



*---------------PPML



ppmlhdfe investment  $varlist  ,  absorb(host source time) vce(cluster idp) 
eststo ppmlinvHDFE
capture drop xb
capture drop fit2
capture drop fit
qui predict fit, xb
qui gen fit2 = fit^2
qui ppmlhdfe investment fit2  $varlist  , absorb(host source time) vce(cluster idp) 
test fit2=0 //pvalue = 0.0000

ppmlhdfe divestment  $varlist  , absorb(host source time) vce(cluster idp) 

eststo ppmldivHDFE
capture drop xb
capture drop fit2
capture drop fit
predict fit, xb
qui gen fit2 = fit^2
qui ppmlhdfe divestment fit2  $varlist  , absorb(host source time) vce(cluster idp) 
 
test fit2=0 //pvalue = 0.9360


esttab using 1stsubmissionREsults.rtf, star(* 0.1 ** 0.05 *** 0.01) stats (ll N) ///
se(3) b(3) nodepvars noomitted nogaps title(Table: Results from PPML-HDFE) compress append



*Descriptive Statistic Table

use Step3DataForRegression.dta, clear

sum  divestment investment lgdp_s lgdp_h patentshare_s patentshare_h remoteness_s remoteness_h startcost_h BIT Currency Legal religion Language colony  Border  ldist Volatility

use Chapter3log.dta, clear
outreg2 using DescriptiveTableM.rtf, replace sum(log)  
inspect divestment
log close
exit
