***Replication code for:***
***Smith, Daniel M., and Shane Martin. 2017. “Political Dynasties and the Selection of Cabinet Ministers.” Legislative Studies Quarterly, 42(1): 131-165.***
clear

set more off
set matsize 1000

********************************************************************************
*************************** CANDIDATE ANALYSIS *********************************
********************************************************************************

use "~IRELAND-CANDIDATES.dta", replace

*** Variable preparation; Define label for legacyscale ***

gen legacyscale = 0
replace legacyscale = 1 if pre_mp == 1
replace legacyscale = 2 if cablegacy == 1

label define legacy_scale 0 "Non-Legacy" 1 "Non-Cabinet Legacy" 2 "Cabinet Legacy"
label values legacyscale legacy_scale

qui tab legacyscale, gen(legacy_)

gen cwins2 = cwins*cwins
gen est_age2 = est_age*est_age
gen age2 = age*age

gen agegroup = 0
replace agegroup = 1 if est_age > 29
replace agegroup = 2 if est_age > 39
replace agegroup = 3 if est_age > 49
replace agegroup = 4 if est_age > 59

bysort pid: egen prelocal = min(local)
bysort pid: egen preseanad = min(seanad)

gen educode2 = educode
replace educode2 = 1 if educode == 2
replace educode2 = 2 if educode == 3
replace educode2 = 2 if educode == 4
replace educode2 = 3 if educode == 5

qui tab educode2, gen(educode_)

sort pid cruns
by pid: gen first_age = est_age if cwins == 1
by pid: egen agefirst = min(first_age)
by pid: gen first_party = party if cruns == 1 
by pid: replace first_party = first_party[_n-1] if cruns > 1

gen firstrun = 0
replace firstrun = 1 if cruns == 1

*** Re-code James Dillon (FG-affiliated indep. who effectively was FG) as FG ***
replace party = "FG" if name == "Dillon, James"
replace partyid = 2 if name == "Dillon, James"
replace govparty = 1 if name == "Dillon, James" & elecyear == 1948

gen partyidsimple = partyid
replace partyidsimple = 8 if partyid == 9
replace partyidsimple = 15 if partyid == 14
replace partyidsimple = 1 if partyid == 11
replace partyidsimple = 2 if partyid == 12
replace partyidsimple = 30 if partyid == 16
replace partyidsimple = 30 if partyid == 18
replace partyidsimple = 30 if partyid == 23
replace partyidsimple = 30 if partyid == 26
replace partyidsimple = 30 if partyid == 16

egen party_year = group(partyidsimple electiondate)
egen dist_year = group(districtid electiondate)
egen party_dail = group(partyidsimple dail)

sort pid cwins
gen prevwins = cwins
by pid: replace prevwins = prevwins - 1 if result == 1
gen prewins2 = prevwins*prevwins

gen dailyear = 0
replace dailyear = 	1918	if dail ==  1
replace dailyear = 	1921	if dail ==  2
replace dailyear = 	1922	if dail ==  3
replace dailyear = 	1923	if dail ==  4
replace dailyear = 	1927	if dail ==  5
replace dailyear = 	1927	if dail ==  6
replace dailyear = 	1932	if dail ==  7
replace dailyear = 	1933	if dail ==  8
replace dailyear = 	1937	if dail ==  9
replace dailyear = 	1938	if dail ==  10
replace dailyear = 	1943	if dail ==  11
replace dailyear = 	1944	if dail ==  12
replace dailyear = 	1948	if dail ==  13
replace dailyear = 	1951	if dail ==  14
replace dailyear = 	1954	if dail ==  15
replace dailyear = 	1957	if dail ==  16
replace dailyear = 	1961	if dail ==  17
replace dailyear = 	1965	if dail ==  18
replace dailyear = 	1969	if dail ==  19
replace dailyear = 	1973	if dail ==  20
replace dailyear = 	1977	if dail ==  21
replace dailyear = 	1981	if dail ==  22
replace dailyear = 	1982	if dail ==  23
replace dailyear = 	1982	if dail ==  24
replace dailyear = 	1987	if dail ==  25
replace dailyear = 	1989	if dail ==  26
replace dailyear = 	1992	if dail ==  27
replace dailyear = 	1997	if dail ==  28
replace dailyear = 	2002	if dail ==  29
replace dailyear = 	2007	if dail ==  30
replace dailyear = 	2011	if dail ==  31
replace dailyear = 	2016	if dail ==  32

keep if elecyear > 1943

sum result quotashare i.legacyscale female firstrun

**************************STATS FOR INDIVIDUALS (and Figure 1)******************

sort pid cruns
by pid: gen n_obs = _n

tab first_party if n_obs == 1 & pre_mp == 1
tab partyidsimple if n_obs == 1 & pre_mp == 1
tab predrelation if n_obs == 1 & pre_mp == 1
tab generation if n_obs == 1 & pre_mp == 1
tab legacyscale if n_obs == 1 

tab legacyscale
tab legacyscale if result == 1
tab legacyscale if result == 1 & govparty == 1
tab legacyscale if cabappt == 1

********************************************************************************
************************* APPENDIX TRENDS FIGURES ******************************
********************************************************************************
labmask dail, values(dailyear)

bysort dail: egen meancandpre_mp = mean(pre_mp) if duplicate == 0
replace meancandpre_mp = meancandpre_mp*100

bysort dail: egen meantdpre_mp = mean(pre_mp) if result == 1 & duplicate == 0
replace meantdpre_mp = meantdpre_mp*100

bysort dail: egen meangovtdpre_mp = mean(pre_mp) if result == 1 & govparty == 1 & duplicate == 0
replace meangovtdpre_mp = meangovtdpre_mp*100

bysort dail: egen meancabpre_mp = mean(pre_mp) if result == 1 & cabappt == 1 & duplicate == 0
replace meancabpre_mp = meancabpre_mp*100

twoway (connected meancandpre_mp dail, sort mcolor(white) msymbol(circle) mfcolor(white) mlcolor(black) ///
lcolor(black) lpattern(dash)) (connected meantdpre_mp dail, sort mcolor(black) msymbol(circle) lcolor(black) ///
lpattern(solid)) (connected meangovtdpre_mp dail, sort mcolor(white) msymbol(square) mlcolor(black) lcolor(black) ///
lpattern(dash))(connected meancabpre_mp dail, sort mcolor(green) msymbol(square) mlcolor(green) lcolor(green) ///
lpattern(solid)), ytitle(Percent) ylabel(0(10)50, angle(horizontal) gmin gmax) xtitle(Year) xlabel(12(1)32, labsize(small) ///
angle(forty_five) valuelabel) title(Legacies (both types included), size(medium) color(black)) note(, position(5)) ///
legend(order(1 "Candidates" 2 "TDs" 3 "Government TDs" 4 "Cabinet ministers") cols(2))
graph save Graph "~appendix-figureA1a.gph", replace
*Note: formatting style of this and other figures was adjusted in Graph Editor following running the code*

***

bysort dail: egen meancandleg1 = mean(legacy_2) if duplicate == 0
replace meancandleg1 = meancandleg1*100

bysort dail: egen meantdleg1 = mean(legacy_2) if result == 1 & duplicate == 0
replace meantdleg1 = meantdleg1*100

bysort dail: egen meangovtdleg1 = mean(legacy_2) if result == 1 & govparty == 1 & duplicate == 0
replace meangovtdleg1 = meangovtdleg1*100

bysort dail: egen meancableg1 = mean(legacy_2) if result == 1 & cabappt == 1 & duplicate == 0
replace meancableg1 = meancableg1*100

twoway (connected meancandleg1 dail, sort mcolor(white) msymbol(circle) mfcolor(white) mlcolor(black) ///
lcolor(black) lpattern(dash)) (connected meantdleg1 dail, sort mcolor(black) msymbol(circle) lcolor(black) ///
lpattern(solid)) (connected meangovtdleg1 dail, sort mcolor(white) msymbol(square) mlcolor(black) lcolor(black) ///
lpattern(dash))(connected meancableg1 dail, sort mcolor(green) msymbol(square) mlcolor(green) lcolor(green) ///
lpattern(solid)), ytitle(Percent) ylabel(0(10)50, angle(horizontal) gmin gmax) xtitle(Year) xlabel(12(1)32, labsize(small) ///
angle(forty_five) valuelabel) title(Non-cabinet legacies, size(medium) color(black)) note(, position(5)) ///
legend(order(1 "Candidates" 2 "TDs" 3 "Government TDs" 4 "Cabinet ministers") cols(2))
graph save Graph "~appendix-figureA1b.gph", replace

***

bysort dail: egen meancandleg2 = mean(legacy_3) if duplicate == 0
replace meancandleg2 = meancandleg2*100

bysort dail: egen meantdleg2 = mean(legacy_3) if result == 1 & duplicate == 0
replace meantdleg2 = meantdleg2*100

bysort dail: egen meangovtdleg2 = mean(legacy_3) if result == 1 & govparty == 1 & duplicate == 0
replace meangovtdleg2 = meangovtdleg2*100

bysort dail: egen meancableg2 = mean(legacy_3) if result == 1 & cabappt == 1 & duplicate == 0
replace meancableg2 = meancableg2*100

twoway (connected meancandleg2 dail, sort mcolor(white) msymbol(circle) mfcolor(white) mlcolor(black) ///
lcolor(black) lpattern(dash)) (connected meantdleg2 dail, sort mcolor(black) msymbol(circle) lcolor(black) ///
lpattern(solid)) (connected meangovtdleg2 dail, sort mcolor(white) msymbol(square) mlcolor(black) lcolor(black) ///
lpattern(dash))(connected meancableg2 dail, sort mcolor(green) msymbol(square) mlcolor(green) lcolor(green) ///
lpattern(solid)), ytitle(Percent) ylabel(0(10)50, angle(horizontal) gmin gmax) xtitle(Year) xlabel(12(1)32, labsize(small) ///
angle(forty_five) valuelabel) title(Cabinet legacies, size(medium) color(black)) note(, position(5)) ///
legend(order(1 "Candidates" 2 "TDs" 3 "Government TDs" 4 "Cabinet ministers") cols(2))
graph save Graph "~appendix-figureA1c.gph", replace

********************************************************************************
*************************** ELECTORAL ADVANTAGE ********************************
********************************************************************************

***WIN ELECTION***

reg result i.legacyscale, cluster(pid)
eststo
estadd local PartyYearFE "No"
estadd local DistrictYearFE "No"
estadd local Controls "No"
areg result i.legacyscale, absorb(party_year) cluster(pid)
eststo
estadd local PartyYearFE "Yes"
estadd local DistrictYearFE "No"
estadd local Controls "No"
areg result i.legacyscale, absorb(dist_year) cluster(pid)
eststo
estadd local PartyYearFE "No"
estadd local DistrictYearFE "Yes"
estadd local Controls "No"
areg result i.legacyscale female firstrun, absorb(party_year) cluster(pid)
eststo
estadd local PartyYearFE "Yes"
estadd local DistrictYearFE "No"
estadd local Controls "Yes"
areg result i.legacyscale female firstrun, absorb(dist_year) cluster(pid)
eststo
estadd local PartyYearFE "No"
estadd local DistrictYearFE "Yes"
estadd local Controls "Yes"

esttab using table3.tex, replace style(tex) r2 se nostar b(%9.3f) se(%9.3f) ///
drop() nomtitles nonotes stats(N r2 PartyYearFE DistrictYearFE Controls, labels ("N" "R2" "Party-Year FE" "District-Year FE" "Controls")) ///
prehead("\begin{tabular}{l*{@M}{rr}}" "\hline") posthead(\hline) postfoot("\hline" "\end{tabular}")  

***QUOTA PROPORTION***

estimates clear
reg quotashare i.legacyscale, cluster(pid)
eststo
estadd local PartyYearFE "No"
estadd local DistrictYearFE "No"
estadd local Controls "No"
areg quotashare i.legacyscale, absorb(party_year) cluster(pid)
eststo
estadd local PartyYearFE "Yes"
estadd local DistrictYearFE "No"
estadd local Controls "No"
areg quotashare i.legacyscale, absorb(dist_year) cluster(pid)
eststo
estadd local PartyYearFE "No"
estadd local DistrictYearFE "Yes"
estadd local Controls "No"
areg quotashare i.legacyscale female firstrun, absorb(party_year) cluster(pid)
eststo
estadd local PartyYearFE "Yes"
estadd local DistrictYearFE "No"
estadd local Controls "Yes"
areg quotashare i.legacyscale female firstrun, absorb(dist_year) cluster(pid)
eststo
estadd local PartyYearFE "No"
estadd local DistrictYearFE "Yes"
estadd local Controls "Yes"

esttab using table4.tex, replace style(tex) r2 se nostar b(%9.3f) se(%9.3f) ///
drop() nomtitles nonotes stats(N r2 PartyYearFE DistrictYearFE Controls, labels ("N" "R2" "Party-Year FE" "District-Year FE" "Controls")) ///
prehead("\begin{tabular}{l*{@M}{rr}}" "\hline") posthead(\hline) postfoot("\hline" "\end{tabular}")  

********************************************************************************
******************************* TD ANALYSIS ************************************
********************************************************************************

keep if result == 1

sort pid cwins
by pid: gen n_obs_tds = _n
tab predrelation if n_obs_tds == 1 & pre_mp == 1
tab generation if n_obs_tds == 1 & pre_mp == 1
tab legacyscale if n_obs_tds == 1 & pre_mp == 1

keep if govparty == 1
keep if cabexp == 0
drop if cab1 == "Taoiseach"
drop if speaker == 1

sort pid cwins
by pid: gen n_obs_tds2 = _n

***Descriptive statistics of data sample***

sum cabappt i.legacyscale cwins quotashare female est_age i.agegroup prelocal preseanad i.iscoid localborn educode2 i.educode2 feeschool ucd i.partyidsimple

tab partyidsimple

***BASIC FINDINGS: THE LEGACY ADVANTAGE***

estimates clear
reg cabappt i.legacyscale, cluster(pid)
eststo
estadd local PartyFE "No"
estadd local PartyYearFE "No"
areg cabappt i.legacyscale, absorb(partyidsimple) cluster(pid)
eststo
estadd local PartyFE "Yes"
estadd local PartyYearFE "No"
areg cabappt i.legacyscale, absorb(party_dail) cluster(pid)
eststo
estadd local PartyFE "No"
estadd local PartyYearFE "Yes"
areg cabappt i.legacyscale cwins cwins2, absorb(party_dail) cluster(pid)
eststo
estadd local PartyFE "No"
estadd local PartyYearFE "Yes"
areg cabappt i.legacyscale cwins cwins2 quotashare, absorb(party_dail) cluster(pid)
eststo
estadd local PartyFE "No"
estadd local PartyYearFE "Yes"

esttab using table5.tex, replace style(tex) r2 se nostar b(%9.3f) se(%9.3f) ///
drop() nomtitles nonotes stats(N r2 PartyFE PartyYearFE, labels ("N" "R2" "Party FE" "Party-Year FE")) ///
prehead("\begin{tabular}{l*{@M}{rr}}" "\hline") posthead(\hline) postfoot("\hline" "\end{tabular}")  

***UNPACKING THE INFORMATIONAL ADVANTAGE***

replace iscoid = 10 if iscoid == 0

**EXPERIENCE**
estimates clear
areg cabappt i.legacyscale cwins cwins2, absorb(party_dail) cluster(pid)
eststo
estadd local PartyYearFE "Yes"
areg cabappt i.legacyscale cwins cwins2 prelocal, absorb(party_dail) cluster(pid)
eststo
estadd local PartyYearFE "Yes"
areg cabappt i.legacyscale cwins cwins2 preseanad, absorb(party_dail) cluster(pid)
eststo
estadd local PartyYearFE "Yes"
areg cabappt i.legacyscale cwins cwins2 i.iscoid, absorb(party_dail) cluster(pid)
eststo
estadd local PartyYearFE "Yes"

esttab using appendix-tableA4.tex, replace style(tex) r2 se nostar b(%9.3f) se(%9.3f) ///
drop() nomtitles nonotes stats(N r2 PartyYearFE, labels ("N" "R2" "Party-Year FE")) ///
prehead("\begin{tabular}{l*{@M}{rr}}" "\hline") posthead(\hline) postfoot("\hline" "\end{tabular}")  

**DEMOGRAPHICS**
estimates clear
areg cabappt i.legacyscale cwins cwins2 female, absorb(party_dail) cluster(pid)
eststo
estadd local PartyYearFE "Yes"
areg cabappt i.legacyscale cwins cwins2 i.agegroup, absorb(party_dail) cluster(pid)
eststo
estadd local PartyYearFE "Yes"
areg cabappt i.legacyscale cwins cwins2 localborn, absorb(party_dail) cluster(pid)
eststo
estadd local PartyYearFE "Yes"

esttab using appendix-tableA5.tex, replace style(tex) r2 se nostar b(%9.3f) se(%9.3f) ///
drop() nomtitles nonotes stats(N r2 PartyYearFE, labels ("N" "R2" "Party-Year FE")) ///
prehead("\begin{tabular}{l*{@M}{rr}}" "\hline") posthead(\hline) postfoot("\hline" "\end{tabular}")  

**EDUCATION**
estimates clear
areg cabappt i.legacyscale cwins cwins2 i.educode2, absorb(party_dail) cluster(pid)
eststo
estadd local PartyYearFE "Yes"
areg cabappt i.legacyscale cwins cwins2 ucd, absorb(party_dail) cluster(pid)
eststo
estadd local PartyYearFE "Yes"
areg cabappt i.legacyscale cwins cwins2 feeschool, absorb(party_dail) cluster(pid)
eststo
estadd local PartyYearFE "Yes"

esttab using appendix-tableA6.tex, replace style(tex) r2 se nostar b(%9.3f) se(%9.3f) ///
drop() nomtitles nonotes stats(N r2 PartyYearFE, labels ("N" "R2" "Party-Year FE")) ///
prehead("\begin{tabular}{l*{@M}{rr}}" "\hline") posthead(\hline) postfoot("\hline" "\end{tabular}")  

**COMBINED (for FIGURE 5)**
xi: reg cabappt i.legacyscale 
**to create dummies

estimates clear
areg cabappt _Ilegacysca_1 _Ilegacysca_2 cwins cwins2, absorb(party_dail) cluster(pid)
eststo baseline
estadd local Model "Baseline"
areg cabappt _Ilegacysca_1 _Ilegacysca_2 cwins cwins2 prelocal, absorb(party_dail) cluster(pid)
eststo priorlocal
estadd local Model "Prior Local"
areg cabappt _Ilegacysca_1 _Ilegacysca_2 cwins cwins2 preseanad, absorb(party_dail) cluster(pid)
eststo priorseanad
estadd local Model "Prior Seanad"
areg cabappt _Ilegacysca_1 _Ilegacysca_2 cwins cwins2 i.iscoid, absorb(party_dail) cluster(pid)
eststo occupation
estadd local Model "Occupation"
areg cabappt _Ilegacysca_1 _Ilegacysca_2 cwins cwins2 female, absorb(party_dail) cluster(pid)
eststo female
estadd local Model "Female"
areg cabappt _Ilegacysca_1 _Ilegacysca_2 cwins cwins2 i.agegroup, absorb(party_dail) cluster(pid)
eststo age
estadd local Model "Age"
areg cabappt _Ilegacysca_1 _Ilegacysca_2 cwins cwins2 localborn, absorb(party_dail) cluster(pid)
eststo localborn
estadd local Model "Local Born"
areg cabappt _Ilegacysca_1 _Ilegacysca_2 cwins cwins2 i.educode2, absorb(party_dail) cluster(pid)
eststo education
estadd local Model "Educ. Level"
areg cabappt _Ilegacysca_1 _Ilegacysca_2 cwins cwins2 ucd, absorb(party_dail) cluster(pid)
eststo ucd
estadd local Model "UCD"
areg cabappt _Ilegacysca_1 _Ilegacysca_2 cwins cwins2 feeschool, absorb(party_dail) cluster(pid)
eststo feeschool
estadd local Model "Fee School"

coefplot baseline priorlocal priorseanad occupation female age localborn education ucd feeschool, ///
drop(_cons _Ilegacysca_1 cwins cwins2 quotashare prelocal preseanad *iscoid* female *agegroup* localborn *educode2* ucd feeschool) ///
xline(0) levels(95 90) legend(title(Additional controls) position(6) cols(5))

esttab using regression_full.tex, replace style(tex) r2 se nostar b(%9.3f) se(%9.3f) ///
drop(*cwins *cwins2 *prelocal *preseanad *iscoid *female *agegroup *localborn *educode *ucd *feeschool) nomtitles nonotes stats(N r2 Model, labels ("N" "R2" "Model")) ///
prehead("\begin{tabular}{l*{@M}{rr}}" "\hline") posthead(\hline) postfoot("\hline" "\end{tabular}")  

***Descriptive figures***
gen offprof = 0
replace offprof = 1 if iscoid < 3

**Figure 2**
graph bar (mean) prelocal if n_obs_tds2 == 1, over(legacyscale) bar(1, fcolor(green) fintensity(inten10) lcolor(black) lwidth(thin)) ytitle(, size(zero)) title(Prior Local, color(black))
graph save Graph "~prelocal.gph", replace

graph bar (mean) preseanad if n_obs_tds2 == 1, over(legacyscale) bar(1, fcolor(gs14) lcolor(black) lwidth(thin)) ytitle(, size(zero)) title(Prior Seanad, color(black))
graph save Graph "~preseanad.gph", replace

graph bar (mean) offprof if n_obs_tds2 == 1, over(legacyscale) bar(1, fcolor(gs14) lcolor(black) lwidth(thin)) ytitle(, size(zero)) title(Senior Official or Professional, color(black))
graph save Graph "/Users/dsmith/Dropbox/Smith-Martin/Data and Analysis/offprof.gph", replace

**Figure 3**
graph bar (mean) female if n_obs_tds2 == 1, over(legacyscale) bar(1, fcolor(gs14) lcolor(black) lwidth(thin)) ytitle(, size(zero)) title(Female, color(black))
graph save Graph "~female.gph", replace

graph bar (mean) agefirst if n_obs_tds2 == 1, over(legacyscale) bar(1, fcolor(gs14) lcolor(black) lwidth(thin)) ytitle(, size(zero)) title(Age First Elected, color(black))
graph save Graph "~agefirst.gph", replace

graph bar (mean) localborn if n_obs_tds2 == 1, over(legacyscale) bar(1, fcolor(gs14) lcolor(black) lwidth(thin)) ytitle(, size(zero)) title(Born Locally, color(black))
graph save Graph "~localborn.gph", replace

**Figure 4**
graph bar (mean) educode_2 if n_obs_tds2 == 1, over(legacyscale) bar(1, fcolor(gs14) lcolor(black) lwidth(thin)) ytitle(, size(zero)) title(Post-Secondary, color(black))
graph save Graph "~educode_2.gph", replace

graph bar (mean) educode_3 if n_obs_tds2 == 1, over(legacyscale) bar(1, fcolor(gs14) lcolor(black) lwidth(thin)) ytitle(, size(zero)) title(Post-Graduate, color(black))
graph save Graph "~educode_3.gph", replace

graph bar (mean) ucd if n_obs_tds2 == 1, over(legacyscale) bar(1, fcolor(gs14) lcolor(black) lwidth(thin)) ytitle(, size(zero)) title(UCD, color(black))
graph save Graph "~ucd.gph", replace

graph bar (mean) feeschool if n_obs_tds2 == 1, over(legacyscale) bar(1, fcolor(gs14) lcolor(black) lwidth(thin)) ytitle(, size(zero)) title(Fee School, color(black))
graph save Graph "~feeschool.gph", replace

**Figure 6**
graph bar (mean) generation if n_obs_tds2 == 1 & pre_mp ==1, over(legacyscale) bar(1, fcolor(gs14) lcolor(black) lwidth(thin)) ytitle(, size(zero)) title(Generation, color(black))
graph save Graph "~generation.gph", replace

graph bar (mean) samename if n_obs_tds2 == 1 & pre_mp ==1, over(legacyscale) bar(1, fcolor(gs14) lcolor(black) lwidth(thin)) ytitle(, size(zero)) title(Same Name, color(black))
graph save Graph "~samename.gph", replace

graph bar (mean) samedistrict if n_obs_tds2 == 1 & pre_mp ==1, over(legacyscale) bar(1, fcolor(gs14) lcolor(black) lwidth(thin)) ytitle(, size(zero)) title(Same District, color(black))
graph save Graph "~samedistrict.gph", replace

***********************DYNASTY STRENGTH TESTS (Table 6)*************************

estimates clear
areg cabappt i.legacyscale cwins cwins2 generation if pre_mp ==1, absorb(party_dail) cluster(pid)
eststo
estadd local PartyYearFE "Yes"
areg cabappt i.legacyscale cwins cwins2 samename if pre_mp ==1, absorb(party_dail) cluster(pid)
eststo
estadd local PartyYearFE "Yes"
areg cabappt i.legacyscale cwins cwins2 samedistrict if pre_mp ==1, absorb(party_dail) cluster(pid)
eststo
estadd local PartyYearFE "Yes"

esttab using table6.tex, replace style(tex) r2 se nostar b(%9.3f) se(%9.3f) ///
 nomtitles nonotes stats(N r2 PartyYearFE, labels ("N" "R2" "Party-Year FE")) ///
prehead("\begin{tabular}{l*{@M}{rr}}" "\hline") posthead(\hline) postfoot("\hline" "\end{tabular}")  


*********************PARTY REGIONAL STRENGTH (Table 7)**************************

gen region = district
replace region = 	"Carlow-Kildare-Kilkenny"	if district ==	"Carlow Kildare"
replace region = 	"Carlow-Kildare-Kilkenny"	if district ==	"Carlow Kilkenny"
replace region = 	"Carlow-Kildare-Kilkenny"	if district ==	"Kildare"
replace region = 	"Carlow-Kildare-Kilkenny"	if district ==	"Kildare North"
replace region = 	"Carlow-Kildare-Kilkenny"	if district ==	"Kildare South"
replace region = 	"Cavan-Monaghan"	if district ==	"Cavan"
replace region = 	"Cavan-Monaghan"	if district ==	"Cavan Monaghan"
replace region = 	"Cavan-Monaghan"	if district ==	"Monaghan"
replace region = 	"Clare-Galway"	if district ==	"Clare"
replace region = 	"Clare-Galway"	if district ==	"Clare Galway South"
replace region = 	"Cork"	if district ==	"Cork Borough"
replace region = 	"Cork"	if district ==	"Cork City"
replace region = 	"Cork"	if district ==	"Cork City North"
replace region = 	"Cork"	if district ==	"Cork City South"
replace region = 	"Cork"	if district ==	"Cork East"
replace region = 	"Cork"	if district ==	"Cork Mid"
replace region = 	"Cork"	if district ==	"Cork North"
replace region = 	"Cork"	if district ==	"Cork North Central"
replace region = 	"Cork"	if district ==	"Cork North East"
replace region = 	"Cork"	if district ==	"Cork North West"
replace region = 	"Cork"	if district ==	"Cork South"
replace region = 	"Cork"	if district ==	"Cork South Central"
replace region = 	"Cork"	if district ==	"Cork South East"
replace region = 	"Cork"	if district ==	"Cork South West"
replace region = 	"Cork"	if district ==	"Cork West"
replace region = 	"Donegal"	if district ==	"Donegal"
replace region = 	"Donegal"	if district ==	"Donegal East"
replace region = 	"Donegal"	if district ==	"Donegal Leitrim"
replace region = 	"Donegal"	if district ==	"Donegal North East"
replace region = 	"Donegal"	if district ==	"Donegal South West"
replace region = 	"Donegal"	if district ==	"Donegal West"
replace region = 	"Dublin"	if district ==	"Dublin Artane"
replace region = 	"Dublin"	if district ==	"Dublin Ballyfermot"
replace region = 	"Dublin"	if district ==	"Dublin Cabra"
replace region = 	"Dublin"	if district ==	"Dublin Central"
replace region = 	"Dublin"	if district ==	"Dublin Clontarf"
replace region = 	"Dublin"	if district ==	"Dublin County"
replace region = 	"Dublin"	if district ==	"Dublin County Mid"
replace region = 	"Dublin"	if district ==	"Dublin County North"
replace region = 	"Dublin"	if district ==	"Dublin County South"
replace region = 	"Dublin"	if district ==	"Dublin County West"
replace region = 	"Dublin"	if district ==	"Dublin Finglas"
replace region = 	"Dublin"	if district ==	"Dublin Mid West"
replace region = 	"Dublin"	if district ==	"Dublin North"
replace region = 	"Dublin"	if district ==	"Dublin North Central"
replace region = 	"Dublin"	if district ==	"Dublin North East"
replace region = 	"Dublin"	if district ==	"Dublin North West"
replace region = 	"Dublin"	if district ==	"Dublin Rathmines"
replace region = 	"Dublin"	if district ==	"Dublin South"
replace region = 	"Dublin"	if district ==	"Dublin South Central"
replace region = 	"Dublin"	if district ==	"Dublin South East"
replace region = 	"Dublin"	if district ==	"Dublin South West"
replace region = 	"Dublin"	if district ==	"Dublin Townships"
replace region = 	"Dublin"	if district ==	"Dublin West"
replace region = 	"Dublin"	if district ==	"Dun Laoghaire"
replace region = 	"Dublin"	if district ==	"Dun Laoghaire Rathdown"
replace region = 	"Clare-Galway"	if district ==	"Galway East"
replace region = 	"Clare-Galway"	if district ==	"Galway North"
replace region = 	"Clare-Galway"	if district ==	"Galway North East"
replace region = 	"Clare-Galway"	if district ==	"Galway South"
replace region = 	"Clare-Galway"	if district ==	"Galway West"
replace region = 	"Kerry"	if district ==	"Kerry North"
replace region = 	"Kerry"	if district ==	"Kerry North-West Limerick"
replace region = 	"Kerry"	if district ==	"Kerry South"
replace region = 	"Laois-Offaly"	if district ==	"Laoighis Offaly"
replace region = 	"Laois-Offaly"	if district ==	"Leix Offaly"
replace region = 	"Limerick"	if district ==	"Limerick"
replace region = 	"Limerick"	if district ==	"Limerick City"
replace region = 	"Limerick"	if district ==	"Limerick County"
replace region = 	"Limerick"	if district ==	"Limerick East"
replace region = 	"Limerick"	if district ==	"Limerick West"
replace region = 	"Louth"	if district ==	"Louth"
replace region = 	"Mayo"	if district ==	"Mayo"
replace region = 	"Mayo"	if district ==	"Mayo East"
replace region = 	"Mayo"	if district ==	"Mayo North"
replace region = 	"Mayo"	if district ==	"Mayo South"
replace region = 	"Mayo"	if district ==	"Mayo West"
replace region = 	"Meath-Westmeath"	if district ==	"Athlone Longford"
replace region = 	"Meath-Westmeath"	if district ==	"Longford Westmeath"
replace region = 	"Meath-Westmeath"	if district ==	"Meath"
replace region = 	"Meath-Westmeath"	if district ==	"Meath East"
replace region = 	"Meath-Westmeath"	if district ==	"Meath West"
replace region = 	"Meath-Westmeath"	if district ==	"Meath Westmeath"
replace region = 	"Meath-Westmeath"	if district ==	"Westmeath"
replace region = 	"Sligo-Leitrim-Roscommon"	if district ==	"Leitrim"
replace region = 	"Sligo-Leitrim-Roscommon"	if district ==	"Longford Roscommon"
replace region = 	"Sligo-Leitrim-Roscommon"	if district ==	"Roscommon"
replace region = 	"Sligo-Leitrim-Roscommon"	if district ==	"Roscommon Leitrim"
replace region = 	"Sligo-Leitrim-Roscommon"	if district ==	"Roscommon Leitrim South"
replace region = 	"Sligo-Leitrim-Roscommon"	if district ==	"Sligo"
replace region = 	"Sligo-Leitrim-Roscommon"	if district ==	"Sligo Leitrim"
replace region = 	"Sligo-Leitrim-Roscommon"	if district ==	"Sligo Leitrim North"
replace region = 	"Tipperary"	if district ==	"Tipperary"
replace region = 	"Tipperary"	if district ==	"Tipperary North"
replace region = 	"Tipperary"	if district ==	"Tipperary South"
replace region = 	"Waterford"	if district ==	"Waterford"
replace region = 	"Wexford"	if district ==	"Wexford"
replace region = 	"Wicklow"	if district ==	"Wicklow"

encode region, gen(regionid)
egen party_dist = group(partyid districtid)
egen party_region = group(partyid regionid)
egen party_dist_dail = group(partyid districtid dail)
egen party_region_dail = group(partyid regionid dail)

estimates clear
areg cabappt i.legacyscale, absorb(party_region_dail) cluster(pid)
eststo
estadd local PartyYearFE "No"
estadd local PartyRegionYearFE "Yes"
estadd local Controls "No"
areg cabappt i.legacyscale cwins cwins2, absorb(party_region_dail) cluster(pid)
eststo
estadd local PartyYearFE "No"
estadd local PartyRegionYearFE "Yes"
estadd local Controls "Yes"

esttab using table7.tex, replace style(tex) r2 se nostar b(%9.3f) se(%9.3f) ///
 nomtitles nonotes stats(N r2 PartyRegionYearFE Controls, labels ("N" "R2" "Party-Region-Year FE" "Controls")) ///
prehead("\begin{tabular}{l*{@M}{rr}}" "\hline") posthead(\hline) postfoot("\hline" "\end{tabular}")  

**********************LSEM approach (Appendix Table A3)*************************
estimates clear

areg cabappt _Ilegacysca_2 cwins cwins2, absorb(party_dail) cluster(pid)
eststo
estadd local PartyYearFE "Yes"
areg quotashare _Ilegacysca_2 cwins cwins2, absorb(party_dail) cluster(pid)
eststo
estadd local PartyYearFE "Yes"
areg cabappt _Ilegacysca_2 quotashare cwins cwins2, absorb(party_dail) cluster(pid)
eststo
estadd local PartyYearFE "Yes"

esttab using appendix-tableA3.tex, replace style(tex) r2 se nostar b(%9.3f) se(%9.3f) ///
drop() nomtitles nonotes stats(N r2 PartyYearFE, labels ("N" "R2" "Party-Year FE")) ///
prehead("\begin{tabular}{l*{@M}{rr}}" "\hline") posthead(\hline) postfoot("\hline" "\end{tabular}")  

********************************************************************************

**END**

