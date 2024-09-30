clear all

ssc install winsor2, replace all
ssc install blindschemes, replace all
ssc install moss, replace all
set scheme plotplainblind

********************************************************************************
************************Import and Transform WDI Data***************************
********************************************************************************

import excel "Data_Extract_From_World_Development_Indicators.xlsx", sheet("Sheet1") firstrow

label variable AGLNDIRIGAGZS "Agricultural irrigated land (% of total agricultural land)"
label variable AGLNDAGRIZS "Agricultural land (% of land area)"
label variable AGLNDAGRIK2 "Agricultural land (sq. km)"
label variable AGAGRTRACNO "Agricultural machinery, tractors"
label variable AGLNDTRACZS "Agricultural machinery, tractors per 100 sq. km of arable land"
label variable TXVALAGRIZSUN "Agricultural raw materials exports (% of merchandise exports)"
label variable TMVALAGRIZSUN "Agricultural raw materials imports (% of merchandise imports)"
label variable EAPRDAGRIKD "Agriculture value added per worker (constant 2010 US$)"
label variable NVAGRTOTLZS "Agriculture, value added (% of GDP)"
label variable NVAGRTOTLKDZG "Agriculture, value added (annual % growth)"
label variable NVAGRTOTLKD "Agriculture, value added (constant 2010 US$)"
label variable AGLNDARBLZS "Arable land (% of land area)"
label variable AGLNDARBLHAPC "Arable land (hectares per person)"
label variable AGLNDARBLHA "Arable land (hectares)"
label variable AGYLDCRELKG "Cereal yield (kg per hectare)"
label variable AGPRDCRELMT "Cereal production (metric tons)"
label variable SLAGREMPLZS "Employment in agriculture (% of total employment)"
label variable SLAGREMPLFEZS "Employment in agriculture, female (% of female employment)"
label variable SLAGREMPLMAZS "Employment in agriculture, male (% of male employment)"
label variable SLINDEMPLZS "Employment in industry (% of total employment)"
label variable SLINDEMPLFEZS "Employment in industry, female (% of female employment)"
label variable SLINDEMPLMAZS "Employment in industry, male (% of male employment)"
label variable SLSRVEMPLZS "Employment in services (% of total employment)"
label variable SLSRVEMPLFEZS "Employment in services, female (% of female employment)"
label variable SLSRVEMPLMAZS "Employment in services, male (% of male employment)"
label variable TXVALFOODZSUN "Food exports (% of merchandise exports)"
label variable TMVALFOODZSUN "Food imports (% of merchandise imports)"
label variable NYGDPMKTPKDZG "GDP growth (annual %)"
label variable NYGDPPCAPKD "GDP per capita (constant 2010 US$)"
label variable NYGDPPCAPKDZG "GDP per capita growth (annual %)"
label variable AGLNDTOTLK2 "Land area (sq. km)"
label variable NYGDPMKTPCD "GDP (current US$)"
label variable SPPOPGROW "Population growth (annual %)"
label variable SPPOPTOTL "Population, total"
label variable NVSRVTETCZS "Services, etc., value added (% of GDP)"
label variable NVINDTOTLZS "Industry, value added (% of GDP)"
label variable SLTLFTOTLIN "Total labor force"

drop in 1
drop in 57

foreach var of varlist Year- SLTLFTOTLIN {
	replace `var' = "." if `var' == ".."
	}

destring AGLNDIRIGAGZS- SLTLFTOTLIN, replace

mvencode AGLNDIRIGAGZS- SLTLFTOTLIN, mv(0)

replace NVAGRTOTLKDZG = NVAGRTOTLKDZG*-1 in 43/43

moss Year, match("([0-9]+)") regex prefix(n_)

drop n_count n_pos1 n_match2 n_pos2 Year

rename n_match1 Year
label variable Year "Year"

order Year

destring Year, replace

save WBDI.dta, replace


********************************************************************************
**********Figure 1: Sectoral changes in GDP and employment**********************
********************************************************************************

gen ag = NVAGRTOTLZS
gen ms = NVAGRTOTLZS + NVINDTOTLZS
gen sr = NVAGRTOTLZS + NVINDTOTLZS + NVSRVTETCZS

label variable sr "Services (% of GDP)"
label variable ms "Industry (% of GDP)"
label variable ag "Agriculture (% of GDP)"

*Figure 1.A
twoway	(area sr Year, color(vermillion)) || (area ms Year) (area ag Year), ///
		xlabel(1960 (5) 2015, labsize(small)) ylabel(0 (25) 100, labsize(small) nogrid val) ///
		xtitle("Year") ytitle("% of GDP") text(10 1970 "{bf:Agriculture}") ///
		text(45 1985 "{bf:Industry}") text(90 2005 "{bf:Service}") legend(off)

		graph export "GDP_share.pdf", as(pdf) replace

mvdecode SLAGREMPLZS SLINDEMPLZS SLSRVEMPLZS, mv(0)

gen ag_l = SLAGREMPLZS
gen ms_l = SLAGREMPLZS + SLINDEMPLZS
gen sr_l = SLAGREMPLZS + SLINDEMPLZS + SLSRVEMPLZS

label variable sr_l "Services (% of Employment)"
label variable ms_l "Industry (% of Employment)"
label variable ag_l "Agriculture (% of Employment)"

drop if Year < 1980

*Figure 1.B
twoway	(area sr_l Year, color(vermillion)) || (area ms_l Year) (area ag_l Year), ///
		xlabel(1980 (5) 2015, labsize(small)) xscale(r(1980 2015) noextend) ylabel(0 (25) 100, labsize(small) nogrid val) ///
		xtitle("Year") ytitle("% of Employment") text(10 1985 "{bf:Agriculture}") ///
		text(70 2000 "{bf:Industry}") text(90 2010 "{bf:Service}") legend(off)

		graph export "Emp_share.pdf", as(pdf) replace

********************************************************************************
***********Figure 2: National level yield data by crop**************************
********************************************************************************

import delimited "FAOSTAT_data_11-27-2018.csv", clear

gen lnva = asinh(value/10)

save FAOSTAT.dta, replace

*Figure 2
twoway 	(line lnva year if item == "Rice, paddy", lcolor(ananas) lwidth(medthick) lpattern(solid) ) || ///
		(line lnva year if item == "Seed cotton", lcolor(vermillion) lwidth(medthick) lpattern(dash) ) || ///
		(line lnva year if item == "Castor oil seed", lcolor(sea) lwidth(medthick) lpattern(dot) ) || ///
		(line lnva year if item == "Pigeon peas", lcolor(turquoise) lwidth(medthick) lpattern(dash_dot) ) || ///
		(line lnva year if item == "Sorghum", lcolor(sky) lwidth(medthick) lpattern(shortdash) ) || ///
		(line lnva year if item == "Soybeans", lcolor(reddish) lwidth(medthick) lpattern(shortdash_dot) ) || ///
		(line lnva year if item == "Wheat", lcolor(orangebrown) lwidth(medthick) lpattern(longdash_dot)  ///
		ytitle("ln(yield)") yscale(r(3 9)) ylabel(3 4 5 6 7 8 9, labsize(small)) ///
		xtitle("Year") xscale(r(1961 2014)) xlabel(1960 1965 1970 1975 1980 1985 1990 1995 2000 2005 2010 2015, labsize(small))), ///
		legend(order(3 2 4 1 5 6 7) label(3 "Castor") label(2 "Cotton") label(4 "Pigeon pea") label(1 "Rice") label(5 "Sorghum") ///
		label(6 "Soybean") label(7 "Wheat") position(6) col(4) row(2) )

		graph export "tot_yield.pdf", as(pdf) replace

********************************************************************************
*****************Figure 3: Value added per hectare and worker********************
********************************************************************************

use WBDI.dta, clear

gen lny = asinh(NVAGRTOTLKD/(AGLNDAGRIK2*100))
gen lnl = asinh(EAPRDAGRIKD)
replace lnl = . if lnl == 0

*Figure 3
twoway 	(line lny Year if Year > 1979, lcolor(dknavy) lwidth(medthick) ) || ///
		(line lnl Year if Year > 1979, lcolor(sienna) lwidth(medthick) ///
		ytitle("ln(value added)") yscale(r(6 11)) ylabel(6 (1) 11, labsize(small))  ///
		xtitle("Year") xscale(r(1980 2015)) xlabel(1980 (5) 2015, labsize(small))), ///
		legend(order(1 2) label(1 "Value added per hectare") label(2 "Value added per worker") position(6) col(2))

		graph export "value_added.pdf", as(pdf) replace

********************************************************************************
**************Figure 4: Specialization in Indian agriculture********************
********************************************************************************

import delimited "FAOSTAT_data_7-12-2018.csv", clear

gen croptype = 7 if item == "Anise, badian, fennel, coriander"
replace croptype = 1 if item == "Apples"
replace croptype = 7 if item == "Areca nuts"
replace croptype = 1 if item == "Bananas"
replace croptype = 2 if item == "Barley"
replace croptype = 5 if item == "Bastfibres, other"
replace croptype = 3 if item == "Beans, dry"
replace croptype = 1 if item == "Beans, green"
replace croptype = 1 if item == "Cabbages and other brassicas"
replace croptype = 7 if item == "Cashew nuts, with shell"
replace croptype = 4 if item == "Castor oil seed"
replace croptype = 1 if item == "Cauliflowers and broccoli"
replace croptype = 1 if item == "Cherries"
replace croptype = 3 if item == "Chick peas"
replace croptype = 7 if item == "Chillies and peppers, dry"
replace croptype = 5 if item == "Coconuts"
replace croptype = 7 if item == "Coffee, green"
replace croptype = 5 if item == "Cotton lint"
replace croptype = 5 if item == "Cottonseed"
replace croptype = 6 if item == "Eggs, hen, in shell"
replace croptype = 1 if item == "Fruit, citrus nes"
replace croptype = 1 if item == "Fruit, fresh nes"
replace croptype = 1 if item == "Fruit, tropical fresh nes"
replace croptype = 7 if item == "Garlic"
replace croptype = 7 if item == "Ginger"
replace croptype = 1 if item == "Grapes"
replace croptype = 3 if item == "Groundnuts, with shell"
replace croptype = 7 if item == "Honey, natural"
replace croptype = 5 if item == "Jute"
replace croptype = 1 if item == "Lemons and limes"
replace croptype = 3 if item == "Lentils"
replace croptype = 4 if item == "Linseed"
replace croptype = 2 if item == "Maize"
replace croptype = 1 if item == "Mangoes, mangosteens, guavas"
replace croptype = 6 if item == "Meat indigenous, cattle"
replace croptype = 6 if item == "Meat indigenous, pig"
replace croptype = 6 if item == "Meat indigenous, sheep"
replace croptype = 6 if item == "Milk, whole fresh buffalo"
replace croptype = 6 if item == "Milk, whole fresh cow"
replace croptype = 2 if item == "Millet"
replace croptype = 7 if item == "Nutmeg, mace and cardamoms"
replace croptype = 1 if item == "Onions, dry"
replace croptype = 1 if item == "Oranges"
replace croptype = 1 if item == "Papayas"
replace croptype = 1 if item == "Pears"
replace croptype = 1 if item == "Peas, dry"
replace croptype = 1 if item == "Peas, green"
replace croptype = 1 if item == "Pepper (piper spp.)"
replace croptype = 3 if item == "Pigeon peas"
replace croptype = 1 if item == "Pineapples"
replace croptype = 8 if item == "Potatoes"
replace croptype = 4 if item == "Rapeseed"
replace croptype = 2 if item == "Rice, paddy"
replace croptype = 5 if item == "Rubber, natural"
replace croptype = 4 if item == "Safflower seed"
replace croptype = 5 if item == "Seed cotton"
replace croptype = 7 if item == "Sesame seed"
replace croptype = 6 if item == "Silk-worm cocoons, reelable"
replace croptype = 2 if item == "Sorghum"
replace croptype = 3 if item == "Soybeans"
replace croptype = 7 if item == "Spices, nes"
replace croptype = 7 if item == "Sugar cane"
replace croptype = 7 if item == "Sunflower seed"
replace croptype = 8 if item == "Sweet potatoes"
replace croptype = 5 if item == "Tea"
replace croptype = 5 if item == "Tobacco, unmanufactured"
replace croptype = 1 if item == "Tomatoes"
replace croptype = 1 if item == "Vegetables, fresh nes"
replace croptype = 7 if item == "Walnuts, with shell"
replace croptype = 2 if item == "Wheat"
replace croptype = 6 if item == "Wool, greasy"
replace croptype = 1 if item == "Apricots"
replace croptype = 1 if item == "Carrots and turnips"
replace croptype = 8 if item == "Cassava"
replace croptype = 1 if item == "Chillies and peppers, green"
replace croptype = 7 if item == "Cocoa, beans"
replace croptype = 1 if item == "Cucumbers and gherkins"
replace croptype = 1 if item == "Eggplants (aubergines)"
replace croptype = 1 if item == "Figs"
replace croptype = 1 if item == "Fruit, stone nes"
replace croptype = 1 if item == "Grapefruit (inc. pomelos)"
replace croptype = 1 if item == "Lettuce and chicory"
replace croptype = 6 if item == "Meat indigenous, buffalo"
replace croptype = 6 if item == "Meat indigenous, chicken"
replace croptype = 6 if item == "Meat indigenous, duck"
replace croptype = 6 if item == "Meat indigenous, goat"
replace croptype = 6 if item == "Meat, buffalo"
replace croptype = 6 if item == "Meat, cattle"
replace croptype = 6 if item == "Meat, chicken"
replace croptype = 6 if item == "Meat, duck"
replace croptype = 6 if item == "Meat, goat"
replace croptype = 6 if item == "Meat, nes"
replace croptype = 6 if item == "Meat, pig"
replace croptype = 6 if item == "Meat, sheep"
replace croptype = 1 if item == "Melons, other (inc.cantaloupes)"
replace croptype = 6 if item == "Milk, whole fresh goat"
replace croptype = 7 if item == "Mushrooms and truffles"
replace croptype = 4 if item == "Oilseeds nes"
replace croptype = 1 if item == "Okra"
replace croptype = 1 if item == "Peaches and nectarines"
replace croptype = 1 if item == "Plums and sloes"
replace croptype = 3 if item == "Pulses, nes"
replace croptype = 1 if item == "Pumpkins, squash and gourds"
replace croptype = 1 if item == "Watermelons"

label define type 1 "Fruits & vegetables" 2 "Cereals" 3 "Legumes" 4 "Oil crops" 5 "Others" 6 "Livestock" 7 "Spices, sugar, & nuts" 8 "Roots & tubers", replace
label values croptype type
label variable croptype "Crop Type"

sort croptype year
egen cval = total(value), by(croptype year)
label variable cval "Value Added by Crop Type"

duplicates drop year croptype cval, force

egen tval = total(cval), by(year)
label variable tval "Total Value Added"

gen sval = cval/tval
label variable sval "Share of Value Added"

gen fv = sval if croptype == 1
gen cr = sval if croptype == 2
gen lg = sval if croptype == 3
gen oi = sval if croptype == 4
gen ot = sval if croptype == 5
gen lv = sval if croptype == 6
gen sp = sval if croptype == 7
gen rt = sval if croptype == 8

egen FV = max(fv), by(year)
egen CR = max(cr), by(year)
egen LG = max(lg), by(year)
egen OI = max(oi), by(year)
egen OT = max(ot), by(year)
egen LV = max(lv), by(year)
egen SP = max(sp), by(year)
egen RT = max(rt), by(year)

sort year
by year: replace FV = FV^2
by year: replace CR = CR^2
by year: replace LG = LG^2
by year: replace OI = OI^2
by year: replace OT = OT^2
by year: replace LV = LV^2
by year: replace SP = SP^2
by year: replace RT = RT^2

by year: gen simp = FV +CR +LG +OI +OT +LV +SP +RT	

duplicates drop year simp, force

*Figure 4
twoway (line simp year,  ///
		ytitle("Herfindahl Index") yscale(r(.15 .85)) ylabel(.15 (.10) .85, labsize(small))  ///
		xtitle("Year") xscale(r(1961 2014)) xlabel(1960 1965 1970 1975 1980 1985 1990 1995 2000 2005 2010 2015, labsize(small))) 

		graph export "HHI.pdf", as(pdf) replace
		
********************************************************************************
****************Figure 5: Agricultural earnings gap*****************************
********************************************************************************

use WBDI.dta, clear

gen ag = NVAGRTOTLZS
label variable ag "Agri. (% of GDP)"

gen ag_l = SLAGREMPLZS
label variable ag_l "Agri. (% of employment)"
replace ag_l = . if ag_l == 0

gen ag_agl = ag - ag_l
label variable ag_agl "Agri. earnings gap"

*Figure 5
twoway 	(line ag_agl Year if Year > 1979, lcolor(black) lwidth(medthick) ) || ///
		(line ag_l Year if Year > 1979, lcolor(emerald) lwidth(medthick)) || ///
		(line ag Year if Year > 1979, lcolor(maroon) lwidth(medthick)  ///
		ytitle("Percentage (%)") yscale(r(-50 75)) ylabel(-50 -25 0 25 50 75, labsize(small)) ///
		xtitle("Year") xscale(r(1980 2015)) xlabel(1980 (5) 2015, labsize(small))), ///
		legend(order(3 2 1) position(6) col(3) row(1) )

		graph export "conv.pdf", as(pdf) replace

********************************************************************************
**************Figure 6: Sectoral changes in income and employment***************
********************************************************************************

use "household_data.dta", clear		

*Collapse to single year values
collapse (sum) ag_days-sr_days ag_wage_inc-sr_wage_inc ag_inc-sr_inc val_area val_labor, by(tindex year)

*Generate household income share
gen tot_inc = ag_inc + ms_inc + sr_inc

replace ag_inc = (ag_inc/tot_inc)*100
replace ms_inc = (ms_inc/tot_inc)*100
replace sr_inc = (sr_inc/tot_inc)*100

gen ag = ag_inc
gen ms = ag_inc + ms_inc
gen sr = ag_inc + ms_inc + sr_inc

label variable sr "Services (% of Income)"
label variable ms "Industry (% of Income)"
label variable ag "Agriculture (% of Income)"

*Figure 6.a
twoway	(area sr tindex, color(vermillion)) || (area ms tindex) (area ag tindex), ///
		xtitle("Year") xscale(r(0 25)) xlabel(1 "1975" 5 "1979" 10 "1984" 15 "2004" 20 "2009" 25 "2014", labsize(small)) ///
		ytitle("% of Income") ylabel(0 (25) 100, labsize(small) nogrid val) ///
		text(10 5 "{bf:Agriculture}") text(65 15 "{bf:Industry}") text(90 20 "{bf:Service}") legend(off)

		graph export "VDSA_inc_share.pdf", as(pdf) replace

*Generate household employment share
gen tot_days = ag_days + ms_days + sr_days

replace ag_days = (ag_days/tot_days)*100
replace ms_days = (ms_days/tot_days)*100
replace sr_days = (sr_days/tot_days)*100

gen ag_1 = ag_days
gen ms_1 = ag_days + ms_days
gen sr_1 = ag_days + ms_days + sr_days

label variable sr_1 "Services (% of Employment)"
label variable ms_1 "Industry (% of Employment)"
label variable ag_1 "Agriculture (% of Employment)"

*Figure 6.b
twoway 	(area sr_1 tindex, color(vermillion)) || (area ms_1 tindex) (area ag_1 tindex), ///
		xtitle("Year") xscale(r(0 25)) xlabel(1 "1975" 5 "1979" 10 "1984" 15 "2004" 20 "2009" 25 "2014", labsize(small)) ///
		ytitle("% of Employment") ylabel(0 (25) 100, labsize(small) nogrid val) ///
		text(10 5 "{bf:Agriculture}") text(50 15 "{bf:Industry}") text(90 20 "{bf:Service}") legend(off)

		graph export "VDSA_emp_share.pdf", as(pdf) replace

********************************************************************************
**********Figure 7: Change in the elasticity and productivity of labor**********
********************************************************************************

use "household_data.dta", clear		

*Generate single year values
egen float hh_id = group(vdsid_hhid)
gen tot_inc = ag_inc + ms_inc + sr_inc

xtset hh_id tindex

bysort hh_id (tindex): gen g_ag_emp = (ag_days - ag_days[_n-1])/ag_days[_n-1]
bysort hh_id (tindex): gen g_ms_emp = (ms_days - ms_days[_n-1])/ms_days[_n-1]
bysort hh_id (tindex): gen g_sr_emp = (sr_days - sr_days[_n-1])/sr_days[_n-1]
bysort hh_id (tindex): gen g_emp = (tot_days - tot_days[_n-1])/tot_days[_n-1]

bysort hh_id (tindex): gen g_ag_inc = (ag_inc - ag_inc[_n-1])/ag_inc[_n-1]
bysort hh_id (tindex): gen g_ms_inc = (ms_inc - ms_inc[_n-1])/ms_inc[_n-1]
bysort hh_id (tindex): gen g_sr_inc = (sr_inc - sr_inc[_n-1])/sr_inc[_n-1]
bysort hh_id (tindex): gen g_inc = (tot_inc - tot_inc[_n-1])/tot_inc[_n-1]

winsor2 g_ag_emp- g_inc if year < 2001, replace
winsor2 g_ag_emp- g_inc if year > 1989, replace

gen Eag = g_ag_emp/g_ag_inc
gen Ems = g_ms_emp/g_ms_inc
gen Esr = g_sr_emp/g_sr_inc
gen Etot = g_emp/g_inc
gen Pag = g_ag_inc - g_ag_emp
gen Pms = g_ms_inc - g_ms_emp
gen Psr = g_sr_inc - g_sr_emp
gen Ptot = g_inc - g_emp

winsor2 Eag Ems Esr Etot Pag Pms Psr Ptot if year < 2001, replace
winsor2 Eag Ems Esr Etot Pag Pms Psr Ptot if year > 1989, replace

egen ag_70s = mean(Eag) if year < 2001
egen ag_00s = mean(Eag) if year > 1989
egen ms_70s = mean(Ems) if year < 2001
egen ms_00s = mean(Ems) if year > 1989
egen sr_70s = mean(Esr) if year < 2001
egen sr_00s = mean(Esr) if year > 1989
egen tot_70s = mean(Etot) if year < 2001
egen tot_00s = mean(Etot) if year > 1989

mvencode ag_70s ag_00s ms_70s ms_00s sr_70s sr_00s tot_70s tot_00s, mv(0)

egen Eag_70s = max(ag_70s)
egen Eag_00s = max(ag_00s)
egen Ems_70s = max(ms_70s)
egen Ems_00s = max(ms_00s)
egen Esr_70s = max(sr_70s)
egen Esr_00s = max(sr_00s)
egen Etot_70s = max(tot_70s)
egen Etot_00s = max(tot_00s)

drop ag_70s ag_00s ms_70s ms_00s sr_70s sr_00s tot_70s tot_00s

egen ag_70s = mean(Pag) if year < 2001
egen ag_00s = mean(Pag) if year > 1989
egen ms_70s = mean(Pms) if year < 2001
egen ms_00s = mean(Pms) if year > 1989
egen sr_70s = mean(Psr) if year < 2001
egen sr_00s = mean(Psr) if year > 1989
egen tot_70s = mean(Ptot) if year < 2001
egen tot_00s = mean(Ptot) if year > 1989

mvencode ag_70s ag_00s ms_70s ms_00s sr_70s sr_00s tot_70s tot_00s, mv(0)

egen Pag_70s = min(ag_70s)
egen Pag_00s = min(ag_00s)
egen Pms_70s = min(ms_70s)
egen Pms_00s = min(ms_00s)
egen Psr_70s = max(sr_70s)
egen Psr_00s = min(sr_00s)
egen Ptot_70s = min(tot_70s)
egen Ptot_00s = min(tot_00s)

drop ag_70s ag_00s ms_70s ms_00s sr_70s sr_00s tot_70s tot_00s	

replace Eag_70s = abs(Eag_70s)
replace Eag_00s = abs(Eag_00s)
replace Ems_70s = abs(Ems_70s)
replace Ems_00s = abs(Ems_00s)
replace Esr_70s = abs(Esr_70s)
replace Esr_00s = abs(Esr_00s)
replace Etot_70s = abs(Etot_70s)
replace Etot_00s = abs(Etot_00s)

*Figure 7
twoway 	(scatter Etot_70s Ptot_70s, msize(tiny) mcolor(black) msymbol(circle) msize(small)) || ///
		(scatter Etot_00s Ptot_00s, msize(tiny) mcolor(black) msymbol(circle) msize(small)) || ///
		(pcarrow Etot_70s Ptot_70s Etot_00s Ptot_00s, lcolor(black) mcolor(black))  || ///
		(scatter Eag_70s Pag_70s, msize(tiny) mcolor(sky) msymbol(Oh) msize(small)) || ///
		(scatter Eag_00s Pag_00s, msize(tiny) mcolor(sky) msymbol(Oh) msize(small)) || ///
		(pcarrow Eag_70s Pag_70s Eag_00s Pag_00s, lcolor(sky) mcolor(sea))  || ///
		(scatter Ems_70s Pms_70s, msize(tiny) mcolor(gs10) msymbol(Dh) msize(small)) || ///
		(scatter Ems_00s Pms_00s, msize(tiny) mcolor(gs10) msymbol(Dh) msize(small)) || ///
		(pcarrow Ems_70s Pms_70s Ems_00s Pms_00s, lcolor(gs10) mcolor(gs10))  || ///
		(scatter Esr_70s Psr_70s, msize(tiny) mcolor(vermillion) msymbol(Th) msize(small)) || ///
		(scatter Esr_00s Psr_00s, msize(tiny) mcolor(vermillion) msymbol(Th) msize(small)) || ///
		(pcarrow Esr_70s Psr_70s Esr_00s Psr_00s, lcolor(vermillion) mcolor(vermillion) ///
		ytitle("Elasticity of Labor") yscale(r(0 3)) ylabel(0 1 2 3, labsize(small)) ///
		xtitle("Productivity of Labor") xscale(r(-1 0.5)) xlabel(-1 -0.5 0 0.5, labsize(small))), ///
		legend(order(1 4 7 10) label(1 "Total") label(4 "Agriculture") label(7 "Industry") label(10 "Service")  )
	
		graph export "elasticity.pdf", as(pdf) replace
	
********************************************************************************
******************Figure 8: Mean seasonal yield by crop*************************
********************************************************************************

use "cultivation_data.dta", clear

*Generate log output and inputs
gen lnyield = asinh(yield)
gen lnlab = asinh(lab)
gen lnfert = asinh(fert)
gen lnirr = asinh(irr)
gen lnmech = asinh(mech)
gen lnpest = asinh(pest)
gen lnseed = asinh(seed)

*Figure 8
twoway 	(lpolyci lnyield tindex if crop == "Paddy", lcolor(ananas%80) fcolor(gs14%60) clcolor(ananas%80) ) || ///
		(lpolyci lnyield tindex if crop == "Cotton", lcolor(vermillion%80) fcolor(gs14%60) clcolor(vermillion%80) ) || ///
		(lpolyci lnyield tindex if crop == "Castor", lcolor(sea%80) fcolor(gs14%60) clcolor(sea%80) ) || ///
		(lpolyci lnyield tindex if crop == "Pigeonpea", lcolor(turquoise%80) fcolor(gs14%60) clcolor(turquoise%80) ) || ///
		(lpolyci lnyield tindex if crop == "Sorghum", lcolor(sky%80) fcolor(gs14%60) clcolor(sky%80) ) || ///
		(lpolyci lnyield tindex if crop == "Soybean", lcolor(reddish%80) fcolor(gs14%60) clcolor(reddish%80) ) || ///
		(lpolyci lnyield tindex if crop == "Wheat", lcolor(orangebrown%80) fcolor(gs14%60) clcolor(orangebrown%80)  ///
		ytitle("ln(yield)") yscale(r(3 9)) ylabel(3 4 5 6 7 8 9 , labsize(small)) ///
		xtitle("Year") xscale(r(0 48)) xlabel(0 "1975" 8 "1978" 16 "1982" 24 "2001" 32 "2005" 40 "2009" 48 "2013", labsize(small))), ///
		legend(order(6 4 8 2 10 12 14) label(6 "Castor") label(4 "Cotton") label(8 "Pigeon pea") label(2 "Rice") label(10 "Sorghum") ///
		label(12 "Soybean") label(14 "Wheat") position(6) col(4) row(2) )

		graph export "yields.pdf", as(pdf) replace

********************************************************************************
*****************Figure 9: Revenue per hectare and worker***********************
********************************************************************************

use "household_data.dta", clear		

*Generate log values
replace lny = asinh(val_area)
replace lny = . if lny == 0
replace lnl = asinh(val_labor)
replace lnl = . if lnl == 0

*Figure 9
sort tindex
twoway 	(lpolyci lny tindex, lcolor(dknavy) fcolor(gs14) clcolor(dknavy) ) || ///
		(lpolyci lnl tindex, lcolor(sienna) fcolor(gs14) clcolor(sienna) ///
		ytitle("ln(revenue)") yscale(r(5 11)) ylabel(5 (1) 11, labsize(small)) ///
		xtitle("Year") xscale(r(0 25)) xlabel(1 "1975" 5 "1979" 10 "1984" 15 "2004" 20 "2009" 25 "2014", labsize(small))), ///
		legend(order(2 4) label(2 "Revenue per hectare") label(4 "Revenue per worker days") position(6) col(2))

		graph export "vaVDSA.pdf", as(pdf) replace
		
********************************************************************************
**********Figure 10: Patterns of change in agricultural productivity************
********************************************************************************

use "cultivation_data.dta", clear

*Generate single year values
sort vdsa_hh_id year
egen farm_area = sum(plot_area), by(vdsa_hh_id year)
gen y_a = output/ plot_area
gen y_l = output/Lab_q
gen lny_a = asinh(output/ plot_area)
gen lny_l = asinh(output/ Lab_q)

sort crop year
egen avgy_a = mean(lny_a), by(crop year)
egen avgy_l = mean(lny_l), by(crop year)

sort crop
egen crops = group(crop)
label define crop 1 "castor" 2 "cotton" 3 "rice" 4 "pigeon pea" 5 "sorghum" 6 "soybean" 7 "wheat"
label values crops crop

gen y_75 = avgy_a if year==1975 & crops == 1
gen x_75 = avgy_l if year==1975 & crops == 1
gen y_13 = avgy_a if year==2013 & crops == 1
gen x_13 = avgy_l if year==2013 & crops == 1
mvencode y_75 x_75 y_13 x_13, mv(0)

egen cast_y_75 = max(y_75)
egen cast_x_75 = max(x_75)
egen cast_y_13 = max(y_13)
egen cast_x_13 = max(x_13)

drop y_75 x_75 y_13 x_13

gen y_75 = avgy_a if year==1975 & crops == 2
gen x_75 = avgy_l if year==1975 & crops == 2
gen y_13 = avgy_a if year==2013 & crops == 2
gen x_13 = avgy_l if year==2013 & crops == 2
mvencode y_75 x_75 y_13 x_13, mv(0)

egen cott_y_75 = max(y_75)
egen cott_x_75 = max(x_75)
egen cott_y_13 = max(y_13)
egen cott_x_13 = max(x_13)

drop y_75 x_75 y_13 x_13

gen y_75 = avgy_a if year==1975 & crops == 3
gen x_75 = avgy_l if year==1975 & crops == 3
gen y_13 = avgy_a if year==2013 & crops == 3
gen x_13 = avgy_l if year==2013 & crops == 3
mvencode y_75 x_75 y_13 x_13, mv(0)

egen rice_y_75 = max(y_75)
egen rice_x_75 = max(x_75)
egen rice_y_13 = max(y_13)
egen rice_x_13 = max(x_13)

drop y_75 x_75 y_13 x_13

gen y_75 = avgy_a if year==1975 & crops == 4
gen x_75 = avgy_l if year==1975 & crops == 4
gen y_13 = avgy_a if year==2013 & crops == 4
gen x_13 = avgy_l if year==2013 & crops == 4
mvencode y_75 x_75 y_13 x_13, mv(0)

egen ppea_y_75 = max(y_75)
egen ppea_x_75 = max(x_75)
egen ppea_y_13 = max(y_13)
egen ppea_x_13 = max(x_13)

drop y_75 x_75 y_13 x_13

gen y_75 = avgy_a if year==1975 & crops == 5
gen x_75 = avgy_l if year==1975 & crops == 5
gen y_13 = avgy_a if year==2013 & crops == 5
gen x_13 = avgy_l if year==2013 & crops == 5
mvencode y_75 x_75 y_13 x_13, mv(0)

egen sorg_y_75 = max(y_75)
egen sorg_x_75 = max(x_75)
egen sorg_y_13 = max(y_13)
egen sorg_x_13 = max(x_13)

drop y_75 x_75 y_13 x_13

gen y_75 = avgy_a if year==2002 & crops == 6
gen x_75 = avgy_l if year==2002 & crops == 6
gen y_13 = avgy_a if year==2013 & crops == 6
gen x_13 = avgy_l if year==2013 & crops == 6
mvencode y_75 x_75 y_13 x_13, mv(0)

egen soya_y_02 = max(y_75)
egen soya_x_02 = max(x_75)
egen soya_y_13 = max(y_13)
egen soya_x_13 = max(x_13)

drop y_75 x_75 y_13 x_13

gen y_75 = avgy_a if year==1975 & crops == 7
gen x_75 = avgy_l if year==1975 & crops == 7
gen y_13 = avgy_a if year==2013 & crops == 7
gen x_13 = avgy_l if year==2013 & crops == 7
mvencode y_75 x_75 y_13 x_13, mv(0)

egen whea_y_75 = max(y_75)
egen whea_x_75 = max(x_75)
egen whea_y_13 = max(y_13)
egen whea_x_13 = max(x_13)

drop y_75 x_75 y_13 x_13

egen toty_a = mean(lny_a), by(year)
egen toty_l = mean(lny_l), by(year)

gen y_75 = toty_a if year==1975
gen x_75 = toty_l if year==1975
gen y_13 = toty_a if year==2013
gen x_13 = toty_l if year==2013
mvencode y_75 x_75 y_13 x_13, mv(0)

egen Y_75 = max(y_75)
egen X_75 = max(x_75)
egen Y_13 = max(y_13)
egen X_13 = max(x_13)
replace y_75 = Y_75
replace x_75 = X_75
replace y_13 = Y_13
replace x_13 = X_13

drop Y_75 X_75 Y_13 X_13

*Figure 10
twoway 	(scatter avgy_a avgy_l if year==1975 & crops == 1, msize(tiny) mcolor(sea) msymbol(Oh) msize(small)) || ///
		(scatter avgy_a avgy_l if year==2013 & crops == 1, msize(tiny) mcolor(sea) msymbol(Oh) msize(small)) || ///
		(pcarrow cast_y_75 cast_x_75 cast_y_13 cast_x_13 if crops == 1, lcolor(sea) mcolor(sea)) || ///
		(scatter avgy_a avgy_l if year==1975 & crops == 2, msize(tiny) mcolor(vermillion) msymbol(Dh) msize(small)) || ///
		(scatter avgy_a avgy_l if year==2013 & crops == 2, msize(tiny) mcolor(vermillion) msymbol(Dh) msize(small)) || ///
		(pcarrow cott_y_75 cott_x_75 cott_y_13 cott_x_13 if crops == 2, lcolor(vermillion) mcolor(vermillion)) || ///
		(scatter avgy_a avgy_l if year==1975 & crops == 3, msize(tiny) mcolor(ananas) msymbol(Th) msize(small)) || ///
		(scatter avgy_a avgy_l if year==2013 & crops == 3, msize(tiny) mcolor(ananas) msymbol(Th) msize(small)) || ///		
		(pcarrow rice_y_75 rice_x_75 rice_y_13 rice_x_13 if crops == 3, lcolor(ananas) mcolor(ananas)) || ///
		(scatter avgy_a avgy_l if year==1975 & crops == 4, msize(tiny) mcolor(turquoise) msymbol(Sh) msize(small)) || ///
		(scatter avgy_a avgy_l if year==2013 & crops == 4, msize(tiny) mcolor(turquoise) msymbol(Sh) msize(small)) || ///		
		(pcarrow ppea_y_75 ppea_x_75 ppea_y_13 ppea_x_13 if crops == 4, lcolor(turquoise) mcolor(turquoise)) || ///
		(scatter avgy_a avgy_l if year==1975 & crops == 5, msize(tiny) mcolor(sky) msymbol(+) msize(small)) || ///
		(scatter avgy_a avgy_l if year==2013 & crops == 5, msize(tiny) mcolor(sky) msymbol(+) msize(small)) || ///		
		(pcarrow sorg_y_75 sorg_x_75 sorg_y_13 sorg_x_13 if crops == 5, lcolor(sky) mcolor(sky)) || ///
		(scatter avgy_a avgy_l if year==2002 & crops == 6, msize(tiny) mcolor(reddish) msymbol(X) msize(small)) || ///
		(scatter avgy_a avgy_l if year==2013 & crops == 6, msize(tiny) mcolor(reddish) msymbol(X) msize(small)) || ///		
		(pcarrow soya_y_02 soya_x_02 soya_y_13 soya_x_13 if crops == 6, lcolor(reddish) mcolor(reddish)) || ///
		(scatter avgy_a avgy_l if year==1975 & crops == 7, msize(tiny) mcolor(orangebrown) msymbol(diamond) msize(small)) || ///
		(scatter avgy_a avgy_l if year==2013 & crops == 7, msize(tiny) mcolor(orangebrown) msymbol(diamond) msize(small)) || ///		
		(pcarrow whea_y_75 whea_x_75 whea_y_13 whea_x_13 if crops == 7, lcolor(orangebrown) mcolor(orangebrown)) || ///
		(scatter toty_a toty_l if year==1975, msize(tiny) mcolor(black) msymbol(circle) msize(small)) || ///
		(scatter toty_a toty_l if year==2013, msize(tiny) mcolor(black) msymbol(circle) msize(small)) || ///
		(pcarrow y_75 x_75 y_13 x_13, lcolor(black) lwidth(medium) mcolor(black) msize(medium) ) || ///
		(function y = x + 2, range(0 4) lstyle(dot)) || (function y = x + 4, range(0 4) lstyle(dot)) || (function y = x + 6, range(0 4) lstyle(dot)) || ///
		(function y = x + 8, range(0 2) lstyle(dot)) || (function y = x - 2, range(2 4) lstyle(dot))  ||(function y=x, range(0 4) lstyle(dot) ///
		ytitle("ln(output/area)") ylabel(, nogrid) xtitle("ln(output/labor)") xlabel(, nogrid) ), ///
		legend(order(22 1 4 7 10 13 16 19) label(1 "castor") label(4 "cotton") label(7 "rice") ///
		label(10 "pigeon pea") label(13 "sorghum") label(16 "soybean") label(19 "wheat") label(22 "overall") )
						
		graph export "ag_prod.pdf", as(pdf) replace

********************************************************************************
******************Figure 11: Yield and input use over time**********************
********************************************************************************

*Generate variables for input/output graphs
gen lnyield = asinh(yield)
gen lnlab = asinh(lab)
gen lnfert = asinh(fert)
gen lnirr = asinh(irr)
gen lnmech = asinh(mech)
gen lnpest = asinh(pest)
gen lnseed = asinh(seed)

*Figure 11.a - Labor
twoway	(scatter lnyield tindex, mcolor(black%05) msize(small) mlwidth(none) msymbol(circle) ///
		ytitle("ln(yield), ln(labor)") yscale(r(0 12)) ylabel(0 2 4 6 8 10 12, labsize(small))) ///
		(scatter lnlab tindex, mcolor(vermillion%05) msize(small) mlwidth(none) msymbol(circle) ///
		xtitle("Year") xscale(r(0 50)) xlabel(0 "1975" 10 "1979" 20 "1984" 30 "2004" 40 "2009" 50 "2014", labsize(small))) ///
		(lpoly lnyield tindex, lwidth(medthick) lcolor(black) lpattern(solid)) (lpoly lnlab tindex, lwidth(medthick) lcolor(vermillion) lpattern(solid)), ///
		aspect(1) legend( order(3 "ln(yield)" 4 "ln(labor)") pos(3) region(color(none) lwidth(none)) col(1)) title("(a)")

		graph export "lnylnl.pdf", as(pdf) replace

*Figure 11.b - Fertilizer
twoway 	(scatter lnyield tindex, mcolor(black%05) msize(small) mlwidth(none) msymbol(circle) ///
		ytitle("ln(yield), ln(fertilizer)") yscale(r(0 12)) ylabel(0 2 4 6 8 10 12, labsize(small))) ///
		(scatter lnfert tindex, mcolor(sky%05) msize(small) mlwidth(none) msymbol(circle) ///
		xtitle("Year") xscale(r(0 50)) xlabel(0 "1975" 10 "1979" 20 "1984" 30 "2004" 40 "2009" 50 "2014", labsize(small))) ///
		(lpoly lnyield tindex, lwidth(medthick) lcolor(black) lpattern(solid)) (lpoly lnfert tindex, lwidth(medthick) lcolor(sky) lpattern(solid)), ///
		aspect(1) legend( order(3 "ln(yield)" 4 "ln(fertilizer)") pos(3) region(color(none) lwidth(none)) col(1)) title("(b)")

		graph export "lnylnf.pdf", as(pdf) replace

*Figure 11.c - Pesticide
twoway	(scatter lnyield tindex, mcolor(black%05) msize(small) mlwidth(none) msymbol(circle) ///
		ytitle("ln(yield), ln(pesticide)") yscale(r(0 12)) ylabel(0 2 4 6 8 10 12, labsize(small))) ///
		(scatter lnpest tindex, mcolor(sea%05) msize(small) mlwidth(none) msymbol(circle) ///
		xtitle("Year") xscale(r(0 50)) xlabel(0 "1975" 10 "1979" 20 "1984" 30 "2004" 40 "2009" 50 "2014", labsize(small))) ///
		(lpoly lnyield tindex, lwidth(medthick) lcolor(black) lpattern(solid)) (lpoly lnpest tindex, lwidth(medthick) lcolor(sea) lpattern(solid)), ///
		aspect(1) legend( order(3 "ln(yield)" 4 "ln(pesticide)") pos(3) region(color(none) lwidth(none)) col(1)) title("(c)")

		graph export "lnylnp.pdf", as(pdf) replace

*Figure 11.d - Seed
twoway	(scatter lnyield tindex, mcolor(black%05) msize(small) mlwidth(none) msymbol(circle) ///
		ytitle("ln(yield), ln(seed)") yscale(r(0 12)) ylabel(0 2 4 6 8 10 12, labsize(small))) ///
		(scatter lnseed tindex, mcolor(orangebrown%05) msize(small) mlwidth(none) msymbol(circle) ///
		xtitle("Year") xscale(r(0 50)) xlabel(0 "1975" 10 "1979" 20 "1984" 30 "2004" 40 "2009" 50 "2014", labsize(small))) ///
		(lpoly lnyield tindex, lwidth(medthick) lcolor(black) lpattern(solid)) (lpoly lnseed tindex, lwidth(medthick) lcolor(orangebrown) lpattern(solid)), ///
		aspect(1) legend( order(3 "ln(yield)" 4 "ln(seed)") pos(3) region(color(none) lwidth(none)) col(1)) title("(d)")

		graph export "lnylns.pdf", as(pdf) replace

*Figure 11.e - Irrigation
twoway 	(scatter lnyield tindex, mcolor(black%05) msize(small) mlwidth(none) msymbol(circle) ///
		ytitle("ln(yield), ln(irrigation)") yscale(r(0 12)) ylabel(0 2 4 6 8 10 12, labsize(small))) ///
		(scatter lnirr tindex, mcolor(turquoise%05) msize(small) mlwidth(none) msymbol(circle) ///
		xtitle("Year") xscale(r(0 50)) xlabel(0 "1975" 10 "1979" 20 "1984" 30 "2004" 40 "2009" 50 "2014", labsize(small))) ///
		(lpoly lnyield tindex, lwidth(medthick) lcolor(black) lpattern(solid)) (lpoly lnirr tindex, lwidth(medthick) lcolor(turquoise) lpattern(solid)), ///
		aspect(1) legend( order(3 "ln(yield)" 4 "ln(irrigation)") pos(3) region(color(none) lwidth(none)) col(1)) title("(e)")

		graph export "lnylni.pdf", as(pdf) replace

*Figure 11.f - Mechanization
twoway	(scatter lnyield tindex, mcolor(black%05) msize(small) mlwidth(none) msymbol(circle) ///
		ytitle("ln(yield), ln(mechanization)") yscale(r(0 12)) ylabel(0 2 4 6 8 10 12, labsize(small))) ///
		(scatter lnmech tindex, mcolor(reddish%05) msize(small) mlwidth(none) msymbol(circle) ///
		xtitle("Year") xscale(r(0 50)) xlabel(0 "1975" 10 "1979" 20 "1984" 30 "2004" 40 "2009" 50 "2014", labsize(small))) ///
		(lpoly lnyield tindex, lwidth(medthick) lcolor(black) lpattern(solid)) (lpoly lnmech tindex, lwidth(medthick) lcolor(reddish) lpattern(solid)), ///
		aspect(1) legend( order(3 "ln(yield)" 4 "ln(mechanization)") pos(3) region(color(none) lwidth(none)) col(1)) title("(f)")

		graph export "lnylnm.pdf", as(pdf) replace

********************************************************************************
***************Figure 12: Relative prices of inputs and output******************
********************************************************************************

*Generate log of prices
gen lnlop = asinh(Lab_w/price) 
gen lnfop = asinh(Fert_p/price) 
gen lniop = asinh(Irr_p/price) 
gen lnmop = asinh(Mech_p/price) 
gen lnpop = asinh(Pest_p/price) 
gen lnsop = asinh(Seed_p/price) 

*Figure 12
twoway 	(lpolyci lnlop tindex, lcolor(vermillion%50) fcolor(gs14%50) clcolor(vermillion%50) ) || ///
		(lpolyci lnfop tindex, lcolor(sky%50) fcolor(gs14%50) clcolor(sky%50) ) || ///
		(lpolyci lniop tindex, lcolor(turquoise%50) fcolor(gs14%50) clcolor(turquoise%50) ) || ///
		(lpolyci lnmop tindex, lcolor(reddish%50) fcolor(gs14%50) clcolor(reddish%50)) || ///
		(lpolyci lnpop tindex, lcolor(sea%50) fcolor(gs14%50) clcolor(sea%50) ) || ///
		(lpolyci lnsop tindex, lcolor(orangebrown%50) fcolor(gs14%50) clcolor(orangebrown%50) ///
		ytitle("Relative Prices") yscale(r(0 6)) ylabel(0 1 2 3 4 5 6, labsize(small)) ///
		xtitle("Year") xscale(r(0 50)) xlabel(0 "1975" 10 "1979" 20 "1984" 30 "2004" 40 "200p" 50 "2014", labsize(small))), ///
		legend( order(2 "Labor" 4 "Fertilizer" 6 "Irrigation" 8 "Mechanization" 10 "Pesticide" 12 "Seed") pos(6) ///
		region(color(none) lwidth(none)) col(3))

		graph export "relative.pdf", as(pdf) replace

		
********************************************************************************
*****************Figure 13: Specialization in household agriculture*************
********************************************************************************

save "specialization.dta", replace

*Generate HHI
sort vdsa_hh_id crop year
egen cval = total(value), by(vdsa_hh_id crop year)
label variable cval "Value of Crop"

duplicates drop vdsa_hh_id year crop cval, force

egen tval = total(cval), by(vdsa_hh_id year)
label variable tval "Total Crop Value"

gen sval = cval/tval
label variable sval "Share of Total Value"

gen cas = sval if crop == "Castor"
gen cot = sval if crop == "Cotton"
gen pad = sval if crop == "Paddy"
gen pig = sval if crop == "Pigeonpea"
gen sor = sval if crop == "Sorghum"
gen soy = sval if crop == "Soybean"
gen whe = sval if crop == "Wheat"

egen CAS = max(cas), by(vdsa_hh_id year)
egen COT = max(cot), by(vdsa_hh_id year)
egen PAD = max(pad), by(vdsa_hh_id year)
egen PIG = max(pig), by(vdsa_hh_id year)
egen SOR = max(sor), by(vdsa_hh_id year)
egen SOY = max(soy), by(vdsa_hh_id year)
egen WHE = max(whe), by(vdsa_hh_id year)

mvdecode CAS-WHE, mv(0)
mvdecode cas-whe, mv(0)
mvencode CAS-WHE, mv(0)
mvencode cas-whe, mv(0)

replace cas = CAS*100
replace cot = cas + COT*100
replace pad = cot + PAD*100
replace pig = pad + PIG*100
replace sor = pig + SOR*100
replace soy = sor + SOY*100
replace whe = soy + WHE*100

drop if year == 2014
sort tindex

sort vdsa_hh_id year
by vdsa_hh_id year: replace CAS = CAS^2
by vdsa_hh_id year: replace COT = COT^2
by vdsa_hh_id year: replace PAD = PAD^2
by vdsa_hh_id year: replace PIG = PIG^2
by vdsa_hh_id year: replace SOR = SOR^2
by vdsa_hh_id year: replace SOY = SOY^2
by vdsa_hh_id year: replace WHE = WHE^2

by vdsa_hh_id year: gen simp = CAS + COT + PAD + PIG + SOR + SOY + WHE

*Figure 13
twoway (lpolyci simp tindex,  ///
		ytitle("Herfindahl Index") yscale(r(.15 .85)) ylabel(.15 (.10) .85, labsize(small)) ///
		xtitle("Year") xscale(r(0 50)) xlabel(0 "1975" 10 "1979" 20 "1984" 30 "2004" 40 "2008" 50 "2014", labsize(small))), ///
		legend(off)

		graph export "HHI_VDSA.pdf", as(pdf) replace

********************************************************************************
*****************Figure 14: Share of production value for each crop*************
********************************************************************************

*Reload and build index
use "specialization.dta", clear

sort crop year
egen cval = total(value), by(crop year)
label variable cval "Value of Crop"

duplicates drop year crop cval, force

egen tval = total(cval), by(year)
label variable tval "Total Crop Value"

gen sval = cval/tval
label variable sval "Share of Total Value"

gen cas = sval if crop == "Castor"
gen cot = sval if crop == "Cotton"
gen pad = sval if crop == "Paddy"
gen pig = sval if crop == "Pigeonpea"
gen sor = sval if crop == "Sorghum"
gen soy = sval if crop == "Soybean"
gen whe = sval if crop == "Wheat"

egen CAS = max(cas), by(year)
egen COT = max(cot), by(year)
egen PAD = max(pad), by(year)
egen PIG = max(pig), by(year)
egen SOR = max(sor), by(year)
egen SOY = max(soy), by(year)
egen WHE = max(whe), by(year)

mvencode CAS-WHE, mv(0)
mvencode cas-whe, mv(0)

replace cas = CAS*100
replace cot = cas + COT*100
replace pad = cot + PAD*100
replace pig = pad + PIG*100
replace sor = pig + SOR*100
replace soy = sor + SOY*100
replace whe = soy + WHE*100

drop if year == 2014
sort tindex

*Figure 14
twoway  (area whe tindex, fcolor(orangebrown%90) lcolor(orangebrown) ) ///
		(area soy tindex, fcolor(reddish%90) lcolor(reddish) ) ///
		(area sor tindex, fcolor(sky%90) lcolor(sky) ) ///
		(area pig tindex, fcolor(turquoise%90) lcolor(turquoise) ) ///
		(area pad tindex, fcolor(ananas%90) lcolor(ananas) ) ///
		(area cot tindex, fcolor(vermillion%90) lcolor(vermillion) ) ///
		(area cas tindex, fcolor(sea%90) lcolor(sea) ), ///
		xscale(r(0 50)) xlabel(0 "1975" 10 "1979" 20 "1984" 30 "2004" 40 "2008" 50 "2014", labsize(small)) ///
		ylabel(0 (25) 100, alternate nogrid val) ///
		xtitle("Year") ytitle("% of Production Value") legend(label(1 "Wheat") ///
		label(2 "Soybean") label(3 "Sorghum") label(4 "Pigeon pea") label(5 "Rice") ///
		label(6 "Cotton") label(7 "Castor") position(6) col(4) row(2) order(7 6 4 5 3 2 1))

		graph export "Crop_Share.pdf", as(pdf) replace

********************************************************************************
*********************Figure 15: Agriculture earnings gap************************
********************************************************************************

use "household_data.dta", clear

*Generate share values
gen tot_inc = ag_inc + ms_inc + sr_inc

replace ag_inc = (ag_inc/tot_inc)*100
replace ms_inc = (ms_inc/tot_inc)*100
replace sr_inc = (sr_inc/tot_inc)*100

gen ag = ag_inc
gen ms = ag_inc + ms_inc
gen sr = ag_inc + ms_inc + sr_inc

label variable sr "Services (% of Income)"
label variable ms "Industry (% of Income)"
label variable ag "Agriculture (% of Income)"

replace tot_days = ag_days + ms_days + sr_days

replace ag_days = (ag_days/tot_days)*100
replace ms_days = (ms_days/tot_days)*100
replace sr_days = (sr_days/tot_days)*100

gen ag_1 = ag_days
gen ms_1 = ag_days + ms_days
gen sr_1 = ag_days + ms_days + sr_days

label variable sr_1 "Services (% of Employment)"
label variable ms_1 "Industry (% of Employment)"
label variable ag_1 "Agriculture (% of Employment)"

label variable ag "Agri. (% of income)"
label variable ag_1 "Agri. (% of employment)"

gen ag_ag1 = ag - ag_1
label variable ag_ag1 "Agri. earnings gap"

*Figure 15
twoway 	(lpolyci ag_ag1 tindex, lcolor(black%60) fcolor(gs14%60) clcolor(black%60) ) || ///
		(lpolyci ag_1 tindex, lcolor(emerald%60) fcolor(gs14%60) clcolor(emerald%60) ) || ///
		(lpolyci ag tindex, lcolor(maroon%60) fcolor(gs14%60) clcolor(maroon%60)   ///
		ytitle("Percentage (%)") yscale(r(-50 75)) ylabel(-50 -25 0 25 50 75, labsize(small)) ///
		xtitle("Year") xscale(r(0 25)) xlabel(1 "1975" 5 "1979" 10 "1984" 15 "2004" 20 "2009" 25 "2014", labsize(small))), ///
		legend(order(6 4 2) label(6 "Agri. (% of income)") label(4 "Agri. (% of employment)") label(2 "Agri. earnings gap")  ///
		position(6) col(3) row(1) )

		graph export "VDSA_conv.pdf", as(pdf) replace

********************************************************************************
***********************Figure 16: Farm household income*************************
********************************************************************************

use "WBDI.dta", clear
rename Year year
replace NYGDPPCAPKD = NYGDPPCAPKD*53.0650

merge 1:m year using "household_data.dta"
drop if _merge != 3

gen tot_inc = ag_inc + ms_inc + sr_inc

gen inc_pc = tot_inc/hh_size

sort vdsid_hhid year

egen float hh_id = group(vdsid_hhid)

xtset hh_id tindex

sort hh_id tindex
by hh_id: gen hh_gr = (hh_size/hh_size[_n-1] - 1)*100

order hh_gr, after(hh_size)

winsor2 inc_pc, replace

gen ag_pc = ag_inc/hh_size
gen ms_pc = ms_inc/hh_size
gen sr_pc = sr_inc/hh_size

winsor2 ag_pc ms_pc sr_pc, replace

gen lninc = asinh(inc_pc)

winsor2 lninc inc_pc if year < 2001, replace
winsor2 lninc inc_pc if year > 1989, replace


*Figure 16
sort tindex
twoway 	(lpoly NYGDPPCAPKD tindex, lcolor(black) ) || ///
		(lpolyci inc_pc tindex, lcolor(turquoise%60) fcolor(gs14%60) clcolor(turquoise%60) ) || ///
		(lpolyci ag_pc tindex, lcolor(sky%60) fcolor(gs14%60) clcolor(sky%60) ) || ///
		(lpolyci sr_pc tindex, lcolor(vermillion%60) fcolor(gs14%60) clcolor(vermillion%60) ) || ///
		(lpolyci ms_pc tindex, lcolor(gs10%60) fcolor(gs14%60) clcolor(gs10%60)  ///
		ytitle("Rupees") yscale(r(0 90000)) ylabel(0 20000 40000 60000 80000, labsize(small)) ///
		xtitle("Year") xscale(r(0 25)) xlabel(1 "1975" 5 "1979" 10 "1984" 15 "2004" 20 "2009" 25 "2014", labsize(small))), ///
		legend( order(1 "GDP per capita" 3 "Total income per capita" 5 "Agricultural income per capita" ///
		7 "Service income per capita" 9 "industry income per capita") pos(6) region(color(none) lwidth(none)) col(3))

		graph export "inc_pc.pdf", as(pdf) replace

/* END */