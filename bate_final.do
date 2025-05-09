*=========+=========+=========+=========+=========+=========+=========+====
* TITLE: Rethinking the achievement of social benefits from residential battery storage systems
* AUTHORS: H. Shimada, Y. Qiu, and T. Honda
* CODE WRITTEN BY HIDEKI SHIMADA
* LAST UPDATE: FEB 01, 2024
*=========+=========+=========+=========+=========+=========+=========+====
set more off
*--------------------------------------------------------------------------
* Define Global and Local Macros
*--------------------------------------------------------------------------
global mainwd	"your_directory"
global dofile	"$mainwd\dofile"
global data0	"$mainwd\data\0_raw_data"
global data1	"$mainwd\data\1_temporary"
global data2	"$mainwd\data\2_for-analysis"
global figures	"$mainwd\output\figures"
global tables	"$mainwd\output\tables"
*--------------------------------------------------------------------------
capture mkdir "$mainwd\dofile"
capture mkdir "$mainwd\data"
capture mkdir "$mainwd\data\0_raw_data"
capture mkdir "$mainwd\data\1_temporary"
capture mkdir "$mainwd\data\2_for-analysis"
capture mkdir "$mainwd\output"
capture mkdir "$mainwd\output\figures"
capture mkdir "$mainwd\output\tables"


/* Table of Contents
Figure 1. Locations of solar PV and battery co-adopters in our sample 
Figure 2. Charging behavior 
Figure 3. Discharging behavior 
Figure 4. Sensitivity to outdoor temperature 
Figure 5. Overview of household power flow 

Table 1. Regression of solar charging on MC of midnight charging 

Supplementary Figure 1. Hourly variations in solar generation and battery charging 
Supplementary Figure 2. Electricity exported to the grid and residual consumption
Supplementary Figure 3. Electricity market price and emission factors in 2021
Supplementary Figure 4. Electricity imported from the grid by battery and non-battery households
Supplementary Figure 5. Observed and simulated electricity imports from the grid
Supplementary Figure 6. Temperature response functions for different adoption years
Supplementary Figure 7. Temperature response with a smaller bandwidth 
Supplementary Figure 8. Share of PV-only adopters and PV and battery co-adopters 
Supplementary Figure 9. Standardized mean differences in household characteristics between battery and non-battery households 
Supplementary Figure 10. Components of the MC of midnight charging
Supplementary Figure 11. Monthly variations in the MC of midnight charging by retailer

Supplementary Table 1. Robustness of price sensitivity to weather variables
Supplementary Table 2. Robustness of price sensitivity to time window 
Supplementary Table 3. Descriptive statistics  

*/

/* data description
"$data2\main_data.dta" -- electricity data connected with weather data
"$data2\attributes_data.dta" -- attribute data
"$data2\price_data.dta" -- price data
"$data2\Hourly_emission_factor.dta" -- area-level hourly emission factors computed by Shimada et al. (2025)
"$data2\spot_summary_2021.dta" -- 30-min-level electricity market price by areas
*/

*----------------------------------------------------------
* Figure 1. Locations of solar PV and battery co-adopters
*----------------------------------------------------------
use "$data2\attributes_data.dta", clear
destring pref_id, replace

keep if year==2020
collapse (sum) bat_yn, by(pref_id)
drop if bat_yn==0

ren pref_id pref
set obs 47
local i=1
foreach p in 4 10 16 17 31 32 33 34 39 47 { // No batteries in 10 prefectures
	local ii=`i'+37
	replace pref = `p' in `ii'
	local i=`i'+1
}
replace bat_yn=0 if bat_yn==.
sort pref
g cutlist=25
replace cutlist=50 if bat_yn<50 & bat_yn>=25
replace cutlist=100 if bat_yn<100 & bat_yn>=50
replace cutlist=150 if bat_yn<150 & bat_yn>=100
replace cutlist=200 if bat_yn<200 & bat_yn>=150
maptile bat_yn, compressed geography(jpn_pref)  fcolor(GnBu) cutp(cutlist) twopt(legend(title("Co-adopter count", size(small)) lab(2 "0 - 50") lab(7 "200 -")))  
graph export "$figures\Map_number_of_coadopters.png", replace


*----------------------------------------------------------
* Figure 2(a). Average charging behavior
*----------------------------------------------------------
use "$data2\main_data.dta", clear
keep if bat_yn==1 & year==2020 // 2020 co-adopters
collapse (mean) e_use b_charge , by(id hour)
collapse (mean) m_euse=e_use m_bcharge=b_charge (semean) se_euse=e_use se_bcharge=b_charge, by(hour)
foreach x in euse bcharge {
	g ub_`x' = m_`x' + abs(invnormal(0.025))*se_`x'
	g lb_`x' = m_`x' - abs(invnormal(0.025))*se_`x'
}
twoway (rarea ub_euse lb_euse hour, color(gs10%50)) (rarea ub_bcharge lb_bcharge hour, color(gs10%50))  (line m_euse hour, color(stc1)) (line m_bcharge hour, color(stc2)) , legend(position(6) rows(1) order(4 "Charge" 3 "Consumption" 1 "95% CIs") size(medlarge)) ytitle("kWh", size(medlarge))  ylab(, format(%03.1f) labsize(medlarge)) xtitle("Hour of day", size(medlarge)) xlab(0(3)24, labsize(medlarge)) ysize(5) xsize(15) saving(graph export "$figures\Hourly_cons_and_charge_cis.png", replace)


*----------------------------------------------------------
* Figure 2(b). Heterogenous charging behavior
*----------------------------------------------------------
use "$data2\main_data.dta", clear

keep if bat_yn==1 & year==2020 // 2020 co-adopters

collapse (mean) e_use b_charge, by(id hour month_e)
reshape wide e_use b_charge, i(id month_e) j(hour)
tostring month_e, replace
replace month_e="_m" + month_e
reshape wide b_charge* e_use*, i(id) j(month_e) string

merge m:1 id using "$data2\attributes_data.dta"
keep if _merge==3


capture cluster drop cres
capture drop myclus
local l_clust=" "
local h_start=0
local h_last=23
local optk=5
forv m=1(1)12{
	forv h=`h_start'(1)`h_last'{
		local l_clust = "`l_clust'" +  " b_charge`h'_m`m'"
	}
}
cluster kmeans `l_clust', k(`optk') start(random(1101)) gen(myclus) name(cres)


keep if myclus!=.
local l_reshape=" "
forv h=0(1)23{
	local l_reshape = "`l_reshape'" +  " b_charge`h'_m" +  " e_use`h'_m"
}
reshape long `l_reshape', i(id) j(month) 
ds
forv h=`h_start'(1)`h_last'{
	ren b_charge`h'_m b_charge`h' 
	ren e_use`h'_m e_use`h' 
}
reshape long b_charge e_use, i(id month) j(hour)
keep if hour>=`h_start' & hour<=`h_last'
collapse (mean) e_use b_charge, by(id myclus hour)
g ones=1
collapse (mean) m_euse=e_use m_bcharge=b_charge (semean) se_euse=e_use se_bcharge=b_charge (sum) ones, by(myclus hour)
foreach x in euse bcharge {
	g ub_`x' = m_`x' + abs(invnormal(0.025))*se_`x'
	g lb_`x' = m_`x' - abs(invnormal(0.025))*se_`x'
}
tostring myclus ones, replace
replace myclus = myclus + " (N=" + ones + ")"
twoway (rarea ub_euse lb_euse hour, color(gs10%50) lw(medthin)) (rarea ub_bcharge lb_bcharge hour, color(gs10%50) lw(medthin)) (line m_euse hour, color(stc1)) (line m_bcharge hour, color(stc2) m(Oh)), by(myclus, legend(position(6)) note("") iscale(*1.3)) xtitle("Hour of day", size(medlarge)) ytitle("kWh", size(medlarge)) xlab(`h_start'(2)`h_last', labsize(medlarge)) ylab( , labsize(medlarge)) legend(order( 4 "Charge" 3 "Consumption" 1 "95% CIs") rows(1) size(medlarge)) ysize(5) xsize(15) saving("$figures\Res_kmeans_charge_k`optk'_cis.png", replace)


*----------------------------------------------------------
* Figure 3(a). Average discharging behavior
*----------------------------------------------------------
use "$data2\main_data.dta", clear

keep if bat_yn==1 & year==2020 // 2020 co-adopters

collapse (mean) e_use b_discharge , by(id hour)
collapse (mean) m_euse=e_use m_bdischarge=b_discharge (semean) se_euse=e_use se_bdischarge=b_discharge, by(hour)

foreach x in euse bdischarge {
	g ub_`x' = m_`x' + abs(invnormal(0.025))*se_`x'
	g lb_`x' = m_`x' - abs(invnormal(0.025))*se_`x'
}

twoway (rarea ub_euse lb_euse hour, color(gs10%50)) (rarea ub_bdischarge lb_bdischarge hour, color(gs10%50))  (line m_euse hour, color(stc1)) (line m_bdischarge hour, color(stc2)) , legend(position(6) rows(1) order(4 "Discharge" 3 "Consumption" 1 "95% CIs") size(medlarge)) ytitle("kWh", size(medlarge))  ylab(, format(%03.1f) labsize(medlarge)) xtitle("Hour of day", size(medlarge)) xlab(0(3)24, labsize(medlarge)) ysize(5) xsize(15) saving("$figures\Hourly_cons_and_discharge_cis.png", replace)


*----------------------------------------------------------
* Figure 3(b). Heterogenous discharging behavior
*----------------------------------------------------------
use "$data2\main_data.dta", clear

keep if bat_yn==1 & year==2020 // 2020 co-adopters

collapse (mean) e_use b_discharge, by(id hour month_e)
reshape wide e_use b_discharge, i(id month_e) j(hour)
tostring month_e, replace
replace month_e="_m" + month_e
reshape wide b_discharge* e_use*, i(id) j(month_e) string


capture cluster drop cres
capture drop myclus
local l_clust=" "
local h_start=0
local h_last=23
local optk=5
forv m=1(1)12{
	forv h=`h_start'(1)`h_last'{
		local l_clust = "`l_clust'" +  " b_discharge`h'_m`m'"
	}
}
cluster kmeans `l_clust', k(`optk') start(random(1101)) gen(myclus) name(cres)

keep if myclus!=.
local l_reshape=" "
forv h=0(1)23{
	local l_reshape = "`l_reshape'" +  " b_discharge`h'_m" +  " e_use`h'_m"
}
reshape long `l_reshape', i(id) j(month) 
ds
forv h=`h_start'(1)`h_last'{
	ren b_discharge`h'_m b_discharge`h' 
	ren e_use`h'_m e_use`h' 
}
reshape long b_discharge e_use, i(id month) j(hour)
keep if hour>=`h_start' & hour<=`h_last'
collapse (mean) e_use b_discharge, by(id myclus hour)
g ones=1
collapse (mean) m_euse=e_use m_bcharge=b_discharge (semean) se_euse=e_use se_bcharge=b_discharge (sum) ones, by(myclus hour)
foreach x in euse bcharge {
	g ub_`x' = m_`x' + abs(invnormal(0.025))*se_`x'
	g lb_`x' = m_`x' - abs(invnormal(0.025))*se_`x'
}
tostring myclus ones, replace
replace myclus = myclus + " (N=" + ones + ")"
twoway (rarea ub_euse lb_euse hour, color(gs10%50) lw(medthin)) (rarea ub_bcharge lb_bcharge hour, color(gs10%50) lw(medthin)) (line m_euse hour, color(stc1)) (line m_bcharge hour, color(stc2) m(Oh)), by(myclus, legend(position(6)) note("") iscale(*1.3)) xtitle("Hour of day", size(medlarge)) ytitle("kWh", size(medlarge)) xlab(`h_start'(2)`h_last', labsize(medlarge)) ylab( , labsize(medlarge)) legend(order( 4 "Discharge" 3 "Consumption" 1 "95% CIs") rows(1) size(medlarge)) ysize(5) xsize(15) saving("$figures\Res_kmeans_discharge_k`optk'_cis.png", replace)


*----------------------------------------------------------
* Figure 4. Sensitivity to outdoor temperature
*----------------------------------------------------------
use "$data2\main_data.dta", clear

keep if bat_yn==1 & year==2020 // 2020 co-adopters

egen tid = group(ymd hour)
egen gid = group(id)

capture drop ft_bin_*
g ft_bin_10 =    (f_temp<10)
g ft_bin_10_20 = (f_temp>=10 & f_temp<20)
g ft_bin_20_30 = (f_temp>=20 & f_temp<30)
g ft_bin_30_40 = (f_temp>=30 & f_temp<40)
g ft_bin_40_50 = (f_temp>=40 & f_temp<50)
g ft_bin_50_60 = (f_temp>=50 & f_temp<60)
g ft_bin_60_70 = (f_temp>=60 & f_temp<70)
g ft_bin_70_80 = (f_temp>=70 & f_temp<80)
g ft_bin_80_90 = (f_temp>=80 & f_temp<90)
g ft_bin_90 =    (f_temp>=90)
capture drop ft_bin_40_50 // Baseline temperature
foreach x of varlist ft_bin_* {
	replace `x'=. if wx_temp==.
}

frame change BatE_base_2020adopters
capture frame create res_ft
frame res_ft {
	clear
	set obs 10
	g temp_bin = _n
	g coef_e_use = .
	g se_e_use = .
	g coef_b_dc = .
	g se_b_dc = .
}

reghdfe e_use ft_bin_* wx_preci wx_humid, cluster(city_id) a(tid gid#hour)
frame res_ft {
	replace coef_e_use = _b[ft_bin_10] in 1
	replace se_e_use = _se[ft_bin_10] in 1
	forv i=1(1)8{
		local ii=`i'+1
		capture replace coef_e_use = _b[ft_bin_`i'0_`ii'0] in `ii'
		capture replace se_e_use = _se[ft_bin_`i'0_`ii'0] in `ii'
	}
	replace coef_e_use = _b[ft_bin_90] in 10
	replace se_e_use = _se[ft_bin_90] in 10
}
reghdfe b_discharge ft_bin_* wx_preci wx_humid, cluster(city_id) a(tid gid#hour)
frame res_ft {
	replace coef_b_dc = _b[ft_bin_10] in 1
	replace se_b_dc = _se[ft_bin_10] in 1
	forv i=1(1)8{
		local ii=`i'+1
		capture replace coef_b_dc = _b[ft_bin_`i'0_`ii'0] in `ii'
		capture replace se_b_dc = _se[ft_bin_`i'0_`ii'0] in `ii'
	}
	replace coef_b_dc = _b[ft_bin_90] in 10
	replace se_b_dc = _se[ft_bin_90] in 10
}


frame change res_ft
foreach x of varlist coef_* se_* {
	replace `x'=0 in 5 // Baseline
}

reshape long coef se, i(temp_bin) j(vars) string
replace vars="Consumption" if vars=="_e_use"
replace vars="Discharge" if vars=="_b_dc"
g ub = coef + 1.96*se
g lb = coef - 1.96*se

label define l_temb_bin 1 "<10" 2 "10-20" 3 "20-30" 4 "30-40" 5 "40-50" 6 "50-60" 7 "60-70" 8 "70-80" 9 "80-90" 10 ">=90", replace
label values temp_bin l_temb_bin

twoway (rarea ub lb temp_bin, color(gs10%50)) (scatter coef temp_bin, msize(small)), by(vars, note("") legend(position(6))) ylabel(, format(%03.1f) angle(horizontal)) legend(rows(1) symx(7) order(2 "Point estimates" 1 "95% CI")) yline(0, lp(shortdash) lc(blue%30) lw(medthin)) xtitle("Temperature bins", size(medlarge) height(6)) xlabel(1(1)10, val angle(45)) ysize(5) xsize(10) ytitle("kWh") saving("$figures\Temperature\Temp-sense_cons-and-discharge.png", replace)



*----------------------------------------------------------
* Table 1. Regression of solar charging on MC of midnight charging 
*----------------------------------------------------------
capture frame create Adj_price
frame change Adj_price
use "$data2\price_data.dta", clear

g elec_n = "Tokyo" if elec_tdb=="811015442"
replace elec_n = "Chubu" if elec_tdb=="400083612"
replace elec_n = "Kansai" if elec_tdb=="580111183"
replace elec_n = "Kyushu" if elec_tdb=="800035241"
replace elec_n = "Chugoku" if elec_tdb=="600028671"
replace elec_n = "Shikoku" if elec_tdb=="710029011"
replace elec_n = "Hokuriku" if elec_tdb=="370053821"
replace elec_n = "Hokkaido" if elec_tdb=="10132818"
replace elec_n = "Okinawa" if elec_tdb=="900001432"
replace elec_n = "Tohoku" if elec_tdb=="100034834"

destring year month, replace
drop ym
g ym = ym(year, month)
format ym %tmMon,_CCYY

scalar p_Kansai = 15.20 	
scalar p_Tokyo = 17.78  	 
scalar p_Chubu = 16.3   	
scalar p_Kyushu = 13.21	 	
scalar p_Tohoku = 15.88		
scalar p_Hokkaido = 17.63	
scalar p_Hokuriku = 12.50	 
scalar p_Chugoku = 14.87	 
scalar p_Shikoku = 21.64	


g p_night = 0
foreach x in Tokyo Chubu Kansai Kyushu Tohoku Hokkaido Hokuriku Chugoku Shikoku {
	replace p_night = `=p_`x'' + adj_price if elec_n=="`x'"
}

g p_night_unit = 0
foreach x in Tokyo Chubu Kansai Kyushu Tohoku Hokkaido Hokuriku Chugoku Shikoku {
	replace p_night_unit = `=p_`x''  if elec_n=="`x'"
}

keep if ym>=ym(2021,1) & ym<=ym(2022,3) 
g p_diff = p_night - 21  // FiT2020 is 21JPY/kWh
drop if elec_n=="Okinawa"


egen elec_n_id = group(elec_n) 
labmask elec_n_id, values(elec_n)


* Battery data
frame change default
use "$data2\main_data.dta", clear

keep if bat_yn==1 & year==2020 // 2020 co-adopters
drop if year_e==2022 & month_e==3 & day_e>=18 // New Fucushima crisis
bys id: egen max_soc = max(bs_soc)
drop if max_soc==0 // Some households have never charged their battery
keep if hour>=10 & hour<=14 // Time window


* To daily
collapse (sum) e_* b_* wx_preci wx_radi (mean) bs_soc elec_company_n wx_humid wx_temp, by(year_e month_e day_e id)
g ymd=mdy(month_e, day_e, year_e)
format ymd %tdDD_Mon,_CCYY
capture drop solar
g solar = b_charge/e_gen*100 // share of charged electricity; variable of interest; this may contain households who purchase electricity from the grid to charge battery
replace solar=0 if e_gen<1 // small number would expand the outcome
g solar_d = (solar>0)

keep id-solar_d
merge m:1 id using "$data2\attributes_data.dta"
keep if _merge==3
drop if month<7 // May not have enrolled in the 2020 FiT

g elec_n = "Hokkaido"
replace elec_n = "Tohoku" if elec_company_n== 2 
replace elec_n = "Tokyo" if elec_company_n== 3
replace elec_n = "Hokuriku" if elec_company_n== 4
replace elec_n = "Chubu" if elec_company_n== 5
replace elec_n = "Kansai" if elec_company_n== 6
replace elec_n = "Chugoku" if elec_company_n== 7
replace elec_n = "Shikoku" if elec_company_n== 8
replace elec_n = "Kyushu" if elec_company_n== 9


* To monthly
collapse (sum) e_* b_* wxs_preci=wx_preci wxs_radi=wx_radi (mean) bs_soc solar elec_company_n  wxm_preci=wx_preci wxm_radim=wx_radi wxm_humid=wx_humid wxm_temp=wx_temp, by(year_e month_e id)
g ym=ym(year_e, month_e)
format ym %tmMon,_CCYY

merge m:1 id using "$data2\attributes_data.dta"
keep if _merge==3

capture drop solar_d
g solar_d = (b_charge>0) 


* Analysis: exploit price difference variation
frame change BatE_base_2020adopters_daily
capture drop Adj_price p_night p_diff

g elec_n = "Hokkaido"
replace elec_n = "Tohoku" if elec_company_n== 2 
replace elec_n = "Tokyo" if elec_company_n== 3
replace elec_n = "Hokuriku" if elec_company_n== 4
replace elec_n = "Chubu" if elec_company_n== 5
replace elec_n = "Kansai" if elec_company_n== 6
replace elec_n = "Chugoku" if elec_company_n== 7
replace elec_n = "Shikoku" if elec_company_n== 8
replace elec_n = "Kyushu" if elec_company_n== 9

frlink m:1 ym elec_n, frame(Adj_price)
frget p_night p_diff, from(Adj_price)
g treat_pdiff = treat*p_diff

frame change BatE_base_2020adopters_daily
log using "$tables\reg_price_sense_main.smcl", replace
reghdfe solar p_diff, cluster(city_id) a(tid gid) 
reghdfe solar_d p_diff, cluster(city_id) a(tid gid) 
reghdfe b_charge p_diff, cluster(city_id) a(tid gid) 
reghdfe bs_soc p_diff, cluster(city_id) a(tid gid) 
log close

preserve  // Regress outcomes on price differences
	*keep if ym>ym(2021,7)
	log using "$tables\reg_price_sense_weathervars.smcl", replace
	global wxm_list wxm_temp wxm_preci wxm_humid
	reghdfe solar p_diff $wxm_list, cluster(city_id) a(tid gid) // treat treat_pdiff
	reghdfe solar_d p_diff $wxm_list, cluster(city_id) a(tid gid) // treat treat_pdiff
	reghdfe b_charge p_diff $wxm_list, cluster(city_id) a(tid gid) // treat treat_pdiff
	reghdfe bs_soc p_diff $wxm_list, cluster(city_id) a(tid gid) // treat treat_pdiff
	log close
restore






*----------------------------------------------------------
* Figure S1. Hourly variations in solar generation and battery charging 
*----------------------------------------------------------
use "$data2\main_data.dta", clear

keep if bat_yn==1 & year==2020 & year_e==2021 // 2020 co-adopters

collapse (mean) e_gen b_charge, by(hour)
twoway (connected e_gen hour, lp(shortdash) msize(medlarge) ms(Oh)) (connected b_charge hour, lp(shortdash) msize(medlarge) ms(O)), legend(position(6) rows(1) order(1 "Generation" 2 "Charge")) ytitle("kWh") xtitle("Hour of day") xlab(0(3)24) saving ("$figures\Hourly_gen_and_charge.png", replace)



*----------------------------------------------------------
* Supplementary Figure 2. Electricity exported to the grid and residual consumption
*----------------------------------------------------------
use "$data2\main_data.dta", clear

keep if bat_yn==1 & year==2020 & year_e==2021 // 2020 co-adopters


g e_resid = e_use - e_gen
replace e_resid = 0 if e_resid<0 & e_resid!=. // Residual consumption

collapse (mean) e_resid bs_soc e_self e_sell, by(id hour)

collapse (mean) m_e_resid=e_resid m_e_sell=e_sell (semean) se_e_resid=e_resid se_e_sell=e_sell, by(hour)
foreach x in e_resid e_sell {
g ub_`x' = m_`x' + abs(invnormal(0.005))*se_`x'
g lb_`x' = m_`x' - abs(invnormal(0.005))*se_`x'
}
twoway  (rarea ub_e_sell lb_e_sell hour, color(gs10%50) lw(medthin))  (rarea ub_e_resid lb_e_resid hour, color(gs10%50) lw(medthin))  (line m_e_sell hour, color(stc1)) (line m_e_resid hour, color(stc2) m(Oh)), legend(position(6) rows(1) order(3 "Export" 4 "Residual" 1 "99% CIs") size(large)) ytitle("kWh", size(large))  ylab(, format(%03.1f) labsize(large)) xtitle("Hour of day", size(large)) xlab(0(3)24, labsize(large)) ysize(5) xsize(15) 
graph export "$figures\Hourly_sell_and_residual_cis.png", replace


*----------------------------------------------------------
* Supplementary Figure 3. Electricity market price and emission factors in 2021
*----------------------------------------------------------
use "$data2\spot_summary_2021.dta", clear 
merge 1:1 ymd hour area using "$data2\Hourly_emission_factor.dta"

collapse (mean) eq_price dyn_em_coef, by(hour)
twoway  (connected eq_price hour, lp(shortdash) msize(medlarge) ms(Oh)) (connected dyn_em_coef hour, lp(shortdash) msize(medlarge) ms(O) yaxis(2)), legend(position(6) rows(1) order(1 "Average price (JPY/kWh)" 2 "Aeverage emission factor (t-CO{subscript:2}/kWh)")) ylab(0(5)25, axis(1)) ylab(0(.1).6, format(%03.1f) axis(2)) ytitle("JPY/kWh", axis(1)) ytitle("t-CO{subscript:2}/kWh", axis(2)) xlab(0(3)24, format(%3.0f))
graph export "$figures\spot_price_emission_factor_2021.png", replace


*----------------------------------------------------------
* Figure S4. Electricity imported from the grid by battery and non-battery households
*----------------------------------------------------------
use "$data2\main_data.dta", clear
keep if year==2020 & year_e==2021 // 2020 co-adopters
collapse (mean) e_buy, by(hour bat_yn)
twoway (connected e_buy hour if bat_yn==0, lp(shortdash) msize(medlarge) ms(Oh)) (connected e_buy hour if bat_yn==1, lp(shortdash) msize(medlarge) ms(O)), legend(position(6) rows(1) order(1 "PV-only adopters" 2 "PV and battery co-adopters")) ytitle("Import from the grid (kWh)") xtitle("Hour of day") xlab(0(3)24) ylab(, format(%03.1f))
graph export "$figures\Hourly_import_of_adopters_vs_non-adopters.png", replace


*----------------------------------------------------------
* Supplementary Figure 5. Observed and simulated electricity imports from the grid
*----------------------------------------------------------
use "$data2\main_data.dta", clear
keep if year==2020 & year_e==2021 // 2020 co-adopters

replace e_buy = e_buy - b_charge if bat_yn==1  // electricity purchase without batteries
collapse (mean) e_buy b_discharge, by(hour bat_yn)
reshape wide e_buy b_discharge, i(hour) j(bat_yn)
g b_discharge_sft =  .
forv hh = 1(1)24 {
	if `hh'<=9 {
		local sft = `hh'+15
	}
	if `hh'>9 {
		local sft = `hh'-9
	}
	replace b_discharge_sft = b_discharge1[`sft'] in `hh'
	}
g e_buy_simu0 = e_buy1 + b_discharge1
g e_buy_simu = e_buy1 + b_discharge1 - b_discharge_sft
g e_buy_simu2 = e_buy1 - b_discharge_sft
replace e_buy_simu=0 if e_buy_simu<0
replace e_buy_simu2=0 if e_buy_simu2<0
order hour e_buy* b_discharge*
sum e_buy*
twoway (connected e_buy1 hou, lp(shortdash) msize(medlarge) ms(Oh)) (connected e_buy_simu hour, lp(shortdash) msize(medlarge) ms(O)) , legend(position(6) rows(1) order(1 "Observed" 2 "Simulated (time shift)" 3 "Simulated (no battery)")) ytitle("Import from the grid (kWh)") xtitle("Hour of day") xlab(0(3)24) ylab(, format(%03.1f))
graph export "$figures\Hourly_import_vs_simulated_import_vs_no_battery.png", replace 


*----------------------------------------------------------
* Figure S6. Temperature response functions for different adoption years
*----------------------------------------------------------
use "$data2\main_data.dta", clear

drop if bat_yn==0
drop if year_e==2021 // 2020 and 2021 co-adopters

egen tid = group(ymd hour)
egen gid = group(id)

capture drop ft_bin_*
g ft_bin_10 =    (f_temp<10)
g ft_bin_10_20 = (f_temp>=10 & f_temp<20)
g ft_bin_20_30 = (f_temp>=20 & f_temp<30)
g ft_bin_30_40 = (f_temp>=30 & f_temp<40)
g ft_bin_40_50 = (f_temp>=40 & f_temp<50)
g ft_bin_50_60 = (f_temp>=50 & f_temp<60)
g ft_bin_60_70 = (f_temp>=60 & f_temp<70)
g ft_bin_70_80 = (f_temp>=70 & f_temp<80)
g ft_bin_80_90 = (f_temp>=80 & f_temp<90)
g ft_bin_90 =    (f_temp>=90)
capture drop ft_bin_40_50 // Baseline temperature
foreach x of varlist ft_bin_* {
	replace `x'=. if wx_temp==.
}

forv ay=2020(1)2021{
	frame change default
	capture frame create res_ft_`ay'
}
frame res_ft_`ay' {
	clear
	set obs 10
	g temp_bin = _n
	g coef_e_use = .
	g se_e_use = .
	g coef_b_dc = .
	g se_b_dc = .
}

reghdfe e_use ft_bin_* wx_preci wx_humid if year==`ay', cluster(city_id) a(tid gid#hour)
frame res_ft_`ay' {
	replace coef_e_use = _b[ft_bin_10] in 1
	replace se_e_use = _se[ft_bin_10] in 1
	forv i=1(1)8{
		local ii=`i'+1
		capture replace coef_e_use = _b[ft_bin_`i'0_`ii'0] in `ii'
		capture replace se_e_use = _se[ft_bin_`i'0_`ii'0] in `ii'
	}
	replace coef_e_use = _b[ft_bin_90] in 10
	replace se_e_use = _se[ft_bin_90] in 10
}
reghdfe b_discharge ft_bin_* wx_preci wx_humid if year==`ay', cluster(city_id) a(tid gid#hour)
frame res_ft_`ay' {
	replace coef_b_dc = _b[ft_bin_10] in 1
	replace se_b_dc = _se[ft_bin_10] in 1
	forv i=1(1)8{
		local ii=`i'+1
		capture replace coef_b_dc = _b[ft_bin_`i'0_`ii'0] in `ii'
		capture replace se_b_dc = _se[ft_bin_`i'0_`ii'0] in `ii'
	}
	replace coef_b_dc = _b[ft_bin_90] in 10
	replace se_b_dc = _se[ft_bin_90] in 10
}


frame change res_ft_`ay'
foreach x of varlist coef_* se_* {
	replace `x'=0 in 5 // Baseline
}

reshape long coef se, i(temp_bin) j(vars) string
replace vars="Consumption" if vars=="_e_use"
replace vars="Discharge" if vars=="_b_dc"
g ub = coef + 1.96*se
g lb = coef - 1.96*se

forv ay=2020(1)2021{
	frame change res_ft_`ay'
	label define l_temb_bin 1 "<10" 2 "10-20" 3 "20-30" 4 "30-40" 5 "40-50" 6 "50-60" 7 "60-70" 8 "70-80" 9 "80-90" 10 ">=90", replace
	drop if temp_bin==10
	label values temp_bin l_temb_bin
	twoway (rarea ub lb temp_bin, color(gs10%50)) (scatter coef temp_bin, msize(small)), by(vars, note("") legend(position(6))) ylabel(, format(%03.1f) angle(horizontal)) legend(rows(1) symx(7) order(2 "Point estimates" 1 "95% CI")) yline(0, lp(shortdash) lc(blue%30) lw(medthin)) xtitle("Temperature bins", size(medlarge) height(6)) xlabel(1(1)9, val angle(45)) ysize(5) xsize(10) ytitle("kWh")
 saving("$figures\Temp-sense_cons-and-discharge_2022e_a`ay'.png", replace)
}


*----------------------------------------------------------
* Figure S7. Temperature response with a smaller bandwidth 
*----------------------------------------------------------
use "$data2\main_data.dta", clear

keep if bat_yn==1 & year==2020 // 2020 co-adopters

egen tid = group(ymd hour)
egen gid = group(id)

capture drop ft_bin_*
g ft_bin_05 =    (f_temp<5)
g ft_bin_05_10 = (f_temp>=5 & f_temp<10)
g ft_bin_10_15 = (f_temp>=10 & f_temp<15)
g ft_bin_15_20 = (f_temp>=15 & f_temp<20)
g ft_bin_20_25 = (f_temp>=20 & f_temp<25)
g ft_bin_25_30 = (f_temp>=25 & f_temp<30)
g ft_bin_30_35 = (f_temp>=30 & f_temp<35)
g ft_bin_35_40 = (f_temp>=35 & f_temp<40)
g ft_bin_40_45 = (f_temp>=40 & f_temp<45)
g ft_bin_45_50 = (f_temp>=45 & f_temp<50)
g ft_bin_50_55 = (f_temp>=50 & f_temp<55)
g ft_bin_55_60 = (f_temp>=55 & f_temp<60)
g ft_bin_60_65 = (f_temp>=60 & f_temp<65)
g ft_bin_65_70 = (f_temp>=65 & f_temp<70)
g ft_bin_70_75 = (f_temp>=70 & f_temp<75)
g ft_bin_75_80 = (f_temp>=75 & f_temp<80)
g ft_bin_80_85 = (f_temp>=80 & f_temp<85)
g ft_bin_85_90 = (f_temp>=85 & f_temp<90)
g ft_bin_90_95 = (f_temp>=90 & f_temp<95)
g ft_bin_95 =    (f_temp>=95)
capture drop ft_bin_40_45 // Baseline temperature
foreach x of varlist ft_bin_* {
	replace `x'=. if wx_temp==.
}

frame change default
capture frame create res_ft_bw
frame res_ft_bw {
	clear
	set obs 20
	g temp_bin = _n
	g coef_e_use = .
	g se_e_use = .
	g coef_b_dc = .
	g se_b_dc = .
}

reghdfe e_use ft_bin_* wx_preci wx_humid, cluster(city_id) a(tid gid#hour)
frame res_ft_bw {
	replace coef_e_use = _b[ft_bin_05] in 1
	replace se_e_use = _se[ft_bin_05] in 1
	replace coef_e_use = _b[ft_bin_05_10] in 2
	replace se_e_use = _se[ft_bin_05_10] in 2
	forv i=10(5)90{
		local ii=`i'+5
		local iii=`i'/5+1
		capture replace coef_e_use = _b[ft_bin_`i'_`ii'] in `iii'
		capture replace se_e_use = _se[ft_bin_`i'_`ii'] in `iii'
	}
	replace coef_e_use = _b[ft_bin_95] in 20
	replace se_e_use = _se[ft_bin_95] in 20
}
reghdfe b_discharge ft_bin_* wx_preci wx_humid, cluster(city_id) a(tid gid#hour)
frame res_ft_bw {
	replace coef_b_dc = _b[ft_bin_05] in 1
	replace se_b_dc = _se[ft_bin_05] in 1
	replace coef_b_dc = _b[ft_bin_05_10] in 2
	replace se_b_dc = _se[ft_bin_05_10] in 2
	forv i=10(5)90{
		local ii=`i'+5
		local iii=`i'/5+1
		capture replace coef_b_dc = _b[ft_bin_`i'_`ii'] in `iii'
		capture replace se_b_dc = _se[ft_bin_`i'_`ii'] in `iii'
	}
	replace coef_b_dc = _b[ft_bin_95] in 20
	replace se_b_dc = _se[ft_bin_95] in 20
}


frame change res_ft_bw
foreach x of varlist coef_* se_* {
	replace `x'=0 in 9 // Baseline
}

reshape long coef se, i(temp_bin) j(vars) string
replace vars="Consumption" if vars=="_e_use"
replace vars="Discharge" if vars=="_b_dc"
g ub = coef + 1.96*se
g lb = coef - 1.96*se

label define l_temb_bin 1 "<5" 2 "5-10" 3 "10-15" 4 "15-20" 5 "20-25" 6 "25-30" 7 "30-35" 8 "35-40" 9 "40-45" 10 "45-50" 11 "50-55" 12 "55-60" 13 "60-65" 14 "65-70" 15 "70-75" 16 "75-80" 17 "80-85" 18 "85-90" 19 "90-95" 20 ">=95", replace
label values temp_bin l_temb_bin


twoway (rarea ub lb temp_bin, color(gs10%50)) (scatter coef temp_bin, msize(small)), by(vars, note("") legend(position(6))) ylabel(, format(%03.1f) angle(horizontal)) legend(rows(1) symx(7) order(2 "Point estimates" 1 "95% CI")) yline(0, lp(shortdash) lc(blue%30) lw(medthin)) xtitle("Temperature bins", size(medlarge) height(6)) xlabel(1(1)20, val angle(60)) ysize(5) xsize(10) ytitle("kWh") saving("$figures\Temp-sense_cons-and-discharge_bin5.png", replace)



*----------------------------------------------------------
* Figure S8. Share of PV-only adopters and PV and battery co-adopters 
*----------------------------------------------------------
use "$data2\attributes_data.dta", clear
drop if pv_yn==.
g nobs = 1
format ym %tm
keep if year==2020 | year==2021

collapse (sum) nobs, by(ym bat_yn)
reshape wide nobs, i(ym) j(bat_yn)
replace nobs1=0 if nobs1==.
replace nobs0=0 if nobs0==.
format ym %tmMon,_CCYY
g nobs_all = nobs1+nobs0
replace nobs1=nobs1/nobs_all*100
replace nobs0=nobs0/nobs_all*100 // 
twoway (connected nobs0 ym, lp(shortdash) msize(medlarge) ms(Oh)) (connected nobs1 ym, lp(shortdash) msize(medlarge) ms(O)),  legend(position(6) rows(1) order(1 "PV-only adopters" 2 "PV and battery co-adopters"))   ytitle("Share (%)")  xlab(720(2)743, angle(45)) xtitle("")  saving("$figures\Change_of_co-adopters_share.png", replace)



*-------------------------------------------------
* Figure S9. Standardized mean differences in household characteristics between battery and non-battery households
*-------------------------------------------------
use "$data2\attributes_data.dta", clear
keep if year==2020 // 2020 co-adopters
g ecn_kansai =  (elec_company=="3120001059632")
g ecn_chubu =   (elec_company=="3180001017428")
g ecn_chugoku = (elec_company=="4240001006753")
g ecn_kyushu =  (elec_company=="4290001007004")
g ecn_tohoku =  (elec_company=="4370001011311")
g ecn_hokkaido =(elec_company=="4430001022351")
g ecn_hokuriku =(elec_company=="7230001003022")
g ecn_tokyo =   (elec_company=="8010001166930")
g ecn_shikoku = (elec_company=="9470001001933")

global testvars capa hsize age_current area story ua_value allelec ecn_kansai ecn_chubu ecn_chugoku ecn_kyushu ecn_tohoku ecn_hokkaido ecn_hokuriku ecn_tokyo ecn_shikoku
foreach x of global testvars  {
	dis "Outcome variable: `x'"
	ranksum `x', by(bat_yn)	
	dis " "
	dis " "
}
foreach x of global testvars {
	 local l "`l' (mean) m_`x'=`x' (sd) sd_`x'=`x'"
}
collapse `l', by(bat_yn)
g ones=1
reshape wide m_* sd_*, i(ones) j(bat_yn)
foreach x of global testvars {
	 g d_`x' = (m_`x'1 - m_`x'0)/sqrt((sd_`x'1 + sd_`x'0)/2)
}
reshape long d_, i(ones) j(vars) string
replace vars="Solar PV capacity (kW)" if vars=="capa"
replace vars="Household size" if vars=="hsize"
replace vars="Age of household head" if vars=="age_current"
replace vars="Total floor area (m{superscript:2})" if vars=="area"
replace vars="# stories" if vars=="story"
replace vars="Insulation (W/m{superscript:2}k)" if vars=="ua_value"
replace vars="All electricity house" if vars=="allelec"
replace vars="Electric company: Kansai" if vars=="ecn_kansai"
replace vars="Electric company: Chubu" if vars=="ecn_chubu"
replace vars="Electric company: Chugoku" if vars=="ecn_chugoku"
replace vars="Electric company: Kyushu" if vars=="ecn_kyushu"
replace vars="Electric company: Tohoku" if vars=="ecn_tohoku"
replace vars="Electric company: Hokkaido" if vars=="ecn_hokkaido"
replace vars="Electric company: Hokuriku" if vars=="ecn_hokuriku"
replace vars="Electric company: Tokyo" if vars=="ecn_tokyo"
replace vars="Electric company: Shikoku" if vars=="ecn_shikoku"
graph dot d_, over(vars)  ylab(, format(%03.1f)) yline(0, lc(red%50) lp(solid) lw(medthin)) ytitle("Standardized mean differences") m(1, msize(medlarge))  
graph export "$figures\Diffs_in_chars_bw_adopters_and_nonadopters.png", replace


*----------------------------------------------------------
* Figure S10. Components of the MC of midnight charging
* Figure S11. Monthly variations in the MC of midnight charging by retailer
*----------------------------------------------------------
frame change Adj_price

twoway (scatter elec_n_id adj_fuel, ms(Oh) msize(medlarge)) (scatter elec_n_id p_night_unit, ms(O) msize(medlarge)),  xline(21, lp(solid) lc(gs10) lw(medthick)) xtitle("JPY/kWh")  legend(position(6) rows(1) order(1 "Fuel cost adjustment" 2 "TOU rate of midnight electricity")) ylab(1(1)9, val) ytitle("") text(9.5 21 "FiT in 2020", color(gs5))
saving("$figures\\Prices_by_companies.png", replace) // Figure S3


twoway (connected p_diff ym, lp(shortdash) msize(medium)), by(elec_n, note("")) ytitle("Marginal cost of midnight charging (JPY/kWh)") xtitle("") xlab(732(2)746, angle(45)) saving("$figures\Trend_of_adj_price_diffs_all.png", replace) // Figure S4







*----------------------------------------------------------
* Table S1. Robustness of price sensitivity to weather variables
*----------------------------------------------------------
frame change default
use "$data2\main_data.dta", clear

keep if bat_yn==1 & year==2020 // 2020 co-adopters
drop if year_e==2022 & month_e==3 & day_e>=18 // New Fucushima crisis
bys id: egen max_soc = max(bs_soc)
drop if max_soc==0 // Some households have never charged their battery
keep if hour>=10 & hour<=14 // Time window


* To daily
collapse (sum) e_* b_* wx_preci wx_radi (mean) bs_soc elec_company_n wx_humid wx_temp, by(year_e month_e day_e id)
g ymd=mdy(month_e, day_e, year_e)
format ymd %tdDD_Mon,_CCYY
capture drop solar
g solar = b_charge/e_gen*100 // share of charged electricity; variable of interest; this may contain households who purchase electricity from the grid to charge battery
replace solar=0 if e_gen<1 // small number would expand the outcome
g solar_d = (solar>0)

keep id-solar_d
merge m:1 id using "$data2\attributes_data.dta"
keep if _merge==3
drop if month<7 // May not have enrolled in the 2020 FiT

g elec_n = "Hokkaido"
replace elec_n = "Tohoku" if elec_company_n== 2 
replace elec_n = "Tokyo" if elec_company_n== 3
replace elec_n = "Hokuriku" if elec_company_n== 4
replace elec_n = "Chubu" if elec_company_n== 5
replace elec_n = "Kansai" if elec_company_n== 6
replace elec_n = "Chugoku" if elec_company_n== 7
replace elec_n = "Shikoku" if elec_company_n== 8
replace elec_n = "Kyushu" if elec_company_n== 9


* To monthly
collapse (sum) e_* b_* wxs_preci=wx_preci wxs_radi=wx_radi (mean) bs_soc solar elec_company_n  wxm_preci=wx_preci wxm_radim=wx_radi wxm_humid=wx_humid wxm_temp=wx_temp, by(year_e month_e id)
g ym=ym(year_e, month_e)
format ym %tmMon,_CCYY

merge m:1 id using "$data2\attributes_data.dta"
keep if _merge==3

capture drop solar_d
g solar_d = (b_charge>0) 


* Analysis: exploit price difference variation
capture drop Adj_price p_night p_diff

g elec_n = "Hokkaido"
replace elec_n = "Tohoku" if elec_company_n== 2 
replace elec_n = "Tokyo" if elec_company_n== 3
replace elec_n = "Hokuriku" if elec_company_n== 4
replace elec_n = "Chubu" if elec_company_n== 5
replace elec_n = "Kansai" if elec_company_n== 6
replace elec_n = "Chugoku" if elec_company_n== 7
replace elec_n = "Shikoku" if elec_company_n== 8
replace elec_n = "Kyushu" if elec_company_n== 9

frlink m:1 ym elec_n, frame(Adj_price)
frget p_night p_diff, from(Adj_price)
g treat_pdiff = treat*p_diff


log using "$tables\reg_price_sense_weathervars.smcl", replace
global wxm_list wxm_temp wxm_preci wxm_humid
reghdfe solar p_diff $wxm_list, cluster(city_id) a(tid gid) 
reghdfe solar_d p_diff $wxm_list, cluster(city_id) a(tid gid) 
reghdfe b_charge p_diff $wxm_list, cluster(city_id) a(tid gid) 
reghdfe bs_soc p_diff $wxm_list, cluster(city_id) a(tid gid) 
log close


*----------------------------------------------------------
* Table S2. Robustness of price sensitivity to time window 
*----------------------------------------------------------
use "$data2\main_data.dta", clear

keep if bat_yn==1 & year==2020 // 2020 co-adopters
drop if year_e==2022 & month_e==3 & day_e>=18 // New Fucushima crisis
bys id: egen max_soc = max(bs_soc)
drop if max_soc==0 // Some households have never charged their battery

keep if hour>=7 & hour<=17 // Time window

* To daily
collapse (sum) e_* b_* wx_preci wx_radi (mean) bs_soc elec_company_n wx_humid wx_temp, by(year_e month_e day_e id)
g ymd=mdy(month_e, day_e, year_e)
format ymd %tdDD_Mon,_CCYY
capture drop solar
g solar = b_charge/e_gen*100 // share of charged electricity
replace solar=0 if e_gen<1 // small number will lead to very large outcome
g solar_d = (solar>0)

keep id-solar_d
merge m:1 id using "$data2\attributes_data.dta"
keep if _merge==3
drop if month<7 // May not have enrolled in the 2020 FiT

g elec_n = "Hokkaido"
replace elec_n = "Tohoku" if elec_company_n== 2 
replace elec_n = "Tokyo" if elec_company_n== 3
replace elec_n = "Hokuriku" if elec_company_n== 4
replace elec_n = "Chubu" if elec_company_n== 5
replace elec_n = "Kansai" if elec_company_n== 6
replace elec_n = "Chugoku" if elec_company_n== 7
replace elec_n = "Shikoku" if elec_company_n== 8
replace elec_n = "Kyushu" if elec_company_n== 9

* To monthly
collapse (sum) e_* b_* wxs_preci=wx_preci wxs_radi=wx_radi (mean) bs_soc solar elec_company_n  wxm_preci=wx_preci wxm_radim=wx_radi wxm_humid=wx_humid wxm_temp=wx_temp, by(year_e month_e id)
g ym=ym(year_e, month_e)
format ym %tmMon,_CCYY

merge m:1 id using "$data2\attributes_data.dta"
keep if _merge==3

capture drop solar_d
g solar_d = (b_charge>0) 

* Average treatment effects
g tid=group(ym)
egen gid=group(id)

* Analysis: exploit price difference variation
capture drop Adj_price p_night p_diff

g elec_n = "Hokkaido"
replace elec_n = "Tohoku" if elec_company_n== 2 
replace elec_n = "Tokyo" if elec_company_n== 3
replace elec_n = "Hokuriku" if elec_company_n== 4
replace elec_n = "Chubu" if elec_company_n== 5
replace elec_n = "Kansai" if elec_company_n== 6
replace elec_n = "Chugoku" if elec_company_n== 7
replace elec_n = "Shikoku" if elec_company_n== 8
replace elec_n = "Kyushu" if elec_company_n== 9

frlink m:1 ym elec_n, frame(Adj_price)
frget p_night p_diff, from(Adj_price)
g treat_pdiff = treat*p_diff

log using "$tables\reg_price_sense_diff_time.smcl", replace
reghdfe solar p_diff, cluster(city_id) a(tid gid) 
reghdfe solar_d p_diff, cluster(city_id) a(tid gid)
reghdfe b_charge p_diff, cluster(city_id) a(tid gid) 
reghdfe bs_soc p_diff, cluster(city_id) a(tid gid) 
log close



*----------------------------------------------------------
* Table S3. Descriptive statistics  
*----------------------------------------------------------
use "$data2\attributes_data.dta", clear

g ecn_kansai =  (elec_company=="3120001059632")
g ecn_chubu =   (elec_company=="3180001017428")
g ecn_chugoku = (elec_company=="4240001006753")
g ecn_kyushu =  (elec_company=="4290001007004")
g ecn_tohoku =  (elec_company=="4370001011311")
g ecn_hokkaido =(elec_company=="4430001022351")
g ecn_hokuriku =(elec_company=="7230001003022")
g ecn_tokyo =   (elec_company=="8010001166930")
g ecn_shikoku = (elec_company=="9470001001933")
g agroup = 1 if bat_yn==1 & year==2020
replace agroup = 2 if bat_yn==1 & year==2021
replace agroup = 3 if bat_yn==0
tab agroup, miss
drop if agroup==.
bys agroup: sum ecn_hokuriku
dtable story age_current allelec ecn_chubu ecn_chugoku ecn_hokkaido ecn_hokuriku ecn_kansai ecn_kyushu ecn_shikoku ecn_tohoku ecn_tokyo hsize ua_value capa area, by(agroup) export("$tables\desc_2020a_2021a_pvonly.xlsx", replace) 
