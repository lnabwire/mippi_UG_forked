clear
clear matrix
set more off
capture log close

log using "C:\Users\LNabwire\OneDrive - CGIAR\L.Nabwire\git\Maize_Uganda\mippi_UG_forked\baseline\LN_2slides_for_Lusaka23\2slides_Lusaka_Apr23.smcl", replace

import delimited "C:\Users\LNabwire\OneDrive - CGIAR\L.Nabwire\git\Maize_Uganda\mippi_UG_forked\baseline\data\public\baseline.csv", varnames(1) 

count
sort farmer_id
drop if farmer_id == ""
count // 2320 total sample

// gender of hh_head
tab gender // gender of hh_head, 2320 observations

gen sex = gender
replace sex = "1" if sex == "Female"
replace sex = "2" if sex == "Male"
label define sex 1 "Female" 2 "Male"
destring sex, replace
label values sex sex
label var sex "gender of hh head"
tab sex

// % farmers using improved varieties: Q20. Did you use any quality maize seed like **OPV or hybrid seed** in the previous season (Nsambya of 2022) on any of your plots?
gen improved = quality_use
replace improved = "1" if improved =="Yes"
replace improved = "2" if improved =="No"
destring improved, replace
recode improved (1=1) (nonmissing=0)
tab improved
mean improved
mean improved, over (sex)

// which varieties: Q28. What maize variety did you plant in second season (Nsambya) of 2022 on this ${plot_select_name} plot?
gen varieties = maize_var
replace varieties = "1" if varieties == "Bazooka"
replace varieties = "1" if varieties == "KH_series"
replace varieties = "1" if varieties == "Longe_10H"
replace varieties = "1" if varieties == "Longe_6H"
replace varieties = "1" if varieties == "Longe_7H"
replace varieties = "1" if varieties == "Other_hybrid"
replace varieties = "1" if varieties == "Longe_7R_Kayongo-go"
replace varieties = "1" if varieties == "Panner"
replace varieties = "1" if varieties == "Wema"
replace varieties = "1" if varieties == "DK"
replace varieties = "2" if varieties == "Longe_4"
replace varieties = "2" if varieties == "Longe_5"
replace varieties = "3" if varieties == "Land_Races"
replace varieties = "4" if varieties == "98"

label define varieties 1 "Hybrids" 2 "OPV" 3 "Landraces" 4 "Don't know"
destring varieties, replace
label values varieties varieties
tab varieties // Most farmers use land races (52%), then hybrids (29%), then OPV (15%), the rest--- use other hybrid (5%)
tab varieties sex, col nofreq

// used bazooka: Q21. Did you use Bazooka in the second season of **2022 (Nsambya 2022)** on any of your plots? 
tab bazo_use
tab bazo_use sex, col nofreq


// For how long in years have you been using ${maize_var}?
mean long_var // overall, farmers have used the varieties they reported for 30years
mean long_var, over (sex) 
 
mean long_var, over (varieties) // 49 years for landraces; 6 years for hybrids; 14 years for OPVs.
mean long_var if sex == 1, over (varieties) 
mean long_var if sex == 2, over (varieties)

// period of use for specific varietyplanted
gen variety = maize_var

replace variety = "1" if variety == "Bazooka"
replace variety = "2" if variety == "KH_series"
replace variety = "3" if variety == "Longe_10H"
replace variety = "4" if variety == "Longe_6H"
replace variety = "5" if variety == "Longe_7H"
replace variety = "6" if variety == "Other_hybrid"
replace variety = "7" if variety == "Longe_7R_Kayongo-go"
replace variety = "8" if variety == "Panner"
replace variety = "9" if variety == "Wema"
replace variety = "10" if variety == "DK"
replace variety = "11" if variety == "Longe_4"
replace variety = "12" if variety == "Longe_5"
replace variety = "13" if variety == "Land_Races"
destring variety, replace

label define variety 1 "Bazooka" 2 "KH_series" 3 "Longe_10H" 4 "Longe_6H" 5 "Longe_7H" 6 "Other_hybrid" 7 "Longe_7R_Kayongo-go" 8 "Panner" 9 "Wema" 10 "DK" 11 "Longe_4" 12 "Longe_5" 13 "Land_Races"

label values variety variety

mean long_var, over (variety) // period (years) that the farmer has used specific varieties
mean long_var if sex == 1, over (variety) // period (years) that the farmer has used specific varieties, female headed hhs
mean long_var if sex == 2, over (variety) // period (years) that the farmer has used specific varieties, male headed hhs

save "C:\Users\LNabwire\OneDrive - CGIAR\L.Nabwire\git\Maize_Uganda\mippi_UG_forked\baseline\LN_2slides_for_Lusaka23\2slides_Lusaka_Apr23.dta", replace

// Why farm households are using these varieties (also gender-disaggregated) // used 2019 dataset. The subsequent surveys did not have this question
clear
import delimited "C:\Users\LNabwire\OneDrive - CGIAR\L.Nabwire\git\Maize_Dairy_Ug\USAID_SME_project\Country folders\Uganda maize\data\public\wave_0\farmers.csv", varnames(1) 

count
sort id
ed
drop if date == ""


// rating the importance (1 to 5) of the reasons for using the a given variety on a given plot 

// First, varieties planted on the selected plot in 2018
replace hhmaizemaizenq48 = "1" if hhmaizemaizenq48 == "a"
replace hhmaizemaizenq48 = "2" if hhmaizemaizenq48 == "b"
replace hhmaizemaizenq48 = "3" if hhmaizemaizenq48 == "c"
replace hhmaizemaizenq48 = "4" if hhmaizemaizenq48 == "d"
replace hhmaizemaizenq48 = "5" if hhmaizemaizenq48 == "e"
replace hhmaizemaizenq48 = "6" if hhmaizemaizenq48 == "f"
replace hhmaizemaizenq48 = "7" if hhmaizemaizenq48 == "g"
replace hhmaizemaizenq48 = "8" if hhmaizemaizenq48 == "h"
replace hhmaizemaizenq48 = "9" if hhmaizemaizenq48 == "i"
replace hhmaizemaizenq48 = "10" if hhmaizemaizenq48 == "j"
replace hhmaizemaizenq48 = "11" if hhmaizemaizenq48 == "k"
replace hhmaizemaizenq48 = "12" if hhmaizemaizenq48 == "l"

destring hhmaizemaizenq48, replace

rename hhmaizemaizenq48 variety
label define variety 1 "Longe 10H" 2 "Longe 7H" 3 "Longe 7R/Kayongo-go" 4 "Bazooka" 5 "Longe 6H" 6 "Longe 5 (nalongo)" 7 "Longe 4" 8 "Panner" 9 "Wema" 10 "KH series" 11 "Land Races/local varieties" 12 "Others" 98 "Don't know"
label values variety variety
// variety planted on the selected plot
tab variety

// rating the importance (1 to 5) of the reasons for using the a given variety on a given plot 
rename hhmaizemaizenq51a highyield_m
rename hhmaizemaizenq51b drought_m
rename hhmaizemaizenq51c pestdisease_m
rename hhmaizemaizenq51d earlymaturing_m
rename hhmaizemaizenq51e highMktPx_m
rename hhmaizemaizenq51f goodtaste_m
rename hhmaizemaizenq51g lowprice_m
rename hhmaizemaizenq51h availability_m
rename hhmaizemaizenq51i germination_m
rename hhmaizemaizenq51j experience_m

label var highyield_m "High yield"
label var drought_m "Drought tolerant"
lab var pestdisease_m "Pest/disease tolerant"
lab var earlymaturing_m "Early maturing"
lab var highMktPx_m "High market price"
lab var highMktPx_m "Higher market price/demand"
lab var goodtaste_m "Good taste or high nutrition"
lab var lowprice_m "Low price for seed"
lab var availability_m "Availability"
lab var germination_m "Germination rate"
lab var experience_m "Experience(knows and trust the quality"


// 2018 mean ratings of the reasons for using a given variety
sum highyield_m- experience_m // overall sample
sum highyield_m- experience_m if hhmaizeq25 == "Female"
sum highyield_m- experience_m if hhmaizeq25 == "Male"

// per variety: 2018 mean ratings of the reasons for using a given variety 
sum highyield_m- experience_m if variety == 1 // Longe 10H
sum highyield_m- experience_m if variety == 2 // Longe 7H
sum highyield_m- experience_m if variety == 3 // Longe 7R/Kayongo-go
sum highyield_m- experience_m if variety == 4 // Bazooka
sum highyield_m- experience_m if variety == 5 // Longe 6H
sum highyield_m- experience_m if variety == 6 // Longe 5 (nalongo)
sum highyield_m- experience_m if variety == 7 // Longe 4
sum highyield_m- experience_m if variety == 8 // Panner
sum highyield_m- experience_m if variety == 9 // Wema
sum highyield_m- experience_m if variety == 10 // KH series
sum highyield_m- experience_m if variety == 11 // Land Races/local varieties


// recycling of seed: Q30. How often was this ${maize_var}  that was used on **${plot_select_name}** in the second season (Nsambya) of 2022 recycled?
use "C:\Users\LNabwire\OneDrive - CGIAR\L.Nabwire\git\Maize_Uganda\mippi_UG_forked\baseline\LN_2slides_for_Lusaka23\2slides_Lusaka_Apr23.dta", clear 

tab often
tab often sex, col nofreq
tab often if varieties == 1
tab often sex if varieties == 1, col nofreq

tab often if varieties == 2
tab often sex if varieties == 2, col nofreq

tab often if varieties == 3
tab often sex if varieties == 3, col nofreq


// Attribute of Bazooka: Please rate this  particular ${maize_var} seed you used on ${plot_select_name} plot in the second season (Nsambya) of 2022, on these dimensions:
destring gen_qlty- cook, replace ignore ("n/a")
destring group4gen_qlty1 - group4germ_rate1, replace ignore ("n/a")

gen bazo_genQlty = group4gen_qlty1
gen bazo_yield = group4yield_rate1
gen bazo_drt = group4drt_tol1
gen bazo_dies = group4dies_tol1
gen bazo_early = group4erly_mat1
gen bazo_mkt = group4mrkt_dem1
gen bazo_taste = group4taste1
gen bazo_price = group4price_rate1
gen bazo_avail = group4avail1
gen bazo_germ = group4germ_rate1

replace bazo_genQlty = gen_qlty if bazo_genQlty == . & variety == 1
replace bazo_yield = yield_rate if bazo_yield == . & variety == 1
replace bazo_drt = drt_tol if bazo_drt == . & variety == 1
replace bazo_dies = dies_tol if bazo_dies == . & variety == 1
replace bazo_early = erly_mat if bazo_early == . & variety == 1
replace bazo_mkt = mrkt_dem if bazo_mkt == . & variety == 1
replace bazo_taste = taste if bazo_taste == . & variety == 1
replace bazo_price = price_rate if bazo_price == . & variety == 1
replace bazo_avail = avail if bazo_avail == . & variety == 1
replace bazo_germ = germ_rate if bazo_germ == . & variety == 1






