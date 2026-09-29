

************************** MISSING DEFINITION **********************************
****  Data set: "ZA7505_v5-0-0.dta" - Joint EVS/WVS Dataset 2017- 2022.
****   Version: 5.0.0 (2024-06-24) doi:10.4232/1.14320.
****   Sources: Joint EVS/WVS dataset can be downloaded from:
*****  GESIS: https://search.gesis.org/research_data/ZA7505?lang=en
*****  WVS: https://www.worldvaluessurvey.org/WVSEVSjoint2017.jsp



/* The current syntax converts missing values from negative numbers to 
extended missing values in STATA format and saves a new dataset with 
the suffix _miss  */

* Fill in your path below
cd ""

* Load EVS Trend file
use "ZA7505_v5-0-0.dta" , clear


* Define missing values for numeric variables
qui ds , has(type numeric)
foreach var in `r(varlist)' {
recode `var' (-1 = .a ) ///
(-2 = .b ) ///
(-3 = .c ) ///
(-4 = .d ) ///
(-5 = .e )
}

*  Define missing values for string variables
ds , has(type string)
foreach var in `r(varlist)' {
replace `var' = ".a" if `var' == "-1"
replace `var' = ".b" if `var' == "-2"
replace `var' = ".c" if `var' == "-3" 
replace `var' = ".d" if `var' == "-4" 
replace `var' = ".e" if `var' == "-5" 
}


* Attach missing value labels
qui ds , has(type numeric)
foreach var in `r(varlist)'  {
lab def `var' .a "Don't know" , modify
lab def `var' .b "No answer" , modify
lab def `var' .c "Not applicable" , modify
lab def `var' .d "Not asked in survey" , modify
lab def `var' .e "Missing: Other" , modify 
}


save "ZA7505_v5-0-0_miss.dta" , replace