import delimited "/Users/aguilars/OneDrive - University of Southern California/Teachers and Social Media/Processed CSVs/1) ESM+Survey+-+Wk+1+Monday+(first)_February+27,+2020_15.02.csv"

clear all

import delimited "/Users/aguilars/OneDrive - University of Southern California/Teachers and Social Media/Processed CSVs/2) ESM+Survey+-+Wk+1+Friday_February+27,+2020_15.00.csv"

clear all

import delimited "/Users/aguilars/OneDrive - University of Southern California/Teachers and Social Media/Processed CSVs/3) ESM+Survey+-+Wk+2+Monday_February+27,+2020_15.05.csv"

clear all

import delimited "/Users/aguilars/OneDrive - University of Southern California/Teachers and Social Media/Processed CSVs/4) ESM+Survey+-+Wk+2+Friday_February+27,+2020_15.06.csv"

clear all

import delimited "/Users/aguilars/OneDrive - University of Southern California/Teachers and Social Media/Processed CSVs/5) ESM+Survey+-+Wk+3+Monday_February+27,+2020_15.06.csv"

clear all

import delimited "/Users/aguilars/OneDrive - University of Southern California/Teachers and Social Media/Processed CSVs/6) ESM+Survey+-+Wk+3+Friday_February+27,+2020_15.07.csv"

clear all

import delimited "/Users/aguilars/OneDrive - University of Southern California/Teachers and Social Media/Processed CSVs/7) ESM+Survey+-+Wk+4+Monday_February+27,+2020_15.07.csv"

clear all

import delimited "/Users/aguilars/OneDrive - University of Southern California/Teachers and Social Media/Processed CSVs/8) ESM+Survey+-+Wk+4+Friday+(final)_February+27,+2020_15.08.csv"




merge 1:m recipientemail using "/Users/aguilars/OneDrive - University of Southern California/Teachers and Social Media/Processed CSVs/wk2d1.dta", generate(_merge2)

merge 1:m recipientemail using "/Users/aguilars/OneDrive - University of Southern California/Teachers and Social Media/Processed CSVs/wk2d2.dta", generate(_merge3)
merge 1:m recipientemail using "/Users/aguilars/OneDrive - University of Southern California/Teachers and Social Media/Processed CSVs/wk3d1.dta", generate(_merge4)
merge 1:m recipientemail using "/Users/aguilars/OneDrive - University of Southern California/Teachers and Social Media/Processed CSVs/wk3d2.dta", generate(_merge5)
merge 1:m recipientemail using "/Users/aguilars/OneDrive - University of Southern California/Teachers and Social Media/Processed CSVs/wk4d1.dta", generate(_merge6)
merge 1:m recipientemail using "/Users/aguilars/OneDrive - University of Southern California/Teachers and Social Media/Processed CSVs/wk4d2.dta", generate(_merge7)


