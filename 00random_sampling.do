cd "C:\Users\liu\Desktop\Empirics\Raw data"

use ASIF_combined.dta, clear
set seed 12345
sample 10000, count
save sample.dta, replace
