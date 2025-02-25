* This do-file generates Table A1: Descriptive Statistics

* Each command creates one of the panels in the table.

clear
set more off

use "C:\Research\Data Municipal Elections\Files Sent to QJPS\Supplementary Materials\Table_A1.dta"

sum share1 share2 more_share2 electorate candidatos if treat==0
sum share1 share2 more_share2 electorate candidatos if treat==1
sum share1 share2 more_share2 electorate candidatos if treat==0 & bw<50000
sum share1 share2 more_share2 electorate candidatos if treat==1 & bw<50000


