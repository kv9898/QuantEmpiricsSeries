* Generates the results on Table 6.

* Each series of commands grouped together 
* (without a line skipped between them) generate a row of results in the 
* specified table.


clear
use "C:\Research\Data Municipal Elections\Files Sent to QJPS\Supplementary Materials\Table_6.dta"

*Panel A
sum skilled if bw<25000 & treat==0 & rank==1
sum skilled if bw<25000 & treat==0 & rank==2
sum skilled if bw<25000 & treat==0 & rank==3
sum skilled if bw<25000 & treat==0 & rank==4
		
sum hschool	if bw<25000 & treat==0 & rank==1
sum hschool	if bw<25000 & treat==0 & rank==2
sum hschool	if bw<25000 & treat==0 & rank==3
sum hschool	if bw<25000 & treat==0 & rank==4
		
sum college	if bw<25000 & treat==0 & rank==1
sum college	if bw<25000 & treat==0 & rank==2
sum college	if bw<25000 & treat==0 & rank==3
sum college	if bw<25000 & treat==0 & rank==4

* Panel B
reg skilled treat dep deptreat dy* if bw<50000 & rank==1, cl(cod_mun)
reg skilled treat dep deptreat dy* if bw<50000 & rank==2, cl(cod_mun)
reg skilled treat dep deptreat dy* if bw<50000 & rank==3, cl(cod_mun)
reg skilled treat dep deptreat dy* if bw<50000 & rank==4, cl(cod_mun)

reg hschool treat dep deptreat dy* if bw<50000 & rank==1, cl(cod_mun)
reg hschool treat dep deptreat dy* if bw<50000 & rank==2, cl(cod_mun)
reg hschool treat dep deptreat dy* if bw<50000 & rank==3, cl(cod_mun)
reg hschool treat dep deptreat dy* if bw<50000 & rank==4, cl(cod_mun)

reg college treat dep deptreat dy* if bw<50000 & rank==1, cl(cod_mun)
reg college treat dep deptreat dy* if bw<50000 & rank==2, cl(cod_mun)
reg college treat dep deptreat dy* if bw<50000 & rank==3, cl(cod_mun)
reg college treat dep deptreat dy* if bw<50000 & rank==4, cl(cod_mun)
