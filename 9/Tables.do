* Unless specified otherwise, each series of commands grouped together 
* (without a line skipped between them) generate a row of results in the 
* specified table.

clear
set more off

use "C:\Research\Data Municipal Elections\Files Sent to QJPS\Supplementary Materials\Tables.dta"

************
*  TABLE 1 *
************ 

sum more_share2 if bw<25000 & treat==0
reg more_share2 treat dep deptreat dy* if bw<50000, cl(cod_mun)
reg more_share2 treat dep deptreat dy* if bw<25000, cl(cod_mun)
reg more_share2 treat dep deptreat dy* if bw<75000, cl(cod_mun)
reg more_share2 treat dep deptreat dep2 deptreat2 dy* if bw<50000, cl(cod_mun)
reg more_share2 treat dep deptreat dep2 deptreat2 dy* if bw<75000, cl(cod_mun)

sum more_share3 if bw<25000 & treat==0
reg more_share3 treat dep deptreat dy* if bw<50000, cl(cod_mun)
reg more_share3 treat dep deptreat dy* if bw<25000, cl(cod_mun)
reg more_share3 treat dep deptreat dy* if bw<75000, cl(cod_mun)
reg more_share3 treat dep deptreat dep2 deptreat2 dy* if bw<50000, cl(cod_mun)
reg more_share3 treat dep deptreat dep2 deptreat2 dy* if bw<75000, cl(cod_mun)

sum more_share4 if bw<25000 & treat==0
reg more_share4 treat dep deptreat dy* if bw<50000, cl(cod_mun)
reg more_share4 treat dep deptreat dy* if bw<25000, cl(cod_mun)
reg more_share4 treat dep deptreat dy* if bw<75000, cl(cod_mun)
reg more_share4 treat dep deptreat dep2 deptreat2 dy* if bw<50000, cl(cod_mun)
reg more_share4 treat dep deptreat dep2 deptreat2 dy* if bw<75000, cl(cod_mun)

sum registration_rate if bw<25000 & treat==0
reg registration_rate treat dep deptreat dy* if bw<50000, cl(cod_mun)
reg registration_rate treat dep deptreat dy* if bw<25000, cl(cod_mun)
reg registration_rate treat dep deptreat dy* if bw<75000, cl(cod_mun)
reg registration_rate treat dep deptreat dep2 deptreat2 dy* if bw<50000, cl(cod_mun)
reg registration_rate treat dep deptreat dep2 deptreat2 dy* if bw<75000, cl(cod_mun)

sum turnout_rate if bw<25000 & treat==0
reg turnout_rate treat dep deptreat dy* if bw<50000, cl(cod_mun)
reg turnout_rate treat dep deptreat dy* if bw<25000, cl(cod_mun)
reg turnout_rate treat dep deptreat dy* if bw<75000, cl(cod_mun)
reg turnout_rate treat dep deptreat dep2 deptreat2 dy* if bw<50000, cl(cod_mun)
reg turnout_rate treat dep deptreat dep2 deptreat2 dy* if bw<75000, cl(cod_mun)

************
*  TABLE 2 *
************ 

sum longitude if bw<25000 & treat==0
reg longitude treat dep deptreat dy* if bw<50000, cl(cod_mun)
reg longitude treat dep deptreat dy* if bw<25000, cl(cod_mun)
reg longitude treat dep deptreat dy* if bw<75000, cl(cod_mun)
reg longitude treat dep deptreat dep2 deptreat2 dy* if bw<50000, cl(cod_mun)
reg longitude treat dep deptreat dep2 deptreat2 dy* if bw<75000, cl(cod_mun)

sum latitude if bw<25000 & treat==0
reg latitude treat dep deptreat dy* if bw<50000, cl(cod_mun)
reg latitude treat dep deptreat dy* if bw<25000, cl(cod_mun)
reg latitude treat dep deptreat dy* if bw<75000, cl(cod_mun)
reg latitude treat dep deptreat dep2 deptreat2 dy* if bw<50000, cl(cod_mun)
reg latitude treat dep deptreat dep2 deptreat2 dy* if bw<75000, cl(cod_mun)

sum income if bw<25000 & treat==0
reg income treat dep deptreat dy* if bw<50000, cl(cod_mun)
reg income treat dep deptreat dy* if bw<25000, cl(cod_mun)
reg income treat dep deptreat dy* if bw<75000, cl(cod_mun)
reg income treat dep deptreat dep2 deptreat2 dy* if bw<50000, cl(cod_mun)
reg income treat dep deptreat dep2 deptreat2 dy* if bw<75000, cl(cod_mun)

sum gini if bw<25000 & treat==0
reg gini treat dep deptreat dy* if bw<50000, cl(cod_mun)
reg gini treat dep deptreat dy* if bw<25000, cl(cod_mun)
reg gini treat dep deptreat dy* if bw<75000, cl(cod_mun)
reg gini treat dep deptreat dep2 deptreat2 dy* if bw<50000, cl(cod_mun)
reg gini treat dep deptreat dep2 deptreat2 dy* if bw<75000, cl(cod_mun)

sum schooling if bw<25000 & treat==0
reg schooling treat dep deptreat dy* if bw<50000, cl(cod_mun)
reg schooling treat dep deptreat dy* if bw<25000, cl(cod_mun)
reg schooling treat dep deptreat dy* if bw<75000, cl(cod_mun)
reg schooling treat dep deptreat dep2 deptreat2 dy* if bw<50000, cl(cod_mun)
reg schooling treat dep deptreat dep2 deptreat2 dy* if bw<75000, cl(cod_mun)

sum rural if bw<25000 & treat==0
reg rural treat dep deptreat dy* if bw<50000, cl(cod_mun)
reg rural treat dep deptreat dy* if bw<25000, cl(cod_mun)
reg rural treat dep deptreat dy* if bw<75000, cl(cod_mun)
reg rural treat dep deptreat dep2 deptreat2 dy* if bw<50000, cl(cod_mun)
reg rural treat dep deptreat dep2 deptreat2 dy* if bw<75000, cl(cod_mun)

***********
* Table 3 * 
***********

*Logit regression that generates predicted to be contested status
logit contest lshare1 lcontest dy* if bw<75000
predict pred_cont, pr

*Numbers presented on the table: 

sum more_share2 if treat==0 & bw<25000 & year>1996 & pred_cont>0.5
reg more_share2 treat dep deptreat dy* if bw<50000 & year>1996 & pred_cont>0.5, cl(cod_mun)
reg more_share2 treat dep deptreat dy* if bw<25000 & year>1996 & pred_cont>0.5, cl(cod_mun)
reg more_share2 treat dep deptreat dy* if bw<75000 & year>1996  & pred_cont>0.5 , cl(cod_mun)
reg more_share2 treat dep deptreat dep2 deptreat2 dy* if bw<50000  & pred_cont>0.5 , cl(cod_mun)
reg more_share2 treat dep deptreat dep2 deptreat2 dy* if bw<75000 & year>1996  & pred_cont>0.5, cl(cod_mun)

sum more_share2 if treat==0 & bw<25000 & year>1996 & pred_cont<0.5
reg more_share2 treat dep deptreat dy* if bw<50000 & year>1996 & pred_cont<0.5, cl(cod_mun)
reg more_share2 treat dep deptreat dy* if bw<25000 & year>1996 & pred_cont<0.5, cl(cod_mun)
reg more_share2 treat dep deptreat dy* if bw<75000 & year>1996  & pred_cont<0.5 , cl(cod_mun)
reg more_share2 treat dep deptreat dep2 deptreat2 dy* if bw<50000 & year>1996 & pred_cont<0.5 , cl(cod_mun)
reg more_share2 treat dep deptreat dep2 deptreat2 dy* if bw<75000 & year>1996  & pred_cont<0.5, cl(cod_mun)

************
*  TABLE 4 *
************

sum more_cand3 if bw<25000 & treat==0
reg more_cand3 treat dep deptreat dy* if bw<50000, cl(cod_mun)
reg more_cand3 treat dep deptreat dy* if bw<25000, cl(cod_mun)
reg more_cand3 treat dep deptreat dy* if bw<75000, cl(cod_mun)
reg more_cand3 treat dep deptreat dep2 deptreat2 dy* if bw<50000, cl(cod_mun)
reg more_cand3 treat dep deptreat dep2 deptreat2 dy* if bw<75000, cl(cod_mun)

sum more_cand4 if bw<25000 & treat==0
reg more_cand4 treat dep deptreat dy* if bw<50000, cl(cod_mun)
reg more_cand4 treat dep deptreat dy* if bw<25000, cl(cod_mun)
reg more_cand4 treat dep deptreat dy* if bw<75000, cl(cod_mun)
reg more_cand4 treat dep deptreat dep2 deptreat2 dy* if bw<50000, cl(cod_mun)
reg more_cand4 treat dep deptreat dep2 deptreat2 dy* if bw<75000, cl(cod_mun)

sum more_cand5 if bw<25000 & treat==0
reg more_cand5 treat dep deptreat dy* if bw<50000, cl(cod_mun)
reg more_cand5 treat dep deptreat dy* if bw<25000, cl(cod_mun)
reg more_cand5 treat dep deptreat dy* if bw<75000, cl(cod_mun)
reg more_cand5 treat dep deptreat dep2 deptreat2 dy* if bw<50000, cl(cod_mun)
reg more_cand5 treat dep deptreat dep2 deptreat2 dy* if bw<75000, cl(cod_mun)

sum candidatos if bw<25000 & treat==0
reg candidatos treat dep deptreat dy* if bw<50000, cl(cod_mun)
reg candidatos treat dep deptreat dy* if bw<25000, cl(cod_mun)
reg candidatos treat dep deptreat dy* if bw<75000, cl(cod_mun)
reg candidatos treat dep deptreat dep2 deptreat2 dy* if bw<50000, cl(cod_mun)
reg candidatos treat dep deptreat dep2 deptreat2 dy* if bw<75000, cl(cod_mun)

***********
* Table 5 *
***********

* The following commands generate the Single-Ballot Means
sum 	party_ent1	if bw<25000 & treat==0	
sum 	party_ent2	if bw<25000 & treat==0	
sum 	party_ent3	if bw<25000 & treat==0	
sum 	party_ent4	if bw<25000 & treat==0	
sum 	party_ent5	if bw<25000 & treat==0	
sum 	party_ent6	if bw<25000 & treat==0	
sum 	party_ent7	if bw<25000 & treat==0	
sum 	party_ent8	if bw<25000 & treat==0	
sum 	party_ent9	if bw<25000 & treat==0	
sum 	party_ent10	if bw<25000 & treat==0	
sum 	party_ent11	if bw<25000 & treat==0	
sum 	party_ent12	if bw<25000 & treat==0	
sum 	party_ent13	if bw<25000 & treat==0	
sum 	party_ent14	if bw<25000 & treat==0	
sum 	party_ent15	if bw<25000 & treat==0	
sum 	party_ent16	if bw<25000 & treat==0	

* The following commands generate the Treatment Effects
reg	party_ent1	treat	dep	deptreat	dy*	if			bw<50000	,	cl(cod_mun)
reg	party_ent2	treat	dep	deptreat	dy*	if			bw<50000	,	cl(cod_mun)
reg	party_ent3	treat	dep	deptreat	dy*	if			bw<50000	,	cl(cod_mun)
reg	party_ent4	treat	dep	deptreat	dy*	if			bw<50000	,	cl(cod_mun)
reg	party_ent5	treat	dep	deptreat	dy*	if			bw<50000	,	cl(cod_mun)
reg	party_ent6	treat	dep	deptreat	dy*	if			bw<50000	,	cl(cod_mun)
reg	party_ent7	treat	dep	deptreat	dy*	if			bw<50000	,	cl(cod_mun)
reg	party_ent8	treat	dep	deptreat	dy*	if			bw<50000	,	cl(cod_mun)
reg	party_ent9	treat	dep	deptreat	dy*	if			bw<50000	,	cl(cod_mun)
reg	party_ent10	treat	dep	deptreat	dy*	if			bw<50000	,	cl(cod_mun)
reg	party_ent11	treat	dep	deptreat	dy*	if			bw<50000	,	cl(cod_mun)
reg	party_ent12	treat	dep	deptreat	dy*	if			bw<50000	,	cl(cod_mun)
reg	party_ent13	treat	dep	deptreat	dy*	if			bw<50000	,	cl(cod_mun)
reg	party_ent14	treat	dep	deptreat	dy*	if			bw<50000	,	cl(cod_mun)
reg	party_ent15	treat	dep	deptreat	dy*	if			bw<50000	,	cl(cod_mun)
reg	party_ent16	treat	dep	deptreat	dy*	if			bw<50000	,	cl(cod_mun)

*The following generates the Chi-Sq test statistic
forvalues i=1(1)16 {
quietly reg party_ent`i' treat dep deptreat dy* if bw<50000
estimates store p`i'
}
quietly suest p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 , cl(cod_mun)
test treat


***********
* Table 7 *
***********

sum more_share2 if bw<25000 & treat==0
reg more_share2 treat dep deptreat dcolcomp* dy* if bw<50000, cl(cod_mun)
reg more_share2 treat dep deptreat dcolcomp* dy* if bw<25000, cl(cod_mun)
reg more_share2 treat dep deptreat dcolcomp* dy* if bw<75000, cl(cod_mun)
reg more_share2 treat dep deptreat dep2 deptreat2 dcolcomp*  dy* if bw<50000, cl(cod_mun)
reg more_share2 treat dep deptreat dep2 deptreat2 dcolcomp*  dy* if bw<75000, cl(cod_mun)

sum more_share2 if bw<25000 & treat==0
reg more_share2 treat dep deptreat dhscomp* dy* if bw<50000, cl(cod_mun)
reg more_share2 treat dep deptreat dhscomp* dy* if bw<25000, cl(cod_mun)
reg more_share2 treat dep deptreat dhscomp* dy* if bw<75000, cl(cod_mun)
reg more_share2 treat dep deptreat dep2 deptreat2 dhscomp*  dy* if bw<50000, cl(cod_mun)
reg more_share2 treat dep deptreat dep2 deptreat2 dhscomp*  dy* if bw<75000, cl(cod_mun)

sum more_share2 if bw<25000 & treat==0
reg more_share2 treat dep deptreat dskillcomp* dy* if bw<50000, cl(cod_mun)
reg more_share2 treat dep deptreat dskillcomp* dy* if bw<25000, cl(cod_mun)
reg more_share2 treat dep deptreat dskillcomp* dy* if bw<75000, cl(cod_mun)
reg more_share2 treat dep deptreat dep2 deptreat2 dskillcomp*  dy* if bw<50000, cl(cod_mun)
reg more_share2 treat dep deptreat dep2 deptreat2 dskillcomp*  dy* if bw<75000, cl(cod_mun)

***********
* Table 8 *
***********

sum singleparty if bw<25000 & treat==0
reg singleparty treat dep deptreat dy* if bw<50000, cl(cod_mun)
reg singleparty treat dep deptreat dy* if bw<25000, cl(cod_mun)
reg singleparty treat dep deptreat dy* if bw<75000, cl(cod_mun)
reg singleparty treat dep deptreat dep2 deptreat2 dy* if bw<50000, cl(cod_mun)
reg singleparty treat dep deptreat dep2 deptreat2 dy* if bw<75000, cl(cod_mun)

sum cr1 if bw<25000 & treat==0
reg cr1 treat dep deptreat dy* if bw<50000, cl(cod_mun)
reg cr1 treat dep deptreat dy* if bw<25000, cl(cod_mun)
reg cr1 treat dep deptreat dy* if bw<75000, cl(cod_mun)
reg cr1 treat dep deptreat dep2 deptreat2 dy* if bw<50000, cl(cod_mun)
reg cr1 treat dep deptreat dep2 deptreat2 dy* if bw<75000, cl(cod_mun)

sum cr2 if bw<25000 & treat==0
reg cr2 treat dep deptreat dy* if bw<50000, cl(cod_mun)
reg cr2 treat dep deptreat dy* if bw<25000, cl(cod_mun)
reg cr2 treat dep deptreat dy* if bw<75000, cl(cod_mun)
reg cr2 treat dep deptreat dep2 deptreat2 dy* if bw<50000, cl(cod_mun)
reg cr2 treat dep deptreat dep2 deptreat2 dy* if bw<75000, cl(cod_mun)

sum hhi if bw<25000 & treat==0
reg hhi treat dep deptreat dy* if bw<50000, cl(cod_mun)
reg hhi treat dep deptreat dy* if bw<25000, cl(cod_mun)
reg hhi treat dep deptreat dy* if bw<75000, cl(cod_mun)
reg hhi treat dep deptreat dep2 deptreat2 dy* if bw<50000, cl(cod_mun)
reg hhi treat dep deptreat dep2 deptreat2 dy* if bw<75000, cl(cod_mun)

*************
* Table A2  *
*************

sum more_share2 if bw<25000 & treat==0
reg more_share2 treat dep deptreat turnout_rate registration_rate dy* if bw<50000, cl(cod_mun)
reg more_share2 treat dep deptreat turnout_rate registration_rate dy* if bw<25000, cl(cod_mun)
reg more_share2 treat dep deptreat turnout_rate registration_rate dy* if bw<75000, cl(cod_mun)
reg more_share2 treat dep deptreat dep2 deptreat2 turnout_rate registration_rate dy* if bw<50000, cl(cod_mun)
reg more_share2 treat dep deptreat dep2 deptreat2 turnout_rate registration_rate dy* if bw<75000, cl(cod_mun)

sum more_share2 if bw<25000 & treat==0
reg more_share2 treat dep deptreat rural income schooling gini dy* if bw<50000, cl(cod_mun)
reg more_share2 treat dep deptreat rural income schooling gini dy* if bw<25000, cl(cod_mun)
reg more_share2 treat dep deptreat rural income schooling gini dy* if bw<75000, cl(cod_mun)
reg more_share2 treat dep deptreat dep2 deptreat2 rural income schooling gini dy* if bw<50000, cl(cod_mun)
reg more_share2 treat dep deptreat dep2 deptreat2 rural income schooling gini dy* if bw<75000, cl(cod_mun)

sum more_share2 if bw<25000 & treat==0
reg more_share2 treat dep deptreat latitude longitude dy* if bw<50000, cl(cod_mun)
reg more_share2 treat dep deptreat latitude longitude dy* if bw<25000, cl(cod_mun)
reg more_share2 treat dep deptreat latitude longitude dy* if bw<75000, cl(cod_mun)
reg more_share2 treat dep deptreat dep2 deptreat2 latitude longitude dy* if bw<50000, cl(cod_mun)
reg more_share2 treat dep deptreat dep2 deptreat2 latitude longitude dy* if bw<75000, cl(cod_mun)


