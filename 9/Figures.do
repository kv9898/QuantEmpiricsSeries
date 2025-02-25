clear
use "C:\Research\Data Municipal Elections\Files Sent to QJPS\Supplementary Materials\Figures.dta"

cd "C:\Research\Data Municipal Elections\Files Sent to QJPS\Paper\"

egen bin_voters=cut(electorate), at(50000(25000)375000) 
replace bin_voters=bin_voters+12500
gen y_util=1-twoparty

egen bin_y_util=mean(y_util), by(bin_voters)
egen bin_singleparty=mean(singleparty), by(bin_voters)
egen bin_cr1=mean(cr1), by(bin_voters)
egen bin_cr2=mean(cr2), by(bin_voters)
egen bin_hhi=mean(hhi), by(bin_voters)
egen bin_turnout=mean(turnout_rate), by(bin_voters)
egen bin_regist=mean(registration_rate), by(bin_voters)
egen bin_candidatos=mean(candidatos), by(bin_voters)
gen one=1 if electorate~=.
egen bin_size=sum(one) if bin_voters~=., by(bin_voters)


***Figure 1***
label var bin_y_util "Vote Share - Third and Lower Placed Candidates"
scatter bin_y_util bin_voters, mc(black) || qfit y_util electorate if electorate<200000 & electorate>50000 & bin_voters~=., lc(black) || qfit y_util electorate if electorate>200000 & electorate<375000 & bin_voters~=., yscale(r(0.05 0.3)) xline(200000) lc(black) legend(order(1)) ytick(0.05(0.05)0.3) ylabel(0.05(0.05)0.3) xtitle("Number of Registered Voters")
graph2tex, eps(figure1)

***Figure 2***
label var bin_turnout "Turnout Rate"
label var bin_regist "Registration Rate"
#delimit ;
scatter bin_turnout bin_voters, mc(black) m(circle) yaxis(1) || lfit turnout_rate electorate if electorate<200000 & electorate>50000, lc(black) yaxis(1) || lfit turnout_rate electorate if electorate>200000 & electorate<375000, lc(black) yaxis(1)||
scatter bin_regist bin_voters, mc(black) m(t) yaxis(1)|| qfit registration_rate electorate if electorate<200000 & electorate>50000, lc(black) yaxis(1)|| qfit registration_rate electorate if electorate>200000 & electorate<375000, lc(black) yaxis(1) xline(200000) lc(black) legend(order(1 4 )) xtitle("Number of Registered Voters");
#delimit cr
graph2tex, eps(figure2)

***Figure 3***
label var bin_size "# of Municipalities in Each Bin"
scatter bin_size bin_voters if bin_size<250, mc(black) xline(200000) legend(order(1)) xtitle("Number of Registered Voters")
graph2tex, eps(figure3) 

***Figure 4***
label var bin_candidatos "Number of Candidates"
scatter bin_candidatos bin_voters, mc(black) m(square) || lfit candidatos electorate if electorate<200000 & electorate>50000, lc(black)  || lfit candidatos electorate if electorate>200000 & electorate<375000, xline(200000) lc(black) legend(order(1 )) xtitle("Number of Registered Voters") yscale(r(2 7)) ytick(2(1)7) ylabel(2(1)7)
graph2tex, eps(figure4)

***Figure 7***
label var bin_cr1 "Seat Share - Most Voted Party"
label var bin_cr2 "Seat Share - Two Most Voted Parties"
label var bin_singleparty "Seat Share - Mayor's Party"
label var bin_hhi "HHI"
#delimit ;
scatter bin_cr1 bin_voters, mc(black)|| qfit bin_cr1 electorate if electorate<200000 & electorate>50000, lc(black) || qfit cr1 electorate if electorate>200000  & electorate<375000, lc(black) ||
scatter bin_cr2 bin_voters, mc(black) m(s)|| qfit cr2 electorate if electorate<200000 & electorate>50000, lc(black) || qfit cr2 electorate if electorate>200000  & electorate<375000, lc(black) ||
scatter bin_hhi bin_voters, mc(black) m(Oh)|| qfit hhi electorate if electorate<200000 & electorate>50000, lc(black) || qfit hhi electorate if electorate>200000  & electorate<375000, lc(black) legend(order(1 4 7 10)) xtitle("Number of Registered Voters") xline(200000) yscale(r(.1 .6)) ytick(.1(.1).6) ylabel(.1(.1).6);
#delimit cr
graph2tex, eps(figure7)


