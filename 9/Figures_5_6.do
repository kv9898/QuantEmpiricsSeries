clear
use "C:\Research\Data Municipal Elections\Files Sent to QJPS\Supplementary Materials\Figures_5_6.dta"

cd "C:\Research\Data Municipal Elections\Files Sent to QJPS\Paper\"


graph bar (asis) te, over(party, sort(order) axis(off)) bargap(10000) blabel(group, justification(center) alignment(baseline)) yscale(range (.35 -.35))
graph2tex, eps(Figure5)

graph bar (asis) tstat, over(party, sort(order) axis(off)) bargap(10000) blabel(group, box fcolor(white) lcolor(none) justification(center)  alignment(baseline)) yscale(range(2.5 -2.5)) yscale(line) yline(1.65 -1.65 1.96 -1.96 2.58 -2.58, lwidth(medthick) lpattern(dot) lcolor(gs8) noextend)
graph2tex, eps(Figure6)
