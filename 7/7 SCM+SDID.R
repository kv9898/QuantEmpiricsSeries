#setup
rm(list = ls()) # clear workspace
#remotes::install_github("bcastanho/SCtools")
#devtools::install_github("synth-inference/synthdid") #manually install sdid if the codes above fail to do so automatically
need <- c("modelsummary","rstudioapi","haven","fixest","car","parameters",
          "Synth","synthdid","SCtools","ggplot2","tidyverse","fwildclusterboot") # list packages needed
have <- need %in% rownames(installed.packages()) # checks packages you have
if(any(!have)) install.packages(need[!have]) # install missing packages
invisible(lapply(need, library, character.only=T)) # load needed packages

setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #set working directory to the file directory
list.files()

######################Synthetic Contol Method#########################
#read data
data(synth.data)

# create matrices from panel data that provide inputs for synth()
dataprep.out<-
  dataprep(
    foo = synth.data,
    predictors = c("X1", "X2", "X3"),
    predictors.op = "mean",
    dependent = "Y",
    unit.variable = "unit.num",
    time.variable = "year",
    special.predictors = list(
      list("Y", 1991, "mean"),
      list("Y", 1985, "mean"),
      list("Y", 1980, "mean")
    ),
    treatment.identifier = 7,
    controls.identifier = c(29, 2, 13, 17, 32, 38),
    time.predictors.prior = c(1984:1989),
    time.optimize.ssr = c(1984:1990),
    unit.names.variable = "name",
    time.plot = 1984:1996
  )

## run the synth command to identify the weights
## that create the best possible synthetic 
## control unit for the treated.
synth.out <- synth(dataprep.out)

## there are two ways to summarize the results
## we can either access the output from synth.out directly
round(synth.out$solution.w,2)
# contains the unit weights or
synth.out$solution.v 
## contains the predictor weights. 

## the output from synth opt 
## can be flexibly combined with 
## the output from dataprep to 
## compute other quantities of interest
## for example, the period by period 
## discrepancies between the 
## treated unit and its synthetic control unit
## can be computed by typing
gaps<- dataprep.out$Y1plot-(
  dataprep.out$Y0plot%*%synth.out$solution.w
) ; gaps

## also there are three convenience functions to summarize results.
## to get summary tables for all information 
## (V and W weights plus balance btw. 
## treated and synthetic control) use the 
## synth.tab() command
synth.tables <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res = synth.out)
print(synth.tables)

## to get summary plots for outcome trajectories 
## of the treated and the synthetic control unit use the 
## path.plot() and the gaps.plot() commands

## plot in levels (treated and synthetic)
path.plot(dataprep.res = dataprep.out,synth.res = synth.out)

## plot the gaps (treated - synthetic)
gaps.plot(dataprep.res = dataprep.out,synth.res = synth.out)

#placebo test
placebo <- generate.placebos(dataprep.out, synth.out, Sigf.ipop=5)
ratio <- mspe.test(placebo)
placebo_result <- ratio$test
placebo_result$RMSPE.ratio <- sqrt(placebo_result$MSPE.ratios)

placebo_result <- placebo_result %>% arrange(desc(RMSPE.ratio))

ggplot(placebo_result, aes(x=reorder(unit, RMSPE.ratio), y=RMSPE.ratio))+geom_point(shape=15) +coord_flip()+theme_bw()+
  theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank())+
  labs(x="Region", y="Postperiod RMSPE/Preperiod RMSPE", title="Ratio of post-treatment RMSPE to pre-treatment RMSPE: Toy Data")

p <- which(placebo_result$unit=="treated.region")/nrow(placebo_result)
print(paste0("p=",round(p,3)))

###############################Synthetic Difference-in-Differences#############
data <- read.csv("month.csv")
seed=666 #seed for wild-cluster bootstrapping
set.seed(seed)

#logbilateral.trade
data$logbilateral.trade <- log(data$bilateral.trade)
data$treated <- ifelse(data$Unit == 1 & data$M.unit >= 0, 1,0 ) #data cleaning for synthdid
setup = panel.matrices(data, unit = "Country", time = "M.unit", outcome = "logbilateral.trade", treatment = "treated")

#3 in one diagram
tau.hat.log = synthdid_estimate(setup$Y, setup$N0, setup$T0)
#plot(tau.hat.log, treated.name = "log C-P trade", control.name = "log synthetic C-P trade")
tau.hat.sc = sc_estimate(setup$Y, setup$N0, setup$T0)
tau.hat.did = did_estimate(setup$Y, setup$N0, setup$T0)

estimates = list(tau.hat.log, tau.hat.sc, tau.hat.did)
synthdid_units_plot(estimates, se.method="none") + labs(size="Weight:") +
  theme(legend.background=element_blank(), legend.title = element_text(),
        legend.direction='horizontal', legend.position=c(.17,.80),
        strip.background=element_blank(), strip.text.x = element_blank()) 
ggsave(file="sdid_total_unitplot.pdf",device="pdf", units="in", width=10.75, height=4.53)
names(estimates) = c('Synthetic Diff-in-Diff', 'Synthetic Control', 'Diff-in-Diff')
synthdid_plot(estimates, facet.vertical=FALSE, se.method="none",
              control.name='Synthetic C-P trade', treated.name='C-P trade',
              lambda.comparable=TRUE,
              trajectory.linetype = 1, line.width=.75, effect.curvature=-.4,
              trajectory.alpha=.7, effect.alpha=.7,
              diagram.alpha=1, onset.alpha=.7) +
  theme(legend.position=c(.08,.17), legend.direction='vertical',
        legend.key=element_blank(), legend.background=element_blank(),
  ) + xlab("Months since treatment") +ylab("Log(Trade Volumes)")
ggsave(file="sdid_total_trendplot.pdf",device="pdf", units="in", width=10.75, height=4.53)

#original SE
orig_se <- sqrt(vcov(tau.hat.log, method="placebo"))
print(paste0("Placebo p-value=",round(orig_se,3)))

#Weighted TWFE regression
#data <- subset(data, select = -c(omega, lambda, weights))
data$omega [data$Country == data$Country [data$treated==1][1]] <- 1
T1 <- length(data$Country[data$treated==1])
data$lambda [data$M.unit %in% data$M.unit[data$treated==1]] <- 1/T1

omega.df <- data.frame(Country = rownames(attributes(tau.hat.log)$setup$Y)[-nrow(attributes(tau.hat.log)$setup$Y)], omega = attributes(tau.hat.log)$weights$omega)
lambda.df <- data.frame(M.unit = colnames(attributes(tau.hat.log)$setup$Y)[1:(length(colnames(attributes(tau.hat.log)$setup$Y))-T1)], lambda = attributes(tau.hat.log)$weights$lambda)

data <- merge(data, omega.df, by = "Country", all = TRUE, no.dups = TRUE)
data$omega <- ifelse(is.na(data$omega.x),data$omega.y,data$omega.x)
data <- subset(data, select = -c(omega.x,omega.y))

data <- merge(data, lambda.df, by = "M.unit", all = TRUE, no.dups = TRUE)
data$lambda <- ifelse(is.na(data$lambda.x),data$lambda.y,data$lambda.x)
data <- subset(data, select = -c(lambda.x, lambda.y))

data$weights <- data$lambda * data$omega

data_only <- subset(data, weights!=0)
#dk method
lm_sdid_dk <- feols(logbilateral.trade ~ treated|Country + M.unit, data = data_only, weights = data_only$weights, panel.id=~Country+M.unit, vcov="DK") 
lm_sdid_dk
#WCB method
lm_sdid <- feols(logbilateral.trade ~ treated + factor(Country) + factor(M.unit), data = data_only, weights = data_only$weights) 
dqrng::dqset.seed(seed)
lm_sdid_wild <-  boottest(lm_sdid, param="treated", B=9999, clustid="Unit", fe=NULL, type="webb")
lm_sdid_wild

#make regression table
models <- list(lm_sdid_dk, lm_sdid_wild)

glance_custom.boottest <- function(x, ...) {
  data.frame(
    'fe.u'=ifelse(x$call$object!="lm_sc",'Yes','No'),
    'fe.t'= "Yes",
    "vcov.type"="WCB by Country"
  )
}

glance_custom.fixest <- function(x, ...) {
  data.frame('fe.u'=ifelse("Country" %in% names(x$fixef_id),"Yes","No"),
             'fe.t'=ifelse("M.unit" %in% names(x$fixef_id),"Yes","No")
  )}

gofmap <- list(list("raw" = "fe.u", "clean" = "Country FE", "fmt" = NULL),
               list("raw" = "fe.t", "clean" = "Time FE", "fmt" = NULL),
               list("raw" = "nobs", "clean" = "Effective N", "fmt" = 0),
               list('raw'="vcov.type", "clean" = "Inference type", "fmt" = NULL)) 

modelsummary(models, estimate = "{estimate} ({p.value}){stars}", statistic = "[{conf.low}, {conf.high}]",
             notes=c("* p < 0.1, ** p < 0.05, *** p < 0.01",
                     "Effective observations for SDID are smaller due to zero time and unit weights.",
                     "P-values in parentheses; WCB CIs are calculated with Webb weights."),
             stars = c('*' = .1, '**' = .05, '***' = .01), gof_map = gofmap,
             coef_rename = c("1*treated = 0" = "ATT", "treated"="ATT"), fmt=4,
             output="default")

#make event studies plot
data_es <- subset(data[order(data$Unit,data$M.unit),], weights!=0)
data_es$time_to_treat = ifelse(data_es$Unit==1, data_es$M.unit, 0)
lm_ev_dk <- feols(logbilateral.trade ~ i(time_to_treat, treated, ref=-4)|Country + M.unit, data = data_es, weights = data_es$weights, panel.id=~Country+M.unit, vcov="DK")
lm_ev <- feols(logbilateral.trade ~ i(time_to_treat, treated, ref=-4) + factor(Country) + factor(M.unit), data = data_es, weights = data_es$weights, cluster='Country')
es_results <- data.frame(event_dates = numeric(), estimates = numeric(), lower_ci = numeric(),
                         upper_ci = numeric(), lower_90 = numeric(), upper_90 = numeric(), p.value=numeric(), dk_se=numeric(), stringsAsFactors = FALSE)
for (x in 0:6){
  dqrng::dqset.seed(seed)
  lm_ev_wild <- boottest(lm_ev, param=paste0("time_to_treat::",x,":treated"), B=999, clustid="Country", fe=NULL, type="webb")
  dqrng::dqset.seed(seed)
  lm_ev_wild90 <- boottest(lm_ev, sign_level=0.10, param=paste0("time_to_treat::",x,":treated"), B=999, clustid="Country", fe=NULL, type="webb")
  es_results[x+1,1] <- x
  es_results[x+1,2] <- lm_ev_wild$point_estimate
  es_results[x+1,3] <- lm_ev_wild$conf_int[1]
  es_results[x+1,4] <- lm_ev_wild$conf_int[2]
  es_results[x+1,5] <- lm_ev_wild90$conf_int[1]
  es_results[x+1,6] <- lm_ev_wild90$conf_int[2]
  es_results[x+1,7] <- lm_ev_wild$p_val
  es_results[x+1,8] <- se(lm_ev_dk)[[paste0("time_to_treat::",x,":treated")]]
}
lm_ev_sum=list(nobs=lm_ev$nobs, r.squared=get_gof(lm_ev)$r.squared,
               adj.r.squared=get_gof(lm_ev)$adj.r.squared,
               estimates=es_results)
es_results <- rbind(data.frame(event_dates=-1, estimates=0, lower_ci=0, upper_ci=0, lower_90=0, upper_90=0, dk_se=0, p.value=0), es_results)
ggplot(es_results, aes(x = event_dates, y = estimates))+ ggtitle("Event Study Style Plot: Total bilateral trade volumes") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  geom_errorbar(aes(x = event_dates-0.1, ymin = lower_ci, ymax = upper_ci, color="WCB"), width = 0.2) +
  geom_line() +
  geom_errorbar(aes(x = event_dates-0.1, ymin = lower_90, ymax = upper_90, color="WCB"), width = 0.1) +
  geom_errorbar(aes(x = event_dates+0.1, ymin = estimates - 1.96*dk_se, ymax = estimates + 1.96*dk_se,
                color = "DK"), width = 0.2) +
  geom_errorbar(aes(x = event_dates+0.1, ymin = estimates - 1.645*dk_se, ymax = estimates + 1.645*dk_se,
                color = "DK"), width = 0.1) +
  labs(x="Months since treatment",y="ATT with 90% and 95% CIs")  +
  geom_vline(xintercept = -1, linetype = "dashed") + geom_point(size = 2) + 
  theme_minimal() + theme(panel.background = element_rect(fill = "white"))+ 
  scale_color_manual(name= 'Inference Method',
                     breaks=c('WCB','DK'),
                     values=c('WCB'="#1f78b4", "DK"="#E41A1C"))

#event-study regression table
class(lm_ev_sum) <- "bootevent"
tidy.bootevent <- function(x, ...) {
  ret <- data.frame(
    term      = x$estimates$event_dates,
    estimate  = x$estimates$estimates,
    conf.low  = x$estimates$lower_ci,
    conf.high = x$estimates$upper_ci,
    p.value   = x$estimates$p.value)
  ret
}
glance.bootevent <- function(x, ...) {
  ret <- data.frame(
    r.squared = x$r.squared,
    adj.r.squared= x$adj.r.squared,
    nobs  = x$nobs,
    fe.u  = 'Yes',
    fe.t  = 'Yes')
  ret
}
gofmap <- list(list("raw" = "fe.u", "clean" = "Country FE", "fmt" = NULL),
               list("raw" = "fe.t", "clean" = "Time FE", "fmt" = NULL),
               list("raw" = "nobs", "clean" = "Effective N", "fmt" = 0),
               list("raw" = "r.squared", "clean" = "R2", "fmt" = 3),
               list("raw" = "adj.r.squared", "clean" = "R2 Adj.", "fmt" = 3)) 

coefmap <- c()
for (x in 0:6){
  coefmap[paste0("time_to_treat::", x, ":treated")] <- as.character(x)
  coefmap[as.character(x)] <- as.character(x)
}

modelsummary(list('WCB'=lm_ev_sum, "DK"=lm_ev_dk), estimate = "{estimate} ({p.value}){stars}", statistic = "[{conf.low}, {conf.high}]",
             title="ATT by month", coef_map=coefmap,
             notes=c("* p < 0.1, ** p < 0.05, *** p < 0.01",
                     "Effective observations for SDID are smaller due to zero time and unit weights.",
                     "P-values in parentheses; WCB CIs are calculated with Webb weights."),
             stars = c('*' = .1, '**' = .05, '***' = .01), gof_map = gofmap,
             output="default")

