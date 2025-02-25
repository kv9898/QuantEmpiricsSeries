#Originally rom GV249, LSE

#Originally by Florian Foos, Modified by Dianyi Yang

#Difference-in-Differences (DiD) Estimation

# Effect of Earned Income Tax Credit on female labour force participation (code adapted from Kevin Goulding, https://thetarzan.wordpress.com/2011/06/20/differences-in-differences-estimation-in-r-and-stata/)
##########################################################################

rm(list=ls()) 
#install.packages("ggiplot", repos = "https://grantmcdermott.r-universe.dev")
#options(repos = c(skranz = 'https://skranz.r-universe.dev',CRAN = 'https://cloud.r-project.org'))
need <- c("modelsummary","rstudioapi","haven","fixest","ggfixest","car", "tidyverse",
          "parameters",'ParallelTrendsPlot',"ggdist",'ggcorrplot') # list packages needed
have <- need %in% rownames(installed.packages()) # checks packages you have
if(any(!have)) install.packages(need[!have]) # install missing packages
invisible(lapply(need, library, character.only=T)) # load needed packages

setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #set working directory to the file directory

# Import dataset

data <- read_dta("eitc.dta")

summary(data) #view dataframe and variables

# Create two dummy variables to indicate before/after and treatment/control groups.

# the programme went into effect in the year 1994

data$post93 <- as.numeric(data$year >= 1994)
table(data$post93)

# The programme only affected women with at least 1 child, so the treatment group will be all women with children.

data$anykids <- as.numeric(data$children > 0)
table(data$anykids)

#produce a treatment variable
data$treatment <- data$post93 * data$anykids
table(data$treatment)

# Compute the four data points needed for the DiD estimation:

pre93_nokids = colMeans(subset(data, post93 == 0 & anykids == 0, select=work))
pre93_kids = colMeans(subset(data, post93 == 0 & anykids == 1, select=work))
post93_nokids = colMeans(subset(data, post93 == 1 & anykids == 0, select=work))
post93_kids = colMeans(subset(data, post93 == 1 & anykids == 1, select=work))

DiD<-(post93_kids-pre93_kids)-(post93_nokids-pre93_nokids) #estimate DiD

DiD #display DiD estimate

#DiD estimation using Linear Regression
model1 = feols(work ~ post93 + anykids + post93*anykids, data = data, cluster="state")
summary(model1)
  model1_fe <- feols(work ~  treatment|anykids+post93, data = data, cluster="state") #use a fixed effects sepcification
  summary(model1_fe)
  
#Including Covariates
model2 = feols(work ~ anykids + post93 + post93*anykids + age + ed, data = data, cluster="state")
summary(model2)
  model2_fe <- feols(work ~  treatment+ age + ed|anykids+post93, data = data, cluster="state") #use a fixed effects sepcification
  summary(model2_fe)


#Including Covariates and interactions
model3 = feols(work ~ anykids + post93 + post93*anykids + age + ed + age*anykids + ed*anykids + age*post93 + ed*post93, data = data, cluster="state")
summary(model3)
  model3_fe <- feols(work ~  treatment+ age + ed + age*anykids + ed*anykids + age*post93 + ed*post93|anykids+post93, data = data, cluster="state") #use a fixed effects sepcification
  summary(model3_fe)

#make a result table
models=list(model1_fe, model2_fe, model3_fe)
modelsummary(models, stars=TRUE)

#plot the coefficients
coef_map <- c("treatment"="ATT")

modelplot(models, coef_map=coef_map) + scale_x_continuous(limits= c(-0.02, 0.12)) +
  geom_vline(xintercept=0) #95% CI only

dat <- map_dfr(c(.9, .95, .99), function(x) {
  modelplot(models, conf_level = x, draw = FALSE, , coef_map=coef_map) %>%
    mutate(.width = x)
})

ggplot(dat, aes(y = term, x = estimate, xmin = conf.low, xmax = conf.high,
  color = model)) + ggdist::geom_pointinterval(position = "dodge",
    interval_size_range = c(1, 3), fatten_point=1.2) + theme_bw() +
  scale_x_continuous(limits= c(-0.02, 0.12)) + geom_vline(xintercept=0) +
  labs(x="Coefficient estimates with 90% and 95% CIs", y="ATT")  #90% and 95% CI


## Placebo

# Create a placebo "treatment":

# sub set the data, including only years before 1994.
data.sub = data[data$year <= 1993,]

# Create a new hypothetical "after treatment" dummy variable

data.sub$post91 = as.numeric(data.sub$year >= 1992)

table(data.sub$post91)

# Run a placebo regression
model4 <- feols(work ~ post91*anykids|anykids + post91, data = data.sub, cluster="state")
summary(model4)


#placebo treatment for more than 1 child
data.sub2 = data[data$children >= 1,]

data.sub2$morethan1 <- as.numeric(data.sub2$children > 1)
table(data.sub2$morethan1)



model5 <- feols(work ~ post93*morethan1|morethan1 + post93, data = data.sub2, cluster="state")
summary(model5)

#alternatively, do a event-study style plot
data_es <- data
data_es$time_to_treat = ifelse(data_es$anykids==1, data_es$year, 1993)
  #without covariates
lm_ev <- feols(work ~ i(time_to_treat, anykids, ref=1993)| anykids+post93, data = data_es, cluster="state")
ggiplot(lm_ev, geom_style="ribbon", ci_level=c(0.9,0.95), theme = theme_minimal(),xlab = 'Year')
  #with covariates and interactions
lm_ev3 <- feols(work ~ i(time_to_treat, anykids, ref=1993)+ age + ed + age*anykids + ed*anykids + age*post93 + ed*post93| anykids+post93, data = data_es, cluster="state") #without covariates
ggiplot(lm_ev3, geom_style="ribbon", ci_level=c(0.9,0.95), theme = theme_minimal(),xlab = 'Year')
  #f-test for pre-treatment periods for lm_ev3
lm_ev3$df.residual=degrees_of_freedom(lm_ev3) #add degrees of freedom information to the model
hypotheses <- c("time_to_treat::1991:anykids=0","time_to_treat::1992:anykids=0")
linearHypothesis(lm_ev3, hypotheses, test="F")

#Graphical Display

# Take average value of 'work' by year, conditional on whether a woman has kids
data_graph = aggregate(data$work, list(data$year,data$anykids == 1), mean)

# rename column headings (variables)
names(data_graph) = c("YR","Treatment","LFPR")

# Attach a new column with labels
data_graph$Group[1:6] = "no children"
data_graph$Group[7:12] = "children"

qplot(YR, LFPR, data=data_graph, geom=c("point","line"), colour=Group,
      xlab="Year", ylab="Labor Force Participation Rate") + geom_vline(xintercept = 1994)


#Alternatively
pdata <- parallel.trends.data(data,timevar='year',yvar="work",treatdummy='anykids',expdummy='post93',
                              cvars="age + ed + age*anykids + ed*anykids + age*post93 + ed*post93")
parallel.trends.plot(pdata, facet.mode=TRUE, add.exp.line="first")+theme_bw()

#with numbers (without covariates)
data$treatment_group=ifelse(data$anykids==1,"Treatment","Control")

  gdat = data %>%
    group_by(treatment_group, year,post93,anykids) %>%
    summarize(y = mean(work))
  
  gg = ggplot(gdat, aes(y=y,x=year, color= treatment_group)) +
    geom_line() + 
    geom_vline(xintercept=1993) + guides(color = guide_legend(title = "Group"))+
    theme_bw() + labs(x="Year", y="Labor Force Participation Rate", title="Diff-in-Diff Plot")
  

    y.pre.tr <<- mean(filter(data,anykids==1, post93==0)$work) %>% round(3)
    y.exp.tr <<- mean(filter(data,anykids==1, post93==1)$work) %>% round(3)
    y.pre.co <<- mean(filter(data,anykids==0, post93==0)$work) %>% round(3)
    y.exp.co <<- mean(filter(data,anykids==0, post93==1)$work) %>% round(3)
    gg = gg + 
      geom_segment(x=1992, y=y.pre.tr, xend=1994.5, yend=y.exp.tr, linetype=1, linewidth=0.75, colour="#00BFC4")+
      geom_segment(x=1992, y=y.pre.co, xend=1994.5, yend=y.exp.co, linetype=1, linewidth=0.75, colour="#F8766D") +
      geom_segment(x=1992, y=y.pre.co, xend=1992, yend=y.pre.tr, linetype=5, colour="grey", linewidth=0.75) +
      geom_segment(x=1994.5, y=y.exp.co, xend=1994.5, yend=y.pre.tr+y.exp.co-y.pre.co, linetype=5, colour="grey", linewidth=0.75) +
      geom_segment(x=1992, y=y.pre.tr, xend=1994.5, yend=y.pre.tr+y.exp.co-y.pre.co, linetype=5, colour="black", linewidth=0.75) +
      annotate("label", x=1992, y=y.pre.co-0.01,label=y.pre.co) +
      annotate("label", x=1994.5, y=y.exp.tr+0.01,label=y.exp.tr) +
      annotate("label", x=1994.5, y=y.exp.co-0.01,label=y.exp.co) + annotate("label", x=1992, y=y.pre.tr+0.01,label=y.pre.tr) +
      geom_curve(aes(x = 1994.5, xend = 1994.5, y = y.pre.tr+y.exp.co-y.pre.co, yend = y.exp.tr),  
                 curvature=.2, color = 'black', linewidth = 0.75, arrow = arrow(length = unit(.2, 'cm')))
  
  gg

  y.pre.tr <<- mean(filter(data,anykids==1, post93==0)$work) #not rounded
  y.exp.tr <<- mean(filter(data,anykids==1, post93==1)$work) 
  y.pre.co <<- mean(filter(data,anykids==0, post93==0)$work) 
  y.exp.co <<- mean(filter(data,anykids==0, post93==1)$work) 

  (y.exp.tr-y.pre.tr)-(y.exp.co-y.pre.co) #result from plot
  
  model1_fe #compare with regression result

#with numbers (with covariates)
datapred <- subset(pdata, .mode=="with control variables")
datapred$year <- datapred$.t
datapred$treatment_group[datapred$.group[,1]=="treatment"] <- "Treatment"
datapred$treatment_group[datapred$.group[,1]=="control"] <- "Control"

gdat_cov = datapred %>%
  group_by(treatment_group, year,post93,anykids) %>%
  summarize(y = mean(.y))

gg_cov = ggplot(gdat_cov, aes(y=y,x=year, color= treatment_group)) +
  geom_line() + 
  geom_vline(xintercept=1993) + guides(color = guide_legend(title = "Group"))+
  theme_bw() + labs(x="Year", y="Labor Force Participation Rate", title="Diff-in-Diff Plot (with covariates)")


y.pre.tr <<- mean(filter(datapred,anykids==1, post93==0)$.y) %>% round(3)
y.exp.tr <<- mean(filter(datapred,anykids==1, post93==1)$.y) %>% round(3)
y.pre.co <<- mean(filter(datapred,anykids==0, post93==0)$.y) %>% round(3)
y.exp.co <<- mean(filter(datapred,anykids==0, post93==1)$.y) %>% round(3)
gg_cov = gg_cov + 
  geom_segment(x=1992, y=y.pre.tr, xend=1994.5, yend=y.exp.tr, linetype=1, linewidth=0.75, colour="#00BFC4")+
  geom_segment(x=1992, y=y.pre.co, xend=1994.5, yend=y.exp.co, linetype=1, linewidth=0.75, colour="#F8766D") +
  geom_segment(x=1992, y=y.pre.co, xend=1992, yend=y.pre.tr, linetype=5, colour="grey", linewidth=0.75) +
  geom_segment(x=1994.5, y=y.exp.co, xend=1994.5, yend=y.pre.tr+y.exp.co-y.pre.co, linetype=5, colour="grey", linewidth=0.75) +
  geom_segment(x=1992, y=y.pre.tr, xend=1994.5, yend=y.pre.tr+y.exp.co-y.pre.co, linetype=5, colour="black", linewidth=0.75) +
  annotate("label", x=1992, y=y.pre.co-0.01,label=y.pre.co) +
  annotate("label", x=1994.5, y=y.exp.tr+0.01,label=y.exp.tr) +
  annotate("label", x=1994.5, y=y.exp.co-0.01,label=y.exp.co) + annotate("label", x=1992, y=y.pre.tr+0.01,label=y.pre.tr) +
  geom_curve(aes(x = 1994.5, xend = 1994.5, y = y.pre.tr+y.exp.co-y.pre.co, yend = y.exp.tr),  
             curvature=.2, color = 'black', linewidth = 0.75, arrow = arrow(length = unit(.2, 'cm')))

gg_cov

y.pre.tr <<- mean(filter(datapred,anykids==1, post93==0)$.y) #not rounded
y.exp.tr <<- mean(filter(datapred,anykids==1, post93==1)$.y) 
y.pre.co <<- mean(filter(datapred,anykids==0, post93==0)$.y)
y.exp.co <<- mean(filter(datapred,anykids==0, post93==1)$.y)

(y.exp.tr-y.pre.tr)-(y.exp.co-y.pre.co) #result from plot

model3_fe #compare with regression result

#correlation plot
data1 <- subset(data,select=c(urate, age, children, finc, earn, ed, work, unearn, year))

#ggcorrplot
ggcorrplot(cor(data1), lab=TRUE, colors=c("#BB4444","#FFFFFF","#4477AA"))

#corrplot
#libary(corrplot)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor(data1), method = "shade", shade.col = NA, tl.col = "black", tl.srt = 45,
         col = col(200), addCoef.col = "black", cl.pos = "n", order = "AOE")
