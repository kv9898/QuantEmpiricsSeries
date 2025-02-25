#setup
rm(list = ls()) # clear workspace
need <- c("modelsummary","rstudioapi",'haven','rdd','rdrobust','scales','rdrobust', "fixest","car","parameters",'tidyverse') # list packages needed
have <- need %in% rownames(installed.packages()) # checks packages you have
if(any(!have)) install.packages(need[!have]) # install missing packages
invisible(lapply(need, library, character.only=T)) # load needed packages

setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #set working directory to the file directory
list.files()

#read fujiwara data
data<-read_dta("Figures.dta")
data <- as_factor(data) #load factor labels

##################################################Sharp RDD Starts#########################################
#Figure 1
data$y_util <- 1-data$twoparty
data_f1 <- subset(data, electorate<375000)
p <- rdplot(data_f1$y_util,data_f1$electorate, c=200000,p=2, nbins=c(6,7), h=c(150000,175000), 
       x.lim=c(0,400000),y.lim=c(0.05,0.3),x.label='Number of Registered Voters',y.label='',title='')
p$rdplot+scale_x_continuous(labels = comma)
  #or alternatively:
data_bin <- data
data_bin$bin_voters <- cut(data_bin$electorate, 
                       breaks = seq(50000, 375000, by = 25000), 
                       labels = FALSE, right = FALSE)
data_bin$bin_voters <- 25000*data_bin$bin_voters+37500
data_bin <- data_bin %>%
            drop_na('bin_voters') %>%
            group_by(bin_voters) %>%
            summarise(bin_y_util = mean(y_util, na.rm =T),
                      bin_singleparty = mean(singleparty, na.rm =T),
                      bin_cr1 = mean(cr1, na.rm =T),
                      bin_cr2 = mean(cr2, na.rm =T),
                      bin_hhi = mean(hhi, na.rm =T),
                      bin_turnout = mean(turnout_rate, na.rm =T),
                      bin_regist = mean(registration_rate, na.rm =T),
                      bin_candidatos = mean(candidatos, na.rm =T),
                      bin_size = n())

ggplot(data_bin, aes(x=bin_voters ,y=bin_y_util))+geom_point(aes(shape='Vote Share - Third and Lower Placed Candidates'), size=2)+
       geom_smooth(data=subset(data, 50000<=electorate & electorate<=200000),aes(x=electorate, y=y_util), na.rm=T, method='lm',formula=y~poly(x,2), se=F)+
       geom_smooth(data=subset(data, electorate>=200000 & electorate<=375000),aes(x=electorate, y=y_util), na.rm=T, method='lm',formula=y~poly(x,2), se=F)+
       geom_vline(xintercept=200000, colour='brown')+theme_bw()+coord_cartesian(xlim=c(0,400000),ylim=c(0.05,0.3))+
       labs(x='Number of Registered Voters',y='')+scale_x_continuous(labels = comma)+
       scale_shape_manual(name= '',
                     breaks='Vote Share - Third and Lower Placed Candidates',
                     values=16)+ theme(legend.position="bottom")

#Figure 2
ggplot(data_bin, aes(x=bin_voters)) + geom_point(aes(y=bin_turnout, shape='Turnout Rate'), size=2)+
  geom_point(aes(y=bin_regist, shape='Registration Rate'), size=2) +
  geom_smooth(data=subset(data, 50000<=electorate & electorate<=200000),aes(x=electorate, y=turnout_rate), na.rm=T, method='lm',formula=y~x, se=F)+
  geom_smooth(data=subset(data, electorate>=200000 & electorate<=375000),aes(x=electorate, y=turnout_rate), na.rm=T, method='lm',formula=y~x, se=F)+
  geom_smooth(data=subset(data, 50000<=electorate & electorate<=200000),aes(x=electorate, y=registration_rate), na.rm=T, method='lm',formula=y~poly(x,2), se=F)+
  geom_smooth(data=subset(data, electorate>=200000 & electorate<=375000),aes(x=electorate, y=registration_rate), na.rm=T, method='lm',formula=y~poly(x,2), se=F)+
  geom_vline(xintercept=200000, colour='brown')+theme_bw()+coord_cartesian(xlim=c(0,400000),ylim=c(0.6,0.9))+
  labs(x='Number of Registered Voters',y='')+scale_x_continuous(labels = comma)+
  scale_shape_manual(name= '',
                     breaks=c('Turnout Rate','Registration Rate'),
                     values=c(16,24))+ theme(legend.position="bottom")


#Figure 3 - McCrary Test
ggplot(subset(data_bin, bin_size<250), aes(x=bin_voters ,y=bin_size))+geom_point(size=2)+
  geom_vline(xintercept=200000, colour='brown')+theme_bw()+scale_x_continuous(labels = comma)+
  labs(x='Number of Registered Voters',y='# of Municipalities in Each Bin')

#a more formal way
DCdensity(runvar=data$electorate, cutpoint=200000, bin=25000, htest=T)
McCrary <- DCdensity(runvar=data$electorate, cutpoint=200000, bin=25000, htest=T, ext.out=T)

ggplot(McCrary$data, aes(x=cellmp, y=cellval))+geom_col()+scale_x_continuous(limits=c(0,400000), labels = comma)+
  labs(x='Number of Registered Voters',y='')+geom_vline(xintercept=200000, colour='brown')+theme_bw()

#############################Table 1##############################
data_t1<-read_dta("Tables.dta")
data_t1 <- as_factor(data_t1) #load factor labels

#3rd and lower
mean3 <- feols(more_share2~1, data=subset(data_t1, bw<25000 & treat==0))
linear3_50 <- feols(more_share2~treat+dep+deptreat+dy1+dy2+dy3, data=subset(data_t1, bw<50000), cluster='cod_mun')
linear3_25 <- feols(more_share2~treat+dep+deptreat+dy1+dy2+dy3, data=subset(data_t1, bw<25000), cluster='cod_mun')
linear3_75 <- feols(more_share2~treat+dep+deptreat+dy1+dy2+dy3, data=subset(data_t1, bw<75000), cluster='cod_mun')
quad3_50 <- feols(more_share2~treat+dep+deptreat+dep2+deptreat2+dy1+dy2+dy3, data=subset(data_t1, bw<50000), cluster='cod_mun')
quad3_75 <- feols(more_share2~treat+dep+deptreat+dep2+deptreat2+dy1+dy2+dy3, data=subset(data_t1, bw<75000), cluster='cod_mun')

#4th and lower
mean4 <- feols(more_share3~1, data=subset(data_t1, bw<25000 & treat==0))
linear4_50 <- feols(more_share3~treat+dep+deptreat+dy1+dy2+dy3, data=subset(data_t1, bw<50000), cluster='cod_mun')
linear4_25 <- feols(more_share3~treat+dep+deptreat+dy1+dy2+dy3, data=subset(data_t1, bw<25000), cluster='cod_mun')
linear4_75 <- feols(more_share3~treat+dep+deptreat+dy1+dy2+dy3, data=subset(data_t1, bw<75000), cluster='cod_mun')
quad4_50 <- feols(more_share3~treat+dep+deptreat+dep2+deptreat2+dy1+dy2+dy3, data=subset(data_t1, bw<50000), cluster='cod_mun')
quad4_75 <- feols(more_share3~treat+dep+deptreat+dep2+deptreat2+dy1+dy2+dy3, data=subset(data_t1, bw<75000), cluster='cod_mun')

#5th and lower
mean5 <- feols(more_share4~1, data=subset(data_t1, bw<25000 & treat==0))
linear5_50 <- feols(more_share4~treat+dep+deptreat+dy1+dy2+dy3, data=subset(data_t1, bw<50000), cluster='cod_mun')
linear5_25 <- feols(more_share4~treat+dep+deptreat+dy1+dy2+dy3, data=subset(data_t1, bw<25000), cluster='cod_mun')
linear5_75 <- feols(more_share4~treat+dep+deptreat+dy1+dy2+dy3, data=subset(data_t1, bw<75000), cluster='cod_mun')
quad5_50 <- feols(more_share4~treat+dep+deptreat+dep2+deptreat2+dy1+dy2+dy3, data=subset(data_t1, bw<50000), cluster='cod_mun')
quad5_75 <- feols(more_share4~treat+dep+deptreat+dep2+deptreat2+dy1+dy2+dy3, data=subset(data_t1, bw<75000), cluster='cod_mun')

#registration rate
meanreg <- feols(registration_rate ~1, data=subset(data_t1, bw<25000 & treat==0))
linearreg_50 <- feols(registration_rate ~treat+dep+deptreat+dy1+dy2+dy3, data=subset(data_t1, bw<50000), cluster='cod_mun')
linearreg_25 <- feols(registration_rate ~treat+dep+deptreat+dy1+dy2+dy3, data=subset(data_t1, bw<25000), cluster='cod_mun')
linearreg_75 <- feols(registration_rate ~treat+dep+deptreat+dy1+dy2+dy3, data=subset(data_t1, bw<75000), cluster='cod_mun')
quadreg_50 <- feols(registration_rate ~treat+dep+deptreat+dep2+deptreat2+dy1+dy2+dy3, data=subset(data_t1, bw<50000), cluster='cod_mun')
quadreg_75 <- feols(registration_rate ~treat+dep+deptreat+dep2+deptreat2+dy1+dy2+dy3, data=subset(data_t1, bw<75000), cluster='cod_mun')

#turnout
meantur <- feols(turnout_rate~1, data=subset(data_t1, bw<25000 & treat==0))
lineartur_50 <- feols(turnout_rate~treat+dep+deptreat+dy1+dy2+dy3, data=subset(data_t1, bw<50000), cluster='cod_mun')
lineartur_25 <- feols(turnout_rate~treat+dep+deptreat+dy1+dy2+dy3, data=subset(data_t1, bw<25000), cluster='cod_mun')
lineartur_75 <- feols(turnout_rate~treat+dep+deptreat+dy1+dy2+dy3, data=subset(data_t1, bw<75000), cluster='cod_mun')
quadtur_50 <- feols(turnout_rate~treat+dep+deptreat+dep2+deptreat2+dy1+dy2+dy3, data=subset(data_t1, bw<50000), cluster='cod_mun')
quadtur_75 <- feols(turnout_rate~treat+dep+deptreat+dep2+deptreat2+dy1+dy2+dy3, data=subset(data_t1, bw<75000), cluster='cod_mun')

models = list('Vote Share - 3rd and lower placed candidates'=list(mean3,linear3_50,linear3_25,linear3_75,quad3_50,quad3_75),
              'Vote Share - 4th and lower placed candidates'=list(mean4,linear4_50,linear4_25,linear4_75,quad4_50,quad4_75),
              'Vote Share - 5th and lower placed candidates'=list(mean5,linear5_50,linear5_25,linear5_75,quad5_50,quad5_75),
              'Registration rate' =list(meanreg,linearreg_50,linearreg_25,linearreg_75,quadreg_50,quadreg_75),
              'Turnout rate' =list(meantur,lineartur_50,lineartur_25,lineartur_75,quadtur_50,quadtur_75))

coefmap = c('(Intercept)' = 'Intercept',
            'treat' = 'effect')
gofmap = list(list("raw" = "nobs", "clean" = "$N$", "fmt" = 0))
modelsummary(models, shape='rbind', gof_map=gofmap, coef_map=coefmap, stars=T)

##################other RDD method from package
#rdrobust
rdrob <- rdrobust(data_f1$y_util,data_f1$electorate, c=200000)
summary(rdrob)

summary(rdplot(data_f1$y_util,data_f1$electorate, c=200000))