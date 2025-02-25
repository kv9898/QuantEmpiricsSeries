#setup
rm(list = ls()) # clear workspace

need <- c("modelsummary","rstudioapi","fixest","car") # list packages needed
have <- need %in% rownames(installed.packages()) # checks packages you have
if(any(!have)) install.packages(need[!have]) # install missing packages
invisible(lapply(need, library, character.only=T)) # load needed packages

setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #set working directory to the file directory
list.files()

#make some nice summary statistic tables (Table 1)

url <- 'https://vincentarelbundock.github.io/Rdatasets/csv/palmerpenguins/penguins.csv' #load a cleaned dataset
penguins <- read_csv(url) #read the csv file into a data frame

#penguins$year=as.factor(penguins$year)

datasummary_balance(~sex,data=penguins, fmt=4, stars=TRUE) #comparison by sex
t1 <- t.test(bill_length_mm~factor(sex), data=penguins)
t1$stderr
t2 <- t.test(bill_depth_mm~factor(sex), data=penguins)
t2$stderr
t3 <- t.test(flipper_length_mm~factor(sex), data=penguins)
t3$stderr
t4 <- t.test(body_mass_g~factor(sex), data=penguins)
t4$stderr

lm1 <- lm(bill_length_mm~factor(sex), data=penguins)
summary(lm1)
lm2 <- lm(bill_depth_mm~factor(sex), data=penguins)
summary(lm2)
lm3 <- lm(flipper_length_mm~factor(sex), data=penguins)
summary(lm3)
lm4 <- lm(body_mass_g~factor(sex), data=penguins)
summary(lm4)

lm <- list('bill_length_mm'=lm1,'bill_depth_mm'=lm2,'flipper_length_mm'=lm3,'body_mass_g'=lm4)
modelsummary(lm,fmt=4,stars=TRUE, coef_rename=c('factor(sex)male'='difference by sex'))

tab1 <- data.frame('outcome'=c(names(lm1$model)[[1]],names(lm2$model)[[1]],names(lm3$model)[[1]],names(lm4$model)[[1]]),
                    't-test difference'=c(-get_estimates(t1)$estimate,-get_estimates(t2)$estimate,-get_estimates(t3)$estimate,-get_estimates(t4)$estimate),
                   'ols difference'=c(coef(lm1)[[2]],coef(lm2)[[2]],coef(lm3)[[2]],coef(lm4)[[2]]),
                   't-test se'=c(t1$stderr,t2$stderr,t3$stderr,t4$stderr),
                   'ols se'=c(sqrt(diag(vcov(lm1)))[[2]],sqrt(diag(vcov(lm2)))[[2]],sqrt(diag(vcov(lm3)))[[2]],sqrt(diag(vcov(lm4)))[[2]]))

m1 <- list(tidy=data.frame(
  term=c('t-test','ols'),
  estimate = c(tab1$t.test.difference[1],tab1$ols.difference[1]),
  std.error = c(tab1$t.test.se[1],tab1$ols.se[1]),
  p.value= c(t1$p.value,summary(lm1)$coefficients[ ,4][2]))
)
class(m1) <- "modelsummary_list"

m2 <- list(tidy=data.frame(
  term=c('t-test','ols'),
  estimate = c(tab1$t.test.difference[2],tab1$ols.difference[2]),
  std.error = c(tab1$t.test.se[2],tab1$ols.se[2]),
  p.value= c(t2$p.value,summary(lm2)$coefficients[ ,4][2]))
)
class(m2) <- "modelsummary_list"

m3 <- list(tidy=data.frame(
  term=c('t-test','ols'),
  estimate = c(tab1$t.test.difference[3],tab1$ols.difference[3]),
  std.error = c(tab1$t.test.se[3],tab1$ols.se[3]),
  p.value= c(t3$p.value,summary(lm3)$coefficients[ ,4][2]))
)
class(m3) <- "modelsummary_list"

m4 <- list(tidy=data.frame(
  term=c('t-test','ols'),
  estimate = c(tab1$t.test.difference[4],tab1$ols.difference[4]),
  std.error = c(tab1$t.test.se[4],tab1$ols.se[4]),
  p.value= c(t4$p.value,summary(lm4)$coefficients[ ,4][2]))
)
class(m4) <- "modelsummary_list"

m <- list('bill_length_mm'=m1,
          'bill_depth_mm'=m2,
          'flipper_length_mm'=m3,
          'body_mass_g'=m4)
modelsummary(m, stars=T)

#test for hetero - Breusch-Pagan test.
homotest1 <- ncvTest(lm1) #acceptable
homotest2 <- ncvTest(lm2) #very homo
homotest3 <- ncvTest(lm3) #dodgy
homotest4 <- ncvTest(lm4) #hetero for sure

#run ols with robust SE
lm1_robust <- feols(bill_length_mm~factor(sex), data=penguins,vcov='HC1')
summary(lm1_robust)
lm2_robust <- feols(bill_depth_mm~factor(sex), data=penguins,vcov='HC1')
summary(lm2_robust)
lm3_robust <- feols(flipper_length_mm~factor(sex), data=penguins,vcov='HC1')
summary(lm3_robust)
lm4_robust <- feols(body_mass_g~factor(sex), data=penguins,vcov='HC1')
summary(lm4_robust)

#add these results to the table

m1 <- list(tidy=data.frame(
  term=c('t-test','ols',"robust"),
  estimate = c(tab1$t.test.difference[1],tab1$ols.difference[1],coef(lm1_robust)[[2]]),
  std.error = c(tab1$t.test.se[1],tab1$ols.se[1],lm1_robust$se[[2]]),
  p.value= c(t1$p.value,summary(lm1)$coefficients[ ,4][2],get_estimates(lm1_robust)$p.value[[2]]))
)
class(m1) <- "modelsummary_list"

m2 <- list(tidy=data.frame(
  term=c('t-test','ols',"robust"),
  estimate = c(tab1$t.test.difference[2],tab1$ols.difference[2],coef(lm2_robust)[[2]]),
  std.error = c(tab1$t.test.se[2],tab1$ols.se[2],lm2_robust$se[[2]]),
  p.value= c(t2$p.value,summary(lm2)$coefficients[ ,4][2],get_estimates(lm2_robust)$p.value[[2]]))
)
class(m2) <- "modelsummary_list"

m3 <- list(tidy=data.frame(
  term=c('t-test','ols',"robust"),
  estimate = c(tab1$t.test.difference[3],tab1$ols.difference[3],coef(lm3_robust)[[2]]),
  std.error = c(tab1$t.test.se[3],tab1$ols.se[3],lm3_robust$se[[2]]),
  p.value= c(t3$p.value,summary(lm3)$coefficients[ ,4][2],get_estimates(lm3_robust)$p.value[[2]]))
)
class(m3) <- "modelsummary_list"

m4 <- list(tidy=data.frame(
  term=c('t-test','ols',"robust"),
  estimate = c(tab1$t.test.difference[4],tab1$ols.difference[4],coef(lm4_robust)[[2]]),
  std.error = c(tab1$t.test.se[4],tab1$ols.se[4],lm4_robust$se[[2]]),
  p.value= c(t4$p.value,summary(lm4)$coefficients[ ,4][2],get_estimates(lm4_robust)$p.value[[2]]))
)
class(m4) <- "modelsummary_list"

m <- list('bill_length_mm'=m1,
          'bill_depth_mm'=m2,
          'flipper_length_mm'=m3,
          'body_mass_g'=m4)
modelsummary(m, stars=T)
