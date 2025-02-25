#setup
rm(list = ls()) # clear workspace
need <- c("modelsummary","rstudioapi","haven","fixest","car","parameters",'tidyverse') # list packages needed
have <- need %in% rownames(installed.packages()) # checks packages you have
if(any(!have)) install.packages(need[!have]) # install missing packages
invisible(lapply(need, library, character.only=T)) # load needed packages

setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #set working directory to the file directory
list.files()
source('hausman.R')
######################table 4##############################################
data <- read_dta("AJR2001.dta")
data <- as_factor(data) #load factor labels
data <- subset(data, baseco==1)

#base sample
iv1 = feols(logpgp95 ~1|avexpr~logem4,data=data)
summary(iv1, stage=1)
iv2 = feols(logpgp95 ~lat_abst|avexpr~logem4,data=data)
summary(iv2, stage=1)

#without neo-europes
iv3 = feols(logpgp95 ~1|avexpr~logem4,data=subset(data,rich4!=1))
summary(iv3, stage=1)
iv4 = feols(logpgp95 ~lat_abst|avexpr~logem4,data=subset(data,rich4!=1))
summary(iv4, stage=1)

#without africa
iv5 = feols(logpgp95 ~1|avexpr~logem4,data=subset(data,africa!=1))
summary(iv5, stage=1)
iv6 = feols(logpgp95 ~lat_abst|avexpr~logem4,data=subset(data,africa!=1))
summary(iv6, stage=1)

#with continent dummies
data$other_cont = ifelse(data$shortnam %in% c("AUS","MLT","NZL"),1,0)
iv7 = feols(logpgp95 ~africa+asia+other_cont|avexpr~logem4,data=data)
summary(iv7, stage=1)
iv8 = feols(logpgp95 ~lat_abst+africa+asia+other_cont|avexpr~logem4,data=data)
summary(iv8, stage=1)

#Base Sample, log GDP per worker
iv9 = feols(loghjypl ~1|avexpr~logem4,data=subset(data))
summary(iv9, stage=1)

#Panel A of Table 4
models_a <- list('Base sample(1)'= iv1,
                 'Base sample(2)'=iv2,
                 'Without Neo-Europes(1)'=iv3,
                 'Without Neo-Europes(2)'=iv4,
                 'Without Africa(1)'=iv5,
                 'Without Africa(2)'=iv6,
                 'with continent dummies(1)'=iv7,
                 'with continent dummies(2)'=iv8,
                 'Y=log output/worker'=iv9)
coef_map <- c('fit_avexpr' = 'Average protection against expropriation risk 1985-1995',
              'logem4' = 'Log European settler mortality',
              'lat_abst' = 'Lattitude',
              'asia' = "Asia dummy",
              'africa' = "Africa dummy",
              'other_cont' = '"Other" continent dummy')
format3=fmt_decimal(digits=3)
format1=fmt_decimal(digits=1)
glance_custom.fixest <- function(x, ...) {
  if ('iv_first_stage' %in% names(x)){
    data.frame('F_1st'= paste0(format1(fitstat(x, 'ivf1')[[1]]$stat),' [',format3(fitstat(x, 'ivf1')[[1]]$p),']'),
               'Hausman' = paste0(format1(fitstat(x, 'wh')[[1]]$stat),' [',format3(fitstat(x, 'wh')[[1]]$p),']'))
  } else {
    data.frame('F_1st'= paste0(format1(fitstat(x, 'ivf1')[[1]]$stat),' [',format3(fitstat(x, 'ivf1')[[1]]$p),']'))
  }
}
gof_map <- list(list("raw" = "F_1st", "clean" = "F-test (1st stage)", "fmt" = NULL),
                list("raw" = "Hausman", "clean" = "Wu-Hausman", "fmt" = NULL),
                list("raw" = "nobs", "clean" = "$N$", "fmt" = 0),
                list("raw" = "r.squared", "clean" = "$R^2$", "fmt" = 3),
                list("raw" = "adj.r.squared", "clean" = "$R^2$ Adj.", "fmt" = 3))

#second stage table
modelsummary(models_a, coef_map=coef_map, gof_map = gof_map, stars=TRUE)

#Panel b of Table 4 first stage table
models_b <- list('Base sample(1)'= iv1$iv_first_stage[[1]],
                 'Base sample(2)'=iv2$iv_first_stage[[1]],
                 'Without Neo-Europes(1)'=iv3$iv_first_stage[[1]],
                 'Without Neo-Europes(2)'=iv4$iv_first_stage[[1]],
                 'Without Africa(1)'=iv5$iv_first_stage[[1]],
                 'Without Africa(2)'=iv6$iv_first_stage[[1]],
                 'with continent dummies(1)'=iv7$iv_first_stage[[1]],
                 'with continent dummies(2)'=iv8$iv_first_stage[[1]],
                 'Y=log output/worker'=iv9$iv_first_stage[[1]])

modelsummary(models_b, stars = T, gof_map = gof_map, coef_map=coef_map)

#Tabel 4: aggregate table
models_c = list('Panel A: Two-Stage Least Squares'=models_a, 'Panel B: First Stage for Average Protection Against Expropriation Risk in 1985-1995'=models_b)
modelsummary(models_c, coef_map=coef_map, gof_map = gof_map, stars=TRUE, shape='rbind')

######################over-identification test##################################
overid <- read_dta("AJR overidentification.dta")
overid <- as_factor(overid) #load factor labels


oi1 = feols(logpgp95~1|avexpr~euro1900, data=subset(overid, baseco==1))
oi2 = feols(logpgp95~lat_abst|avexpr~euro1900, data=subset(overid, baseco==1))
oi3 = feols(logpgp95~1|avexpr~cons00a, data=subset(overid, baseco==1))
oi4 = feols(logpgp95~lat_abst|avexpr~cons00a, data=subset(overid, baseco==1))
oi5 = feols(logpgp95~1|avexpr~democ00a, data=subset(overid, baseco==1))
oi6 = feols(logpgp95~lat_abst|avexpr~democ00a, data=subset(overid, baseco==1))
oi7 = feols(logpgp95~indtime|avexpr~cons1, data=subset(overid, baseco==1))
oi8 = feols(logpgp95~lat_abst+indtime|avexpr~cons1, data=subset(overid, baseco==1))
oi9 = feols(logpgp95~indtime|avexpr~democ1, data=subset(overid, baseco==1))
oi10 = feols(logpgp95~lat_abst+indtime|avexpr~democ1, data=subset(overid, baseco==1))
models_d <- list(oi1,oi2,oi3,oi4,oi5,oi6,oi7,oi8,oi9,oi10)
coef_map <- c('fit_avexpr' = 'Average protection against expropriation risk 1985-1995',
              'lat_abst' = 'Lattitude')
modelsummary(models_d, coef_map=coef_map, gof_map=gof_map)

oi1e = feols(logpgp95~1|avexpr~euro1900+logem4, data=subset(overid, baseco==1))
oi2e = feols(logpgp95~lat_abst|avexpr~euro1900+logem4, data=subset(overid, baseco==1))
oi3e = feols(logpgp95~1|avexpr~cons00a+logem4, data=subset(overid, baseco==1))
oi4e = feols(logpgp95~lat_abst|avexpr~cons00a+logem4, data=subset(overid, baseco==1))
oi5e = feols(logpgp95~1|avexpr~democ00a+logem4, data=subset(overid, baseco==1))
oi6e = feols(logpgp95~lat_abst|avexpr~democ00a+logem4, data=subset(overid, baseco==1))
oi7e = feols(logpgp95~indtime|avexpr~cons1+logem4, data=subset(overid, baseco==1))
oi8e = feols(logpgp95~lat_abst+indtime|avexpr~cons1+logem4, data=subset(overid, baseco==1))
oi9e = feols(logpgp95~indtime|avexpr~democ1+logem4, data=subset(overid, baseco==1))
oi10e = feols(logpgp95~lat_abst+indtime|avexpr~democ1+logem4, data=subset(overid, baseco==1))
for (x in 1:10){
  oi <- get(paste0('oi',x))
  oie <- get(paste0('oi',x,'e'))
  oi$iv_first_stage[[1]]$overid <- hausman(consistent=oi, efficient=oie)
  assign(paste0('oi',x),oi)
}

models_e <- list(oi1$iv_first_stage[[1]],
                 oi2$iv_first_stage[[1]],
                 oi3$iv_first_stage[[1]],
                 oi4$iv_first_stage[[1]],
                 oi5$iv_first_stage[[1]],
                 oi6$iv_first_stage[[1]],
                 oi7$iv_first_stage[[1]],
                 oi8$iv_first_stage[[1]],
                 oi9$iv_first_stage[[1]],
                 oi10$iv_first_stage[[1]])

glance_custom.fixest <- function(x, ...) {
  data.frame('F_1st'= paste0(format1(fitstat(x, 'ivf1')[[1]]$stat),' [',format3(fitstat(x, 'ivf1')[[1]]$p),']'),
               'overid' = paste0('[',format3(x$overid$p.value),']'))
}

coef_map <- c('euro1900' = 'European settlements in 1900',
              'cons00a' = 'Constraint on executive in 1900',
              'democ00a' = 'Democracy in 1900',
              'cons1' = 'Constraint on executive in first year of independence',
              'democ1' = 'Democracy in first year of independence')

gof_map <- list(list("raw" = "overid", "clean" = "Overidentification test", "fmt" = NULL),
                list("raw" = "F_1st", "clean" = "F-test (1st stage)", "fmt" = NULL),
                list("raw" = "nobs", "clean" = "$N$", "fmt" = 0),
                list("raw" = "r.squared", "clean" = "$R^2$", "fmt" = 3),
                list("raw" = "adj.r.squared", "clean" = "$R^2$ Adj.", "fmt" = 3))
modelsummary(models_e, coef_map=coef_map, gof_map=gof_map)