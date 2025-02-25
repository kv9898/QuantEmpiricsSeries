#setup
rm(list = ls()) # clear workspace

need <- c("modelsummary","rstudioapi","haven","fixest","car","parameters") # list packages needed
have <- need %in% rownames(installed.packages()) # checks packages you have
if(any(!have)) install.packages(need[!have]) # install missing packages
invisible(lapply(need, library, character.only=T)) # load needed packages

setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #set working directory to the file directory
list.files()

#read data
data<-read_dta("fatality.dta")
data <- as_factor(data) #load factor labels

#regress traffic fatality rate on beer tax + fixed effects
lm1 <- feols(mrall*10000~beertax, data=data, vcov="HC1")
lm2 <- feols(mrall*10000~beertax|state, data=data)
lm3 <- feols(mrall*10000~beertax+factor(year)|state, data=data)

models <- list(lm1,lm2,lm3)

coef.map <- c("beertax" = "Beer tax") #in the order that will displayed in the table
glance_custom.fixest <- function(x, ...) {
  data.frame(
    "Cluster" = ifelse(attributes(broom::tidy(x)$std.error)$type=="Clustered (state)","Yes","No"),
    'fe.s'=ifelse("state" %in% names(x$fixef_id),"Yes","No"),
    'fe.y'=ifelse("factor(year)1983" %in% names(x$coefficients),"Yes","No"))}

gofmap <- list(list("raw" = "fe.s", "clean" = "FE: State", "fmt" = NULL),
               list("raw" = "fe.y", "clean" = "FE: Year", "fmt" = NULL),
               list("raw" = "Cluster","clean"="Clustered SE", "fmt"=NULL),
               list("raw" = "nobs", "clean" = "$N$", "fmt" = 0),
               list("raw" = "adj.r.squared", "clean" = "$R^2$ Adj.", "fmt" = 3))     

modelsummary(models, stars=T, coef_map = coef.map, gof_map=gofmap, escape=FALSE)

# + control variables
data$jailorcomser <- ifelse(data$jaild==1 | data$comserd==1,1,0) 
data$vmiles_t <- data$vmiles/1000

data$drink18 <- ifelse(data$mlda>=18 & data$mlda<19,1,0)
data$drink19 <- ifelse(data$mlda>=19 & data$mlda<20,1,0)
data$drink20 <- ifelse(data$mlda>=20 & data$mlda<21,1,0)

lm4 <- feols(mrall*10000~beertax+drink18+drink19+drink20+jailorcomser+vmiles_t+unrate+log(perinc)+factor(year)|state, data=data)
lm5 <- feols(mrall*10000~beertax+drink18+drink19+drink20+jailorcomser+vmiles_t+factor(year)|state, data=data)
lm6 <- feols(mrall*10000~beertax+mlda+jailorcomser+vmiles_t+unrate+log(perinc)+factor(year)|state, data=data)

models <- list(lm1,lm2,lm3,lm4,lm5,lm6)
coef.map <- c("beertax" = "Beer tax",
              "drink18" = "Drinking age 18",
              "drink19" = "Drinking age 19",
              "drink20" = "Drinking age 20",
              "mlda"    = "Drinking age",
              "jailorcomser"="Mandatory jail or community service?",
              "vmiles_t"= "Average vehicle (thousand) miles per driver",
              "unrate"  = "Unemployement rate",
              "log(perinc)"="Real income per capita\n(logarithm)"
)
modelsummary(models, stars=T, coef_map = coef.map, gof_map=gofmap)

###################f-tests starts###############################################
lm3$df.residual=degrees_of_freedom(lm3) #add degrees of freedom information to the model
lm4$df.residual=degrees_of_freedom(lm4)
lm5$df.residual=degrees_of_freedom(lm5)
lm6$df.residual=degrees_of_freedom(lm6)

#time effects
time_hypotheses <- c()
for (x in 1983:1988){
  time_hypotheses <- append(time_hypotheses,paste0("factor(year)",x,"=0"))
}

for (x in 3:6){
  lm_obj <- get(paste0("lm", x))
  a <- linearHypothesis(lm_obj, time_hypotheses, test="F")
  p <- ifelse(a[2,4]<0.001,"<0.001",round(a[2,4],3))
  lm_obj$time_effects <- paste0(round(a[2,3],2),"\n(",p,")")
  assign(paste0("lm", x), lm_obj)
}

#drinking age
a <- linearHypothesis(lm4, c("drink18=0","drink19=0","drink20=0"), test="F")
p <- ifelse(a[2,4]<0.001,"<0.001",round(a[2,4],3))
lm4$f_drinking <- paste0(round(a[2,3],2),"\n(",p,")")
a <- linearHypothesis(lm5, c("drink18=0","drink19=0","drink20=0"), test="F")
p <- ifelse(a[2,4]<0.001,"<0.001",round(a[2,4],3))
lm5$f_drinking <- paste0(round(a[2,3],2),"\n(",p,")")

#unemployment rate and income per capita
a <- linearHypothesis(lm4, c("unrate=0","log(perinc)=0"), test="F")
p <- ifelse(a[2,4]<0.001,"<0.001",round(a[2,4],3))
lm4$unandin <- paste0(round(a[2,3],2),"\n(",p,")")
a <- linearHypothesis(lm6, c("unrate=0","log(perinc)=0"), test="F")
p <- ifelse(a[2,4]<0.001,"<0.001",round(a[2,4],3))
lm6$unandin <- paste0(round(a[2,3],2),"\n(",p,")")

glance_custom.fixest <- function(x, ...) {
  data.frame("Cluster" = ifelse(attributes(broom::tidy(x)$std.error)$type=="Clustered (state)","Yes","No"),
    'fe.s'=ifelse("state" %in% names(x$fixef_id),"Yes","No"),
    'fe.y'=ifelse("factor(year)1983" %in% names(x$coefficients),"Yes","No"),
    'timeeffects'=ifelse(exists("time_effects", x),x$time_effects,""),
    'f_drinking'=ifelse(exists("f_drinking", x),x$f_drinking,""),
    'unandin'=ifelse(exists("unandin", x),x$unandin,"")
    )}

gofmap <- list(list("raw" = "fe.s", "clean" = "FE: State", "fmt" = NULL),
               list("raw" = "fe.y", "clean" = "FE: Year", "fmt" = NULL),
               list("raw" = "Cluster","clean"="Clustered SE", "fmt"=NULL),
               list("raw" = "nobs", "clean" = "$N$", "fmt" = 0),
               list("raw" = "timeeffects", "clean" = "Time effects=0", "fmt" = NULL),
               list("raw" = "f_drinking", "clean" = "Drinking age coefficients=0", "fmt" = NULL),
               list("raw" = "unandin", "clean" = "u/e and income(pc)=0", "fmt" = NULL),
               list("raw" = "adj.r.squared", "clean" = "$R^2$ Adj.", "fmt" = 3))     
models <- list(lm1,lm2,lm3,lm4,lm5,lm6)
modelsummary(models, stars=T, coef_map = coef.map, gof_map=gofmap)
