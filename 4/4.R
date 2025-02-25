#setup
rm(list = ls()) # clear workspace

need <- c("modelsummary","rstudioapi","readstata13","fixest","car","parameters") # list packages needed
have <- need %in% rownames(installed.packages()) # checks packages you have
if(any(!have)) install.packages(need[!have]) # install missing packages
invisible(lapply(need, library, character.only=T)) # load needed packages

setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #set working directory to the file directory
list.files()

#read data
data<-read.dta13("caschool.dta")

#regress test score on student-teacher ratio and pctel
lm <- feols(testscr~str+PctEL, data=data, vcov="HC1")
modelsummary(lm, stars=T)
fitstat(lm,"f")

#alternative model
lm1 <- feols(testscr~str+expn_stu+PctEL, data=data, vcov="HC1")
models <- list(lm,lm1)
modelsummary(models, stars=T)

#f-test
lm1$df.residual=degrees_of_freedom(lm1) #add degrees of freedom information to the model
linearHypothesis(lm1, c("str=0", "expn_stu=0"), test="F")
linearHypothesis(lm1, "str=expn_stu", test="F")

rows <- tribble(~term, ~"(1)", ~"(2)",
                "F(str=expn_stu=0)", "","5.434**")
attr(rows, 'position') <- 9
modelsummary(models, stars=T, add_rows=rows)

summary$'(2)'$tidy <- rbind(summary$'(2)'$tidy,list("F(str=expn_stu=0)",ftest$F,NA,ftest$F,ftest$F,ftest$`Pr(>F)`,""))
