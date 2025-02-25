#setup
rm(list = ls()) # clear workspace

need <- c("modelsummary","tidyverse","rstudioapi","readstata13", "ggplot2") # list packages needed
have <- need %in% rownames(installed.packages()) # checks packages you have
if(any(!have)) install.packages(need[!have]) # install missing packages
invisible(lapply(need, library, character.only=T)) # load needed packages

setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #set working directory to the file directory
list.files()

#read data
data<-read.dta13("bsa2017.dta")

#Let's make a simple table
with(data, table(Country)) #within the dataframe "data", tabulate the variable "Country"

with(data, table(EURefb)) #within the dataframe "data", tabulate the variable "EURefb"


#Recode the variable EURefb into three categories
#First, let's create an empty object

data$EURef<-NA 

#Now let's fill in the values of the object

data$EURef[data$EURefb=="Remain a member of the European Union"]<-"Remain"
data$EURef[data$EURefb=="Leave the European Union"]<-"Leave"
data$EURef[data$EURefb=="I would not vote"]<-"Would not vote"

#Let's recode the variable into a numeric binary variable 
#First, let's create an empty object

data$EURef_num<-NA #Create an empty object

data$EURef_num[data$EURefb=="Remain a member of the European Union"]<-0
data$EURef_num[data$EURefb=="Leave the European Union"]<-1


#Tabulate left-right scale

with(data, table(leftrigh))

#the "99" value is strange. The codebook tells us that we are dealing with missing data

#Recode left-right scale to get rid of missing values (99)

data$leftright<-data$leftrigh
data$leftright[data$leftrigh==99]<-NA


#Intro to the tidyverse and the use of "pipes" %>% 

#In the tidyverse grammar, pipes are used to perform multiple commands (functions) subsequently, usually starting with a data frame.

#Take the data frame "data" and PIPE it into the count function

data %>% 
  count(EURef) 

#What is the proportion of voters who would vote leave in a second referendum?
#Use the summarise function

data %>% 
  summarise(mean_leave = mean(EURef_num, na.rm=TRUE))


#We want to know the mean of respondents who would vote Leave per GB nation

data %>% 
  group_by(Country)  %>% 
  summarise(mean_leave = mean(EURef_num, na.rm=TRUE))

#What is the average left-right position per GB nation? (lower values = more leftwing, higher values = more rightwing)

data %>% 
  group_by(Country)  %>% 
  summarise(mean_leftright = mean(leftright, na.rm=TRUE))

#Do the results square with your intuition about politics in the UK?

#Bar graph

ggplot(subset(data, !is.na(EURef)), aes(EURef, after_stat(count), fill = EURef)) + geom_bar() + xlab("Vote in 2nd referendum") + theme(legend.position = "none")

ggsave("eu_bar_count.pdf")

ggplot(subset(data, !is.na(EURef)), aes(EURef, after_stat(prop), group=1, fill=factor(after_stat(x)))) + geom_bar() + xlab("Vote in 2nd referendum")  + theme(legend.position = "none")

ggsave("eu_bar_prop.pdf")

#Histogram

ggplot(data, aes(x = leftright)) +  geom_histogram() + xlab("left-right scale") + geom_vline(aes(xintercept=mean(data$leftright, na.rm = TRUE)))

ggsave("left_right.png") #save histogram 
ggsave("left_right.pdf") #save histogram (vectorgraph)


#Boxplot

ggplot(data, aes(x=Country, y=leftright)) + stat_boxplot(geom = "errorbar",width = 0.25) +
  geom_boxplot()  + ylab("left-right scale") #plain plot

ggplot(data, aes(x=Country, y=leftright, fill=Country)) + stat_boxplot(geom = "errorbar",width = 0.25) +
  geom_boxplot()  + ylab("left-right scale") +
  theme(legend.position = "none") #Coloured

ggplot(data, aes(x=Country, y=leftright, fill=Country)) + stat_boxplot(geom = "errorbar",width = 0.25) +
  geom_boxplot()  + ylab("left-right scale") + coord_flip() #flipped

ggplot(data, aes(x=Country, y=leftright, fill=Country)) + stat_boxplot(geom = "errorbar",width = 0.25) +
  geom_boxplot()  + ylab("left-right scale") #Coloured (with legend)

ggplot(data, aes(x=Country, y=leftright, fill=Country)) + stat_boxplot(geom = "errorbar",width = 0.25) +
  geom_boxplot()  + ylab("left-right scale") + guides(fill = guide_legend(title = "Legend")) + 
  scale_fill_hue(labels = c("a", "b", "c")) #customise the legend

#boxplots are great for visualising the spread (variability) of your data


#Correlation

data$RAgeE->data$age
data$age[data$age==99]<-NA

data_tab1 <-
  data %>% 
  group_by(age)  %>% 
  summarise(mean_leave_agg = mean(EURef_num, na.rm=TRUE))

data_tab1 

#Please interpret the test statistics below

#Covariance
with(data_tab1, cov(age, mean_leave_agg, use="complete.obs")) 

#R statistic
with(data_tab1, cor(age, mean_leave_agg, use="complete.obs")) 

#R squared statistic
with(data_tab1, cor(age, mean_leave_agg, use="complete.obs")*cor(age, mean_leave_agg, use="complete.obs")) 


#Scatter plots

ggplot(data_tab1 , aes(x=age, y=mean_leave_agg)) +
  geom_point(size=2, shape=23) + ylab("Vote leave in 2nd referendum") + xlab("Age")  +  scale_x_continuous(breaks=c(20, 30, 40, 50, 60, 70, 80, 90, 100)) + theme_bw()

ggsave("scatter_age.pdf") #save graph in working directory

#Scatter plot with OLS regression line

ggplot(data_tab1 , aes(x=age, y=mean_leave_agg)) +
  geom_point(size=2, shape=23) + geom_smooth(method='lm', se=FALSE) + ylab("Vote leave in 2nd referendum") + xlab("Age") + scale_x_continuous(breaks=c(20, 30, 40, 50, 60, 70, 80, 90, 100)) + theme_bw()

ggsave("scatter_age_linear.pdf")

#make some nice summary statistic tables (Table 1)

url <- 'https://vincentarelbundock.github.io/Rdatasets/csv/palmerpenguins/penguins.csv' #load a cleaned dataset
penguins <- read_csv(url) #read the csv file into a data frame

#penguins$year=as.factor(penguins$year)

summary(penguins) #simple summary statistics

datasummary_balance(~1,data=penguins) #numeric and categorical variables in one table
datasummary_balance(~1,data=penguins, output="Table1.png") #in a picture (do not recommend)
datasummary_balance(~1,data=penguins, output="Table1.html") #in a webpage
datasummary_balance(~1,data=penguins, output="Table1.docx") #in a webpage
datasummary_balance(~1,data=penguins, output="latex") #in latex


datasummary_skim(penguins) #more information for numeric variables only
datasummary_skim(penguins, type = "categorical") #categorical variables only


datasummary(bill_length_mm + body_mass_g ~ sex*(Mean + SD), data=penguins) #customise - select needed variables only
datasummary((`Bill Length (mm)` = bill_length_mm) + (`Body Mass (g)` = body_mass_g) 
            ~ sex*(Mean + (Std.Dev. = SD)), data=penguins) #customise - compare different sexes
datasummary(species+1+island+1~sex*(N+Percent()), data=penguins) #customise - categorical variables +1 for "All"

datasummary_balance(~sex,data=penguins) #comparison by sex
