
# someSetup -------------
# clean memory
rm(list = ls())

# github link to data:
link="https://github.com/eScienceWinterSchool/PythonSession/raw/master/hdidem_plus.RDS"

# collecting_DataTable -------------
#opening data
fileLink=url(link)
mergedData=readRDS(fileLink)

# knowing column names -------------
names(mergedData)

# data types-------------
str(mergedData)

# categorical exploration -----------
## display frequency table of  Regime Types
library(summarytools)

freq(mergedData$Regimetype, 
     plain.ascii = FALSE,
     report.nas=FALSE)

# compute statistics -------------

DescTools::Mode(mergedData$Regimetype)
DescTools::Median(mergedData$Regimetype)

# prepare plot -------------
barplot(table(mergedData$Regimetype),
        main = "Democracy in the world (2021)",
        xlab = "Regime Type",
        ylab = "count of countries",
        las=2,
        col='orange',
        border='red',
        cex.names=0.5)

# Explore Numeric 

## table of summary statistics ----
summary(mergedData[,c(9:13)]) # democracy indicators

# numBoxplot ------
boxplot(mergedData[,c(9:13)],
        las=2,
        horizontal = T,
        cex.axis=0.3,
        col='lightblue',
        border='red')

# linear regression -----

## prepare regressions
h1=formula(HumanDevelopmentIndex~Functioningofgovernment)
regre1=glm(h1,data=mergedData,family = "gaussian")

h2=formula(HumanDevelopmentIndex~Functioningofgovernment + Politicalparticipation)
regre2=glm(h2,data=mergedData,family = "gaussian")

# show result of first regression -----
summary(regre1)
library(rsq)
rsq(regre1)

# show result of second regression -----
summary(regre2)
lm.beta::lm.beta(regre2)
rsq(regre2,adj=T)

# compare regression -----
anova(regre1,regre2)

# logistic regression -----
Selection=c('Authoritarian','Flawed democracy')
mergedData_Dico=mergedData[mergedData$Regimetype%in%Selection,]

h1=formula(Regimetype~MeanYearsOfSchooling)
regreLogi1=glm(h1,data=mergedData_Dico,family = "binomial")

h2=formula(Regimetype~MeanYearsOfSchooling + LifeExpectancyAtBirth)
regreLogi2=glm(h2,data=mergedData_Dico,family = "binomial")

# show result of first regression -----
summary(regreLogi1)

# show result of second regression -----
summary(regreLogi2)

# compare regression -----
anova(regreLogi1,regreLogi2)
