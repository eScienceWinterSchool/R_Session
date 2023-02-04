
# someSetup -------------
# github data folder:
link1="https://github.com/profemagallanes-unmsm/"
link2="FGV_Python_prepublish/raw/main/DataFiles/"
where=paste0(link1,link2)

# collecting_DataTable -------------
#opening data
fileLink=url(paste0(where,"mergedData.RDS"))
mergedData=readRDS(fileLink)


# catexploreTable -----------
## display frequency table of Freedom Index
library(summarytools)

freq(mergedData[, 9], 
     plain.ascii = FALSE,
     report.nas=FALSE)

# prepare_catBarplot ----
## prepare frequency table as data frame
theTable_Free=table(mergedData$PressFreedomIndex_or)
theTable_Free_asDF=as.data.frame(theTable_Free)
names(theTable_Free_asDF)=c("var","count") #renaming

# catBarplot -----
library(ggplot2)
base=ggplot(theTable_Free_asDF,aes(x=var,y=count))
bar1=base + geom_bar(stat = 'identity')
bar1=bar1+labs(x="Press Freedom")
##plotting...
bar1

# summaryNumeric ----
## table of summary statistics
library(kableExtra)
library(vtable)
st(mergedData[,c("DemocracyIndex","mili_pctGDP")],
   title ="Stat summary for nummeric vars",
   digits = 2)

# numBoxplot ------

base=ggplot(data=mergedData)
box = base + geom_boxplot(aes(y=mili_pctGDP)) 
box = box + labs(x="", y = "% of country GDP")
box=box + theme(axis.text.x = element_blank(),
                axis.ticks.x = element_blank())
## plotting
box

# regresPrep -------

## prepare regressions
h1=formula(mili_pctGDP~DemocracyIndex)
regre1=lm(h1,data=mergedData)

h2=formula(mili_pctGDP~DemocracyIndex + IndexofEconomicFreedom)
regre2=lm(h2,data=mergedData)

# regs1 ----
## show result of both
summary(regre1)

summary(regre1)