
# First steps 

# someSetup -------------
# clean memory


rm(list = ls())

# collecting_DataTable -------------

# github link to data:
link="https://github.com/eScienceWinterSchool/R_Session/raw/main/data/hdidemiso_plus.RDS"

#opening data
fileLink=url(link)
mergedData=readRDS(fileLink)

# knowing column names -------------
names(mergedData)

# data types-------------
str(mergedData)

# categorical exploration 

## frequency table (I)-----------
(FTRegime=table(mergedData$Regimetype)) #counts

## frequency table (II)-----------
(FTRegimeShare=prop.table(FTRegime)) #share

# compute statistics -------------
(mode=which.max(FTRegime)) #mode

(median=FTRegimeShare[cumsum(FTRegimeShare)>0.5][1])

# prepare plot -------------
barplot(FTRegime,
        main = "Democracy in the world (2021)",
        xlab = "Regime Type",
        ylab = "count of countries",
        col='orange',
        border='red')

# Numeric exploration 

## table of summary statistics ----

summary(Filter(is.numeric, mergedData)) # all numeric indicators

# numBoxplot ------
mM_Positions=grep('mM',names(mergedData))
boxplot(mergedData[,mM_Positions],# all numeric indicators mM
        las=2,
        horizontal = T,
        cex.axis=0.3,
        col='lightblue',
        border='red')

# linear regression 

## Hipotheses and regressions -----
h1=formula(HumanDevelopmentIndex~Functioningofgovernment)
regre1=glm(h1,data=mergedData,family = "gaussian")

h2=formula(HumanDevelopmentIndex~Functioningofgovernment + Politicalparticipation)
regre2=glm(h2,data=mergedData,family = "gaussian")

# show result of first regression -----
summary(regre1) # results
library(rsq)
rsq(regre1) # rsquare

# show result of second regression -----
summary(regre2)
lm.beta::lm.beta(regre2) # standardized coeff
rsq(regre2,adj=T)

# compare regressions -----
anova(regre1,regre2,test = 'Chisq') 

# logistic regression 

## Hipotheses and regressions -----

# data selection (just two values)
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

# compare regressions -----
anova(regreLogi1,regreLogi2,test = 'LRT')

## Clustering countries

# subset ----
positions_mM=grep("mM", names(mergedData)) #easy way detect positions
dataToCluster= mergedData[,positions_mM]
row.names(dataToCluster)=mergedData$Countryname

# clustering with k-medoids -----
set.seed(111) # for replicability
# process
library(cluster)
distances=cluster::daisy(dataToCluster)
res.pam=cluster::pam(distances,4,cluster.only = F)
#saving result in subset
dataToCluster$pam=res.pam$cluster

# understanding cluster labels  ----
pamMeans=aggregate(data=dataToCluster,Overallscore_mM~pam,mean)
pamMeans[order(pamMeans$Overallscore_mM),]

# recoding labels-----
recoded=dplyr::recode_factor(dataToCluster$pam,
                             `3`=1,`1`=2,`2`=3,`4`=4,
                             .ordered = T)
# new column 
mergedData$pam=recoded

## map file

# open file -----
# link in Github
linkMap="https://github.com/eScienceWinterSchool/R_Session/raw/main/worldMap.geojson"
# read in file
library(sf)
mapWorld=read_sf(linkMap)

# columns ------
names(mapWorld)

# quick look ------
head(mapWorld)

# structure ------
str(mapWorld)

# merge ----
mapWorld_Data=merge(mapWorld,mergedData,
                    by.x = 'ISO3', 
                    by.y = 'iso3')

# plot  ----

plot(mapWorld_Data['pam'], 
     main="Countries by HDI & Democracy\n(missing in grey)",
     border='grey50',
     lwd=0.4)


# customizing color -----
library(RColorBrewer)
TheColors <- brewer.pal(4, "YlOrRd")
plot(st_geometry(mapWorld),
     col = 'grey90', 
     main="Countries by HDI & Democracy",
     border = NA)
plot(mapWorld_Data['pam'], 
     border='grey50',
     lwd=0.4,
     col=TheColors[mapWorld_Data$pam],
     add=T) # line width 
legend("bottom",horiz = T,
       fill=TheColors,title = "Levels (ascending)",
       legend = c(1,2,3,4),xpd=T, 
       inset=c(0,-0.2))


