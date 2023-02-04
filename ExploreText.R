# someSetup ----
# github data folder:
link1="https://github.com/profemagallanes-unmsm/"
link2="FGV_Python_prepublish/raw/main/DataFiles/"
where=paste0(link1,link2)

# collecting_DataText ----

location=paste0(where,'rebelData.csv')
wordData=read.csv(location)

# prepareDataForCloud ----
library(tidyr)
wordDataNames=separate_rows(wordData,StateName) # key process
stateCounts=as.data.frame(table(wordDataNames$StateName))
names(stateCounts)=c("state","aCount")


# theCloudPlot ----

library(ggwordcloud)

set.seed(123)
base = ggplot(stateCounts, aes(label = state,
                               size= aCount,
                               color = aCount))
cloudRebel= base + geom_text_wordcloud() + theme_minimal()
cloudRebel= cloudRebel+scale_color_gradient(low = "orange",
                                            high = "red") 
cloudRebel + labs(title = "Nations by presence of rebel movements.")
