# load data

setwd(dirname(rstudioapi::getSourceEditorContext()$path))  # sets the directory of location of this script as the current directory

data<-read.csv('Hedge&Leonardsdata.csv')
# remove trials not included in RT analysis
data2<-data[data$Block!=1 & data$TrialCorrect!=0 & data$Trial_type!=0,] 
#aggregate
aggdata <-aggregate(data2$RT, by=list(data2$Experiment,data2$Participant,data2$Trial_type), 
                    FUN=median, na.rm=TRUE)
names(aggdata) <- c("Experiment", "PPT","Update_type","RT")
aggdata$PPT<-as.factor(aggdata$PPT)
aggdata$Update_type<-as.factor(aggdata$Update_type)
aggdata$Experiment<-as.factor(aggdata$Experiment)

# Run ANOVA (Experiment x Update type)
library(ez)
ezANOVA(data=aggdata, wid=PPT, dv=RT, within=Update_type, between=Experiment, type=3)
# Descriptives (Main effect of update type: repetition vs. switch)
ezStats(data=aggdata, wid=PPT, dv=RT, within=Update_type, type=3)
# Descriptives (Experiment x Update type)
ezStats(data=aggdata, wid=PPT, dv=RT, within=Update_type, between=Experiment, type=3)
