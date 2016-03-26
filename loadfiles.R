##Load the Data.  DATA Must be in working directory!
library(plyr)
activity<-read.csv("./activity.csv")

sums<-ddply(activity, .(date), summarize, val = sum(steps)) 
hist(sums$val, main="Steps taken", xlab="Steps")
