rm(list=ls());gc()
library(dplyr)
source("https://raw.githubusercontent.com/joshua-a-becker/RTools/master/beckerfunctions.R")


#

### download main experimental data
d <- read.csv(url("http://finzi.psych.upenn.edu/~baron/R/bg/lorenz/newDataApr30.csv"), stringsAsFactors=F, header=T) %>%
  group_by(question.no) %>%
  mutate(
      truth=unique(true.values)
    ### skip zero values b/c they're non-sensical
    , err1 = abs(log(est1[est1!=0]/truth))
    , err2 = abs(log(est2[est2!=0]/truth))
  )

### confidence negatively correlated with error
### (more confidence, less error)
cor.test(d$conf1, d$err1)
cor.test(d$conf2, d$err2)

### confidence goes up
mean(d$conf1, na.rm=T)
mean(d$conf2, na.rm=T)
