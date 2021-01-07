library(ggplot2)
library(dplyr)

##################
### SETUP DATA ###
##################

### Download data from PNAS server
d=read.csv(url("http://www.pnas.org/highwire/filestream/825101/field_highwire_adjunct_files/1/pnas.1722664115.sd01d.csv"))


# calculate the % correct at each Round
d = d %>% mutate(
    correct1 = Response_1<Endpoint
  , correct3 = Response_3<Endpoint
)

# retain only the data where people answered at both Round 1 and Round 3
d_valid = subset(d, !is.na(Response_1) & !is.na(Response_3))

# aggregate by group & political party
by_group = d_valid %>%
  group_by(Group_ID, Political, Condition) %>%
  summarize(
        correct1 = mean(correct1)
      , correct3 = mean(correct3)
      , N=n()
)

##########################
### Replicate Figure 4 ###
##########################

mean(subset(by_group, Condition=="Net_No_Prime")$correct3)

# arrange data
# (these lines must all be run together -- note the pipe operator)
reshape2::melt(by_group, id.vars=c("Group_ID","Political","Condition","N")) %>%
  # extract conditions of interest
  subset(., Condition %in% c("Net_No_Prime","Net_W_ID","Net_W_Prime","Control")) %>%
  #plot
  ggplot(., aes(x=variable, y=value, fill=Political)) + 
    stat_summary(fun.y="mean", geom="bar", position="dodge") +
    facet_grid(~Condition) +
    coord_cartesian(ylim=c(0.5,1))


#########################
### STATISTICAL TESTS ###
#########################


# Extract the 3 main conditions of interest
soc=subset(by_group, Condition %in% c("Net_No_Prime","Net_W_ID","Net_W_Prime", "Control"))

## A quick 'n dirty pairing for control
soc[soc$Condition=="Control" & soc$Political=="Liberal","Group_ID"]=1:12
soc[soc$Condition=="Control" & soc$Political=="Conservative","Group_ID"]=1:12

## Pair up repubcs & dems by trial
paired=soc %>%
  group_by(Group_ID, Condition) %>%
  summarize(
  # Calculate difference (polarization) at round 1
    diff_1 = abs(correct1[Political=="Conservative"]-correct1[Political=="Liberal"])
  
  # Calculate difference (polarizaiton) at round 2
  , diff_3 = abs(correct3[Political=="Conservative"]-correct3[Political=="Liberal"])
  
  # Calculate diff-in-diff (Change in polarization)
  , change=diff_3 - diff_1
)


## Plot change in polarization for every trial for the 3 main conditions
ggplot(paired, aes(x=Condition, y=change)) + geom_point() +
  stat_summary(fun.y="mean", color="red", geom="point", size=3) +
  labs(y="Change in Polarization", title="Red Dot = Mean")

paired %>% 
  group_by(Condition) %>%
  summarize(mean(change))

## Test whether they're different, and evaluate the mean
## Mean with ID:   -0.036 (decrease)
## Mean w/o prime: -0.037 (decrease)
## Not significantly different.
t.test( paired$change[paired$Condition=="Net_W_ID"]
        ,paired$change[paired$Condition=="Net_No_Prime"])

t.test(paired$change[paired$Condition=="Net_No_Prime"])



## A non-paremetric test.
wilcox.test( paired$change[paired$Condition=="Net_W_ID"]
             ,paired$change[paired$Condition=="Net_No_Prime"])



