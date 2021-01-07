rm(list=ls());gc();
library(readxl)
library(httr)
library(dplyr)
library(ggplot2)

source("https://raw.githubusercontent.com/joshua-a-becker/RTools/master/beckerfunctions.R")

raw_d = read.csv(url("http://www.pnas.org/highwire/filestream/30360/field_highwire_adjunct_files/1/pnas.1615978114.sd01.csv")
                 , stringsAsFactors=F)

d =  raw_d %>% 
  #subset(network!="Solo") %>%
  mutate(
    pre_influence = log(response_1)
  , post_influence = log(response_3)
  , truth_raw=truth
  , truth=log(truth)
  , question = paste0("becker",task)
  , trial=paste0(group_number, question)
  , err1 = abs(pre_influence-truth)
  , err3 = abs(post_influence-truth)
  , q = substr(question, 7, 7)
  ) %>% 
  subset(is.finite(pre_influence)&is.finite(post_influence))

aggreg = d %>% group_by(trial, q, task, group_number) %>% 
  subset(is.finite(post_influence)) %>%
  summarize(
      network=unique(network)
    , mu = ifelse(network=="Solo", mean(post_influence), mean(post_influence))
    , mu1 = mean(pre_influence)
    , mu3 = mean(post_influence) 
    , med1 = median(pre_influence)
    , med3 = median(post_influence) 
    , truth = unique(truth)
    , sd_pool = unique(sd_pool)
    , err_mu = abs(mu - truth)
    , err_mu1 = abs(mu1 - truth)
    , err_mu3 = abs(mu3 - truth)
    , err_ind_3 = mean(err3, na.rm=T)
    , change_err_mu = mean(err_mu3 - err_mu1)
    , change_mu = mu3 - mu1
    , majority_away_truth = ifelse((med1 < mu1 & mu1 <= truth) | (med1 > mu1 & mu1 >= truth), "Away","Toward")
    , mean_improve = ifelse(change_err_mu<0, "Improve","Worse")
  )

trial = aggreg %>%
  group_by(group_number, network) %>%
  summarize(
      
      abs_change_err_mu=mean(abs(change_err_mu))
    , abs_change_mu=mean(abs(change_mu))
    , change_err_mu=mean(change_err_mu)
  )

summary(lm(err_mu1 ~ network + q, aggreg))

ggplot(aggreg, aes(x=network, y=err_mu))+ 
  geom_point() +
  stat_summary(fun.y="mean", color="red", geom="point")

ggplot(aggreg, aes(x=network, y=err_mu1))+ 
  geom_point() +
  stat_summary(fun.y="mean", color="red", geom="point")

ggplot(aggreg, aes(x=network, y=err_mu3))+ 
  geom_point() +
  stat_summary(fun.y="mean", color="red", geom="point")

ggplot(aggreg, aes(x=network, y=change_err_mu))+ 
  geom_point() +
  stat_summary(fun.y="mean", color="red", geom="point")


q = d %>% 
  group_by(task) %>%
  summarize(
    q_err = abs(mean(pre_influence, na.rm=T) - unique(truth))
  )



sp_d = merge(aggreg, q, by="task")%>%subset(network!="Solo")

#sp_d = merge(aggreg, q, by="task")%>%subset(network=="Centralized")


ggplot(sp_d, 
       aes(x=q_err, y=(change_err_mu)*1, color=network))+ 
  #geom_point()+
  stat_summary(fun.y="mean", geom="point", size=2) +
  geom_smooth(method='lm',formula=y~x, fill=NA) + 
  geom_hline(yintercept=0)



summary(lm(change_err_mu ~ network + q_err*network,
  data=sp_d
))

summary(lm(change_err_mu ~ network + q_err,
            data=sp_d
))

summary(glm(change_err_mu<0 ~ network + q_err,
           data=sp_d
))


plot(sp_d$change_err_mu ~ sp_d$q_err, col=as.numeric(as.factor(sp_d$network)))
abline(coef=c(-0.0098+0.022, -0.138-0.057))
abline(coef=c(0.022, -0.138))


cent = subset(aggreg, network=="Centralized")
decent = subset(aggreg, network=="Decentralized")


t_cent = subset(trial, network=="Centralized")
t_decent = subset(trial, network=="Decentralized")


prop.table(table(cent$mean_improve))
prop.table(table(  cent$majority_away_truth
                   , cent$mean_improve), margin=1)

chisq.test(table(  cent$majority_away_truth
                   , cent$mean_improve))

t.test( abs(cent$change_err_mu)
       , abs(decent$change_err_mu))


wilcox.test( abs(cent$change_err_mu)
             , abs(decent$change_err_mu))

wilcox.test( abs(cent$change_err_mu)
             , abs(decent$change_err_mu))


mean( abs(t_cent$abs_change_err_mu) )
mean( abs(t_decent$abs_change_err_mu))


t.test(  t_cent$abs_change_err_mu
       , t_decent$abs_change_err_mu)


t.test(  t_cent$abs_change_mu
         , t_decent$abs_change_mu)


wilcox.test(  t_cent$abs_change_err_mu
              , t_decent$abs_change_err_mu)


summary(lm(change_err_mu ~ network + group_number + task, aggreg%>%subset(network!="Solo")))
summary(lm(abs(change_err_mu) ~ network + group_number + task, aggreg%>%subset(network!="Solo")))
summary(lm(abs(change_err_mu) ~ network + group_number, trial%>%subset(network!="Solo")))
 


byt=d %>%
  group_by(task) %>%
  summarize(
      mean=mean(response_1)
    , med=median(response_1)
    , theta=unique(truth_raw)
    , N=n()
    , med_mean = unique(med)<unique(mean)
    ,  mean_t = unique(mean)<unique(truth_raw)
    , med_mean_t = unique(med<mean) & unique(mean<truth_raw)
  )

View(byt)

