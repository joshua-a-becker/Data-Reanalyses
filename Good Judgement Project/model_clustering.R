rm(list=ls());gc()
library(stats)
library(tidyverse)

last=function(x){x[length(x)]}

d.1 = read.csv("data/survey_fcasts.yr1.csv") %>%
  mutate(year=1)
d.2 = read.csv("data/survey_fcasts.yr2.csv") %>%
  mutate(year=2) %>%
  subset(user_id %in% d.1$user_id)

d.3 = read.csv("data/survey_fcasts.yr3.csv") %>%
  mutate(year=3)


d = rbind(d.1, d.2, d.3)

q_all = read.csv("data/ifps.csv")
q = q_all %>%
  subset(
    q_status=="closed" 
    & q_type == 0
    & n_opts==2
    ) %>%
  mutate(
    outcome = outcome=='a'
  ) %>%
  select(c("ifp_id","q_status","outcome"))

myd = d %>%
  subset(
    substr(ctt,1,2)%in%c("1a") 
    & ifp_id %in% q$ifp_id
    & answer_option == 'a'
    ) %>%
  merge(q, by="ifp_id")

ind_d=myd %>% 
  group_by(ifp_id, user_id, year) %>%
  summarize(
      final_answer = value[fcast_date==max(fcast_date)] %>% last
    , initial_answer = value[fcast_date==min(fcast_date)][1]
    , did_change = length(fcast_date)>1
    , count = length(fcast_date)
    , outcome=unique(outcome)
    , brier_final = (final_answer - outcome)^2
    , brier_initial = (initial_answer - outcome) ^2
    , conf = abs(initial_answer-0.5)*2
  )

### set up data from year 1 to run clustering algorithm

by_user = ind_d %>%
  subset(year==1) %>%
  pivot_wider(  id_cols=c(user_id)
              , values_from=final_answer
              , names_from=ifp_id)

u_mat = by_user[,-1] %>%
  as.matrix

u_mat[is.na(u_mat)]<-0.5

my_clust = kmeans(u_mat, centers=10, nstart=25)

by_user$cluster = my_clust$cluster


with_cluster = ind_d %>% 
  merge(by_user[,c("user_id","cluster")], by="user_id") %>%
  group_by(ifp_id) %>%
  mutate(
    normalized = final_answer - mean(final_answer)
  )

### test whether
### clustering from year 1
### predicts year 2 and 3

aov(normalized ~ as.factor(cluster), 
    with_cluster %>% subset(year==2)
    ) %>%
  summary

aov(normalized ~ as.factor(cluster), 
    with_cluster %>% subset(year==3)
) %>%
  summary



###
### try some regrouping...
### and see if diversity impacts accuracy!
###

assemble_group = function(n, k, clusters) {
  use_clusters = sample(unique(clusters), k)
  sample(which(clusters %in% use_clusters), n)
}


set.seed(42)
synthetic = data.frame(
    est=numeric()
  , ifp_id=character()
  , clust = numeric()
  , m=numeric()
  , n=numeric()
  , year=numeric()
)

for(i in 1:100) {
  print(i)
  for(m in 1:10) {
    for(n in c(5,10,15,20,25)) {
      for(yr in 1:3) {
        this_yr = subset(with_cluster, year==yr)
        for(q_id in unique(with_cluster$ifp_id)) {
          
          dat = subset(this_yr, ifp_id == q_id)
          
          tryCatch(# so if it can't find the right combo of people to form a group
          ########## it moves on
            {    
              group = dat[assemble_group(n, m, dat$cluster),]
              synthetic[nrow(synthetic)+1,] = data.frame(
                  est = mean(group$final_answer)
                , ifp_id = q_id
                , clust = paste0(group$cluster,collapse=",")
                , m=m
                , n=n
                , year=yr
              )
            }, error=function(e){}
          )
        }
      }
    }
  }
}




final_d = synthetic %>%
  merge(q[,c("ifp_id","outcome")]) %>%
  rowwise%>%
  mutate(
      brier = (est-outcome*1)^2
    , diversity = 1/sum(prop.table(table(
        strsplit(clust,",")%>%unlist%>%as.numeric
      ))^2)
  )


ggplot(final_d, aes(x=m, y=brier)) +
  stat_summary(fun="mean", geom="point") +
  facet_wrap(n~year)

ggplot(final_d, aes(x=m, y=diversity)) +
  stat_summary(fun="mean", geom="point")


ggplot(final_d, aes(x=diversity, y=brier)) +
  stat_summary(fun="mean", geom="point")


my_stats = final_d %>%
  group_by(ifp_id, year, n) %>%
  summarize(
    cor.m = cor.test(brier, m)$est
    ,cor.div = cor.test(brier, diversity)$est
  )

wilcox.test(my_stats$cor.m)
wilcox.test(my_stats$cor.div)
