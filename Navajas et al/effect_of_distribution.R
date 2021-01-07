out_df = read.csv("navajas_et_al.csv", stringsAsFactors=F) %>%
  mutate(
      network="Decentralized"
    , dataset="navajas"
    , communication="Discussion"
    #, pre_influence = pre_influence_norm
    #, post_influence = post_influence_norm
    #, truth=truth_norm
  )

aggreg = out_df %>% 
  subset(!is.na(pre_influence) & !is.na(post_influence)) %>%
  group_by(task, trial, network, dataset, communication) %>%
  mutate(
    #  truth=truth/truth
      mu1 = mean(pre_influence)
    , err1 = abs(pre_influence - truth)
    , err2 = abs(post_influence - truth)
    , toward_truth = ifelse((pre_influence < mean(pre_influence) & mu1 <= truth) | (pre_influence > mu1 & mu1 >= truth), "Away","Toward")
  ) %>%
  summarize(
    truth=unique(truth)
    , N = length(pre_influence)
    
    ## calc mean
    , mu1 = mean(pre_influence)
    , mu2 = mean(post_influence)
    
    ## calc median
    , med1 = median(pre_influence)
    , med2 = median(post_influence) 
    
    ## raw change
    , change_mu = abs(mu1-mu2)
    
    ## error of mean
    , err_mu1 = abs(mu1 - truth)
    , err_mu2 = abs(mu2 - truth)
    , change_err_mu = mean(err_mu2 - err_mu1)/truth
    , mean_improve = ifelse(change_err_mu<0, "Improve","Worse")
    
    ## error of median
    , err_med1 = abs(med1 - truth)
    , err_med2 = abs(med2 - truth)
    , change_err_med = mean(err_med2 - err_med1)
    , med_improve = ifelse(change_err_med<0, "Improve","Worse")
    , med_improve = ifelse(change_err_med==0, "Same",med_improve)
    
    ## ind err
    , err_ind1 = mean(err1)
    , err_ind2 = mean(err2)
    , change_err_ind = mean(abs(err2)) - mean(abs(err1))
    , change_ind = mean(abs(pre_influence-post_influence))
    
    ## organizing stats
    , majority_away_truth = ifelse((med1 < mu1 & mu1 <= truth) | (med1 > mu1 & mu1 >= truth), "Away","Toward")
    , prop_toward = mean(toward_truth=="Toward")
    
    ## diversity
    , sd1 = sd(pre_influence)
    , sd2 = sd(post_influence)
    , change_sd = sd2-sd1
    
    ## measuring movement of median
    , med_mean_gap_1 = abs(med1-mu1)
    , med_mean_gap_2 = abs(med2-mu1)
    , change_med_gap = med_mean_gap_2 - med_mean_gap_1
    
  ) %>%
  mutate(
    prop_toward_round=round(prop_toward,1)
    #, prop_away_quarters = floor(prop_away_truth*5)/5
    , improve=(change_err_mu<0)*1
    , majority = ifelse(prop_toward>0.5, "Toward", NA)
    , majority = ifelse(prop_toward<0.5, "Away", majority)
    , majority = ifelse(prop_toward==0.5, "Split", majority)
  )




ag_sum = aggreg %>% 
  group_by(prop_toward_round,network,communication) %>%
  summarize(
    N = length(improve)
    ,upper=ifelse(mean(improve)%%1!=0, binom.test(table(improve))$conf.int[2], NA)
    ,lower=ifelse(mean(improve)%%1!=0, binom.test(table(improve))$conf.int[1], NA)
    ,improve = mean(improve)
  )

adjust=c(rep(0.07,7), -0.05)
ggplot(ag_sum %>% subset(communication=="Discussion"), 
       aes(x=prop_toward_round, y=improve)) +
  geom_errorbar(aes(ymin=1-lower, ymax=1-upper), color="red", alpha=0.2, size=6, width=0)+
  geom_point(size=3)+
  geom_hline(yintercept=0.5, linetype="dashed") + 
  geom_vline(xintercept=0.5, linetype="dashed") +
  geom_label(aes(label=paste0(N), y=improve+adjust), size=2, label.padding=unit(0.15,"lines"))+
  xlim(c(0,1))+ 
  scale_y_continuous(expand = c(0,0), lim=c(0,1))+
  #beckertheme +
  labs( y=""
        ,x="")

trial = aggreg %>%
  group_by(trial) %>%
  summarize(
      mu1 = mean(mu1)
    , mu2 = mean(mu2)
  )

byq = aggreg %>%
  group_by(task) %>%
  summarize(
      btw_group_1 = sd(mu1)
    , btw_group_2 = sd(mu2)
  )




