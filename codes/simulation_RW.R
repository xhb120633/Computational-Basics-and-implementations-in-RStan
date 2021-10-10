rm(list = ls())
graphics.off()
cat("\014")

#package preparation
library(ggplot2)
library(rstan)

## the theme for plotting 
pic_theme <- theme(axis.title = element_text(face="bold", size = 15),
                   axis.text = element_text(face="bold", size = 13),
                   legend.title = element_text(face="bold", size = 15),
                   legend.text = element_text(face="bold", size = 15),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black", size = 1),
                   axis.ticks = element_line(size = 1),
                   panel.grid.major.y = element_blank(),
                   panel.grid.minor.y = element_blank(),
                   panel.grid.major.x = element_blank(),
                   panel.grid.minor.x = element_blank(),
                   text = element_text(family = "sans"),
                   legend.position = c(0.10, 0.93))

##task setting (probability reversal learing task)
set.seed(2021)
reverse_trials=sample(c(8,9,10,11,12,13), replace = F)
ntrials=60
reward_prob=matrix(c(0.7,0.3,0.3,0.7),ncol=2,nrow=2)

##agent setting
nsubjects=60
lr=rnorm(n=nsubjects,mean=0.4,sd=0.2)
tau=rnorm(n=nsubjects,mean=3,sd=1)

#function of softmax/sigmoid
softmax<-function(v){
  i=length(v)
  p<-rep(0,i)
  for(d in 1:i){
    p[d]=exp(v[d])/sum(exp(v))
  }
  return(p)
}


#run the simulation
agent_data=list()
for (i in 1:nsubjects){
  tmp_lr=lr[i]
  tmp_tau=tau[i]
  v=matrix(rep(0,2*ntrials),nrow=2,ncol=ntrials)
  p=c(0,0)
  consistency=3**tmp_tau-1
  choice=rep(0,ntrials)
  reward=rep(0,ntrials)
  correct=rep(0,ntrials)
  n_reverse=1
  count_trial=0
  for (t in 1:ntrials){
    if(t>1){
      v[choice[t-1],t]<-v[choice[t-1],t-1]+tmp_lr*(reward[t-1]-v[choice[t-1],t-1])
    }
    p<-softmax(v[,t]*consistency)
    choice[t]=sample(c(1,2),prob = p,replace=T)
    reward[t]=sample(c(1,0),prob = reward_prob[choice[t],],replace=T)
    count_trial=count_trial+1
    if(count_trial==reverse_trials[n_reverse]){
      reward_prob=1-reward_prob
      count_trials=0
      n_reverse=n_reverse+1
    }
    if (reward_prob[choice[t],1]==0.7){
      correct[t]=1
    }else{
      correct[t]=0
    }
  }
  agent_data[[i]]=list(c(1:ntrials),choice,reward,correct,v,tmp_lr,tmp_tau)
}

###visualization
correct_matrix=matrix(rep(-1,nsubjects*ntrials),ncol=ntrials,nrow=nsubjects)
for (i in 1:nsubjects){
  correct_matrix[i,]=agent_data[[i]][[4]]
}
mean_correction<-apply(correct_matrix,2,mean)
mean_correction<-data.frame(c(1:ntrials),mean_correction)
colnames(mean_correction)<-c('Trial','acc')

reverse_trial=reverse_trials
for (i in 1:6){
  reverse_trial[i]=sum(reverse_trials[1:i])
}
reverse_trial<-as.data.frame(reverse_trial)

ggplot(data=mean_correction,aes(x=Trial,y=acc))+
  geom_line()+
  pic_theme