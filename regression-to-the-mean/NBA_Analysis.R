library(tidyverse)
library(lubridate)

box_scores = read_csv("2012-18_playerBoxScore.csv")

hist(box_scores$gmDate, breaks=50)


percentages = box_scores %>% mutate(season_ending_year = year(gmDate+90),
                      first_half = if_else(month(gmDate) %in%c(10, 11, 12, 1), "first_half", "second_half"),
                      name=paste(playFNm, playLNm)) %>%
  group_by(season_ending_year, first_half, name) %>%
  summarize(attempted = sum(`play3PA`), made = sum(`play3PM`)) %>%
#  group_by(name, season_ending_year) %>%
#  filter(all(attempted>=20), n()==2)%>%
  mutate(percentage = made/attempted) %>%
  ungroup %>%
  pivot_wider(id_cols=c(season_ending_year, name), values_from=c(attempted, percentage),
              names_from = first_half) %>%
  filter(attempted_first_half>=20) %>%
  drop_na()

percentages %>% filter(season_ending_year==2017) %>%
  ggplot(aes(x=percentage_first_half, y=percentage_second_half, size=attempted_first_half))+
  geom_point()+geom_smooth(method="lm", se=F)

summary(lm(percentage_second_half~percentage_first_half+attempted_first_half, data=percentages %>% filter(season_ending_year==2017)))

regression_2017 = lm(percentage_second_half~percentage_first_half, data=percentages %>% filter(season_ending_year==2017))
summary(regression_2017)
#Using regression observed in 2017, we create a prediction by taking their current %, multiplying by .478, and adding 0.16

percentages_2018 = percentages %>% filter(season_ending_year==2018)

##Bayesian model
p_0s = percentages_2018$percentage_first_half
s_0s = log(p_0s/(1-p_0s))
mu_0 =mean(s_0s)
sigma_0 = sd(s_0s)


zs = percentages_2018$percentage_first_half*percentages_2018$attempted_first_half
ns = percentages_2018$attempted_first_half

mu = c(mu_0, rep(0, 5000))
sigma = c(sigma_0, rep(0, 5000))
ss = rbind(s_0s, matrix(0, nrow=5000, ncol=length(s_0s)))

mu_like = function(mu, sigma, ss){
  sum(-1/2*((ss-mu)/sigma)^2)
}

sigma_like <- function(mu, sigma, ss){
  if(sigma<0)return(-Inf)
  mu_like(mu,sigma,ss)-log(sigma)*(length(ss)+2)
}

ss_like <- function(mu, sigma, ss, zs, ns){
  ps = exp(ss)/(1+exp(ss))
  sum(dbinom(zs, ns, ps, log=T))+mu_like(mu, sigma, ss)
}

for (i in 2:5001){
  mu_prop = mu[i-1] + rnorm(1, 0, .01)
  if (mu_like(mu_prop, sigma[i-1], ss[i-1,])-mu_like(mu[i-1], sigma[i-1], ss[i-1,])>log(runif(1))){
    #print(glue::glue("accepted mu prop {mu_prop}"))
    mu[i]=mu_prop
  }else mu[i]=mu[i-1]

  sigma_prop = sigma[i-1]+rnorm(1, 0, .02)
  if (sigma_like(mu[i], sigma_prop, ss[i-1,])-sigma_like(mu[i], sigma[i-1], ss[i-1,])>log(runif(1))){
    sigma[i]=sigma_prop
    #print(glue::glue("accepted sigma prop {sigma_prop}"))
  }else sigma[i]=sigma[i-1]


  ss_prop = ss[i-1,]
  for(j in 1:ncol(ss)){
    ss_cand = ss_prop
    ss_cand[j] = ss_cand[j]+rnorm(1, 0, .3)
    if (ss_like(mu[i], sigma[i], ss_cand, zs, ns)-ss_like(mu[i], sigma[i], ss_prop, zs, ns)>log(runif(1))){
      ss_prop=ss_cand
    }

  }
  ss[i,]=ss_prop
}


ss %>% apply(2, mean) %>% boot::inv.logit() %>% hist


preds=percentages_2018 %>%
  mutate(naive_prediction=percentage_first_half,
         previous_year_prediction = predict(regression_2017, .),
         bayes_prediction=ss[-(1:1000),] %>% apply(2, mean) %>% boot::inv.logit(),
         luck_prediction=mean(percentage_first_half)) %>%
  arrange(percentage_first_half)

preds%>%
 # slice((1:29)*10) %>%
  mutate(name=fct_inorder(factor(name))) %>%

  ggplot(aes(y=name, x=percentage_second_half))+
  geom_point(alpha= .5, size=2, aes(color="Truth", shape="Truth"))+
  geom_point(alpha= .5, aes(x=naive_prediction, color="Naive", shape="Pred"))+
  geom_point(alpha= .5, aes(x=previous_year_prediction, color="Prev. Year", shape="Pred"))+
  geom_point(alpha= .5, aes(x=bayes_prediction, color="Bayes", shape="Pred"))+
  geom_point(alpha= .5, aes(x=luck_prediction, color="Total Luck", shape="Pred"))+
  scale_y_discrete(name="<- Worse first half; Better first half ->", breaks=NULL)+
  scale_x_continuous(name="2nd Half Shooting Percentage", labels=scales::label_percent())+
  theme_minimal()+
  theme(legend.position = "bottom", legend.title = element_blank())+
  guides(shape=F)+
  ggtitle("2018 Second Half Predictions vs. Actual")

ggplot(preds, aes(x=percentage_first_half, y=bayes_prediction, size=attempted_first_half))+geom_point(alpha=.25)+
  geom_abline(aes(slope=1, color="Naive", intercept=0))+
  geom_abline(aes(slope=.4788, intercept=.167, color="Regressed"))+
  coord_fixed(xlim=c(.1, .5), ylim=c(.1,.5))

preds %>% summarize(across(ends_with("prediction"), ~sqrt(mean((.x-percentage_second_half)^2)))) %>%
  setNames(c("Naive (no regression toward mean)", "Using 2017 regression", "Bayes model", "Predict the mean")) %>%
  pivot_longer(everything()) %>%
  arrange(desc(value)) %>%
  mutate(name=fct_inorder(factor(name)))%>%
  ggplot(aes(y=value-.08, x=name))+
  geom_col()+
  scale_y_continuous("RMSE", labels=function(.x)scales::label_percent()(.x+.08))+
  xlab(NULL)+
  theme_minimal()+
  ggtitle("Prediction Accuracy")
