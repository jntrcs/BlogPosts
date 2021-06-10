library(tidyverse)
library(tayloRswift)

set.seed(123)
teams = tibble(id = 1:31, skill = rnorm(31, .5, .05),
                 first_half = rbinom(31, 50, skill),
                 observed_skill = first_half/50)

teams %>% arrange(observed_skill) %>%
  mutate(id = fct_inorder(as.character(id))) %>%
  ggplot(aes(y = id))+
  geom_segment(linetype=1, aes(x=.5, xend=skill, yend=id, color="Skill"))+
  geom_segment(linetype=2, aes(color=if_else(observed_skill>skill, "Good Luck", "Bad Luck"), yend=id, x=skill, xend = observed_skill))+
  geom_point(aes(x=observed_skill))+
  scale_x_continuous("Observed FG Percentage", labels=scales::label_percent())+
  scale_y_discrete(name="Mid Season Rank", labels=31:1)+
  theme_minimal()+
  #scale_color_taylor(palette = "taylorSwift")+
  #scale_color_manual(values=tayloRswift::swift_palettes$taylorSwift[c(1,6,4)])+
  guides(color=guide_legend(title=NULL))+
  theme(legend.position = "bottom")+
  ggtitle("FG% as a Mixture of Skill and Luck")

set.seed(123)
teams = tibble(id = 1:31, skill = rnorm(31, .5, .05),
               first_half = rbinom(31, 50, skill),
               observed_skill = first_half/50)

teams %>% arrange(observed_skill) %>%
  mutate(id = fct_inorder(as.character(id))) %>%
  ggplot(aes(y = id))+
  geom_segment(linetype=1, aes(x=.5, xend=skill, yend=id, color="Skill"))+
  geom_segment(linetype=2, aes(color=if_else(observed_skill>skill, "Good Luck", "Bad Luck"), yend=id, x=skill, xend = observed_skill))+
  geom_point(aes(x=observed_skill))+
  scale_x_continuous("Test Score", labels=function(x)x*100)+
  scale_y_discrete(name="Class Rank", labels=31:1)+
  theme_minimal()+
  scale_color_manual(values=tayloRswift::swift_palettes$taylorSwift[c(1,6,4)])+
  guides(color=guide_legend(title=NULL))+
  theme(legend.position = "bottom")

teams %>% arrange(observed_skill) %>%
  mutate(id = fct_inorder(as.character(id))) %>%
  ggplot(aes(y = id))+
  geom_segment(linetype=1, aes(x=.5, xend=observed_skill, yend=id, color="Skill"))+
  #geom_segment(linetype=2, aes(color=if_else(observed_skill>skill, "Good Luck", "Bad Luck"), yend=id, x=skill, xend = observed_skill))+
  geom_point(aes(x=observed_skill))+
  scale_x_continuous("Test Score", labels=function(x)x*100)+
  scale_y_discrete(name="Class Rank", labels=31:1)+
  theme_minimal()+
  guides(color=guide_legend(title=NULL))+
  scale_color_manual(values=tayloRswift::swift_palettes$taylorSwift[c(4)])+
  theme(legend.position = "bottom")

teams %>% arrange(observed_skill) %>%
  mutate(id = fct_inorder(as.character(id))) %>%
  ggplot(aes(y = id))+
  geom_segment(linetype=2, aes(x=.5, xend=observed_skill, yend=id, color=if_else(observed_skill>.5, "Good Luck", "Bad Luck")))+
  #geom_segment(linetype=2, aes(color=if_else(observed_skill>skill, "Good Luck", "Bad Luck"), yend=id, x=skill, xend = observed_skill))+
  geom_point(aes(x=observed_skill))+
  scale_x_continuous("Test Score", labels=function(x)x*100)+
  scale_y_discrete(name="Class Rank", labels=31:1)+
  theme_minimal()+
  guides(color=guide_legend(title=NULL))+
  theme(legend.position = "bottom")+
  scale_color_manual(values=tayloRswift::swift_palettes$taylorSwift[c(1,6)])
  #scale_color_manual(values="blue")



#Now let's simulate the second half of the season
full_season = teams %>% mutate(second_half = rbinom(31, 50, skill), whole_season = first_half+second_half,
                 observed_skill_season = whole_season/100,
                 naive_prediction = first_half*2)

ggplot(full_season)+
  geom_abline( aes(slope=2, intercept= 0, color="Naive Prediction"), key_glyph=draw_key_path)+
  geom_abline( aes(slope=0, intercept= 500, color="Total Luck"), key_glyph=draw_key_path)+
  geom_point(aes(x=first_half*10, y=whole_season*10, fill="Actual Shots Made"))+
  geom_smooth(aes(x=first_half*10, y=whole_season*10, color="Regression to the Mean Line"), method="lm", se=F)+
  ylim(c(300, 800))+
  ylab("Shots made over whole season")+
  xlab("Shots made first half of season")+
  theme_minimal()+
  guides(color=guide_legend(title=NULL), fill=F)+
  theme(legend.position = "bottom")+
  ggtitle("Regression to the Mean", "Each point represents a team's shooting")


#### Prove that extrapolating the performance is a biased estimator for "top of the league" performance

one_season <-function(){
  tibble(ID = 1:31, s = rnorm(31, 0, 1), p=boot::inv.logit(s),
                   success_first_half = rbinom(31, 40, p),
                   success_second_half = rbinom(31, 40, p),
                   season_performance = (success_first_half+success_second_half)/80,
                   predicted_performance=success_first_half/40,
                   bias = season_performance - predicted_performance) %>%
    arrange(success_first_half) %>%
    slice(1, 16, 31) %>%
    pull
}

reps = replicate(5000, one_season())
hist(reps[1,])
hist(reps[2,])
hist(reps[3,])

test_data = tibble(ID = 1:31, s = rnorm(31, 0, 1), p=boot::inv.logit(s),
       success_first_half = rbinom(31, 40, p))
mu_0 =0
sigma_0 = 1
p_0s = test_data$success_first_half/40
s_0s = log(p_0s/(1-p_0s))

zs = test_data$success_first_half
ns = rep(40, nrow(test_data))

mu = c(mu_0, rep(0, 5000))
sigma = c(sigma_0, rep(0, 5000))
ss = rbind(s_0s, matrix(0, nrow=5000, ncol=length(s_0s)))

mu_like = function(mu, sigma, ss){
  exp(sum(-1/2*((ss-mu)/sigma)^2))
}

sigma_like <- function(mu, sigma, ss){
  mu_like(mu,sigma,ss)/sigma^(length(ss)+2)
}

ss_like <- function(mu, sigma, ss, zs, ns){
  ps = exp(ss)/(1+exp(ss))
  exp(sum(dbinom(zs, ns, ps, log=T)))*mu_like(mu, sigma, ss)
}

for (i in 2:5001){
  mu_prop = mu[i-1] + rnorm(1, 0, .7)
  if (mu_like(mu_prop, sigma[i-1], ss[i-1,])/mu_like(mu[i-1], sigma[i-1], ss[i-1,])>runif(1)){
    #print(glue::glue("accepted mu prop {mu_prop}"))
    mu[i]=mu_prop
  }else mu[i]=mu[i-1]

  sigma_prop = sigma[i-1]+rnorm(1, 0, .15)
  if (sigma_like(mu[i], sigma_prop, ss[i-1,])/sigma_like(mu[i], sigma[i-1], ss[i-1,])>runif(1)){
    sigma[i]=sigma_prop
    #print(glue::glue("accepted sigma prop {sigma_prop}"))
  }else sigma[i]=sigma[i-1]


  ss_prop = ss[i-1,]
  for(j in 1:ncol(ss)){
    ss_cand = ss_prop
    ss_cand[j] = ss_cand[j]+rnorm(1, 0, .3)
    if (ss_like(mu[i], sigma[i], ss_cand, zs, ns)/ss_like(mu[i], sigma[i], ss_prop, zs, ns)>runif(1)){
      ss_prop=ss_cand
    }

  }
  ss[i,]=ss_prop
}
plot(mu)
plot(sigma)
plot(ss[,1])
ss_est = apply(ss, 2, mean)
plot(ss_est, test_data$s)

prev_season = tibble(ID = 1:31, s = rnorm(31, 0, 1), p=boot::inv.logit(s),
                   success_first_half = rbinom(31, 40, p),
                   success_second_half=rbinom(31, 40, p))
prev_season_mod = lm(success_second_half~success_first_half, data=prev_season)

post_pred=apply(ss[-c(1:1000),], 2, function(s)rbinom(length(s), 40, boot::inv.logit(s)))
t_dat =test_data %>%mutate(post_pred=apply(post_pred, 2, mean), naive_pred = success_first_half)
ggplot(t_dat)+
  geom_line(aes(x=success_first_half, y=naive_pred, color="Naive prediction (no regression)"))+
  geom_line(aes(x=success_first_half, y = post_pred, color="Hierarchical prediction"))+
  geom_abline(aes(slope=coef(prev_season_mod)[2], intercept = coef(prev_season_mod)[1], color="Last season regression"))+
  guides(color=guide_legend(title=NULL))+
  theme_bw()+
  theme(legend.position = "bottom")+
  ylab("2nd Half Prediction")+
  xlab("1st Half Performance")

