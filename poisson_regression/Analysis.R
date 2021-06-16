library(tidyverse)


all_reports = read_csv("GameReports.csv")

calls = all_reports %>%
  filter(!is.na(CallRatingName)) %>%
  group_by(game_id) %>%
  summarize(minutes = length(unique(PeriodName))*2,
            ICs = sum(CallRatingName=="IC")+sum(CallRatingName=="INC"),
            CCs = sum(CallRatingName=="CC")+sum(CallRatingName=="CNC"))

refs = read_csv("AllRefsAllGames.csv")

refs_wide = refs %>%
  filter(game_id!="0022000215") %>% #Remove this row as some weird duplicate
  pivot_wider(id_cols=game_id, names_from=Position,
                     values_from=Ref)

data= calls %>% left_join(refs_wide)

data_wide = data %>% pivot_longer(official1:official3) %>%
  mutate(name=1) %>%
  pivot_wider(id_cols=game_id:CCs, names_from=value, values_from=name, values_fill=0)



stan_data = list(refs = as.matrix(data_wide %>% select(-(1:4))),
                 bad_calls = data_wide %>% pull(ICs),
                 minutes = data_wide %>% pull(minutes),
                 N_games=nrow(data_wide),
                 N_refs = ncol(data_wide)-4)

library(rstan)
fit1 <- stan(
  file = "PoissonModel.stan",  # Stan program
  data = stan_data,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 4000,          # number of warmup iterations per chain
  iter = 8000,            # total number of iterations per chain
  cores = 4,              # number of cores (could use one per chain)
)

shinystan::launch_shinystan(fit1)

rs=rstan::extract(fit1, "r")$r
a=apply(rs, 1, rank)

stan_rank = tibble(name = names(data_wide)[-c(1:4)], bad_calls_per_minute = apply(rs, 2, mean),
                   bad_calls_median = apply(rs, 2, median),
                   lb = apply(a, 1, quantile, .025),
                   ub = apply(a, 1, quantile, .975),
                   poisson_rank = rank(bad_calls_per_minute)) %>%
  left_join(
    data_wide %>%
     summarize(across(-c(1:4), ~sum(minutes[.x==1]))) %>%
     pivot_longer(everything(),values_to="minutes")
)


stan_rank %>% arrange(bad_calls_per_minute) %>%
  ggplot(aes(y=fct_rev(fct_inorder(name))))+
  geom_point(aes(x=bad_calls_per_minute*48))+
  theme_minimal()+
  xlab("Expected number of bad calls in a 48 minute game by each official")+
  ylab(NULL)+
  scale_x_continuous(limits = c(0, 21))+
  ggtitle("Poisson Modeled Bad Call Rate", "For normal games with three officials")

stan_rank %>% arrange(bad_calls_per_minute) %>%
  ggplot(aes(y=fct_inorder(name)))+
  #geom_point(aes(x=bad_calls_per_minute*48))+
  geom_errorbar(aes(xmin=lb, xmax=ub))+
  theme_minimal()+
  xlab("Possible Ref Rank")+
  ylab(NULL)

##Helpful explanations


no_model=data_wide %>%
  summarize(across(-c(game_id:CCs), ~mean(ICs[.x==1]))) %>%
  pivot_longer(everything()) %>%arrange(desc(value)) %>%
  mutate(no_mod_rank = nrow(.):1)

comp=stan_rank %>% left_join(no_model)
comp_plot=ggplot(comp, aes(x=no_mod_rank, y=poisson_rank, text=paste0("Name: ", name, "\nMinutes Reffed: ", minutes),
                           label=name, size=minutes))+
  geom_point()+
  geom_point(color="forestgreen", data=~.x%>%filter(name%in% c("JT Orr", "Eric Lewis", "Bill Spooner", "Pat Fraher")))+
  #ggrepel::geom_label_repel(data=~.x %>% filter(abs(no_mod_rank-poisson_rank)>20))+
  ylab("Bayesian Poisson Rank")+
  xlab("No Model Rank")+
  theme_minimal()+
  theme(legend.position = "bottom")+
  scale_y_continuous(breaks=seq(0, 80, 10), minor_breaks = seq(0, 80, 5))+
  scale_x_continuous(breaks=seq(0,80, 10), minor_breaks = seq(0, 80, 5))+
  guides(size=guide_legend(title="Time in L2M Reports"), color=NULL)+
  ggtitle("Naive vs. Modeled Ref Ranks")
comp_plot

p=plotly::ggplotly(comp_plot, tooltip = "text")
p
plotly::api_create(p, filename="Ranking_Comparisons")

ggplot(comp, aes(x=bad_calls_per_minute, y=value, size=minutes))+geom_point()+
  ylab("Average number of bad calls when ref is reffing")+
  xlab("Inferred bad call rate")+
  theme(legend.position = "bottom")+
  guides(size=guide_legend(title="Time in L2M Reports"))

mod_lm = lm(ICs~-1+.-minutes,
          weights=data_wide$minutes,
          data=data_wide %>%
            select(-c(game_id, CCs)) %>%
            mutate(ICs=ICs/minutes))
summary(mod_lm)

lm_coefs = broom::tidy(mod_lm)  %>%
  #mutate(estimate= replace_na(estimate,0)) %>%
  select(name=term, estimate) %>%
  mutate(name=substr(name, 2, nchar(name)-1))%>%
  arrange(estimate) %>%
  mutate(lm_rank=1:nrow(.)) %>%
  rename(lm_estimate=estimate)


comp = comp %>% left_join(lm_coefs)
ggplot(comp, aes(y=no_mod_rank, x=lm_rank))+
  geom_point()
