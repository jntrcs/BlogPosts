library(tidyverse)

##See how the confidence intervals change if we had 24 times more data

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

minutes=data_wide %>% pull(minutes)

stan_data = list(refs = as.matrix(data_wide %>% select(-(1:4))),
                 bad_calls = data_wide %>% pull(ICs) * case_when(minutes==2~24,
                                                                 minutes==4~12,
                                                                 minutes==6~6),
                 minutes = rep(48, length(minutes)),
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
stan_rank = tibble(name = names(data_wide)[-c(1:4)], bad_calls_per_minute = apply(rs, 2, mean),
                   bad_calls_median = apply(rs, 2, median),
                   lb = apply(rs, 2, quantile, .025),
                   ub = apply(rs, 2, quantile, .975),
                   poisson_rank = rank(bad_calls_per_minute))


a=apply(rs, 1, rank)
hist(a[57,])

stan_rank %>% arrange(bad_calls_per_minute) %>%
  ggplot(aes(y=fct_inorder(name)))+
  geom_point(aes(x=bad_calls_per_minute*48))+
  geom_errorbar(aes(xmin=lb*48, xmax=ub*48))+
  theme_minimal()+
  xlab("Bad Calls per 48 Minute Game")+
  ylab(NULL)

##Helpful explanations


no_model=data_wide %>%
  summarize(across(-c(game_id:CCs), ~mean(ICs[.x==1]))) %>%
  pivot_longer(everything()) %>%arrange(desc(value)) %>%
  mutate(no_mod_rank = nrow(.):1)

comp=stan_rank %>% left_join(no_model)
ggplot(comp, aes(x=no_mod_rank, y=poisson_rank))+geom_point()

ggplot(comp, aes(x=bad_calls_per_minute, y=value))+geom_point()

mod_lm = lm(ICs~.-minutes,
            weights=data_wide$minutes,
            data=data_wide %>%
              select(-c(game_id, CCs)) %>%
              mutate(ICs=ICs/minutes))
summary(mod_lm)

lm_coefs = broom::tidy(mod_lm) %>% slice(-1) %>%
  mutate(estimate= replace_na(estimate,0)) %>%
  select(name=term, estimate) %>%
  mutate(name=substr(name, 2, nchar(name)-1))%>%
  arrange(estimate) %>%
  mutate(lm_rank=1:nrow(.))


comp = comp %>% left_join(lm_coefs)
ggplot(comp, aes(y=rank, x=lm_rank))+
  geom_point()
