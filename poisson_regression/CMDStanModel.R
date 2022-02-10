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


library(cmdstanr)
stan_data = list(refs = as.matrix(data_wide %>% select(-(1:4))),
                 bad_calls = data_wide %>% pull(ICs),
                 minutes = data_wide %>% pull(minutes),
                 N_games=nrow(data_wide),
                 N_refs = ncol(data_wide)-4)

file <- file.path("PoissonModel.stan")
mod <- cmdstan_model(file, force_recompile=T)
mod$print()



fit <- mod$sample(
  data = stan_data,
  seed = 123,
  chains = 8,
  parallel_chains = 8,
  refresh = 500,
  iter_warmup=2000,
  iter_sampling=10000
)

fit$summary()
