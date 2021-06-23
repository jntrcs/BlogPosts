## Why Leon Wood

data_wide %>% filter(`Pat Fraher`==1) %>%
  summarize(across(-(1:4), ~sum(minutes[.x==1]))) %>%
  pivot_longer(everything(), values_to="minutes_with_pf") %>%
  filter(minutes_with_pf!=0, name!="Pat Fraher") %>%
  left_join(stan_rank) %>%
  summarize(48*sum(bad_calls_per_minute*minutes_with_pf)/ sum(minutes_with_pf),
            mean(rep(poisson_rank, minutes)))



data_wide %>% filter(`Nate Green`==1) %>%
  summarize(across(-(1:4), ~sum(minutes[.x==1]))) %>%
  pivot_longer(everything(), values_to="minutes_with_el") %>%
  filter(minutes_with_el!=0, name!="Nate Green") %>%
  left_join(stan_rank) %>%
  summarize(48*sum(bad_calls_per_minute*minutes_with_el)/ sum(minutes_with_el),
            mean(rep(poisson_rank, minutes)))




data_wide %>% filter(`Leon Wood`==1) %>%
  summarize(across(-(1:4), ~sum(minutes[.x==1]))) %>%
  pivot_longer(everything(), values_to="minutes_with_el") %>%
  filter(minutes_with_el!=0, name!="Leon Wood") %>%
  left_join(stan_rank) %>%
  summarize(48*sum(bad_calls_per_minute*minutes_with_el)/ sum(minutes_with_el),
            mean(rep(poisson_rank, minutes)))

