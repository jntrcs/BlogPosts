## Why Eric Lewis

data_wide %>% filter(`Eric Lewis`==1) %>%
  summarize(across(-(1:4), ~sum(minutes[.x==1]))) %>%
  pivot_longer(everything(), values_to="minutes_with_el") %>%
  filter(minutes_with_el!=0, name!="Eric Lewis") %>%
  left_join(stan_rank) %>%
  summarize(48*sum(bad_calls_per_minute*minutes_with_el)/ sum(minutes_with_el),
            mean(rep(poisson_rank, minutes)))


data_wide %>% filter(`Pat Fraher`==1) %>%
  summarize(across(-(1:4), ~sum(minutes[.x==1]))) %>%
  pivot_longer(everything(), values_to="minutes_with_pf") %>%
  filter(minutes_with_pf!=0, name!="Pat Fraher") %>%
  left_join(stan_rank) %>%
  summarize(48*sum(bad_calls_per_minute*minutes_with_pf)/ sum(minutes_with_pf),
            mean(rep(poisson_rank, minutes)))


data_wide %>% filter(`CJ Washington`==1) %>%
  summarize(across(-(1:4), ~sum(minutes[.x==1]))) %>%
  pivot_longer(everything(), values_to="minutes_with_cw") %>%
  filter(minutes_with_cw!=0, name!="CJ Washington") %>%
  left_join(stan_rank) %>%
  summarize(48*sum(bad_calls_per_minute*minutes_with_cw)/ sum(minutes_with_cw),
            mean(rep(poisson_rank, minutes)))
