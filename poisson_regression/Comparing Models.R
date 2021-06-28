##Simulate from prior


calculate_range_exp = function(){
  theta <- rexp(1, 10)
  rs = rexp(79, 1/theta)
  hist(rs*48)
}


#Compare the two different priors
rates= stan_rank %>% select(name, exp_rate = bad_calls_per_minute) %>%
  left_join(stan_rank_new %>% select(name, norm_rate = bad_calls_per_minute))

predicted = data %>% select(-official4) %>% pivot_longer(official1:official3, values_to="name", names_to="o") %>% left_join(rates) %>%
  group_by(game_id) %>%
  summarize(n=n(),observed_rate =ICs[1]/minutes[1],
            exp_expected_rate=sum(exp_rate),
            norm_expected_rate = sum(norm_rate))

predicted  %>% summarize(exp_mse = mean((observed_rate-exp_expected_rate)^2),
                         norm_mse = mean((observed_rate-norm_expected_rate)^2))

predicted %>% ggplot(aes(x=observed_rate,y=exp_expected_rate))+geom_point()+
  geom_smooth()
predicted %>% ggplot(aes(x=observed_rate,y=norm_expected_rate))+geom_point()+
  geom_smooth()


## Which model produces a higher likelihood of observing the data we did
data %>% select(-official4) %>% pivot_longer(official1:official3, values_to="name", names_to="o") %>% left_join(rates) %>%
  group_by(game_id) %>%
  summarize(n=n(),
            ICs=ICs[1],
            minutes=minutes[1],
            exp_expected_rate=sum(exp_rate)*minutes,
            norm_expected_rate = sum(norm_rate)*minutes) %>%
  mutate(log_like_exp = dpois(ICs, exp_expected_rate, log=F),
         log_like_norm = dpois(ICs, norm_expected_rate, log=F)) %>%
  summarize(across(log_like_exp:log_like_norm, sum))
