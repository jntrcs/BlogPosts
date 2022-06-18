library(tidyverse)
library(MASS)

set.seed(5)
data = MASS::mvrnorm(100, mu=c(0, 0), 
                     Sigma=matrix(c(1, .99, .99, 1), nrow=2, byrow=T)) %>%
  as_tibble %>% setNames(c("x1", "x2")) %>%
  mutate(x3 = rnorm(100, 0, 1),
    y=3*x1+4*x2 + x3+rnorm(100, sd=1))


ggplot(data, aes(x=x1, y=x2))+geom_point()
ggplot(data, aes(x=x1, y=y))+geom_point()
ggplot(data, aes(x=x2, y=y))+geom_point()

model = lm(y~x1+x2+x3, data= data)
summary(model)
confint(model)

library(car)

tests = expand.grid(x1=seq(0.5, 5, length.out=100), x2=seq(1.8, 6.5, length.out=100))
test_res = tests %>% rowwise %>%
  mutate(hypothesis = list(linearHypothesis(model, c(glue::glue("x1={x1}"), glue::glue("x2={x2}")))))

data = test_res %>% rowwise %>% mutate(p_val = hypothesis$`Pr(>F)`[2]) %>%
  ungroup() %>%
  mutate(sig=p_val<.05) %>%
  filter(!sig)

ggplot(data,aes(x=x1, y=x2))+
  geom_point(color="blue", alpha=.4, size=3)+
  geom_point(data=tibble(x1=3, x2=4))+
  geom_label(data=tibble(x1=3, x2=4), aes(label="True ROI"),
            vjust=0, hjust=-.1)+
  scale_x_continuous("Channel 1 ROI")+
  scale_y_continuous("Channel 2 ROI")+
  theme_bw()+
  ggtitle("Bivariate Confidence Region")+
  guides(color="none")

lm_int = tibble(var = c("Channel 1", "Channel 2", "Channel 3"),
                lower = c(1.11, 2.92, .87),
                point=c(2.58, 4.36, 1.09),
                upper=c(4.05, 5.81, 1.31))

ggplot(lm_int, aes(y=var, x=point))+
  geom_point()+
  geom_errorbar(aes(xmin=lower, xmax=upper), width= .5)+
  scale_y_discrete(NULL)+
  scale_x_continuous("ROI Estimate & Confidence Interval")+
  ggtitle("Confidence Interval from Linear Model")+
  theme_bw()


ggplot(data, aes(x=x1, y=x2))+
  geom_rect(xmin = lm_int$lower[1], xmax=lm_int$upper[1],
            ymin = lm_int$lower[2], ymax=lm_int$upper[2],
            fill="forestgreen")+
  geom_point(color="blue", alpha=.4, size=3)+
  geom_point(data=tibble(x1=3, x2=4))+
  geom_label(data=tibble(x1=3, x2=4), aes(label="True ROI"),
             vjust=0, hjust=-.1)+
  annotate("label", x=1.2,y=3.2, label="Perceived Confidence Region", hjust=0,
           color="forestgreen")+
  annotate("label", x=.9,y=6, label="True Confidence Region", hjust=0,
           color="blue")+
  scale_x_continuous("Channel 1 ROI")+
  scale_y_continuous("Channel 2 ROI")+
  theme_bw()+
  ggtitle("Bivariate Confidence Region")+
  guides(color="none")
