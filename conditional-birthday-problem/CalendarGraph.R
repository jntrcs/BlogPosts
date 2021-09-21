library(tidyverse)
library(lubridate)

birthdays=tibble(names=c("Jackson", "Monica", "Mark", "Maureen", "Ryan", "Kyle", "Jared",
         "Tyler", "Danielle", "Jordan", "Hailey", "Graham", "Laura", "Chris",
         "Michelle", "Aaron", "Spencer", "Ryan", "Adelaide", "Myra", "Lydia"),
       birthday = c("2019-02-11", "2019-10-20", "2019-08-14", "2019-08-15",
       "2019-06-01", "2019-01-06", "2019-05-03", "2019-04-29", "2019-01-20",
       "2019-09-23", "2019-01-14", "2019-05-12", "2019-07-23", "2019-08-22",
       "2019-01-29", "2019-12-18", "2019-08-04", "2019-12-17", "2019-05-14",
       "", "2019-11-23")) %>%
  mutate(birthday = ymd(birthday))


df  <-  tibble(
  DateCol = seq(
    dmy("01/01/2019"),
    dmy("31/12/2019"),
    "days"
  ),
  ValueCol=DateCol %in% birthdays$birthday
)


dfPlot <- df %>%
  mutate(weekday = wday(DateCol, label = T, week_start = 7), # can put week_start = 1 to start week on Monday
         month = month(DateCol, label = T),
         date = yday(DateCol),
         week = epiweek(DateCol))

# isoweek makes the last week of the year as week 1, so need to change that to week 53 for the plot
dfPlot$week[dfPlot$month=="Dec" & dfPlot$week ==1] = 53

dfPlot <- dfPlot %>%
  group_by(month) %>%
  mutate(monthweek = 1 + week - min(week),
         Day = as.character(day(DateCol)),
         Day=if_else(DateCol=="2019-01-13", "?", Day))

dfPlot %>%
  ggplot(aes(weekday,-week, fill = ValueCol)) +
  geom_tile(colour = "white")  +
  geom_text(aes(label = Day), size = 2.5, color = "black") +
  theme(aspect.ratio = 1/2,
        legend.position = "top",
        legend.key.width = unit(3, "cm"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.title.align = 0.5,
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 15),
        panel.border = element_rect(colour = "grey", fill=NA, size=1),
        plot.title = element_text(hjust = 0.5, size = 21, face = "bold",
                                  margin = margin(0,0,0.5,0, unit = "cm"))) +
  facet_wrap(~month, nrow = 4, ncol = 3, scales = "free") +
  labs(title = "Curtis/Yeates Birthday Calendar")+
  scale_fill_manual(values=tayloRswift::swift_palettes$lover[c(2,4)])+
  guides(fill=F)
