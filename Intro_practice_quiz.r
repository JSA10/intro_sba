## INTRO TO SBA
##
library(tidyverse)
pasta <- read_csv("_eba2c079135882131db3690701bc9c97_PASTAPURCHASE_EDITED.csv")
View(slice(pasta, 1:100))
glimpse(pasta)

#1 What is the correct mean and standard deviation of the quantity of pasta
#purchased by time unit by household?
summary(pasta)
mean(pasta$PASTA)
sd(pasta$PASTA)

#2 In which area are located (i) the poorest household and (ii) the wealthiest
#household?
wealth <- pasta %>%
    arrange(INCOME)
head(wealth)
tail(wealth)

#3 What is the maximum pasta quantity a household has bought over the whole
#time period?
#(Sum the quantity of pasta by household over time and indicate the maximum)
hh_ps <- pasta %>%
    group_by(HHID) %>%
    summarise(
        pasta_total = sum(PASTA)
    ) %>%
    arrange(pasta_total)

tail(hh_ps)

#4 What is the average income of households living in area 4?
area_income <- pasta %>%
    group_by(AREA) %>%
    summarise(
        avg_income = mean(INCOME)
    )

area_income

#5 How many households live in area 2, earn more than 20k, and have purchased
#more than 30 units of pasta over the whole time period?

q5 <- pasta %>%
    group_by(HHID) %>%
    filter(AREA == 2 & INCOME > 20000) %>%
    summarise(
        pasta_total = sum(PASTA)
    ) %>%
    filter(pasta_total > 30)

#6 What is the correlation between the purchases of pasta and the exposures?
cor(pasta$PASTA, pasta$EXPOS)


#7 Which of the following graphs reports the correct histogram by household of
#the total purchase of pasta made by the household over the whole period?
#(Sum the purchases by household and make a histogram.)

hist(hh_ps$pasta_total)

#8 Which of the following graphs reports the correct time series of the overall
#total purchase of pasta? (Sum the purchases by time units and plot the quantity
#by time unit.)

ts_ps <- pasta %>%
    group_by(TIME) %>%
    summarise(
        pasta_total = sum(PASTA)
    )

plot(ts_ps)

### PASSED 1ST TIME 8/8 RIGHT



