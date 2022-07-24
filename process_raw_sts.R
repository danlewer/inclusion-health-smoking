library(haven)
library(data.table)

setwd("~/inclusion_health_smoking_trends")

d <- read_sav("Data to share with Dan L.sav")
setDT(d)

d[, q := factor(quarter, 1:65, c(outer(paste0('Q', 1:4, '-'), 2006:2022, paste0))[-(1:3)])]
agelims <- c(18, seq(20, 65, 5))
d[, age_group := findInterval(actage, agelims)]
d[, age_group := factor(age_group, seq_along(agelims), agelims)]
d[, sex := factor(sexz, 1:2, c('male', 'female'))]

sts_summary <- d[!is.na(age_group) & !is.na(cigsmok) & !is.na(sex), .N, c('q', 'age_group', 'sex', 'cigsmok')]
sts_summary <- sts_summary[order(q, age_group, sex, cigsmok)]

fwrite(sts_summary, 'sts_summary_24july2022.csv')