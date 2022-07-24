library(haven)
library(data.table)

setwd("~/inclusion_health_smoking_trends")

d <- read_sav("Data to share with Dan L.sav")
ndtms_age <- read.csv(url('https://raw.githubusercontent.com/danlewer/inclusion-health-smoking/main/ndtms_age_new_presentations_2020_2021.csv'))
setDT(d); setDT(ndtms_age)

d[, q := factor(quarter, 1:65, c(outer(paste0('Q', 1:4, '-'), 2006:2022, paste0))[-(1:3)])]
d <- droplevels(d)
agelims <- c(18, seq(20, 65, 5))
d[, age_group := findInterval(actage, agelims)]
d[, age_group := factor(age_group, seq_along(agelims), agelims)]
d[, age_group := as.integer(as.character(age_group))]
d[, sex := factor(sexz, 1:2, c('male', 'female'))]
d[, wt := as.numeric(d$`@weight0`)]
d[, `@weight0` := NULL]
d[, c('xwave', 'sexz', 'agez', 'actage', 'quarter') := NULL]
d <- d[!is.na(age_group) & !is.na(cigsmok) & !is.na(sex)]

# create NDTMS age/sex distribution assuming age distribution is the same for men and women

ndtms_age[, age_group := as.integer(substr(age_group, 0, 2))]
ndtms_age[, c('non_opiate', 'non_opiate_and_alcohol') := NULL]
# assume opiate is 72% male; alcohol is 58% male
ndtms_age_sex <- rbind(ndtms_age[, .(age_group = age_group, sex = 'male', opiate = as.integer(opiate * 0.72), alcohol = as.integer(alcohol * 0.58))],
                       ndtms_age[, .(age_group = age_group, sex = 'female', opiate = as.integer(opiate * 0.28), alcohol = as.integer(alcohol * 0.42))])

# estimate standardised prevalence

rs <- d[, .(p = mean(cigsmok)), c('q', 'age_group', 'sex')]
rs <- ndtms_age_sex[rs, on = c('age_group', 'sex')]
rs$opiateE <- rs$p * rs$opiate
rs$alcoholE <- rs$p * rs$alcohol
rs <- rs[, .(alcohol = sum(alcoholE) / sum(alcohol), opiate = sum(opiateE) / sum(opiate)), q]
rs <- rs[order(q)]
rs[, alcoholD := alcohol - shift(alcohol)]
rs[, opiateD := opiate - shift(opiate)]

# estimate precision of standardised prevalence of smoking using non-parametric bootstrap

set.seed(27)
B <- lapply(seq_len(10000), function(x) {
  if (x %% 100 == 0) print(x)
  r <- d[sample(.N, .N, replace = T)]
  rs <- r[, .(p = mean(cigsmok)), c('q', 'age_group', 'sex')]
  rs <- ndtms_age_sex[rs, on = c('age_group', 'sex')]
  rs$opiateE <- rs$p * rs$opiate
  rs$alcoholE <- rs$p * rs$alcohol
  rs <- rs[, .(alcohol = sum(alcoholE) / sum(alcohol), opiate = sum(opiateE) / sum(opiate)), q]
  rs <- rs[order(q)]
  rs$alcoholD <- rs$alcohol - shift(rs$alcohol)
  rs$opiateD <- rs$opiate - shift(rs$opiate)
  return(rs)
})

# # try bootstrapping within quarters - same results
# 
# set.seed(2071)
# B2 <- lapply(seq_len(10000), function(x) {
#   if (x %% 100 == 0) print(x)
#   r <- split(d, f = d$q)
#   r <- lapply(r, function (z) z[sample(.N, .N, replace = T)])
#   r <- rbindlist(r)
#   rs <- r[, .(p = mean(cigsmok)), c('q', 'age_group', 'sex')]
#   rs <- ndtms_age_sex[rs, on = c('age_group', 'sex')]
#   rs$opiateE <- rs$p * rs$opiate
#   rs$alcoholE <- rs$p * rs$alcohol
#   rs <- rs[, .(alcohol = sum(alcoholE) / sum(alcohol), opiate = sum(opiateE) / sum(opiate)), q]
#   rs <- rs[order(q)]
#   rs$alcoholD <- rs$alcohol - shift(rs$alcohol)
#   rs$opiateD <- rs$opiate - shift(rs$opiate)
#   return(rs)
# })

Bq <- lapply(2:5, function (z) {
  t(apply(do.call(cbind, sapply(B, function (x) x[, ..z])), 1, quantile, probs = c(0.025, 0.5, 0.975), na.rm = T))
})
cn <- lapply(c('alcohol', 'opiate', 'alcoholD', 'opiateD'), function (x) paste0(x, c('_0.025', '_0.5', '_0.975')))
Bq <- mapply(`colnames<-`, x = Bq, value = cn, SIMPLIFY = F)
rs <- cbind(rs, do.call(cbind, Bq))

# Bq2 <- lapply(2:5, function (z) {
#   t(apply(do.call(cbind, sapply(B2, function (x) x[, ..z])), 1, quantile, probs = c(0.025, 0.5, 0.975), na.rm = T))
# })
# cn2 <- lapply(c('alcohol2', 'opiate2', 'alcohol2D', 'opiate2D'), function (x) paste0(x, c('_0.025', '_0.5', '_0.975')))
# Bq2 <- mapply(`colnames<-`, x = Bq2, value = cn2, SIMPLIFY = F)
# rs <- cbind(rs, do.call(cbind, Bq2))

fwrite(rs, 'sts_summary_bootstrap_24july2022.csv')
