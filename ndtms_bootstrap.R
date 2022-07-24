library(data.table)

ndtms <- read.csv(url('https://raw.githubusercontent.com/danlewer/inclusion-health-smoking/main/ndtms_tobacco_trends.csv'))
setDT(ndtms)

ndtms <- ndtms[substance %in% c('Alcohol', 'Opiate')]
ndtms[, q := paste0('Q', quarter, '-', year)]
ndtms[, c('year', 'quarter', 'monthname', 'period_end') := NULL]
ndtms[, no := N - tobacco]
setnames(ndtms, 'tobacco', 'yes')
ndtms <- melt(ndtms, id.vars = c('q', 'substance'), measure.vars = c('yes', 'no'), variable.name = 'smok', value.name = 'N')
ndtms <- ndtms[, .(q = rep(q, N), substance = rep(substance, N), smok = rep(smok, N))]
ndtms[, smok := as.integer(smok == 'yes')]
qs <- c(outer(paste0('Q', 1:4, '-'), 2006:2022, paste0))
ndtms[, q := factor(q, qs)]
ndtms <- droplevels(ndtms)

rs <- ndtms[, .(p = mean(smok)), c('q', 'substance')]
rs <- dcast(rs, q ~ substance, value.var = 'p')
rs <- rs[order(q)]
rs$alcoholD <- rs$Alcohol - shift(rs$Alcohol)
rs$OpiateD <- rs$Opiate - shift(rs$Opiate)

set.seed(27)
B <- lapply(seq_len(10000), function(x) {
  if (x %% 100 == 0) print(x)
  r <- ndtms[sample(.N, .N, replace = T)]
  rs <- r[, .(p = mean(smok)), c('q', 'substance')]
  rs <- dcast(rs, q ~ substance, value.var = 'p')
  rs <- rs[order(q)]
  rs$alcoholD <- rs$Alcohol - shift(rs$Alcohol)
  rs$OpiateD <- rs$Opiate - shift(rs$Opiate)
  return(rs)
})

Bq <- lapply(2:5, function (z) {
  t(apply(do.call(cbind, sapply(B, function (x) x[, ..z])), 1, quantile, probs = c(0.025, 0.5, 0.975), na.rm = T))
})
cn <- lapply(c('Alcohol', 'Opiate', 'alcoholD', 'opiateD'), function (x) paste0(x, c('_0.025', '_0.5', '_0.975')))
Bq <- mapply(`colnames<-`, x = Bq, value = cn, SIMPLIFY = F)
rs <- cbind(rs, do.call(cbind, Bq))

fwrite(rs, 'ndtms_bootstrap_24july2022.csv')
