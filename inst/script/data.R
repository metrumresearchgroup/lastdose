library(dplyr)
library(mrgsolve)
mod <- modlib("pk1", end = 12*168, delta = 4)
dose <- ev(amt=1000,ii= 24, addl = 12*7-1)
d1 <- ev(amt = 1000, ii = 24, addl = 4*7-1)
d2 <- ev(amt = 500, ii = 24, addl = 4*7-1)
d3 <- ev(amt = 300, ii = 24, addl = 4*7-1)
dose <- seq(d1,d2,d3) %>% ev_rep(1:1000)
out <- mrgsim(mod,dose,carry_out="amt,ii,evid,addl,cmt",Req="DV=CP",digits=3)

df <- as_tibble(out)

names(df) <- toupper(names(df))

write.csv(
  filter(df, ID==1),
  file = "inst/csv/data1.csv",
  quote=FALSE,
  row.names=FALSE
)

saveRDS(object=df, file = "inst/csv/data_big.RDS",version=2)
