library(dplyr)
library(mrgsolve)
library(lastdose)
mod <- modlib("pk1", end = 12*168, delta = 4)
dose <- ev(amt=1000,ii= 24, addl = 12*7-1)
d1 <- ev(amt = 1000, ii = 24, addl = 4*7-1)
d2 <- ev(amt = 500, ii = 24, addl = 4*7-1)
d3 <- ev(amt = 300, ii = 24, addl = 4*7-1)
dose <- seq(d1,d2,d3) %>% ev_rep(1:1000)
out <- mrgsim(mod,dose,carry_out="amt,ii,evid,addl,cmt",Req="DV=CP",digits=3)

df <- as_tibble(out)

names(df) <- toupper(names(df))


df1 <- filter(df, ID==1)
df2 <- filter(df1, TIME <= 28)
df2 <- mutate(df2, TIME = ifelse(AMT > 0, 12, TIME)) %>% arrange(ID,TIME,EVID)
df2 <- mutate(df2, CP = ifelse(TIME <= 12, 0, CP))

write.csv(
  df1,
  file = "inst/csv/data1.csv",
  quote=FALSE,
  row.names=FALSE
)
write.csv(
  df2,
  file = "inst/csv/data2.csv",
  quote=FALSE,
  row.names=FALSE
)


saveRDS(object=df, file = "inst/csv/data_big.RDS",version=2)


set1 <- ev(amt = 100, ii = 12, addl = 1) %>% ev_rep(1:2)
set1 <- expand_observations(set1, c(seq(0,24,4))) %>% mutate(set = 1)
set2 <- ev(amt = 100, ii = 12, addl = 1, time = 6) %>% ev_rep(1:2)
set2 <- expand_observations(set2, c(seq(0,44))) %>% mutate(set = 2)
set3 <- mrgsolve::realize_addl(set1) %>% mutate(set = 3)
set4 <- mrgsolve::realize_addl(set2) %>% mutate(set = 4)

set <- bind_rows(set1,set2,set3,set4)

write.csv(
  set,
  file = "inst/csv/setn.csv",
  quote=FALSE,
  row.names=FALSE
)


