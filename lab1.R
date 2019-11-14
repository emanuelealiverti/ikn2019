
dat=read.csv("energia_clean.csv")
day_cat = hour( as_datetime(dat$h) )
new_hour = numeric(length(day_cat))
new_hour[day_cat < 6] = "notte"
new_hour[day_cat >= 6 & 
           day_cat < 13] = "mattina"
new_hour[day_cat >= 13 & 
           day_cat < 19] = "pomeriggio"
new_hour[day_cat >= 19] = "sera"
dat$day_cat = factor(new_hour)
dat_clean = na.omit(dat)
dat_clean$FEB_L = log(dat_clean$FEB)
dat_clean$FEB_L[
  is.infinite(dat_clean$FEB_L)] = 0
plot(dat_clean$FEB_L)
names(dat_clean)
dat_clean$X = NULL
dat_clean$h = NULL
dat_clean$u = NULL

set.seed(1234)
stima = sample(1:NROW(dat_clean),
               0.75*NROW(dat_clean))
dat_stima = dat_clean[stima,]    
dat_verifica = dat_clean[-stima, ]

f0 = as.formula(
  "FEB~JAN+DEC+NOV+OCT+day_cat")

m0 = tree(formula = f0, 
          data=dat_stima)
plot(m0)
text(m0)


































