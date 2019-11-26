
# ----data
rm(list=ls())
dat=read.csv(unzip("./dati.zip","energia_clean.csv"))

# ----pack
require(lubridate)
day_cat = hour( as_datetime(dat$h) )
new_hour = numeric(length(day_cat))
new_hour[day_cat < 6] = "notte"
new_hour[day_cat >= 6 & 
           day_cat < 13] = "mattina"
new_hour[day_cat >= 13 & 
           day_cat < 19] = "pomeriggio"
new_hour[day_cat >= 19] = "sera"

# ----preproc
dat$day_cat = factor(new_hour)
dat =  na.omit(dat)
dat$FEB_L = log(dat$FEB)
dat$FEB_L[is.infinite(dat$FEB_L)] = 0 
dat$u = NULL
dat$h = NULL
dat$X = NULL

# ----stima
set.seed(1234)
stima = sample(1:NROW(dat), .75*NROW(dat))
verifica = setdiff(1:NROW(dat), stima)
dat_stima = dat[stima,]
dat_verifica = dat[verifica,]
NROW(dat_stima) + NROW(dat_verifica) 
NROW(dat)

# ----albero
require(tree)
f0 = as.formula(paste0("FEB~",paste0(names(dat_stima)[2:6], collapse = "+")))

# Comando di base
m0 = tree(f0,
	  data = dat_stima, 
	  control = tree.control(nobs=length(stima), minsize = 2,mindev = 0.001))

# Convalida incrociata
cv_tree = cv.tree(m0)
plot(cv_tree)
m_final = prune.tree(m0,best = 7)


plot(m_final)
text(m_final,pretty=0)

### valutare previsione
p0 = predict(m_final,dat_verifica)
(mse_tree = mean((dat_verifica$FEB - p0)^2))

# ----alberoL
#++++++++++++++++++
# Scala logaritmica
#++++++++++++++++++
f1 = as.formula(paste0("FEB_L~",paste0(names(dat_stima)[2:6], collapse = "+")))
m0L = tree(f1, 
	  data = dat_stima, 
	  control = tree.control(nobs=length(stima), minsize = 2,mindev = 0.001))
plot(m0L)
text(m0L)

cv_treeL = cv.tree(m0L)
plot(cv_treeL)
m_finalL = prune.tree(m0L,best = 7)

plot(m_finalL)
text(m_finalL,pretty=0)


### valutare previsione
p0L = predict(m_finalL,dat_verifica)
(mse_treeL = mean((dat_verifica$FEB_L - p0L)^2))


#++++++++
# BAGGING
#++++++++
require(ipred)
?bagging
m1 = bagging(formula = f0,data = dat_stima)
p1 = predict(m1,newdata= dat_verifica)


m1L = bagging(formula = f1,data = dat_stima)
p1L = predict(m1L,newdata= dat_verifica)

(mse_bagL = mean((dat_verifica$FEB_L - p1L)^2))
(mse_bag = mean((dat_verifica$FEB - p1)^2))


#++++++++
# Random Forest
#++++++++
require(ranger)
?ranger
m2 = ranger(formula = f0,data = dat_stima,num.trees = 100,importance = "impurity")
print(m2)
plot(m2$variable.importance)
plot(m2$variable.importance,type="h")
plot(m2$variable.importance,type="h", xlab="variabe",ylab="importance")

plot(m2$variable.importance,type="h", xlab="variabe",ylab="importance",xaxt="n")
axis(1, at=1:5, labels=names(m2$variable.importance))
points(m2$variable.importance, col = "red")

p2 = predict(m2, data= dat_verifica)

m2L = ranger(formula = f1,data = dat_stima,num.trees = 100,importance = "impurity")

p2L = predict(m2L, data= dat_verifica)


(mse_rfL = mean((dat_verifica$FEB_L - p2L$predictions)^2))
(mse_rf = mean((dat_verifica$FEB - p2$predictions)^2))

ls()
grep("mse",ls(),value = T)
sapply(grep("mse",ls(),value = T), get)
res_mat = matrix(sapply(grep("mse",ls(),value = T), get), ncol = 2, byrow = T)
rownames(res_mat) = c("Bagging", "RF", "tree")
colnames(res_mat) = c("originale", "log") 
res_mat


m_lin = lm(f0,dat=dat_stima)
p_lin = predict(m_lin, dat_verifica)

m_linL = lm(f1,dat=dat_stima)
p_linL = predict(m_linL, dat_verifica)


(mse_L = mean((dat_verifica$FEB_L - p_linL)^2))
(mse = mean((dat_verifica$FEB - p_lin)^2))

#+++++++++++++++++++++++
# Miglioramento relativo
#+++++++++++++++++++++++
res_mat[,1]
c(res_mat[,1],mse) / max(c(res_mat[,1],mse))
1-c(res_mat[,1],mse) / max(c(res_mat[,1],mse))

c(res_mat[,2],mse) / max(c(res_mat[,2],mse_L))
1-c(res_mat[,2],mse) / max(c(res_mat[,2],mse))



