rm(list=ls())
#+++++++++++++++++++++++
# Consumo di elettricità
#+++++++++++++++++++++++
dat = read.csv(file='./energia_clean.csv')
str(dat)
head(dat)
dat$X = NULL

#+++++++++++++++++++++++++++++++++++++++++++
# Costruzione di variabile che ci dica l'ora
#+++++++++++++++++++++++++++++++++++++++++++
require(lubridate)
as_datetime(dat$h)
day_cat = h = hour(as_datetime(dat$h))
day_cat[h < 6] = "notte"
day_cat[h >= 6 & h < 13] = "mattino"
day_cat[h >= 13 & h < 19] = "pomeriggio"
day_cat[h >= 19] = "sera"

dat$day_cat = factor(day_cat)
dat$day_cat
summary(dat)
dat =  na.omit(dat)
FEB_C = ifelse(dat$FEB > quantile(dat$FEB, .5),"high","low")
dat$FEB_C = factor(FEB_C)
dat$u = NULL
dat$h = NULL



set.seed(1234)
stima = sample(1:NROW(dat), .75*NROW(dat))
verifica = setdiff(1:NROW(dat), stima)
#verifica = sample(setdiff(1:NROW(dat), stima),.25*NROW(dat))
#validazione = setdiff(1:NROW(dat), c(stima,verifica))
head(dat)
dat_stima = dat[stima,]
dat_verifica = dat[verifica,]
#dat_validazione = dat[validazione,]
str(dat_verifica)
#NROW(dat_stima) + NROW(dat_verifica) + NROW(dat_validazione)
#NROW(dat)
f0 = as.formula(paste0("FEB_C~",paste0(names(dat_stima)[2:6], collapse = "+")))
f0
## SVM
require(nnet)
m3 = nnet(formula = f0, data =dat_stima, size = 2,rang = .1, decay = 0.005)
str(m3)
table(dat_verifica$FEB_C, predict(m3,dat_verifica) > .5)

ce = function(true,predicted,tresh=.5) {
	tmp = table(predicted>tresh, true)
	1 - ( sum(diag(tmp)) / sum(tmp) )
}

ce(dat_verifica$FEB_C, predicted = predict(m3,dat_verifica))

rang = seq(0.1,0.7,length.out = 2)
decay = seq(0.001, 0.1, length.out = 2)

par_grid = expand.grid(rang,decay)
res_matr = matrix(0, NROW(par_grid), 3)
id_split = matrix(sample(1:NROW(dat_stima)),ncol=3)
head(id_split)
par_grid
for(it in 1:NROW(par_grid)){
	for(k in 1:3) {
		id_cv = id_split[,k]
		m_tmp = nnet(formula = f0, data =dat_stima[-id_cv,], 
			     size = 2, rang = par_grid[it,1], decay = par_grid[it,2])
		res_matr[it,k] = ce(dat_verifica$FEB_C[id_cv], predict(m_tmp, dat_verifica[id_cv,]) > 0.5)
		cat("Configurazione ", it, "fold ", k , "\n")
	}
}


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
