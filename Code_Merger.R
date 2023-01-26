library(haven)
library(dplyr)

airline <- read_dta("C:/Users/flore/OneDrive/Bureau/2023/Drive/EOC/donnees_airline.dta")
airline

airlines_reshape <- read_dta("C:/Users/flore/OneDrive/Bureau/2023/Drive/EOC/airlines_reshape.dta")

iv = list("alpha"= 25.47462, 
           "beta"= 0.08364,
           "gamma"= -1.84076,
           "mu"= 1.33431) 

# Compute cost and margin for each firms: 
airline$c1 = airline$p1 - 1/(iv$beta*(1 - airline$s1))
airline$c2 = airline$p2 - 1/(iv$beta*(1 - airline$s2))

airline$margin1 = airline$p1 - airline$c1
airline$margin2 = airline$p2 - airline$c2

airline$s0 = 1 - airline$s1 - airline$s2
airline$lns1 = log(airline$s1/airline$s0)
airline$dist_1000km =  airline$dist/1000

param = c(1,1)

airline$chsi_1 =  log(airline$s1/airline$s0) - iv$alpha + iv$beta*airline$p1
  - iv$gamma*airline$dist_1000km - iv$mu*airline$escale1


airline$chsi_2 = log(airline$s2/airline$s0) - iv$alpha + iv$beta*airline$p2 
  - iv$gamma*airline$dist_1000km - iv$mu*airline$escale2



# Ici on est sur que ça soit des fonctions :
delta_1 = function(param, k){iv$alpha - iv$beta*param[1] + iv$gamma*airline$escale1[k] + iv$mu*airline$dist_1000km[k]+ airline$chsi_1[k]}
delta_2 = function(param, k){iv$alpha - iv$beta*param[2] + iv$gamma*airline$escale2[k] + iv$mu*airline$dist_1000km[k]+ airline$chsi_2[k]}

# On continue avec les fonctions suivantes: 
s1 = function(param, k) {
  exp(delta_1(param, k)) / (1 + exp(delta_1(param, k)) + exp(delta_2(param, k)))
}

s2 = function(param, k) {
  (exp(delta_2(param, k))) / (1 + exp(delta_1(param, k)) + exp(delta_2(param, k)))
}

# on vérifie si on a des chiffres cohérents: 
mean(airline$p1)
mean(airline$p2)
param = c(201.0613,200.5109)
s1(param,1)
s2(param,1)
airline$s1[1]
airline$s2[1]

# On définit ensuite notre fonction d'optimisation : 

# ini: 
result= list()
j = 0
param = c(mean(airline$p1),mean(airline$p2))

Merge <- function(param,k) { - (
  (param[1] - airline$c1[k])*s1(param)[k] + (param[2] - airline$c2[k])*s2(param)[k]  
)}
FOC(param,10) # check
optim(par = param,Merge, k = 2) # check

for (i in 1:500) {
  result[i] = optim(par = param, Merge, k = i)
}
result <- t(as.data.frame(do.call(cbind, result)))
#View(result)

airline$p1_merged = result[,1]
airline$p2_merged = result[,2]


## Market share : 

delta_1 = list()
delta_2 = list()
for (i in 1:500) {
  delta_1[i] = iv$alpha - iv$beta*airline$p1_merged[i] + iv$gamma*airline$escale1[i] + iv$mu*airline$dist_1000km[i]+ airline$chsi_1[i]
  delta_2[i] = iv$alpha - iv$beta*airline$p2_merged[i] + iv$gamma*airline$escale2[i] + iv$mu*airline$dist_1000km[i]+ airline$chsi_2[i]
}

# ini: 
delta_1 = unlist(delta_1)
delta_2 = unlist(delta_2)
airline$s1_merged[] = 0
airline$s2_merged[] = 0

for (i in 1:500) {
  airline$s1_merged[i] = exp(delta_1[i]) / (1 + exp(delta_1[i]) + exp(delta_2[i]))
}

for (i in 1:500) {
  airline$s2_merged[i] = exp(delta_2[i]) / (1 + exp(delta_1[i]) + exp(delta_2[i]))
}

## avec synergie de coûts : 

## 90% 
result_90 = list()
Merge_90 <- function(param,k) { - (
  (param[1] - 0.9*airline$c1[k])*s1(param)[k] + (param[2] - 0.9*airline$c2[k])*s2(param)[k]  
)}
for (i in 1:500) {
  result_90[i] = optim(par = param, Merge_90, k = i)
}

## 85%
result_85 = list()
Merge_85 <- function(param,k) { - (
  (param[1] - 0.85*airline$c1[k])*s1(param)[k] + (param[2] - 0.85*airline$c2[k])*s2(param)[k]  
)}
for (i in 1:500) {
  result_85[i] = optim(par = param, Merge_85, k = i)
}

## 80%
result_80 = list()
Merge_80 <- function(param,k) { - (
  (param[1] - 0.80*airline$c1[k])*s1(param)[k] + (param[2] - 0.80*airline$c2[k])*s2(param)[k]  
)}
for (i in 1:500) {
  result_80[i] = optim(par = param, Merge_80, k = i)
}

## 75%
result_75 = list()
Merge_75 <- function(param,k) { - (
  (param[1] - 0.75*airline$c1[k])*s1(param)[k] + (param[2] - 0.75*airline$c2[k])*s2(param)[k]  
)}
for (i in 1:500) {
  result_75[i] = optim(par = param, Merge_75, k = i)
}
# result à rajouter à la BDD


## TO DO: 
# On considère que chaque ent va conservé la ligne avec le coût le + faible 
# si c1 est plus faible que c2 alors on conserve que p1 merge (après le merge)

airline$p = 0
for (i in 1:500){
  if (airline$c1[i] < airline$c2[i]){airline$p[i] = airline$p1_merged[i]}
  else {airline$p[i] = airline$p2_merged[i]}
}


# Market share en se basant sur p1 
# re faire la même avec les efficicency gain 
