library(haven)
library(dplyr)

airline <- read_dta("C:/Users/flore/OneDrive/Bureau/2023/Drive/EOC/donnees_airline.dta")


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


# Ici on est sur que ?a soit des fonctions :
delta_1 = function(param, k){iv$alpha - iv$beta*param[1] + iv$gamma*airline$escale1[k] + iv$mu*airline$dist_1000km[k]+ airline$chsi_1[k]}
delta_2 = function(param, k){iv$alpha - iv$beta*param[2] + iv$gamma*airline$escale2[k] + iv$mu*airline$dist_1000km[k]+ airline$chsi_2[k]}

# On continue avec les fonctions suivantes: 
s1 = function(param, k) {
        exp(delta_1(param, k)) / (1 + exp(delta_1(param, k)) + exp(delta_2(param, k)))
}

s2 = function(param, k) {
        (exp(delta_2(param, k))) / (1 + exp(delta_1(param, k)) + exp(delta_2(param, k)))
}

# on v?rifie si on a des chiffres coh?rents: 
mean(airline$p1)
mean(airline$p2)
param = c(201.0613,200.5109)
s1(param,1)
s2(param,1)
airline$s1[1]
airline$s2[1]

# On dÃ©finit ensuite notre fonction d'optimisation : 

# ini: 
result= list()
j = 0
param = c(mean(airline$p1),mean(airline$p2))

Merge <- function(param,k) { - (
        (param[1] - airline$c1[k])*s1(param)[k]  + (param[2] - airline$c2[k])*s2(param)[k]
)}
Merge(param,10) # check
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
# !!!   A noter que parfois la création de s1_merged ne marche pas. Dans ce cas, 
# il faut runer une fois la boucle, obtenir un msg d'erreur car la colonne s1_merged n'a pas
# été initialisée puis refaire tourner airline$s1_merged[] = 0 et ensuite refaire 
# tourner la boucle


## with cost synergies: 


## 90% 

delta_1 = function(param, k){iv$alpha - iv$beta*param[1] + iv$gamma*airline$escale1[k] + iv$mu*airline$dist_1000km[k]+ airline$chsi_1[k]}
delta_2 = function(param, k){iv$alpha - iv$beta*param[2] + iv$gamma*airline$escale2[k] + iv$mu*airline$dist_1000km[k]+ airline$chsi_2[k]}
s1 = function(param, k) {
        exp(delta_1(param, k)) / (1 + exp(delta_1(param, k)) + exp(delta_2(param, k)))
}
s2 = function(param, k) {
        (exp(delta_2(param, k))) / (1 + exp(delta_1(param, k)) + exp(delta_2(param, k)))
}


result_90 = list()
Merge_90 <- function(param,k) { - (
        (param[1] - 0.1*airline$c1[k])*s1(param)[k] + (param[2] - 0.1*airline$c2[k])*s2(param)[k]  
)}
for (i in 1:500) {
        result_90[i] = optim(par = param, Merge_90, k = i)
}

result_90 <- t(as.data.frame(do.call(cbind, result_90)))
#View(result_90)


airline$p1_merged_90 = result_90[,1]
airline$p2_merged_90 = result_90[,2]

# View(airline)

## Market share : 

delta_1 = list()
delta_2 = list()
for (i in 1:500) {
        delta_1[i] = iv$alpha - iv$beta*airline$p1_merged_90[i] + iv$gamma*airline$escale1[i] + iv$mu*airline$dist_1000km[i]+ airline$chsi_1[i]
        delta_2[i] = iv$alpha - iv$beta*airline$p2_merged_90[i] + iv$gamma*airline$escale2[i] + iv$mu*airline$dist_1000km[i]+ airline$chsi_2[i]
}

# ini: 
delta_1 = unlist(delta_1)
delta_2 = unlist(delta_2)
airline$s1_merged_90[] = 0
airline$s2_merged_90[] = 0

for (i in 1:500) {
        airline$s1_merged_90[i] = exp(delta_1[i]) / (1 + exp(delta_1[i]) + exp(delta_2[i]))
}

for (i in 1:500) {
        airline$s2_merged_90[i] = exp(delta_2[i]) / (1 + exp(delta_1[i]) + exp(delta_2[i]))
}




## 85%


delta_1 = function(param, k){iv$alpha - iv$beta*param[1] + iv$gamma*airline$escale1[k] + iv$mu*airline$dist_1000km[k]+ airline$chsi_1[k]}
delta_2 = function(param, k){iv$alpha - iv$beta*param[2] + iv$gamma*airline$escale2[k] + iv$mu*airline$dist_1000km[k]+ airline$chsi_2[k]}
s1 = function(param, k) {
        exp(delta_1(param, k)) / (1 + exp(delta_1(param, k)) + exp(delta_2(param, k)))
}
s2 = function(param, k) {
        (exp(delta_2(param, k))) / (1 + exp(delta_1(param, k)) + exp(delta_2(param, k)))
}

result_85 = list()
Merge_85 <- function(param,k) { - (
        (param[1] - 0.85*airline$c1[k])*s1(param)[k] + (param[2] - 0.85*airline$c2[k])*s2(param)[k]  
)}
for (i in 1:500) {
        result_85[i] = optim(par = param, Merge_85, k = i)
}


result_85 <- t(as.data.frame(do.call(cbind, result_85)))
#View(result_85)

airline$p1_merged_85 = result_85[,1]
airline$p2_merged_85 = result_85[,2]


### Market share : 

delta_1 = list()
delta_2 = list()
for (i in 1:500) {
        delta_1[i] = iv$alpha - iv$beta*airline$p1_merged_85[i] + iv$gamma*airline$escale1[i] + iv$mu*airline$dist_1000km[i]+ airline$chsi_1[i]
        delta_2[i] = iv$alpha - iv$beta*airline$p2_merged_85[i] + iv$gamma*airline$escale2[i] + iv$mu*airline$dist_1000km[i]+ airline$chsi_2[i]
}

# ini: 
delta_1 = unlist(delta_1)
delta_2 = unlist(delta_2)
airline$s1_merged_85[] = 0
airline$s2_merged_85[] = 0

for (i in 1:500) {
        airline$s1_merged_85[i] = exp(delta_1[i]) / (1 + exp(delta_1[i]) + exp(delta_2[i]))
}

for (i in 1:500) {
        airline$s2_merged_85[i] = exp(delta_2[i]) / (1 + exp(delta_1[i]) + exp(delta_2[i]))
}






## 80%

delta_1 = function(param, k){iv$alpha - iv$beta*param[1] + iv$gamma*airline$escale1[k] + iv$mu*airline$dist_1000km[k]+ airline$chsi_1[k]}
delta_2 = function(param, k){iv$alpha - iv$beta*param[2] + iv$gamma*airline$escale2[k] + iv$mu*airline$dist_1000km[k]+ airline$chsi_2[k]}
s1 = function(param, k) {
        exp(delta_1(param, k)) / (1 + exp(delta_1(param, k)) + exp(delta_2(param, k)))
}
s2 = function(param, k) {
        (exp(delta_2(param, k))) / (1 + exp(delta_1(param, k)) + exp(delta_2(param, k)))
}

result_80 = list()
Merge_80 <- function(param,k) { - (
        (param[1] - 0.8*airline$c1[k])*s1(param)[k] + (param[2] - 0.8*airline$c2[k])*s2(param)[k]  
)}
for (i in 1:500) {
        result_80[i] = optim(par = param, Merge_80, k = i)
}


result_80 <- t(as.data.frame(do.call(cbind, result_80)))
#View(result_80)

airline$p1_merged_80 = result_80[,1]
airline$p2_merged_80 = result_80[,2]


### Market share : 

delta_1 = list()
delta_2 = list()
for (i in 1:500) {
        delta_1[i] = iv$alpha - iv$beta*airline$p1_merged_80[i] + iv$gamma*airline$escale1[i] + iv$mu*airline$dist_1000km[i]+ airline$chsi_1[i]
        delta_2[i] = iv$alpha - iv$beta*airline$p2_merged_80[i] + iv$gamma*airline$escale2[i] + iv$mu*airline$dist_1000km[i]+ airline$chsi_2[i]
}

# ini: 
delta_1 = unlist(delta_1)
delta_2 = unlist(delta_2)
airline$s1_merged_80[] = 0
airline$s2_merged_80[] = 0

for (i in 1:500) {
        airline$s1_merged_80[i] = exp(delta_1[i]) / (1 + exp(delta_1[i]) + exp(delta_2[i]))
}

for (i in 1:500) {
        airline$s2_merged_80[i] = exp(delta_2[i]) / (1 + exp(delta_1[i]) + exp(delta_2[i]))
}

#View(airline)


## 75%

delta_1 = function(param, k){iv$alpha - iv$beta*param[1] + iv$gamma*airline$escale1[k] + iv$mu*airline$dist_1000km[k]+ airline$chsi_1[k]}
delta_2 = function(param, k){iv$alpha - iv$beta*param[2] + iv$gamma*airline$escale2[k] + iv$mu*airline$dist_1000km[k]+ airline$chsi_2[k]}
s1 = function(param, k) {
        exp(delta_1(param, k)) / (1 + exp(delta_1(param, k)) + exp(delta_2(param, k)))
}
s2 = function(param, k) {
        (exp(delta_2(param, k))) / (1 + exp(delta_1(param, k)) + exp(delta_2(param, k)))
}


result_75 = list()
Merge_75 <- function(param,k) { - (
        (param[1] - 0.75*airline$c1[k])*s1(param)[k] + (param[2] - 0.75*airline$c2[k])*s2(param)[k]  
)}
for (i in 1:500) {
        result_75[i] = optim(par = param, Merge_75, k = i)
}


result_75 <- t(as.data.frame(do.call(cbind, result_75)))
#View(result_75)

airline$p1_merged_75 = result_80[,1]
airline$p2_merged_75 = result_80[,2]


### Market share : 

delta_1 = list()
delta_2 = list()
for (i in 1:500) {
        delta_1[i] = iv$alpha - iv$beta*airline$p1_merged_75[i] + iv$gamma*airline$escale1[i] + iv$mu*airline$dist_1000km[i]+ airline$chsi_1[i]
        delta_2[i] = iv$alpha - iv$beta*airline$p2_merged_75[i] + iv$gamma*airline$escale2[i] + iv$mu*airline$dist_1000km[i]+ airline$chsi_2[i]
}

# ini: 
delta_1 = unlist(delta_1)
delta_2 = unlist(delta_2)
airline$s1_merged_75[] = 0
airline$s2_merged_75[] = 0

for (i in 1:500) {
        airline$s1_merged_75[i] = exp(delta_1[i]) / (1 + exp(delta_1[i]) + exp(delta_2[i]))
}

for (i in 1:500) {
        airline$s2_merged_75[i] = exp(delta_2[i]) / (1 + exp(delta_1[i]) + exp(delta_2[i]))
}




# View(airline)


# On définit ici la stratégie 'p' : 
airline$p = 0
for (i in 1:500){
        if (airline$c1[i] < airline$c2[i]){airline$p[i] = airline$p1_merged[i]}
        else {airline$p[i] = airline$p2_merged[i]}
}


# Market share en se basant sur p

delta_1_p = list()
delta_2_p = list()

for (i in 1:500) {
        if (airline$c1[i] < airline$c2[i]){
        delta_1_p[i] = iv$alpha - iv$beta*airline$p[i] + iv$gamma*airline$escale1[i] + iv$mu*airline$dist_1000km[i]+ airline$chsi_1[i]
        }
        else {
                delta_2_p[i] = iv$alpha - iv$beta*airline$p[i] + iv$gamma*airline$escale2[i] + iv$mu*airline$dist_1000km[i]+ airline$chsi_2[i]   
        }
        
}

#View(airline)

# ini: 
delta_1_p = unlist(delta_1_p)
delta_2_p = unlist(delta_2_p)


airline$s = 0
for (i in 1:500){
        if (airline$c1[i] < airline$c2[i]){
                airline$s[i] = exp(delta_1_p[i]) / (1 + exp(delta_1_p[i]) + exp(delta_2_p[i]))
                
        }
        
        else { airline$s[i] = exp(delta_2_p[i]) / (1 + exp(delta_1_p[i]) + exp(delta_2_p[i]))
        
        }
        
}

# for (i in 1:500) {
#   if (airline$s[i] == 0.000000000) {airline$s[i] = 'NA'} else {}
# }



## with cost synergies: 


##  90% 
param = (mean(airline$p1)+mean(airline$p2))/2
delta = function(param, k){iv$alpha - iv$beta*param + iv$gamma*airline$escale1[k] + iv$mu*airline$dist_1000km[k]+ airline$chsi_1[k]}
s = function(param, k) {
  exp(delta(param, k)) / (1 + exp(delta(param, k)) + exp(delta(param, k)))
}

result_p_90 = list()
Merge_p_90 <- function(param,k) { - (
  (param - 0.9*airline$c1[k])*s(param)[k] + (param - 0.9*airline$c2[k])*s(param)[k]  
)}
#View(airline)

for (i in 1:500) {
  result_p_90[i] = optim(par = param, Merge_p_90, k = i)
}

result_p_90 <- t(as.data.frame(do.call(cbind, result_p_90)))
#View(result_p_90)
airline$p_90 = result_90[,1]

# Market share : 
delta_1_p_90 = list()
delta_2_p_90 = list()
for (i in 1:500) {
        if (airline$c1[i] < airline$c2[i]){
          delta_1_p_90[i] = iv$alpha - iv$beta*airline$p_90[i] + iv$gamma*airline$escale1[i] + iv$mu*airline$dist_1000km[i]+ airline$chsi_1[i]
        }
        else {
          delta_2_p_90[i] = iv$alpha - iv$beta*airline$p_90[i] + iv$gamma*airline$escale2[i] + iv$mu*airline$dist_1000km[i]+ airline$chsi_2[i]   
        }
        
}


# ini: 
delta_1_p_90 = unlist(delta_1_p_90)
delta_2_p_90 = unlist(delta_2_p_90)


airline$s_90 = 0
for (i in 1:500){
        if (airline$c1[i] < airline$c2[i]){
                airline$s_90[i] = exp(delta_1_p_90[i]) / (1 + exp(delta_1_p_90[i]) + exp(delta_2_p_90[i]))
                
        }
        
        else { airline$s_90[i] = exp(delta_2_p_90[i]) / (1 + exp(delta_1_p_90[i]) + exp(delta_2_p_90[i]))
        
        }
        
}

#View(airline)

## 85%
param = (mean(airline$p1)+mean(airline$p2))/2
delta = function(param, k){iv$alpha - iv$beta*param + iv$gamma*airline$escale1[k] + iv$mu*airline$dist_1000km[k]+ airline$chsi_1[k]}
s = function(param, k) {
  exp(delta(param, k)) / (1 + exp(delta(param, k)) + exp(delta(param, k)))
}

result_p_85 = list()
Merge_p_85 <- function(param,k) { - (
  (param - 0.85*airline$c1[k])*s(param)[k] + (param - 0.85*airline$c2[k])*s(param)[k]  
)}

for (i in 1:500) {
  result_p_85[i] = optim(par = param, Merge_p_85, k = i)
}

result_p_85 <- t(as.data.frame(do.call(cbind, result_p_85)))
#View(result_p_85)
airline$p_85 = result_85[,1]
View(airline)
# Market share : 
delta_1_p_85 = list()
delta_2_p_85 = list()
for (i in 1:500) {
  if (airline$c1[i] < airline$c2[i]){
    delta_1_p_85[i] = iv$alpha - iv$beta*airline$p_85[i] + iv$gamma*airline$escale1[i] + iv$mu*airline$dist_1000km[i]+ airline$chsi_1[i]
  }
  else {
    delta_2_p_85[i] = iv$alpha - iv$beta*airline$p_85[i] + iv$gamma*airline$escale2[i] + iv$mu*airline$dist_1000km[i]+ airline$chsi_2[i]   
  }
  
}


# ini: 
delta_1_p_85 = unlist(delta_1_p_85)
delta_2_p_85 = unlist(delta_2_p_85)


airline$s_85 = 0
for (i in 1:500){
  if (airline$c1[i] < airline$c2[i]){
    airline$s_85[i] = exp(delta_1_p_85[i]) / (1 + exp(delta_1_p_85[i]) + exp(delta_2_p_85[i]))
    
  }
  
  else { airline$s_85[i] = exp(delta_2_p_85[i]) / (1 + exp(delta_1_p_85[i]) + exp(delta_2_p_85[i]))
  
  }
  
}
View(airline)



## 80%
param = (mean(airline$p1)+mean(airline$p2))/2
delta = function(param, k){iv$alpha - iv$beta*param + iv$gamma*airline$escale1[k] + iv$mu*airline$dist_1000km[k]+ airline$chsi_1[k]}
s = function(param, k) {
  exp(delta(param, k)) / (1 + exp(delta(param, k)) + exp(delta(param, k)))
}

result_p_80 = list()
Merge_p_80 <- function(param,k) { - (
  (param - 0.8*airline$c1[k])*s(param)[k] + (param - 0.8*airline$c2[k])*s(param)[k]  
)}

for (i in 1:500) {
  result_p_80[i] = optim(par = param, Merge_p_80, k = i)
}

result_p_80 <- t(as.data.frame(do.call(cbind, result_p_80)))
#View(result_p_80)
airline$p_80 = result_80[,1]
View(airline)
# Market share : 
delta_1_p_80 = list()
delta_2_p_80 = list()
for (i in 1:500) {
  if (airline$c1[i] < airline$c2[i]){
    delta_1_p_80[i] = iv$alpha - iv$beta*airline$p[i] + iv$gamma*airline$escale1[i] + iv$mu*airline$dist_1000km[i]+ airline$chsi_1[i]
  }
  else {
    delta_2_p_80[i] = iv$alpha - iv$beta*airline$p[i] + iv$gamma*airline$escale2[i] + iv$mu*airline$dist_1000km[i]+ airline$chsi_2[i]   
  }
  
}


# ini: 
delta_1_p_80 = unlist(delta_1_p_80)
delta_2_p_80 = unlist(delta_2_p_80)


airline$s_80 = 0
for (i in 1:500){
  if (airline$c1[i] < airline$c2[i]){
    airline$s_80[i] = exp(delta_1_p_80[i]) / (1 + exp(delta_1_p_80[i]) + exp(delta_2_p_80[i]))
    
  }
  
  else { airline$s_80[i] = exp(delta_2_p_80[i]) / (1 + exp(delta_1_p_80[i]) + exp(delta_2_p_80[i]))
  
  }
  
}

# View(airline)


## 75%
param = (mean(airline$p1)+mean(airline$p2))/2
delta = function(param, k){iv$alpha - iv$beta*param + iv$gamma*airline$escale1[k] + iv$mu*airline$dist_1000km[k]+ airline$chsi_1[k]}
s = function(param, k) {
  exp(delta(param, k)) / (1 + exp(delta(param, k)) + exp(delta(param, k)))
}

result_p_75 = list()
Merge_p_75 <- function(param,k) { - (
  (param - 0.75*airline$c1[k])*s(param)[k] + (param - 0.75*airline$c2[k])*s(param)[k]  
)}

for (i in 1:500) {
  result_p_75[i] = optim(par = param, Merge_p_75, k = i)
}

result_p_75 <- t(as.data.frame(do.call(cbind, result_p_75)))
#View(result_p_75)
airline$p_75 = result_75[,1]
View(airline)
# Market share : 
delta_1_p_75 = list()
delta_2_p_75 = list()
for (i in 1:500) {
  if (airline$c1[i] < airline$c2[i]){
    delta_1_p_75[i] = iv$alpha - iv$beta*airline$p[i] + iv$gamma*airline$escale1[i] + iv$mu*airline$dist_1000km[i]+ airline$chsi_1[i]
  }
  else {
    delta_2_p_75[i] = iv$alpha - iv$beta*airline$p[i] + iv$gamma*airline$escale2[i] + iv$mu*airline$dist_1000km[i]+ airline$chsi_2[i]   
  }
  
}


# ini: 
delta_1_p_75 = unlist(delta_1_p_75)
delta_2_p_75 = unlist(delta_2_p_75)


airline$s_75 = 0
for (i in 1:500){
  if (airline$c1[i] < airline$c2[i]){
    airline$s_75[i] = exp(delta_1_p_75[i]) / (1 + exp(delta_1_p_75[i]) + exp(delta_2_p_75[i]))
    
  }
  
  else { airline$s_75[i] = exp(delta_2_p_75[i]) / (1 + exp(delta_1_p_75[i]) + exp(delta_2_p_75[i]))
  
  }
  
}
# View(airline)


library("writexl")
write_xlsx(airline,"C:/Users/flore/OneDrive/Bureau/2023/Drive/EOC/BDD_airline.xlsx")


