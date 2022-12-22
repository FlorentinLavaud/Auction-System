library(haven)
library(readxl)
library(Hmisc)
library(dplyr)
library(tidyr)
library(stargazer)

vinbis <- read_excel("C:/Users/flore/OneDrive/Bureau/2023/Drive/EOC/vinbis.xlsx", 
                     col_types = c("numeric", "numeric", "text", 
                                   "numeric", "text", "text", "numeric", 
                                   "text", "text", "numeric", "numeric", 
                                   "text", "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric"))


#### Estimation of N
round(mean(vinbis$nbrpers),0.1) # 62 personnes 



#### Estimation of F
# We first split the database: 
vinbis1<- subset(vinbis,opt=="0")
#View(vinbis1)
vinbis2<-mutate(vinbis1,newprice = 2*p1/(estimb+estimh))
plot(vinbis2$newprice)
#View(vinbis2)
plot(vinbis2$newprice, ylab = , main="Plot of newprice 1")

# We then define h(x): 
h = function(x){
  62*x^(61) - (61)*x^62
}
#plot(h)

# Let's define a function that inverse a function: 
inverse = function(f, interval = NULL, lower = 0, upper = 1, ...){
  Vectorize(function(y){
    uniroot(f=function(x){f(x)-y}, lower=0, upper=1, ...)$root
  })
}
# Let's compute h^-1(.): 
h_inv = inverse(h, lower=0,upper=1)
plot(h_inv)


G = ecdf(vinbis2$newprice)
plot(G)

F = h_inv(G(v= vinbis2$newprice))


F = function(x){
  h_inv(G(x))
}


# Let's fit a distribution function using Maximum Likelihood:
# Weibull 
# Lognormal

##Weibull
x = vinbis2$newprice
y = F(x)

weibull_func = function(par, x) sum((pweibull (x, par[1], par[2]) - y)**2)

out = optim(c(1.1,0.1), weibull_func, lower = c(0,0), upper = c(3.5,5), 
            method = "L-BFGS-B", x=x)
print(out)

f = curve(pweibull(x, 0.7571622 , 0.2152933 ), from = 0, to = 3.5)
plot(f, type='l')
points(x, y)


###With lognormal
lognormal_func = function(par, x) sum((plnorm (x, par[1], par[2]) - y)**2)

out = optim(c(1.4,0.4), lognormal_func, lower = c(0,0), upper = c(3.5,10), 
            method = "L-BFGS-B", x=x)
out = out$par
print(out)


f = curve(plnorm(x, 0.0000000  , 0.4185565 ), from = 0, to = 3.5)
plot(f, type='l')
points(x, y)


#### Let's now move to the estimation of k:
vinbis3<- subset(vinbis,vinbis$nbrlots==2)
vinbis3<- subset(vinbis3,vinbis3$nbrvente==2)
#View(vinbis3)

vinbis3$p1 = as.numeric(vinbis3$p1)
vinbis3$p2 = as.numeric(vinbis3$p2)
class(vinbis3$p1)


# We compute the value for k in each case:
k = vinbis3$p2/vinbis3$p1
min(k)
# k = 0.909091


### We prepare the reference matrix  
# Index of the different loops will use this matrix 
vinbis4 = vinbis %>%
  dplyr::filter(opt == 1) %>%
  select(nbrpers, nbrlots, estimh, estimb, nombre)


#Initialisation of storages:
Save3 = list()
P1_store = double()
result_list <- list() # I'll contain nbrpers random values that follow a log normal distrib. 
result_list_k <- list() # list of nbrpers random values after applying k and sort it 
result_list_k2 <- list() # So on and so forth
result_list_k3 <- list()
result_list_k4 <- list()
result_list_k5 <- list()
result_list_k6 <- list()
result_list_k7 <- list()
result_list_k8 <- list()
result_list_k9 <- list()
result_list_k10 <- list()
result_list_k11 <- list()
result_list_k12 <- list()
matrix_store <- list() 
result_revenue <- list() 


###############################################
# ! NOTE : run from row 160 to 392 directly ! # 
###############################################

for (u in 1:100) {
  for (i in 1:nrow(vinbis4)){
    valuation = rlnorm(vinbis4$nbrpers[i], 0.0000000,0.4185565) # generate as many random values as nbrpers
    valuation = sort(valuation, decreasing = T) # sort the list
    result_list[[i]] <- matrix(data = valuation) # save the list
    
    
    # In the code below, it is worth mentioning that 
    # if nbrlots = 4 then it'll go  while loop n*4
    # Otherwise, result_list_k5 will be NULL and so on and so forth
    
    while (vinbis4$nbrlots[i] >= 2){
      # k = 2
      result_list_k2[[i]] = result_list[[i]]  
      result_list_k2[[i]][1] = 0.909091*result_list_k2[[i]][1]
      valuation_k2 = sort(result_list_k2[[i]], decreasing=T)
      result_list_k2[[i]] <- matrix(data = valuation_k2)
      print(i)
      break 
    } 
    
    while (vinbis4$nbrlots[i] >= 3){
      result_list_k3[[i]] = result_list_k2[[i]]  
      result_list_k3[[i]][1] = 0.909091*result_list_k3[[i]][1]
      valuation_k3 = sort(result_list_k3[[i]], decreasing=T)
      result_list_k3[[i]] <- matrix(data = valuation_k3)      
      break
    }
    
    while (vinbis4$nbrlots[i] >= 4){
      # k = 4
      result_list_k4[[i]] = result_list_k3[[i]]  
      result_list_k4[[i]][1] = 0.909091*result_list_k4[[i]][1]
      valuation_k4 = sort(result_list_k4[[i]], decreasing=T)
      result_list_k4[[i]] <- matrix(data = valuation_k4)
      break 
      
    }
    while (vinbis4$nbrlots[i] >= 5){
      # k = 5
      result_list_k5[[i]] = result_list_k4[[i]]  
      result_list_k5[[i]][1] = 0.909091*result_list_k5[[i]][1]
      valuation_k5 = sort(result_list_k5[[i]], decreasing=T)
      result_list_k5[[i]] <- matrix(data = valuation_k5)
      break 
      
    }
    while (vinbis4$nbrlots[i] >= 6){
      # k = 6
      result_list_k6[[i]] = result_list_k5[[i]]  
      result_list_k6[[i]][1] = 0.909091*result_list_k6[[i]][1]
      valuation_k6 = sort(result_list_k6[[i]], decreasing=T)
      result_list_k6[[i]] <- matrix(data = valuation_k6)
      break
      
    }
    while (vinbis4$nbrlots[i] >= 7){
      # k = 7
      result_list_k7[[i]] = result_list_k6[[i]]  
      result_list_k7[[i]][1] = 0.909091*result_list_k7[[i]][1]
      valuation_k7 = sort(result_list_k7[[i]], decreasing=T)
      result_list_k7[[i]] <- matrix(data = valuation_k7)
      break
      
    }
    while (vinbis4$nbrlots[i] >= 8){
      # k = 8
      result_list_k8[[i]] = result_list_k7[[i]]  
      result_list_k8[[i]][1] = 0.909091*result_list_k8[[i]][1]
      valuation_k8 = sort(result_list_k8[[i]], decreasing=T)
      result_list_k8[[i]] <- matrix(data = valuation_k8)
      break
      
    }
    while (vinbis4$nbrlots[i] >= 9){
      # k = 9
      result_list_k9[[i]] = result_list_k8[[i]]  
      result_list_k9[[i]][1] = 0.909091*result_list_k9[[i]][1]
      valuation_k9 = sort(result_list_k9[[i]], decreasing=T)
      result_list_k9[[i]] <- matrix(data = valuation_k9)
      break
      
    }
    while (vinbis4$nbrlots[i] >= 10){
      # k = 10
      result_list_k10[[i]] = result_list_k9[[i]]  
      result_list_k10[[i]][1] = 0.909091*result_list_k10[[i]][1]
      valuation_k10 = sort(result_list_k10[[i]], decreasing=T)
      result_list_k10[[i]] <- matrix(data = valuation_k10)
      break
      
    }
    while (vinbis4$nbrlots[i] >= 11){
      # k = 11
      result_list_k11[[i]] = result_list_k10[[i]]  
      result_list_k11[[i]][1] = 0.909091*result_list_k11[[i]][1]
      valuation_k11 = sort(result_list_k11[[i]], decreasing=T)
      result_list_k11[[i]] <- matrix(data = valuation_k11)
      break
      
    } 
    while (vinbis4$nbrlots[i] >= 12){
      # k = 12
      result_list_k12[[i]] = result_list_k11[[i]]  
      result_list_k12[[i]][1] = 0.909091*result_list_k12[[i]][1]
      valuation_k12 = sort(result_list_k12[[i]], decreasing=T)
      result_list_k12[[i]] <- matrix(data = valuation_k12)
      break
    } 
  }  
  
  
  # intialisation of the storage matrix: 
  prix <- matrix() # Prix will contain the sum of price per observations
  
  for (i in 1:nrow(vinbis4)) {
    Sumprix = result_list[[i]][2] + result_list_k2[[i]][2]
    
    while (vinbis4$nbrlots[i] > 2){
      Sumprix = result_list_k3[[i]][2] + Sumprix
      prix[[i]] <- matrix(data = Sumprix)
      break
      
    }
    prix[[i]] <- matrix(data = Sumprix)
    
    while (vinbis4$nbrlots[i] > 3){
      Sumprix = result_list_k4[[i]][2] + Sumprix
      prix[[i]] <- matrix(data = Sumprix)
      break
      
    }
    prix[[i]] <- matrix(data = Sumprix)
    
    while (vinbis4$nbrlots[i] > 4){
      Sumprix = result_list_k5[[i]][2] + Sumprix
      prix[[i]] <- matrix(data = Sumprix)
      break
      
    }
    prix[[i]] <- matrix(data = Sumprix)
    
    while (vinbis4$nbrlots[i] > 5){
      Sumprix = result_list_k6[[i]][2] + Sumprix
      prix[[i]] <- matrix(data = Sumprix)
      break
      
    }
    prix[[i]] <- matrix(data = Sumprix)
    
    while (vinbis4$nbrlots[i] > 6){
      Sumprix = result_list_k7[[i]][2] + Sumprix
      prix[[i]] <- matrix(data = Sumprix)
      break
      
    }
    prix[[i]] <- matrix(data = Sumprix)
    
    while (vinbis4$nbrlots[i] > 7){
      Sumprix = result_list_k8[[i]][2] + Sumprix
      prix[[i]] <- matrix(data = Sumprix)
      break
      
    }
    prix[[i]] <- matrix(data = Sumprix)
    
    while (vinbis4$nbrlots[i] > 8){
      Sumprix = result_list_k9[[i]][2] + Sumprix
      prix[[i]] <- matrix(data = Sumprix)
      break
      
    }
    prix[[i]] <- matrix(data = Sumprix)
    
    while (vinbis4$nbrlots[i] > 9){
      Sumprix = result_list_k10[[i]][2] + Sumprix
      prix[[i]] <- matrix(data = Sumprix)
      break
      
    }
    prix[[i]] <- matrix(data = Sumprix)
    
    while (vinbis4$nbrlots[i] > 10){
      Sumprix = result_list_k11[[i]][2] + Sumprix
      prix[[i]] <- matrix(data = Sumprix)
      break
      
    }
    prix[[i]] <- matrix(data = Sumprix)
    
    while (vinbis4$nbrlots[i] > 11){
      Sumprix = result_list_k12[[i]][2] + Sumprix
      prix[[i]] <- matrix(data = Sumprix)
      break
      
    }
    prix[[i]] <- matrix(data = Sumprix)
  }
  as.matrix(prix)
  sum(prix)
  
  
  # We retrieve the true price: 
  Drouot_revenue  = as.matrix(vinbis4$estimb + vinbis4$estimh)* as.matrix(prix/2)* as.matrix(vinbis4$nombre/vinbis4$nbrlots)
  Drouot_revenue_without_opt = sum(Drouot_revenue) # this is the revenue that Drouot would have had if they didn't offer an option
  
  # Let's compute the revenue of Drouot with an option
  vinbis_na = vinbis
  vinbis_na$u2 = replace_na(vinbis_na$u2,0)
  vinbis_na$u3 = replace_na(vinbis_na$u3,0)
  vinbis_na$u4 = replace_na(vinbis_na$u4,0)
  vinbis_na$u5 = replace_na(vinbis_na$u5,0)
  vinbis_na$u6 = replace_na(vinbis_na$u5,0)
  
  vinbis_na$p2 = replace_na(vinbis_na$p2,0)
  vinbis_na$p3 = replace_na(vinbis_na$p3,0)
  vinbis_na$p4 = replace_na(vinbis_na$p4,0)
  vinbis_na$p5 = replace_na(vinbis_na$p5,0)
  vinbis_na$p6 = replace_na(vinbis_na$p5,0)
  
  vinbis_na1 = vinbis_na %>%
    filter(opt ==1)
  
  Drouot_revenue_with_opt = sum(vinbis_na1$p1*vinbis_na1$u1*(vinbis_na1$nombre/vinbis_na1$nbrlots),vinbis_na1$p2*vinbis_na1$u2*(vinbis_na1$nombre/vinbis_na1$nbrlots),
                                vinbis_na1$p3*vinbis_na1$u3*(vinbis_na1$nombre/vinbis_na1$nbrlots), vinbis_na1$p4*vinbis_na1$u4*(vinbis_na1$nombre/vinbis_na1$nbrlots),
                                vinbis_na1$p5*vinbis_na1$u5*(vinbis_na1$nombre/vinbis_na1$nbrlots),vinbis_na1$p6*vinbis_na1$u6*(vinbis_na1$nombre/vinbis_na1$nbrlots))
  Drouot_revenue_with_opt
  
  ### RESULT:
  loss = Drouot_revenue_with_opt -Drouot_revenue_without_opt
  loss
  result_revenue[[length(result_revenue)+1]] = loss
} 



# RUN LOOP U until here! #
##########################

result_revenue = unlist(result_revenue)
result_revenue
# Descriptive statistics 
summary(result_revenue)
# CDF
plot(ecdf(result_revenue))
# Density Function
d <- density(result_revenue)
plot(d, main="Density of the loss of Drouot")


#############
# We then compute the same loop but with a Weibull law
##############
#Initialisation of storages:
Save3 = list()
P1_store = double()
result_list <- list() # I'll contain nbrpers random values that follow a log normal distrib. 
result_list_k <- list() # list of nbrpers random values after applying k and sort it 
result_list_k2 <- list() # So on and so forth
result_list_k3 <- list()
result_list_k4 <- list()
result_list_k5 <- list()
result_list_k6 <- list()
result_list_k7 <- list()
result_list_k8 <- list()
result_list_k9 <- list()
result_list_k10 <- list()
result_list_k11 <- list()
result_list_k12 <- list()
matrix_store <- list() 
result_revenue <- list() 

###############################################
# ! NOTE : run from row 434 to 665 directly ! # 
###############################################


for (u in 1:100) {
  for (i in 1:nrow(vinbis4)){
    valuation = rweibull(vinbis4$nbrpers[i], 0.7571622,0.2152933) # generate as many random values as nbrpers
    valuation = sort(valuation, decreasing = T) # sort the list
    result_list[[i]] <- matrix(data = valuation) # save the list
    
    # In the code below, it is worth mentioning that 
    # if nbrlots = 4 then it'll go  while loop n*4
    # Otherwise, result_list_k5 will be NULL and so on and so forth
    
    while (vinbis4$nbrlots[i] >= 2){
      # k = 2
      result_list_k2[[i]] = result_list[[i]]  
      result_list_k2[[i]][1] = 0.909091*result_list_k2[[i]][1]
      valuation_k2 = sort(result_list_k2[[i]], decreasing=T)
      result_list_k2[[i]] <- matrix(data = valuation_k2)
      print(i)
      break 
    } 
    
    while (vinbis4$nbrlots[i] >= 3){
      result_list_k3[[i]] = result_list_k2[[i]]  
      result_list_k3[[i]][1] = 0.909091*result_list_k3[[i]][1]
      valuation_k3 = sort(result_list_k3[[i]], decreasing=T)
      result_list_k3[[i]] <- matrix(data = valuation_k3)      
      break
    }
    
    while (vinbis4$nbrlots[i] >= 4){
      # k = 4
      result_list_k4[[i]] = result_list_k3[[i]]  
      result_list_k4[[i]][1] = 0.909091*result_list_k4[[i]][1]
      valuation_k4 = sort(result_list_k4[[i]], decreasing=T)
      result_list_k4[[i]] <- matrix(data = valuation_k4)
      break 
      
    }
    while (vinbis4$nbrlots[i] >= 5){
      # k = 5
      result_list_k5[[i]] = result_list_k4[[i]]  
      result_list_k5[[i]][1] = 0.909091*result_list_k5[[i]][1]
      valuation_k5 = sort(result_list_k5[[i]], decreasing=T)
      result_list_k5[[i]] <- matrix(data = valuation_k5)
      break 
      
    }
    while (vinbis4$nbrlots[i] >= 6){
      # k = 6
      result_list_k6[[i]] = result_list_k5[[i]]  
      result_list_k6[[i]][1] = 0.909091*result_list_k6[[i]][1]
      valuation_k6 = sort(result_list_k6[[i]], decreasing=T)
      result_list_k6[[i]] <- matrix(data = valuation_k6)
      break
      
    }
    while (vinbis4$nbrlots[i] >= 7){
      # k = 7
      result_list_k7[[i]] = result_list_k6[[i]]  
      result_list_k7[[i]][1] = 0.909091*result_list_k7[[i]][1]
      valuation_k7 = sort(result_list_k7[[i]], decreasing=T)
      result_list_k7[[i]] <- matrix(data = valuation_k7)
      break
      
    }
    while (vinbis4$nbrlots[i] >= 8){
      # k = 8
      result_list_k8[[i]] = result_list_k7[[i]]  
      result_list_k8[[i]][1] = 0.909091*result_list_k8[[i]][1]
      valuation_k8 = sort(result_list_k8[[i]], decreasing=T)
      result_list_k8[[i]] <- matrix(data = valuation_k8)
      break
      
    }
    while (vinbis4$nbrlots[i] >= 9){
      # k = 9
      result_list_k9[[i]] = result_list_k8[[i]]  
      result_list_k9[[i]][1] = 0.909091*result_list_k9[[i]][1]
      valuation_k9 = sort(result_list_k9[[i]], decreasing=T)
      result_list_k9[[i]] <- matrix(data = valuation_k9)
      break
      
    }
    while (vinbis4$nbrlots[i] >= 10){
      # k = 10
      result_list_k10[[i]] = result_list_k9[[i]]  
      result_list_k10[[i]][1] = 0.909091*result_list_k10[[i]][1]
      valuation_k10 = sort(result_list_k10[[i]], decreasing=T)
      result_list_k10[[i]] <- matrix(data = valuation_k10)
      break
      
    }
    while (vinbis4$nbrlots[i] >= 11){
      # k = 11
      result_list_k11[[i]] = result_list_k10[[i]]  
      result_list_k11[[i]][1] = 0.909091*result_list_k11[[i]][1]
      valuation_k11 = sort(result_list_k11[[i]], decreasing=T)
      result_list_k11[[i]] <- matrix(data = valuation_k11)
      break
      
    } 
    while (vinbis4$nbrlots[i] >= 12){
      # k = 12
      result_list_k12[[i]] = result_list_k11[[i]]  
      result_list_k12[[i]][1] = 0.909091*result_list_k12[[i]][1]
      valuation_k12 = sort(result_list_k12[[i]], decreasing=T)
      result_list_k12[[i]] <- matrix(data = valuation_k12)
      break
    } 
  }  
  
  
  # intialisation of the storage matrix: 
  prix <- matrix() # Prix will contain the sum of price per observations
  
  for (i in 1:nrow(vinbis4)) {
    Sumprix = result_list[[i]][2] + result_list_k2[[i]][2]
    
    while (vinbis4$nbrlots[i] > 2){
      Sumprix = result_list_k3[[i]][2] + Sumprix
      prix[[i]] <- matrix(data = Sumprix)
      break
      
    }
    prix[[i]] <- matrix(data = Sumprix)
    
    while (vinbis4$nbrlots[i] > 3){
      Sumprix = result_list_k4[[i]][2] + Sumprix
      prix[[i]] <- matrix(data = Sumprix)
      break
      
    }
    prix[[i]] <- matrix(data = Sumprix)
    
    while (vinbis4$nbrlots[i] > 4){
      Sumprix = result_list_k5[[i]][2] + Sumprix
      prix[[i]] <- matrix(data = Sumprix)
      break
      
    }
    prix[[i]] <- matrix(data = Sumprix)
    
    while (vinbis4$nbrlots[i] > 5){
      Sumprix = result_list_k6[[i]][2] + Sumprix
      prix[[i]] <- matrix(data = Sumprix)
      break
      
    }
    prix[[i]] <- matrix(data = Sumprix)
    
    while (vinbis4$nbrlots[i] > 6){
      Sumprix = result_list_k7[[i]][2] + Sumprix
      prix[[i]] <- matrix(data = Sumprix)
      break
      
    }
    prix[[i]] <- matrix(data = Sumprix)
    
    while (vinbis4$nbrlots[i] > 7){
      Sumprix = result_list_k8[[i]][2] + Sumprix
      prix[[i]] <- matrix(data = Sumprix)
      break
      
    }
    prix[[i]] <- matrix(data = Sumprix)
    
    while (vinbis4$nbrlots[i] > 8){
      Sumprix = result_list_k9[[i]][2] + Sumprix
      prix[[i]] <- matrix(data = Sumprix)
      break
      
    }
    prix[[i]] <- matrix(data = Sumprix)
    
    while (vinbis4$nbrlots[i] > 9){
      Sumprix = result_list_k10[[i]][2] + Sumprix
      prix[[i]] <- matrix(data = Sumprix)
      break
      
    }
    prix[[i]] <- matrix(data = Sumprix)
    
    while (vinbis4$nbrlots[i] > 10){
      Sumprix = result_list_k11[[i]][2] + Sumprix
      prix[[i]] <- matrix(data = Sumprix)
      break
      
    }
    prix[[i]] <- matrix(data = Sumprix)
    
    while (vinbis4$nbrlots[i] > 11){
      Sumprix = result_list_k12[[i]][2] + Sumprix
      prix[[i]] <- matrix(data = Sumprix)
      break
      
    }
    prix[[i]] <- matrix(data = Sumprix)
  }
  as.matrix(prix)
  sum(prix)
  
  
  # We retrieve the true price: 
  Drouot_revenue  = as.matrix(vinbis4$estimb + vinbis4$estimh)* as.matrix(prix/2)* as.matrix(vinbis4$nombre/vinbis4$nbrlots)
  Drouot_revenue_without_opt = sum(Drouot_revenue) # this is the revenue that Drouot would have had if they didn't offer an option
  
  # Let's compute the revenue of Drouot with an option
  vinbis_na = vinbis
  vinbis_na$u2 = replace_na(vinbis_na$u2,0)
  vinbis_na$u3 = replace_na(vinbis_na$u3,0)
  vinbis_na$u4 = replace_na(vinbis_na$u4,0)
  vinbis_na$u5 = replace_na(vinbis_na$u5,0)
  vinbis_na$u6 = replace_na(vinbis_na$u5,0)
  
  vinbis_na$p2 = replace_na(vinbis_na$p2,0)
  vinbis_na$p3 = replace_na(vinbis_na$p3,0)
  vinbis_na$p4 = replace_na(vinbis_na$p4,0)
  vinbis_na$p5 = replace_na(vinbis_na$p5,0)
  vinbis_na$p6 = replace_na(vinbis_na$p5,0)
  
  vinbis_na1 = vinbis_na %>%
    filter(opt ==1)
  
  Drouot_revenue_with_opt = sum(vinbis_na1$p1*vinbis_na1$u1*(vinbis_na1$nombre/vinbis_na1$nbrlots),vinbis_na1$p2*vinbis_na1$u2*(vinbis_na1$nombre/vinbis_na1$nbrlots),
                                vinbis_na1$p3*vinbis_na1$u3*(vinbis_na1$nombre/vinbis_na1$nbrlots), vinbis_na1$p4*vinbis_na1$u4*(vinbis_na1$nombre/vinbis_na1$nbrlots),
                                vinbis_na1$p5*vinbis_na1$u5*(vinbis_na1$nombre/vinbis_na1$nbrlots),vinbis_na1$p6*vinbis_na1$u6*(vinbis_na1$nombre/vinbis_na1$nbrlots))
  Drouot_revenue_with_opt
  
  ### RESULT:
  loss = Drouot_revenue_with_opt -Drouot_revenue_without_opt
  loss
  result_revenue[[length(result_revenue)+1]] = loss
} 
result_revenue = unlist(result_revenue)
mean(result_revenue)
# Descriptive statistics 
summary(result_revenue)
mean(result_revenue)
# CDF
plot(ecdf(result_revenue))
# Density Function
d <- density(result_revenue)
plot(d, main="Density of the loss of Drouot")