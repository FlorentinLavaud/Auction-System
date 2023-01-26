#############################
# Auction simulation # 
#############################

#### Let's first download all required packages

#install.packages("haven")
#install.packages("writexl")
#install.packages("Hmisc")
library(writexl)
library(readxl)
library(Hmisc)
library(haven)
library(tidyr)
library(tidyverse)
library(dplyr)


# Note that most plots/View() are in comments 
# Hence they need to be uncomment 


# We import the database
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
# We compute G^(.): 
G = ecdf(vinbis2$newprice)
#plot(G)

G <- function(x){
  G = ecdf(x)
}

G(vinbis2$newprice)

  

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

# We finally get F:
F = h_inv(G(v= vinbis2$newprice))
plot(vinbis2$newprice, F)


#plot(vinbis2$newprice, F)
# we check if our inverse function worked: 
verif = h(F) - G(v= vinbis2$newprice)
verif # null values which is logical 

# The issue is that we only have the end of the distribution 
#plot(vinbis2$newprice, F)

# We first make a graph analysis to assess which distribution has the best fit:
pnorm = curve(pnorm, from = 0, to = 3, main="LogNormal distribution")
weibull = curve(dweibull(x, scale=5, shape=1.5),from=0, to=3, main="Weibull distribution")

# Comparison
# plot(vinbis2$newprice, F)
# plot(plnorm)
# plot(weibull)

# We then use the maximum likelihood estimator to find the best 
# parameters for pnorm so that we fit with the data
llik = function(x,par){
  m=par[1]
  s=par[2]
  m2=par[1]*par[1]
  s2=par[2]*par[2]
  ll = -151*0.5*log(2*pi)-0.5*151*log(s2) - (1/(2*s2))*sum((log(x)^2)) + (m/s2)*sum(log(x)) - (151*m2/s2)
  return(-ll)
}

x= sample(F, 151, replace= TRUE)

optim(x = vinbis2$newprice, par=c(0,1), llik)
f = curve(plnorm(x, meanlog = 0.1048789 , sdlog = 0.3701233), from = min(vinbis2$newprice), to = max(vinbis2$newprice))
plot(G)


#### Let's now move to the estimation of k:
xls = write_xlsx(vinbis,"C:/Users/flore/OneDrive/Bureau/2023/Drive/EOC/vinbis_K_estim.xlsx")
vinbis3<- subset(vinbis,vinbis$nbrlots==2)
vinbis3<- subset(vinbis3,vinbis3$nbrvente==2)
View(vinbis3)

vinbis3$p1 = as.numeric(vinbis3$p1)
vinbis3$p2 = as.numeric(vinbis3$p2)
class(vinbis3$p1)

# We compute the value for k in each case:
k = vinbis3$p2/vinbis3$p1
min(k)
# k = 0.909091


### We prepare the reference matrix  
vinbis4 = vinbis %>%
  filter(opt == 1) %>%
  select(nbrpers, nbrlots, estimh, estimb)
#View(vinbis4)

# initialisation of storages:
Save3 = list()
P1_store = double()
result_list <- list()
result_list_k <- list()
result_list_k2 <- list()
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
  valuation = rlnorm(vinbis4$nbrpers[i], 0.1048789,0.3701233) # generate as many random values as nbrpers
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
prix <- matrix()

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
Revenue  = (as.matrix((vinbis4$estimh + vinbis4$estimh)) * as.matrix(prix)/2)
Revenue_without_opt = sum(Revenue) # this is the revenue that the company would have had if they didn't offer an option

# Let's compute the revenue  with an option
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

revenue_with_opt = sum(vinbis_na1$p1 *vinbis_na1$u1,vinbis_na1$p2 *vinbis_na1$u2,
    vinbis_na1$p3 *vinbis_na1$u3, vinbis_na1$p4 *vinbis_na1$u4,
    vinbis_na1$p5 *vinbis_na1$u5,vinbis_na1$p6 *vinbis_na1$u6)
revenue_with_opt

### RESULT:
loss = revenue_with_opt -Revenue_without_opt
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
plot(d, main="Density of the loss")
