library(reshape)
library(dplyr)
library(xtable)
library(tidyr)
library(stringr)
library(gtools)

setwd("~/Code_scientific_revolution/estimates_pomp/")
Results = read.csv("../Results.csv")
Results$Nobs = log(Results$Nobs+1)
Results$Nobs = Results$Nobs/sd(Results$Nobs)
Results$gdp = Results$gdp - min(Results$gdp,na.rm=T)
Distances = read.csv("../Distances.csv",row.names = 1)
R = Results[c("Countries","Date","Nobs")]
R  = spread(R,"Countries","Nobs")
rownames(R) = R$Date
R = R[-1]
R = R[Countries]

Results$diff = NA
Results$cum = NA
Results$cum_diff = NA
for(i in 1:nrow(Results)){
  Results$diff[i] = sum(untable(R[as.character(Results$Date[i]),])/untable(Distances[as.character(Results$Countries[i]),])**2)
  d = Results[1:i,]
  d = d[d$Countries==d$Countries[i],]
  Results$cum[i] = sum(d$Nobs[1:nrow(d)-1])
  Results$cum_diff[i] = sum(d$diff)
}
PARAM = c("bla","a","b","c","d","e","f","z","sigma","sigma_obs","N_0","sigma2","g")
job = list()
job2 = list()
mifs_pomp = list()
unused_parameters = list()
names = c("A","B","C","D",'E','F','G','H','I','J','K')
mifs_pomp = list()
unused_parameters = list()
unused_parameters[[1]] = c(1,2,3,5,6,7,8,13)
unused_parameters[[2]] = c(1,2,3,5,6,7,13)
unused_parameters[[3]] = c(1,3,5,6,7,13)
unused_parameters[[4]] = c(1,2,3,5,7,13)
unused_parameters[[5]] = c(1,3,5,6,13)
unused_parameters[[6]] = c(1,2,3,6,7,13)
unused_parameters[[7]] = c(1,3,6,7,13)
unused_parameters[[8]] = c(1,2,5,6,7,13)
unused_parameters[[9]] = c(1,5,6,7,13)
unused_parameters[[10]] = c(1,2,3,5,6,7)
unused_parameters[[11]] = c(1)
names(unused_parameters) = names



estimates = list()
max_loglik = vector()
aic = vector()
bic = vector()
for(name in names){
  file = paste("estimates__MLE_with_sigma2",name,sep="")
  estimates[[name]] = read.csv(paste(file,".csv",sep=""))
  print(max(as.numeric(as.character(estimates[[name]]$loglik)),na.rm = T))
  aic[name] = 2*(length(PARAM)-length(unused_parameters[[name]])-max(as.numeric(as.character(estimates[[name]]$loglik)),na.rm = T))
  print(paste("AIC =",as.character(aic[name])))
  bic[name] = log(nrow(Results)-2)*(length(PARAM)-length(unused_parameters[[name]]))-2*(max(as.numeric(as.character(estimates[[name]]$loglik)),na.rm = T))
  print(paste("BIC =",as.character(bic[name])))
  max_loglik[name] = max(estimates[[name]]$loglik)
  print(name)
  
}
coefs = data.frame(matrix(ncol=6,nrow=0),stringsAsFactors=F)
colnames(coefs) = c("GDPpc","Diff", "GDPpc*Diff", "Cum", "GDPpc*Cum", "Cum_diff")
for(name in names){
  print(name)
  a = estimates[[name]][order(-estimates[[name]]$loglik),][1,]
  a$d = a$d*sd(Results$diff)
  a$e = a$e*sd(Results$cum)
  a$f = a$f*sd(Results$cum*Results$gdp,na.rm = T)
  a$b = a$b*sd(Results$diff*Results$gdp,na.rm = T)
  a$g = a$g*sd(Results$cum_diff)
  coefs[name,1:6] = untable(a[c(2,7,8,10,11,13)])
}
coefs$AIC = round(aic[names],1)
coefs$BIC = round(bic[names],1)
coefs = round(coefs,3) %>% mutate_all(as.character)
coefs[coefs==0] = ""
xtable(coefs)


#Table S3
perf = data.frame(aic,bic,max_loglik) %>% round(2)
xtable(perf)


"%contain%" <- function(values,x) {
  tx <- table(x)
  tv <- table(values)
  z <- tv[names(tx)] - tx
  all(z >= 0 & !is.na(z))
}

p_values = data.frame(matrix(nrow=length(names)-1,ncol= length(names)-1))
colnames(p_values) = names[-1]
rownames(p_values) = names[-length(names)]
for(name_1 in names[-length(names)]){
  for(name_2 in names[-1]){
    if(name_1==name_2){next}
    if(unused_parameters[name_1] %contain% unused_parameters[name_2]){
      p=pchisq(max_loglik[name_2]-max_loglik[name_1],length(unused_parameters[[name_1]])-length(unused_parameters[[name_2]]))
      print(str_c(name_2," vs ",name_1,": p=",1-p))
      star = stars.pval(1-p)
      p = as.character(round(1-p,3))
      if(p=="0"){p = "< 10^{-6}"}
      p_values[name_1,name_2] = paste(p,star)
    }
  }
}

p_values[is.na(p_values)] = ""
xtable(p_values)




z = vector("numeric")
for(country in Countries){z = c(z,untable(d[[1]][country]@unit.objects[[1]]@data))}

z = c()
for(i in 1:length(a)){z[i] = logLik(a[[i]])}


