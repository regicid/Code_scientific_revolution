library(dplyr)
library(stringr)
library(spaMM)
library(gtools)
library(xtable)
setwd("~/Code_scientific_revolution/")
Population = read.table("Population.csv",row.names = 1,sep=",",header = T,check.names = F,stringsAsFactors = F)
Data = read.table("data_scientists.csv",row.names = 1,sep=",",header = T,check.names = F,stringsAsFactors = F)
Data_cultural = list()
disciplines = c("scientists","composers","painters","philosophers","sculptors","writers","all")
for(d in disciplines){Data_cultural[[d]] = read.table(str_c("data_",d,".csv"),row.names = 1,sep=",",header = T,check.names = F,stringsAsFactors = F)}

Countries<-c("Netherlands","France","Germany","United Kingdom","Belgium","Russia","Iberia","Italy","Poland","Scandinavia")


Envir_var = list()
GDP = read.table("Environment_variables/GDP_pc.csv",row.names = 1,sep=",",header = T,check.names = F)
Envir_var$gdp = GDP
Parliament = read.table("Environment_variables/Parliament.csv",row.names = 1,sep=",",header = T,check.names = F)
Envir_var$parliament = Parliament
Urb = read.table("Environment_variables/Urbanization.csv",row.names = 1,sep=",",header = T,check.names = F)
Envir_var$urb = Urb
Univ = read.table("Environment_variables/Univ.csv",row.names = 1,sep=",",header = T,check.names = F)
Envir_var$univ_pc = Univ/Population[rownames(Univ),colnames(Univ)]
Envir_var$population = Population



Dates = 1300+50*0:11
#Dates = 1300+100*0:

Var = c(names(Envir_var),"Protestant")

index = function(Data,start=1300,end=1850,countries=Countries,per_capita=TRUE,gap=10,Var = NULL,proxy="pca"){
  Results = data.frame(matrix(ncol=3,nrow=0))
  colnames(Results) = c("Date","Countries","Nobs")
  Dates = seq(start,end,gap)
  for(i in 1:length(Dates)){
    for(j in 1:length(countries)){
      n = nrow(Results)
      Results[n+1,] = NA
      Results[n+1,"Date"] = Dates[i]
      Results[n+1,"Countries"] = countries[j]
      m = which(Data$Countries == countries[j] & Data$Birth_dates %in% (Dates[i]-60):(Dates[i]-11))
      if(per_capita){Results[n+1,"Nobs"] = sum(Data[m,proxy],na.rm = T)/Population[countries[j],as.character(Dates[i])] 
      }else{Results[n+1,"Nobs"] = sum(Data[m,proxy],na.rm = T)}
      for(var in Var){
        z = Envir_var[[var]][countries[j],as.character(Dates[i])]
        if(length(z)>0){Results[n+1,var] = z}}
      if(Dates[i]> 1500 & countries[j] %in% Protestant){
        Results[n+1,"protestant"] = 1
      }else{Results[n+1,"protestant"] = 0}
      
      
    }
  }
  Results$Nobs = Results$Nobs/sd(Results$Nobs)
  for(var in Var){Results[var] = scale(Results[var])}
  Results$Nobs_log = log(Results$Nobs+1)
  Results$Nobs_log = Results$Nobs_log/sd(Results$Nobs_log)
  Results$date = scale(Results$Date)
  return(Results)
}

disciplines = c("scientists","composers","painters","philosophers","writers","sculptors","all")
coef = data.frame(matrix(nrow=length(disciplines),ncol=length(Var)))
Var = c("population","gdp","urb","univ_pc","parliament","protestant")
colnames(coef) = Var
rownames(coef) = disciplines
for(discipline in disciplines){
  Results = index(Data_cultural[[discipline]],gap=50,start = 1500,Var = Var)
  if(discipline=="scientists"){Results = index(Data_cultural[[discipline]],gap=50,start = 1300,Var = Var)}
  Results$Date = as.numeric(as.factor(Results$Date))
  for(var in Var){
    fm <- as.formula(paste("Nobs_log~AR1(1|Date %in% Countries) + date +", var))
    a = summary(fitme(data=Results,fm),details=c(p_value="Wald"))
    coef[discipline,var] = paste(as.character(round(a$beta_table[3,1],2)),stars.pval(a$beta_table[3,4])[1])
  }
}
library(xtable)
xtable(coef)

library(strucchange)
Data_break = Data
Data_break$Countries = "Europe"
Results = index(Data_break,countries = "Europe",1300,1650)
breakpoints(data = Results,Nobs~Date,breaks = 1)
plot(Fstats(data = Results,Scientific_production~Date))

Data_protestant = Data
Protestant = c("Germany","Scandinavia","Netherlands","United Kingdom")
Catholic = c("France","Italy","Iberia","Poland","Belgium")
z = which(Data$Countries %in% Protestant)
Data_protestant$Countries[z] = "Protestant countries"
z = which(Data$Countries %in% Catholic)
Data_protestant$Countries[z] = "Catholic countries"
Population["Catholic countries",] = colSums(Population[Catholic,])
Population["Protestant countries",] = colSums(Population[Protestant,])
Results = index(Data_protestant,countries = c("Protestant countries","Catholic countries"),1300,1650)

plot_results(Results)
z = filter(Results,Countries=="Protestant countries")$Nobs - filter(Results,Countries=="Catholic countries")$Nobs
Diff = data.frame(diff=z,Date = unique(Results$Date))
ggplot(Diff,aes(Date,diff)) + geom_point() + geom_smooth(span=.7)
breakpoints(data = Diff,diff~Date,breaks = 1)
plot(Fstats(data = Diff,diff~Date))




disciplines = names(Data_cultural)[1:6]
coef = data.frame(matrix(nrow=length(disciplines),ncol=length(disciplines)))
rownames(coef) = disciplines
colnames(coef) = disciplines

for(d in disciplines){
  for(d2 in disciplines){
    if(d==d2){next}
    a = index(Data_cultural[[d]],gap=50,1500)
    a$Nobs_log2 = index(Data_cultural[[d2]],gap=50,1500)$Nobs_log
    a$Date = as.numeric(as.factor(a$Date))
    a = summary(fitme(data=a,Nobs_log2~Nobs_log+AR1(1|Date %in% Countries) + date),details=c(p_value="Wald"))
    coef[d,d2] = paste(as.character(round(a$beta_table[2,1],2)),stars.pval(a$beta_table[2,4])[1])
  }   
}
xtable(coef)
Results = index(Data,gap=50,1500,Var="gdp")
for(d in disciplines){Results[d] = index(Data_cultural[[d]],gap=50,1500)$Nobs_log}
library(FactoMineR)
pca = PCA(Results[7:12],scale.unit = T)

