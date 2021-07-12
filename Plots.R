library(ggplot2)
library(dplyr)
library(scales)
library(grid)

setwd("~/Code_scientific_revolution")
Data = read.csv("data_scientists.csv",stringsAsFactors = F) 
Population = read.table("Population.csv",row.names = 1,sep=",",header = T,check.names = F,stringsAsFactors = F)
Countries<-c("Netherlands","France","Germany","United Kingdom","Belgium","Russia","Iberia","Italy","Poland","Scandinavia")
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
  
  
plot_results = function(Results){
  ggplot(Results,aes(Date,Nobs_log,col=Countries)) +
    geom_point(size=1,alpha=0.4,aes(col=Countries)) + 
    geom_smooth(span=0.7,se = FALSE) + theme_classic() +  
    theme(legend.title = element_blank(),axis.ticks = element_line(colour = "grey")) + 
    guides(color=guide_legend(override.aes=list(fill=NA))) + ylab("") + xlab("") + 
    scale_x_continuous(expand=c(0,0)) + theme(plot.title = element_text(hjust = 0.5)) +
    scale_y_continuous(limit=c(0,NA),oob=squish)
  
}


Results = index(Data)
h = 3.6
a = ggplot(Results,aes(Date,Nobs_log,col=Countries)) +
  geom_point(size=1,alpha=0.4,aes(col=Countries)) + 
  geom_smooth(span=0.7,se = FALSE) + theme_classic() +  
  theme(legend.title = element_blank(),axis.ticks = element_line(colour = "grey")) + 
  guides(color=guide_legend(override.aes=list(fill=NA))) + ylab("") + xlab("") + 
  scale_x_continuous(expand=c(0,0)) + theme(plot.title = element_text(hjust = 0.5)) + 
  geom_rect(data=data.frame(matrix(nrow=3)),xmin=c(1300,1550,1700),xmax=c(1550,1700,1850),ymin=c(-Inf,-Inf,-Inf),ymax=c(Inf,Inf,Inf),inherit.aes=F,fill=c("green","blue","red"),alpha=.04) + 
  annotate("text",x=1772,y=h+.2,label = "British economic",size=5) + 
  annotate("text",x=1777,y=h,label = "leadership",size=5) + 
  annotate("text",x=1625,y=h+.2,label = "Dutch economic",size=5) + 
  annotate("text",x=1630,y=h,label = "leadership",size=5) + 
  annotate("text",x=1425,y=h+.2,label = "Italian economic",size=5) + 
  annotate("text",x=1430,y=h,label = "leadership",size=5) + 
  theme(plot.title = element_text(size=22)) + 
  theme(axis.text.x = element_text(size=11),axis.text.y = element_text(size=11)) + scale_y_continuous(limit=c(0,NA),oob=squish)
print(a)
ggsave("plots/BBS.png")


Data$rank = rank(Data$pca)
Results = index(filter(Data,rank>nrow(Data)*.9))
plot_results(Results)
ggsave("plots/top10.png")
Results = index(filter(Data,rank<nrow(Data)*.9))
plot_results(Results)
ggsave("plots/bottom90.png")



Data_north = Data
z = which(Data$Countries %in% c("Belgium","Netherlands","United Kingdom"))
Data_north$Countries[z] = "Northwestern Europe"
Population["Northwestern Europe",] = colSums(Population[c("Belgium","Netherlands","United Kingdom"),])
Results = index(Data_north,countries = c("Northwestern Europe","France","Italy","Germany","Scandinavia","Iberia","Poland","Russia"))
plot_results(Results)
ggsave("plots/north.png")


Results = index(Data,per_capita = FALSE)
plot_results(Results)
ggsave("plots/brut.png")

Results = index(filter(Data,Occupation=="scientist"))
plot_results(Results)
ggsave("plots/scientists_only.png")

Data$century = as.character(as.numeric(Data$Birth_dates)+100)
Data$century = substring(Data$century,1,2)
Data$century = paste(Data$century,"th-century",sep="")
a = ggplot(filter(Data,Birth_dates %in% 1500:1799),aes(Countries)) + geom_bar() + 
  facet_grid(~century,scales="free_x") + theme_minimal() + coord_flip() +
  theme(axis.ticks = element_blank(),strip.background = element_blank(),panel.grid.minor.y =element_blank(),panel.grid.major.y =element_blank(),panel.grid.minor = element_blank(),panel.grid.major = element_blank()) + ylab("Scientists numbers")
ggsave("plots/Hist.png")

Data_cultural = read.csv("data_all.csv")
Results = index(Data = Data_cultural,1300)
plot_results(Results)
ggsave("plots/cultural.png")

Protestant = c("Germany","Scandinavia","Netherlands","United Kingdom")
Catholic = c("France","Italy","Iberia","Poland","Belgium")
Data_protestant = Data
z = which(Data$Countries %in% Protestant)
Data_protestant$Countries[z] = "Protestant countries"
z = which(Data$Countries %in% Catholic)
Data_protestant$Countries[z] = "Catholic countries"
Population["Catholic countries",] = colSums(Population[Catholic,])
Population["Protestant countries",] = colSums(Population[Protestant,])
Results = index(Data_protestant,countries = c("Protestant countries","Catholic countries"))
plot_results(Results)
ggsave("plots/protestant.png")

proxies = colnames(Data)[7:15]
Results = index(Data)
for(proxy in proxies){
  Results[proxy] = log(index(Data,proxy = proxy)$Nobs+1)
}
library(tidyr)
Results = Results[-c(3:5)]
Results = gather(Results,"key","value",-Date,-Countries)
Results = separate(Results,key,c("Proxy","Language"),sep="_")
ggplot(Results,aes(Date,value,col=Countries)) + geom_point(size=.4) + geom_smooth(span=.7,se=F) + facet_grid(Language~Proxy)+ theme_classic() +  
  theme(legend.title = element_blank(),axis.ticks = element_line(colour = "grey")) + 
  guides(color=guide_legend(override.aes=list(fill=NA))) + ylab("") + xlab("") + 
  scale_x_continuous(expand=c(0,0)) + theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limit=c(0,NA),oob=squish) + theme(panel.spacing.x = unit(1, "lines"))
ggsave("plots/panel.png")

