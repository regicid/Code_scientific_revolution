library(ggplot2)
library(dplyr)
library(scales)

setwd("~/Code_scientific_revolution")
Data = read.csv("data_scientists.csv") 
Population = read.csv("Population.csv")
Countries<-c("Netherlands","France","Germany","United Kingdom","Belgium","Russia","Iberia","Italy","Poland","Scandinavia")
index = function(Data,start=1300,end=1850,countries=Countries,per_capita=TRUE,gap=10){
  Results = data.frame(matrix(ncol=3,nrow=0))
  colnames(Results) = c("Date","Countries","Scientific_production")
  Dates = seq(start,end,gap)
  for(i in 1:length(Dates)){
    for(j in 1:length(countries)){
      n = nrow(Results)
      Results[n+1,] = NA
      Results[n+1,"Date"] = Dates[i]
      Results[n+1,"Countries"] = countries[j]
      #Results[n+1,"gdp"] = GDP[countries[j],as.character(Dates[i])]
      m = which(Data$Countries == countries[j] & Data$Birth_dates %in% (Dates[i]-60):(Dates[i]-11))
      if(per_capita){Results[n+1,"Scientific_production"] = sum(Data[m,"pca"])/Population[countries[j],as.character(Dates[i])] 
      }else{Results[n+1,"Scientific_production"] = sum(Data[m,"pca"])}
    }
  }
  Results$Scientific_production = Results$Scientific_production/sd(Results$Scientific_production)
  return(Results)
}
  
  
plot_results = function(Results){
  ggplot(Results,aes(Date,Scientific_production,col=Countries)) +
    geom_point(size=1,alpha=0.4,aes(col=Countries)) + 
    geom_smooth(span=0.7,se = FALSE) + theme_classic() +  
    theme(legend.title = element_blank(),axis.ticks = element_line(colour = "grey")) + 
    guides(color=guide_legend(override.aes=list(fill=NA))) + ylab("") + xlab("") + 
    scale_x_continuous(expand=c(0,0)) + theme(plot.title = element_text(hjust = 0.5)) +
    scale_y_continuous(limit=c(0,NA),oob=squish)
  
}


Results = index(Data,1500)
a = ggplot(Results,aes(Date,Scientific_production,col=Countries)) +
  geom_point(size=1,alpha=0.4,aes(col=Countries)) + 
  geom_smooth(span=0.7,se = FALSE) + theme_classic() +  
  theme(legend.title = element_blank(),axis.ticks = element_line(colour = "grey")) + 
  guides(color=guide_legend(override.aes=list(fill=NA))) + ylab("") + xlab("") + 
  scale_x_continuous(expand=c(0,0)) + theme(plot.title = element_text(hjust = 0.5)) + 
  geom_rect(data=prosp,xmin=c(1550,1700),xmax=c(1700,1850),ymin=-Inf,ymax=+Inf,inherit.aes=F,fill=c("red","blue"),alpha=0.1) + 
  annotate("text",x=1770,y=4.2,label = "UK prosperity",size=5) + 
  annotate("text",x=1625,y=4.2,label = "Netherlands prosperity",size=5) + 
  #annotate("text",x=1625,y=3.8,label = "prosperity",size=5) + 
  theme(plot.title = element_text(size=22)) + 
  theme(axis.text.x = element_text(size=11),axis.text.y = element_text(size=11)) + scale_y_continuous(limit=c(0,NA),oob=squish)
print(a)
ggsave("plots/BBS.png")


Data$rank = rank(Data$pca)
Results = index(filter(Data,rank>nrow(Data)*.9),1500)
plot_results(Results)
Results = index(filter(Data,rank<nrow(Data)*.9),1500)
a = plot_results(Results)
print(a)

Data_north = Data
z = which(Data$Countries %in% c("Belgium","Netherlands","United Kingdom"))
Data_north$Countries[z] = "Northwestern Europe"
Results = index(Data_north,1500,countries = c("Northwestern Europe","France","Italy","Germany","Scandinavia","Iberia","Poland","Russia"))
plot_results(Results)

Results = index(Data,1500,per_capita = FALSE)
a = plot_results(Results)
ggsave("plots/Results_brut.png")

Results = index(filter(Data,Occupation=="scientist"),1500)
a = plot_results(Results)
ggsave("plots/Results_scientists_only.png")

Data$century = as.character(as.numeric(Data$Birth_dates)+100)
Data$century = substring(Data$century,1,2)
Data$century = paste(Data$century,"th-century",sep="")
a = ggplot(filter(Data,Birth_dates %in% 1500:1799),aes(Countries)) + geom_bar() + 
  facet_grid(~century,scales="free_x") + theme_minimal() + coord_flip() +
  theme(axis.ticks = element_blank(),strip.background = element_blank(),panel.grid.minor.y =element_blank(),panel.grid.major.y =element_blank(),panel.grid.minor = element_blank(),panel.grid.major = element_blank()) + ylab("Scientists numbers")
ggsave("plots/Hist.png")

Data_cultural = read.csv("data_cultural.csv")
Results = index(Data = Data_cultural,1500)
plot_results(Results)
Data = read.csv("data_scientists.csv")
Results2 = index(Data,1500)
plot_results(Results)

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
