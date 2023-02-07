library(ggplot2)
library(dplyr)
library(scales)
library(grid)

setwd("~/Code_scientific_revolution")
Data = read.csv("data_scientists.csv",stringsAsFactors = F) 
Population = read.table("Population.csv",row.names = 1,sep=",",header = T,check.names = F,stringsAsFactors = F)
Countries<-c("Netherlands","France","Germany","United Kingdom","Belgium","Russia","Iberia","Italy","Poland","Scandinavia")
Protestant = c("Germany","Scandinavia","Netherlands","United Kingdom")
Catholic = c("France","Italy","Iberia","Poland","Belgium")

Colors= c('#1f78b4','#a6cee3','#6a3d9a','#cab2d6','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#b2df8a','#33a02c')
ma <- function(x, n = 5){stats::filter(x, rep(1 / n, n), sides = 2)}
index = function(Data,start=1300,end=1850,countries=Countries,per_capita=TRUE,gap=10,Var = NULL,proxy="pca"){
  Results = data.frame(matrix(ncol=4,nrow=0))
  colnames(Results) = c("Date","Countries","Nobs","Total")
  Dates = seq(start,end,gap)
  for(i in 1:length(Dates)){
    for(j in 1:length(countries)){
      n = nrow(Results)
      Results[n+1,] = NA
      Results[n+1,"Date"] = Dates[i]
      Results[n+1,"Countries"] = countries[j]
      m = which(Data$Countries == countries[j] & Data$Birth_dates %in% (Dates[i]-35-gap/2):(Dates[i]-36+gap/2))
      if(per_capita){Results[n+1,"Nobs"] = sum(Data[m,proxy],na.rm = T)/Population[countries[j],as.character(Dates[i])] 
      }else{Results[n+1,"Nobs"] = sum(Data[m,proxy],na.rm = T)}
      m = which(Data$Birth_dates %in% (Dates[i]-35-gap/2):(Dates[i]-36+gap/2))
      if(per_capita){Results[n+1,"Total"] = sum(Data[m,proxy],na.rm = T)/Population["Europe",as.character(Dates[i])] 
      }else{Results[n+1,"Total"] = sum(Data[m,proxy],na.rm = T)}
      for(var in Var){
        z = Envir_var[[var]][countries[j],as.character(Dates[i])]
        if(length(z)>0){Results[n+1,var] = z}}
      if(Dates[i]> 1500 & countries[j] %in% Protestant){
        Results[n+1,"protestant"] = 1
      }else{Results[n+1,"protestant"] = 0}
      
      
    }
  }
  Results$Nobs = Results$Nobs/sd(Results$Nobs,na.rm = TRUE)
  for(var in Var){Results[var] = scale(Results[var])}
  Results$Nobs_log = log(Results$Nobs+1)
  Results$Nobs_log = Results$Nobs_log/sd(Results$Nobs_log,na.rm = TRUE)
  Results$date = scale(Results$Date)
  return(Results)
}
  
  
plot_results = function(Results,span=.7,colors = Colors){
  ggplot(Results,aes(Date,Nobs_log,col=Countries)) +
    geom_point(size=1,alpha=0.4,aes(col=Countries)) + 
    geom_smooth(span=span,se = FALSE) + theme_classic() +  
    theme(legend.title = element_blank(),axis.ticks = element_line(colour = "grey"),axis.text.x = element_text(size=15)) + 
    guides(color=guide_legend(override.aes=list(fill=NA))) + ylab("") + xlab("") + 
    scale_x_continuous(expand=c(0,0)) + theme(plot.title = element_text(hjust = 0.5)) +
    scale_y_continuous(limit=c(0,NA),oob=squish) + scale_color_manual(values=colors)
  
}

plot_results_ma = function(Results,n=7){
  Results$Nobs_log_ma = NA
  for(country in unique(Results$Countries)){
    z = which(Results$Countries==country)
    Results$Nobs_log_ma[z] = ma(Results$Nobs_log[z],n=n)
  }
  ggplot(Results,aes(Date,Nobs_log_ma,col=Countries)) + geom_line(size=1) + geom_point(size=.4,aes(Date,Nobs_log)) + theme_classic() +  
    theme(legend.title = element_blank(),axis.ticks = element_blank()  ,axis.line.y = element_blank(),axis.text.y=element_blank()) + 
    guides(color=guide_legend(override.aes=list(fill=NA))) + 
    scale_x_continuous(expand=c(.02,0)) + theme(plot.title = element_text(hjust = 0.5))+ theme(panel.spacing.x = unit(1, "lines"))+
    labs(y="Estimated scientific production",x="Date") + scale_y_continuous(limit=c(0,NA),oob=squish) +
    scale_color_brewer(palette="Paired")
  
}


Results = index(Data)
Data$bla = "Individual scientists"
Results$Countries = factor(Results$Countries,levels=c("United Kingdom","Belgium","Netherlands","Scandinavia","France","Germany","Iberia","Italy","Poland","Russia"))

h = 1.1
a =  ggplot(Results) +geom_smooth(aes(Date,1.3*Nobs_log/max(Nobs_log),col=Countries),span=0.7,se = FALSE) +
  geom_point(data = filter(Data,Birth_dates>1264,Birth_dates<1816),size=.8,alpha=0.2,aes(x=Birth_dates+35,y=pca/max(pca),fill=bla)) + theme_classic() +  
  theme(legend.title = element_blank(),axis.ticks = element_line(colour = "grey")) + 
  guides(color=guide_legend(override.aes=list(fill=NA))) + ylab("") + xlab("") + 
  scale_x_continuous(expand=c(0,0)) + theme(plot.title = element_text(hjust = 0.5)) + 
  #geom_rect(data=data.frame(matrix(nrow=3)),xmin=c(1300,1550,1700),xmax=c(1550,1700,1850),ymin=c(-Inf,-Inf,-Inf),ymax=c(Inf,Inf,Inf),inherit.aes=F,fill=c("orange","purple","blue"),alpha=.08) + 
  annotate("text",x=1768,y=h-.03,label = "British economic leadership",size=2.7,color='#1f78b4') + 
  #annotate("text",x=1777,y=h,label = "leadership",size=6) + 
  annotate("text",x=1621,y=h-.03,label = "Dutch economic leadership",size=2.7,color='#6a3d9a') + 
  #annotate("text",x=1630,y=h,label = "leadership",size=6) + 
  annotate("text",x=1425,y=h-.03,label = "Italian economic leadership",size=2.7,color='#ff7f00') + 
  #annotate("text",x=1430,y=h,label = "leadership",size=6) + 
  annotate("text",x = 1600,y=.94,label="Galilei Galileo",color="black",size=3) +
  annotate("text",x = 1678,y=.98,label="Isaac Newton",color="black",size=3) +
  annotate("text",x = 1680,y=.85,label="Gottfried Wilhelm Leibniz",color="black",size=3) +
  annotate("text",x = 1807,y=.74,label="Charles Darwin",color="black",size=3) +
  annotate("text",x = 1792,y=.59,label="Carl Friedrich Gauss",color="black",size=3) +
  annotate("text",x = 1508,y=.73,label="Nicolaus Copernicus",color="black",size=3) +
  annotate("text",x = 1606,y=.66,label="Johannes Kepler",color="black",size=3) +
  annotate("text",x = 1487,y=.85,label="Leonardo da Vinci",color="black",size=3) +
  annotate("text",x = 1631,y=.89,label="René Descartes",color="black",size=3) +
  annotate("text",x = 1658,y=.6,label="Blaise Pascal",color="black",size=3) +
  theme(plot.title = element_text(size=22)) + 
  geom_segment(x = 1300, y = h, xend = 1550, yend = h,size=.6,color='#ff7f00',
               arrow = arrow(length = unit(0.05, "npc"))) +
  geom_segment(x = 1700, y = h, xend = 1850, yend = h,size=.6,color='#1f78b4',
               arrow = arrow(length = unit(0.05, "npc"))) +
  geom_segment(x = 1550, y = h, xend = 1700, yend = h,size=.6,color='#6a3d9a',
               arrow = arrow(length = unit(0.05, "npc"))) +
  theme(axis.text.x = element_text(size=15),axis.text.y = element_text(size=15),legend.text=element_text(size=12)) + 
  scale_y_continuous(limit=c(0,1.15),oob=squish,expand=c(0,0)) +
  scale_color_manual(values=Colors) + ylab("Estimated scientific production") 
print(a)
ggsave("plots/BBS.png")


Data$rank = rank(Data$pca)
Results = index(filter(Data,rank>nrow(Data)*.9))
Results$Countries = factor(Results$Countries,levels=c("United Kingdom","Belgium","Netherlands","Scandinavia","France","Germany","Iberia","Italy","Poland","Russia"))
plot_results(Results)
ggsave("plots/top10.png")
Results = index(filter(Data,rank<nrow(Data)*.9))
Results$Countries = factor(Results$Countries,levels=c("United Kingdom","Belgium","Netherlands","Scandinavia","France","Germany","Iberia","Italy","Poland","Russia"))
plot_results(Results)
ggsave("plots/bottom90.png")



Data_north = Data
z = which(Data$Countries %in% c("Belgium","Netherlands","United Kingdom"))
Data_north$Countries[z] = "Northwestern Europe"
Population["Northwestern Europe",] = colSums(Population[c("Belgium","Netherlands","United Kingdom"),])
Results = index(Data_north,countries = c("Northwestern Europe","France","Italy","Germany","Scandinavia","Iberia","Poland","Russia"))
Results$Countries = factor(Results$Countries,levels=c("Northwestern Europe","Scandinavia","France","Germany","Iberia","Italy","Poland","Russia"))
plot_results(Results,colors = Colors[c(1,4:10)])
ggsave("plots/north.png")


Results = index(Data,per_capita = FALSE)
Results$Countries = factor(Results$Countries,levels=c("United Kingdom","Belgium","Netherlands","Scandinavia","France","Germany","Iberia","Italy","Poland","Russia"))
plot_results(Results)
ggsave("plots/brut.png")

Results = index(filter(Data,Occupation=="scientist"))
Results$Countries = factor(Results$Countries,levels=c("United Kingdom","Belgium","Netherlands","Scandinavia","France","Germany","Iberia","Italy","Poland","Russia"))
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
Results$Countries = factor(Results$Countries,levels=c("United Kingdom","Belgium","Netherlands","Scandinavia","France","Germany","Iberia","Italy","Poland","Russia"))
plot_results(Results,colors=Colors)
ggsave("plots/cultural.png")

Data_protestant = Data
z = which(Data$Countries %in% Protestant)
Data_protestant$Countries[z] = "Protestant countries"
z = which(Data$Countries %in% Catholic)
Data_protestant$Countries[z] = "Catholic countries"
Population["Catholic countries",] = colSums(Population[Catholic,])
Population["Protestant countries",] = colSums(Population[Protestant,])
Results = index(Data_protestant,countries = c("Protestant countries","Catholic countries"))
plot_results(Results,colors = Colors[c(1,6)])
ggsave("plots/protestant.png")

proxies = colnames(Data)[9:17]
Results = index(Data)
for(proxy in proxies){
  Results[proxy] = log(index(Data,proxy = proxy)$Nobs+1)
}
library(tidyr)
Results = Results[-c(3:7)]
Results = gather(Results,"key","value",-Date,-Countries)
Results = separate(Results,key,c("Proxy","Language"),sep="_")
Results$Countries = factor(Results$Countries,levels=c("United Kingdom","Belgium","Netherlands","Scandinavia","France","Germany","Iberia","Italy","Poland","Russia"))
ggplot(Results,aes(Date,value,col=Countries)) + geom_point(size=.4) + geom_smooth(span=.7,se=F) + facet_grid(Language~Proxy)+ theme_classic() +  
  theme(legend.title = element_blank(),axis.ticks = element_line(colour = "grey")) + 
  guides(color=guide_legend(override.aes=list(fill=NA))) + ylab("") + xlab("") + 
  scale_x_continuous(expand=c(0,0)) + theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limit=c(0,NA),oob=squish) + theme(panel.spacing.x = unit(1, "lines")) + 
  scale_color_manual(values=Colors)
ggsave("plots/panel.png")


Ancient = read.csv("Ancient_scientists.csv",stringsAsFactors = F)
Ancient$bla = "Individual scientists"
Population = read.table("Population_ancient.csv",row.names = 1,sep=",",header = T,check.names = F,stringsAsFactors = F)
Results = index(Ancient,-800,1500,gap=100,countries = c("Arab","Europe","Greece"))
Results$Countries = recode(Results$Countries,"Arab" = "Muslim world","Greece"="Greek world","Europe"="Latin world")
ggplot(Results) +geom_smooth(aes(Date,2*Nobs_log/max(Nobs_log,na.rm=T),col=Countries),span=0.7,se = FALSE) +
  geom_point(data = filter(Ancient,Birth_dates>-800,Birth_dates<1450),size=.8,alpha=0.2,aes(x=Birth_dates+35,y=pca/max(pca),fill=bla)) + theme_classic() +  
  theme(legend.title = element_blank(),axis.ticks = element_line(colour = "grey")) + 
  guides(color=guide_legend(override.aes=list(fill=NA))) + ylab("") + xlab("") + 
  scale_x_continuous(expand=c(0,0)) + theme(plot.title = element_text(hjust = 0.5)) + 
  annotate("text",x = 1000,y=.45,label="Ibn al-Haytham",color="black",size=3) +
  annotate("text",x = 1015,y=.71,label="Avicenna",color="black",size=3) +
  annotate("text",x = 1155,y=.56,label="Hildegard of Bingen",color="black",size=3) +
  annotate("text",x = 1172,y=.63,label="Maimonides",color="black",size=3) +
  annotate("text",x = 58,y=.57,label="Pliny the Elder",color="black",size=3) +
  annotate("text",x = 135,y=.66,label="Ptolemy",color="black",size=3) +
  annotate("text",x = -349,y=.95,label="Aristotle",color="black",size=3) +
  annotate("text",x = -252,y=.62,label="Archimedes",color="black",size=3) +
  annotate("text",x = -545,y=.67,label="Pythagoras",color="black",size=3) +
  annotate("text",x = -392,y=1.03,label="Plato",color="black",size=3) +
  annotate("text",x = 835,y=.485,label="Al-Khwârizmî",color="black",size=3) +
  annotate("text",x = 1161,y=.5,label="Averroes",color="black",size=3) +
  annotate("text",x = 395,y=.475,label="Hypatia",color="black",size=3) +
  annotate("text",x = -500,y=.44,label="Heraclitus",color="black",size=3) +
  annotate("text",x = 1367,y=.44,label="Ibn Khaldun",color="black",size=3) +
  theme(plot.title = element_text(size=22)) + 
  theme(axis.text.x = element_text(size=15),axis.text.y = element_text(size=11),legend.text=element_text(size=12)) + 
  scale_y_continuous(limit=c(0,1.15),oob=squish,expand=c(0,0)) +
  scale_color_manual(values=Colors[c(1,6,10)]) + ylab("Estimated scientific production") 


#plot_results(Results,span=.7,colors = Colors[c(1,6,10)])
ggsave("plots/Antiquity.png")


Population = read.table("Population.csv",row.names = 1,sep=",",header = T,check.names = F,stringsAsFactors = F)
Data_europe = Data
Data_europe$Countries = "Europe"
Results = index(Data_europe,countries = "Europe",per_capita = FALSE)
Results$Nobs_log_ma = ma(Results$Nobs_log)
ggplot(filter(Results,Date>1200),aes(Date,Nobs_log_ma)) + geom_line(aes(col=Countries)) + 
  geom_point(data = filter(Data,Birth_dates>1264,Birth_dates<1816),size=.8,alpha=0.2,aes(x=Birth_dates+35,y=pca/max(pca),fill=bla)) +
  theme_classic() +  theme(legend.title = element_blank()) + 
  guides(color=guide_legend(override.aes=list(fill=NA))) + 
  scale_x_continuous(expand=c(.02,0)) + theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(size=15))+ theme(panel.spacing.x = unit(1, "lines"))+ labs(y="Estimated scientific production",x="Date") + scale_color_manual(values=c("black"))
ggsave("plots/Total_europe_crude.png")

Population["Europe",] = colSums(Population)
Results = index(Data_europe,countries = "Europe",per_capita = TRUE,gap = 10)
Results$Nobs_log_ma = ma(Results$Nobs_log)
Results$Nobs_ma = ma(Results$Nobs)
ggplot(filter(Results,Date>1200),aes(Date,Nobs_log_ma)) + geom_line(aes(col=Countries)) + 
  #geom_point(data = filter(Data,Birth_dates>1264,Birth_dates<1816),size=.8,alpha=0.2,aes(x=Birth_dates+35,y=pca/max(pca),fill=bla)) +
  theme_classic() +  theme(legend.title = element_blank()) + 
  guides(color=guide_legend(override.aes=list(fill=NA))) + 
  scale_x_continuous(expand=c(.02,0)) + theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(size=15))+ theme(panel.spacing.x = unit(1, "lines"))+ labs(y="Estimated scientific production",x="Date") + scale_color_manual(values=c("black"))

ggsave("plots/europe_per_capita.png")


Results$diff[2:nrow(Results)] = diff(Results$Nobs_ma)
Results$diff_ma = ma(Results$diff,3)
ggplot(Results,aes(Date,diff_ma)) + geom_line() + theme_classic() +  
  theme(legend.title = element_blank(),axis.ticks = element_line(colour = "grey")) + 
  guides(color=guide_legend(override.aes=list(fill=NA))) + ylab("") + xlab("") + 
  scale_x_continuous(expand=c(0,0)) + theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(size=15))+ theme(panel.spacing.x = unit(1, "lines"))+
  xlab("Date") + ylab("Estimated scientific production variation")
ggsave("plots/derivative_europe_per_capita.png")
 
    
    
share = function(Data,n=7){
Results = index(Data,per_capita = FALSE)   
Results$Nobs_log_ma = NA
for(country in unique(Results$Countries)){
  z = which(Results$Countries==country)
  Results$Nobs_log_ma[z] = ma(Results$Nobs_log[z],n=n)
}

Results <- Results  %>%
  group_by(Date, Countries) %>%
  summarise(n = sum(Nobs_log_ma)) %>%
  mutate(percentage = n / sum(n))
Results$Countries = factor(Results$Countries,levels=rev(c("United Kingdom","Belgium","Netherlands","Scandinavia","France","Germany","Iberia","Italy","Poland","Russia")))

colors= rev(c('#1f78b4','#a6cee3','#6a3d9a','#cab2d6','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#b2df8a','#33a02c'))
ggplot(filter(Results,!is.na(percentage)),aes(Date,percentage,fill=Countries)) + 
  geom_area(color="black") + ylab("Share of the estimated scientific production") + 
  scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) + 
  scale_fill_manual(values=colors) + theme(axis.text.x = element_text(size=15))}
Results = index(Data)
share(Data)
ggsave("plots/Share.png")


Data_mathematician = filter(Data,Occupation_precise==" mathematician ")
Data_physicist = filter(Data,Occupation_precise%in% c(" physicist "," astronomer "," chemist "))
Data_biologist = filter(Data,Occupation_precise%in% c(" biologist"," entomologist "," naturalist "," lepidopterist ", " anatomist "," botanist "," ornithologist "))



share(filter(Data_mathematician,Birth_dates>1490))
ggsave("plots/Share_mathematician.png")

share(filter(Data_physicist,Birth_dates>1490))
ggsave("plots/Share_physicists.png")

share(filter(Data_biologist,Birth_dates>1490))
ggsave("plots/Share_biologists.png")

