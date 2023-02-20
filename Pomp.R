library(tidyr)
library(panelPomp)
library(reshape)
library(rslurm)
library(doParallel)
library(dplyr)
library(foreach)
analysis = "MLE_with_sigma2"
options =  list("secondgen","benoit2c@gmail.com","ALL")
names(options) = c("partition","mail-user","mail-type")
pkg = c("panelPomp")
cpus = c(12,12,16,16,40,48)
names(cpus) = c("firstgen","fastgen","secondgen","lastgen","dellgen","gpu")
#partitions = c(rep("dellgen",7),"gnt")
##Importations
Countries<-c("Italy","France","Poland","Germany","United Kingdom","Iberia","Russia","Belgium","Netherlands","Scandinavia")
Results = read.csv("Results.csv",row.names = 1)
Results$Nobs = log(Results$Nobs+1)
Results$Nobs = Results$Nobs/sd(Results$Nobs)
Results$gdp = Results$gdp - min(Results$gdp,na.rm=T)
Results$protestant = scale(Results$protestant)
Distances = read.csv("Distances.csv",row.names = 1)

#Create the diffusion matrix
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
  print(i)
}

Csnippet("
         N = N_0;
         ") -> rinit
Csnippet("
         double eps_obs = rnorm(0,pow(sigma_obs,2));
         Nobs = N+eps_obs;
         ") -> rmeas
Csnippet("
         lik = dnorm(Nobs,N,pow(sigma_obs,2),give_log);
         ") -> dmeas
Csnippet("double eps = fmax(rnorm(1,pow(sigma,2)),0);
         double eps2 = rnorm(0,pow(sigma2,2));
         N = z*N*eps + e*cum + f*gdp*cum + a*gdp  + d*diff + b*gdp*diff + g*cum_diff + c + eps2;
         ") -> evol_diff

eval = function(i){
  mf = mifs[[i]]
  replicate(5000, mf %>% panelPomp::pfilter(Np = 20000) %>% panelPomp::logLik()) %>% logmeanexp(se=TRUE) -> ll
  return(c(coef(mf),loglik=ll[1],loglik.se=ll[2]))
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
#unused_parameters[[12]] = c(1,3,5,6,7,13)
names(unused_parameters) = names

#names = c("L")
names = c("B","D",'E','F','G','H','I','J','K')
submit_job <- function(nmif=10000,np=20000,
                       cooling_fraction=.95,n=200){
  Pomps = list()
  for(country in Countries){
    data = dplyr::filter(Results,Countries==country)
    t0=1300
    if(country=="Russia"){
      data = dplyr::filter(data,Date>=1500)
      t0=1500
    }
    z = which(!is.na(data$gdp))
    Gdp = data[,c("Date","gdp")]
    Gdp$diff = rowSums(R/untable(Distances[country,],12)**2)[as.character(data$Date)]
    data = data[c("Date","Nobs")]
    Gdp$cum = vector("numeric",length = nrow(Gdp))
    Gdp$cum[2:nrow(Gdp)] = cumsum(data$Nobs)[1:nrow(Gdp)-1]
    Gdp$cum_diff = cumsum(Gdp$diff)
    
    data %>%
      pomp(
        times="Date", t0=t0,rinit=rinit,
        covar = covariate_table(Gdp, times= "Date"),
        rprocess=discrete_time(evol_diff,delta.t = 50),
        dmeasure = dmeas,obsnames = c("Nobs"),
        statenames=c("N"),rmeasure=rmeas,
        paramnames=PARAM[-1],covarnames = c("gdp","protestant","diff","cum","cum_diff")
      ) -> Pomps[[country]]}
  p = rep(0,length(PARAM[-1]))
  names(p) = PARAM[-1]
  Model_diff = panelPomp(Pomps,shared = p)
  lower = c(a=.05,b=-.5,c=0,d=-.5,e=-.2,f=-.1,z=1,sigma=.8,sigma_obs=.07,N_0 = -.1,sigma2=.1,g=-.1,h=-.1)
  upper = c(a=.1,b=.5,c=0,d=.5,e=.2,f=.1,z=1.3,sigma=1.1,sigma_obs=.15,N_0 = .1,sigma2=.3,g=-.1,h=.1)
  lower[unused_parameters[[name]]-1] = 0
  upper[unused_parameters[[name]]-1] = 0
  sobolDesign(lower = lower[PARAM[-1]], upper = upper[PARAM[-1]], nseq = n) -> guesses
  rwsd = rw.sd(a=.1,b=.1,c=.1,d=.1,e=.1,f=.1,z=.2,sigma=.15,sigma_obs=.15,N_0 = ivp(.1),sigma2=.1,g=.1,h=.1)
  rwsd@call[PARAM[unused_parameters[[name]]][-1]] = 0
  Model_diff %>%
    panelPomp::mif2(
      shared.start=unlist(guesses[6,]),
      specific.start = Model_diff@specific,
      Np=np,
      Nmif=3,
      cooling.fraction.50=cooling_fraction,
      cooling.type="geometric",
      rw.sd= rwsd,
      pars = PARAM
    ) -> mf1
  mif3 <- function(a,sigma,N_0,sigma_obs,z,d,c,b,e,f,sigma2,g,h){
    k = panelPomp::mif2(mf1,Nmif = nmif,shared = c(a = a,sigma = sigma,N_0 = N_0,sigma_obs = sigma_obs,z = z,d = d,b=b,c=c,e=e,f=f,sigma2=sigma2,g=g,h=h), specific = Model_diff@specific)
    return(k)
  }
  
  k = slurm_apply(f = mif3,params = guesses,jobname = paste(name,analysis,sep="_"),nodes = 15,cpus_per_node = cpus[options[[1]]],pkgs = pkg,slurm_options = options,add_objects = c("mf1","Model_diff"))
  return(k)
}

for(name in names){
  job[[name]] = submit_job()
}

for(name in names){
  mifs_pomp[[name]] = get_slurm_out(job[[name]],wait = TRUE)
  #cleanup_files(job[[name]])
  file1 = paste(paste("mifs_pomp_",analysis,sep="_"),name,sep="")
  file1 = paste(file1, ".RDS",sep="")
  saveRDS(mifs_pomp[[name]],file1)
}


for(name in names){
  mifs = mifs_pomp[[name]]
  d = data.frame(i = 1:length(mifs))
  job2[[name]] = slurm_apply(f = eval,params = d,jobname = paste(paste("ev",name,sep="_"),analysis,sep="_"),nodes = 15,cpus_per_node = cpus[options[[1]]],pkgs = pkg,slurm_options = options,add_objects = c("mifs"))
}

for(name in names){
  estimates = get_slurm_out(job2[[name]], outtype = "table",wait=TRUE)
  #cleanup_files(job2[[name]])
  file2 = paste(paste("estimates_",analysis,sep="_"),name,sep="")
  file2 = paste(file2, ".csv",sep="")
  write.csv(estimates,file2)
}


