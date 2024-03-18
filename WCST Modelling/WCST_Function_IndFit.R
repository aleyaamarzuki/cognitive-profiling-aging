require(rjags)
require(coda)
require(MCMCvis)
library(Rcpp)  
library(data.table)
library(ggplot2)
library(lme4)
library(lmerTest)
library(parallel)
library(plyr)
library(readr)
library(tidyr)
library(HDInterval)
#library(jagsUI)
library(R2jags)
library(bridgesampling)
library(dplyr)

rm(list=ls())

#### Function for Running Models ####

#instructions: type run_wcst_models("rpdf") for example in Console to run model rpdf

run_wcst_models <- function(mdl) {
  
  ## set working directory and read in data
  outputdir<-'C:/Users/aleya/Documents/Tasks/Scripts/WCST IndFit/'
  setwd('C:/Users/aleya/Documents/Tasks/WCST/')
  
  
  data<-read.table('wcst_data.txt', header=TRUE,sep=",")
  data <- data.table(data)
  
  #subset data for now
  #data <- data[(data$subnum == 2 | data$subnum == 5),]
  
  ## prepare data for jags ##
  data$patient <- as.factor(data$subnum)
  
  all_patients_map <- data.table(
    patient=sort(unique(data$patient))
  )
  all_patients_map[, patientseqnum := order(patient)]
  # all_patients_map
  data[all_patients_map, patientseqnum := patientseqnum,
       on = c(patient = "patient")]
  
  
  trial_per_subj<-plyr::count(data$subnum)
  trial_per_subj<-trial_per_subj$freq
  
  
  #### prepare matching matrix for jags, should be in the format (n_subjects x ntrials x 1 x 3)
  library(tidyverse)
  deck_match_rule<-xtabs(cbind(corr_col,corr_shape,corr_num) ~ patient+trial, data)
  
  # make 'one' to model stochastically in model
  one <- matrix(1,nrow=length(unique(data$subnum)), ncol = 128)
  
  
  #arrange feedback
  outcome<-xtabs(corr ~ patient + trial,data)
  
  # get sub numbers and total subjects
  subj <- unique(data$subnum)
  N_SUBJECTS <- as.numeric(length(subj))
  
  # define parameters and initial values per model

  #initialise variables
  nchain <- 4
  init <- list()
  
  # if statements
  if (mdl == "rpdf" ) {
    params <- c("r", "p", "d", "f", "postpredmatch")
   for (i in 1:nchain) {
     
    r = rbeta(1,1,1)
    p = rbeta(1,1,1)
    d.prime = rbeta(1,1,1)
    f.prime = rbeta(1,1,1)
    
    init[[i]] <- list(r = r, p = p, d.prime = d.prime, f.prime = f.prime)
   }
  };
  
  if (mdl == "rpd1" | mdl == "rpd0") {
    params <- c("r", "p", "d", "postpredmatch")
    
    for (i in 1:nchain) {
      
      r = rbeta(1,1,1)
      p = rbeta(1,1,1)
      d.prime = rbeta(1,1,1)
      
      init[[i]] <- list(r = r, p = p, d.prime = d.prime)
    }
  }
  
  if (mdl == "rp1f") {
    params <- c("r", "p", "f", "postpredmatch")
    
    for (i in 1:nchain) {
      
      r = rbeta(1,1,1)
      p = rbeta(1,1,1)
      f.prime = rbeta(1,1,1)
      
      init[[i]] <- list(r = r, p = p, f.prime = f.prime)
    }
    
  }

  if (mdl == "rrdf") {
    params <- c("r", "d", "f", "postpredmatch")
    for (i in 1:nchain) {
      
      r = rbeta(1,1,1)
      d.prime = rbeta(1,1,1)
      f.prime = rbeta(1,1,1)
      
      init[[i]] <- list(r = r, d.prime = d.prime, f.prime = f.prime)
    }
  }
  
 # initialise variables for storage
  
  # to store model
  samps <- vector(mode = "list", length = N_SUBJECTS)
  
  # to store deviance
  deviance <- vector("numeric", N_SUBJECTS)
  estimParam <- vector("numeric", N_SUBJECTS)
  pdeviance <- vector("numeric", N_SUBJECTS)
  
  for (s in 1:N_SUBJECTS) {
    
    # prepare all data for jags
    jagsdata <- list(
      
      N_TRIALS = trial_per_subj[s],  # TOTAL number of trials per participant
      deck_match_rule=deck_match_rule[s,,], #equivalent to matching matrix (m) in matlab script
      outcome = outcome[s,],
      one = one[s,]
    )
    
    #print current subject ID
    cat("\n----- Modelling Subject",subj[s],"-----\n")
    
    # run model 
    samps[[s]] <- jags(jagsdata, inits=init, params,
                            model.file = paste("C:/Users/aleya/Documents/Tasks/Scripts/WCST_IndFit/",mdl,"_indFit.txt",sep=""), 
                            n.chains=4, n.iter=2000, 
                            n.burnin=500, n.thin=1, DIC=T)
    
    #print current subject ID
    cat("\n----- Calculating pDIC Subject",subj[s],"-----\n")
    
    pDIC<-dic.samples(samps[[s]]$model, n.iter=1000, type="pD")
    
    #estimated no. parameters
    estimParam[s] <- samps[[s]]$BUGSoutput$pD #store penalised DIC per subject
    
    #normal DIC
    deviance[s] <- sum(pDIC$deviance) #store DIC per subject
    
    #penalised DIC
    pdeviance[s] <- sum(pDIC$penalty)+sum(pDIC$deviance) #store DIC per subject
    
  } #Subject loop end
  
  
  #get sum of DIC
  cat("\n----- Total DIC -----\n")
  print(sum(deviance))
  #get mean estimated parameters 
  cat("\n----- Mean Estim Parameters -----\n")
  print(mean(estimParam))
  #get penalised DIC
  cat("\n----- Total Penalised DIC -----\n")
  print(sum(pdeviance))
  
  
  #save DIC results
  dev <- data.frame(matrix(ncol = 3, nrow = N_SUBJECTS))
  
  #provide column names
  colnames(dev) <- c('Deviance', 'pDeviance', 'EstParam')
  
  dev$Deviance<-deviance
  dev$pDeviance<-pdeviance
  dev$EstParam<-estimParam
  
  save(samps, file = paste("samps_", mdl, "_ind.RData", sep=""))
  save(dev, file = paste("devResults_", mdl, ".RData", sep=""))
  

} # function end

#### Compare DIC ####

models<-c("rpdf", "rpd0", "rp1f", "rrdf", "rpd1")
#models<-c("rpdf", "rpd0", "rp1f")

#initialise data frame
summary_dic <- data.frame(matrix(ncol = 4, 
                                 nrow = length(models)))

summary_dic[1:length(models),1]<-models

x <- c("Model", "Deviance", "Estimated Parameters", "pDeviance")
colnames(summary_dic) <- x

for (m in 1:length(models)) {

load(paste("C:/Users/aleya/Documents/Tasks/WCST/devResults_", models[m] ,".RData", sep = ""))

sum <- c(sum(dev$Deviance),
                    mean(dev$EstParam),
                    sum(dev$pDeviance))

summary_dic[m,2:4]<-sum

}

write.csv(summary_dic,"C:/Users/aleya/Documents/Tasks/WCST/SummaryDIC.csv")


#### Extract subject parameters ####

#load saved JAGS results (winning model)
setwd('C:/Users/aleya/Documents/Tasks/WCST/')
load("samps_rpdf_ind.RData")

# read in raw data
data<-read.table('wcst_data.txt', header=TRUE,sep=",")
data <- data.table(data)

# get sub numbers and total subjects
subj <- unique(data$subnum)
N_SUBJECTS <- as.numeric(length(subj))

# get subject level parameters ##

subject_level_parameters <- data.frame(matrix(ncol = 5, 
                                 nrow = N_SUBJECTS))

x <- c("Participants", "r", "p", "d", "f")
colnames(subject_level_parameters) <- x
subject_level_parameters$Participants<-subj

for (k in 1:N_SUBJECTS){
subject_level_parameters[k,2]<-samps[[k]]$BUGSoutput$mean$r
subject_level_parameters[k,3]<-samps[[k]]$BUGSoutput$mean$p
subject_level_parameters[k,4]<-samps[[k]]$BUGSoutput$mean$d
subject_level_parameters[k,5]<-samps[[k]]$BUGSoutput$mean$f
}

# save output
write.csv(subject_level_parameters, file="subject_level_parameters.csv")



