# second part
## site occupancy modeling
## find coral - associated fish

# upper part:
# models in BUGS language
# 1) model with coral effect on site occupancy probability; depth and time effect on fish detection
# 2) model with coral effect on site occupancy probability; random intercept (p per video), depth, time effect on fish detection

# inner part:
# models to run

# start of upper part

# create a dir to host models 

dir.create("bugs_models")

# Model 1
require(here)
sink(here ("bugs_models","StaticModel_LONGO_comm_reviewed.txt"))
cat("
    
    model {
            
      # priors
      ## priors for P
      ## depth effect
      for (k in 1:nspec) {
         for (i in 1:2){ # Implicitly define alpha of depth as a vector
           alpha.depth[k,i] ~ dunif(0,1)
           intercept.depth[k,i] <- logit (alpha.depth[k,i])
        #alpha0 [k] ~ dunif (0,1)
        #intercept.p[k] <- logit(alpha0[k]) 
         }
      }
       
      ## alpha1 - effect of time on detection p
      # it also comesfrom the community
      for (k in 1:nspec) {
          alpha1.time[k] ~ dnorm (mu.time[k],tau.time[k]) 
          mu.time[k] ~ dnorm(0, 0.001)
          tau.time[k] <- 1/(sigma.time[k]*sigma.time[k])
          sigma.time[k] ~ dunif(0,10)
      }
      
      ## occupancy priors
      for (k in 1:nspec) {
      #  for (j in 1:nreg) {
            beta0 [k] ~ dunif (0,1)
            intercept.psi [k] <- logit(beta0[k])
        # }
       }
    
      ## regression coefficient
      for (k in 1:nspec) {
      #  for (j in 1:nreg){
            beta1[k] ~ dnorm (mu.int[k],tau.mu[k])  
      #     ## priors for them
            mu.int[k] ~ dnorm(0, 0.001)
            tau.mu[k] <- 1/(sigma.int[k]*sigma.int[k])
            sigma.int[k] ~ dunif(0,10)
      #   }
      }
    
      # Ecological submodel: Define state conditional on parameters
      for (k in 1:nspec) {
         for(i in 1:nsite){    ## occupancy model, loop across sites
            
            z [i,k] ~ dbern(psi[i,k])
                    
            ## This keeps the program on the track
            psi[i,k]<-max(0.00001,min(0.99999, psi0[i,k]))
        
            logit(psi0 [i,k]) <- intercept.psi[k] + beta1 [k]* coral [i]
                                                      
            }
       }
            
       # # # # # # # #  # # # # # # #
       ####### observation model ####
                    
       for (k in 1:nspec) {# loop across spp
                       
          for (n in 1:nobs) { ## loop over observations
                          
             y [n,k] ~ dbern(muY[site[n], occa[n],k])
             muY [site[n], occa[n],k] <- z[site[n],k] * p[k,n]
             logit (p[k,n]) <-  intercept.depth[k,prof[n]]+alpha1.time[k]*time[n] 
              
           }
    
        }
            
            
        ##############################################################
        #                      Goodness of fit                       #
        ##############################################################
        #       (based on posterior predictive distributions)       #
        #############################################################
        # Draw a replicate data set under the fitted model
        for (k in 1:nspec) {
           for (n in 1:nobs){
              yrep[n,k] ~ dbern(muYrep[site [n], occa[n],k])
              muYrep [site [n], occa[n],k] <- z[site [n],k]*p[k,n]
           }
        }
        
        # Compute detection frequencies for observed and replicated data
        ## the first loop is used to extract data for each site    
        ## The outside function sum is used to aggregate data
        
        for (k in 1:nspec) {
          for (i in 1:nsite) {
            for (n in 1:nobs) {
        
              y.prov [i,n,k] <- ifelse (site[n] == i, y [n,k],0)## temporary data
              yrep.prov [i,n,k] <- ifelse (site[n] == i, yrep [n,k],0)## temporary data
            
               }
        
          detfreq [i,k] <- sum (y.prov[i,,k]) ## aggregate data
          detfreqrep [i,k] <- sum (yrep.prov[i,,k]) ## aggregate data
        
         }
       }
        
      # Expected detection frequencies under the model
      for (k in 1:nspec) {
         for (n in 1:nobs){
            tmp[n,k] <- z[site[n],k] * p[k,n]
         }
       }
        
      ## the first loop is used to extract data for each site    
      ## The outside function sum is used to aggregate data
      for (k in 1:nspec) {
          for (i in 1:nsite) {
             for (n in 1:nobs) {
        
                E.prov [i,n,k] <- ifelse (site[n] == i, tmp [n,k],0) ## temporary data
                
            }
        
            E [i,k] <- sum (E.prov[i,,k]) ## aggregate data
        
         }
       }
        
      # discrepancy statistics
      for (k in 1:nspec) {
         for (i in 1:nsite) {
        
            # Chi-square and Freeman-Tukey discrepancy measures
            # ..... for actual data set
            x2Closed[i,k] <- pow((detfreq[i,k] - E[i,k]),2) / (E[i,k]+e)
            ftClosed[i,k] <- pow((sqrt(detfreq[i,k]) - sqrt(E[i,k])),2)
            # ..... for replicated data set
            x2repClosed[i,k] <- pow((detfreqrep[i,k] - E[i,k]),2) / (E[i,k]+e)
            ftrepClosed[i,k] <- pow((sqrt(detfreqrep[i,k]) - sqrt(E[i,k])),2)
           }
       }
        
      # Add up Chi-square and FT discrepancies and compute fit stat ratio
      # (closed part)
      for (k in 1:nspec) {
         Chi2Closed[k] <- sum(x2Closed[1:nsite,k])
         FTClosed[k] <- sum(ftClosed[1:nsite,k])
         Chi2repClosed[k] <- sum(x2repClosed[1:nsite,k])
         FTrepClosed[k] <- sum(ftrepClosed[1:nsite,k])
         Chi2ratioClosed[k] <- Chi2Closed[k] / Chi2repClosed[k]
         FTratioClosed[k] <- FTClosed[k] / FTrepClosed[k]
            
         # Derived parameters
         mutot[k] <- sum(psi[1:nsite,k]) ## expected number of occupied sites (infinite sample)
         n.occ[k] <- sum(z[1:nsite,k]) ## finite sample size
         mean.p[k] <- mean (p[k,]) # mean detection prob across species
      }
      
      ## site richness
      for (i in 1:nsite) {
         
         rich[i] <- sum (z[i,])
         
      }
         
    } # end of the model
            

    ",fill = TRUE)

sink()


# Model 2

sink(here ("bugs_models","StaticModel_LONGO_comm_RdmP_reviewed.txt"))
cat("
    
      model {
              
        # priors
        ## priors for P
        ## depth effect
        for (k in 1:nspec) {
           for (i in 1:2){ # Implicitly define alpha of depth as a vector
             alpha.depth[k,i] ~ dunif(0,1)
             intercept.depth[k,i] <- logit (alpha.depth[k,i])
           }
        }
         
        ## alpha1 - effect of time on detection p
        # it also comes from the community
        for (k in 1:nspec) {
            alpha1.time[k] ~ dnorm (mu.time[k],tau.time[k]) 
            mu.time[k] ~ dnorm(0, 0.001)
            tau.time[k] <- 1/(sigma.time[k]*sigma.time[k])
            sigma.time[k] ~ dunif(0,10)
        }
        
        ## random effect for video
        for (k in 1:nspec){
           alpha0[k] ~ dunif (0,1)
           lalpha0[k] <- logit (alpha0[k])
           sd.p[k] ~ dunif (0,10) ## sd of p on logit scale
           tau.p[k] <- pow (sd.p[k],2)## p precision on logit scale
        }
        
        # one estimate per transect
        for (k in 1:nspec) {
           for (j in 1:nocca){
             logit (intercept.p [k,j]) <- lp [k,j]
             lp[k,j] ~ dnorm (lalpha0[k],tau.p[k])
           }
        }
      
        ## occupancy priors
        for (k in 1:nspec) {
        #  for (j in 1:nreg) {
              beta0 [k] ~ dunif (0,1)
              intercept.psi [k] <- logit(beta0[k])
          # }
         }
      
        ## regression coefficient
        for (k in 1:nspec) {
        #  for (j in 1:nreg){
              beta1[k] ~ dnorm (mu.int[k],tau.mu[k])  
        #     ## priors for them
              mu.int[k] ~ dnorm(0, 0.001)
              tau.mu[k] <- 1/(sigma.int[k]*sigma.int[k])
              sigma.int[k] ~ dunif(0,10)
        #   }
        }
      
        # Ecological submodel: Define state conditional on parameters
        for (k in 1:nspec) {
           for(i in 1:nsite){    ## occupancy model
              
              z [i,k] ~ dbern(psi[i,k])
                      
              ## This keeps the program on the track
              psi[i,k]<-max(0.00001,min(0.99999, psi0[i,k]))
          
              logit(psi0 [i,k]) <- intercept.psi[k] + beta1 [k]* coral [i]
                                                        
              }
         }
              
         # # # # # # # #  # # # # # # # #
         ####### observation model ######
                      
         for (k in 1:nspec) { # loop across spp
                         
            for (n in 1:nobs) { ## loop over observations
                            
               y [n,k] ~ dbern(muY[site[n], occa[n],k])
               muY [site[n], occa[n],k] <- z[site[n],k] * p[k,n]
               logit (p[k,n]) <-  intercept.depth[k,prof[n]]+ # detection model
                                  alpha1.time[k]*time[n] + 
                                  intercept.p[k, occa[n]]
                
             }
      
          }
              
              
          ##############################################################
          #                      Goodness of fit                       #
          ##############################################################
          #       (based on posterior predictive distributions)       #
          #############################################################
          # Draw a replicate data set under the fitted model
          for (k in 1:nspec) {
             for (n in 1:nobs){
                yrep[n,k] ~ dbern(muYrep[site [n], occa[n],k])
                muYrep [site [n], occa[n],k] <- z[site [n],k]*p[k,n]
             }
          }
          
          # Compute detection frequencies for observed and replicated data
          ## the first loop is used to extract data for each site    
          ## The outside function sum is used to aggregate data
          
          for (k in 1:nspec) {
            for (i in 1:nsite) {
              for (n in 1:nobs) {
          
                y.prov [i,n,k] <- ifelse (site[n] == i, y [n,k],0)## temporary data
                yrep.prov [i,n,k] <- ifelse (site[n] == i, yrep [n,k],0)## temporary data
              
                 }
          
            detfreq [i,k] <- sum (y.prov[i,,k]) ## aggregate data
            detfreqrep [i,k] <- sum (yrep.prov[i,,k]) ## aggregate data
          
           }
         }
          
        # Expected detection frequencies under the model
        for (k in 1:nspec) {
           for (n in 1:nobs){
              tmp[n,k] <- z[site[n],k] * p[k,n]
           }
         }
          
        ## the first loop is used to extract data for each site    
        ## The outside function sum is used to aggregate data
        for (k in 1:nspec) {
            for (i in 1:nsite) {
               for (n in 1:nobs) {
          
                  E.prov [i,n,k] <- ifelse (site[n] == i, tmp [n,k],0) ## temporary data
                  
              }
          
              E [i,k] <- sum (E.prov[i,,k]) ## aggregate data
          
           }
         }
          
        # discrepancy statistics
        for (k in 1:nspec) {
           for (i in 1:nsite) {
          
              # Chi-square and Freeman-Tukey discrepancy measures
              # ..... for actual data set
              x2Closed[i,k] <- pow((detfreq[i,k] - E[i,k]),2) / (E[i,k]+e)
              ftClosed[i,k] <- pow((sqrt(detfreq[i,k]) - sqrt(E[i,k])),2)
              # ..... for replicated data set
              x2repClosed[i,k] <- pow((detfreqrep[i,k] - E[i,k]),2) / (E[i,k]+e)
              ftrepClosed[i,k] <- pow((sqrt(detfreqrep[i,k]) - sqrt(E[i,k])),2)
             }
         }
          
        # Add up Chi-square and FT discrepancies and compute fit stat ratio
        # (closed part)
        for (k in 1:nspec) {
           Chi2Closed[k] <- sum(x2Closed[1:nsite,k])
           FTClosed[k] <- sum(ftClosed[1:nsite,k])
           Chi2repClosed[k] <- sum(x2repClosed[1:nsite,k])
           FTrepClosed[k] <- sum(ftrepClosed[1:nsite,k])
           Chi2ratioClosed[k] <- Chi2Closed[k] / Chi2repClosed[k]
           FTratioClosed[k] <- FTClosed[k] / FTrepClosed[k]
              
           # Derived parameters
           mutot[k] <- sum(psi[1:nsite,k]) ## expected number of occupied sites (infinite sample size)
           n.occ[k] <- sum(z[1:nsite,k]) ## finite sample
           mean.p[k] <- mean (p[k,]) # mean detection probability per spp
        }
        
        ## richness
        for (i in 1:nsite) {
           
           rich[i] <- sum (z[i,])
           
        }
           
      } # end of the model
        

    ",fill = TRUE)

sink()

# ------------------------
# Inner PART
## load packages
source("R/packages.R")

## fish data
load (here("output_comm_wide","Data_fish_detection_LONGO_AUED.RData"))

## coral data
load (here("output_comm_wide","Data_coral_detection_LONGO_AUED.RData"))

# coral cover data
coral_cover_data <- sp_cover_data
# standardize it
coral_cover_data_std <- apply (coral_cover_data, 2, function (i)
  (i - mean (i))/sd(i))

## standardize time
# all the same across species
std_time <- (sqrt (df_fish_data_per_coral[[1]][,'time',1]) - mean(sqrt (df_fish_data_per_coral[[1]][,'time',1])))/sd(sqrt (df_fish_data_per_coral[[1]][,'time',1]))

# BASIC INFO
basic_data <- cbind(coordenadas, 
                    coral_cover_data[,1:4],
      sites=names(nvideos),
      nphoto=nvideos,
      nvideo=rowSums (table(df_fish_data_per_coral[[1]][,'M',1],
            df_fish_data_per_coral[[1]][,'J',1]))
)

apply (coral_cover_data[,1:4],2,mean)
apply (coral_cover_data[,1:4],2,sd)
apply (coral_cover_data[,1:4],2,range)

colSums(coral_cover_data[,1:4]>0)

# save
write.csv (basic_data,file=here("output_comm_wide","basic_data_table1.csv"))

################
# MCMC settings
ni <- 100000 # n iterations
nb <- 80000 # burn-in phase
na <- 50000 # adaptive phase
nt <- 50 # thinnig each nt samples
nc <- 3 # n MCMC chains

# -------------------------------
#         Run model 1
# ------------------------------

## Parameters to monitor
params <- c(
  ### detection parameters
  "alpha0",
  "alpha1.time", 
  "mu.time","tau.time",
  "intercept.depth","alpha.depth",
  
  ### occupancy parameters
  "beta0","intercept.psi",
  "beta1", #"beta1.sd","beta1.tau",
  "psi",
  "mu.int",
  "tau.mu",
  
  ## goodness of fit parameters
  "FTratioClosed",
  "Chi2Closed",
  "Chi2repClosed",
  
  ## derived par
  "mutot",
  "n.occ",
  "mean.p"
)

### apply the model to each fish per coral spp

cl <- makeCluster(nc) ## number of cores

# export packages
clusterEvalQ(cl, library(jagsUI))
clusterEvalQ(cl, library(vegan))
clusterEvalQ(cl, library(here))

# export your data and function
clusterExport(cl, c("df_fish_data_per_coral", 
                    #"covariates_site",
                    "coral_cover_data_std",
                    "ni","nt","nb","nc","na",
                    "params",
                    "std_time"))

### run in parallel processing
samples_OCCcoral_PdepthTime_longo <- parLapply (cl,seq(1,length(df_fish_data_per_coral)), function (coral) {
    
    ## bundle data -> [,,1] because all dims are equal
    str(jags.data<- list(y= df_fish_data_per_coral [[coral]] [,"y",], 
                         nspec = dim(df_fish_data_per_coral [[coral]] [,"y",])[2],
                         nsite = max (df_fish_data_per_coral [[coral]] [,"M",1]),
                         prof= df_fish_data_per_coral [[coral]] [,"prof",1],
                         nobs = nrow (df_fish_data_per_coral [[coral]] [,,1]),
                         time = std_time,
                         site = df_fish_data_per_coral [[coral]] [,"M",1],
                         occa = df_fish_data_per_coral[[coral]] [,"J",1],
                         coral= coral_cover_data_std[,coral],
                         e= 0.0001))
    
    ## initial values
    zst <- matrix(1,nrow=jags.data$nsite,
                  ncol=jags.data$nspec)
    inits <- function(){list(z = zst)}# Observed occurrence as inits for z
    
    # run jags
    samples <- jags(data = jags.data, 
                    params, 
                    model = here ("bugs_models","StaticModel_LONGO_comm_reviewed.txt"), 
                    inits = inits,
                    n.chains = nc, 
                    n.thin = nt, 
                    n.iter = ni, 
                    n.burnin = nb, 
                    DIC = T,
                    parallel = F)
    
  }  # close
)

stopCluster(cl)
# save MCMC runs
save(samples_OCCcoral_PdepthTime_longo, 
     file=here("output_comm_wide","samples_OCCcoral_PdepthTime_longo.RData"))

### EXAMINE RESULTS
## chi-square statistics
Chi2ratioClosed <- lapply (samples_OCCcoral_PdepthTime_longo, function (i)
  lapply (i, function (k)
    k$sims.list$Chi2Closed/k$sims.list$Chi2repClosed))

## bayesian p-value

bvclosed <- lapply (samples_OCCcoral_PdepthTime_longo, function (i)
  lapply (i, function (j)
    (unlist(lapply (j, function (k)
      
      sum (k$sims.list$Chi2repClosed > k$sims.list$Chi2Closed)/ # proportion of sims that expected was larger than observed
        length(k$sims.list$Chi2repClosed)
    )))))

plot(samples_OCCcoral_PdepthTime_longo[[2]]$sims.list$Chi2Closed, 
     samples_OCCcoral_PdepthTime_longo[[2]]$sims.list$Chi2repClosed)
abline(1,1)

# ----------------------------------- #
# Model 2 - RANDOM EFFECT OF VIDEO

## Parameters to monitor
params <- c(
  ### detection parameters
  "alpha0",
  "alpha1.time", 
  "mu.time","tau.time",
  "alpha.depth", "intercept.depth",
  "intercept.p","lp","tau.p", "lalpha0","alpha0",
  
  ### occupancy parameters
  "beta0","intercept.psi",
  "beta1", #"beta1.sd","beta1.tau",
  "psi",
  "mu.int",
  "tau.mu",
  
  ## goodness of fit parameters
  "FTratioClosed",
  "Chi2Closed",
  "Chi2repClosed",
  
  ## derived par
  "mutot",
  "n.occ",
  "mean.p"
)

# apply the model to each fish and coral

cl <- makeCluster(nc) ## number of cores

# export packages
clusterEvalQ(cl, library(jagsUI))
clusterEvalQ(cl, library(vegan))
clusterEvalQ(cl, library(here))

# export your data and function
clusterExport(cl, c("df_fish_data_per_coral", 
                    "coral_cover_data_std",
                    "ni","nt","nb","nc","na",
                    "params",
                    "std_time"))

### run in parallel processing
samples_OCCcoral_PdepthTime_longo_RdmP <- parLapply (cl,seq(1,length(df_fish_data_per_coral)), function (coral) {
  
  ## bundle data -> [,,1] because all dims are equal (of any object)
  str(jags.data<- list(y= df_fish_data_per_coral [[coral]] [,"y",], 
                       nspec = dim(df_fish_data_per_coral [[coral]] [,"y",])[2],
                       nsite = max (df_fish_data_per_coral [[coral]] [,"M",1]),
                       prof= df_fish_data_per_coral [[coral]] [,"prof",1],
                       nobs = nrow (df_fish_data_per_coral [[coral]] [,,1]),
                       time = std_time,
                       nocca = max(df_fish_data_per_coral[[coral]] [,"J",1]),
                       site = df_fish_data_per_coral [[coral]] [,"M",1],
                       occa = df_fish_data_per_coral[[coral]] [,"J",1],
                       coral= coral_cover_data_std[,coral],
                       e= 0.0001))
  
  
  ## initial values
  zst <- matrix(1,nrow=jags.data$nsite,
                ncol=jags.data$nspec)
  inits <- function(){list(z = zst)}# Observed occurrence as inits for z
  
  # run jags
  
  samples <- jags(data = jags.data, 
                  params, 
                  model = here ("bugs_models","StaticModel_LONGO_comm_RdmP_reviewed.txt"), 
                  inits = inits,
                  n.chains = nc, 
                  n.thin = nt, 
                  n.iter = ni, 
                  n.burnin = nb, 
                  DIC = T,
                  parallel = F)
  
  
  }
)

stopCluster(cl)
# save it
save(samples_OCCcoral_PdepthTime_longo_RdmP, 
     file=here("output_comm_wide","samples_OCCcoral_PdepthTime_longo_RdmP.RData"))
