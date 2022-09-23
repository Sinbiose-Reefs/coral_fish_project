# second part
## site occupancy modeling
## find coral - associated fish

# upper part:

# models in BUGS language (PS: at this point you need to have JAGS installed in your computer)

# 1) model with coral effect on site occupancy probability; random intercept (p per video), depth, time effect on fish detection

# inner part:
# models to run


# ============================================



# start the upper part



# create a dir to host models 
dir.create("bugs_models")

# Model 1
require(here)

sink(here ("bugs_models",
           "StaticModel_LONGO_comm_RdmP_reviewed.txt"))
cat("
    
      model {
              
       
       # ===================================================================
       
       # Priors
        
        # Priors for detection probability
        
        ### depth effect
        for (k in 1:nspec) {
           for (i in 1:2){ # Implicitly define alpha of depth as a vector
             alpha.depth[k,i] ~ dunif(0,1)
             intercept.depth[k,i] <- logit (alpha.depth[k,i])
           }
        }
         
         
        ### random effect of video
        for (k in 1:nspec){
           alpha0[k] ~ dunif (0,1)
           lalpha0[k] <- logit (alpha0[k])
           sd.p[k] ~ dunif (0,10) ## sd of p on logit scale
           tau.p[k] <- pow (sd.p[k],2)## p precision on logit scale
        }
        
        ### one estimate per transect
        for (k in 1:nspec) {
           for (j in 1:nocca){
             logit (intercept.p [k,j]) <- lp [k,j]
             lp[k,j] ~ dnorm (lalpha0[k],tau.p[k])
           }
        }
      
      
        # Site - occupancy priors
        ### the intercept
        for (k in 1:nspec) {
              beta0 [k] ~ dunif (0,1)
              intercept.psi [k] <- logit(beta0[k])
         }
      
        ### regression coefficient depicting the influence of coral cover
        for (k in 1:nspec) {
              beta1[k] ~ dnorm (mu.int[k],tau.mu[k])  
              mu.int[k] ~ dnorm(0, 0.001)
              tau.mu[k] <- 1/(sigma.int[k]*sigma.int[k])
              sigma.int[k] ~ dunif(0,10)
        }
      
        ### regression coefficient depicting the influence of turf cover
        for (k in 1:nspec) {
              beta2[k] ~ dnorm (mu.int2[k],tau.mu2[k])  
              mu.int2[k] ~ dnorm(0, 0.001)
              tau.mu2[k] <- 1/(sigma.int2[k]*sigma.int2[k])
              sigma.int2[k] ~ dunif(0,10)
        }
        
        
        # =====================================================================
        
        # MODELS 
        
        
        # Ecological submodel: Define state conditional on parameters
        
        
        for (k in 1:nspec) {
           for(i in 1:nsite){    ## occupancy model
              
              z [i,k] ~ dbern(psi[i,k])
                      
              ## This keeps the program on the track
              psi[i,k]<-max(0.00001,min(0.99999, psi0[i,k]))
          
              logit(psi0 [i,k]) <- intercept.psi[k] + beta1 [k]*coral [i] + beta2 [k]*turf [i]
                                                        
              }
         }
              
         
         # observation submodel: detection probability based on depth and videos
                      
         for (k in 1:nspec) { # loop across spp
                         
            for (n in 1:nobs) { ## loop over observations
                            
               y [n,k] ~ dbern(muY[site[n], occa[n],k])
               muY [site[n], occa[n],k] <- z[site[n],k] * p[k,n]
               logit (p[k,n]) <-  intercept.depth[k,prof[n]]+ # detection model
                                  #alpha1.time[k]*time[n] + 
                                  intercept.p[k, occa[n]]
                
             }
      
          }
              
              
          ##############################################################
          #                      Goodness of fit                       #
          #       (based on posterior predictive checks)               #
          ##############################################################
          
          
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
        ## The outside function 'sum' is used to aggregate data
        
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
        
        ## richness likely supported by each coral species
        
        for (i in 1:nsite) {
           
           rich[i] <- sum (z[i,])
           
        }
        
        # mean regression coefficients
           mean.beta1 <- mean(beta1)
           mean.beta2 <- mean(beta2)
           
           
           
      } # end of the model
        

    ",fill = TRUE)

sink()

# ============================================

# Inner PART
## load packages
source("./R_comm_wide/packages.R")

## fish data
load (here("output_comm_wide_R1","Data_fish_detection_LONGO_AUED.RData"))

# standardize data
coral_cover_data_std <- apply (cob_corals, 2, function (i)
  (i - mean (i))/sd(i))

# standardize turf
turf_cover_data_std <- (cob_algae$`calcareous turf` - mean(cob_algae$`calcareous turf`))/sd(cob_algae$`calcareous turf`)


# correlation between benthic components    
correlations <-cor(data.frame(coral_cover_data_std,turf_cover_data_std))
range(as.numeric(correlations)[which(as.numeric(correlations) != 1)])

# summary stats

round(apply (cob_corals,2,mean),3)
round(apply (cob_corals,2,sd),3)
round(apply (cob_corals,2,range),3)


# number of sites with coral detection
colSums(cob_corals>0)
# number of sites with turf detection
sum(cob_bentos$`calcareous turf`>0)

# number of analyzed species
adult <- lapply (fish_species, function (i) i[[1]]) 
adult<-unique(unlist(adult)) # adult
juvenile <- lapply (fish_species, function (i) i[[2]]) 
juvenile<-unique(unlist(juvenile))#juvenile

# number of fish in both phases (true) and only juvenile (false)
table(juvenile %in% adult)# both
unique(c(adult, juvenile))


# doubt
doubt<- df_fish_data_per_coral[[1]][[1]][,,"Chaenopsis ocellata"]
coordenadas$Group.1 [unique(doubt[which(doubt[,"y"]>0),"M"])]

# table of sites
tabS1 <- cbind (coordenadas, 
       cob_algae$`calcareous turf`,
       cob_algae$sites_analysis,
       cob_corals)
write.csv (tabS1, file = here ("output_comm_wide_R1",
                                   "tabS1.1.csv"))

# ============================================

set.seed (1234)

# MCMC settings
ni <- 100000 # n iterations
nb <- 50000 # burn-in phase
na <- 30000 # adaptive phase
nt <- 50 # thinnig each nt samples
nc <- 3 # n MCMC chains


# ============================================
# RUN Model - RANDOM EFFECT OF VIDEO


## Parameters to monitor
params <- c(
  ### detection parameters
  "alpha0",
  #"alpha1.time", 
  #"mu.time","tau.time",
  "alpha.depth", "intercept.depth",
  "intercept.p","lp","tau.p", "lalpha0",
  
  ### occupancy parameters
  "beta0","intercept.psi",
  "beta1", #"beta1.sd","beta1.tau",
  "beta2", # turf effect
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
  "mean.p",
  "mean.beta1",
  "mean.beta2"
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
                    "turf_cover_data_std",
                    "ni","nt","nb","nc","na",
                    "params"
                    ))

### run in parallel processing
samples_OCCcoral_PdepthTime_longo_RdmP <- parLapply (cl,seq(1,length(df_fish_data_per_coral)), function (coral) # across corals
            
            
            lapply (seq (1,length(df_fish_data_per_coral[[coral]])), function (age)   { # across ages
              
              
              #tryCatch( {# catch any error
                
                ## bundle data -> [,,1] because all dims are equal (of any object)
                str(jags.data<- list(y= df_fish_data_per_coral [[coral]][[age]] [,"y",], 
                                     nspec = dim(df_fish_data_per_coral [[coral]] [[age]][,"y",])[2],
                                     nsite = max (df_fish_data_per_coral [[coral]] [[age]][,"M",1]),
                                     prof= df_fish_data_per_coral [[coral]][[age]] [,"prof",1],
                                     nobs = nrow (df_fish_data_per_coral [[coral]][[age]] [,,1]),
                                     nocca = max(df_fish_data_per_coral[[coral]][[age]] [,"J",1]),
                                     site = df_fish_data_per_coral [[coral]][[age]] [,"M",1],
                                     occa = df_fish_data_per_coral[[coral]][[age]] [,"J",1],
                                     coral= coral_cover_data_std[,coral],
                                     turf = turf_cover_data_std,
                                     e= 0.0001))
  
  
                  ## initial values
                  zst <- matrix(1,nrow=jags.data$nsite,
                                ncol=jags.data$nspec)
                  inits <- function(){list(z = zst)}# Observed occurrence as inits for z
                  
                  # run jags
                  
                  samples <- jags(data = jags.data, 
                                  params, 
                                  model = here ("bugs_models",
                                                "StaticModel_LONGO_comm_RdmP_reviewed.txt"), 
                                  inits = inits,
                                  n.chains = nc, 
                                  n.thin = nt, 
                                  n.iter = ni, 
                                  n.burnin = nb, 
                                  DIC = T,
                                  parallel = F)
                  
                  #},
                  
  #error = function(e) return ("NULL")#{print(e); print("retrying...")
             # )

    }
  ) # close age
) # close corals

stopCluster(cl)




# save it
save(samples_OCCcoral_PdepthTime_longo_RdmP, 
     file=here("output_comm_wide_R1",
               "samples_OCCcoral_PdepthTime_longo_RdmP.RData"))

