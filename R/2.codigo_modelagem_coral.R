
## Implementação do modelo de ocupação de sítios
## aplicado a espécies de corais


### load packages

source("R/packages.R")

### na parte superior do codigo tem codigos dos modelos em linguagem BUGS
## tem 1 modelo
# 1) modelo de ocupação com probabilidade de ocupacao por coral dependendendo da distancia espacial
# p constante

### na parte do meio do codigo tem as funcoes para rodar os modelos
# ele funciona em listas. Tem duas listas de dados envolvidas
# 1) lista das espécies (ou gênero) de 5 espécies/generos  de coral
# 2) lista de especies de peixes (dados de deteccao e não deteccao em observações em 32 sitios, e em n transecções por sitios)

### na parte final tem uma interpretacao breve dos resultados, visando relacionar a ocupacao de sitios estimada com a cobertura original dos corais

### PARTE SUPERIOR

### HELP HERE

#  https://arxiv.org/pdf/1201.2375.pdf
# https://www.ime.usp.br/~sferrari/beta.pdf
# https://core.ac.uk/reader/11056214
# http://www.biometrica.tomsk.ru/lib/kery.pdf


# BUGS model
sink(here ("bugs","StaticCARModel_coral_dbeta_comm_GOF.txt"))

cat("

model{

    ## priors 
    # CAR prior distribution for spatial random effects:
    for (k in 1:nspec) {
       rho[k,1:nsite] ~ car.normal(adj[], weights[], num[], spacetau[k])
       # hyperprior of rho 
       spacesigma[k] ~ dunif(0,5)
       spacetau[k]  <- 1/(spacesigma[k]*spacesigma[k])
    }
    

    # hyperprior of rho 
    
    # regression parameters
    for (k in 1:nspec) {
       b0 [k]~ dnorm(0,1.0E-2)
       c0 [k]~ dnorm(0,1.0E-2)
       c1 [k]~ dnorm(0,1.0E-2)
    }

   ## likelihood
   for(k in 1:nspec) {
      for(i in 1:nsite) {
   
         ## cover dataset 
         C[i,k] ~ dbeta(p[i,k],q[i,k])
         p[i,k] <- mu[i,k]*phi[i,k]
         q[i,k] <- (1-mu[i,k])*phi[i,k]  # phi[i,k]-mu[i,k]*phi[i,k]
         ## or an alternative parameterization: phi[i]-mu.s[i]*phi[i]

         # model for average cover
         mu0[i,k]<- b0[k] + rho[k,i]
         mu.lim[i,k] <- min(10,max(-10,mu0[i,k]))
         logit(mu[i,k]) <- mu.lim[i,k]

         # precision model
         phi[i,k] <- exp(c0[k] + c1[k]*nvideos[i])

         } ## close site loop
      } ## close spp loop

    # Assess model fit using a sums-of-squares-type discrepancy
    #for (k in 1:nspec) {
    #   for (i in 1:nsite) {
    #      residual[i,k] <- C[i,k]-p[i,k] # Residuals for observed data
    #      predicted[i,k] <- p[i,k] # Predicted values
    #      sq[i,k] <- pow(residual[i,k], 2) # Squared residuals for observed data
    
    #      # Generate replicate data and compute fit stats for them
    #      C.new[i,k] ~ dbeta(mu[i,k],q[i,k]) # one new data set at each MCMC iteration
    #      sq.new[i,k] <- pow(C.new[i,k]-predicted[i,k], 2) # Squared residuals for new data
    #   }

    #   fit[k] <- sum(sq[,k]) # Sum of squared residuals for actual data set
    #   fit.new[k] <- sum(sq.new[,k]) # Sum of squared residuals for new data set
    #   test[k] <- step(fit.new[k] - fit[k]) # Test whether new data set more extreme
    #   bpvalue[k] <- mean(test[k]) # Bayesian p-value
    #}

  # derived parameters
  # MEAN COVER per species
  for (k in 1:nspec) {
     meanCov [k] <- mean(p[,k]) 
  }

  # total cover per site
  for (i in 1:nsite) {
     totCov [i] <- sum(p[i,]) 
  }
  
  # MEAN COVER per species (mu)
  for (k in 1:nspec) {
    meanCovmu [k] <- mean(mu[,k]) 
  }

  # total cover per site (considering mu)
  for (i in 1:nsite) {
    totCovmu [i] <- sum(mu[i,]) 
  }

}
  ",fill = TRUE)

sink()

### binomial model
sink(here ("bugs","StaticCARModel_coral_dbin_comm_GOF.txt"))

cat("
    
    model{
    
    ## priors 
    # CAR prior distribution for spatial random effects:
   ## priors 
    for (k in 1:nspec) {
    # CAR prior distribution for spatial random effects:
       rho[k,1:nsite] ~ car.normal(adj[], weights[], num[], spacetau[k])
       # hyperprior of rho 
       spacesigma[k] ~ dunif(0,5)
       spacetau[k]  <- 1/(spacesigma[k]*spacesigma[k])
    }
    
    
    # regression parameters
    for (k in 1:nspec) {
        b0[k] ~ dunif(-10,10)
    }

    ## likelihood
    for (k in 1:nspec) {
       for(i in 1:nsite) {
    
          ## cover dataset 
          C[i,k] ~ dbin(p[i,k],N[i]) ## N is constant over sites
          
          # model for p
          mup[i,k] <- b0[k] + rho[k,i]
          mup.lim[i,k] <- min(10,max(-10,mup[i,k]))
          logit(p[i,k]) <- mup.lim[i,k]
       }
    }
    
   # Assess model fit using a sums-of-squares-type discrepancy
   for (k in 1:nspec) {
      for (i in 1:nsite) {
         residual[i,k] <- C[i,k]-p[i,k] # Residuals for observed data
         predicted[i,k] <- p[i,k] # Predicted values
         sq[i,k] <- pow(residual[i,k], 2) # Squared residuals for observed data
    
         # Generate replicate data and compute fit stats for them
         C.new[i,k] ~ dbin(p[i,k], N[i]) # one new data set at each MCMC iteration
         sq.new[i,k] <- pow(C.new[i,k]-predicted[i,k], 2) # Squared residuals for new data
      }
      fit[k] <- sum(sq[,k]) # Sum of squared residuals for actual data set
      fit.new[k] <- sum(sq.new[,k]) # Sum of squared residuals for new data set
      test[k] <- step(fit.new[k] - fit[k]) # Test whether new data set more extreme
      bpvalue[k] <- mean(test[k]) # Bayesian p-value
   }

   # derived parameters
   # mean probability per species
   for (k in 1:nspec) {
      meanP [k] <- mean(p[,k]) 
   }

   # number of sites per species
   for (k in 1:nspec) {
      nSiteP [k] <- sum(p[,k]) 
   }

   # expected number of species per site
   for (i in 1:nsite) {
      totSp [i] <- sum(p[i,]) 
   }

   # mean number of species per site
   for (i in 1:nsite) {
      meanSp [i] <- mean(p[i,]) 
   }

}
    ",fill = TRUE)

sink()

# binomial bernoulli model

### binomial model
sink(here ("bugs","StaticCARModel_coral_dbin_bern_comm_GOF.txt"))

cat("
    
    model{
    
    ## priors 
    for (k in 1:nspec) {
       # CAR prior distribution for spatial random effects:
       rho[k,1:nsite] ~ car.normal(adj[], weights[], num[], spacetau[k])
       # hyperprior of rho 
       spacesigma[k] ~ dunif(0,5)
       spacetau[k]  <- 1/(spacesigma[k]*spacesigma[k])
    }

    
    # regression parameters
    for (k in 1:nspec) {
       b0[k] ~ dunif(-10,10)
       ## detection probability
       p[k] ~ dunif (0,1)
    }
    
    ## likelihood
    for (k in 1:nspec) { # for each species
       for(i in 1:nsite) { # for each site
          ## occupancy model
          z [i,k] ~ dbern (psi[i,k])

          mu[i,k] <- b0[k] + rho[k,i]
          mu.lim[i,k] <- min(10,max(-10,mu[i,k]))
          logit(psi[i,k]) <- mu.lim[i,k]

          ## observation model 
          C[i,k] ~ dbin(p.eff[i,k],N[i])
          p.eff[i,k] <- p[k] * z[i,k]
       
          }## close site loop
       } ## close species loop

    # Assess model fit using a sums-of-squares-type discrepancy
    for (k in 1:nspec) {
       for (i in 1:nsite) {
          residual[i,k] <- C[i,k]-psi[i,k] # Residuals for observed data
          predicted[i,k] <- psi[i,k] # Predicted values
          sq[i,k] <- pow(residual[i,k], 2) # Squared residuals for observed data
    
          # Generate replicate data and compute fit stats for them
          C.new[i,k] ~ dbin(p.eff[i,k],N[i]) # one new data set at each MCMC iteration
          sq.new[i,k] <- pow(C.new[i,k]-predicted[i,k], 2) # Squared residuals for new data
       }
       fit[k] <- sum(sq[,k]) # Sum of squared residuals for actual data set
       fit.new[k] <- sum(sq.new[,k]) # Sum of squared residuals for new data set
       test[k] <- step(fit.new[k] - fit[k]) # Test whether new data set more extreme
       bpvalue[k] <- mean(test[k]) # Bayesian p-value
    }
    
    # Derived parameters
    # number of species per site (finite sample size)
    for (i in 1:nsite) {
      n.occ [i] <- sum(z[i,]) 
      mutot [i] <- sum(psi[i,])
    }
    
    # number of sites per species 
    for (k in 1:nspec) {
      n.spp[k] <- sum(z[,k])
      n.spp.mu[k] <- sum(psi[,k])
    }

    # mean detection probabability
    meanP <- mean(p[])
    meanPsi <- mean(psi[,])
    meanZ <- mean(z[,])
}
    ",fill = TRUE)

sink()

########################################

#### PARTE DO MEIO (Modelos)

# MCMC settings
# common to all datasets

ni <- 50000
nt <- 10
nb <- 40000
nc <- 3
na <- 30000

#############################################
############### load data
#############################################

load (here ("output", "Data_coral_detection.RData"))

# Generate several  neighbours for sensitivity analyses
coord <- coordenadas [,c("Lon","Lat")]
coordinates(coord) <- ~ Lon + Lat
crs(coord) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

## d1=0; d2 varying as (c(3,6,9,12,15,18,21,24))

#neigh_winnb <- lapply (as.list(c(3,6,9,12,15,18,21,24)), function (i)  {
nei<-6
#create neighborhood
neigh <- dnearneigh((coord), 0, nei)
# Number of neighbours
table(card(neigh))
# Convert the neighbourhood
winnb <- nb2WB(neigh)

###############################
## model to estimate coral cover

## calcular a cobertura (soma dos videos)
cover_data <- apply(arranjo_corais,c(1,3),max,na.rm=T)
## as dbeta can not deal with 0's and 1', I transformed zeros into very small numbers
cover_data <- ifelse (cover_data == 0, 0.00000001,cover_data)

# standardize number of videos  
nvideos <- rowSums(is.na(arranjo_corais[,,1])!=T)
std_videos <- (nvideos-mean(nvideos))/sd(nvideos)
str(win.data<- list(C =  cover_data,# i[,"y"], 
                    nspec = ncol (cover_data),
                    nsite = nrow (cover_data),
                    nvideos = std_videos,
                    num = winnb$num, 
                    adj = winnb$adj, 
                    weights = winnb$weights
                    ))

## inits for the spatial factor
inits <- function(){list(
  b0 = runif (20,-10,10),#runif (20,-10,10)
  c0 = runif (20,-10,10),
  c1 = runif (20,-10,10)
  #rho = matrix (0, nrow=20,ncol=48)
  )} #rep(0, win.data$nsite)
                           
# run winbugs
## Parameters to monitor
  
params <- c(
  ### cover-model parameters
    #"bpvalue", "fit","fit.new","test",
    "meanCov","totCov","meanCovmu","totCovmu",
    "b0",#"c0","c1",
    "mu","p","q","spacesigma",
    "spacetau", "rho"
    )
  
## call and run winBUGS  
samples <- bugs(data = win.data, parameters.to.save = params, 
                model.file = here ("bugs","StaticCARModel_coral_dbeta_comm_GOF.txt"), 
                inits = NULL,
                n.chains = nc, 
                n.thin = nt, 
                n.iter = ni, 
                n.burnin = nb, 
                DIC = T,
                bugs.directory = "C:/Program Files (x86)/winbugs14_unrestricted/WinBUGS14",
                debug=T) ## you don't need close manually if debug = F 

save (samples,file=here("output", "StaticCARModel_coral_dbeta_comm.RData"))  
  
  
####
  
# binomial model used to estimate the probability of finding a cover higher than 1% 
# relative to total possible cover 

# maximum cover a species could reach
tot_cover <- rep(100,48) # apply (arranjo_corais,c(1,3),max, na.rm=T)
#tot_cover <- ifelse (rowSums (tot_cover)>1,1, rowSums (tot_cover))
## subset dos sitios com cibertura de coral > 0 
#coral_sites <- which( tot_cover >0)
# select 
#tot_cover <- tot_cover [coral_sites]
# focal species data 
local_data <- apply(arranjo_corais,c(1,3),max,na.rm=T)*100
  
##  subset of space (as we can't analyze sites with no coral cover)
#neigh_winnb <- lapply (as.list(c(3,6,9,12,15,18,21,24)), function (i)  {
#create neighborhood
#neigh <- dnearneigh((coord[coral_sites,]), 0, nei)
# Number of neighbours
#table(card(neigh))
# Convert the neighbourhood
#winnb <- nb2WB(neigh)

## bundle data
str(win.data<- list(C =  round(local_data),# i[,"y"], 
                      nsite = nrow(local_data),
                      nspec = ncol (local_data),
                      N = tot_cover,
                      num = winnb$num, 
                      adj = winnb$adj, 
                      weights = winnb$weights
))
  
  
## inits
inits <- function(){list(
    rho = rep(0, win.data$nsite)
  )}
  
# run winbugs
params <- c(
    ### binomial model parameters
  "bpvalue", "fit","fit.new","test",
  "totSp","meanP", "nSiteP", "meanSp",
   "b0", "p","spacesigma",
    "spacetau", "rho"
    
  )
  
samples_dbinV1 <- bugs(data = win.data, parameters.to.save = params, 
                  model.file = here ("bugs","StaticCARModel_coral_dbin_comm_GOF.txt"), 
                  inits = NULL,
                  n.chains = nc, 
                  n.thin = nt, 
                  n.iter = ni, 
                  n.burnin = nb, 
                  DIC = T,
                  bugs.directory = "C:/Program Files (x86)/winbugs14_unrestricted/WinBUGS14",
                  debug=F) ## you don't need close manually if debug = F 
  
save (samples_dbinV1,file=here("output", "StaticCARModel_coral_dbin_commV1.RData"))

# binomial model V2: nesta versao da mesma estrutura do modelo anterior, estimamos a probabilidade de ao menos uma deteccao
# em relacao ao numero de videos alocados em um dado sitio
  
## encontrar o numero de videos ( constante entre spp )
nvideos <- rowSums (is.na(arranjo_corais [,,1])!=T )

## numero observado de deteccoes da especie
local_data <- lapply (seq(1,dim(arranjo_corais)[3]), function (i) 
  rowSums (arranjo_corais [,,i]>0,na.rm=T)
  )
local_data<- do.call(cbind,local_data)
#create neighborhood
neigh <- dnearneigh((coord), 0, nei)
# Number of neighbours
table(card(neigh))
# Convert the neighbourhood
winnb <- nb2WB(neigh)

# bundle data
str(win.data<- list(C =  local_data,
                      nsite = nrow(local_data),
                      nspec= ncol(local_data),
                      N = nvideos,
                      num = winnb$num, 
                      adj = winnb$adj, 
                      weights = winnb$weights
  ))
  
## inits
inits <- function(){list(
    rho = matrix(0, win.data$nspec,win.data$nsite)
  )}
  
# parameters to monitor
params <- c(
    ### binomial regression parameters
  "bpvalue", "fit","fit.new","test",
    "meanP","totSp","nSiteP", "meanSp",
    "b0",
    "p","spacesigma",
    "spacetau", "rho"
    
  )

# call and run winbugs  
samples_dbinV2 <- bugs(data = win.data, parameters.to.save = params, 
                  model.file = here ("bugs","StaticCARModel_coral_dbin_comm_GOF.txt"), 
                  inits = NULL,
                  n.chains = nc, 
                  n.thin = nt, 
                  n.iter = ni, 
                  n.burnin = nb, 
                  DIC = T,
                  bugs.directory = "C:/Program Files (x86)/winbugs14_unrestricted/WinBUGS14",
                  debug=F) ## you don't need close manually if debug = F 
  
save (samples_dbinV2,file=here("output", "StaticCARModel_coral_dbin_commV2.RData"))  

## V3 rounded cover
# binomial model used to estimate the probability of finding a cover higher than 1% 
# relative to total coral cover 

# maximum cover a species could reach
tot_coral_cover <- apply (arranjo_corais,c(1,3),mean, na.rm=T)*100
tot_coral_cover <- rowSums (tot_coral_cover)
tot_coral_cover <- ifelse ((tot_coral_cover)>100,100,  (tot_coral_cover))

## subset dos sitios com cibertura de coral > 0 
coral_sites <- which( tot_coral_cover >0)
# select 
tot_coral_cover_sub <- tot_coral_cover [coral_sites]
tot_coral_cover_sub [tot_coral_cover_sub <1] <- 1
# focal species data 
local_data <- apply(arranjo_corais,c(1,3),mean,na.rm=T)*100
local_data <- local_data [coral_sites,]

##  subset of space (as we can't analyze sites with no coral cover)
#create neighborhood
neigh <- dnearneigh((coord[coral_sites,]), 0, nei)
# Number of neighbours
table(card(neigh))
# Convert the neighbourhood
winnb <- nb2WB(neigh)
local_data[38,15] 
tot_coral_cover_sub [38]

## bundle data
str(win.data<- list(C =  round(local_data),# i[,"y"], 
                    nsite = nrow(local_data),
                    nspec = ncol (local_data),
                    N = round(tot_coral_cover_sub),
                    num = winnb$num, 
                    adj = winnb$adj, 
                    weights = winnb$weights
))


## inits
inits <- function(){list(
  rho = rep(0, win.data$nsite)
)}

# run winbugs
params <- c(
  ### binomial model parameters
  "bpvalue", "fit","fit.new","test",
  "totSp","meanP", "nSiteP", "meanSp",
  "b0", "p","spacesigma",
  "spacetau", "rho"
  
)

samples_dbinV3 <- bugs(data = win.data, parameters.to.save = params, 
                       model.file = here ("bugs","StaticCARModel_coral_dbin_comm_GOF.txt"), 
                       inits = NULL,
                       n.chains = nc, 
                       n.thin = nt, 
                       n.iter = ni, 
                       n.burnin = nb, 
                       DIC = T,
                       bugs.directory = "C:/Program Files (x86)/winbugs14_unrestricted/WinBUGS14",
                       debug=T) ## you don't need close manually if debug = F 

save (samples_dbinV3,file=here("output", "StaticCARModel_coral_dbin_commV3.RData"))

## V4
# binomial - bernoulli model, where the detection is a binomial process, with p dependent on Nvideos,
# and occupancy is a bernoulli process, depending on site spatial neighborhood
  
# the number of videos per site

nvideos <- rowSums (is.na(arranjo_corais [,,1])!=T )
## finding the number of detections
local_data <- do.call (cbind,lapply (seq (1,20), function (i)
  rowSums (arranjo_corais[,,i] >0,na.rm=T)
))

## spatial neighborhood
#create neighborhood
neigh <- dnearneigh((coord), 0, nei)
# Number of neighbours
table(card(neigh))
# Convert the neighbourhood
winnb <- nb2WB(neigh)

## bundle data  
str(win.data<- list(C =  local_data,# i[,"y"], 
                      nsite = nrow(local_data),
                      N = nvideos,
                      nspec = ncol(local_data), 
                      num = winnb$num, 
                      adj = winnb$adj, 
                      weights = winnb$weights
  ))
 
# parameters to monitor
params <- c(
    
    ### bern-bin model
  "bpvalue", "fit","fit.new","test",
    "p","z","psi",  "meanP", "meanPsi", 
    "meanZ", "n.spp.mu","n.spp", "mutot","n.occ",
    "b0",
    "spacetau", "spacesigma","rho"
    
  )

## call and run bugs  
samples_dbin_bern <- bugs(data = win.data, parameters.to.save = params, 
                         model.file = here ("bugs","StaticCARModel_coral_dbin_bern_comm_GOF.txt"), 
                         inits = NULL,
                         n.chains = nc, 
                         n.thin = nt, 
                         n.iter = ni, 
                         n.burnin = nb, 
                         DIC = T,
                         bugs.directory = "C:/Program Files (x86)/winbugs14_unrestricted/WinBUGS14",
                         debug=T) ## you don't need close manually if debug = F 
  
  

save (samples_dbin_bern,file=here("output", "StaticCARModel_coral_dbin_bern_comm.RData"))  

## spatial neighborhodd n = 4

#create neighborhood
neigh <- dnearneigh((coord), 0, 4)
# Number of neighbours
table(card(neigh))
# Convert the neighbourhood
winnb <- nb2WB(neigh)

## bundle data  
str(win.data<- list(C =  local_data,# i[,"y"], 
                    nsite = nrow(local_data),
                    N = nvideos,
                    nspec = ncol(local_data), 
                    num = winnb$num, 
                    adj = winnb$adj, 
                    weights = winnb$weights
))

# parameters to monitor
params <- c(
  
  ### bern-bin model
  "bpvalue", "fit","fit.new","test",
  "p","z","psi",  "meanP", "meanPsi", 
  "meanZ", "n.spp.mu","n.spp", "mutot","n.occ",
  "b0",
  "spacetau", "spacesigma","rho"
  
)

## call and run bugs  
samples_dbin_bern_N4 <- bugs(data = win.data, parameters.to.save = params, 
                             model.file = here ("bugs","StaticCARModel_coral_dbin_bern_comm_GOF.txt"), 
                             inits = NULL,
                             n.chains = nc, 
                             n.thin = nt, 
                             n.iter = ni, 
                             n.burnin = nb, 
                             DIC = T,
                             bugs.directory = "C:/Program Files (x86)/winbugs14_unrestricted/WinBUGS14",
                             debug=F) ## you don't need close manually if debug = F 



save (samples_dbin_bern_N4,file=here("output", "StaticCARModel_coral_dbin_bern_comm_N4.RData"))  

## spatial neighborhood N=9
#create neighborhood
neigh <- dnearneigh((coord), 0, 9)
# Number of neighbours
table(card(neigh))
# Convert the neighbourhood
winnb <- nb2WB(neigh)

## bundle data  
str(win.data<- list(C =  local_data,# i[,"y"], 
                    nsite = nrow(local_data),
                    N = nvideos,
                    nspec = ncol(local_data), 
                    num = winnb$num, 
                    adj = winnb$adj, 
                    weights = winnb$weights
))

# parameters to monitor
params <- c(
  
  ### bern-bin model
  "bpvalue", "fit","fit.new","test",
  "p","z","psi",  "meanP", "meanPsi", 
  "meanZ", "n.spp.mu","n.spp", "mutot","n.occ",
  "b0",
  "spacetau", "spacesigma","rho"
  
)

## call and run bugs  
samples_dbin_bern_N9 <- bugs(data = win.data, parameters.to.save = params, 
                          model.file = here ("bugs","StaticCARModel_coral_dbin_bern_comm_GOF.txt"), 
                          inits = NULL,
                          n.chains = nc, 
                          n.thin = nt, 
                          n.iter = ni, 
                          n.burnin = nb, 
                          DIC = T,
                          bugs.directory = "C:/Program Files (x86)/winbugs14_unrestricted/WinBUGS14",
                          debug=F) ## you don't need close manually if debug = F 


save (samples_dbin_bern_N9,file=here("output", "StaticCARModel_coral_dbin_bern_comm_N9.RData"))  
