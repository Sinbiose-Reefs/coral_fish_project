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

##

### binomial model
sink(here ("bugs","StaticCARModel_coral_teste_comm_GOF.txt"))

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
    C[i,k] ~ dbin(mu[i,k],N[i,k])
    #p[i,k] <- mu[i,k]#*phi[i,k]
    #q[i,k] <- phi[i,k]-mu[i,k]*phi[i,k] # (1-mu[i,k])*phi[i,k]  # 
    ## or an alternative parameterization: phi[i]-mu.s[i]*phi[i]
    
    # model for average cover
    logit(mu0[i,k])<- b0[k] + rho[k,i]
    # keep cover on the track
    mu.lim[i,k] <- min(10,max(-10,mu0[i,k]))
    logit(mu[i,k]) <- mu.lim[i,k]
    
    # precision model
    #phi[i,k] <- exp(c0[k] + c1[k]*nvideos[i])
    
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
    meanCov [k] <- mean(mu[,k]) 
    }
    
    # total cover per site
    for (i in 1:nsite) {
    totCov [i] <- sum(mu[i,]) 
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
sink(here ("bugs","StaticCARModel_coral_dbin_comm_GOFb.txt"))

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
   #for (k in 1:nspec) {
  #    meanP [k] <- mean(p[,k]) 
   #}

   # number of sites per species
   #for (k in 1:nspec) {
  #    nSiteP [k] <- sum(p[,k]) 
   #}

   # expected number of species per site
   #for (i in 1:nsite) {
  #    totSp [i] <- sum(p[i,]) 
   #}

   # mean number of species per site
   #for (i in 1:nsite) {
  #    meanSp [i] <- mean(p[i,]) 
   #}

}
    ",fill = TRUE)

sink()

# binomial bernoulli model
## species as fixed effects

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


# binomial bernoulli model 
# species as random effects
### binomial model
sink(here ("bugs","StaticCARModel_coral_dbin_bern_rdm_comm_GOF.txt"))

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
       b0[k] ~ dnorm(mu.int,tau.mu)
       ## detection probability
       mu.p[k] ~ dnorm(p.int,tau.p)
      }
   
    mu.int ~ dnorm(0, 0.001)
    tau.mu <- 1/(sigma.int*sigma.int)
    sigma.int ~ dunif(0,10)
    p.int ~ dnorm(0, 0.001)
    tau.p <- 1/(sigma.p*sigma.p)
    sigma.p ~ dunif(0,10)
    
    ## likelihood
    for (k in 1:nspec) { # for each species
       logit(p[k]) <- mu.p[k]

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

###############################
## model to estimate coral cover

## cobertura total do video k no sitio i
cover_data <- apply(arranjo_corais,c(1,2),sum,na.rm=T) 
## cobertura de cada sp no video relativo ao total, using for =[

shell_array <- array (NA,dim = dim(arranjo_corais)) 

for (i in 1:dim(arranjo_corais)[3]) {
  
  shell_array[,,i] <- arranjo_corais [,,i]/cover_data
  
}

## maximo de cobertura detectado em um video
sp_cover_data <- apply (shell_array,c(1,3),max,na.rm=T)
# infinite  eh gerado para os locais onde nao tem  nada de coral
sp_cover_data [is.infinite (sp_cover_data)] <-  0

# subset_sites
subset_sites <- which(rowSums (sp_cover_data) >0)
## removendo sitios sem cobertura alguma de coral
# sp_cover_data <- sp_cover_data#[subset_sites,]
#sp_cover_data_sub[which(sp_cover_data_sub ==0)] <- 0.00001

#neigh_winnb <- lapply (as.list(c(3,6,9,12,15,18,21,24)), function (i)  {
nei<-6
#create neighborhood
neigh <- dnearneigh((coord), 0, nei) # 

# Number of neighbours
table(card(neigh))
# Convert the neighbourhood
winnb <- nb2WB(neigh)

# standardize number of videos  
nvideos <- rowSums(is.na(arranjo_corais[,,1])!=T)#[which(rowSums (sp_cover_data)>0)]
std_videos <- (nvideos-mean(nvideos))/sd(nvideos)
str(win.data<- list(C =  sp_cover_data_sub ,# i[,"y"], 
                    nspec = ncol (sp_cover_data_sub ),
                    nsite = nrow (sp_cover_data_sub ),
                    N = cover_data,
                    num = winnb$num, 
                    adj = winnb$adj, 
                    weights = winnb$weights
                    ))

## inits for the spatial factor
inits <- function(){list(
  w = rep (1,48),
  rho = matrix (1, nrow=ncol(sp_cover_data_sub),
                ncol=nrow(sp_cover_data_sub))
  )} #rep(0, win.data$nsite)
                           
# run winbugs
## Parameters to monitor
  
params <- c(
  ### cover-model parameters
    #"bpvalue", "fit","fit.new","test",
    "meanCov","totCov","meanCovmu","totCovmu",
    "b0",#"c0","c1",
    "mu","spacesigma",
    "spacetau", "rho"
    )
  
## call and run winBUGS  
samples <- bugs(data = win.data, parameters.to.save = params, 
                model.file = here ("bugs","StaticCARModel_coral_teste_comm_GOF.txt"), 
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

# maximum cover a site could have
tot_cover <- rep(1,48) # apply (arranjo_corais,c(1,3),max, na.rm=T)

## bundle data
str(win.data<- list(C =  (sp_cover_data),# i[,"y"], 
                      nsite = nrow(sp_cover_data),
                      nspec = ncol (sp_cover_data),
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
                  debug=T) ## you don't need close manually if debug = F 
  
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

neigh <- dnearneigh((coord), 0, nei)#[subset_sites,]
# Number of neighbours
table(card(neigh))
# Convert the neighbourhood
winnb <- nb2WB(neigh)
## bundle data
str(win.data<- list(C =  round (sp_cover_data*100),# i[,"y"], 
                    nsite = nrow(local_data),
                    nspec = ncol (local_data),
                    N = round(),
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
                       model.file = here ("bugs","StaticCARModel_coral_dbin_comm_GOFb.txt"), 
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

## species as random effects

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
  "tau.mu","tau.p",
  "bpvalue", "fit","fit.new","test",
  "p","z","psi",  "meanP", "meanPsi", 
  "meanZ", "n.spp.mu","n.spp", "mutot","n.occ",
  "b0",
  "spacetau", "spacesigma","rho"
  
)

## call and run bugs  
samples_dbin_bern_RDM <- bugs(data = win.data, parameters.to.save = params, 
                          model.file = here ("bugs","StaticCARModel_coral_dbin_bern_rdm_comm_GOF.txt"), 
                          inits = NULL,
                          n.chains = nc, 
                          n.thin = nt, 
                          n.iter = ni, 
                          n.burnin = nb, 
                          DIC = T,
                          bugs.directory = "C:/Program Files (x86)/winbugs14_unrestricted/WinBUGS14",
                          debug=T) ## you don't need close manually if debug = F 


save (samples_dbin_bern_RDM,file=here("output", "StaticCARModel_coral_dbin_bern_rdm_comm_GOF.RData"))  













