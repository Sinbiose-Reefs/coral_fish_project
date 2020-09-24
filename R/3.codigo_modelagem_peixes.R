
## Implementação do modelo de ocupação de sítios
## aplicado a espécies de corais e peixes

### na parte superior do codigo tem codigos dos modelos em linguagem BUGS
## tem 3 modelos
# 1) modelo de ocupação com efeito do coral na ocupação; profundidade e observador na detecção
# 2) modelo de ocupação com efeito do coral na ocupação; profundidade, observador, e random-intercept na detecção
# 3) modelo de ocupação com efeito do coral e profundidade na ocupação; observador e random-intercept na detecção


### na parte inferior do codigo tem as funcoes para rodar o codigo
# ele funciona em listas. Tem duas listas de dados envolvidas
# 1) lista das espécies (ou gênero) de 5 espécies/generos  de coral
# 2) lista de especies de peixes (dados de deteccao e não deteccao em observações em 32 sitios, e em n transecções por sitios)

### PARTE SUPERIOR

## load packages

source("R/packages.R")

#######################################
## modelo com efeito de observador (MODELO 1)
## definir o modelo, em linguagem  JAGS


sink(here ("bugs","StaticModel_ID_obs_comm_NoReg.txt"))
cat("
    
model {
        
        # priors
        
        ## priors for P
        ## observed ID effect
  for (k in 1:nspec) {
           for (o in 1:maxID){ # Implicitly define alpha of obs as a vector
       alpha.obs[k,o] ~ dunif(0,1)
       intercept.p.obs[k,o] <- logit (alpha.obs[k,o])
           }
   }
        
  ## depth effect
  for (k in 1:nspec) {
     for (i in 1:2){ # Implicitly define alpha of depth as a vector
       alpha.depth[k,i] ~ dunif(0,1)
       intercept.depth[k,i] <- logit (alpha.depth[k,i])
     }
   }
  
   ## occupancy priors
        for (k in 1:nspec) {
    #   for (j in 1:nreg) {
         beta0 [k] ~ dunif (0,1)
         intercept.psi [k] <- logit(beta0[k])
      # }
        }

  ## regression coefficient
        for (k in 1:nspec) {
  #   for (j in 1:nreg){
        beta1[k] ~ dnorm (mu.int[k],tau.mu[k])  
  #      ## priors for them
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
    
                  logit(psi0 [i,k]) <- intercept.psi[k] + beta1 [k]* coral [i]# intercept.psi[k,reg[i]] + beta1 [k,reg[i]]
                                                  
                   }
   }
        
                # # # # 
                ####### observation model
                
    for (k in 1:nspec) {
                   
       for (n in 1:nobs) {              ## loop over replicated surveys
                      
                         y [n,k] ~ dbern(muY[site[n], occa[n],k])
                               muY [site[n], occa[n],k] <- z[site[n],k] * p[k,n]
                               logit (p[k,n]) <-  intercept.p.obs [k,obs[n]] + intercept.depth[k,prof[n]]
          
       }

    }
        
        
        ##############################################################
        #                      Goodness of fit
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
    
          y.prov [i,n,k] <- ifelse (site[n] == i, y [n,k],0)## provisorious data
          yrep.prov [i,n,k] <- ifelse (site[n] == i, yrep [n,k],0)## provisorious data
        
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
    
          E.prov [i,n,k] <- ifelse (site[n] == i, tmp [n,k],0) ## provisorious data
          
        }
    
        E [i,k] <- sum (E.prov[i,,k]) ## aggregate data
    
     }
   }
    
    ## discrepancy statistics
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
        
           ##
           # Derived parameters: Sample and population occupancy, growth rate and turnover
           mutot[k] <- sum(psi[1:nsite,k]) ## expected number of occupied sites (infinite sample)
           n.occ[k] <- sum(z[1:nsite,k]) ## finite sample
     mean.p[k] <- mean (p[k,])
  }
     ## richness
     for (i in 1:nsite) {
     
        rich[i] <- sum (z[i,])
     
     }
     
        } # end of the model
        

    ",fill = TRUE)

sink()

### (MODELO 2)
### modelo com random effect
### e
##  com efeito de observador

sink(here("bugs","StaticModel_ID_obsRndmEff_comm.txt"))
cat("
    
    model {
    
    # priors
    
    ## priors for P
    ## observed ID effect
    for (k in 1:nspec) {
       for (o in 1:maxID){ # Implicitly define alpha of p as a vector
          alpha.obs[k,o] ~ dunif(0,1)
          intercept.p.obs[k,o] <- logit (alpha.obs[k,o])
       }
    }
    
    ## depth effect
    for (k in 1:nspec) {
       for (i in 1:2){ # Implicitly define alpha of p as a vector
          alpha.depth[k,i] ~ dunif(0,1)
          intercept.depth[k,i] <- logit (alpha.depth[k,i])
       }
    }
    
    ## occupancy priors
    for (k in 1:nspec) {
       for (j in 1:nreg) {
          beta0[k,j] ~ dunif (0,1)
          intercept.psi[k,j] <- logit(beta0[k,j])
       }
    }

    ## regression coefficient
	  for (k in 1:nspec) {
       for (j in 1:nreg) {
         beta1[k,j] ~ dnorm (mu.int[k,j],tau.mu[k,j])	
         ## priors for them
         mu.int[k,j] ~ dnorm(0, 0.001)
         tau.mu[k,j] <- 1/(sigma.int[k,j]*sigma.int[k,j])
         sigma.int[k,j] ~ dunif(0,10)
       }
    }

    ## probabilidade de deteccao variando por transeccao
    # com fator aleatorio
    for (k in 1:nspec) {
       alpha0[k] ~ dunif (0,1)
       lalpha0[k] <- logit (alpha0[k])
       sd.p[k] ~ dunif (0,10) ## sd of p on logit scale
       tau.p[k] <- pow (sd.p[k],2)## p precision on logit scale
    }

    # one estimate per occasion
    for (k in 1:nspec) {
       for (j in 1:nocca){
          logit (intercept.p [k,j]) <- lp [k,j]
          lp[k,j] ~ dnorm (lalpha0[k],tau.p[k])
    }
    
    # Ecological submodel: Define state conditional on parameters
    for (k in 1:nspec) {
       for(i in 1:nsite){    ## occupancy model
    
          z [i,k] ~ dbern(psi[i,k])
    
       ## This keeps the program on the track
       psi[i,k]<-max(0.00001,min(0.99999, psi0[i,k]))
       
       logit(psi0 [i,k]) <- intercept.psi[k,reg[i]] + beta1 [k,reg[i]] * coral [i]
    
    
       }
    }
    
    # # # # 
    ####### observation model
    for (k in 1:nspec) {
       for (n in 1:nobs) { 		## loop over replicated surveys
    
          y [n,k] ~ dbern(muY[site[n], occa[n],k])
          muY [site[n], occa[n],k] <- z[site[n],k] * p[k,n]
          logit (p[k,n]) <-  intercept.p.obs [k,obs[n]] + 
                             intercept.depth[k,prof[n]] + 
                             intercept.p [k,occa[n]]      
    
       }
    }

	  ##############################################################
	  #                      Goodness of fit
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
    
            y.prov [i,n,k] <- ifelse (site[n] == i, y [n,k],0)## provisorious data
            yrep.prov [i,n,k] <- ifelse (site[n] == i, yrep [n,k],0)## provisorious data
    
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
    
           E.prov [i,n,k] <- ifelse (site[n] == i, tmp [n,k],0) ## provisorious data
    
        }
    
        E [i,k] <- sum (E.prov[i,,k]) ## aggregate data
    
      }
    }
    
    ## discrepancy statistics
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
    
       ##
       # Derived parameters: Sample and population occupancy, growth rate and turnover
       mutot[k] <- sum(psi[1:nsite,k]) ## expected number of occupied sites (infinite sample)
       n.occ[k] <- sum(z[1:nsite,k]) ## finite sample
       mean.p[k] <- mean (p[k,])
       }
    } # end of the model    
    ",fill = TRUE)

sink()

## (MODELO 3)
## 
## modelo com 
## efeito de observador ID no p
## random-intercept P
## coral e profundidade na ocupação

sink(here("bugs","StaticModelDepthOcc_IDobsRdmP.txt"))
cat("
    
    model {
    
    # priors
    ## priors for P
    ## observed ID effect
    for (o in 1:maxID){ # Implicitly define alpha of p as a vector
       alpha.obs[o] ~ dunif(0,1)
       intercept.p.obs[o] <- logit (alpha.obs[o])
    }
    
    ## probabilidade de deteccao variando por transeccao
    # com fator aleatorio
    alpha0 ~ dunif (0,1)
    lalpha0 <- logit (alpha0)
    sd.p ~ dunif (0,10) ## sd of p on logit scale
    tau.p <- pow (sd.p,2)## p precision on logit scale
    
    # one estimate per occasion
    for (j in 1:nocca){
      logit (intercept.p [j]) <- lp [j]
      lp[j] ~ dnorm (lalpha0,tau.p)
    }
    
    ## occupancy priors
    ## depth effect
    for (i in 1:2){ # Implicitly define alpha of p as a vector
       alpha.depth[i] ~ dunif(0,1)
       intercept.depth[i] <- logit (alpha.depth[i])
    }
    
    ## occupancy priors
    ## random coefficient
   	for (j in 1:nreg){
     beta1[j] ~ dnorm (0,0.001)	
	  }

    # Ecological submodel: Define state conditional on parameters
    for(i in 1:nsite){    ## occupancy model
    
       z [i] ~ dbern(psi[i])
    
       ## This keeps the program on the track
       psi[i]<-max(0.00001,min(0.99999, psi0[i]))
       w[i] ~ dbern(psi[i])
    
       logit(psi0 [i]) <- intercept.depth[prof[i]] + beta1[reg[i]] * coral [i]
    
    
    }
    
    # # # # 
    ####### observation model
    
    for (k in 1:nobs) { 		## loop over replicated surveys
    
       y [k] ~ dbern(muY[site[k], occa[k]])
       muY [site[k], occa[k]] <- z[site[k]] * p[k]
       logit (p [k]) <-  intercept.p.obs [obs[k]] + intercept.p [occa[k]]      
      
    }
    
    
    ##############################################################
    #                      Goodness of fit
    ##############################################################
    #       (based on posterior predictive distributions)       #
    #############################################################
    # Draw a replicate data set under the fitted model
    for (k in 1:nobs){
       yrep[k] ~ dbern(muYrep[site [k], occa[k]])
       muYrep [site [k], occa[k]] <- z[site [k]]*p[k]
    }
    
    # Compute detection frequencies for observed and replicated data
    ## the first loop is used to extract data for each site    
    ## The outside function sum is used to aggregate data
    
    for (i in 1:nsite) {
       for (k in 1:nobs) {
    
          y.prov [i,k] <- ifelse (site[k] == i, y [k],0)## provisorious data
          yrep.prov [i,k] <- ifelse (site[k] == i, yrep [k],0)## provisorious data
       }
    
       detfreq [i] <- sum (y.prov[i,]) ## aggregate data
       detfreqrep [i] <- sum (yrep.prov[i,]) ## aggregate data
    
    }
    
    # Expected detection frequencies under the model
    for (k in 1:nobs){
       tmp[k] <- z[site[k]] * p[k]
    }
    
    ## the first loop is used to extract data for each site    
    ## The outside function sum is used to aggregate data
    
    for (i in 1:nsite) {
       for (k in 1:nobs) {
    
           E.prov [i,k] <- ifelse (site[k] == i, tmp [k],0) ## provisorious data
    
       }
    
       E [i] <- sum (E.prov[i,]) ## aggregate data
    
    } 
    
    ## discrepancy statistics
    for (i in 1:nsite) {
    
       # Chi-square and Freeman-Tukey discrepancy measures
       # ..... for actual data set
       x2Closed[i] <- pow((detfreq[i] - E[i]),2) / (E[i]+e)
       ftClosed[i] <- pow((sqrt(detfreq[i]) - sqrt(E[i])),2)
       # ..... for replicated data set
       x2repClosed[i] <- pow((detfreqrep[i] - E[i]),2) / (E[i]+e)
       ftrepClosed[i] <- pow((sqrt(detfreqrep[i]) - sqrt(E[i])),2)
    
    }
    
    
    # Add up Chi-square and FT discrepancies and compute fit stat ratio
    # (closed part)
    Chi2Closed <- sum(x2Closed[1:nsite])
    FTClosed <- sum(ftClosed[1:nsite])
    Chi2repClosed <- sum(x2repClosed[1:nsite])
    FTrepClosed <- sum(ftrepClosed[1:nsite])
    Chi2ratioClosed <- Chi2Closed / Chi2repClosed
    FTratioClosed <- FTClosed / FTrepClosed
    
    ##
    # Derived parameters: Sample and population occupancy, growth rate and turnover
    mutot <- sum(psi[1:nsite]) ## expected number of occupied sites (infinite sample)
    n.occ <- sum(z[1:nsite]) ## finite sample
    mean.p <- mean (p)
    
    } # end of the model
    
    ",fill = TRUE)

sink()


## (MODELO 4)
## 
## modelo com 
## efeito de observador ID no p
## random-intercept P
## coral na ocupação
## nao inclui profundidade em nenhum dos modelos

sink(here("bugs","StaticModelOcc_IDobsRdmP.txt"))
cat("
    
    model {
    
    # priors
    ## priors for P
    ## observed ID effect
    for (o in 1:maxID){ # Implicitly define alpha of p as a vector
       alpha.obs[o] ~ dunif(0,1)
       intercept.p.obs[o] <- logit (alpha.obs[o])
    }
    
    ## probabilidade de deteccao variando por transeccao
    # com fator aleatorio
    alpha0 ~ dunif (0,1)
    lalpha0 <- logit (alpha0)
    sd.p ~ dunif (0,10) ## sd of p on logit scale
    tau.p <- pow (sd.p,2)## p precision on logit scale
    
    # one estimate per occasion
    for (j in 1:nocca){
       logit (intercept.p [j]) <- lp [j]
       lp[j] ~ dnorm (lalpha0,tau.p)
    }
    
    ## occupancy priors
    beta0 ~ dunif (0,1)
    intercept.psi <- logit(beta0)
    
    ## random coefficient
  	for (j in 1:nreg){
      beta1[j] ~ dnorm (0,0.001)	
	  }

    # Ecological submodel: Define state conditional on parameters
    for(i in 1:nsite){    ## occupancy model
    
       z [i] ~ dbern(psi[i])
    
       ## This keeps the program on the track
       psi[i]<-max(0.00001,min(0.99999, psi0[i]))
       w[i] ~ dbern(psi[i])
    
       logit(psi0 [i]) <- intercept.psi + beta1[reg[i]] * coral [i]
    
    
    }
    
    # # # # 
    ####### observation model
    
    for (k in 1:nobs) { 		## loop over replicated surveys
    
       y [k] ~ dbern(muY[site[k], occa[k]])
       muY [site[k], occa[k]] <- z[site[k]] * p[k]
       logit (p [k]) <-  intercept.p.obs [obs[k]] + intercept.p [occa[k]]      
    
    }
    
    
    ##############################################################
    #                      Goodness of fit
    ##############################################################
    #       (based on posterior predictive distributions)       #
    #############################################################
    # Draw a replicate data set under the fitted model
    for (k in 1:nobs){
       yrep[k] ~ dbern(muYrep[site [k], occa[k]])
       muYrep [site [k], occa[k]] <- z[site [k]]*p[k]
    }
    
    # Compute detection frequencies for observed and replicated data
    ## the first loop is used to extract data for each site    
    ## The outside function sum is used to aggregate data
    
    for (i in 1:nsite) {
       for (k in 1:nobs) {
    
          y.prov [i,k] <- ifelse (site[k] == i, y [k],0)## provisorious data
          yrep.prov [i,k] <- ifelse (site[k] == i, yrep [k],0)## provisorious data
       }
    
       detfreq [i] <- sum (y.prov[i,]) ## aggregate data
       detfreqrep [i] <- sum (yrep.prov[i,]) ## aggregate data
    
    }
    
    # Expected detection frequencies under the model
    for (k in 1:nobs){
       tmp[k] <- z[site[k]] * p[k]
    }
    
    ## the first loop is used to extract data for each site    
    ## The outside function sum is used to aggregate data
    
    for (i in 1:nsite) {
       for (k in 1:nobs) {
    
          E.prov [i,k] <- ifelse (site[k] == i, tmp [k],0) ## provisorious data
    
       }
    
       E [i] <- sum (E.prov[i,]) ## aggregate data
    
    } 
    
    ## discrepancy statistics
    for (i in 1:nsite) {
    
       # Chi-square and Freeman-Tukey discrepancy measures
       # ..... for actual data set
       x2Closed[i] <- pow((detfreq[i] - E[i]),2) / (E[i]+e)
       ftClosed[i] <- pow((sqrt(detfreq[i]) - sqrt(E[i])),2)
       # ..... for replicated data set
       x2repClosed[i] <- pow((detfreqrep[i] - E[i]),2) / (E[i]+e)
       ftrepClosed[i] <- pow((sqrt(detfreqrep[i]) - sqrt(E[i])),2)
    
    }
    
    
    # Add up Chi-square and FT discrepancies and compute fit stat ratio
    # (closed part)
    Chi2Closed <- sum(x2Closed[1:nsite])
    FTClosed <- sum(ftClosed[1:nsite])
    Chi2repClosed <- sum(x2repClosed[1:nsite])
    FTrepClosed <- sum(ftrepClosed[1:nsite])
    Chi2ratioClosed <- Chi2Closed / Chi2repClosed
    FTratioClosed <- FTClosed / FTrepClosed
    
    ##
    # Derived parameters: Sample and population occupancy, growth rate and turnover
    mutot <- sum(psi[1:nsite]) ## expected number of occupied sites (infinite sample)
    n.occ <- sum(z[1:nsite]) ## finite sample
    mean.p <- mean (p)
    
    } # end of the model
    
    ",fill = TRUE)

sink()


#### PARTE INFERIOR

#############################################
############### load data
#############################################

## fish data
load (here("output","Data_fish_detection.RData"))

## coral data
load (here("output","Data_coral_detection.RData"))

##### cenarios de perda de cobertura de coral
## descontar de 20 a 80% da cobertura observada

coral_cover_data <- lapply (seq(1,ncol(sp_cover_data)), function (i) {

 coral_cover <- cbind (original= sp_cover_data[,i], 
      less20 = sp_cover_data[,i] * 0.80,
      less40 = sp_cover_data[,i] * 0.60,
      less60 = sp_cover_data[,i] * 0.40,
      less80 = sp_cover_data[,i] * 0.20)
})


################
# MCMC settings
ni <- 30000
nt <- 20
nb <- 28000
nc <- 3
na <- 25000

###### modelo com 
# efeito de coral no psi
# efeito do obs no P
# profundidade no P

## Parameters to monitor
params <- c(
  ### detection parameters
  "alpha.obs", "alpha.depth",
  "intercept.p.obs", "intercept.depth",
  
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

### aplicar o modelo a cada especie de peixe e coral

cl <- makeCluster(2) ## number of cores = generally ncores -1

# exportar pacote para os cores
clusterEvalQ(cl, library(jagsUI))
clusterEvalQ(cl, library(vegan))
clusterEvalQ(cl, library(here))

# export your data and function
clusterExport(cl, c("df_fish_data_per_coral", 
                    "covariates_site",
                    "coral_cover_data",
                    "ni","nt","nb","nc","na",
                    "params"))

### run in parallel processing
## aplicar o modelo para todas as especies de coral e de peixes.
samples_OCCcoral_PdepthObsID <- parLapply (cl, seq(1,length(coral_cover_data)), function (coral)
  
  lapply (seq(1,ncol(coral_cover_data[[coral]])), function (scenario) {
    
    ## data- [,,1] pq eh tudo igual
    str(jags.data<- list(y= df_fish_data_per_coral [[coral]] [,"y",], 
                         nspec = dim(df_fish_data_per_coral [[coral]] [,"y",])[2],
                         nsite = max (df_fish_data_per_coral [[coral]] [,"M",1]),
                         prof= df_fish_data_per_coral [[coral]] [,"prof",1],
                         nobs = nrow (df_fish_data_per_coral [[coral]] [,,1]),
                         nreg = 3,
                         reg = as.numeric(as.factor(covariates_site$region)),
                         obs = df_fish_data_per_coral [[coral]] [,"ID",1],
                         maxID = max(df_fish_data_per_coral [[coral]] [,"ID",1]),
                         #nocca = max(df_fish_data[[i]]$J),
                         site = df_fish_data_per_coral [[coral]] [,"M",1],
                         occa = df_fish_data_per_coral[[coral]] [,"J",1],
                         coral= ((coral_cover_data[[coral]]-mean(coral_cover_data[[coral]]))/sd(coral_cover_data[[coral]]))[,scenario],
                         e= 0.0001))
  

    ## inits
    zst <- matrix(1,nrow=jags.data$nsite,
                  ncol=jags.data$nspec)#aggregate (df_fish_data_per_coral[[coral]] [,"y",fish] , 
            #     list (df_fish_data_per_coral[[coral]] [,"M",fish]),
             #    FUN=max)$x
    
    # Observed occurrence as inits for z
    #zst[zst == '-Inf'] <- 1 # max of c(NA,NA,NA) with na.rm = TRUE returns -Inf, change to 1
    inits <- function(){list(z = zst)}

    # run jags
      
    samples <- jags(data = jags.data, params, 
                    model = here ("bugs","StaticModel_ID_obs_comm_NoReg.txt"), inits = inits,
                      n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                      DIC = T,parallel = F)
    
    
      }
    )
  )


stopCluster(cl)

save(samples_OCCcoral_PdepthObsID, file=here("output","samples_OCCcoral_PdepthObsID.RData"))

### EXAMINE RESULTS
## chi-square statistics
Chi2ratioClosed <- lapply (samples_OCCcoral_PdepthObsID, function (i)
  lapply (i, function (k)
    k$sims.list$Chi2Closed/k$sims.list$Chi2repClosed))

## bayesian p-value

bvclosed <- lapply (samples_OCCcoral_PdepthObsID, function (i)
  lapply (i, function (j)
  (unlist(lapply (j, function (k)
    
    sum (k$sims.list$Chi2repClosed > k$sims.list$Chi2Closed)/
      length(k$sims.list$Chi2repClosed)
  )))))

plot(samples_OCCcoral_PdepthObsID[[2]][[1]][[10]]$sims.list$Chi2Closed, 
     samples_OCCcoral_PdepthObsID[[2]][[1]][[10]]$sims.list$Chi2repClosed)
abline(1,1)


###### modelo com 
# efeito de coral no psi
# efeito do obs no P
# profundidade no P
# random-intercept P

## Parameters to monitor
params <- c(
  ### detection parameters
  "alpha.obs", "alpha.depth",
  "intercept.p.obs", "intercept.depth",
  "intercept.p",
  "sd.p","alpha0",
  
  ### occupancy parameters
  "beta0","intercept.psi",
  "beta1", "psi",
  
  ## goodness of fit parameters
  "FTratioClosed",
  "Chi2Closed",
  "Chi2repClosed",
  
  ## derived par
  "mutot",
  "n.occ",
  "mean.p"
)

### aplicar o modelo a cada especie e especie de coral

require(parallel)

cl <- makeCluster(2) ## number of cores = generally ncores -1

# exportar pacote para os cores
clusterEvalQ(cl, library(jagsUI))
clusterEvalQ(cl, library(vegan))
clusterEvalQ(cl, library(here))

# export your data and function
clusterExport(cl, c("df_fish_data", 
                    "covariates_site",
                    "list_coral_data",
                    "ni","nt","nb","nc","na",
                    "params"))

### run in parallel processing
## aplicar o modelo para todas as especies de coral e de peixes.
samples_OCCcoral_PdepthObsIDRndm <- parLapply (cl, list_coral_data, function (coral)
  
  lapply (seq(1,ncol(coral)), function (k)
    
    lapply (seq (1,length (df_fish_data)), function (i) {
      
    ## data
    jags.data<- list(y= df_fish_data [[i]][,"y"], 
                    nsite = max (df_fish_data [[i]][,"M"]),
                    prof= df_fish_data [[i]]$prof,
                    nobs = nrow (df_fish_data [[i]]),
                    nreg = 2,
                    reg = covariates_site$NE_region+1,
                    obs = df_fish_data [[i]]$ID,
                    maxID = max(df_fish_data [[i]]$ID),
                    nocca = max(df_fish_data[[i]]$J),
                    site = df_fish_data [[i]]$M,
                    occa = df_fish_data [[i]]$J,
                    coral= decostand (coral[,k],"standardize")[,1],
                    e= 0.0001)
    
    
    ## inits
    ## inits
    zst <- aggregate (df_fish_data[[i]][,"y"] , 
                      list (df_fish_data[[i]][,"M"]),
                      FUN=max)$x
    
    zst[zst == '-Inf'] <- 1 # max of c(NA,NA,NA) with na.rm = TRUE returns -Inf, change to 1
    inits <- function(){list(z = zst)}
    
    # run jags
    
    samples <- jags(data = jags.data, params, model = here("bugs","StaticModel_ID_obsRndmEff.txt"), inits = inits,
                    n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                    DIC = T)
    }
  )
 )
)


stopCluster(cl)

save(samples_OCCcoral_PdepthObsIDRndm, file=here("output","samples_OCCcoral_PdepthObsIDRndm.RData"))

###### modelo com 
# efeito de coral no psi
# efeito da depth no psi
# efeito do obs no P
# random-intercept P

## Parameters to monitor
params <- c(
  ### detection parameters
  "alpha.obs", "alpha.depth",
  "intercept.p.obs", 
  "intercept.p",
  "sd.p","alpha0",
  
  ### occupancy parameters
  "beta0","intercept.depth", "intercept.psi",
  "beta1", "psi",
  
  ## goodness of fit parameters
  "FTratioClosed",
  "Chi2Closed",
  "Chi2repClosed",
  
  ## derived par
  "mutot",
  "n.occ",
  "mean.p"
)

### aplicar o modelo a cada especie e especie de coral

require(parallel)

cl <- makeCluster(2) ## number of cores = generally ncores -1

# exportar pacote para os cores
clusterEvalQ(cl, library(jagsUI))
clusterEvalQ(cl, library(vegan))
clusterEvalQ(cl, library(here))

# export your data and function
clusterExport(cl, c("df_fish_data", 
                    "covariates_site",
                    "list_coral_data",
                    "ni","nt","nb","nc","na",
                    "params"))

### run in parallel processing
## aplicar o modelo para todas as especies de coral e de peixes.
samples_OCCcoralDepth_PObsIDRndm <- parLapply (cl, list_coral_data, function (coral)
  
  lapply (seq(1,ncol(coral)), function (k)
    
    lapply (seq (1,length (df_fish_data)), function (i) {
      
      ## data
      jags.data<- list(y= df_fish_data [[i]][,"y"], 
                      nsite = max (df_fish_data [[i]][,"M"]),
                      prof= df_fish_data [[i]]$prof,
                      nobs = nrow (df_fish_data [[i]]),
                      nreg = 2,
                      reg = covariates_site$NE_region+1,
                      obs = df_fish_data [[i]]$ID,
                      maxID = max(df_fish_data [[i]]$ID),
                      nocca = max(df_fish_data[[i]]$J),
                      site = df_fish_data [[i]]$M,
                      occa = df_fish_data [[i]]$J,
                      coral= decostand (coral[,k],"standardize")[,1],
                      e= 0.0001)
    
    ## inits
      zst <- aggregate (df_fish_data[[i]][,"y"] , 
                        list (df_fish_data[[i]][,"M"]),
                        FUN=max)$x
      
      zst[zst == '-Inf'] <- 1 # max of c(NA,NA,NA) with na.rm = TRUE returns -Inf, change to 1
      inits <- function(){list(z = zst)}
    
    # run jags
    
    samples <- jags(data = jags.data, params, model = here("bugs","StaticModelDepthOcc_IDobsRdmP.txt"), inits = inits,
                    n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                    DIC = T)
    }
  )
 )
)

stopCluster(cl)

save(samples_OCCcoralDepth_PObsIDRndm, file=here("output","samples_OCCcoralDepth_PObsIDRndm.RData"))


### EXAMINE RESULTS
## chi-square statistics
Chi2ratioClosed <- lapply (samples_OCCcoralDepth_PObsIDRndm, function (i)
  lapply (i, function (k)
    k$sims.list$Chi2Closed/k$sims.list$Chi2repClosed))

## bayesian p-value

bvclosed <- lapply (samples_OCCcoralDepth_PObsIDRndm, function (i)
  (unlist(lapply (i, function (k)
    
    sum (k$sims.list$Chi2repClosed > k$sims.list$Chi2Closed)/
      length(k$sims.list$Chi2repClosed)
  ))))

plot(samples_OCCcoralDepth_PObsIDRndm[[1]][[13]]$sims.list$Chi2Closed, 
     samples_OCCcoralDepth_PObsIDRndm[[1]][[13]]$sims.list$Chi2repClosed)
abline(1,1)


###### modelo com 
# efeito de coral no psi
# efeito do obs no P
# random-intercept P
# sem profundidade

## Parameters to monitor
params <- c(
  ### detection parameters
  "alpha.obs", 
  "intercept.p.obs", 
  "intercept.p",
  "sd.p","alpha0",
  
  ### occupancy parameters
  "beta0","intercept.psi",
  "beta1", "psi",
  
  ## goodness of fit parameters
  "FTratioClosed",
  "Chi2Closed",
  "Chi2repClosed",
  
  ## derived par
  "mutot",
  "n.occ",
  "mean.p"
)

### aplicar o modelo a cada especie e especie de coral

require(parallel)

cl <- makeCluster(2) ## number of cores = generally ncores -1


# exportar pacote para os cores
clusterEvalQ(cl, library(jagsUI))
clusterEvalQ(cl, library(vegan))
clusterEvalQ(cl, library(here))

# export your data and function
clusterExport(cl, c("df_fish_data", 
                    "covariates_site",
                    "list_coral_data",
                    "ni","nt","nb","nc","na",
                    "params"))

### run in parallel processing
## aplicar o modelo para todas as especies de coral e de peixes.
StaticModelOccCoral_IDobsRdmP <- parLapply (cl, list_coral_data, function (coral)
  
  lapply (seq(1,ncol(coral)), function (k)
    
    lapply (seq (1,length (df_fish_data)), function (i) {
      
      ## data
      jags.data<- list(y= df_fish_data [[i]][,"y"], 
                      nsite = max (df_fish_data [[i]][,"M"]),
                      prof= df_fish_data [[i]]$prof,
                      nobs = nrow (df_fish_data [[i]]),
                      nreg = 2,
                      reg = covariates_site$NE_region+1,
                      obs = df_fish_data [[i]]$ID,
                      maxID = max(df_fish_data [[i]]$ID),
                      nocca = max(df_fish_data[[i]]$J),
                      site = df_fish_data [[i]]$M,
                      occa = df_fish_data [[i]]$J,
                      coral= decostand (coral[,k],"standardize")[,1],
                      e= 0.0001)
      
    ## inits
      zst <- aggregate (df_fish_data[[i]][,"y"] , 
                        list (df_fish_data[[i]][,"M"]),
                        FUN=max)$x
      
      zst[zst == '-Inf'] <- 1 # max of c(NA,NA,NA) with na.rm = TRUE returns -Inf, change to 1
    inits <- function(){list(z = zst)}
    
    # run jags
    
    samples <- jags(data = jags.data, params, model = here("bugs","StaticModelOcc_IDobsRdmP.txt"), inits = inits,
                    n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                    DIC = T)
  }
  )
 )
)


stopCluster(cl)

save(StaticModelOccCoral_IDobsRdmP, file=here("output","StaticModelOccCoral_IDobsRdmP.RData"))



