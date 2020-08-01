
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

source("R/packages_model_fishes.R")

#######################################
## modelo com efeito de observador (MODELO 1)
## definir o modelo, em linguagem  JAGS


sink(here ("bugs","StaticModel_ID_obs.txt"))
cat("
    
model {
	
	# priors
	
	## priors for P
	## observed ID effect
	for (o in 1:maxID){ # Implicitly define alpha of p as a vector
    alpha.obs[o] ~ dunif(0,1)
    intercept.p.obs[o] <- logit (alpha.obs[o])
	}
	
  ## depth effect
  for (i in 1:2){ # Implicitly define alpha of p as a vector
    alpha.depth[i] ~ dunif(0,1)
    intercept.depth[i] <- logit (alpha.depth[i])
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
			    logit (p [k]) <-  intercept.p.obs [obs[k]] + intercept.depth[prof[k]]
          
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

### (MODELO 2)
### modelo com random effect
### e
##  com efeito de observador

sink(here("bugs","StaticModel_ID_obsRndmEff.txt"))
cat("
    
    model {
    
    # priors
    
    ## priors for P
    ## observed ID effect
    for (o in 1:maxID){ # Implicitly define alpha of p as a vector
       alpha.obs[o] ~ dunif(0,1)
       intercept.p.obs[o] <- logit (alpha.obs[o])
    }
    
    ## depth effect
    for (i in 1:2){ # Implicitly define alpha of p as a vector
       alpha.depth[i] ~ dunif(0,1)
       intercept.depth[i] <- logit (alpha.depth[i])
    }
    
    ## occupancy priors
    beta0 ~ dunif (0,1)
    intercept.psi <- logit(beta0)
    
    ## random coefficient
	  for (j in 1:nreg){
      beta1[j] ~ dnorm (0,0.001)	
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
    
    # Ecological submodel: Define state conditional on parameters
    for(i in 1:nsite){    ## occupancy model
    
       z [i] ~ dbern(psi[i])
    
       ## This keeps the program on the track
       psi[i]<-max(0.00001,min(0.99999, psi0[i]))
       w[i] ~ dbern(psi[i])
    
       logit(psi0 [i]) <- intercept.psi + beta1 [reg[i]] * coral [i]
    
    
    }
    
    # # # # 
    ####### observation model
    
    for (k in 1:nobs) { 		## loop over replicated surveys
    
       y [k] ~ dbern(muY[site[k], occa[k]])
       muY [site[k], occa[k]] <- z[site[k]] * p[k]
       logit (p [k]) <-  intercept.p.obs [obs[k]] + 
                         intercept.depth[prof[k]] + 
                         intercept.p [occa[k]]      
    
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
load (here("output","coral_occupancy_data.RData"))

## descontar 25% e 50% da cobertura observada e da probabilidade de ocupacao dos corais em cada sitio 

perc25less <- lapply (list_coral_data, function (i)
                         i * 0.75
                      )

perc50less <- lapply (list_coral_data, function (i)
  i * 0.50
)

## colar na lista com as tabelas dos dados originais
list_coral_data <-  c (list_coral_data ,    
                       perc25less,
                       perc50less)
## ajustar os nomes
names (list_coral_data) <- c("cob_original", "ocupacao_original",
                             "cob_desc25", "ocupacao_desc25",
                             "cob_desc50", "ocupacao_desc50")

################
# MCMC settings
ni <- 50000
nt <- 50
nb <- 30000
nc <- 3
na <- 20000

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

cl <- makeCluster(4) ## number of cores = generally ncores -1

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
samples_OCCcoral_PdepthObsID <- parLapply (cl, list_coral_data, function (coral)
  
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
                     #nocca = max(df_fish_data[[i]]$J),
                     site = df_fish_data [[i]]$M,
                     occa = df_fish_data [[i]]$J,
                     coral= decostand (coral[,k],"standardize")[,1],
                     e= 0.0001)
  

    ## inits
    zst <- aggregate (df_fish_data[[i]][,"y"] , 
                 list (df_fish_data[[i]][,"M"]),
                 FUN=max)$x
    
      # Observed occurrence as inits for z
    zst[zst == '-Inf'] <- 1 # max of c(NA,NA,NA) with na.rm = TRUE returns -Inf, change to 1
    inits <- function(){list(z = zst,beta1=rep(0,2))}

      # run jags
      
      samples <- jags(data = jags.data, params, model = here ("bugs","StaticModel_ID_obs.txt"), inits = inits,
                      n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, 
                      DIC = T)
      }
    )
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

cl <- makeCluster(4) ## number of cores = generally ncores -1

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



