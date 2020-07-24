
## Implementação do modelo de ocupação de sítios
## aplicado a espécies de corais

### na parte superior do codigo tem codigos dos modelos em linguagem BUGS
## tem 1 modelo
# 1) modelo de ocupação com probabilidade de ocupacao por coral dependendendo da distancia espacial
# p constante

### na parte inferior do codigo tem as funcoes para rodar o codigo
# ele funciona em listas. Tem duas listas de dados envolvidas
# 1) lista das espécies (ou gênero) de 5 espécies/generos  de coral
# 2) lista de especies de peixes (dados de deteccao e não deteccao em observações em 32 sitios, e em n transecções por sitios)

### PARTE SUPERIOR

# BUGS model
sink(here ("output","StaticCARModel_coral.txt"))

cat("

model {

  # prior for detection probability
	p ~ dunif(0, 1)

  # prior for occupancy submodel
  beta0 ~ dunif (0,1)
  intercept.psi <- logit(beta0)
    
  # CAR prior distribution for spatial random effects:
  rho[1:nsite] ~ car.normal(adj[], weights[], num[], tau)
  
  # hyperprior of rho 
  vrho ~ dnorm(0, 0.5) I(0,)
  tau  <- 1/vrho
  
  # Ecological submodel: Define state conditional on parameters
	
  for(i in 1:nsite){
	
		z [i] ~ dbern(psi[i])
		
		## This keeps the program on the track
    psi[i]<-max(0.00001,min(0.99999, psi0[i]))
    w[i] ~ dbern(psi[i])
               
		logit(psi0 [i]) <- intercept.psi + rho [i]
	
	}
	
	# Observation submodel: detection conditional on true site-occupancy
		
	for (k in 1:nobs) { 		## loop over replicated surveys
		      
	   y [k] ~ dbern(muY[site[k], occa[k]])
		 muY [site[k], occa[k]] <- z[site[k]] * p
			    
	}

  # Derived parameters: Sample and population occupancy, growth rate and turnover
  n.occ <- sum(z[])#/M
  mutot <- sum(psi[])
    

} 
  ",fill = TRUE)

sink()

#### PARTE INFERIOR

### load packages

source("R/packages_model_coral.R")

## Parameters to monitor
  
params <- c(
  
  ### detection parameters
  "p",
  
  ### occupancy parameters
  "z","psi","psi0",
  "beta0","intercept.psi",
   "rho","tau", "vrho",
 
 ## derived par
 "mutot",
 "n.occ"
 
  
)

# MCMC settings
ni <- 100000
nt <- 100
nb <- 80000
nc <- 3
na <- 60000


#############################################
############### load data
#############################################

load (here ("output", "Data_coral_detection.RData"))

# Generate the neighbours
coord <- coordenadas [,c("Lon","Lat")]
coordinates(coord) <- ~ Lon + Lat
neigh <- dnearneigh((coord), 0, 3)
plot(neigh,coord)
# Number of neighbours
table(card(neigh))
# Convert the neighbourhood
winnb <- nb2WB(neigh)

################################
## parallel-processing settings
cl <- makeCluster(2) ## number of cores = generally ncores -1

# exportar pacote para os cores
clusterEvalQ(cl, library(R2WinBUGS))
clusterEvalQ(cl, library(here))

# export your data and function
clusterExport(cl, c("df_coral_data",
                    "winnb",
                    "ni","nt","nb","nc","na",
                    "params"))

samples_coral <- parLapply (cl,df_coral_data, function (i) {
      
  
  #i= df_coral_data[[4]]
  
  str(win.data<- list(y=  i[,"y"], 
                    nsite = max (i[,"M"]),
                    nobs = nrow (i),
                    site = i[,"M"],
                    occa = i [,"J"],
                    num = winnb$num, 
                    adj = winnb$adj, 
                    weights = winnb$weights))

  # Observed occurrence as inits for z
  zst <- aggregate (i[,"y"] , 
                  list (i[,"M"]),
                  FUN=max)$x
  
  ## inits
  inits <- function(){list(z = zst, 
                         rho = rep(0, win.data$nsite))}
  
  # run winbugs

  samples <- bugs(data = win.data, parameters.to.save = params, 
                model.file = here ("output","StaticCARModel_coral.txt"), 
                inits = inits,
                n.chains = nc, 
                n.thin = nt, 
                n.iter = ni, 
                n.burnin = nb, 
                DIC = T,
                bugs.directory = "C:/Program Files (x86)/winbugs14_unrestricted/WinBUGS14",
                debug=F) ## you don't need close manually if debug = F 

  
  ; samples
  
  }
  
)
  
stopCluster(cl)
  
  
samples_coral
  
###############################################################
#### more adjacency
neigh <- dnearneigh((coord),0,6)
plot(neigh,coord)
# Number of neighbours
table(card(neigh))
# Convert the neighbourhood
winnb <- nb2WB(neigh)
  
################################
## parallel-processing settings
cl <- makeCluster(2) ## number of cores = generally ncores -1

# exportar pacote para os cores
clusterEvalQ(cl, library(R2WinBUGS))
clusterEvalQ(cl, library(here))

# export your data and function
clusterExport(cl, c("df_coral_data",
                    "winnb",
                    "ni","nt","nb","nc","na",
                    "params"))

samples_coral_six_adj <- parLapply (cl,df_coral_data, function (i) {
  
  
  #i= df_coral_data[[4]]
  
  str(win.data<- list(y=  i[,"y"], 
                      nsite = max (i[,"M"]),
                      nobs = nrow (i),
                      site = i[,"M"],
                      occa = i [,"J"],
                      num = winnb$num, 
                      adj = winnb$adj, 
                      weights = winnb$weights))
  
  # Observed occurrence as inits for z
  zst <- aggregate (i[,"y"] , 
                    list (i[,"M"]),
                    FUN=max)$x
  
  ## inits
  inits <- function(){list(z = zst, 
                           rho = rep(0, win.data$nsite))}
  
  # run winbugs
  
  samples_six_adj <- bugs(data = win.data, parameters.to.save = params, 
                  model.file = here ("output","StaticCARModel_coral.txt"), 
                  inits = inits,
                  n.chains = nc, 
                  n.thin = nt, 
                  n.iter = ni, 
                  n.burnin = nb, 
                  DIC = T,
                  bugs.directory = "C:/Program Files (x86)/winbugs14_unrestricted/WinBUGS14",
                  debug=F) ## you don't need close manually if debug = F 
  
  
  ; samples_six_adj
  
}

)

stopCluster(cl)


samples_coral_six_adj


###############################################################
#### even more adjacency
neigh <- dnearneigh((coord), 0, 9)
plot(neigh,coord)
# Number of neighbours
table(card(neigh))
# Convert the neighbourhood
winnb <- nb2WB(neigh)

################################
## parallel-processing settings
cl <- makeCluster(2) ## number of cores = generally ncores -1

# exportar pacote para os cores
clusterEvalQ(cl, library(R2WinBUGS))
clusterEvalQ(cl, library(here))

# export your data and function
clusterExport(cl, c("df_coral_data",
                    "winnb",
                    "ni","nt","nb","nc","na",
                    "params"))

samples_coral_nine_adj <- parLapply (cl,df_coral_data, function (i) {
  
  
  #i= df_coral_data[[4]]
  
  str(win.data<- list(y=  i[,"y"], 
                      nsite = max (i[,"M"]),
                      nobs = nrow (i),
                      site = i[,"M"],
                      occa = i [,"J"],
                      num = winnb$num, 
                      adj = winnb$adj, 
                      weights = winnb$weights))
  
  # Observed occurrence as inits for z
  zst <- aggregate (i[,"y"] , 
                    list (i[,"M"]),
                    FUN=max)$x
  
  ## inits
  inits <- function(){list(z = zst, 
                           rho = rep(0, win.data$nsite))}
  
  # run winbugs
  
  samples_nine_adj <- bugs(data = win.data, parameters.to.save = params, 
                          model.file = here ("output","StaticCARModel_coral.txt"), 
                          inits = inits,
                          n.chains = nc, 
                          n.thin = nt, 
                          n.iter = ni, 
                          n.burnin = nb, 
                          DIC = T,
                          bugs.directory = "C:/Program Files (x86)/winbugs14_unrestricted/WinBUGS14",
                          debug=F) ## you don't need close manually if debug = F 
  
  
  ; samples_nine_adj
  
}

)

stopCluster(cl)


samples_coral_nine_adj [[1]]


###############################################################
#### even more more adjacency
neigh <- dnearneigh((coord), 0, 12)
plot(neigh,coord)
# Number of neighbours
table(card(neigh))
# Convert the neighbourhood
winnb <- nb2WB(neigh)

################################
## parallel-processing settings
cl <- makeCluster(2) ## number of cores = generally ncores -1

# exportar pacote para os cores
clusterEvalQ(cl, library(R2WinBUGS))
clusterEvalQ(cl, library(here))

# export your data and function
clusterExport(cl, c("df_coral_data",
                    "winnb",
                    "ni","nt","nb","nc","na",
                    "params"))

samples_coral_twelve_adj <- parLapply (cl,df_coral_data, function (i) {
  
  
  #i= df_coral_data[[4]]
  
  str(win.data<- list(y=  i[,"y"], 
                      nsite = max (i[,"M"]),
                      nobs = nrow (i),
                      site = i[,"M"],
                      occa = i [,"J"],
                      num = winnb$num, 
                      adj = winnb$adj, 
                      weights = winnb$weights))
  
  # Observed occurrence as inits for z
  zst <- aggregate (i[,"y"] , 
                    list (i[,"M"]),
                    FUN=max)$x
  
  ## inits
  inits <- function(){list(z = zst, 
                           rho = rep(0, win.data$nsite))}
  
  # run winbugs
  
  samples_twelve_adj <- bugs(data = win.data, parameters.to.save = params, 
                           model.file = here ("output","StaticCARModel_coral.txt"), 
                           inits = inits,
                           n.chains = nc, 
                           n.thin = nt, 
                           n.iter = ni, 
                           n.burnin = nb, 
                           DIC = T,
                           bugs.directory = "C:/Program Files (x86)/winbugs14_unrestricted/WinBUGS14",
                           debug=F) ## you don't need close manually if debug = F 
  
  
  ; samples_twelve_adj
  
}

)

stopCluster(cl)


samples_coral_twelve_adj [[1]]

###


###############################################################
#### even more more adjacency
neigh <- dnearneigh((coord), 0, 20)
plot(neigh,coord)
# Number of neighbours
table(card(neigh))
# Convert the neighbourhood
winnb <- nb2WB(neigh)

################################
## parallel-processing settings
cl <- makeCluster(2) ## number of cores = generally ncores -1

# exportar pacote para os cores
clusterEvalQ(cl, library(R2WinBUGS))
clusterEvalQ(cl, library(here))

# export your data and function
clusterExport(cl, c("df_coral_data",
                    "winnb",
                    "ni","nt","nb","nc","na",
                    "params"))

samples_coral_twenty_adj <- parLapply (cl,df_coral_data, function (i) {
  
  
  #i= df_coral_data[[4]]
  
  str(win.data<- list(y=  i[,"y"], 
                      nsite = max (i[,"M"]),
                      nobs = nrow (i),
                      site = i[,"M"],
                      occa = i [,"J"],
                      num = winnb$num, 
                      adj = winnb$adj, 
                      weights = winnb$weights))
  
  # Observed occurrence as inits for z
  zst <- aggregate (i[,"y"] , 
                    list (i[,"M"]),
                    FUN=max)$x
  
  ## inits
  inits <- function(){list(z = zst, 
                           rho = rep(0, win.data$nsite))}
  
  # run winbugs
  
  samples_twenty_adj <- bugs(data = win.data, parameters.to.save = params, 
                             model.file = here ("output","StaticCARModel_coral.txt"), 
                             inits = inits,
                             n.chains = nc, 
                             n.thin = nt, 
                             n.iter = ni, 
                             n.burnin = nb, 
                             DIC = T,
                             bugs.directory = "C:/Program Files (x86)/winbugs14_unrestricted/WinBUGS14",
                             debug=F) ## you don't need close manually if debug = F 
  
  
  ; samples_twenty_adj
  
}

)

stopCluster(cl)


###############################################################
#### even more more more more adjacency
neigh <- dnearneigh((coord), 0, 25)
plot(neigh,coord)
# Number of neighbours
table(card(neigh))
# Convert the neighbourhood
winnb <- nb2WB(neigh)

################################
## parallel-processing settings
cl <- makeCluster(2) ## number of cores = generally ncores -1

# exportar pacote para os cores
clusterEvalQ(cl, library(R2WinBUGS))
clusterEvalQ(cl, library(here))

# export your data and function
clusterExport(cl, c("df_coral_data",
                    "winnb",
                    "ni","nt","nb","nc","na",
                    "params"))

samples_coral_twentyfive_adj <- parLapply (cl,df_coral_data, function (i) {
  
  
  #i= df_coral_data[[4]]
  
  str(win.data<- list(y=  i[,"y"], 
                      nsite = max (i[,"M"]),
                      nobs = nrow (i),
                      site = i[,"M"],
                      occa = i [,"J"],
                      num = winnb$num, 
                      adj = winnb$adj, 
                      weights = winnb$weights))
  
  # Observed occurrence as inits for z
  zst <- aggregate (i[,"y"] , 
                    list (i[,"M"]),
                    FUN=max)$x
  
  ## inits
  inits <- function(){list(z = zst, 
                           rho = rep(0, win.data$nsite))}
  
  # run winbugs
  
  samples_twentyfive_adj <- bugs(data = win.data, parameters.to.save = params, 
                             model.file = here ("output","StaticCARModel_coral.txt"), 
                             inits = inits,
                             n.chains = nc, 
                             n.thin = nt, 
                             n.iter = ni, 
                             n.burnin = nb, 
                             DIC = T,
                             bugs.directory = "C:/Program Files (x86)/winbugs14_unrestricted/WinBUGS14",
                             debug=F) ## you don't need close manually if debug = F 
  
  
  ; samples_twentyfive_adj
  
}

)

stopCluster(cl)


samples_coral [[1]]$mean$psi
samples_coral_six_adj [[1]]$mean$psi
samples_coral_nine_adj [[1]]$mean$psi
samples_coral_twelve_adj [[1]]$mean$psi
samples_coral_twenty_adj [[1]]$mean$psi
samples_coral_twentyfive_adj [[1]]$mean$psi

samples_coral [[1]]$sd$psi
samples_coral_six_adj [[1]]$sd$psi
samples_coral_nine_adj [[1]]$sd$psi
samples_coral_twelve_adj [[1]]$sd$psi
samples_coral_twenty_adj [[1]]$sd$psi
samples_coral_twentyfive_adj [[1]]$sd$psi

###
samples_coral [[1]]$mean$z
samples_coral_six_adj [[1]]$mean$z
samples_coral_nine_adj [[1]]$mean$z
samples_coral_twelve_adj [[1]]$mean$z
samples_coral_twenty_adj [[1]]$mean$z
samples_coral_twentyfive_adj [[1]]$mean$z

samples_coral [[1]]$sd$z
samples_coral_six_adj [[1]]$sd$z
samples_coral_nine_adj [[1]]$sd$z
samples_coral_twelve_adj [[1]]$sd$z
samples_coral_twenty_adj [[1]]$sd$z
samples_coral_twentyfive_adj [[1]]$sd$z

###

