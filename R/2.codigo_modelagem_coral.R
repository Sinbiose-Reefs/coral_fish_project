
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
  vrho ~ dnorm(0, 0.2) I(0,)
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

#############################################
############### load data
#############################################

load (here ("output", "Data_coral_detection.RData"))

# Generate the neighbours
coord <- coordenadas [,c("Lon","Lat")]
coordinates(coord) <- ~ Lon + Lat
neigh <- dnearneigh((coord), 0, 5)
plot(neigh,coord)
# Number of neighbours
table(card(neigh))
# Convert the neighbourhood
winnb <- nb2WB(neigh)

## Parameters to monitor
  
params <- c(
  
  ### detection parameters
  "p",
  
  ### occupancy parameters
  "z","psi",
  "beta0","intercept.psi",
   "rho","tau", "vrho",
 
 ## derived par
 "mutot",
 "n.occ"
 
  
)


## data
i=1
str(win.data<- list(y= df_coral_data [[i]][,"y"], 
                    nsite = max (df_coral_data [[i]][,"M"]),
                    nobs = nrow (df_coral_data [[i]]),
                    #nocca = max(df_coral_data [[i]]$J),
                    site = df_coral_data [[i]]$M,
                    occa = df_coral_data [[i]]$J,
                    num = winnb$num, 
                    adj = winnb$adj, 
                    weights = winnb$weights))

# Observed occurrence as inits for z
zst <- aggregate (df_coral_data [[i]][,"y"] , list (df_coral_data [[i]][,"M"]),
           FUN=max)$x
  
## inits
inits <- function(){list(z = zst, 
                         rho = rep(0, win.data$nsite))}

################
# MCMC settings
ni <- 50000
nt <- 10
nb <- 40000
nc <- 3
na <- 30000

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
                debug=T)

  
samples
  
  
  
  
  
  
  
  
  
  