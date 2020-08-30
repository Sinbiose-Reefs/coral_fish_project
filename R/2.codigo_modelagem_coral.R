
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
sink(here ("bugs","StaticCARModel_coral_dbeta_comm.txt"))

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
         q[i,k] <- (1-mu[i,k])*phi[i,k] 
         ## or an alternative parameterization: phi[i]-mu.s[i]*phi[i]

         # model for average cover
         mu0[i,k]<- b0[k] + rho[k,i]

         # keep cover on the track
         mu.lim[i,k] <- min(10,max(-10,mu0[i,k]))
         logit(mu[i,k]) <- mu.lim[i,k]

         # precision model
         phi[i,k] <-exp(c0[k] + c1[k]*nvideos[i])

         } ## close site loop
      } ## close spp loop

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
sink(here ("bugs","StaticCARModel_coral_dbin_comm.txt"))

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
sink(here ("bugs","StaticCARModel_coral_dbin_bern_comm.txt"))

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

   
    # Derived parameters
    # number of species per site (finite sample size)
    for (i in 1:nsite) {
      n.occ [i] <- sum(z[i,]) 
      mutot [i] <- sum(psi[i,])
    }
    
    # number of sites per species 
    for (k in 1:nspec) {
      n.spp[k] <- sum(z[,k])
      n.spp.mu[k] <- sum (psi[,k])
    }

    # mean detection probabability
    meanP <- mean(p[])
    meanPsi <- mean (psi[])
    meanZ <- mean(z[])
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
na <- 3000

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
cover_data <- apply(arranjo_corais,c(1,3),mean,na.rm=T)
## as dbeta can not deal with 0's and 1', I transformed zeros into very small numbers
cover_data <- ifelse (cover_data == 0, 0.000000000000001,cover_data)

# standardize number of videos  
std_videos <- (rowSums(is.na(arranjo_corais[,,1])!=T) - mean(rowSums(is.na(arranjo_corais[,,1])!=T)))/
  sd(rowSums(is.na(arranjo_corais[,,1])!=T))
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
  b0 = runif (20,-10,10),
  c0 = runif (20,-10,10),
  c1 = runif (20,-10,10)
  #rho = matrix (0, nrow=20,ncol=48)
  )} #rep(0, win.data$nsite)
                           
# run winbugs
## Parameters to monitor
  
params <- c(
  ### cover-model parameters
    "meanCov","totCov","meanCovmu","totCovmu",
    "b0","c0","c1",
    "mu","p","q","spacesigma",
    "spacetau", "rho"
    )
  
## call and run winBUGS  
samples <- bugs(data = win.data, parameters.to.save = params, 
                model.file = here ("bugs","StaticCARModel_coral_dbeta_comm.txt"), 
                inits = NULL,
                n.chains = nc, 
                n.thin = nt, 
                n.iter = ni, 
                n.burnin = nb, 
                DIC = T,
                bugs.directory = "C:/Program Files (x86)/winbugs14_unrestricted/WinBUGS14",
                debug=F) ## you don't need close manually if debug = F 

save (samples,file=here("output", "StaticCARModel_coral_dbeta_comm.RData"))  
  
  
####
  
# binomial model used to estimate the probability of finding a cover higher than 1% 
# relative to total coral cover 

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
str(win.data<- list(C =  local_data,# i[,"y"], 
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
  "totSp","meanP", "nSiteP", "meanSp",
   "b0", "p","spacesigma",
    "spacetau", "rho"
    
  )
  
samples_dbinV1 <- bugs(data = win.data, parameters.to.save = params, 
                  model.file = here ("bugs","StaticCARModel_coral_dbin_comm.txt"), 
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
    "meanP","totSp","nSiteP", "meanSp",
    "b0",
    "p","spacesigma",
    "spacetau", "rho"
    
  )

# call and run winbugs  
samples_dbinV2 <- bugs(data = win.data, parameters.to.save = params, 
                  model.file = here ("bugs","StaticCARModel_coral_dbin_comm.txt"), 
                  inits = NULL,
                  n.chains = nc, 
                  n.thin = nt, 
                  n.iter = ni, 
                  n.burnin = nb, 
                  DIC = T,
                  bugs.directory = "C:/Program Files (x86)/winbugs14_unrestricted/WinBUGS14",
                  debug=F) ## you don't need close manually if debug = F 
  
save (samples_dbinV2,file=here("output", "StaticCARModel_coral_dbin_commV2.RData"))  

## 
## V3
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
  
## inits
#zst <- ifelse (local_data>0,1,0) 
#inits <- function(){list(
#    z=zst,
#    rho = rep(0, win.data$nsite)
#  )}
  
# parameters to monitor
params <- c(
    
    ### bern-bin model
    "p","z","psi",  "meanP", "meanPsi", 
    "meanZ", "n.spp.mu", "mutot",
    "b0",
    "spacetau", "spacesigma","rho"
    
  )

## call and run bugs  
samples_dbin_bern <- bugs(data = win.data, parameters.to.save = params, 
                         model.file = here ("bugs","StaticCARModel_coral_dbin_bern_comm.txt"), 
                         inits = NULL,
                         n.chains = nc, 
                         n.thin = nt, 
                         n.iter = ni, 
                         n.burnin = nb, 
                         DIC = T,
                         bugs.directory = "C:/Program Files (x86)/winbugs14_unrestricted/WinBUGS14",
                         debug=F) ## you don't need close manually if debug = F 
  
  

save (samples_dbin_bern,file=here("output", "StaticCARModel_coral_dbin_bern_comm.RData"))  

  
  
  
  
  
  
  
  
  
  
  
  #; samples
  
  #}
  
#))
  
#stopCluster(cl)
  
save (samples_coral, file=here ("output", "samples_coral_CARModel.RData"))
  

##############################################
########### Looking at the results


##############################################
### load packages

source("R/packages_model_coral.R")

#############################################
############### load data
#############################################

load (here ("output", "Data_coral_detection.RData"))
load (here ("output", "samples_coral_CARModel.RData"))

# Generate several  neighbours for sensitivity analyses
coord <- coordenadas [,c("Lon","Lat")]
coordinates(coord) <- ~ Lon + Lat

pdf(here("output", "implications_neigh.pdf"),onefile=T)
# implications of different distances
par(mfrow=c(2,4), mai=c(0,0,0,0),mar=c(1,1,1,1),lwd=0.2)
map("world","Brazil",xlim=c(-50,-6),col="gray")
plot(dnearneigh(x=(coord), 0, 3),coord,add=T)
title("N=3")
map("world","Brazil",xlim=c(-50,-6),col="gray")
plot(dnearneigh((coord), 0, 6),coord,add=T)
title("N=6")
map("world","Brazil",xlim=c(-50,-6),col="gray")
plot(dnearneigh((coord), 0, 9),coord,add=T)
title("N=9")
map("world","Brazil",xlim=c(-50,-6),col="gray")
plot(dnearneigh((coord), 0, 12),coord,add=T)
title("N=12")
map("world","Brazil",xlim=c(-50,-6),col="gray")
plot(dnearneigh((coord), 0, 15),coord,add=T)
title("N=15")
map("world","Brazil",xlim=c(-50,-6),col="gray")
plot(dnearneigh((coord), 0, 18),coord,add=T)
title("N=18")
map("world","Brazil",xlim=c(-50,-6),col="gray")
plot(dnearneigh((coord), 0, 21),coord,add=T)
title("N=20")
map("world","Brazil",xlim=c(-50,-6),col="gray" )
plot(dnearneigh((coord), 0, 24),coord,add=T)
title("N=24 lat-long")

dev.off()
#
###
par(mfrow=c(1,1))

## df para analisar a relacao entre a cobertura original e as estimativas de ocupacao

df_res <- lapply (seq(1,length(samples_coral[[1]])), function (k)  ## para cada sp de coral
  lapply (seq(1,length(samples_coral)), function (i) { ##  para cada vizinhanca
    
    ## construa um DF com os resultados
    res <- data.frame (cob_orig=apply (arranjo_cob_coral_sitio_video[,,1],1,max,na.rm=T),
        est_z = samples_coral [[i]] [[k]]$mean$z)
  
    ; res[,-1]
    
    }
  )
)

## transformar em df
df_res_neigh <- lapply (df_res,function (i) do.call (cbind,i))

## sp de coral analisada como nome da lista
names (df_res_neigh) <- sp_coral

## obter a cobertura original

subset_corais_analisados <- which(dimnames(arranjo_cob_coral_sitio_video)[[3]] %in% sp_coral)

cob_coral_orig_max <- apply (arranjo_cob_coral_sitio_video [,,subset_corais_analisados],
                               c(1,3),max,na.rm=T)

#### Colar no DF das estimativas de z, e  tb o nome da sp para os plots e fc melt
df_res_neigh <- lapply (seq (1,length(df_res_neigh )), function (i) {
  
  data.frame(df_res_neigh[[i]], 
         colnames(cob_coral_orig_max)[i])
  
  })

## vizinhanca utilizada como nome de coluna
df_res_neigh <- lapply (df_res_neigh, function (i) {
  
  colnames(i) <- c(paste("n",c(3,6,9,12,15,18,21,24),sep=""),
                   "species")
  ; i
  
})

## nomes das linhas (sitios)

df_res_neigh <- lapply (df_res_neigh, function (i) {
  
  rownames(i) <- sitios_bentos
  ; i
  
})

df_long_plot <- lapply(df_res_neigh,melt)

# colar a cob original

df_long_plot <- lapply (seq(1,length (df_long_plot)), function (i) 
  
  data.frame (df_long_plot[[i]], 
             original=cob_coral_orig_max [,i]
))

## e desmanchar
df_long_plot <- do.call(rbind,df_long_plot )

ggplot (df_long_plot, aes (x=original,y=value,col=variable)) + 
  stat_smooth(aes(x=value,y=value),
              method = "glm", 
              method.args = list(family = "binomial"),se=F) +
  #geom_smooth() + 
  facet_wrap(~species) + theme_classic() + 
  geom_point()


##################################################

## CORRELATION entre cobertura observada (maxima) 
## e a ocupacao estimada

correlations <- lapply (seq(1,6), function (k)
  
  unlist(lapply (seq(1,8), function (i)
  
  cor(
        data.frame (cob_orig= cob_coral_orig_max[,k], ## pegar a cobertura observada
                  est_z = apply (samples_coral [[i]] [[k]]$sims.list$z,2,mean,na.rm=T)) ## pegar o zmedio
      )[2,1]
  )))

## representacao da incerteza
correlations_uncertainty <- lapply (seq(1,6), function (k) ## para cada sp. de coral
  
  do.call(cbind,lapply (seq(1,8), function (i) ## para cada neighborhood
    
    lapply (seq(1,600), function (q) ## para cada iteracao
      
    
    cor(
      
      data.frame (cob_orig= cob_coral_orig_max[,k], ## 
                  est_z = samples_coral [[i]] [[k]]$sims.list$z[q,])
         )[2,1] ## selecionar a correlacao que interessa
  
        )
      )
    )
  )


pdf(here ("output","figures_coral", "corr_cover_z.pdf"),onefile = T)

par(mfrow=c(2,3))

lapply (seq(1,6), function (i) {

  plot(NA,xlim=c(3,25),
     ylim=c(-1,1),
     xaxt="n",
     ylab="Correlation",
     xlab="Neighborhood distance (Lat-long degrees)"#,
     #main="Correlation between observed coral cover\nand estimated site occupancy",
     )

  axis(side=1,at=c(3,6,9,12,15,18,21,24),c(3,6,9,12,15,18,21,24))


  lapply (seq (1,600), function (q) {
    
  lines( c(3,6,9,12,15,18,21,25),
         correlations_uncertainty[[i]][q,],
      col=rgb(0.400,0.400,0.400,alpha=0.1))
  })
  
  lines(c(3,6,9,12,15,18,21,25),
         correlations[[i]],lwd=2,col="black")
        
  legend ("bottomright", legend = gsub ("\\."," ",sp_coral[i]),
          bty="n")
  
  }
  )

dev.off()

### correlacao entre deteccao e nao deteccao, e ocupacao estimada

correlations <- lapply (seq(1,6), function (k)
  
  unlist(lapply (seq(1,8), function (i)
    
    cor(
      data.frame (cob_orig= apply (ifelse (arranjo_cob_coral_sitio_video[,,k]>0,1,0),1,max,na.rm=T),
                  est_z = apply (samples_coral [[i]] [[k]]$sims.list$z,2,mean,na.rm=T))
    )[2,1]
  )))

## representacao da incerteza
correlations_uncertainty <- lapply (seq(1,6), function (k) ## para cada sp. de coral
  
  do.call(cbind,lapply (seq(1,8), function (i) ## para cada neighborhood
    
    lapply (seq(1,600), function (q) ## para cada iteracao
      
      
      cor(
        data.frame (cob_orig= apply (ifelse (arranjo_cob_coral_sitio_video[,,k]>0,1,0),1,max,na.rm=T),
                    est_z = samples_coral [[i]] [[k]]$sims.list$z[q,])
      )[2,1] ## selecionar a correlacao que interessa
      
    ))))



pdf(here ("output","figures_coral", "corr_detection_z.pdf"),onefile = T)

par(mfrow=c(2,3))

lapply (seq(1,6), function (i) {
  
  plot(NA,xlim=c(3,25),
       ylim=c(-1,1),
       xaxt="n",
       ylab="Correlation",
       xlab="Neighborhood distance (Lat-long degrees)"#,
       #main="Correlation between observed coral cover\nand estimated site occupancy",
  )
  
  axis(side=1,at=c(3,6,9,12,15,18,21,24),c(3,6,9,12,15,18,21,24))
  
  
  lapply (seq (1,600), function (q) {
    
    lines( c(3,6,9,12,15,18,21,24),
           correlations_uncertainty[[i]][q,],
           col=rgb(0.400,0.400,0.400,alpha=0.1))
  })
  
  lines(c(3,6,9,12,15,18,21,24),
        correlations[[i]],lwd=2,col="black")
  
  legend ("bottomright", legend = gsub ("\\."," ",sp_coral[i]),
          bty="n")
  
}
)

dev.off()

### avaliacao do RHat

Rhat_eval <- lapply (samples_coral, function (k)
  
  unlist(lapply (k, function (i) # extraia o RHat de cada modelo
  
  mean(  i$summary [,'Rhat'])
  
)))

pdf (here ("output", "figures_coral","RHat_eval_coral_models.pdf"),onefile = T,width=4,height=4)
par(mfrow=c(1,1))

plot(NA,xlim=c(3,25),ylim=c(1,3.5),xaxt="n",
     ylab= "Average RHat",
     xlab="Neighborhood distance (Lat-long degrees)")
axis(side=1,at=c(3,6,9,12,15,18,21,24),c(3,6,9,12,15,18,21,24))

lapply (seq (1,6), function (i)
  lines (c(3,6,9,12,15,18,21,24),
         sapply (Rhat_eval,"[",i),col=i))

legend ("topright", legend = gsub("\\."," ",sp_coral),
        col = 1:6,bty="n",cex=0.7,lty=1)
dev.off()

### save data to fish site-occupancy analysis

## the fourth element is 12's neighborhood
occupancy_estimate_nb12 <- do.call (cbind,
                                    
                                    lapply (samples_coral [[4]], function (i)
                                      
                                      i$mean$z)
)

# set colnames
colnames(occupancy_estimate_nb12) <- sp_coral

list_coral_data <- list(cover_original = cob_coral_orig_max,
                        occupancy_estimate_nb12 =  occupancy_estimate_nb12)

save (list_coral_data, file=here ("output","coral_occupancy_data.RData"))

