
## Implementação do modelo de ocupação de sítios
## aplicado a espécies de corais

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

########################################

#### PARTE DO MEIO

### load packages

source("R/packages_model_coral.R")

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

# Generate several  neighbours for sensitivity analyses
coord <- coordenadas [,c("Lon","Lat")]
coordinates(coord) <- ~ Lon + Lat

## d1=0; d2 varying as (c(3,6,9,12,15,18,21,24))

neigh_winnb <- lapply (as.list(c(3,6,9,12,15,18,21,24)), function (i)  {
 
 #create neighborhood
 neigh <- dnearneigh((coord), 0, i)
 # Number of neighbours
 table(card(neigh))
 # Convert the neighbourhood
 winnb <- nb2WB(neigh)
 
 ; # return
 
 winnb
 
 }
)

################################
## parallel-processing settings
cl <- makeCluster(3) ## number of cores = generally ncores -1

# exportar pacote para os cores
clusterEvalQ(cl, library(R2WinBUGS))
clusterEvalQ(cl, library(here))

# export your data and function
clusterExport(cl, c("df_coral_data",
                    "neigh_winnb",
                    "ni","nt","nb","nc","na",
                    "params"))

samples_coral <- parLapply (cl, neigh_winnb, function (k)  ## for each distance of neighborhood
  
                    lapply (df_coral_data, function (i) { ## for each coral species
      
  
  #i= df_coral_data[[4]]
  
  str(win.data<- list(y=  i[,"y"], 
                    nsite = max (i[,"M"]),
                    nobs = nrow (i),
                    site = i[,"M"],
                    occa = i[,"J"],
                    num = k$num, 
                    adj = k$adj, 
                    weights = k$weights))

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
  
))
  
stopCluster(cl)
  
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


pdf(here ("output", "corr_cover_z.pdf"),onefile = T)

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



pdf(here ("output", "corr_detection_z.pdf"),onefile = T)

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

pdf (here ("output", "RHat_eval_coral_models.pdf"),onefile = T,width=4,height=4)
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

