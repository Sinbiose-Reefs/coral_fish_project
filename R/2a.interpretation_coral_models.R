
## maps of richness, occupancy, and uncertainty

source("R/packages.R")
source("R/functions.R")

#############################################
############### load data
#############################################

load (here ("output", "Data_coral_detection.RData"))


# Generate several  neighbours for sensitivity analyses
coord <- coordenadas [,c("Lon","Lat")]
coordinates(coord) <- ~ Lon + Lat
#crs(coord) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

## implications of the neighborhood

pdf(here("output", "implications_neigh.pdf"),onefile=T)
# implications of different distances
par(mfrow=c(1,1), mai=c(0,0,0,0),mar=c(0,0,0,0),lwd=0.2)
map("world","Brazil",xlim=c(-50,-6),col="gray")
plot(dnearneigh(x=(coord), 0, 4),coord,add=T)
title("Nd=4ºlatlon")
map("world","Brazil",xlim=c(-50,-6),col="gray")
plot(dnearneigh((coord), 0, 6),coord,add=T)
title("Nd=6º")
map("world","Brazil",xlim=c(-50,-6),col="gray")
plot(dnearneigh((coord), 0, 9),coord,add=T)
title("Nd=9º")
dev.off()

#############################################
############### load model results
#############################################

load (here ("output", "StaticCARModel_coral_dbeta_comm.Rdata"))
load (here ("output", "StaticCARModel_coral_dbin_commV1.Rdata"))
load (here ("output", "StaticCARModel_coral_dbin_commV2.Rdata"))
load (here ("output", "StaticCARModel_coral_dbin_commV3.Rdata"))
load (here ("output", "StaticCARModel_coral_dbin_bern_comm.Rdata"))
load (here ("output", "StaticCARModel_coral_dbin_bern_comm_N4.Rdata"))
load (here ("output", "StaticCARModel_coral_dbin_bern_comm_N9.Rdata"))

## some basic data for plotting

# maximum cover a species could reach
tot_coral_cover <- apply (arranjo_corais,c(1,3),mean, na.rm=T)*100
tot_coral_cover <- rowSums (tot_coral_cover)
tot_coral_cover <- ifelse ((tot_coral_cover)>100,100,  (tot_coral_cover))

## subset dos sitios com cibertura de coral > 0 
coral_sites <- which( tot_coral_cover >0)

# cover

emptyDF<- data.frame (site = matrix (0,nrow=48,ncol=1))
emptyDF[coral_sites,] <- samples_dbinV3$mean$totSp

cover_plot <- data.frame(OriginalCover=tot_coral_cover,
                         EstDbeta=samples$mean$totCov,
                         EstDbinMaxCov=samples_dbinV1$mean$totSp,
                         EstDbinCOralCov=emptyDF$site)
# jittering coordinates
jitter_coord <- data.frame (LonJitter = jitter (coordenadas$Lon,factor=400),
                            LatJitter=jitter (coordenadas$Lat,factor=600))
# bind data
cover_plot_long <- melt(cover_plot)
cover_plot_long <- cbind(cover_plot_long,jitter_coord)

# map 

wm_pie <- wm + geom_point(aes(x=LonJitter, y=LatJitter, col = value),
                          size=2,alpha=0.9,
                          data = cover_plot_long) +
  facet_wrap( ~ variable) + 
  scale_color_gradient2(guide="colourbar",
                        limits = c(0,50),
                        low="yellow", mid="green",high="darkblue", midpoint=25)

wm_pie

## fazer uma coluna dizendo que nao tem coral em 3 sitios

plot_data <- data.frame(ObsRichness= rowSums(apply (arranjo_corais,c(1,3),max,na.rm=T)>0),
                        EstRichnessNVideos=samples_dbinV2$mean$totSp,
                        EstRichnessBernBinN4 = samples_dbin_bern_N4$mean$n.occ,
                        EstRichnessBernBinN6 = samples_dbin_bern$mean$n.occ,
                        EstRichnessBernBinN9 = samples_dbin_bern_N9$mean$n.occ
)

plot_data <- melt(plot_data)
# coordinates
jitter_coord <- data.frame (LonJitter = jitter (coordenadas$Lon,factor=400),
                            LatJitter=jitter (coordenadas$Lat,factor=600))

## ver como melt funciona
plot_data <- cbind(plot_data, jitter_coord)

## advise to jitter : https://stackoverflow.com/questions/52806580/pie-charts-in-geom-scatterpie-overlapping
## pie: http://www.spectdata.com/index.php/2018/10/25/how-to-use-ggplot-to-plot-pie-charts-on-a-map/

wm_pie <- wm + geom_point(aes(x=LonJitter, y=LatJitter, col = value),
                          size=2,alpha=0.9,
                          data = plot_data) +
  facet_wrap( ~ variable) + 
  scale_color_gradient2(guide="colourbar",
                        breaks = seq (0,18,2),
                        low="yellow", mid="green",high="darkblue", midpoint=7)

wm_pie


### species-specific cover

lapply (seq(1,ncol (samples$mean$p)), function (i) {
  
  
  emptyDF<- data.frame (site = matrix (0,nrow=48,ncol=1))
  emptyDF[coral_sites,] <- samples_dbinV3$mean$p[,i]
  
  dados <- data.frame(OriginalCover=apply (arranjo_corais[,,i],1,max,na.rm=T),
                      EstDbeta=samples$mean$p[,1],
                      EstDbinMaxCov=samples_dbinV1$mean$p[,i],
                      EstDbinCOralCov=emptyDF$site)
  
  
  plot_data <- melt(dados)
  plot_data <- cbind(plot_data, jitter_coord)
  
  wm_pie <- wm + geom_point(aes(x=LonJitter, y=LatJitter, col = value),
                            size=2,alpha=0.9,
                            data = plot_data) +
    facet_wrap( ~ variable) + 
    scale_color_gradient2(guide="colourbar",
                          limits = c(0,1),
                          breaks = seq (0,1,0.1),
                          low="yellow", mid="green",high="darkblue", midpoint=0.5) + 
    theme(legend.position="right") +
    geom_text(label = "Text")
  #  ;
  wm_pie
  
  ggsave (here ("output","figures_coral", paste("cov",sp_coral[i], ".png",sep="")))
  
})

## occurrence 

lapply (seq(1,ncol (samples_dbin_bern_N4$mean$psi)), function (i) {
  
  dados <- data.frame(ObsData= ifelse (apply (arranjo_corais,c(1,3),max,na.rm=T)>0,1,0)[,i],
                      EstOccNVideos=samples_dbinV2$mean$p[,i],
                      EstOccBernBinN4 = samples_dbin_bern_N4$mean$psi[,i],
                      EstOccBernBinN6 = samples_dbin_bern$mean$psi[,i],
                      EstOccBernBinN9 = samples_dbin_bern_N9$mean$psi[,i]
    )
  
  
  plot_data <- melt(dados)
  plot_data <- cbind(plot_data, jitter_coord)
  
    wm_pie <- wm + geom_point(aes(x=LonJitter, y=LatJitter, col = value),
                          size=2,alpha=0.9,
                          data = plot_data) +
      facet_wrap( ~ variable) + 
    scale_color_gradient2(guide="colourbar",
                          limits = c(0,1),
                        breaks = seq (0,1,0.1),
                        low="yellow", mid="green",high="darkblue", midpoint=0.5) + 
      theme(legend.position="right") +
      geom_text(label = "Text")
#  ;
  wm_pie
  
  ggsave (here ("output","figures_coral", paste("occ",sp_coral[i], ".png",sep="")))
  
})



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

