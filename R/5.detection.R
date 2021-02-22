#############################
## Interpreting detection

# load packages and functions
source ("R/packages.R")
source ("R/functions.R")

## fish data
load (here("output","Data_fish_detection_MORAIS_AUED.RData"))

## coral detection - with coordinates
load (here("output","Data_coral_detection_MORAIS_AUED.RData"))

## LOAD MODEL RESULTS
load(here("output","samples_OCCcoral_PdepthObsID.RData")) 

## LOAD MODEL RESULTS
load(here("output","samples_OCCcoral_PdepthObsID_RdmP.RData")) 

# variation in p across transects
range((samples_OCCcoral_PdepthObsID_RdmP[[4]]$mean$intercept.p))
range(samples_OCCcoral_PdepthObsID_RdmP[[1]]$mean$psi)

## load trait data
traits_peixes <- read.csv(here("data","traits","Atributos_especies_Atlantico_&_Pacifico_Oriental_2020_04_28.csv"),
                          h=T,sep=";")
traits_peixes$Name <- tolower (gsub (" ",".", traits_peixes$Name))
traits_peixes$Name <- gsub("\\."," ", paste0(toupper(substr(traits_peixes$Name, 1, 1)), 
                                             substr(traits_peixes$Name, 2, 100)))

# adjusting trait values
traits_peixes$Body_size <- as.numeric(gsub (",",".",traits_peixes$Body_size))
traits_peixes$Aspect_ratio <- as.numeric(gsub (",",".",traits_peixes$Aspect_ratio))
traits_peixes$Trophic_level <- as.numeric(gsub (",",".",traits_peixes$Trophic_level))

# replacing NAs in aspect ratio by genera average
## finding genera
genera <- unlist(
  lapply (
    strsplit (traits_peixes$Name [which(is.na(traits_peixes$Aspect_ratio))]," "),"[",1)
)

# calculate the average for species with congenera 
mean_av_ratio <- unlist(lapply (genera, function (i)
  mean (traits_peixes [grep (i , traits_peixes$Name),"Aspect_ratio"],na.rm=T)
))
# imput in the table
traits_peixes[which(is.na(traits_peixes$Aspect_ratio)),"Aspect_ratio"] <- mean_av_ratio

## det data

det_df <- lapply(seq(1,length(fish_species)), function (i) 
  
  data.frame (sp_fish = fish_species [[i]], 
              sp_coral = coral_species [i],
              det = samples_OCCcoral_PdepthObsID_RdmP[[i]]$mean$mean.p, 
              size = traits_peixes [which(gsub (" ",".",tolower (traits_peixes$Name)) %in% fish_species [[i]]),"Body_size"],
              group = traits_peixes [which(gsub (" ",".",tolower (traits_peixes$Name)) %in% fish_species [[i]]),"Size_group"]
  ))
# melt to ggplot
det_df <- do.call(rbind,det_df)

# size and detection
ggplot (det_df,aes (x=size, y=det)) +
  geom_point()+
  theme_classic() + ylab ("Detection probability")+xlab("Body size (cm)")+
  geom_smooth(method="glm")+
  facet_wrap(~sp_coral,scales = "fixed")

ggsave (here("output", "detectionUVC_size.png"),width=4,height = 4)

# group and detection
# adjust group factor
det_df$group <- factor(det_df$group, 
                       levels = c("sol","pair", "smallg", "medg"))
ggplot (det_df,aes (x=group, y=det)) +
  geom_boxplot(fill="gray80")+
  theme_classic() + ylab ("Detection probability")+xlab("Group size")+
  geom_smooth(method="glm")+
  facet_wrap(~sp_coral,scales = "fixed")

ggsave (here("output", "detectionUVC_group.png"),width=4,height = 4)

## diver effect
obs_df <- lapply(seq(1,length(fish_species)), function (i) 
  ## average sp detection across MCMC sampels
  apply(samples_OCCcoral_PdepthObsID_RdmP[[i]]$sims.list$alpha.obs,c(1,3),mean)
)

names(obs_df) <- coral_species

# melt to plot
obs_df <- lapply(obs_df,melt)

# do.call
obs_df <- do.call (rbind,obs_df)

# sp names

nms <- do.call (rbind, 
                lapply (strsplit(rownames(obs_df),"\\."), function (i)
                  (i[c(1,2)])))

nms <- sapply (seq(1,nrow(nms)), function (i){
  
  paste(nms[i,1],nms[i,2],sep=".")
  
}
)

## cbind
obs_df <- cbind (obs_df,nms)

# plot
ggplot (obs_df, aes(x=Var2, y=value,group=Var2)) + 
  geom_boxplot(fill="gray80") + theme_classic() +
  xlab("Diver ID") + ylab("Detection probability") + 
  facet_wrap(~ nms) + 
  scale_x_discrete(labels =as.character(seq(1,17)),
                   limits=as.character(seq(1,17)))

ggsave (here("output", "detectionUVC_obs.png"),width=6,height = 4)

## depth effect
depth_df <- lapply(seq(1,length(fish_species)), function (i) 
  ## average sp detection across MCMC samples
  apply(samples_OCCcoral_PdepthObsID_RdmP[[i]]$sims.list$alpha.depth,c(1,3),mean)
)

names(depth_df) <- coral_species

# melt to plot
depth_df <- lapply(depth_df,melt)

# do.call
depth_df <- do.call (rbind,depth_df)

# sp names

nms <- do.call (rbind, 
                lapply (strsplit(rownames(depth_df),"\\."), function (i)
                  (i[c(1,2)])))

nms <- sapply (seq(1,nrow(nms)), function (i){
  
  paste(nms[i,1],nms[i,2],sep=".")
  
}
)

## cbind
depth_df <- cbind (depth_df,nms)
depth_df$Var2 <- factor(depth_df$Var2)
levels(depth_df$Var2)[which(levels(depth_df$Var2) == "1")] <- "Shallow"
levels(depth_df$Var2)[which(levels(depth_df$Var2) == "2")] <- "Deep"

# plot
ggplot (depth_df, aes(x=Var2, y=value,group=Var2)) + 
  geom_boxplot(fill="gray80") + theme_classic() +
  xlab("Depth") + ylab("Detection probability") + 
  facet_wrap(~ nms)

ggsave (here("output", "detectionUVC_depth.png"),width=4,height = 4)

# ------------------------------------------------- #  
# ------------------------------------------------- #
# The same  for Longo et al. 
# ------------------------------------------------- #  
# ------------------------------------------------- #

# load packages and functions
source ("R/packages.R")
source ("R/functions.R")

# load basic data for naming the table dims
## fish data
load (here("output","Data_fish_detection_LONGO_AUED.RData"))

## coral detection - with coordinates
load (here("output","Data_coral_detection_LONGO_AUED.RData"))

## LOAD MODEL RESULTS
load(here("output","samples_OCCcoral_PdepthTime_longo.RData")) 

## LOAD MODEL RESULTS
load(here("output","samples_OCCcoral_PdepthTime_longo_RdmP.RData")) 

# ---------------------------------------- #
# FIGURE 2, FISH RELIANCE ON CORAL COVER
# ---------------------------------------- #

## load trait data
traits_peixes <- read.csv(here("data","traits","Atributos_especies_Atlantico_&_Pacifico_Oriental_2020_04_28.csv"),
                          h=T,sep=";")
traits_peixes$Name <- tolower (gsub (" ",".", traits_peixes$Name))
traits_peixes$Name <- gsub("\\."," ", paste0(toupper(substr(traits_peixes$Name, 1, 1)), 
                                             substr(traits_peixes$Name, 2, 100)))

# adjusting trait values
traits_peixes$Body_size <- as.numeric(gsub (",",".",traits_peixes$Body_size))
traits_peixes$Aspect_ratio <- as.numeric(gsub (",",".",traits_peixes$Aspect_ratio))
traits_peixes$Trophic_level <- as.numeric(gsub (",",".",traits_peixes$Trophic_level))

# replacing NAs in aspect ratio by genera average
## finding genera
genera <- unlist(
  lapply (
    strsplit (traits_peixes$Name [which(is.na(traits_peixes$Aspect_ratio))]," "),"[",1)
)

# calculate the average for species with congenera 
mean_av_ratio <- unlist(lapply (genera, function (i)
  mean (traits_peixes [grep (i , traits_peixes$Name),"Aspect_ratio"],na.rm=T)
))
# imput in the table
traits_peixes[which(is.na(traits_peixes$Aspect_ratio)),"Aspect_ratio"] <- mean_av_ratio

# influence of time
# coef plot

df_coef_time <- lapply (seq(1,length(coral_species)), function (i) 
  data.frame (
  coral = coral_species[i],
  peixe = fish_species [[i]],
  size = traits_peixes [which(gsub (" ",".",tolower (traits_peixes$Name)) %in% fish_species [[i]]),"Body_size"],
  group = traits_peixes [which(gsub (" ",".",tolower (traits_peixes$Name)) %in% fish_species [[i]]),"Size_group"],
  meanP = apply (samples_OCCcoral_PdepthTime_longo_RdmP[[i]]$sims.list$mean.p,2,mean),
  estimate = apply (samples_OCCcoral_PdepthTime_longo_RdmP[[i]]$sims.list$alpha1.time,2,mean),
  low = apply (samples_OCCcoral_PdepthTime_longo_RdmP[[i]]$sims.list$alpha1.time,2,quantile,0.05),
  high = apply (samples_OCCcoral_PdepthTime_longo_RdmP[[i]]$sims.list$alpha1.time,2,quantile,0.95)
))

# ordered body size

df_coef_time <- lapply (df_coef_time, function (i) 
  i[order (i$size,decreasing=F),]
)

# working round with size
df_coef_time <- lapply (df_coef_time, function (i) {
    i$peixe <- factor(i$peixe,
                      levels = i$peixe[order(i$size,decreasing=F)])
    i$group <- factor(i$group, 
                                 levels = c("sol","pair", "smallg", "medg"))
    
    ;
    i

    })

# scatter plot detection and time
ggplot (df_coef_time[[1]], aes (x=size, y = estimate)) + 
          geom_point() + theme_classic() +
  xlab("Body size (cm)") + 
  ylab("Regression coefficient estimate") + 
  geom_smooth(method="lm")

summary(lm(estimate~size,data=df_coef_time[[1]]))

# scatter plot detection and time
ggplot (df_coef_time[[1]], aes (x=group, y = estimate)) + 
  geom_boxplot(fill="gray80") + theme_classic() +
  xlab("Body size (cm)") + 
  ylab("Regression coefficient estimate")

summary(lm(estimate~group,data=df_coef_time[[1]]))

# coefficient plot
lapply (seq (1,length(coral_species)), function (i) {
  
  dodge <- c(0.2,0.2)
  pd <- position_dodge(dodge)
  
  a <- ggplot (df_coef_time[[i]], aes  (y=peixe, x=estimate, fill=coral,
                            colour=coral)) + 
    geom_errorbar(aes(xmin=low,xmax=high),width = 0,
                  position=pd) + theme_classic() + 
    geom_point()+ 
    geom_vline(xintercept = 0, linetype="dashed", 
               color = "gray50", size=0.5)+
    scale_color_manual(values=c("gray70", "gray60", "gray50",
                                "gray40","black")) + 
    xlab("Regression coefficient estimate") + 
    ylab ("Reef fish species") + 
    xlim(-8, 8) + 
    theme (legend.position = "none") + 
    facet_wrap(~coral, scales="fixed")
  
  a
  
  ggsave (here("output", paste("detectionVideoTime",i,".png")),width=5,heigh=5)
  
  }
)

## depth effect
depth_df <- lapply(seq(1,length(fish_species)), function (i) 
  ## average sp detection across MCMC sampels
  apply(samples_OCCcoral_PdepthTime_longo_RdmP[[i]]$sims.list$alpha.depth,c(1,3),mean)
)

names(depth_df) <- coral_species

# melt to plot
depth_df <- lapply(depth_df,melt)

# do.call
depth_df <- do.call (rbind,depth_df)

# sp names

nms <- do.call (rbind, 
                lapply (strsplit(rownames(depth_df),"\\."), function (i)
                  (i[c(1,2)])))

nms <- sapply (seq(1,nrow(nms)), function (i){
  
  paste(nms[i,1],nms[i,2],sep=".")
  
}
)

## cbind
depth_df <- cbind (depth_df,nms)
depth_df$Var2 <- factor(depth_df$Var2)
levels(depth_df$Var2)[which(levels(depth_df$Var2) == "1")] <- "Shallow"
levels(depth_df$Var2)[which(levels(depth_df$Var2) == "2")] <- "Deep"

# plot
ggplot (depth_df, aes(x=Var2, y=value,group=Var2)) + 
  geom_boxplot(fill="gray80") + theme_classic() +
  xlab("Depth") + ylab("Detection probability") + 
  facet_wrap(~ nms)

ggsave (here("output", "detectionVideo_depth.png"),width=4,height = 4)


## with body size
# size and detection
df_coef_time <- do.call(rbind,df_coef_time)
ggplot (df_coef_time,aes (x=size, y=meanP)) +
  geom_point()+
  theme_classic() + ylab ("Detection probability")+xlab("Body size (cm)")+
  geom_smooth(method="glm")+
  facet_wrap(~coral,scales = "fixed")

ggsave (here("output", "detectionVideo_size.png"),width=4,height = 4)

# group size
ggplot (df_coef_time,aes (x=group, y=meanP)) +
  geom_boxplot(fill="gray80")+
  theme_classic() + ylab ("Detection probability")+xlab("Group size")+
  geom_smooth(method="glm")+
  facet_wrap(~coral,scales = "fixed")

ggsave (here("output", "detectionVideo_group.png"),width=4,height = 4)



