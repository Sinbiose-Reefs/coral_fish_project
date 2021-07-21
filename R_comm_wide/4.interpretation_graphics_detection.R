#############################
## Interpreting detection

# load packages and functions
source ("R/packages.R")
source ("R/functions.R")

# load basic data for naming the table dims
## fish data
load (here("output_comm_wide","Data_fish_detection_LONGO_AUED.RData"))

## coral detection - with coordinates
load (here("output_comm_wide","Data_coral_detection_LONGO_AUED.RData"))

## LOAD MODEL RESULTS
load(here("output_comm_wide","samples_OCCcoral_PdepthTime_longo.RData")) 

## LOAD MODEL RESULTS
load(here("output_comm_wide","samples_OCCcoral_PdepthTime_longo_RdmP.RData")) 

# ---------------------------------------- #
# load traits to link traits and detection
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

# adjusting names

traits_peixes$Name <- gsub (" ",".",tolower (traits_peixes$Name))

# influence of time
# coef plot

df_coef_time <- lapply (seq(1,length(coral_species)), function (i) 
  data.frame (
    coral = coral_species[i],
    peixe = fish_species [[i]],
    size = traits_peixes [match(fish_species [[i]],traits_peixes$Name),"Body_size"],# 
    group = traits_peixes [match(fish_species [[i]],traits_peixes$Name),"Size_group"],
    meanP = apply (samples_OCCcoral_PdepthTime_longo_RdmP[[i]]$sims.list$mean.p,2,mean),
    estimate = apply (samples_OCCcoral_PdepthTime_longo_RdmP[[i]]$sims.list$alpha1.time,2,mean),
    low = apply (samples_OCCcoral_PdepthTime_longo_RdmP[[i]]$sims.list$alpha1.time,2,quantile,0.05),
    high = apply (samples_OCCcoral_PdepthTime_longo_RdmP[[i]]$sims.list$alpha1.time,2,quantile,0.95)
))

# ordered body size

df_coef_time <- lapply (df_coef_time, function (i) 
  i[order (i$size,decreasing=F),]
)

# ordering based on body size and group size
df_coef_time <- lapply (df_coef_time, function (i) {
    i$peixe <- factor(i$peixe,
                      levels = i$peixe[order(i$size,decreasing=F)])
    i$group <- factor(i$group, 
                                 levels = c("sol","pair", "smallg", "medg", "largeg"))
    
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

summary(lm(estimate~group,data=df_coef_time[[4]]))

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
  
  ggsave (here("output_comm_wide", paste("detectionVideoTime",i,".pdf")),width=5,heigh=4.5)
  
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

ggsave (here("output_comm_wide", "detectionVideo_depth.png"),width=4,height = 4)


## with body size
# size and detection
df_coef_time <- do.call(rbind,df_coef_time)
ggplot (df_coef_time,aes (x=size, y=meanP)) +
  geom_point()+
  theme_classic() + ylab ("Detection probability")+xlab("Body size (cm)")+
  geom_smooth(method="glm")+
  facet_wrap(~coral,scales = "fixed")

ggsave (here("output_comm_wide", "detectionVideo_size.png"),width=4,height = 4)

# group size
ggplot (df_coef_time,aes (x=group, y=meanP)) +
  geom_boxplot(fill="gray80")+
  theme_classic() + ylab ("Detection probability")+xlab("Group size")+
  geom_smooth(method="glm")+
  facet_wrap(~coral,scales = "fixed")

ggsave (here("output_comm_wide", "detectionVideo_group.png"),width=4,height = 4)

# end


