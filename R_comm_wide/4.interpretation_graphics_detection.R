#############################
## Interpreting detection

# load packages and functions
source ("R_comm_wide/packages.R")
source ("R_comm_wide/functions.R")

# load basic data for naming the table dims
## fish data
load (here("output_comm_wide_R1","Data_fish_detection_LONGO_AUED.RData"))

## LOAD MODEL RESULTS
load(here("output_comm_wide_R1","samples_OCCcoral_PdepthTime_longo_RdmP.RData")) 

# ---------------------------------------- #
# load traits to link traits and detection
# ---------------------------------------- #

## load trait data
traits_peixes <- read.csv(here("output_comm_wide_R1","tabS1.2.csv"),
                          h=T,sep=";")

# influence of depth and average detection
# coef plot

df_coef_time <- lapply (seq(1,length(coral_species)), function (i) 
  
  lapply (seq (1,length(samples_OCCcoral_PdepthTime_longo_RdmP[[1]])), function (age)
  
    data.frame (
      coral = coral_species[i],
      peixe = fish_species [[i]][[age]],
      age = age,
      size = traits_peixes [match(fish_species [[i]][[age]],traits_peixes$Scientific.name),"Maximum.body.size"],# 
      meanP = apply (samples_OCCcoral_PdepthTime_longo_RdmP[[i]][[age]]$sims.list$mean.p,2,mean),
      lowP = apply (samples_OCCcoral_PdepthTime_longo_RdmP[[i]][[age]]$sims.list$mean.p,2,quantile,0.05),
      highP = apply (samples_OCCcoral_PdepthTime_longo_RdmP[[i]][[age]]$sims.list$mean.p,2,quantile,0.95),
      #shallow (1-7m)
      estimate.Shallow = samples_OCCcoral_PdepthTime_longo_RdmP[[i]][[age]]$mean$alpha.depth[,1],
      estimate.shallow.lowP = apply (samples_OCCcoral_PdepthTime_longo_RdmP[[i]][[age]]$sims.list$alpha.depth[,,1],2,quantile,0.05),
      estimate.shallow.highP = apply (samples_OCCcoral_PdepthTime_longo_RdmP[[i]][[age]]$sims.list$alpha.depth[,,2],2,quantile,0.05),
      # deep
      estimate.Deep = samples_OCCcoral_PdepthTime_longo_RdmP[[i]][[age]]$mean$alpha.depth[,2],
      estimate.deep.lowP = apply (samples_OCCcoral_PdepthTime_longo_RdmP[[i]][[age]]$sims.list$alpha.depth[,,1],2,quantile,0.05),
      estimate.deep.highP = apply (samples_OCCcoral_PdepthTime_longo_RdmP[[i]][[age]]$sims.list$alpha.depth[,,2],2,quantile,0.05)
      
)))

# ordered body size

df_coef_time <- lapply (df_coef_time, function (i) 
  lapply (i, function (k)
  k[order (k$size,decreasing=F),]
))


# melt to fit to the format ggplot likes

df_detection <- lapply (df_coef_time, function (coral)
  
 do.call(rbind, lapply (coral, function (age)
  
  melt (age,id.var = c("coral","peixe","age","size","meanP","lowP","highP",
                       "estimate.deep.lowP","estimate.deep.highP",
                       "estimate.shallow.lowP","estimate.shallow.highP"))
  
  )))

# melt again
df_detection<-do.call(rbind,df_detection)
df_detection$variable<-gsub ("estimate.","",
                             df_detection$variable)
df_detection$coral <- firstup(df_detection$coral)

# aggregate across corals
require(dplyr)
df_detection <- df_detection %>% group_by (peixe,age,variable) %>%

  summarize(size = mean(size,na.rm=T),
            meanP = mean(meanP, na.rm = TRUE),
            lowP = min(lowP,na.rm=T),
            highP = min(highP,na.rm=T),
            estimate.deep.lowP = min(estimate.deep.lowP,na.rm=T),
            estimate.deep.highP  = min(estimate.deep.highP , na.rm=T),
            estimate.shallow.lowP = min(estimate.shallow.lowP,na.rm=T), 
            estimate.shallow.highP = min(estimate.shallow.highP,na.rm=T), 
            value = mean(value,na.rm=T))


# binomial smooth

binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
}

df_detection$age <- ifelse(df_detection$age == 1, 
                           "Adult", "Juvenile")

dodge <- c(0.2,0.2)
pd <- position_dodge(dodge)

# scatter plot detection and time
ggplot (df_detection, aes (x=size, y = value,
                   group = variable,
                   colour=variable)) + 
          geom_point() +
  facet_wrap(~age,ncol=4,scales="free_x")+
  theme_classic() +
  theme(legend.position = c(0.93,0.88),
        legend.title = element_blank(),
        strip.text = element_text(face="italic"))+
  xlab("Body size (cm)") + 
  ylab("Detection probability (p)") + 
  binomial_smooth(se=T,alpha=0.3) #+ 
  #geom_errorbar(aes(ymin=lowP,ymax=highP),
  #              position=pd,
  #              width = 0)

# save
ggsave(here ("output_comm_wide_R1", "figures", "detection.png"),
       height=4,width=6)



