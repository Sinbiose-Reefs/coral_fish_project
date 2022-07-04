## code with routine for interpretation of the results


set.seed (1234)

# load packages and functions
source ("R_comm_wide/packages.R")
source ("R_comm_wide/functions.R")

## function to test space quality
source("R_comm_wide/quality_funct_space_fromdist2.R")


# ------------------------------# 


#       MAP (figure 1)


# ------------------------------#

# mapa mundi
world <- ne_countries(scale = "medium", returnclass = "sf")

# crop mapa mundi
wm <- ggplot() + 
  geom_sf (data=world, size = 0.1, 
           fill= "gray90",colour="gray90") +
  coord_sf (xlim = c(-60, -23),  ylim = c(-30, 2), expand = FALSE) +
  theme_bw() +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "lightcyan",
                                        colour = "lightcyan"),
        axis.text.x = element_text(size=6),
        axis.ticks.x=element_line(size=1),
        axis.text.y = element_text(size=6),
        axis.ticks.y=element_line(size=1),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        title = element_blank()) 


# ----------------------------# 

# load fish and coral data

# ----------------------------# 


load (here("output_comm_wide_R1","Data_fish_detection_LONGO_AUED.RData"))


# summarize information for corals and turf
cob_bentos_turf_corals <- cob_bentos [,which(colnames(cob_bentos) %in% c("calcareous turf",coral_species))]
cob_bentos_turf_corals <- cob_bentos_turf_corals/rowSums(cob_bentos_turf_corals) # cover relative to total cover in the site
# find the total covered by corals
cob_bentos_turf_corals$corals <- 1- cob_bentos_turf_corals$`calcareous turf`
cob_bentos_turf_corals$None <- ifelse (cob_bentos_turf_corals$corals == 0,1,0)

# bind coordinates  to project in space the cover of these benthic components

cob_bentos_turf_corals <- cbind (cob_bentos_turf_corals,
                                 coordenadas)



# jitter long and lat

cob_bentos_turf_corals <- cbind (cob_bentos_turf_corals,
                                LonJitter = jitter (cob_bentos_turf_corals$decimalLongitude,factor=400),
                                LatJitter=jitter (cob_bentos_turf_corals$decimalLatitude,factor=600))



## advice to jitter : https://stackoverflow.com/questions/52806580/pie-charts-in-geom-scatterpie-overlapping
## pie chart: http://www.spectdata.com/index.php/2018/10/25/how-to-use-ggplot-to-plot-pie-charts-on-a-map/


# pie chart 


plot1_turf <- wm + geom_scatterpie(aes(x=LonJitter, y=LatJitter),
                     data = cob_bentos_turf_corals,
                     cols = c("calcareous turf","corals"),
                     pie_scale = 2,
                     size=0.2,
                     alpha = 0.65,
                     color="black",
                     sorted_by_radius = F,
                     legend_name = "Corals")  
  


# jitter to separate pie charts


further_jitter <-cob_bentos_turf_corals 
further_jitter$LonJitter<-further_jitter$LonJitter+5


# plot turf and total coral cover

plot2_corals <- plot1_turf + geom_scatterpie(aes(x=LonJitter, y=LatJitter),
                               data =further_jitter ,
                              cols = c("agaricia spp",
                                       "favia gravida",
                                       "millepora alcicornis",
                                       "montastraea cavernosa",
                                       "mussismilia harttii",
                                       "mussismilia hispida",
                                       "porites astreoides",
                                       "siderastrea spp",
                                     "None"),
                               pie_scale = 2,
                               size=0.2,
                               alpha = 0.65,
                               color="black",
                               sorted_by_radius = F,
                               legend_name = "Corals") + 
  
  theme (legend.title = element_text(size=8),
         legend.text = element_text(size=7),
         legend.position = c(.35, .8),
         legend.justification = c("right", "top"),
         legend.box.just = "right",
         legend.margin = margin(6,6,6,6),
         legend.background = element_blank(),
         title=element_text(size=8)) 


# plot the cover of each coral species

plot2_corals <- plot2_corals +  scale_fill_manual(
  
    values= c("calcareous turf"="#9EB23B", 
              "corals" = "#990000",
              "agaricia spp" = "#ff6961",
              "favia gravida" = "#ffb480",
              "millepora alcicornis" = "#f8f38d",
               "montastraea cavernosa" = "#42d6a4",
              "mussismilia harttii" = "#08cad1",
               "mussismilia hispida" = "#59adf6",
               "porites astreoides" = "#9d94ff",
               "siderastrea spp" = "#c780e8",
              "None" = "white"))

# labs for x and y axes
wm_pie_longo <- plot2_corals + #ggtitle("Species cover relative to total coral cover")+
  xlab("Longitude") + ylab("Latitude")

# histogram showing the average coral cover
df_hist <- data.frame (cover = cob_bentos$`calcareous turf`,
                       group = "turf")
df_hist <- rbind (df_hist, 
                  
                    data.frame (cover = apply (cob_corals[,-1],1,sum),
                                       group = "corals")) 

# hist
# corals
corals_hist <- ggplot (data = df_hist[which(df_hist$group == "corals"),], 
        aes (x = cover*100), fill = group) +
   
  geom_histogram(bins = 20,fill="#990000") +
  geom_vline(xintercept = mean(df_hist[which(df_hist$group == "corals"),"cover"]*100),
             size = 2)+
  xlab ("Average cover") + 
  ylab ("Frequency (number of sites)") + 
  theme_classic() 
 

## turf
turf_hist <- ggplot (data = df_hist[which(df_hist$group == "turf"),], 
                       aes (x = cover*100), fill = group) +
  
  geom_histogram(bins = 20,fill="#9EB23B") +
  geom_vline(xintercept = mean(df_hist[which(df_hist$group == "turf"),"cover"]*100),
             size = 2)+
  xlab ("Average cover") + 
  ylab ("Frequency (number of sites)") + 
  theme_classic()

# save in pdf

pdf(here ("output_comm_wide_R1", "MapPoints_longo.pdf"),
    width = 10,heigh=7)

grid.arrange(corals_hist,
             turf_hist,
             wm_pie_longo,
             ncol = 4,
             nrow=5,
             layout_matrix = rbind (c(NA,1,2,NA),
                                    rep(3,4),
                                    rep(3,4),
                                    rep(3,4),
                                    rep(3,4)))
  

dev.off()


# avreages

aggregate (df_hist, by  = list (df_hist$group), mean,na.rm=T)
aggregate (df_hist, by  = list (df_hist$group), range,na.rm=T)

# ---------------------------------------- #

#           ORGANIZE TRAIT DATA

# ---------------------------------------- #


## get data from Quimbayo et al. 2021

#  please check if it is correct
require(dplyr)

trait_dataset <- fish_size %>%
  
  select(scientificName, 
         family,
         measurementValue,  # actually the measurement of size
         Fish_age,
         Body_size,
         Aspect_ratio,Trophic_level, Size_group,
         TempPref_max, Depth_max) %>%
  
  group_by(scientificName,Fish_age) %>% 
  
  summarise (family = unique(family),
             actual_size=mean(as.numeric(measurementValue),na.rm=T),
             Body_size = mean(Body_size,na.rm=T),
             Aspect_ratio = mean(Aspect_ratio,na.rm=T),
             Trophic_level = mean(Trophic_level,na.rm=T),
             Size_group = unique(Size_group),
             TempPref_max = mean(TempPref_max,na.rm=T),
             Depth_max = mean(Depth_max,na.rm=T)) %>% # count the number of rows that a given answered appeared
  
  mutate (log_actual_size = log(actual_size),
          log_Body_size = log(Body_size),
          ordered_Size_group = ordered(Size_group, 
                                       levels = c("sol","pair", "smallg",
                                                  "medg", "largeg"))) 


# analyzed fish
# number of analyzed species
adult <- lapply (fish_species, function (i) i[[1]]) 
adult<-unique(unlist(adult)) # adult
juvenile <- lapply (fish_species, function (i) i[[2]]) 
juvenile<-unique(unlist(juvenile))#juvenile
table(juvenile %in% adult)# both

# subset
trait_dataset <- trait_dataset[which(trait_dataset$scientificName %in% unique(c(adult, juvenile))),]

# ordering group size
trait_dataset$Size_group <- sapply(trait_dataset$Size_group , function(x) {
  if (x=="sol") {1} 
  else if (x=="pair") {2} 
  else if (x=="smallg") {3} 
  else if (x=="medg") {4} 
  else if (x=="largeg") {5}}
)

require(dplyr)
tabS2<- trait_dataset %>% 
  mutate_at(c(4,5,6,7,8,9,10,11,12), round, 2)

write.csv (tabS2, file = here ("output_comm_wide_R1",
                                   "tabS1.2.csv"))

# ------------------------------------------------- #
# site occupancy estimates
# ------------------------------------------------- #  

# load model output


load(here("output_comm_wide_R1",
          "samples_OCCcoral_PdepthTime_longo_RdmP.RData")) 


# community response

lapply (seq(1,length(samples_OCCcoral_PdepthTime_longo_RdmP)), function (i)
  samples_OCCcoral_PdepthTime_longo_RdmP [[i]][[1]]$summary [grep("mean.beta1",rownames(samples_OCCcoral_PdepthTime_longo_RdmP[[i]][[1]]$summary)),]
  #  samples_OCCcoral_PdepthTime_longo_RdmP [[i]][[2]]$summary [grep("mean.beta1",rownames(samples_OCCcoral_PdepthTime_longo_RdmP[[i]][[2]]$summary)),]
)

# end

sims_to_df <- lapply (seq(1,length(coral_species)), function (i)
  data.frame (coral = coral_species[i],
              adult.coral = samples_OCCcoral_PdepthTime_longo_RdmP [[i]][[1]]$sims.list$mean.beta1,
              juvenile.coral = samples_OCCcoral_PdepthTime_longo_RdmP [[i]][[2]]$sims.list$mean.beta1,
              adult.turf = samples_OCCcoral_PdepthTime_longo_RdmP [[i]][[1]]$sims.list$mean.beta2,
              juvenile.turf = samples_OCCcoral_PdepthTime_longo_RdmP [[i]][[2]]$sims.list$mean.beta2)
)
sims_to_df<- do.call(rbind, sims_to_df)
sims_to_df<-melt (sims_to_df)
sims_to_df$group <- NA
sims_to_df$group[grep("coral",sims_to_df$variable)] <- "coral"
sims_to_df$group[grep("turf",sims_to_df$variable)] <- "turf"

# library
library(ggridges)
library(ggplot2)

# ridgeline plot
assem_res <- ggplot(sims_to_df, aes(x = value, y = coral, fill = variable)) +
  geom_density_ridges(alpha=0.5) +
  theme_ridges() + 
  theme(legend.position = "none") + 
  scale_fill_manual(values = c("adult.coral" = "#990000",
                               "juvenile.coral" = "#F47C7C",
                               "adult.turf" = "#4B8673",
                               "juvenile.turf" = "#C7D36F")) +
  facet_wrap(~group,scales = "fixed")



# community effect

# save as pdf
pdf (file=here("output_comm_wide_R1",
               "assemblage_response.pdf"),
     width=10,heigh=7)

assem_res # save plot

dev.off()


# ---------------------------------------- #
# information of fish species
# ---------------------------------------- #

# 
tab_spp <- data.frame (sp = todas_sp_longo[-which(is.na(todas_sp_longo))])#unique spp






### -------------------- #
###  assessing model fit
### -------------------- #



bpv_video <- lapply (seq (1,length(samples_OCCcoral_PdepthTime_longo_RdmP)), function (coral) # across corals 
  
          lapply (seq(1,length(samples_OCCcoral_PdepthTime_longo_RdmP[[coral]])) ,function (age) # across ages
  
            do.call (rbind, lapply(seq(1,length (fish_species[[coral]][[age]])), function (k) { # across species
                    
                  sum_squares <- sum (samples_OCCcoral_PdepthTime_longo_RdmP[[coral]][[age]]$sims.list$Chi2repClosed[,k] > samples_OCCcoral_PdepthTime_longo_RdmP[[coral]][[age]]$sims.list$Chi2Closed[,k]) 
                  bpvRdmP<- sum_squares/length(samples_OCCcoral_PdepthTime_longo_RdmP[[coral]][[age]]$sims.list$Chi2Closed[,k])
                  res <- data.frame (sp = fish_species[[coral]][[age]][k],
                                     mRdmP=round(bpvRdmP,3))
                  
                  ;# return
                  res
                  
        }))
        )
)
      
## data to use in the plot
extracted_data <-   lapply (seq(1,length(coral_species)), function (coral)
    
  lapply (seq(1,length(samples_OCCcoral_PdepthTime_longo_RdmP[[coral]])) ,function (age) # across ages
    
    do.call(rbind,lapply(seq(1,length (fish_species[[coral]][[age]])), function (k)  # across fish species
      
      data.frame (
        # coral
        coral = coral_species[coral],
        # fish
        peixe = fish_species [[coral]][[age]][k],
        age = age,
        # intercept and credible interval
        intercept = mean (samples_OCCcoral_PdepthTime_longo_RdmP[[coral]][[age]]$sims.list$intercept.psi [,k]),
        low.int = quantile (samples_OCCcoral_PdepthTime_longo_RdmP[[coral]][[age]]$sims.list$intercept.psi [,k], 0.05),
        high.int = quantile (samples_OCCcoral_PdepthTime_longo_RdmP[[coral]][[age]]$sims.list$intercept.psi [,k], 0.95),
        # regression coefficient and credible interval
        # corals
        estimate.coral = mean (samples_OCCcoral_PdepthTime_longo_RdmP[[coral]][[age]]$sims.list$beta1 [,k]),
        low.coral = quantile (samples_OCCcoral_PdepthTime_longo_RdmP[[coral]][[age]]$sims.list$beta1 [,k], 0.05),
        high.coral = quantile (samples_OCCcoral_PdepthTime_longo_RdmP[[coral]][[age]]$sims.list$beta1 [,k],0.95),
        # ruef
        estimate.turf = mean (samples_OCCcoral_PdepthTime_longo_RdmP[[coral]][[age]]$sims.list$beta2 [,k]),
        low.turf = quantile (samples_OCCcoral_PdepthTime_longo_RdmP[[coral]][[age]]$sims.list$beta2 [,k], 0.05),
        high.turf = quantile (samples_OCCcoral_PdepthTime_longo_RdmP[[coral]][[age]]$sims.list$beta2 [,k],0.95)
        
        
       ) # close df
     ) # close fish 
    ) # close fish
  ) # close age
) # close corals

## binding the bayeasian P- value
extracted_data<- lapply (seq(1,length(extracted_data)), function (coral)
  
  lapply (seq(1,length(extracted_data[[coral]])), function (age)
  
    cbind (extracted_data[[coral]][[age]],bpv_video [[coral]][[age]])

    )
)


# melt these data within age
extracted_data <- lapply (extracted_data, function (coral)
  
  do.call(rbind, coral)
)
                                  


# put the complete set of results in the main page of the GitHub !!

tabS3 <- do.call(rbind,extracted_data) %>% 
  mutate_at(c(4:12), round, 2) %>%
  rename ("Coral" = "coral",
          "Fish" = "peixe",
          "Age class" = "age",
          "Bayesian P-value" = "mRdmP") %>%
  select (-sp)
# save this complete result to be shown in the supoporting information
# table with all estimates, Credible intervals, and bayesian P-value
write.csv (tabS3, file = here ("output_comm_wide_R1",
                               "tabS1.3.csv"))

#############################################################
## plots of regression coefficients (fig 3 of the main text)

sp_analyzed_response <- do.call(rbind, extracted_data) # melt data
# find coral associated fish
sp_analyzed_response<-(sp_analyzed_response[which(sp_analyzed_response$low.coral>0 & 
                                    sp_analyzed_response$estimate.turf<=0),])

## organize spp names in axis-Y
## subset of each coral species

sp_analyzed_response <- cbind (sp_analyzed_response,
                               
                               trait_dataset [match (sp_analyzed_response$peixe, 
                                                   trait_dataset$scientificName), 
                                            "Body_size"])

# ordering species according to body size
sp_analyzed_response <- sp_analyzed_response[order(sp_analyzed_response$Body_size),]
sp_analyzed_response$age<-as.numeric(sp_analyzed_response$age)

# first up
sp_analyzed_response$peixe<-firstup(sp_analyzed_response$peixe)
sp_analyzed_response$coral<-firstup(sp_analyzed_response$coral)


# plotting
dodge <- c(0.2,0.2)
pd <- position_dodge(dodge)

# coefficients
a <- ggplot (sp_analyzed_response, 
             aes  (y=reorder (peixe,Body_size),
                          x=estimate.coral, 
                          fill=age,
                          colour=age,
                          group = age)) + 
  geom_errorbar(aes(xmin=low.coral,
                    xmax=high.coral),width = 0,
                position=pd) +
  facet_wrap(~coral,nrow=2)+
  theme_classic() + 
  geom_point(aes(size = mRdmP),alpha = 0.4)+ 
  geom_vline(xintercept = 0, linetype="dashed", 
             color = "gray50", size=0.5)+
  scale_colour_viridis_c(option = "magma",  begin = 0.5, end=0.8) + 
  xlab("Regression coefficient estimate") + 
  ylab ("Reef fish species") + 
  #xlim(-20, 20) + 
  #ggtitle (coral_species[[i]]) + 
  theme (#plot.title = element_text(size=9),
         legend.position = "right",
         axis.title = element_text(size=10),
         axis.text = element_text(size=7),
         axis.text.y = element_text(face = "italic"),
         strip.text = element_text(face = "italic"))

a

# save as pdf
pdf (file=here("output_comm_wide_R1",
               "fish_response.pdf"),
     width=10,heigh=7)

a # save plot

dev.off() 


## barplot to inset into the coeff plot
bar_plot_data_coral <-lapply (extracted_data, function (i)
  
  lapply (unique(extracted_data[[1]]$age), function (age) {

   
    # filter of age 
  i <- i [which(i$age == age),]    
  # dataframe for plot
  df_bar <- data.frame (
      # # positive effect of corals
      Positive.coral = ifelse(length(table (i$low.coral >0 ))==1,
                           0,
                           (table (i$low.coral >0))[2]),
    
      ## no effect coral
      Neutral.coral= ifelse(length(table (i$low.coral <0 &  i$high.coral >0))==1,
               0,
               (table (i$low.coral <0 &  i$high.coral >0))[2]),
                   
      ## negative effect
      Negative.coral =ifelse(length(table (i$high.coral <0))==1,
                      0,
                      (table (i$high.coral <0))[2]),
      
      
      ## positive effect of turf
      Positive.turf = ifelse(length(table (i$low.turf >0))==1,
                             0,
                             (table (i$low.turf >0  ))[2]),
      
      
      ## no effect
      Neutral.turf= ifelse(length(table (i$low.turf <0 &  i$high.turf >0))==1,
                 0,
                 (table (i$low.turf <0 &  i$high.turf >0))[2]),
      
      ## negative effect
      Negative.turf=ifelse(length(table (i$high.turf <0))==1,
                      0,
                      (table (i$high.turf <0))[2]),
      
      # total number of spp
      nSP = sum(table (i$low.coral >0)),
      
      age = age
      
      )
  df_bar
    }
))


# results of percentage of each effect (positive, no/neutral, negative on occupancy probability)
names (bar_plot_data_coral)<- coral_species

# melt to df

#bar_plot_data_turf<- do.call(rbind,lapply (bar_plot_data_turf, function (i) do.call(rbind,i)))
#bar_plot_data_turf<-cbind (bar_plot_data_turf, group = "turf")
bar_plot_data_coral<-do.call(rbind,lapply (bar_plot_data_coral, function (i) do.call(rbind,i)))
# coral names
bar_plot_data_coral$coral <- sapply (strsplit(rownames(bar_plot_data_coral), "\\."), "[[",1)


# melt into a df
bar_plot_data_turf_coral <- melt (bar_plot_data_coral,c("age", "coral"))
# rm number of spp
#bar_plot_data_turf_coral <- bar_plot_data_turf_coral[-which(bar_plot_data_turf_coral$variable == "nSP"),]
bar_plot_data_turf_coral$coral<-firstup(bar_plot_data_turf_coral$coral)
bar_plot_data_turf_coral<- bar_plot_data_turf_coral[-which(bar_plot_data_turf_coral$variable == "nSP"),]
bar_plot_data_turf_coral$group <-  sapply (strsplit(as.character(bar_plot_data_turf_coral$variable), "\\."), "[[",2)
bar_plot_data_turf_coral$group<-firstup(bar_plot_data_turf_coral$group)
bar_plot_data_turf_coral$Relationship <-  sapply (strsplit(as.character(bar_plot_data_turf_coral$variable), "\\."), "[[",1)
bar_plot_data_turf_coral$age <- ifelse(bar_plot_data_turf_coral$age == 1, "Adult","Juvenile")


# plot

barplots<-ggplot(bar_plot_data_turf_coral,
                              
                                aes(fill=Relationship,
                                     y=value, 
                                     x=coral,
                                     group=Relationship
                                    )) + 
  geom_bar(position="stack", stat="identity",size=1,colour="black") + 
  
  theme_classic()+

  theme (axis.text.x = element_text(angle=45,  hjust = 1,face="italic"),
         legend.position = "top",
         axis.ticks.x = element_blank(),
         axis.line.x = element_blank()) + 
  scale_fill_viridis_d(option="magma",begin =0.5,end=1) + 
  facet_wrap(~age+group,scales = "free_y")+
  xlab ("Coral species") + 
  ylab ("Number of species")

barplots


# save plots png
png (here ("output_comm_wide_R1","figures", "fig3.png"),
     width = 15, height = 15, units = "cm",res=300)

barplots

dev.off()


# ------------------------------------------------------- #
# functional spaces
# here considering that all fish species with influence 
# of corals would disappear in a future coral-less world
# ------------------------------------------------------ #



#---------------------------------------
# 		LOSS PER CORAL
# ------------------------------------

# a complete functional space per species of coral
total <- do.call (rbind,extracted_data) # rbind extracted data (with coefficients and CI)


# first calculate gower distance on traits
sel_traits <- trait_dataset[which(trait_dataset$scientificName %in% total$peixe),
                            c("Aspect_ratio","Trophic_level","Size_group",
                              "TempPref_max","Depth_max",
                              "log_actual_size",
                              "scientificName")]

# the correlation between traits
(cor(sel_traits[,-which(colnames(sel_traits) == "scientificName")], 
    use = "complete.obs"))
#  correlation is fine



# distance matrix 
gower_matrix <- daisy (apply (sel_traits[,-which(colnames(sel_traits) == "scientificName")],
                              2,scale), 
                       metric=("gower"),
                       type = list (ordratio = "Size_group")) 


# principal coordinate analysis
# Building the functional space based on a PCOA 
pco<-dudi.pco(quasieuclid(gower_matrix), scannf=F, nf=10) # quasieuclid() transformation to make the gower matrix as euclidean. nf= number of axis 



#barplot(pco$eig) # barplot of eigenvalues for each axis 
(Inertia2<-(pco$eig[1]+pco$eig[2]+pco$eig[3]) /(sum(pco$eig))) # percentage of inertia explained by the two first axes

# estimate quality of f space
quality<-quality_funct_space_fromdist( gower_matrix,  nbdim=10,   
                                       plot="quality_funct_space_I") # it will produce a plot (hosted in the root folder)



## only the frst axis
(Inertia.first <- (pco$eig[1]) /(sum(pco$eig)))
## only the frst axis
(Inertia.scnd <- (pco$eig[2]) /(sum(pco$eig)))
## only the frst axis
(Inertia.trd <- (pco$eig[3]) /(sum(pco$eig)))
Inertia.first+Inertia.scnd
## complete space
all <- cbind (pco$li[,1:2],
              ext = F,
              sp = sel_traits$scientificName)
a <- all [chull(all[,1:2], y = NULL),] # its convex hull

# match ordination and traits
total<- cbind (total,
               all[(match (total$peixe, all$sp)),c("A1", "A2")])


# coral associated
c_assoc <-cbind(all, ext1=ifelse(all$sp %in% 
                                   unique(tolower (sp_analyzed_response$peixe)),T,F))
c_assoc <-c_assoc[which(c_assoc$ext1==T),]
c_assoc_set <- c_assoc [chull(c_assoc, y = NULL),]

# age

RTS_age <- lapply (unique(total$age), function (age){

      # coral-associated fish
      coral_associated <- total[which(total$age == age &
                                      total$low.coral > 0 & 
                                      total$estimate.turf<=0),] 
      
      
      # reduced space
      setB<-cbind(all, ext1=ifelse(all$sp %in% 
                                    unique(coral_associated$peixe),T,F))
      pk <-setB[which(setB$ext1==F),]
      f <- pk [chull(pk, y = NULL),]
      
      # turf-associated fish
      #turf_associated <- total[which(total$age == age &
      #                                 total$low.turf > 0&
      #                               total$estimate.coral <= 0  
      #                               ),] 
      
      # reduced space
      #setT<-cbind(all, ext1=ifelse(all$sp %in% 
      #                               unique(turf_associated$peixe),T,F))
      #pkT <-setT[which(setT$ext1==F),]
      #turf <- pkT [chull(pkT, y = NULL),]
      
      
      # quantifying reduction in functional space
      # https://chitchatr.wordpress.com/2015/01/23/calculating-the-area-of-a-convex-hull/
      chull.poly.complete <- Polygon(a[,1:2], hole=F)
      chull.area.complete <- chull.poly.complete@area
      
      
      
      # coral
      chull.poly.coral <- Polygon(f[,1:2], hole=F)
      chull.area.coral <- chull.poly.coral@area
      
      
      
      # turf
      #chull.poly.turf <- Polygon(turf[,1:2], hole=F)
      #chull.area.turf <- chull.poly.turf@area
      
      
      
      # calculating reductions across all corals
      
      RTS<-data.frame (corals=(1-(chull.area.coral/chull.area.complete))*100#,
                  #turf=(1-(chull.area.turf/chull.area.complete))*100
                  )
      # resuts to report
      res <- list (RTS = RTS,
                   coral.associated =  coral_associated$peixe[order(coral_associated$peixe)],
                   #turf_associated = turf_associated$peixe[order(turf_associated$peixe)],
                   space.coral = f#,
                   #space.turf = turf
                   )
      ;
      res
})

#  results
names (RTS_age) <- c("adult", "juvenile")

# the loss per coral
RTS_per_coral <-lapply (unique(total$coral), function (coral) 
  
                  lapply (unique(total$age), function (age)   {
    
    # coral-associated fish
    coral_associated <- total[which(total$low.coral > 0 & 
                                      total$estimate.turf <= 0 & 
                                      total$age == age & 
                                      total$coral == coral),] 
    
    
    # reduced space
    setB<-cbind(all, ext1=ifelse(all$sp %in% 
                                   unique(coral_associated$peixe),T,F))
    pk <-setB[which(setB$ext1==F),]
    f <- pk [chull(pk, y = NULL),]
    
    # turf-associated fish
    #turf_associated <- total[which(total$high.coral < 0 & 
    #                                 total$low.turf > 0 & 
    #                                 total$age == age & 
    #                                 total$coral == coral),] 
    
    # reduced space
    #setT<-cbind(all, ext1=ifelse(all$sp %in% 
    #                               unique(turf_associated$peixe),T,F))
    #pkT <-setT[which(setT$ext1==F),]
    #turf <- pkT [chull(pkT, y = NULL),]
    
    
    
    # quantifying reduction in functional space
    # https://chitchatr.wordpress.com/2015/01/23/calculating-the-area-of-a-convex-hull/
    chull.poly.complete <- Polygon(a[,1:2], hole=F)
    chull.area.complete <- chull.poly.complete@area
    
    
    
    # coral
    chull.poly.coral <- Polygon(f[,1:2], hole=F)
    chull.area.coral <- chull.poly.coral@area
    
    
    
    # turf
    #chull.poly.turf <- Polygon(turf[,1:2], hole=F)
    #chull.area.turf <- chull.poly.turf@area
    
    
    
    
    # calculating reductions across all corals
    
    RTS<-data.frame (corals=(1-(chull.area.coral/chull.area.complete))*100#,
                     #turf=(1-(chull.area.turf/chull.area.complete))*100
                     )
    
    # results
    res <- list (RTS = RTS,
                 coral.associated =  coral_associated$peixe[order(coral_associated$peixe)],
                 #turf_associated = turf_associated$peixe[order(turf_associated$peixe)],
                 space.coral = f#,
                 #space.turf = turf
                 )
    ; # return
    res

    }
    )# close age
    ) # close corals

names (RTS_per_coral) <- coral_species


# =======================================================
# simulations of total random loss

## extracted data of impaired species of each coral spp
impaired_sp <- replicate (100,sample (total$peixe,# 100 samples 
                                      length(unique(total [which(total$low.coral >0 & 
                                                                   total$estimate.turf <= 0),
                                                           "peixe"]))
                                      
                                      ))# of size equal N associated fish

# run random sampling
extinction_random <- lapply(seq (1,ncol(impaired_sp)), function (i) {
  
  # reduced space
  setB.rdm<-cbind(all, ext1=ifelse(all$sp %in% impaired_sp[,i],T,F))
  pk.rdm <-setB.rdm[which(setB.rdm$ext1==F),]
  f.rdm <- pk.rdm [chull(pk.rdm, y = NULL),]
  f.rdm<- cbind(f.rdm,dt=i)
  ## quantifying reduction in functional space
  
  # https://chitchatr.wordpress.com/2015/01/23/calculating-the-area-of-a-convex-hull/
  chull.poly.complete <- Polygon(a[,1:2], hole=F)
  chull.area.complete <- chull.poly.complete@area
  
  
  # random area
  chull.poly.random <- Polygon(f.rdm[,1:2], hole=F)
  chull.area.random <- chull.poly.random@area
  
  # calculating reductions across all corals
  RTS<-data.frame (value =(1-(chull.area.random/chull.area.complete))*100)
  RTS <- list (RTS=RTS,
               space = f.rdm)
  ; # return
  RTS
})
  


# simulations of  random loss per coral

## extracted data of impaired species of each coral spp
impaired_sp_per_coral <- lapply (unique(total$coral), function (coral) { 
  
  
        (replicate (100,# 100 samples 
                   sample (total$peixe[total$coral == coral], # maintain composition
                                length(unique(total [which(total$low.coral >0 & 
                                                    total$estimate.turf <= 0 &
                                                    total$coral == coral),
                                              "peixe"]))
                           
                           )
                   ))
  
  }
                                 
)# of size equal N associated fish

## adjusting this case of only one species
#impaired_sp_per_coral[[6]]<-matrix(impaired_sp_per_coral[[6]],nrow=1)

# run random sampling
extinction_random_per_coral <- lapply(impaired_sp_per_coral, function (coral)
  
          lapply (seq (1,ncol(coral)), function (i) {
  
        # reduced space
        setB.rdm<-cbind(all, ext1=ifelse(all$sp %in% coral[,i],T,F))
        pk.rdm <-setB.rdm[which(setB.rdm$ext1==F),]
        f.rdm <- pk.rdm [chull(pk.rdm, y = NULL),]
        f.rdm<- cbind(f.rdm,dt=i)
        ## quantifying reduction in functional space
        
        # https://chitchatr.wordpress.com/2015/01/23/calculating-the-area-of-a-convex-hull/
        chull.poly.complete <- Polygon(a[,1:2], hole=F)
        chull.area.complete <- chull.poly.complete@area
        
        
        # random area
        chull.poly.random <- Polygon(f.rdm[,1:2], hole=F)
        chull.area.random <- chull.poly.random@area
        
        # calculating reductions across all corals
        RTS<-data.frame (value =(1-(chull.area.random/chull.area.complete))*100)
        RTS <- list (RTS=RTS,
                     space = f.rdm)
        ; # return
        RTS
}))

# ===================================================

# violin plot showing deviations from random

# total loss
loss_total <- lapply  (RTS_age, function (i) i$RTS)
loss_total<- do.call (rbind, loss_total)
loss_total<-cbind (loss_total,scenario = "Total loss")
loss_total$age <- rownames(loss_total)
loss_total$coral <- "all"
loss_total <- melt (loss_total)

# loss per coral
loss_per_coral <- lapply (RTS_per_coral, function (i) 
  
      do.call(rbind ,
              lapply (i, function (k) k$RTS)
      )
      
      )


loss_per_coral <- lapply (loss_per_coral, function (i){
  
  rownames(i) <- c("adult", "juvenile")
  
  i
  })


# melt
loss_per_coral <- do.call(rbind.data.frame, loss_per_coral)
loss_per_coral<-cbind (loss_per_coral, scenario = "Loss per coral")
loss_per_coral$age <- rownames(loss_per_coral)
loss_per_coral$age <- sapply (strsplit(loss_per_coral$age, "\\."), "[[",2)
loss_per_coral$coral <- sapply (strsplit(rownames(loss_per_coral), "\\."), "[[",1)
loss_per_coral <- melt (loss_per_coral)

# random trait space
RTS_random <- do.call(rbind, sapply (extinction_random, "[","RTS"))
RTS_random <- data.frame (RTS_random, 
                          scenario="random",
                          age = NA, 
                          coral = NA,
                          variable = "random")
RTS_random <- RTS_random[,match (colnames(loss_per_coral),colnames(RTS_random))]

# impaired spp per coral
RTS_random_per_coral <- lapply (extinction_random_per_coral, function (coral)
  
  do.call(rbind,lapply (coral, function (sim) 
    
    sim$RTS
  ))
)

# melt
RTS_random_per_coral<- do.call(cbind,RTS_random_per_coral)
colnames (RTS_random_per_coral) <- coral_species

# long format
RTS_random_per_coral<-melt (RTS_random_per_coral)
RTS_random_per_coral <- data.frame (coral = RTS_random_per_coral$variable,
                                    value = RTS_random_per_coral$value,
                                    scenario="random_per_coral",
                                    age = NA, 
                                    variable = "random")
RTS_random_per_coral <- RTS_random_per_coral[,match (colnames(loss_per_coral),colnames(RTS_random_per_coral))]

# bind all these data for violin plots
data_violin <- rbind (loss_total,
                      loss_per_coral,
                      RTS_random, 
                      RTS_random_per_coral)


# group for comparison
data_violin$comparison <- ifelse (data_violin$scenario %in% c("random","Total loss"),
                                   "random",
                                   "random_per_coral")
data_violin$coral<-firstup(data_violin$coral)
# adjust size
# number is the point size
data_violin$age<- (ifelse (data_violin$age == "adult",2,1))

# plot
require(ggplot2)

violin1 <- ggplot(data_violin[which(data_violin$scenario == "random"),],  # subset (null dataset)
       aes (x = comparison, 
            y = value,
            fill=scenario)) +
  
  # facets
  #facet_wrap(~Trait,ncol=3,scales = "free_y") +
  
  # violin plot for the null dataset
  geom_violin(size=1,
              col = "gray80",
              fill = "gray",
              alpha = 0.1) +
  
  # boxplot for the observed dataset
  geom_jitter(data = data_violin[which(data_violin$scenario == "Total loss"),],  # subset (observed dataset)
               aes (x = comparison, 
                    y = value,
                    fill= variable,
                    size=as.factor(age),
                    colour= variable),
              width = 0.12, height = 0,
               alpha=0.8) +
  scale_colour_viridis_d (option="magma", begin = 0.6,end=0.6)+
  stat_summary(fun = "mean",
               geom = "point",
               color = "gray50",
               size=4)+
  
  #stat_summary(fun=mean,
  #             geom="point",
  #             shape=19, size=3) + 
  theme_classic() + xlab ("") + 
  ylab (expression("Reduction in Functional trait Space (RFS, %)")) +
  theme (axis.title = element_text(size=15),
         axis.text.x = element_blank(),
         legend.position = c(0.8,0.7)) + 
  ggtitle ("Total loss vs. random loss")

violin1


# violin 2, loss per coral

# plot
head(data_violin)

violin2 <- ggplot(data_violin[which(data_violin$scenario == "random_per_coral"),],  # subset (null dataset)
                  aes (x = comparison, 
                       y = value,
                       fill=scenario)) +
  
  # facets
  facet_wrap(~coral,ncol=4,scales = "fixed") +
  
  # violin plot for the null dataset
  geom_violin(size=1,
              col = "gray80",
              fill = "gray",
              alpha = 0.1) +
  
  # boxplot for the observed dataset
  geom_jitter(data = data_violin[which(data_violin$scenario == "Loss per coral"),],  # subset (observed dataset)
              aes (x = comparison, 
                   y = value,
                   fill= variable,
                   size=age,
                   colour= variable),
              width = 0.12, height = 0,
              alpha=0.8) +
  scale_colour_viridis_d (option="magma", begin = 0.6,end=0.6)+
  stat_summary(fun = "mean",
               geom = "point",
               color = "gray50",
               size=4)+

  #stat_summary(fun=mean,
  #             geom="point",
  #             shape=19, size=3) + 
  theme_classic() + xlab ("") + 
  ylab ("") +
  theme (axis.title = element_text(size=15),
         axis.text.x = element_blank(),
         legend.position = "none",
         strip.text = element_text(face = "italic")) + 
  ggtitle ("Loss per coral vs. random loss per coral")


violin2

pdf(here ("output_comm_wide_R1", "RTS.pdf"),height=5,width=9)
grid.arrange(violin1,
             violin2,ncol=6,nrow=1,
             layout_matrix = rbind (c(1,1,2,2,2,2)))

dev.off()




# ========================================================
# trait spaces

# plots

## plot A (complete space)
plotA <- ggplot(a, aes(A1, A2)) + 
  geom_point(size=2) + theme_bw()+
  geom_polygon(data=a, aes (A1,A2),
               alpha=0.6,
               fill="gray",
               colour = "black",
               size=1,
               linetype = 2) + # complete space
  geom_polygon(data=c_assoc_set, aes (A1,A2),
               alpha=0.3,
               fill="yellow",
               colour = "yellow",
               size=1,
               linetype = 3) +
  geom_polygon(data=RTS_age$juvenile$space.coral, aes (A1,A2,group=ext1, fill=ext1),
               alpha=0.3, # reduced space
               fill="#990000",
               size=3) +
  geom_polygon(data=RTS_age$adult$space.coral, 
               aes (A1,A2,group=ext1, fill=ext1),
               alpha=0.5, # reduced space
               fill="#F47C7C",size=3) + 
  geom_text_repel(data = a, aes (x=A1, y=A2, label=firstup(sp)),
                  size=3)+
  
  ggtitle ("Total loss") + 
  xlab(paste ("Axis I:", round(Inertia.first*100,2),"%"))+
  ylab(paste ("Axis II:", round(Inertia.scnd*100,2),"%"))+
  theme (plot.title = element_text(size=12))


plotA

## space per coral
# plot per coral spp

plotB <- lapply (seq (1,length(RTS_per_coral)), function (coral) { 

  
  ggplot(a, aes(A1, A2)) + 
  geom_point(size=2) + theme_bw()+
  geom_polygon(data=a, aes (A1,A2),
               alpha=0.3,
               fill="gray",
               colour = "black",
               size=1,
               linetype = 2) + # complete space
  geom_polygon(data=RTS_per_coral[[coral]][[1]]$space.coral, 
               aes (A1,A2,group=ext1, fill=ext1),
               alpha=0.3, # reduced space
               fill="#990000",
               size=3) +
  geom_polygon(data=RTS_per_coral[[coral]][[2]]$space.coral, 
               aes (A1,A2,group=ext1, fill=ext1),
               alpha=0.3, # reduced space
               fill="#F47C7C",size=3,
               linetype = 4) + 
  ggtitle (firstup(coral_species[coral]))+
    theme (plot.title = element_text(size=12,face="italic"),
           axis.title = element_blank(),
           axis.text = element_blank())

  }
)

plotB[[3]]

## correlations to project trait values into the ordination
correlations <- cor (data.frame(sel_traits[,-which(colnames(sel_traits) == "scientificName")],
                                pco$li[,1:3]),
                     use = "complete.obs")
correlations<-correlations [,c("A1","A2")]# interesting correlations


# plot of the random trait space
random_space <- do.call(rbind, sapply (extinction_random, "[","space"))

plotC <- ggplot(random_space, aes(A1, A2,
                          group=dt),
                          fill="#D3EBCD",
                          colour = "#AEDBCE",
                          size=1) + 
  theme_bw()+
  geom_polygon(alpha=0.01)  # complete space
  

plotC <- plotC+ geom_segment(aes(x = 0, y = 0, 
                   xend = correlations[1,1]*0.2, 
                   yend = correlations[1,2]*0.2),size = 1,
                   color="black",
               arrow = arrow(length = unit(.35, "cm")))  + 
  ## annotate
  annotate(geom="text",x=correlations[1,1]*0.25,
           y=correlations[1,2]*0.24,label="Aspect ratio",
           color="black") +
  
  geom_segment(aes(x = 0, y = 0, 
                   xend = correlations[2,1]*0.2, 
                   yend = correlations[2,2]*0.2),size = 1,
               color="black",
               arrow = arrow(length = unit(.35, "cm"))) + 
  annotate(geom="text",x=correlations[2,1]*0.25,
           y=correlations[2,2]*0.25,label="Trophic level",
           color="black") +
  
  geom_segment(aes(x = 0, y = 0, 
                   xend = correlations[3,1]*0.2, 
                   yend = correlations[3,2]*0.2),size = 1,
               color="black",
               arrow = arrow(length = unit(.35, "cm"))) + 
  annotate(geom="text",x=correlations[3,1]*0.20,
           y=correlations[3,2]*0.25,label="Group size",
           color="black") +
  
  geom_segment(aes(x = 0, y = 0, 
                   xend = correlations[4,1]*0.2, 
                   yend = correlations[4,2]*0.2),size = 1,
               color="black",
               arrow = arrow(length = unit(.35, "cm"))) + 
  annotate(geom="text",x=correlations[4,1]*0.25,
           y=correlations[4,2]*0.29,label="TºC max",
           color="black") + 
  
  geom_segment(aes(x = 0, y = 0, 
                   xend = correlations[5,1]*0.2, 
                   yend = correlations[5,2]*0.2),size = 1,
               color="black",
               arrow = arrow(length = unit(.35, "cm"))) + 
  annotate(geom="text",x=correlations[5,1]*0.25,
           y=correlations[5,2]*0.23,label="Depth max",
           color="black",) + 
  
  geom_segment(aes(x = 0, y = 0, 
                   xend = correlations[6,1]*0.2, 
                   yend = correlations[6,2]*0.2),size = 1,
               color="black",
               arrow = arrow(length = unit(.35, "cm"))) + 
  annotate(geom="text",x=correlations[6,1]*0.18,
           y=correlations[6,2]*0.25,label="Body size",
           color="black") + 
  ggtitle ("Random loss") +
  theme (plot.title = element_text(size=12),
         axis.title = element_blank(),
         axis.text = element_blank())

# save to pdf
png (here ("output_comm_wide_R1", "figures","figS1.1.png"),
     width = 40, height = 12, units = "cm",res=300)

# organize the trait spaces
grid.arrange (plotA,
              plotB[[1]],
              plotB[[2]],
              plotB[[3]],
              plotB[[4]],
              plotB[[5]],
              plotB[[6]],
              plotB[[7]],
              plotB[[8]],
              plotC,ncol = 8,nrow=2,
              
              layout_matrix = rbind (c(1,1,2,3,4,5,10,10),
                                     c(1,1,6,7,8,9,10,10))
              )

dev.off()


# ==========================================================

# supporting information with an alternative randomization

## extracted data of impaired species of each coral spp
impaired_sp <- replicate (100,sample (total$peixe,# 100 samples 
                                      length(unique(total [which(total$low.coral >0 & 
                                                                   total$estimate.turf <= 0),
                                                           "peixe"])),
                                      
                                      #prob = ifelse (total$estimate.coral >=0,
                                      #               abs(total$estimate.coral/max(total$estimate.coral)),
                                      #                   0)
                                      
                                      prob = abs(rowSums(total[,c("A1", "A2")])/max(rowSums(total[,c("A1", "A2")])))
                                      
                                      
))# of size equal N associated fish

# run random sampling
extinction_random <- lapply(seq (1,ncol(impaired_sp)), function (i) {
  
  # reduced space
  setB.rdm<-cbind(all, ext1=ifelse(all$sp %in% impaired_sp[,i],T,F))
  pk.rdm <-setB.rdm[which(setB.rdm$ext1==F),]
  f.rdm <- pk.rdm [chull(pk.rdm, y = NULL),]
  f.rdm<- cbind(f.rdm,dt=i)
  ## quantifying reduction in functional space
  
  # https://chitchatr.wordpress.com/2015/01/23/calculating-the-area-of-a-convex-hull/
  chull.poly.complete <- Polygon(a[,1:2], hole=F)
  chull.area.complete <- chull.poly.complete@area
  
  
  # random area
  chull.poly.random <- Polygon(f.rdm[,1:2], hole=F)
  chull.area.random <- chull.poly.random@area
  
  # calculating reductions across all corals
  RTS<-data.frame (value =(1-(chull.area.random/chull.area.complete))*100)
  RTS <- list (RTS=RTS,
               space = f.rdm)
  ; # return
  RTS
})



# simulations of  random loss per coral

## extracted data of impaired species of each coral spp
impaired_sp_per_coral <- lapply (unique(total$coral), function (coral) { 
  
  
  (replicate (100,# 100 samples 
              sample (total$peixe[total$coral == coral], # maintain composition
                      length(unique(total [which(total$low.coral >0 & 
                                                   total$estimate.turf <= 0 &
                                                   total$coral == coral),
                                           "peixe"])),
                      
                      # prob = ifelse (total[total$coral == coral,"estimate.coral"] >=0,
                      #              abs(total[total$coral == coral,"estimate.coral"]/
                      #                    max(total[total$coral == coral,"estimate.coral"])),
                      #              0)
                      prob = abs(rowSums(total[total$coral == coral,c("A1", "A2")])/
                                   max(rowSums(total[total$coral == coral,c("A1", "A2")])))
                      
                      
                      
              )
  ))
  
}

)# of size equal N associated fish

## adjusting this case of only one species
#impaired_sp_per_coral[[6]]<-matrix(impaired_sp_per_coral[[6]],nrow=1)

# run random sampling
extinction_random_per_coral <- lapply(impaired_sp_per_coral, function (coral)
  
  lapply (seq (1,ncol(coral)), function (i) {
    
    # reduced space
    setB.rdm<-cbind(all, ext1=ifelse(all$sp %in% coral[,i],T,F))
    pk.rdm <-setB.rdm[which(setB.rdm$ext1==F),]
    f.rdm <- pk.rdm [chull(pk.rdm, y = NULL),]
    f.rdm<- cbind(f.rdm,dt=i)
    ## quantifying reduction in functional space
    
    # https://chitchatr.wordpress.com/2015/01/23/calculating-the-area-of-a-convex-hull/
    chull.poly.complete <- Polygon(a[,1:2], hole=F)
    chull.area.complete <- chull.poly.complete@area
    
    
    # random area
    chull.poly.random <- Polygon(f.rdm[,1:2], hole=F)
    chull.area.random <- chull.poly.random@area
    
    # calculating reductions across all corals
    RTS<-data.frame (value =(1-(chull.area.random/chull.area.complete))*100)
    RTS <- list (RTS=RTS,
                 space = f.rdm)
    ; # return
    RTS
  }))

# ===================================================

# violin plot showing deviations from random

# total loss
loss_total <- lapply  (RTS_age, function (i) i$RTS)
loss_total<- do.call (rbind, loss_total)
loss_total<-cbind (loss_total,scenario = "Total loss")
loss_total$age <- rownames(loss_total)
loss_total$coral <- "all"
loss_total <- melt (loss_total)

# loss per coral
loss_per_coral <- lapply (RTS_per_coral, function (i) 
  
  do.call(rbind ,
          lapply (i, function (k) k$RTS)
  )
  
)


loss_per_coral <- lapply (loss_per_coral, function (i){
  
  rownames(i) <- c("adult", "juvenile")
  
  i
})


# melt
loss_per_coral <- do.call(rbind.data.frame, loss_per_coral)
loss_per_coral<-cbind (loss_per_coral, scenario = "Loss per coral")
loss_per_coral$age <- rownames(loss_per_coral)
loss_per_coral$age <- sapply (strsplit(loss_per_coral$age, "\\."), "[[",2)
loss_per_coral$coral <- sapply (strsplit(rownames(loss_per_coral), "\\."), "[[",1)
loss_per_coral <- melt (loss_per_coral)

# random trait space
RTS_random <- do.call(rbind, sapply (extinction_random, "[","RTS"))
RTS_random <- data.frame (RTS_random, 
                          scenario="random",
                          age = NA, 
                          coral = NA,
                          variable = "random")
RTS_random <- RTS_random[,match (colnames(loss_per_coral),colnames(RTS_random))]

# impaired spp per coral
RTS_random_per_coral <- lapply (extinction_random_per_coral, function (coral)
  
  do.call(rbind,lapply (coral, function (sim) 
    
    sim$RTS
  ))
)

# melt
RTS_random_per_coral<- do.call(cbind,RTS_random_per_coral)
colnames (RTS_random_per_coral) <- coral_species

# long format
RTS_random_per_coral<-melt (RTS_random_per_coral)
RTS_random_per_coral <- data.frame (coral = RTS_random_per_coral$variable,
                                    value = RTS_random_per_coral$value,
                                    scenario="random_per_coral",
                                    age = NA, 
                                    variable = "random")
RTS_random_per_coral <- RTS_random_per_coral[,match (colnames(loss_per_coral),colnames(RTS_random_per_coral))]

# bind all these data for violin plots
data_violin <- rbind (loss_total,
                      loss_per_coral,
                      RTS_random, 
                      RTS_random_per_coral)


# group for comparison
data_violin$comparison <- ifelse (data_violin$scenario %in% c("random","Total loss"),
                                  "random",
                                  "random_per_coral")
data_violin$coral<-firstup(data_violin$coral)
# adjust size
# number is the point size
data_violin$age<- (ifelse (data_violin$age == "adult",2,1))

# plot
require(ggplot2)

violin1 <- ggplot(data_violin[which(data_violin$scenario == "random"),],  # subset (null dataset)
                  aes (x = comparison, 
                       y = value,
                       fill=scenario)) +
  
  # facets
  #facet_wrap(~Trait,ncol=3,scales = "free_y") +
  
  # violin plot for the null dataset
  geom_violin(size=1,
              col = "gray80",
              fill = "gray",
              alpha = 0.1) +
  
  # boxplot for the observed dataset
  geom_jitter(data = data_violin[which(data_violin$scenario == "Total loss"),],  # subset (observed dataset)
              aes (x = comparison, 
                   y = value,
                   fill= variable,
                   size=as.factor(age),
                   colour= variable),
              width = 0.12, height = 0,
              alpha=0.8) +
  scale_colour_viridis_d (option="magma", begin = 0.6,end=0.6)+
  stat_summary(fun = "mean",
               geom = "point",
               color = "gray50",
               size=4)+
  
  #stat_summary(fun=mean,
  #             geom="point",
  #             shape=19, size=3) + 
  theme_classic() + xlab ("") + 
  ylab (expression("Reduction in Functional trait Space (RFS, %)")) +
  theme (axis.title = element_text(size=15),
         axis.text.x = element_blank(),
         legend.position = c(0.8,0.7)) + 
  ggtitle ("Total loss vs. random loss")

violin1


# violin 2, loss per coral

# plot
head(data_violin)

violin2 <- ggplot(data_violin[which(data_violin$scenario == "random_per_coral"),],  # subset (null dataset)
                  aes (x = comparison, 
                       y = value,
                       fill=scenario)) +
  
  # facets
  facet_wrap(~coral,ncol=4,scales = "fixed") +
  
  # violin plot for the null dataset
  geom_violin(size=1,
              col = "gray80",
              fill = "gray",
              alpha = 0.1) +
  
  # boxplot for the observed dataset
  geom_jitter(data = data_violin[which(data_violin$scenario == "Loss per coral"),],  # subset (observed dataset)
              aes (x = comparison, 
                   y = value,
                   fill= variable,
                   size=age,
                   colour= variable),
              width = 0.12, height = 0,
              alpha=0.8) +
  scale_colour_viridis_d (option="magma", begin = 0.6,end=0.6)+
  stat_summary(fun = "mean",
               geom = "point",
               color = "gray50",
               size=4)+
  
  #stat_summary(fun=mean,
  #             geom="point",
  #             shape=19, size=3) + 
  theme_classic() + xlab ("") + 
  ylab ("") +
  theme (axis.title = element_text(size=15),
         axis.text.x = element_blank(),
         legend.position = "none",
         strip.text = element_text(face = "italic")) + 
  ggtitle ("Loss per coral vs. random loss per coral")


violin2

pdf(here ("output_comm_wide_R1", "RTS_vertices.pdf"),height=5,width=9)
grid.arrange(violin1,
             violin2,ncol=6,nrow=1,
             layout_matrix = rbind (c(1,1,2,2,2,2)))

dev.off()




# ========================================================
# trait spaces

# plots

## plot A (complete space)
plotA <- ggplot(a, aes(A1, A2)) + 
  geom_point(size=2) + theme_bw()+
  geom_polygon(data=a, aes (A1,A2),
               alpha=0.6,
               fill="gray",
               colour = "black",
               size=1,
               linetype = 2) + # complete space
  geom_polygon(data=c_assoc_set, aes (A1,A2),
               alpha=0.3,
               fill="yellow",
               colour = "yellow",
               size=1,
               linetype = 3) +
  geom_polygon(data=RTS_age$juvenile$space.coral, aes (A1,A2,group=ext1, fill=ext1),
               alpha=0.3, # reduced space
               fill="#990000",
               size=3) +
  geom_polygon(data=RTS_age$adult$space.coral, 
               aes (A1,A2,group=ext1, fill=ext1),
               alpha=0.5, # reduced space
               fill="#F47C7C",size=3) + 
  geom_text_repel(data = a, aes (x=A1, y=A2, label=firstup(sp)),
                  size=3)+
  
  ggtitle ("Total loss") + 
  xlab(paste ("Axis I:", round(Inertia.first*100,2),"%"))+
  ylab(paste ("Axis II:", round(Inertia.scnd*100,2),"%"))+
  theme (plot.title = element_text(size=12))


plotA

## space per coral
# plot per coral spp

plotB <- lapply (seq (1,length(RTS_per_coral)), function (coral) { 
  
  
  ggplot(a, aes(A1, A2)) + 
    geom_point(size=2) + theme_bw()+
    geom_polygon(data=a, aes (A1,A2),
                 alpha=0.3,
                 fill="gray",
                 colour = "black",
                 size=1,
                 linetype = 2) + # complete space
    geom_polygon(data=RTS_per_coral[[coral]][[1]]$space.coral, 
                 aes (A1,A2,group=ext1, fill=ext1),
                 alpha=0.3, # reduced space
                 fill="#990000",
                 size=3) +
    geom_polygon(data=RTS_per_coral[[coral]][[2]]$space.coral, 
                 aes (A1,A2,group=ext1, fill=ext1),
                 alpha=0.3, # reduced space
                 fill="#F47C7C",size=3,
                 linetype = 4) + 
    ggtitle (firstup(coral_species[coral]))+
    theme (plot.title = element_text(size=12,face="italic"),
           axis.title = element_blank(),
           axis.text = element_blank())
  
}
)

plotB[[3]]

## correlations to project trait values into the ordination
correlations <- cor (data.frame(sel_traits[,-which(colnames(sel_traits) == "scientificName")],
                                pco$li[,1:3]),
                     use = "complete.obs")
correlations<-correlations [,c("A1","A2")]# interesting correlations


# plot of the random trait space
random_space <- do.call(rbind, sapply (extinction_random, "[","space"))

plotC <- ggplot(random_space, aes(A1, A2,
                                  group=dt),
                fill="#D3EBCD",
                colour = "#AEDBCE",
                size=1) + 
  theme_bw()+
  geom_polygon(alpha=0.01)  # complete space


plotC <- plotC+ geom_segment(aes(x = 0, y = 0, 
                                 xend = correlations[1,1]*0.2, 
                                 yend = correlations[1,2]*0.2),size = 1,
                             color="black",
                             arrow = arrow(length = unit(.35, "cm")))  + 
  ## annotate
  annotate(geom="text",x=correlations[1,1]*0.25,
           y=correlations[1,2]*0.24,label="Aspect ratio",
           color="black") +
  
  geom_segment(aes(x = 0, y = 0, 
                   xend = correlations[2,1]*0.2, 
                   yend = correlations[2,2]*0.2),size = 1,
               color="black",
               arrow = arrow(length = unit(.35, "cm"))) + 
  annotate(geom="text",x=correlations[2,1]*0.25,
           y=correlations[2,2]*0.25,label="Trophic level",
           color="black") +
  
  geom_segment(aes(x = 0, y = 0, 
                   xend = correlations[3,1]*0.2, 
                   yend = correlations[3,2]*0.2),size = 1,
               color="black",
               arrow = arrow(length = unit(.35, "cm"))) + 
  annotate(geom="text",x=correlations[3,1]*0.20,
           y=correlations[3,2]*0.25,label="Group size",
           color="black") +
  
  geom_segment(aes(x = 0, y = 0, 
                   xend = correlations[4,1]*0.2, 
                   yend = correlations[4,2]*0.2),size = 1,
               color="black",
               arrow = arrow(length = unit(.35, "cm"))) + 
  annotate(geom="text",x=correlations[4,1]*0.25,
           y=correlations[4,2]*0.29,label="TºC max",
           color="black") + 
  
  geom_segment(aes(x = 0, y = 0, 
                   xend = correlations[5,1]*0.2, 
                   yend = correlations[5,2]*0.2),size = 1,
               color="black",
               arrow = arrow(length = unit(.35, "cm"))) + 
  annotate(geom="text",x=correlations[5,1]*0.25,
           y=correlations[5,2]*0.23,label="Depth max",
           color="black",) + 
  
  geom_segment(aes(x = 0, y = 0, 
                   xend = correlations[6,1]*0.2, 
                   yend = correlations[6,2]*0.2),size = 1,
               color="black",
               arrow = arrow(length = unit(.35, "cm"))) + 
  annotate(geom="text",x=correlations[6,1]*0.18,
           y=correlations[6,2]*0.25,label="Body size",
           color="black") + 
  ggtitle ("Random loss") +
  theme (plot.title = element_text(size=12),
         axis.title = element_blank(),
         axis.text = element_blank())

# save to pdf
png (here ("output_comm_wide_R1", "figures","figS1.3.png"),
     width = 40, height = 12, units = "cm",res=300)

# organize the trait spaces
grid.arrange (plotA,
              plotB[[1]],
              plotB[[2]],
              plotB[[3]],
              plotB[[4]],
              plotB[[5]],
              plotB[[6]],
              plotB[[7]],
              plotB[[8]],
              plotC,ncol = 8,nrow=2,
              
              layout_matrix = rbind (c(1,1,2,3,4,5,10,10),
                                     c(1,1,6,7,8,9,10,10))
)

dev.off()

# end