#################### composicao com mapa dos pontos, cobertura de coral, e riqueza peixes
# load packages
source ("R/packages.R")
source ("R/functions.R")

# load basic data
## fish data
load (here("output","Data_fish_detection.RData"))

## coral data
load (here("output","coral_occupancy_data.RData"))

## coral detection - with coordinates
load (here("output","Data_coral_detection.RData"))

##################################
############## figura 1   ########
##################################
# mapa mundi
world <- ne_countries(scale = "medium", returnclass = "sf")

# cortar o mapa para ver a america do Sul e parte da central
wm <- ggplot() + 
  geom_sf (data=world, size = 0.1, 
           fill= "gray90",colour="gray90") +
  coord_sf (xlim = c(-50, -28),  ylim = c(-27, -1), expand = FALSE) +
  theme_bw() + #xlab ("Longitude")  + ylab ("Latitude") +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "lightcyan",#darkslategray1
                                        colour = "lightcyan"),
        axis.text.x = element_text(size=6),
        axis.ticks.x=element_line(size=1),
        axis.text.y = element_text(size=6),
        axis.ticks.y=element_line(size=1),
        axis.title.x = element_text(size=8),
        axis.title.y = element_text(size=8),
        title = element_blank()) 
  
## fazer uma coluna dizendo que nao tem coral em 3 sitios
list_coral_data$cover_original <- data.frame(list_coral_data$cover_original ,
  None=ifelse (rowSums (list_coral_data$cover_original) ==0,
                                 1,0))

list_coral_data$cover_original <- data.frame (list_coral_data$cover_original,
                                         LonJitter = jitter (coordenadas$Lon,factor=400),
                                          LatJitter=jitter (coordenadas$Lat,factor=600))
## advise to jitter : https://stackoverflow.com/questions/52806580/pie-charts-in-geom-scatterpie-overlapping
## pie: http://www.spectdata.com/index.php/2018/10/25/how-to-use-ggplot-to-plot-pie-charts-on-a-map/

wm_pie <- wm + geom_scatterpie(aes(x=LonJitter, y=LatJitter),
                     data = list_coral_data$cover_original,
                     cols = c("Millepora.alcicornis",
                              "Montastraea.cavernosa",
                              "Mussismilia.braziliensis",
                              "Mussismilia.hispida",
                              "Siderastrea.spp",
                              "None",
                              "Agaricia.spp"),
                     pie_scale = 1.75,
                     sorted_by_radius = F,
                     legend_name = "Species") + 
  
  theme (legend.title = element_text(size=8),
         legend.text = element_text(size=7),
         legend.position = c(.98, .5),
         legend.justification = c("right", "top"),
         legend.box.just = "right",
         legend.margin = margin(6,6,6,6),
         legend.background = element_blank(),
         title=element_text(size=8)) 

wm_pie <- wm_pie + ggtitle("Original cover")+
  xlab("Longitude") + ylab("Latitude")


wm_pie_col <- wm_pie + scale_fill_manual(values= c("Millepora.alcicornis" = "#ca0020",
                                      "Montastraea.cavernosa" = "#dfc27d",
                                      "Mussismilia.braziliensis" = "#80cdc1",
                                      "Mussismilia.hispida" = "#0571b0",
                                      "Siderastrea.spp" = "#FCBDBD",
                                      "None" = "#FFFFFF",
                                      "Agaricia.spp" = "#E5806A"))
wm_pie_col

ggsave(here ("output",filename = "mapa_coral_cover.png"), width = 6,height=6,dpi =300)

## help with colors : https://www.rapidtables.com/web/color/RGB_Color.html
## cobertura media e range por especie

round(apply(list_coral_data$cover_original[c(1:6)],2,mean),2)
round(apply(list_coral_data$cover_original[c(1:6)],2,sd),2)
round(apply(list_coral_data$cover_original[c(1:6)],2,range),2)

##########
## MAPA DA PROBABILIDADE DE OCUPACAO DE ESTIMADA PELO MODELO ESPACIAL
##########
# jittered coordinates (from the previuos jitter)
list_coral_data$occupancy_estimate_nb12 <- data.frame (list_coral_data$occupancy_estimate_nb12 ,
                                              LonJitter = list_coral_data$cover_original$LonJitter,
                                              LatJitter= list_coral_data$cover_original$LatJitter)

# Mapa

wm_pie_occ <- wm + geom_scatterpie(aes(x=LonJitter, y=LatJitter),
                               data = list_coral_data$occupancy_estimate_nb12,
                               cols = c("Millepora.alcicornis",
                                        "Montastraea.cavernosa",
                                        "Mussismilia.braziliensis",
                                        "Mussismilia.hispida",
                                        "Siderastrea.spp",
                                        "Agaricia.spp"),
                               pie_scale = 1.75,
                               sorted_by_radius = F,
                               legend_name = "Species") + 
  
  theme (legend.title = element_text(size=8),
         legend.text = element_text(size=7),
         legend.position = c(.98, .5),
         legend.justification = c("right", "top"),
         legend.box.just = "right",
         legend.margin = margin(6,6,6,6),
         legend.background = element_blank(),
         title=element_text(size=8)) 

wm_pie_occ <- wm_pie_occ + ggtitle("Site-occupancy probability")+
  xlab("Longitude") + ylab("Latitude")

wm_pie_col_occ <- wm_pie_occ + scale_fill_manual(values= c("Millepora.alcicornis" = "#ca0020",
                                                   "Montastraea.cavernosa" = "#dfc27d",
                                                   "Mussismilia.braziliensis" = "#80cdc1",
                                                   "Mussismilia.hispida" = "#0571b0",
                                                   "Siderastrea.spp" = "#FCBDBD",
                                                   "Agaricia.spp" = "#E5806A"))
wm_pie_col_occ

ggsave(here ("output",filename = "mapa_coral_occ.png"), width = 6,height=6,dpi =300)

### load extracted estimates
load(here("output","coefficients_ORIGINAL.RData"))
load(here("output","coefficients_DISCOUNTING25.RData"))
load(here("output","coefficients_DISCOUNTING50.RData"))

## effect for bipartite plot
## coeficientes positivos
efeito_para_bipartite_original_positivo <- efeito_para_bipartite_original_reg1* significance_original_reg1 
efeito_para_bipartite_original_positivo <- efeito_para_bipartite_original_positivo [which (rowSums (efeito_para_bipartite_original_positivo)!=0),]

## coeficientes negativos
efeito_para_bipartite_original_negative <- efeito_para_bipartite_original_reg1*-1
efeito_para_bipartite_original_negative <- efeito_para_bipartite_original_negative [which (rowSums (efeito_para_bipartite_original_negative)!=0),]

## grafico tipo plotweb

sp_like_corals_original <- ifelse (efeito_para_bipartite_original_positivo <0,0,efeito_para_bipartite_original_positivo)
range(sp_like_corals_original[which(sp_like_corals_original >0 )])

pdf(here ("output","plotWebSp_like_nordeste.pdf"),onefile = T)

plotweb (t(sp_like_corals_original),
         method="cca",
         labsize = 1,
         ybig=1.75,
         col.interaction = "#7FFFD4",
         text.rot=90)

dev.off()


pdf(here ("output","plotWebSp_like_sudeste.pdf"),onefile = T)

plotweb (t(sp_like_corals_original[,seq(2,12,2)]),
         method="cca",
         labsize = 1,
         ybig=1.75,
         col.interaction = "#7FFFD4",
         text.rot=90)

dev.off()


sp_didntlike_corals_original <- ifelse (efeito_para_bipartite_original_negative  <0,0,efeito_para_bipartite_original_negative )

range(sp_didntlike_corals_original[which(sp_didntlike_corals_original >0 )])

pdf(here ("output","plotWebSp_didntlike_nordeste.pdf"),onefile = T)
plotweb (t(sp_didntlike_corals_original[,seq(1,11,2)]),
         method="cca",
         labsize = 0.75,
         ybig=1.75,
         col.interaction = "#ffa500",
         text.rot=90)

dev.off()


pdf(here ("output","plotWebSp_didntlike_sudeste.pdf"),onefile = T)
plotweb (t(sp_didntlike_corals_original[,seq(2,12,2)]),
         method="cca",
         labsize = 0.75,
         ybig=1.75,
         col.interaction = "#ffa500",
         text.rot=90)

dev.off()











## quantos de cada sp de coral tiveram efeito?
# original
lapply (tab_summarized_original, function (i)
  
  cbind(length(i [which (i$effectReg1  == "Yes"),"sp"]),
        length(i [which (i$effectReg2  == "Yes"),"sp"]))
  
)
# occupancy
lapply (tab_summarized_occupancy, function (i)
  
  cbind(length(i [which (i$effectReg1  == "Yes"),"sp"]),
        length(i [which (i$effectReg2  == "Yes"),"sp"]))
  
)

## efeito pra que especies?
# original
lapply (tab_summarized_original, function (i)
  
  list("Reg1"=i [which (i$effectReg1  == "Yes"),"sp"],
        "Reg2"=i [which (i$effectReg2  == "Yes"),"sp"])
)
# occupancy
lapply (tab_summarized_occupancy, function (i)
  
  list("Reg1"=i [which (i$effectReg1  == "Yes"),"sp"],
       "Reg2"=i [which (i$effectReg2  == "Yes"),"sp"])
)

## quantas spp unicas foram influenciadas?
## original cover, disconsidering the species of coral - to have this, remove the #

sp_com_efeito_original <- #unique (unlist (
  lapply (tab_summarized_original, function (i)
  
  list("Reg1"=i [which (i$effectReg1  == "Yes"),"sp"],
    "Reg2"=i [which (i$effectReg2  == "Yes"),"sp"])
  
)#))
# occupancy
sp_com_efeito_occupancy <- #unique (unlist (
  lapply (tab_summarized_occupancy, function (i)
    
    list("Reg1"=i [which (i$effectReg1  == "Yes"),"sp"],
         "Reg2"=i [which (i$effectReg2  == "Yes"),"sp"])
    
  )#))

#sp_com_efeito_original <- data.frame (especie,
#            effect=ifelse (especie %in% sp_com_efeito_original, "Yes", "No"))
## para quantas especies unicas obtivemos algum efeito (independente do coral??)
#sum(sp_com_efeito$effect == "Yes")

lista_modelos_selecionados_original <- do.call(cbind,mod_sel_original)
colnames(lista_modelos_selecionados_original) <- sp_coral
lista_modelos_selecionados_occupancy <- do.call(cbind,mod_sel_occupancy)
colnames(lista_modelos_selecionados_occupancy) <- sp_coral

## qual o modelo mais comumente selecionado?
table(lista_modelos_selecionados_original)
table(lista_modelos_selecionados_occupancy)

# colar os modelos selcionados, para tabela do artigo
sp_com_efeito <- cbind(sp_com_efeito,
                       lista_modelos_selecionados)


write.csv (sp_com_efeito,here ("output", "tabela_efeito_sp.csv"))

## relacao entre distribuicao e efeito - seriam especies com distribuicao em menos sitios as mais
## influenciadas pelos corais?

## anova para testar wide-ranging species em geral nao sao afetadas pela cobertura de coral
lapply (tab_summarized_original, function (i) with (i,
              summary (aov(nocc ~effectReg1))))


agaricia_spp <- ggplot (tab_summarized_original [[1]], aes (effectReg1,nocc)) +
  geom_boxplot(fill="gray") + 
  theme_classic() +
  ggtitle ("Agaricia spp.") +
  xlab("Is there an effect?") +
  ylab ("Number of occupied sites") +
  theme (axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         #axis.ticks.x = element_blank(),
         axis.text.y = element_text(size= 7),
         axis.title.y = element_text(size=9),
         plot.title = element_text (size=10,hjust=0.5))+
  annotate("text",2.1,32,label= 
             expression("F"[1]*""[',']*""[72]*" = 3.47"),size=3)


mill_alc <- ggplot (tab_summarized_original [[2]], aes (effectReg1,nocc)) +
  geom_boxplot(fill="gray") + 
  theme_classic() +
  ggtitle ("Millepora alcicornis") +
  xlab("Is there an effect?") +
  ylab ("Number of occupied sites") +
  theme (axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         #axis.ticks.x = element_blank(),
         axis.text.y = element_blank(),
         axis.title.y = element_blank(),
         #axis.ticks.y = element_blank(),
         plot.title=element_text (size=10,hjust=0.5)) + 
  annotate("text",2.1,32,label= 
             expression("F"[1]*""[',']*""[72]*" = 10.12**"),size=3)

mon_cav <- ggplot (tab_summarized_original [[3]], aes (effectReg1,nocc)) +
  geom_boxplot(fill="gray") + 
  theme_classic() +
  ggtitle ("Montastraea cavernosa") +
  xlab("Is there an effect?") +
  ylab ("Number of occupied sites") +
  theme (axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         #axis.ticks.x = element_blank(),
         axis.text.y = element_blank(),
         axis.title.y = element_blank(),
         #axis.ticks.y = element_blank(),
         plot.title=element_text (size=10,hjust=0.5)) +
annotate("text",2.1,32,label= 
    expression("F"[1]*""[',']*""[72]*" = 3.46"),size=3)

mus_bra <- ggplot (tab_summarized_original [[4]], aes (effectReg1,nocc)) +
  geom_boxplot(fill="gray") + 
  theme_classic() +
  ggtitle ("Mussismilia braziliensis") +
  xlab("") +
  ylab ("Number of occupied sites") +
  theme (axis.text.x = element_text(size= 7),
         axis.title.x = element_text(size=9),
         axis.text.y = element_text(size= 7),
         axis.title.y = element_text(size=9),
         #axis.ticks.y = element_blank(),
         plot.title=element_text (size=10,hjust=0.5))+
  annotate("text",2.1,32,label= 
             expression("F"[1]*""[',']*""[72]*" = 0.80"),size=3)

mus_his <- ggplot (tab_summarized_original [[5]], aes (effectReg1,nocc)) +
  geom_boxplot(fill="gray") + 
  theme_classic() +
  ggtitle ("Mussismilia hispida") +
  xlab("Is there an effect?") +
  ylab ("Number of occupied sites") +
  theme (axis.text.x = element_text(size= 7),
         axis.title.x = element_text(size=9),
         axis.text.y = element_blank(),
         axis.title.y = element_blank(),
         #axis.ticks.y = element_blank(),
         plot.title=element_text (size=10,hjust=0.5))+ 
  annotate("text",2.1,32,label= 
             expression("F"[1]*""[',']*""[72]*" = 7.04**"),size=3)

sid_spp <- ggplot (tab_summarized_original [[6]], aes (effectReg1,nocc)) +
  geom_boxplot(fill="gray") + 
  theme_classic() +
  ggtitle ("Siderastrea spp.") +
  xlab("") +
  ylab ("") +
  theme (axis.text.x = element_text(size= 7),
         axis.title.x = element_text(size= 9),
         axis.text.y = element_blank(),
         axis.title.y = element_blank(),
         #axis.ticks.y = element_blank(),
         plot.title=element_text (size=10,hjust=0.5))+
  annotate("text",2.1,32,label= 
             expression("F"[1]*""[',']*""[12]*" = 12.41***"),size=3)


#### painel com as 5 spp;
png(here ("output", "nocc_effect_nordeste.png"), width=15, 
                height=12, units="cm", res=300)

grid.arrange(
             agaricia_spp,
             mill_alc, 
             mon_cav,
             mus_bra,
             mus_his,
             sid_spp,ncol=3,nrow=2,
                layout_matrix = rbind (c(1,2,3),
                                       c(4,5,6)))


dev.off()

## regiao 2
lapply (tab_summarized_original, function (i) with (i,
                        summary (aov(nocc ~effectReg2))))

agaricia_spp2 <- ggplot (tab_summarized_original [[1]], aes (effectReg2,nocc)) +
  geom_boxplot(fill="gray") + 
  theme_classic() +
  ggtitle ("Agaricia spp.") +
  xlab("Is there an effect?") +
  ylab ("Number of occupied sites") +
  theme (axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         #axis.ticks.x = element_blank(),
         axis.text.y = element_text(size= 7),
         axis.title.y = element_text(size=9),
         plot.title = element_text (size=10,hjust=0.5))+
  annotate("text",2.1,32,label= 
             expression("F"[1]*""[',']*""[72]*" = 0.30"),size=3)

mill_alc2 <- ggplot (tab_summarized_original [[2]], aes (effectReg2,nocc)) +
  geom_boxplot(fill="gray") + 
  theme_classic() +
  ggtitle ("Millepora alcicornis") +
  xlab("Is there an effect?") +
  ylab ("Number of occupied sites") +
  theme (axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         #axis.ticks.x = element_blank(),
         axis.text.y = element_blank(),
         axis.title.y = element_blank(),
         #axis.ticks.y = element_blank(),
         plot.title=element_text (size=10,hjust=0.5)) + 
  annotate("text",2.1,32,label= 
             expression("F"[1]*""[',']*""[72]*" = 0.02"),size=3)

mon_cav2 <- ggplot (tab_summarized_original [[3]], aes (effectReg2,nocc)) +
  geom_boxplot(fill="gray") + 
  theme_classic() +
  ggtitle ("Montastraea cavernosa") +
  xlab("Is there an effect?") +
  ylab ("Number of occupied sites") +
  theme (axis.text.x = element_blank(),
         axis.title.x = element_blank(),
         #axis.ticks.x = element_blank(),
         axis.text.y = element_blank(),
         axis.title.y = element_blank(),
         #axis.ticks.y = element_blank(),
         plot.title=element_text (size=10,hjust=0.5)) +
  annotate("text",2.1,32,label= 
             expression("F"[1]*""[',']*""[72]*" = 1.36"),size=3)

mus_bra2 <- ggplot (tab_summarized_original [[4]], aes (effectReg2,nocc)) +
  geom_boxplot(fill="gray") + 
  theme_classic() +
  ggtitle ("Mussismilia braziliensis") +
  xlab("") +
  ylab ("Number of occupied sites") +
  theme (axis.text.x = element_text(size= 7),
         axis.title.x = element_text(size=9),
         axis.text.y = element_text(size= 7),
         axis.title.y = element_text(size=9),
         #axis.ticks.y = element_blank(),
         plot.title=element_text (size=10,hjust=0.5))+
  annotate("text",2.1,32,label= 
             expression("F"[1]*""[',']*""[72]*" = 3.01"),size=3)

mus_his2 <- ggplot (tab_summarized_original [[5]], aes (effectReg2,nocc)) +
  geom_boxplot(fill="gray") + 
  theme_classic() +
  ggtitle ("Mussismilia hispida") +
  xlab("Is there an effect?") +
  ylab ("Number of occupied sites") +
  theme (axis.text.x = element_text(size= 7),
         axis.title.x = element_text(size=9),
         axis.text.y = element_blank(),
         axis.title.y = element_blank(),
         #axis.ticks.y = element_blank(),
         plot.title=element_text (size=10,hjust=0.5))+ 
  annotate("text",2.1,32,label= 
             expression("F"[1]*""[',']*""[72]*" = 0.09"),size=3)

sid_spp2 <- ggplot (tab_summarized_original [[6]], aes (effectReg2,nocc)) +
  geom_boxplot(fill="gray") + 
  theme_classic() +
  ggtitle ("Siderastrea spp.") +
  xlab("") +
  ylab ("") +
  theme (axis.text.x = element_text(size= 7),
         axis.title.x = element_text(size= 9),
         axis.text.y = element_blank(),
         axis.title.y = element_blank(),
         #axis.ticks.y = element_blank(),
         plot.title=element_text (size=10,hjust=0.5))+
  annotate("text",2.1,32,label= 
             expression("F"[1]*""[',']*""[72]*" = 0.45"),size=3)


#### painel com as 5 spp;
png(here ("output", "nocc_effect_sudeste.png"), width=15, 
    height=12, units="cm", res=300)

grid.arrange(
  agaricia_spp2,
  mill_alc2, 
  mon_cav2,
  mus_bra2,
  mus_his2,
  sid_spp2,ncol=3,nrow=2,
  layout_matrix = rbind (c(1,2,3),
                         c(4,5,6)))


dev.off()


## seguir as analises com o modelo sem o fator aleatorio (considerando a especie de coral)

## quais foram os efeitos diferentes de zero? (intervalos de beta1 nao sobreporam 0)
samples_efeito_original <-  lapply (seq(1,length(tab_efeitos_sp_original)), function (i)
  
  selected.models_original[[i]] [which (tab_efeitos_sp_original[[i]]  == "Yes")] ## i.e., don't overlap 0
  
)

## valores de beta 1 - somente para as spp com efeito
tab_coef_original <- lapply (samples_efeito_original, function (i)
  do.call (rbind,lapply (i ,function (k)
    k$summary [grep("beta1",rownames (k$summary))]
)))

## number of species with positive coefficient
lapply(tab_coef_original,nrow)

lapply (tab_coef_original, function (i)
  sum(i > 0))

## number of species with negativa coefficient

## valores de beta 1 - para todas as especies
efeito_para_bipartite_original <- lapply (selected.models_original, function (i)
  
  do.call (rbind,lapply (i ,function (k)
  
      k$summary [grep("beta1",rownames (k$summary)),"mean"]
  )))

## colar se tem ou nao efeito
efeito_para_bipartite_original <- do.call(cbind, 
          
                                 efeito_para_bipartite_original
)

dimnames(efeito_para_bipartite_original) <- list(especie,
                                                 rep(names(selected.models_original),2)[order(rep(names(selected.models_original),2))])

## overlap 0 para bipartite
overlap0_para_bipartite_original <- lapply (selected.models_original, function (i)
  do.call (rbind,lapply (i ,function (k)
    k$summary [grep("beta1",rownames (k$summary)),"overlap0"]
  )))

## colar se tem ou nao efeito
overlap0_para_bipartite_original <- do.call(cbind, 
                                 
                                   overlap0_para_bipartite_original
                                    
                                    )

dimnames(overlap0_para_bipartite_original) <- list(especie,
                                        rep(names(selected.models_original),2)[order(rep(names(selected.models_original),2))])


## multiplicar a matriz de efeitos pelo overlpa
## se teve efeito, mantem o valor, se nao teve, vai pra -1
overlap0_para_bipartite_original <- ifelse (overlap0_para_bipartite_original ==1,0,1)
## coeficientes positivos
efeito_para_bipartite_original_positivo <- efeito_para_bipartite_original * overlap0_para_bipartite_original
efeito_para_bipartite_original_positivo <- efeito_para_bipartite_original_positivo [which (rowSums (efeito_para_bipartite_original_positivo)!=0),]

## coeficientes negativos
efeito_para_bipartite_original_negative <- efeito_para_bipartite_original_positivo*-1
efeito_para_bipartite_original_negative <- efeito_para_bipartite_original_negative [which (rowSums (efeito_para_bipartite_original_negative)!=0),]

## grafico tipo plotweb

sp_like_corals_original <- ifelse (efeito_para_bipartite_original_positivo <0,0,efeito_para_bipartite_original_positivo)
range(sp_like_corals_original[which(sp_like_corals_original >0 )])

pdf(here ("output","plotWebSp_like_nordeste.pdf"),onefile = T)

plotweb (t(sp_like_corals_original[,seq(1,11,2)]),
         method="cca",
         labsize = 1,
         ybig=1.75,
         col.interaction = "#7FFFD4",
         text.rot=90)

dev.off()


pdf(here ("output","plotWebSp_like_sudeste.pdf"),onefile = T)

plotweb (t(sp_like_corals_original[,seq(2,12,2)]),
         method="cca",
         labsize = 1,
         ybig=1.75,
         col.interaction = "#7FFFD4",
         text.rot=90)

dev.off()


sp_didntlike_corals_original <- ifelse (efeito_para_bipartite_original_negative  <0,0,efeito_para_bipartite_original_negative )

range(sp_didntlike_corals_original[which(sp_didntlike_corals_original >0 )])

pdf(here ("output","plotWebSp_didntlike_nordeste.pdf"),onefile = T)
plotweb (t(sp_didntlike_corals_original[,seq(1,11,2)]),
         method="cca",
         labsize = 0.75,
         ybig=1.75,
         col.interaction = "#ffa500",
         text.rot=90)

dev.off()


pdf(here ("output","plotWebSp_didntlike_sudeste.pdf"),onefile = T)
plotweb (t(sp_didntlike_corals_original[,seq(2,12,2)]),
         method="cca",
         labsize = 0.75,
         ybig=1.75,
         col.interaction = "#ffa500",
         text.rot=90)

dev.off()

#example one (tritrophic)
#plotweb(sp_like_corals,y.width.low=0.1, y.width.high=0.05,method="cca", 
#        y.lim=c(0,3), arrow="down", adj.high=c(0.5,1.5), col.high="orange",
#        high.lablength=3,high.lab.dis=0)
#plotweb(t(sp_didntlike_corals), y.width.low=0.05, y.width.high=0.1, method="cca",
#        add=TRUE,low.y=1.5,high.y=2.5, col.low="green", text.low.col="red", 
#        low.lab.dis=0, arrow="up", adj.low=c(0.5,1.1),low.plot=FALSE)

### opcao com IGRAPH
require(igraph)
### weigh matrix = == overlap 0
g <- graph.incidence(efeito_para_bipartite_positivo, weighted = T)
## help with this: https://r-inspirations.blogspot.com/2016/08/create-bipartite-graph-with-igraph.html

### ajustar as cores

gN <- graph.incidence(efeito_para_bipartite_negative, weighted = T)
V(gN)$type
V(gN)$name
## help with this: https://r-inspirations.blogspot.com/2016/08/create-bipartite-graph-with-igraph.html
## https://www.jessesadler.com/post/network-analysis-with-r/

## positive coefficients
V(g)$color <- V(g)$type
V(g)$color <- gsub("FALSE","#80cdc1",V(g)$color)
V(g)$color <- gsub("TRUE","#FCBDBD",V(g)$color)
V(g)$label.color <- "black" ##ifelse(V(g)$type, "black", "white")
## V(g)$label.font <-  2
V(g)$label.cex <- 0.5 ##ifelse(V(g)$type, 0.8, 1.2)
## V(g)$label.dist <-0
V(g)$frame.color <-  "black"
V(g)$size <- 12
V(g)$vertex.size <- 100

## negative coefficients
V(gN)$color <- V(gN)$type
V(gN)$color <- gsub("FALSE","#80cdc1",V(gN)$color)
V(gN)$color <- gsub("TRUE","#FCBDBD",V(gN)$color)
V(gN)$label.color <- "black" ##ifelse(V(g)$type, "black", "white")
## V(g)$label.font <-  2
V(gN)$label.cex <- 0.5 ##ifelse(V(g)$type, 0.8, 1.2)
## V(g)$label.dist <-0
V(gN)$frame.color <-  "black"
V(gN)$size <- 12
V(gN)$vertex.size <- 1


pdf(file="bipartite_fish_coral.pdf",onefile = T)

par(mfrow=c(1,2),mar=c(0,1,1,1))
plot(gN, edge.width=E(gN)$weight,
     edge.color="gray70",
     layout = layout_with_graphopt,
     main="Negative effect")

legend("bottomleft",
       legend=c(
         "Coefficient range",
         round (range(efeito_para_bipartite_negative[which(efeito_para_bipartite_negative>0)])[1],2)*-1,
         round(range(efeito_para_bipartite_negative[which(efeito_para_bipartite_negative>0)])[2],2)*-1
       ),
       bty="n",
       col="gray70",
       lty=1,
       lwd=c(NA,1,10),
       cex=0.8)
  
plot(g, edge.width=E(g)$weight,
     edge.color= "gray30",
     layout = layout_with_graphopt,
     main="Positive effect")

legend("bottomleft",
       legend=c(
         "Coefficient range",
         round (range(efeito_para_bipartite_positivo[which(efeito_para_bipartite_positivo>0)])[1],2),
         round(range(efeito_para_bipartite_positivo[which(efeito_para_bipartite_positivo>0)])[2],2)
       ),
       bty="n",
       col= "gray30",
       lty=1,
       lwd=c(NA,1,10),
       cex=0.8)


dev.off()


###############################################
####### FIGURE 3 
###### RELATIONSHIP BETWEEN FISH AD CORAL


## #selected species with most positive and most negative coefficients

par(mfrow=c(1,1),mar=rep(4,4))

## qual o valor do cOeficiente?

## valores de beta 1 - somente para as spp com efeito
lapply (samples_efeito, function (i)
  do.call (rbind,lapply (i ,function (k)
    k$summary [grep("beta1",rownames (k$summary))]
  )))

## efeito pra que especies?

lapply (efeitos, function (i)
  especie [which(i == "0")]
)

## ver os valores de bpv aqui para cada sp de coral
tab_summarized[[6]]
## CHECAR PARA SELECIONAR AS ESPECIES

##################################################
## para millepora alcicornis [[1]]
##################################################
par(mar=c(6,5,5,5))
png(here ("output","sp1sp4.png"), width=15, height=15, units="cm", res=300)
plot(seq (-1,4,0.1),
     plogis (mean (samples_efeito [[1]][[4]]$mean$intercept.depth) + 
               samples_efeito [[1]][[4]]$mean$beta1 * seq (-1,4,0.1)),
     type="l",
     xlab = "Coral cover",
     ylab= "Scarus zelindae occupancy probability",
     ylim=c(0,1),
     main = "Millepora alcicornis",
     cex.lab=1.5,
     cex.main=1.5)

lapply (seq (1,1200), function (i)
  
  lines(seq (-1,4,0.1),
      plogis (mean (samples_efeito [[1]][[4]]$sims.list$intercept.depth [i,]) + 
                  samples_efeito [[1]][[4]]$sims.list$beta1 [i] * seq (-1,4,0.1)),
        col=rgb(0.400,0.400,0.400,alpha=0.1))
)


lines(seq (-1,4,0.1),
      plogis (mean (samples_efeito [[1]][[4]]$mean$intercept.depth) + 
                samples_efeito [[1]][[4]]$mean$beta1 * seq (-1,4,0.1)),
      lwd= 3)

text (3.5,0.05, "bpv=0.13")

dev.off()

############################################
#### PARA MONTRASTEA CAVERNOSA
############################################

png(here ("output","sp2sp7.png"), width=15, height=15, units="cm", res=300)

## 4 MAIS NEGATIVA
plot(seq (-1,4,0.1),
     plogis (mean(samples_efeito [[2]][[7]]$mean$intercept.depth) + 
               samples_efeito [[2]][[7]]$mean$beta1 * seq (-1,4,0.1)),
     type="l",
     xlab = "Coral cover",
     ylab= "Cryptotomus roseus occupancy probability",
     ylim=c(0,1),
     main = "Montastraea cavernosa",
     cex.lab=1.5,
     cex.main=1.5)

lapply (seq (1,1200), function (i)
  
  lines(seq (-1,4,0.1),
        plogis (mean (samples_efeito [[2]][[7]]$sims.list$intercept.depth [i,]) + 
                  samples_efeito [[2]][[7]]$sims.list$beta1 [i] * seq (-1,4,0.1)),
        col=rgb(0.400,0.400,0.400,alpha=0.1))
)


lines(seq (-1,4,0.1),
      plogis (mean(samples_efeito [[2]][[7]]$mean$intercept.depth) + 
                samples_efeito [[2]][[7]]$mean$beta1 * seq (-1,4,0.1)),
      lwd= 3)

text (3.5,0.90, "bpv=0.49")

dev.off()

############################################
#### PARA Mussismilia.harttii 5 mais negativa(diplodus.argenteus.argenteus), 12 (a unica positiva  scarus.zelindae)
############################################
png(here ("output","sp3sp5.png"), width=15, height=15, units="cm", res=300)

plot(seq (-1,4,0.1),
     plogis (mean (samples_efeito [[3]][[5]]$mean$intercept.depth) + 
               samples_efeito [[3]][[5]]$mean$beta1 * seq (-1,4,0.1)),
     type="l",
     xlab = "Coral cover",
     ylab= "Diplodus argenteus occupancy probability",
     ylim=c(0,1),
     main = "Mussismilia harttii",
     cex.lab=1.5,
     cex.main=1.5)

lapply (seq (1,1200), function (i)
  
  lines(seq (-1,4,0.1),
        plogis (mean(samples_efeito [[3]][[5]]$sims.list$intercept.depth  [i,]) + 
                  samples_efeito [[3]][[5]]$sims.list$beta1 [i] * seq (-1,4,0.1)),
        col=rgb(0.400,0.400,0.400,alpha=0.1))
)


lines(seq (-1,4,0.1),
      plogis (mean(samples_efeito [[3]][[5]]$mean$intercept.depth) + 
                samples_efeito [[3]][[5]]$mean$beta1 * seq (-1,4,0.1)),
      lwd= 3)

text (3.5,0.90, "bpv=0.60")

dev.off()

############################################
#### PARA MUSSISMILIA HISPIDA
############################################
png(here ("output","sp4sp3.png"), width=15, height=15, units="cm", res=300)

##  MAIS POSITIVA
plot(seq (-1,4,0.1),
     plogis (samples_efeito [[4]][[3]]$mean$intercept.psi + 
               samples_efeito [[4]][[3]]$mean$beta1 * seq (-1,4,0.1)),
     type="l",
     xlab = "Coral cover",
     ylab= "Canthigaster figueiredoi occupancy probability",
     ylim=c(0,1),
     main = "Mussismilia hispida",
     cex.lab=1.5,
     cex.main=1.5)

lapply (seq (1,1200), function (i)
  
  lines(seq (-1,4,0.1),
        plogis (samples_efeito [[4]][[3]]$sims.list$intercept.psi [i] + 
                  samples_efeito [[4]][[3]]$sims.list$beta1 [i] * seq (-1,4,0.1)),
        col=rgb(0.400,0.400,0.400,alpha=0.1))
)


lines(seq (-1,4,0.1),
      plogis (samples_efeito [[4]][[3]]$mean$intercept.psi + 
                samples_efeito [[4]][[3]]$mean$beta1 * seq (-1,4,0.1)),
      lwd= 3)

text (3.5,0.05, "bpv=0.44",
      cex.lab=1.5,
      cex.main=1.5)

dev.off()

###################################################
########## Siderastrea sp.
###################################################
## "acanthurus.chirurgus" [[1]] mais positiva, mais negativa "stegastes.pictus" [[3]]
png(here ("output","sp5sp2.png"), width=15, height=15, units="cm", res=300)

plot(seq (-1,4,0.1),
     plogis (samples_efeito [[5]][[2]]$mean$intercept.psi + 
               samples_efeito [[5]][[2]]$mean$beta1 * seq (-1,4,0.1)),
     type="l",
     xlab = "Coral cover",
     ylab= "Acanthurus coeruleus occupancy probability",
     ylim=c(0,1),
     main = "Siderastrea spp.",
     cex.lab=1.5,
     cex.main=1.5
     )

lapply (seq (1,1200), function (i)
  
  lines(seq (-1,4,0.1),
        plogis (samples_efeito [[5]][[2]]$sims.list$intercept.psi [i] + 
                  samples_efeito [[5]][[2]]$sims.list$beta1 [i] * seq (-1,4,0.1)),
        col=rgb(0.400,0.400,0.400,alpha=0.1))
)


lines(seq (-1,4,0.1),
      plogis (samples_efeito [[5]][[2]]$mean$intercept.psi + 
                samples_efeito [[5]][[2]]$mean$beta1 * seq (-1,4,0.1)),
      lwd= 3)

text (3.5,0.05, "bpv=0.05")

dev.off()

# Agaricia spp

png(here ("output","sp6sp3.png"), width=15, height=15, units="cm", res=300)

plot(seq (-1,4,0.1),
     plogis (mean(samples_efeito [[6]][[6]]$mean$intercept.depth) + 
               samples_efeito [[6]][[3]]$mean$beta1 * seq (-1,4,0.1)),
     type="l",
     xlab = "Coral cover",
     ylab= "Bodianus rufus occupancy probability",
     ylim=c(0,1),
     main = "Agaricia spp.",
     cex.lab=1.5,
     cex.main=1.5
)

lapply (seq (1,1200), function (i)
  
  lines(seq (-1,4,0.1),
        plogis (mean (samples_efeito [[6]][[3]]$sims.list$intercept.depth [i] )+ 
                  samples_efeito [[6]][[3]]$sims.list$beta1 [i] * seq (-1,4,0.1)),
        col=rgb(0.400,0.400,0.400,alpha=0.1))
)


lines(seq (-1,4,0.1),
      plogis (mean(samples_efeito [[6]][[3]]$mean$intercept.depth) + 
                samples_efeito [[6]][[3]]$mean$beta1 * seq (-1,4,0.1)),
      lwd= 3)

text (3.5,0.9, "bpv=0.41")

dev.off()


#############################################
####### fazer um painel com estes plots
############################################33


pdf(here ("output", "coralxfishoccupancy.pdf"),
    width=7,height = 5,onefile = T)

par(mfrow=c(2,3),mar=c(4,5,1.5,0.5))

# agaricia
plot(seq (-1,4,0.1),
     plogis (mean(samples_efeito [[6]][[6]]$mean$intercept.depth) + 
               samples_efeito [[6]][[3]]$mean$beta1 * seq (-1,4,0.1)),
     type="l",
     xlab = NA,
     xaxt = "n",
     ylab= expression (paste ("Bodianus rufus (",psi['i'], ")",sep="")),
     ylim=c(0,1),
     main = "Agaricia spp.",
     sub="",
     cex.lab=1.2,
     cex.main=1.3
)

lapply (seq (1,1200), function (i)
  
  lines(seq (-1,4,0.1),
        plogis (mean (samples_efeito [[6]][[3]]$sims.list$intercept.depth [i] )+ 
                  samples_efeito [[6]][[3]]$sims.list$beta1 [i] * seq (-1,4,0.1)),
        col=rgb(0.400,0.400,0.400,alpha=0.1))
)


lines(seq (-1,4,0.1),
      plogis (mean(samples_efeito [[6]][[3]]$mean$intercept.depth) + 
                samples_efeito [[6]][[3]]$mean$beta1 * seq (-1,4,0.1)),
      lwd= 3)

text (3,0.9, "bpv=0.41")

## millepora
plot(seq (-1,4,0.1),
     plogis (mean (samples_efeito [[1]][[4]]$mean$intercept.depth) + 
               samples_efeito [[1]][[4]]$mean$beta1 * seq (-1,4,0.1)),
     type="l",
     xlab = NA,
     xaxt = "n",
     ylab= expression (paste ("Scarus zelindae (",psi['i'], ")",sep="")),
     ylim=c(0,1),
     main = "Millepora alcicornis",
     cex.lab=1.2,
     cex.main=1.3)

lapply (seq (1,1200), function (i)
  
  lines(seq (-1,4,0.1),
        plogis (mean (samples_efeito [[1]][[4]]$sims.list$intercept.depth [i,]) + 
                  samples_efeito [[1]][[4]]$sims.list$beta1 [i] * seq (-1,4,0.1)),
        col=rgb(0.400,0.400,0.400,alpha=0.1))
)


lines(seq (-1,4,0.1),
      plogis (mean (samples_efeito [[1]][[4]]$mean$intercept.depth) + 
                samples_efeito [[1]][[4]]$mean$beta1 * seq (-1,4,0.1)),
      lwd= 3)

text (3,0.05, "bpv=0.13")

## montastraea cavernosa

plot(seq (-1,4,0.1),
     plogis (mean(samples_efeito [[2]][[7]]$mean$intercept.depth) + 
               samples_efeito [[2]][[7]]$mean$beta1 * seq (-1,4,0.1)),
     type="l",
     xlab = NA,
     xaxt = "n",
     ylab= expression (paste ("Cryptotomus roseus (",psi['i'], ")",sep="")),
     ylim=c(0,1),
     main = "Montastraea cavernosa",
     cex.lab=1.2,
     cex.main=1.3)

lapply (seq (1,1200), function (i)
  
  lines(seq (-1,4,0.1),
        plogis (mean (samples_efeito [[2]][[7]]$sims.list$intercept.depth [i,]) + 
                  samples_efeito [[2]][[7]]$sims.list$beta1 [i] * seq (-1,4,0.1)),
        col=rgb(0.400,0.400,0.400,alpha=0.1))
)


lines(seq (-1,4,0.1),
      plogis (mean(samples_efeito [[2]][[7]]$mean$intercept.depth) + 
                samples_efeito [[2]][[7]]$mean$beta1 * seq (-1,4,0.1)),
      lwd= 3)

text (3,0.90, "bpv=0.49")


## mussismilia harttii


plot(seq (-1,4,0.1),
     plogis (mean (samples_efeito [[3]][[5]]$mean$intercept.depth) + 
               samples_efeito [[3]][[5]]$mean$beta1 * seq (-1,4,0.1)),
     type="l",
     xlab = NA,
     ylab= expression (paste ("Diplodus argenteus (",psi['i'], ")",sep="")),
     ylim=c(0,1),
     main = "Mussismilia harttii",
     cex.lab=1.2,
     cex.main=1.3)

lapply (seq (1,1200), function (i)
  
  lines(seq (-1,4,0.1),
        plogis (mean(samples_efeito [[3]][[5]]$sims.list$intercept.depth  [i,]) + 
                  samples_efeito [[3]][[5]]$sims.list$beta1 [i] * seq (-1,4,0.1)),
        col=rgb(0.400,0.400,0.400,alpha=0.1))
)


lines(seq (-1,4,0.1),
      plogis (mean(samples_efeito [[3]][[5]]$mean$intercept.depth) + 
                samples_efeito [[3]][[5]]$mean$beta1 * seq (-1,4,0.1)),
      lwd= 3)

text (3,0.90, "bpv=0.60")


## mussismilia hispida


##  MAIS POSITIVA
plot(seq (-1,4,0.1),
     plogis (samples_efeito [[4]][[3]]$mean$intercept.psi + 
               samples_efeito [[4]][[3]]$mean$beta1 * seq (-1,4,0.1)),
     type="l",
     xlab = "Coral cover",
     ylab= expression (paste ("Canthigaster figueiredoi (",psi['i'], ")",sep="")),
     ylim=c(0,1),
     main = "Mussismilia hispida",
     cex.lab=1.2,
     cex.main=1.3)

lapply (seq (1,1200), function (i)
  
  lines(seq (-1,4,0.1),
        plogis (samples_efeito [[4]][[3]]$sims.list$intercept.psi [i] + 
                  samples_efeito [[4]][[3]]$sims.list$beta1 [i] * seq (-1,4,0.1)),
        col=rgb(0.400,0.400,0.400,alpha=0.1))
)


lines(seq (-1,4,0.1),
      plogis (samples_efeito [[4]][[3]]$mean$intercept.psi + 
                samples_efeito [[4]][[3]]$mean$beta1 * seq (-1,4,0.1)),
      lwd= 3)

text (3,0.05, "bpv=0.44")


## siderastrea spp.

plot(seq (-1,4,0.1),
     plogis (samples_efeito [[5]][[2]]$mean$intercept.psi + 
               samples_efeito [[5]][[2]]$mean$beta1 * seq (-1,4,0.1)),
     type="l",
     xlab = NA,
     ylab= expression (paste ("Acanthurus coeruleus (",psi['i'], ")",sep="")),
     ylim=c(0,1),
     main = "Siderastrea spp.",
     cex.lab=1.2,
     cex.main=1.3
)

lapply (seq (1,1200), function (i)
  
  lines(seq (-1,4,0.1),
        plogis (samples_efeito [[5]][[2]]$sims.list$intercept.psi [i] + 
                  samples_efeito [[5]][[2]]$sims.list$beta1 [i] * seq (-1,4,0.1)),
        col=rgb(0.400,0.400,0.400,alpha=0.1))
)


lines(seq (-1,4,0.1),
      plogis (samples_efeito [[5]][[2]]$mean$intercept.psi + 
                samples_efeito [[5]][[2]]$mean$beta1 * seq (-1,4,0.1)),
      lwd= 3)

text (3,0.05, "bpv=0.05")

dev.off()

## relacionar a deteccao com o tamanho da especie de peixe
## carregar traits peixes

traits_peixes <- read.csv(here("traits","Atributos_especies_Atlantico_&_Pacifico_Oriental_2020_04_28.csv"),
                          h=T,sep=";")

# edd
traits_peixes$Name_edited <- tolower (gsub (" ",".",traits_peixes$Name))
traits_peixes$Body_size_mod <- as.numeric(gsub (",",".",traits_peixes$Body_size))

especie [which(especie %in% traits_peixes$Name_edited == F)]

subset_traits <- traits_peixes [which(traits_peixes$Name_edited %in% especie),c("Name_edited", "Body_size_mod")]
spp_que_nao_tem <- especie [which(especie %in% traits_peixes$Name_edited == F)]

add_traits <- rbind (
  data.frame (Name_edited = spp_que_nao_tem[1],
    Body_size_mod = mean (as.numeric(traits_peixes [grep ("coryphopterus", traits_peixes$Name_edited),c("Body_size_mod")]))),

  data.frame (Name_edited = spp_que_nao_tem[2],
    Body_size_mod = (traits_peixes [grep ("diplodus.argenteus", traits_peixes$Name_edited),c("Body_size_mod")])),

  data.frame (Name_edited = spp_que_nao_tem[3],
    Body_size_mod = traits_peixes [grep ("haemulon.plumierii", traits_peixes$Name_edited),c("Body_size_mod")]),

  data.frame (Name_edited = spp_que_nao_tem[4],
    Body_size_mod = mean(traits_peixes [grep ("malacoctenus", traits_peixes$Name_edited),c("Body_size_mod")]))
)
## especies incluidas nas analises
subset_traits <- rbind(subset_traits, add_traits)
subset_traits <- subset_traits [match (especie,subset_traits$Name_edited),]
## sp que nao entraram nas analises
# quais das sp tem dados de traits
subset_traits_fora <- traits_peixes [which(traits_peixes$Name_edited %in% todas_sp_Morais),c("Name_edited", "Body_size_mod")]
# retirar as que estao na amostra
subset_traits_fora <- subset_traits_fora [which(subset_traits_fora$Name_edited %in% subset_traits$Name_edited==F),]

body_size_analysis <- data.frame (Body_size_mod = c(subset_traits$Body_size_mod, subset_traits_fora$Body_size_mod),
            Analyzed = as.factor(c(rep ("Yes", nrow(subset_traits)),
                rep ("No", nrow(subset_traits_fora)))))

### relacao efeito - body size
# efeito medio
efeito_para_bipartite <- lapply (selected.models, function (i)
  do.call (rbind,lapply (i ,function (k)
    k$summary [grep("beta1",rownames (k$summary)),"mean"]
  )))

efeito_para_bipartite<-do.call(cbind,efeito_para_bipartite)

# efeito sims
efeito_para_bipartite_sims <- lapply (selected.models, function (i)
  do.call (rbind,lapply (i ,function (k)
    k$sims.list$beta1
  )))

## efeito de cada simulacao

lapply (seq(1,length(efeito_para_bipartite_sims)), function(i) {
  
    png(here ("output",paste ("eff_BS_spB", names(efeito_para_bipartite_sims)[i],".png")), 
        width=12, height=12, units="cm", res=300)
    #pdf(file=paste ("eff_BS_sp", names(efeito_para_bipartite_sims)[i],".pdf"), 
    #    onefile=T,
    #   width=5, height=5#, units="cm", res=300
    #)
    
    plot(NA,
         ylim=c(-6,6),#range(efeito_para_bipartite_sims[[i]])[1]-2,
                #range(efeito_para_bipartite_sims[[i]])[2]+2), 
         xlim=c(3.5,90),
         ylab="Regression coefficient",
         xlab="Body size (cm.)",
         main= gsub("\\."," ",names(efeito_para_bipartite_sims)[i]))
    
    lapply (seq(1,ncol (efeito_para_bipartite_sims[[1]])),function (k) {
      
      #points(efeito_para_bipartite_sims[[i]][,k]~subset_traits$Body_size_mod,
       #  pch=1, col=rgb(0.400,0.400,0.400,alpha=0.1))
  
      abline (lm(efeito_para_bipartite_sims[[i]][,k]~subset_traits$Body_size_mod),
        col=rgb(0.400,0.400,0.400,alpha=0.1))
    })
      
      abline (lm(efeito_para_bipartite[,i]~subset_traits$Body_size_mod),
              col="black",lwd=3)
      
      dev.off()
      
    })

#####


pdf(here ("output", "effectxbodysize.pdf"),
    width=7,height = 5,onefile = T)

par(mfrow=c(2,3),mar=c(4,5,1.5,0.5))

### Agaricia spp
plot(NA,
     ylim=c(-6,6),#range(efeito_para_bipartite_sims[[i]])[1]-2,
     #range(efeito_para_bipartite_sims[[i]])[2]+2), 
     xlim=c(3.5,90),
     ylab="Regression coefficient",
     xlab=NA,
     xaxt="n",
     main= gsub("\\."," ",names(efeito_para_bipartite_sims)[[6]]),
     cex.lab=1.2)

lapply (seq(1,ncol (efeito_para_bipartite_sims[[6]])),function (k) {
  
  #points(efeito_para_bipartite_sims[[i]][,k]~subset_traits$Body_size_mod,
  #  pch=1, col=rgb(0.400,0.400,0.400,alpha=0.1))
  
  abline (lm(efeito_para_bipartite_sims[[6]][,k]~subset_traits$Body_size_mod),
          col=rgb(0.400,0.400,0.400,alpha=0.1))
})

abline (lm(efeito_para_bipartite[,6]~subset_traits$Body_size_mod),
        col="black",lwd=3)


### Millepora alcicornis
plot(NA,
     ylim=c(-6,6),#range(efeito_para_bipartite_sims[[i]])[1]-2,
     #range(efeito_para_bipartite_sims[[i]])[2]+2), 
     xlim=c(3.5,90),
     ylab=NA,
     xlab=NA,
     xaxt="n",
     
     main= gsub("\\."," ",names(efeito_para_bipartite_sims)[[1]]))

lapply (seq(1,ncol (efeito_para_bipartite_sims[[1]])),function (k) {
  
  #points(efeito_para_bipartite_sims[[i]][,k]~subset_traits$Body_size_mod,
  #  pch=1, col=rgb(0.400,0.400,0.400,alpha=0.1))
  
  abline (lm(efeito_para_bipartite_sims[[1]][,k]~subset_traits$Body_size_mod),
          col=rgb(0.400,0.400,0.400,alpha=0.1))
})

abline (lm(efeito_para_bipartite[,1]~subset_traits$Body_size_mod),
        col="black",lwd=3)

## Montastraea cavernosa
plot(NA,
     ylim=c(-6,6),#range(efeito_para_bipartite_sims[[i]])[1]-2,
     #range(efeito_para_bipartite_sims[[i]])[2]+2), 
     xlim=c(3.5,90),
     ylab=NA,
     xlab=NA,
     xaxt="n",
     
     main= gsub("\\."," ",names(efeito_para_bipartite_sims)[[2]]),
     cex.main=1.2)

lapply (seq(1,ncol (efeito_para_bipartite_sims[[2]])),function (k) {
  
  #points(efeito_para_bipartite_sims[[i]][,k]~subset_traits$Body_size_mod,
  #  pch=1, col=rgb(0.400,0.400,0.400,alpha=0.1))
  
  abline (lm(efeito_para_bipartite_sims[[2]][,k]~subset_traits$Body_size_mod),
          col=rgb(0.400,0.400,0.400,alpha=0.1))
})

abline (lm(efeito_para_bipartite[,2]~subset_traits$Body_size_mod),
        col="black",lwd=3)

### Mussismilia harttii
plot(NA,
     ylim=c(-6,6),#range(efeito_para_bipartite_sims[[i]])[1]-2,
     #range(efeito_para_bipartite_sims[[i]])[2]+2), 
     xlim=c(3.5,90),
     ylab="Regression coefficient",
     xlab=NA,
     cex.lab=1.2,
     
     main= gsub("\\."," ",names(efeito_para_bipartite_sims)[[3]]))

lapply (seq(1,ncol (efeito_para_bipartite_sims[[3]])),function (k) {
  
  #points(efeito_para_bipartite_sims[[i]][,k]~subset_traits$Body_size_mod,
  #  pch=1, col=rgb(0.400,0.400,0.400,alpha=0.1))
  
  abline (lm(efeito_para_bipartite_sims[[3]][,k]~subset_traits$Body_size_mod),
          col=rgb(0.400,0.400,0.400,alpha=0.1))
})

abline (lm(efeito_para_bipartite[,3]~subset_traits$Body_size_mod),
        col="black",lwd=3)

### Mussismilia hispida
plot(NA,
     ylim=c(-6,6),#range(efeito_para_bipartite_sims[[i]])[1]-2,
     #range(efeito_para_bipartite_sims[[i]])[2]+2), 
     xlim=c(3.5,90),
     ylab=NA,
     xlab="Body size (cm.)",
     cex.lab=1.2,
     main= gsub("\\."," ",names(efeito_para_bipartite_sims)[[4]]))

lapply (seq(1,ncol (efeito_para_bipartite_sims[[4]])),function (k) {
  
  #points(efeito_para_bipartite_sims[[i]][,k]~subset_traits$Body_size_mod,
  #  pch=1, col=rgb(0.400,0.400,0.400,alpha=0.1))
  
  abline (lm(efeito_para_bipartite_sims[[4]][,k]~subset_traits$Body_size_mod),
          col=rgb(0.400,0.400,0.400,alpha=0.1))
})

abline (lm(efeito_para_bipartite[,4]~subset_traits$Body_size_mod),
        col="black",lwd=3)

## Siderastrea spp.

plot(NA,
     ylim=c(-6,6),#range(efeito_para_bipartite_sims[[i]])[1]-2,
     #range(efeito_para_bipartite_sims[[i]])[2]+2), 
     xlim=c(3.5,90),
     ylab=NA,
     xlab=NA,
     main= gsub("\\."," ",names(efeito_para_bipartite_sims)[[5]]))

lapply (seq(1,ncol (efeito_para_bipartite_sims[[5]])),function (k) {
  
  #points(efeito_para_bipartite_sims[[i]][,k]~subset_traits$Body_size_mod,
  #  pch=1, col=rgb(0.400,0.400,0.400,alpha=0.1))
  
  abline (lm(efeito_para_bipartite_sims[[5]][,k]~subset_traits$Body_size_mod),
          col=rgb(0.400,0.400,0.400,alpha=0.1))
})

abline (lm(efeito_para_bipartite[,5]~subset_traits$Body_size_mod),
        col="black",lwd=3)

dev.off()



#######################################################
######################################################

result_occ <- lapply (seq (1,length(selected.models)), function (coral) {
  
  
  occ_spp <- unlist(
    lapply(seq(1,length(selected.models[[coral]])), function (i)
      mean (selected.models[[coral]][[i]]$mean$psi)
    )
  )
  
  ### dados de deteccao para cada simulacao
  occ_spp_sims <- (
    lapply(seq(1,length(selected.models[[coral]])), function (i)
      rowMeans (selected.models[[coral]][[i]]$sims.list$psi)
    ))
    
  res_occ <- list (occ_spp = occ_spp,
        occ_spp_sims = occ_spp_sims)
  ;  res_occ
    
     
  })
  

## ATRIBUTOS CATEGORICOS
### home range e group size

cat_traits <- traits_peixes[which(traits_peixes$Name_edited %in% subset_traits$Name_edited),
                            c("Name_edited","Home_range", "Diel_activity","Size_group","Diet")]

add_cat_traits <- rbind (
  data.frame (Name_edited = spp_que_nao_tem[1],
              traits_peixes [grep ("coryphopterus", traits_peixes$Name_edited),c("Home_range", "Diel_activity","Size_group","Diet")]
              [2,]),## selecionar o 2, que é um small g e onivoro, que parece o mais predominante
  
  data.frame (Name_edited = spp_que_nao_tem[2],
              traits_peixes [grep ("diplodus.argenteus", traits_peixes$Name_edited),c("Home_range", "Diel_activity","Size_group","Diet")]),
  
  data.frame (Name_edited = spp_que_nao_tem[3],
              traits_peixes [grep ("haemulon.plumierii", traits_peixes$Name_edited),c("Home_range", "Diel_activity","Size_group","Diet")]),
  
  data.frame (Name_edited = spp_que_nao_tem[4],
              traits_peixes [grep ("malacoctenus", traits_peixes$Name_edited),c("Home_range", "Diel_activity","Size_group","Diet")]
              [1,]) ## todos sao iguais, entao pegar o primeiro
)
#
subset_cat_traits <- rbind(cat_traits, add_cat_traits)
subset_cat_traits <- subset_cat_traits [match (especie,subset_cat_traits$Name_edited),]

##
par(mfrow=c(1,1))
plot((subset_traits$Body_size_mod), result_occ[[1]]$occ_spp)
abline(lm(result_occ[[1]]$occ_spp~subset_traits$Body_size_mod))

### mobility
mobility <- as.factor(subset_cat_traits$Home_range)
levels(mobility)[which(levels (mobility) == "vmob")] <-  "mob"
levels(mobility)[which(levels (mobility) == "mob")] <-  "Mobile"
levels(mobility)[which(levels (mobility) == "sed")] <-  "Sedentary"
mobility<-droplevels(mobility)

pdf (here("output", "mobility_psi.pdf"), width=5,height = 5)
plot(mobility, result_occ[[1]]$occ_spp,ylim=c(0,1),
     xaxt="n",
     yaxt="n")


sims_occ_peixe <- lapply (seq (1,1200), function (sim)
  unlist(lapply  (seq (1,42), function (peixe)
  
    result_occ[[1]]$occ_spp_sims[[peixe]][sim]
    
)))


lapply (sims_occ_peixe, function (i)

       
       plot (mobility, i,add=T,
             border=rgb(0.400,0.400,0.400,alpha=0.1),
              col=rgb(0.400,0.400,0.400,alpha=0.1),
              xaxt="n",
              yaxt="n",
               xlab=NA,
               ylab=NA))

plot(mobility, result_occ[[1]]$occ_spp,ylim=c(0,1),add=T,
     xlab="Mobility",
     ylab="Occupancy probability",lwd=2,
     col="gray")

dev.off()

## mobility x regression

pdf (here("output", "mobility_effect.pdf"), width=7,height = 5)

par(mfrow=c(2,3),mar=c(4,5,1.5,0.5))

## agaricia
plot(mobility, efeito_para_bipartite[,6],ylim=c(-20,25),
     col=rgb(0.400,0.400,0.400,alpha=0.1),
     border=rgb(0.400,0.400,0.400,alpha=0.1),
     xlab=NA,
     ylab="Regression coefficient",
     cex.lab=1.2,
     main= gsub("\\."," ",names(efeito_para_bipartite_sims)[[6]]))

lapply (seq(1,ncol (efeito_para_bipartite_sims[[6]])),function (k) {
  
  
  plot (efeito_para_bipartite_sims[[6]][,k]~mobility,add=T,
          col=rgb(0.400,0.400,0.400,alpha=0.1),
          border = rgb(0.400,0.400,0.400,alpha=0.1),
        xaxt="n",
        yaxt="n",
        xlab=NA,
        ylab=NA)
    }
  )

plot(mobility, efeito_para_bipartite[,6],add=T,
     lwd=2,
     col="gray")


## millepora

plot(mobility, efeito_para_bipartite[,1],ylim=c(-5,14),
     col=rgb(0.400,0.400,0.400,alpha=0.1),
     border=rgb(0.400,0.400,0.400,alpha=0.1),
     xlab=NA,
     ylab=NA,
     cex.lab=1.2,
     main= gsub("\\."," ",names(efeito_para_bipartite_sims)[[1]]))

lapply (seq(1,ncol (efeito_para_bipartite_sims[[1]])),function (k) {
  
  
  plot (efeito_para_bipartite_sims[[1]][,k]~mobility,add=T,
        col=rgb(0.400,0.400,0.400,alpha=0.1),
        border = rgb(0.400,0.400,0.400,alpha=0.1),
        xaxt="n",
        yaxt="n",
        xlab=NA,
        ylab=NA)
}
)

plot(mobility, efeito_para_bipartite[,1],add=T,
     lwd=2,
     col="gray")


## montrastea
plot(mobility, efeito_para_bipartite[,2],ylim=c(-20,25),
     col=rgb(0.400,0.400,0.400,alpha=0.1),
     border=rgb(0.400,0.400,0.400,alpha=0.1),
     xlab=NA,
     ylab=NA,
     cex.lab=1.2,
     main= gsub("\\."," ",names(efeito_para_bipartite_sims)[[2]]))

lapply (seq(1,ncol (efeito_para_bipartite_sims[[2]])),function (k) {
  
  
  plot (efeito_para_bipartite_sims[[2]][,k]~mobility,add=T,
        col=rgb(0.400,0.400,0.400,alpha=0.1),
        border = rgb(0.400,0.400,0.400,alpha=0.1),
        xaxt="n",
        yaxt="n",
        xlab=NA,
        ylab=NA)
}
)

plot(mobility, efeito_para_bipartite[,2],add=T,
     lwd=2,
     col="gray")

## muss harttii


plot(mobility, efeito_para_bipartite[,3],ylim=c(-15,20),
     col=rgb(0.400,0.400,0.400,alpha=0.1),
     border=rgb(0.400,0.400,0.400,alpha=0.1),
     xlab=NA,
     ylab="Regression coefficient",
     cex.lab=1.2,
     main= gsub("\\."," ",names(efeito_para_bipartite_sims)[[3]]))

lapply (seq(1,ncol (efeito_para_bipartite_sims[[3]])),function (k) {
  
  
  plot (efeito_para_bipartite_sims[[3]][,k]~mobility,add=T,
        col=rgb(0.400,0.400,0.400,alpha=0.1),
        border = rgb(0.400,0.400,0.400,alpha=0.1),
        xaxt="n",
        yaxt="n",
        xlab=NA,
        ylab=NA)
}
)

plot(mobility, efeito_para_bipartite[,3],add=T,
     lwd=2,
     col="gray")

## mus hisp


plot(mobility, efeito_para_bipartite[,4],ylim=c(-5,14),
     col=rgb(0.400,0.400,0.400,alpha=0.1),
     border=rgb(0.400,0.400,0.400,alpha=0.1),
     xlab="Mobility",
     ylab=NA,
     cex.lab=1.2,
     main= gsub("\\."," ",names(efeito_para_bipartite_sims)[[4]]))

lapply (seq(1,ncol (efeito_para_bipartite_sims[[1]])),function (k) {
  
  
  plot (efeito_para_bipartite_sims[[4]][,k]~mobility,add=T,
        col=rgb(0.400,0.400,0.400,alpha=0.1),
        border = rgb(0.400,0.400,0.400,alpha=0.1),
        xaxt="n",
        yaxt="n",
        xlab=NA,
        ylab=NA)
}
)

plot(mobility, efeito_para_bipartite[,4],add=T,
     lwd=2,
     col="gray")

## siderastra

plot(mobility, efeito_para_bipartite[,5],ylim=c(-5,14),
     col=rgb(0.400,0.400,0.400,alpha=0.1),
     border=rgb(0.400,0.400,0.400,alpha=0.1),
     xlab=NA,
     ylab=NA,
     cex.lab=1.2,
     main= gsub("\\."," ",names(efeito_para_bipartite_sims)[[5]]))

lapply (seq(1,ncol (efeito_para_bipartite_sims[[5]])),function (k) {
  
  
  plot (efeito_para_bipartite_sims[[5]][,k]~mobility,add=T,
        col=rgb(0.400,0.400,0.400,alpha=0.1),
        border = rgb(0.400,0.400,0.400,alpha=0.1),
        xaxt="n",
        yaxt="n",
        xlab=NA,
        ylab=NA)
}
)

plot(mobility, efeito_para_bipartite[,5],add=T,
     lwd=2,
     col="gray")


dev.off()


## occupamncy x body size

pdf (here("output", "bodys_psi.pdf"), width=5,height = 5)

plot(NA,ylim=c(0,1),
     xlim=c(3.5,90),
     xlab="Body size (cm.)",
     ylab="Occupancy probability")

lapply (sims_occ_peixe, function (i)
  
  
  abline (lm(i~subset_traits$Body_size_mod),
        col=rgb(0.400,0.400,0.400,alpha=0.1)))

abline(lm (result_occ[[1]]$occ_spp~subset_traits$Body_size_mod),
     col="black",lwd=2)

dev.off()
#### 

#################


########################
########## FIG S1
########################
# analysis and body size
par(mfrow=c(1,1),
    mar=c(4,4,4,4))
with(body_size_analysis, 
     plot(Body_size_mod ~Analyzed,
          ylab="Body size (cm)",
          col="gray"))

## range of body sizes of analyzed species
range(subset_traits$Body_size_mod)

# rank according to size
subset_traits [order(subset_traits$Body_size_mod),]

### a deteccao media das spp

########################
########## FIG S2 E 4
########################

lapply (seq (1,length(selected.models)), function (coral) {
  
  png(here ("output",paste ("det_spB", names(selected.models)[coral],".png")), 
      width=12, height=12, units="cm", res=300)
  #pdf(file=paste ("det_sp", names(samples_trans_OBS)[coral],".pdf"), 
  #    onefile=T,
  #    width=5, height=5#, units="cm", res=300
  #    )
  
  det_spp <- unlist(
    lapply(seq(1,length(selected.models[[coral]])), function (i)
      selected.models[[coral]][[i]]$mean$mean.p
    )
  )
  
  ### dados de deteccao para cada simulacao
  det_spp_sims <- (
    lapply(seq(1,length(selected.models[[coral]])), function (i)
      selected.models[[coral]][[i]]$sims.list$mean.p
    )
  )
  
  ### plot vazio
  plot(NA,xlim=c(3.5,90),
       ylim=c(0.0,1),
       ylab="Detection probability (p)",
       xlab = "Body size (cm.)",
       main = gsub ("\\."," ",names(selected.models)[coral]))
  
  ## pegar os dados de cada simulacao
  dados_para_plot<- lapply (seq (1,1200), function (sim) 
    
    unlist(lapply (seq (1,42), function (peixe) {
      
      dados <- det_spp_sims[[peixe]][sim]
      
      
      ;dados
      
    }
    )
    )
  )
  
  ## botar os pontos de cada simulacao
  lapply (dados_para_plot, function (i) {
    #points(subset_traits$Body_size_mod,i,pch=1,cex=0.7,
    # col=rgb(0.400,0.400,0.400,alpha=0.1))
    ## botar as linhas do modelo de cada simulacao
    abline (lm (i~subset_traits$Body_size_mod),
            col=rgb(0.400,0.400,0.400,alpha=0.1))
    
  }
  )
  
  ## botar a linha media
  abline (lm (det_spp~subset_traits$Body_size_mod),
          col="black",
          lwd=3)
  
  dev.off()
  
})

## outra parte da figura 4

det_depth <- do.call(rbind , 
                     
                     lapply (seq (1,length(selected.models[[1]])), function (i)
                       
                       selected.models [[1]][[i]]$mean$alpha.depth
                     ))

colnames(det_depth)<- c("Shallow", "Deep")

## simulacoes
det_depth_sims <- lapply (seq (1,length(selected.models[[2]])), function (i)
  
  selected.models [[1]][[i]]$sims.list$alpha.depth
)

## organizar de um modo mais amigavel para plotar
det_depth_sims <- lapply (seq (1,1200), function (sim)
  matrix(unlist(lapply  (seq (1,42), function (peixe)
    
    det_depth_sims[[peixe]][sim,])),
    ncol=2,dimnames = list(NULL, c("Shallow", "Deep")))
  
)

require(reshape)
det_depth <- melt(det_depth)
det_depth_sims <- lapply (det_depth_sims,melt)
#det_depth$X2 <- ifelse (det_depth$X2 == 1, "Shallow","Deep")
#det_depth$X2 <- as.factor (det_depth$X2)

pdf(here ("output","det_depth_OBSID_other.pdf"),width=5,height = 5,onefile=T)

plot(det_depth$value~ det_depth$X2,
     ylim=c(0,1),
     col=  rgb(0.400,0.400,0.400,alpha=0.1),
     xlab="Depth",
     ylab="Detection probability (p)")

## incerteza
lapply (det_depth_sims, function (i)
  
  plot(i$value~ i$X2,add=T,
       ylim=c(0,1),
       border=rgb(0.400,0.400,0.400,alpha=0.1),
       col=rgb(0.400,0.400,0.400,alpha=0.1),
       xlab=NA,
       ylab=NA,
       xaxt="n",
       yaxt="n"))


plot(det_depth$value~ det_depth$X2,add=T,
     ylim=c(0,1),
     col=  "gray",lwd=2,
     xlab="Depth",
     ylab="Detection probability (p)",
     main = "Site depth")


## OBSERVER ID effect

det_obs <- do.call(rbind , lapply (seq (1,42), function (i)
  selected.models [[1]][[i]]$mean$alpha.obs
))

## simulacoes
det_obs_sims <- lapply (seq (1,length(selected.models[[2]])), function (i)
  
  selected.models [[1]][[i]]$sims.list$alpha.obs
)


## organizar de um modo mais amigavel para plotar
det_obs_sims <- lapply (seq (1,1200), function (sim)
  matrix(unlist(lapply  (seq (1,42), function (peixe)
    
    det_obs_sims[[peixe]][sim,])),
    ncol=13,dimnames = list(NULL,paste ("OBS",seq(1,13))))
  
)

colnames(det_obs)<- paste ("OBS",seq(1,13),sep="")
require(reshape)
det_obs <- melt(det_obs)
det_obs_sims <- lapply (det_obs_sims,melt)

det_obs$X2<- factor (det_obs$X2, 
                     levels = levels(det_obs$X2)[order(as.numeric(gsub("OBS","", levels (det_obs$X2))))])

plot(det_obs$value~det_obs$X2,
     xlab = "Observed ID",
     ylab= "Detection probability (p)",
     col="gray",
     cex.axis=0.6,las=2,
     ylim=c(0,1),
     main="Observer ID")

## incerteza
lapply (det_obs_sims, function (i)
  
  plot(i$value~ i$X2,add=T,
       ylim=c(0,1),
       border=rgb(0.400,0.400,0.400,alpha=0.1),
       col=rgb(0.400,0.400,0.400,alpha=0.1),
       xlab=NA,
       ylab=NA,
       xaxt="n",
       yaxt="n"))


plot(det_obs$value~ det_obs$X2,add=T,
     ylim=c(0,1),
     col=  "gray",lwd=2,
     xlab="Observer ID",
     ylab="Detection probability (p)",
     xaxt="n",
     yaxt="n")


## relaçao deteccao x tamanho do grupo, body size, diel activity

## dados de deteccao

## tendencia media
det_spp <- lapply (seq (1,length(selected.models)), function (coral)
  
  
  unlist(
    lapply(seq(1,length(selected.models[[coral]])), function (i)
      selected.models[[coral]][[i]]$mean$mean.p
    )
  ))


### dados de deteccao para cada simulacao
det_spp_sims <- lapply (seq (1,length(selected.models)), function (coral)
    lapply(seq(1,length(selected.models[[coral]])), function (i)
      selected.models[[coral]][[i]]$sims.list$mean.p
    )
  )
  
## organizar de um modo mais amigavel
det_spp_sims <- lapply (seq (1,1200), function (sim)
  unlist(lapply  (seq (1,42), function (peixe)
    
    det_spp_sims[[1]][[peixe]][sim]
    
  )))

## organizar os dados de tamanho de grupo

levels (subset_cat_traits$Size_group) [which(levels (subset_cat_traits$Size_group) == "largeg")] <- "medg"
levels (subset_cat_traits$Size_group) [which(levels (subset_cat_traits$Size_group) == "medg")] <- "Med-Large"
levels (subset_cat_traits$Size_group) [which(levels (subset_cat_traits$Size_group) == "smallg")] <- "Small"
levels (subset_cat_traits$Size_group) [which(levels (subset_cat_traits$Size_group) == "pair")] <- "Pair"
levels (subset_cat_traits$Size_group) [which(levels (subset_cat_traits$Size_group) == "sol")] <- "Solitary"

subset_cat_traits$Size_group <- droplevels(subset_cat_traits$Size_group)
subset_cat_traits$Size_group <- factor (subset_cat_traits$Size_group,
                                        levels = c("Solitary",
                                                   "Pair",
                                                   "Small",
                                                   "Med-Large"))

plot (subset_cat_traits$Size_group, det_spp[[1]],
      xaxt="n",
      yaxt="n",
      ylim=c(0,1),
      main  = "Group size")

lapply (det_spp_sims, function (i)
  
  
  plot (subset_cat_traits$Size_group, i,
        add=T,
        border=rgb(0.400,0.400,0.400,alpha=0.1),
        col=rgb(0.400,0.400,0.400,alpha=0.1),
        xaxt="n",
        yaxt="n",
        xlab=NA,
        ylab=NA))


plot(subset_cat_traits$Size_group,  det_spp[[1]],
     ylim=c(0,1),add=T,
     xlab="Group size",
     ylab="Detection probability (p)",lwd=2,
     col="gray")

#####################
### diel activity
#####################

activity <- as.factor(subset_cat_traits$Diel_activity)
levels(activity)[which(levels (activity) == "night")] <-  "both"
levels(activity)[which(levels (activity) == "day")] <-  "Day"
levels(activity)[which(levels (activity) == "both")] <-  "Day-night"
activity<-droplevels(activity)

plot (activity, det_spp[[1]],
      xaxt="n",
      yaxt="n",
      ylim=c(0,1),
      main  = "Diel activity")

lapply (det_spp_sims, function (i)
  
  
  plot (activity, i,
        add=T,
        border=rgb(0.400,0.400,0.400,alpha=0.1),
        col=rgb(0.400,0.400,0.400,alpha=0.1),
        xaxt="n",
        yaxt="n",
        xlab=NA,
        ylab=NA))


plot(activity,  det_spp[[1]],
     ylim=c(0,1),add=T,
     xlab="Diel activity",
     ylab="Detection probability (p)",lwd=2,
     col="gray")

#####################
### body size
#####################


plot (NA,
      xlim=c (3.5, 90),
      ylim=c(0,1),
      xlab= "Body size (cm.)",
      ylab= "Detection probability",
      main="Body size")

lapply (det_spp_sims, function (i)
  
  
  abline (lm (i ~subset_traits$Body_size_mod),
        col=rgb(0.400,0.400,0.400,alpha=0.1)))


abline (lm(det_spp[[1]] ~subset_traits$Body_size_mod),
     lwd=3,
     col="black")

dev.off()

