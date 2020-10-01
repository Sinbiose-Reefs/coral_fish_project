## code to extract information needed to represent the strength of association between coral cover/site occupancy and fish site-occupancy probability
## I had to do this code because the output of site-occupancy models are too large
## the computer could not deal with such amount of estimates. Thus, for original cover/occupancy, and discounts of
## 25% and 50% of these quantities, I open the model, extracted one of them (original first, 25% discount, 50% discount) and 
## extracted relevant information. The information I need is in a list of tables. Each table refers to the results of one of 6 coral species.
## The fish species are in the table rows, and the interesting quantities are in the cols.

# load packages and functions
source ("R/packages.R")
source ("R/functions.R")

# load basic data for naming the table dims
## fish data
load (here("output","Data_fish_detection.RData"))

## coral detection - with coordinates
load (here("output","Data_coral_detection.RData"))

## LOAD MODEL RESULTS

load(here("output","samples_OCCcoral_PdepthObsID_NoRegion.RData")) 

# cenarios
coral_cover_data <- lapply (seq(1,ncol(sp_cover_data)), function (i) {
  
  coral_cover <- cbind (original= sp_cover_data[,i], 
                        less20 = sp_cover_data[,i] * 0.80,
                        less40 = sp_cover_data[,i] * 0.60,
                        less60 = sp_cover_data[,i] * 0.40,
                        less80 = sp_cover_data[,i] * 0.20)
})


## data to use in the plot
extracted_data <- 
  
  lapply (seq(1,length(coral_species)), function (coral)
            
   lapply (seq (1, length (fish_species [[coral]])), function (fish)
    
     do.call(rbind, lapply (seq (1,length(colnames(coral_cover_data[[coral]]))), function (cenario)
      
         data.frame (
  
          coral = coral_species[coral],

          peixe = fish_species [[coral]][fish],

          cenario = colnames(coral_cover_data[[coral]])[cenario],

          estimate = mean (samples_OCCcoral_PdepthObsID_NoRegion[[coral]][[cenario]]$sims.list$beta1 [,fish]),
  
          low = quantile (samples_OCCcoral_PdepthObsID_NoRegion[[coral]][[cenario]]$sims.list$beta1 [,fish], 0.05),

          high = quantile (samples_OCCcoral_PdepthObsID_NoRegion[[coral]][[cenario]]$sims.list$beta1 [,fish],0.95)

)
)
)
))

teste <- do.call (rbind, extracted_data[[1]])

teste$cenario<-factor(teste$cenario,
       levels = c("less80","less60","less40","less20","original"))


require(ggplot2)
pd <- position_dodge(.2)

a <- ggplot (teste, aes  (y=peixe, x=estimate, fill=cenario,
                                        colour=cenario)) + 
  geom_errorbar(aes(xmin=low,xmax=high),width = .3,
                position=pd) + theme_classic() + 
  geom_point(aes(estimate),size=2,position=pd) + 
  geom_vline(xintercept = 0, linetype="dashed", 
             color = "gray50", size=0.5)+
  scale_color_manual(values=c("gray70", "gray60", "gray50",
                              "gray40","black")) + 
  xlab("Regression coefficient estimate") + 
  ylab ("Reef fish species") + 
  xlim(-20, 20)
  

# regression shapes

data_shape <- lapply (c (-10,-2,0,2,10), function (i)
  
            data.frame (cc=seq(-2,2,0.1),
                          rel=plogis ( 0 + i * seq(-2,2,0.1)))
)


plot_eff <- lapply (data_shape, function (i)
  
  ggplot (i, aes (x=cc,y=rel)) + 
  geom_line(size=2) + theme_classic() + 
  theme (axis.title = element_blank(),
         axis.text = element_blank(),
         axis.ticks = element_blank()) 
)

library(ggplot2)
library(gridExtra)
library(grid)

grid.arrange(
  plot_eff [[1]],
  plot_eff [[2]], 
  plot_eff [[3]],
  plot_eff [[4]],
  plot_eff [[5]],
  a,
  ncol=9,nrow=7, 
  layout_matrix = rbind (c(NA,NA,1,2,3,4,5,NA,NA),
                         c(6,6,6,6,6,6,6,6,6),
                         c(6,6,6,6,6,6,6,6,6),
                         c(6,6,6,6,6,6,6,6,6),
                         c(6,6,6,6,6,6,6,6,6),
                         c(6,6,6,6,6,6,6,6,6),
                         c(6,6,6,6,6,6,6,6,6)))

grid.text("% of coral cover", 
          x = unit(0.5, "npc"), 
          y = unit(.86, "npc"),gp = gpar(fontsize=8))

grid.text(expression (psi['i']), 
          x = unit(0.22, "npc"), 
          y = unit(.94, "npc"),
          gp = gpar(fontsize=10),
          rot=90)


####










## table of bayesian P value for models with original coral cover
bvclosed_original <- lapply (lista_modelos_original, function (model)
  
  lapply (model, function (i)
    
    unlist(lapply (i, function (k)
      
      sum (k$sims.list$Chi2repClosed > k$sims.list$Chi2Closed)/
        length(k$sims.list$Chi2repClosed)
    )
    )
  )
)
## table of bayesian P value for models with estimated site occupancy
bvclosed_occupancy <- lapply (lista_modelos_occupancy, function (model)
  
  lapply (model, function (i)
    
    unlist(lapply (i, function (k)
      
      sum (k$sims.list$Chi2repClosed > k$sims.list$Chi2Closed)/
        length(k$sims.list$Chi2repClosed)
    )
    )
  )
)
# nomear a lista de resultados
bvclosed_original <- lapply (bvclosed_original, function (i) {names (i) <- sp_coral; i})
bvclosed_occupancy <- lapply (bvclosed_occupancy, function (i) {names (i) <- sp_coral; i})

# reorganizing the list
tab_bpv_sp_original <- lapply (sp_coral, function (sp)
  lapply (bvclosed_original, function (i) 
    i [ which (names(i) == sp)])
)
tab_bpv_sp_occupancy <- lapply (sp_coral, function (sp)
  lapply (bvclosed_occupancy, function (i) 
    i [ which (names(i) == sp)])
)

## transforming into matrix
tab_bpv_sp_original <- lapply (tab_bpv_sp_original, function (i) 
  data.frame (matrix(unlist(i),ncol= length(bvclosed_original ),byrow = F )))
names(tab_bpv_sp_original)<- sp_coral

tab_bpv_sp_occupancy <- lapply (tab_bpv_sp_occupancy, function (i) 
  data.frame (matrix(unlist(i),ncol= length(bvclosed_occupancy),byrow = F )))
names(tab_bpv_sp_occupancy)<- sp_coral

## selecao do melhor modelo por especie 
## de acordo com o  BPV mais proximo a 0.5 (using the function 'closest')

# model with BPV closest to 0.5 (better fit to data)
# original cover
mod_sel_original <- lapply (seq (1,length(tab_bpv_sp_original)), function (i) # sp coral - lista
  
  do.call(rbind, lapply (seq (1,nrow (tab_bpv_sp_original [[i]])), function (k) { # sp peixe - linhas
    
    
    ## obter o valor de bpv mais proxima a 0.5
    closestbpv <-as.numeric(closest (tab_bpv_sp_original[[i]][k,],0.5)[1])
    
    ## no caso de BPV=0 em todos, pega o primeiro modelo que eh o mais parcimonioso (menos parametros)
    qual.melhor <- ifelse (closestbpv == 0, 1, which(tab_bpv_sp_original[[i]][k,] == closestbpv)) 
    
    
  }
  )
  )
)

names(mod_sel_original)<- sp_coral
# site-occupancy probability
mod_sel_occupancy <- lapply (seq (1,length(tab_bpv_sp_occupancy)), function (i) # sp coral - lista
  
  do.call(rbind, lapply (seq (1,nrow (tab_bpv_sp_occupancy [[i]])), function (k) { # sp peixe - linhas
    
    
    ## obter o valor de bpv mais proxima a 0.5
    closestbpv <-as.numeric(closest (tab_bpv_sp_occupancy[[i]][k,],0.5)[1])
    
    ## no caso de BPV=0 em todos, pega o primeiro modelo que eh o mais parcimonioso (menos parametros)
    qual.melhor <- ifelse (closestbpv == 0, 1, which(tab_bpv_sp_occupancy[[i]][k,] == closestbpv)) 
    
    
  }
  )
  )
)

names(mod_sel_occupancy)<- sp_coral

## selecting models
# original
selected.models_original <- lapply (seq (1,length (lista_modelos_original [[1]])), function (k)
  lapply (seq (1,length (lista_modelos_original [[1]][[1]])), function (i) {
    
    sp.coral <-  k
    sp.peixe <-  i
    qual.sel <- mod_sel_original [[sp.coral]][sp.peixe,]
    
    
    selected.models <- (lista_modelos_original [[qual.sel]] # ## seleciona o modelo mod, 
                        [[sp.coral]] ## da sp de coral k
                        [[sp.peixe]]) ## sp de peixe i
    ; selected.models
    
  }))

names(selected.models_original) <- sp_coral

# occupancy
selected.models_occupancy <- lapply (seq (1,length (lista_modelos_occupancy [[1]])), function (k)
  lapply (seq (1,length (lista_modelos_occupancy [[1]][[1]])), function (i) {
    
    sp.coral <-  k
    sp.peixe <-  i
    qual.sel <- mod_sel_occupancy [[sp.coral]][sp.peixe,]
    
    
    selected.models <- (lista_modelos_occupancy [[qual.sel]] # ## seleciona o modelo mod, 
                        [[sp.coral]] ## da sp de coral k
                        [[sp.peixe]]) ## sp de peixe i
    ; selected.models
    
  }))

names(selected.models_occupancy) <- sp_coral

## de agora em diante, trabalhar somente com os modelos selecionados

## efeitos original
efeitos_original <-  lapply (selected.models_original, function (i) 
  
  do.call(cbind,lapply (i, function (k)
    
    k$summary [grep("beta1",rownames(k$summary)),"overlap0"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)

tab_efeitos_sp_original <- lapply (efeitos_original, function (i)
  ifelse (i == 1, "No", "Yes")
)

# efeitos occupancy
efeitos_occupancy <-  lapply (selected.models_occupancy, function (i) 
  
  do.call (cbind, lapply (i, function (k)
    
    k$summary [grep("beta1",rownames(k$summary)),"overlap0"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)

tab_efeitos_sp_occupancy <- lapply (efeitos_occupancy, function (i)
  ifelse (i == 1, "No", "Yes")
)

#### coeficientes de regressao
# original cover
coefic_original <-  lapply (selected.models_original, function (i) 
  
  do.call (cbind,lapply (i, function (k)
    
    k$summary [grep("beta1",rownames(k$summary)),"mean"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)
# site-occupancy
coefic_occupancy <-  lapply (selected.models_occupancy, function (i) 
  
  do.call (cbind,lapply (i, function (k)
    
    k$summary [grep("beta1",rownames(k$summary)),"mean"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)


## finite sample size (n.occ)
#### original cover
nocc_original <- lapply (selected.models_original, function (i) 
  
  unlist (lapply (i, function (k)
    
    k$summary [grep("n.occ",rownames(k$summary)),"mean"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)

#### site-occupancy
nocc_occupancy <- lapply (selected.models_occupancy, function (i) 
  
  unlist (lapply (i, function (k)
    
    k$summary [grep("n.occ",rownames(k$summary)),"mean"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)

cbind(nocc_original[[1]],nocc_occupancy[[1]])

######## detection probability
det_prob_original <-  lapply (selected.models_original, function (i) 
  
  unlist (lapply (i, function (k)
    
    k$summary [grep("mean.p",rownames(k$summary)),"mean"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)

det_prob_occupancy <-  lapply (selected.models_occupancy, function (i) 
  
  unlist (lapply (i, function (k)
    
    k$summary [grep("mean.p",rownames(k$summary)),"mean"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)

cbind(det_prob_original[[1]],det_prob_occupancy[[1]])

## bayesian p-value dos selecionados
# original
bpv.selected_original <- lapply (selected.models_original, function (i)
  
  unlist(lapply (i, function (k)
    
    sum (k$sims.list$Chi2repClosed > k$sims.list$Chi2Closed)/
      length(k$sims.list$Chi2repClosed)
  )
  )
)

# occupancy
bpv.selected_occupancy <- lapply (selected.models_occupancy, function (i)
  
  unlist(lapply (i, function (k)
    
    sum (k$sims.list$Chi2repClosed > k$sims.list$Chi2Closed)/
      length(k$sims.list$Chi2repClosed)
  )
  )
)

cbind(bpv.selected_original[[1]], bpv.selected_occupancy[[1]])

## tabela com as info do efeito das covs de coral nas spp
tab_summarized_original <- lapply(seq(1,length(tab_efeitos_sp_original)), function(i)
  data.frame(sp=especie, 
             effectReg1 = tab_efeitos_sp_original[[i]][1,],
             effectReg2 = tab_efeitos_sp_original[[i]][2,],
             coefReg1 = coefic_original[[i]][1,],
             coefReg2 = coefic_original[[i]][2,],
             #int = intercept[[i]],
             nocc = nocc_original[[i]],
             det = det_prob_original [[i]],
             bpv = bpv.selected_original[[i]],
             which_model=mod_sel_original[[i]]))


## nomes das sp de coral
names(tab_summarized_original) <- sp_coral

## occupancy
tab_summarized_occupancy <- lapply(seq(1,length(tab_efeitos_sp_occupancy)), function(i)
  data.frame(sp=especie, 
             effectReg1 = tab_efeitos_sp_occupancy[[i]][1,],
             effectReg2 = tab_efeitos_sp_occupancy[[i]][2,],
             coefReg1 = coefic_occupancy[[i]][1,],
             coefReg2 = coefic_occupancy[[i]][2,],
             #int = intercept[[i]],
             nocc = nocc_occupancy[[i]],
             det = det_prob_occupancy [[i]],
             bpv = bpv.selected_occupancy[[i]],
             which_model=mod_sel_occupancy[[i]])
)

## nomes das sp de coral
names(tab_summarized_occupancy) <- sp_coral
## effect (regression coefficient for each region)
## original cover
efeito_para_bipartite_original_reg1 <- sapply (tab_summarized_original, "[[","coefReg1")
efeito_para_bipartite_original_reg2 <- sapply (tab_summarized_original, "[[","coefReg2")
## setting rownames
rownames(efeito_para_bipartite_original_reg1)<- especie
rownames(efeito_para_bipartite_original_reg2)<- especie

## site occupancy
efeito_para_bipartite_occupancy_reg1 <-sapply (tab_summarized_occupancy, "[[","coefReg1")
efeito_para_bipartite_occupancy_reg2 <-sapply (tab_summarized_occupancy, "[[","coefReg2")
## setting rownames
rownames(efeito_para_bipartite_occupancy_reg1)<- especie
rownames(efeito_para_bipartite_occupancy_reg2)<- especie

## significance of the effect
## original cover
significance_original_reg1 <- sapply (tab_summarized_original, "[[","effectReg1")
significance_original_reg2 <- sapply (tab_summarized_original, "[[","effectReg2")
## transforming No and yes into 1 (significant effect) and 0 (no effect)
significance_original_reg1 <- ifelse (significance_original_reg1 =="Yes",1,0)
significance_original_reg2 <- ifelse (significance_original_reg2 =="Yes",1,0)
## setting rownames
rownames(significance_original_reg1 )<- especie
rownames(significance_original_reg2)<- especie

## site occupancy
significance_occupancy_reg1 <-sapply (tab_summarized_occupancy, "[[","effectReg1")
significance_occupancy_reg2 <-sapply (tab_summarized_occupancy, "[[","effectReg2")
## transforming No and yes into 1 (significant effect) and 0 (no effect)
significance_occupancy_reg1 <- ifelse (significance_occupancy_reg1 =="Yes",1,0)
significance_occupancy_reg2 <- ifelse (significance_occupancy_reg2 =="Yes",1,0)
## setting rownames
rownames(significance_occupancy_reg1)<- especie
rownames(significance_occupancy_reg2)<- especie

save(efeito_para_bipartite_original_reg1 ,
     efeito_para_bipartite_original_reg2 ,
     efeito_para_bipartite_occupancy_reg1 ,
     efeito_para_bipartite_occupancy_reg2 ,
     significance_original_reg1 ,
     significance_original_reg2 ,
     significance_occupancy_reg1 ,
     significance_occupancy_reg2,
     file=here("output","coefficients_ORIGINAL.RData")) 

###### LOSS OF 25% OF COVER OR OCCUPANCY PROBABILITY

## LOAD MODEL RESULTS

load(here("output","samples_OCCcoral_PdepthObsID.RData")) 
res1_orig25 <- samples_OCCcoral_PdepthObsID$cob_desc25
res1_occ25 <- samples_OCCcoral_PdepthObsID$ocupacao_desc25

rm(samples_OCCcoral_PdepthObsID)
gc()
load(here("output","samples_OCCcoral_PdepthObsIDRndm.RData"))
res2_orig25 <- samples_OCCcoral_PdepthObsIDRndm$cob_desc25
res2_occ25 <- samples_OCCcoral_PdepthObsIDRndm$ocupacao_desc25
rm(samples_OCCcoral_PdepthObsIDRndm)
gc()

load(here("output","samples_OCCcoralDepth_PObsIDRndm.RData"))
res3_orig25 <- samples_OCCcoralDepth_PObsIDRndm$cob_desc25
res3_occ25 <- samples_OCCcoralDepth_PObsIDRndm$ocupacao_desc25
rm(samples_OCCcoralDepth_PObsIDRndm)
gc()

load(here("output","StaticModelOccCoral_IDobsRdmP.RData"))
res4_orig25 <- StaticModelOccCoral_IDobsRdmP$cob_desc25
res4_occ25 <- StaticModelOccCoral_IDobsRdmP$ocupacao_desc25
rm(StaticModelOccCoral_IDobsRdmP)
gc()

##

## Creating a list with the results of each model
lista_modelos_original25 <- list(res1_orig25,
                                 res2_orig25,
                                 res3_orig25,
                                 res4_orig25)
lista_modelos_occupancy25 <- list(res1_occ25,
                                  res2_occ25,
                                  res3_occ25,
                                  res4_occ25)

# nomear a lista de resultados
lista_modelos_original25 <- lapply (lista_modelos_original25, function (i) {names (i) <- sp_coral; i})
lista_modelos_occupancy25 <- lapply (lista_modelos_occupancy25, function (i) {names (i) <- sp_coral; i})

## table of bayesian P value for models with original coral cover
bvclosed_original25 <- lapply (lista_modelos_original25, function (model)
  
  lapply (model, function (i)
    
    unlist(lapply (i, function (k)
      
      sum (k$sims.list$Chi2repClosed > k$sims.list$Chi2Closed)/
        length(k$sims.list$Chi2repClosed)
    )
    )
  )
)
## table of bayesian P value for models with estimated site occupancy
bvclosed_occupancy25 <- lapply (lista_modelos_occupancy25, function (model)
  
  lapply (model, function (i)
    
    unlist(lapply (i, function (k)
      
      sum (k$sims.list$Chi2repClosed > k$sims.list$Chi2Closed)/
        length(k$sims.list$Chi2repClosed)
    )
    )
  )
)
# nomear a lista de resultados
bvclosed_original25 <- lapply (bvclosed_original25, function (i) {names (i) <- sp_coral; i})
bvclosed_occupancy25 <- lapply (bvclosed_occupancy25, function (i) {names (i) <- sp_coral; i})

# reorganizing the list
tab_bpv_sp_original25 <- lapply (sp_coral, function (sp)
  lapply (bvclosed_original25, function (i) 
    i [ which (names(i) == sp)])
)
tab_bpv_sp_occupancy25 <- lapply (sp_coral, function (sp)
  lapply (bvclosed_occupancy25, function (i) 
    i [ which (names(i) == sp)])
)

## transforming into matrix
tab_bpv_sp_original25 <- lapply (tab_bpv_sp_original25, function (i) 
  data.frame (matrix(unlist(i),ncol= length(bvclosed_original25),byrow = F )))
names(tab_bpv_sp_original25)<- sp_coral

tab_bpv_sp_occupancy25 <- lapply (tab_bpv_sp_occupancy25, function (i) 
  data.frame (matrix(unlist(i),ncol= length(bvclosed_occupancy25),byrow = F )))
names(tab_bpv_sp_occupancy25)<- sp_coral

## selecao do melhor modelo por especie 
## de acordo com o  BPV mais proximo a 0.5 (using the function 'closest')

# model with BPV closest to 0.5 (better fit to data)
# original cover
mod_sel_original25 <- lapply (seq (1,length(tab_bpv_sp_original25)), function (i) # sp coral - lista
  
  do.call(rbind, lapply (seq (1,nrow (tab_bpv_sp_original25 [[i]])), function (k) { # sp peixe - linhas
    
    
    ## obter o valor de bpv mais proxima a 0.5
    closestbpv <-as.numeric(closest (tab_bpv_sp_original25[[i]][k,],0.5)[1])
    
    ## no caso de BPV=0 em todos, pega o primeiro modelo que eh o mais parcimonioso (menos parametros)
    qual.melhor <- ifelse (closestbpv == 0, 1, which(tab_bpv_sp_original25[[i]][k,] == closestbpv)) 
    
    
  }
  )
  )
)

names(mod_sel_original25)<- sp_coral
# site-occupancy probability
mod_sel_occupancy25 <- lapply (seq (1,length(tab_bpv_sp_occupancy25)), function (i) # sp coral - lista
  
  do.call(rbind, lapply (seq (1,nrow (tab_bpv_sp_occupancy25 [[i]])), function (k) { # sp peixe - linhas
    
    
    ## obter o valor de bpv mais proxima a 0.5
    closestbpv <-as.numeric(closest (tab_bpv_sp_occupancy25[[i]][k,],0.5)[1])
    
    ## no caso de BPV=0 em todos, pega o primeiro modelo que eh o mais parcimonioso (menos parametros)
    qual.melhor <- ifelse (closestbpv == 0, 1, which(tab_bpv_sp_occupancy25[[i]][k,] == closestbpv)) 
    
    
  }
  )
  )
)

names(mod_sel_occupancy25)<- sp_coral

## selecting models
# original
selected.models_original25 <- lapply (seq (1,length (lista_modelos_original25 [[1]])), function (k)
  lapply (seq (1,length (lista_modelos_original25 [[1]][[1]])), function (i) {
    
    sp.coral <-  k
    sp.peixe <-  i
    qual.sel <- mod_sel_original25 [[sp.coral]][sp.peixe,]
    
    
    selected.models <- (lista_modelos_original25 [[qual.sel]] # ## seleciona o modelo mod, 
                        [[sp.coral]] ## da sp de coral k
                        [[sp.peixe]]) ## sp de peixe i
    ; selected.models
    
  }))

names(selected.models_original25) <- sp_coral

# occupancy
selected.models_occupancy25 <- lapply (seq (1,length (lista_modelos_occupancy25 [[1]])), function (k)
  lapply (seq (1,length (lista_modelos_occupancy25 [[1]][[1]])), function (i) {
    
    sp.coral <-  k
    sp.peixe <-  i
    qual.sel <- mod_sel_occupancy25 [[sp.coral]][sp.peixe,]
    
    
    selected.models <- (lista_modelos_occupancy25 [[qual.sel]] # ## seleciona o modelo mod, 
                        [[sp.coral]] ## da sp de coral k
                        [[sp.peixe]]) ## sp de peixe i
    ; selected.models
    
  }))

names(selected.models_occupancy25) <- sp_coral

## de agora em diante, trabalhar somente com os modelos selecionados

## efeitos original
efeitos_original25 <-  lapply (selected.models_original25, function (i) 
  
  do.call(cbind,lapply (i, function (k)
    
    k$summary [grep("beta1",rownames(k$summary)),"overlap0"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)

tab_efeitos_sp_original25 <- lapply (efeitos_original25, function (i)
  ifelse (i == 1, "No", "Yes")
)

# efeitos occupancy
efeitos_occupancy25 <-  lapply (selected.models_occupancy25, function (i) 
  
  do.call (cbind, lapply (i, function (k)
    
    k$summary [grep("beta1",rownames(k$summary)),"overlap0"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)

tab_efeitos_sp_occupancy25 <- lapply (efeitos_occupancy25, function (i)
  ifelse (i == 1, "No", "Yes")
)

#### coeficientes de regressao
# original cover
coefic_original25 <-  lapply (selected.models_original25, function (i) 
  
  do.call (cbind,lapply (i, function (k)
    
    k$summary [grep("beta1",rownames(k$summary)),"mean"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)
# site-occupancy
coefic_occupancy25 <-  lapply (selected.models_occupancy25, function (i) 
  
  do.call (cbind,lapply (i, function (k)
    
    k$summary [grep("beta1",rownames(k$summary)),"mean"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)


## finite sample size (n.occ)
#### original cover
nocc_original25 <- lapply (selected.models_original25, function (i) 
  
  unlist (lapply (i, function (k)
    
    k$summary [grep("n.occ",rownames(k$summary)),"mean"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)

#### site-occupancy
nocc_occupancy25 <- lapply (selected.models_occupancy25, function (i) 
  
  unlist (lapply (i, function (k)
    
    k$summary [grep("n.occ",rownames(k$summary)),"mean"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)

cbind(nocc_original25[[1]],nocc_occupancy25[[1]])

######## detection probability
det_prob_original25 <-  lapply (selected.models_original25, function (i) 
  
  unlist (lapply (i, function (k)
    
    k$summary [grep("mean.p",rownames(k$summary)),"mean"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)

det_prob_occupancy25 <-  lapply (selected.models_occupancy25, function (i) 
  
  unlist (lapply (i, function (k)
    
    k$summary [grep("mean.p",rownames(k$summary)),"mean"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)

cbind(det_prob_original25[[1]],det_prob_occupancy25[[1]])

## bayesian p-value dos selecionados
# original
bpv.selected_original25 <- lapply (selected.models_original25, function (i)
  
  unlist(lapply (i, function (k)
    
    sum (k$sims.list$Chi2repClosed > k$sims.list$Chi2Closed)/
      length(k$sims.list$Chi2repClosed)
  )
  )
)

# occupancy
bpv.selected_occupancy25 <- lapply (selected.models_occupancy25, function (i)
  
  unlist(lapply (i, function (k)
    
    sum (k$sims.list$Chi2repClosed > k$sims.list$Chi2Closed)/
      length(k$sims.list$Chi2repClosed)
  )
  )
)

cbind(bpv.selected_original25[[1]], bpv.selected_occupancy25[[1]])

## tabela com as info do efeito das covs de coral nas spp
tab_summarized_original25 <- lapply(seq(1,length(tab_efeitos_sp_original25)), function(i)
  data.frame(sp=especie, 
             effectReg1 = tab_efeitos_sp_original25[[i]][1,],
             effectReg2 = tab_efeitos_sp_original25[[i]][2,],
             coefReg1 = coefic_original25[[i]][1,],
             coefReg2 = coefic_original25[[i]][2,],
             #int = intercept[[i]],
             nocc = nocc_original25[[i]],
             det = det_prob_original25 [[i]],
             bpv = bpv.selected_original25[[i]],
             which_model=mod_sel_original25[[i]]))


## nomes das sp de coral
names(tab_summarized_original25) <- sp_coral

## occupancy
tab_summarized_occupancy25 <- lapply(seq(1,length(tab_efeitos_sp_occupancy25)), function(i)
  data.frame(sp=especie, 
             effectReg1 = tab_efeitos_sp_occupancy25[[i]][1,],
             effectReg2 = tab_efeitos_sp_occupancy25[[i]][2,],
             coefReg1 = coefic_occupancy25[[i]][1,],
             coefReg2 = coefic_occupancy25[[i]][2,],
             #int = intercept[[i]],
             nocc = nocc_occupancy25[[i]],
             det = det_prob_occupancy25 [[i]],
             bpv = bpv.selected_occupancy25[[i]],
             which_model=mod_sel_occupancy25[[i]])
)

## nomes das sp de coral
names(tab_summarized_occupancy25) <- sp_coral
## effect (regression coefficient for each region)
## original cover
efeito_para_bipartite_original25_reg1 <- sapply (tab_summarized_original25, "[[","coefReg1")
efeito_para_bipartite_original25_reg2 <- sapply (tab_summarized_original25, "[[","coefReg2")
## setting rownames
rownames(efeito_para_bipartite_original25_reg1)<- especie
rownames(efeito_para_bipartite_original25_reg2)<- especie

## site occupancy
efeito_para_bipartite_occupancy25_reg1 <-sapply (tab_summarized_occupancy25, "[[","coefReg1")
efeito_para_bipartite_occupancy25_reg2 <-sapply (tab_summarized_occupancy25, "[[","coefReg2")
## setting rownames
rownames(efeito_para_bipartite_occupancy25_reg1)<- especie
rownames(efeito_para_bipartite_occupancy25_reg2)<- especie

## significance of the effect
## original cover
significance_original25_reg1 <- sapply (tab_summarized_original25, "[[","effectReg1")
significance_original25_reg2 <- sapply (tab_summarized_original25, "[[","effectReg2")
## transforming No and yes into 1 (significant effect) and 0 (no effect)
significance_original25_reg1 <- ifelse (significance_original25_reg1 =="Yes",1,0)
significance_original25_reg2 <- ifelse (significance_original25_reg2 =="Yes",1,0)
## setting rownames
rownames(significance_original25_reg1 )<- especie
rownames(significance_original25_reg2)<- especie

## site occupancy
significance_occupancy25_reg1 <-sapply (tab_summarized_occupancy25, "[[","effectReg1")
significance_occupancy25_reg2 <-sapply (tab_summarized_occupancy25, "[[","effectReg2")
## transforming No and yes into 1 (significant effect) and 0 (no effect)
significance_occupancy25_reg1 <- ifelse (significance_occupancy25_reg1 =="Yes",1,0)
significance_occupancy25_reg2 <- ifelse (significance_occupancy25_reg2 =="Yes",1,0)
## setting rownames
rownames(significance_occupancy25_reg1)<- especie
rownames(significance_occupancy25_reg2)<- especie

## saving
save(efeito_para_bipartite_original25_reg1 ,
     efeito_para_bipartite_original25_reg2 ,
     efeito_para_bipartite_occupancy25_reg1 ,
     efeito_para_bipartite_occupancy25_reg2 ,
     significance_original25_reg1 ,
     significance_original25_reg2 ,
     significance_occupancy25_reg1 ,
     significance_occupancy25_reg2,
     file=here("output","coefficients_DISCOUNTING25.RData")) 


###### LOSS OF 50% OF COVER OR OCCUPANCY PROBABILITY

## LOAD MODEL RESULTS

load(here("output","samples_OCCcoral_PdepthObsID.RData")) 
res1_orig50 <- samples_OCCcoral_PdepthObsID$cob_desc50
res1_occ50 <- samples_OCCcoral_PdepthObsID$ocupacao_desc50
rm(samples_OCCcoral_PdepthObsID)
gc()
load(here("output","samples_OCCcoral_PdepthObsIDRndm.RData"))
res2_orig50 <- samples_OCCcoral_PdepthObsIDRndm$cob_desc50
res2_occ50 <- samples_OCCcoral_PdepthObsIDRndm$ocupacao_desc50
rm(samples_OCCcoral_PdepthObsIDRndm)
gc()

load(here("output","samples_OCCcoralDepth_PObsIDRndm.RData"))
res3_orig50 <- samples_OCCcoralDepth_PObsIDRndm$cob_desc50
res3_occ50 <- samples_OCCcoralDepth_PObsIDRndm$ocupacao_desc50
rm(samples_OCCcoralDepth_PObsIDRndm)
gc()

load(here("output","StaticModelOccCoral_IDobsRdmP.RData"))
res4_orig50 <- StaticModelOccCoral_IDobsRdmP$cob_desc50
res4_occ50 <- StaticModelOccCoral_IDobsRdmP$ocupacao_desc50
rm(StaticModelOccCoral_IDobsRdmP)
gc()

##

## Creating a list with the results of each model
lista_modelos_original50 <- list(res1_orig50,
                                 res2_orig50,
                                 res3_orig50,
                                 res4_orig50)
lista_modelos_occupancy50 <- list(res1_occ50,
                                  res2_occ50,
                                  res3_occ50,
                                  res4_occ50)

# nomear a lista de resultados
lista_modelos_original50 <- lapply (lista_modelos_original50, function (i) {names (i) <- sp_coral; i})
lista_modelos_occupancy50 <- lapply (lista_modelos_occupancy50, function (i) {names (i) <- sp_coral; i})

## table of bayesian P value for models with original coral cover
bvclosed_original50 <- lapply (lista_modelos_original50, function (model)
  
  lapply (model, function (i)
    
    unlist(lapply (i, function (k)
      
      sum (k$sims.list$Chi2repClosed > k$sims.list$Chi2Closed)/
        length(k$sims.list$Chi2repClosed)
    )
    )
  )
)
## table of bayesian P value for models with estimated site occupancy
bvclosed_occupancy50 <- lapply (lista_modelos_occupancy50, function (model)
  
  lapply (model, function (i)
    
    unlist(lapply (i, function (k)
      
      sum (k$sims.list$Chi2repClosed > k$sims.list$Chi2Closed)/
        length(k$sims.list$Chi2repClosed)
    )
    )
  )
)
# nomear a lista de resultados
bvclosed_original50 <- lapply (bvclosed_original50, function (i) {names (i) <- sp_coral; i})
bvclosed_occupancy50 <- lapply (bvclosed_occupancy50, function (i) {names (i) <- sp_coral; i})

# reorganizing the list
tab_bpv_sp_original50 <- lapply (sp_coral, function (sp)
  lapply (bvclosed_original50, function (i) 
    i [ which (names(i) == sp)])
)
tab_bpv_sp_occupancy50 <- lapply (sp_coral, function (sp)
  lapply (bvclosed_occupancy50, function (i) 
    i [ which (names(i) == sp)])
)

## transforming into matrix
tab_bpv_sp_original50 <- lapply (tab_bpv_sp_original50, function (i) 
  data.frame (matrix(unlist(i),ncol= length(bvclosed_original50),byrow = F )))
names(tab_bpv_sp_original50)<- sp_coral

tab_bpv_sp_occupancy50 <- lapply (tab_bpv_sp_occupancy50, function (i) 
  data.frame (matrix(unlist(i),ncol= length(bvclosed_occupancy50),byrow = F )))
names(tab_bpv_sp_occupancy50)<- sp_coral

## selecao do melhor modelo por especie 
## de acordo com o  BPV mais proximo a 0.5 (using the function 'closest')

# model with BPV closest to 0.5 (better fit to data)
# original cover
mod_sel_original50 <- lapply (seq (1,length(tab_bpv_sp_original50)), function (i) # sp coral - lista
  
  do.call(rbind, lapply (seq (1,nrow (tab_bpv_sp_original50 [[i]])), function (k) { # sp peixe - linhas
    
    
    ## obter o valor de bpv mais proxima a 0.5
    closestbpv <-as.numeric(closest (tab_bpv_sp_original50[[i]][k,],0.5)[1])
    
    ## no caso de BPV=0 em todos, pega o primeiro modelo que eh o mais parcimonioso (menos parametros)
    qual.melhor <- ifelse (closestbpv == 0, 1, which(tab_bpv_sp_original50[[i]][k,] == closestbpv)) 
    
    
  }
  )
  )
)

names(mod_sel_original50)<- sp_coral
# site-occupancy probability
mod_sel_occupancy50 <- lapply (seq (1,length(tab_bpv_sp_occupancy50)), function (i) # sp coral - lista
  
  do.call(rbind, lapply (seq (1,nrow (tab_bpv_sp_occupancy50 [[i]])), function (k) { # sp peixe - linhas
    
    
    ## obter o valor de bpv mais proxima a 0.5
    closestbpv <-as.numeric(closest (tab_bpv_sp_occupancy50[[i]][k,],0.5)[1])
    
    ## no caso de BPV=0 em todos, pega o primeiro modelo que eh o mais parcimonioso (menos parametros)
    qual.melhor <- ifelse (closestbpv == 0, 1, which(tab_bpv_sp_occupancy50[[i]][k,] == closestbpv)) 
    
    
  }
  )
  )
)

names(mod_sel_occupancy50)<- sp_coral

## selecting models
# original
selected.models_original50 <- lapply (seq (1,length (lista_modelos_original50 [[1]])), function (k)
  lapply (seq (1,length (lista_modelos_original50 [[1]][[1]])), function (i) {
    
    sp.coral <-  k
    sp.peixe <-  i
    qual.sel <- mod_sel_original50 [[sp.coral]][sp.peixe,]
    
    
    selected.models <- (lista_modelos_original50 [[qual.sel]] # ## seleciona o modelo mod, 
                        [[sp.coral]] ## da sp de coral k
                        [[sp.peixe]]) ## sp de peixe i
    ; selected.models
    
  }))

names(selected.models_original50) <- sp_coral

# occupancy
selected.models_occupancy50 <- lapply (seq (1,length (lista_modelos_occupancy50 [[1]])), function (k)
  lapply (seq (1,length (lista_modelos_occupancy50 [[1]][[1]])), function (i) {
    
    sp.coral <-  k
    sp.peixe <-  i
    qual.sel <- mod_sel_occupancy50 [[sp.coral]][sp.peixe,]
    
    
    selected.models <- (lista_modelos_occupancy50 [[qual.sel]] # ## seleciona o modelo mod, 
                        [[sp.coral]] ## da sp de coral k
                        [[sp.peixe]]) ## sp de peixe i
    ; selected.models
    
  }))

names(selected.models_occupancy50) <- sp_coral

## de agora em diante, trabalhar somente com os modelos selecionados

## efeitos original
efeitos_original50 <-  lapply (selected.models_original50, function (i) 
  
  do.call(cbind,lapply (i, function (k)
    
    k$summary [grep("beta1",rownames(k$summary)),"overlap0"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)

tab_efeitos_sp_original50 <- lapply (efeitos_original50, function (i)
  ifelse (i == 1, "No", "Yes")
)

# efeitos occupancy
efeitos_occupancy50 <-  lapply (selected.models_occupancy50, function (i) 
  
  do.call (cbind, lapply (i, function (k)
    
    k$summary [grep("beta1",rownames(k$summary)),"overlap0"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)

tab_efeitos_sp_occupancy50 <- lapply (efeitos_occupancy50, function (i)
  ifelse (i == 1, "No", "Yes")
)

#### coeficientes de regressao
# original cover
coefic_original50 <-  lapply (selected.models_original50, function (i) 
  
  do.call (cbind,lapply (i, function (k)
    
    k$summary [grep("beta1",rownames(k$summary)),"mean"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)
# site-occupancy
coefic_occupancy50 <-  lapply (selected.models_occupancy50, function (i) 
  
  do.call (cbind,lapply (i, function (k)
    
    k$summary [grep("beta1",rownames(k$summary)),"mean"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)


## finite sample size (n.occ)
#### original cover
nocc_original50 <- lapply (selected.models_original50, function (i) 
  
  unlist (lapply (i, function (k)
    
    k$summary [grep("n.occ",rownames(k$summary)),"mean"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)

#### site-occupancy
nocc_occupancy50 <- lapply (selected.models_occupancy50, function (i) 
  
  unlist (lapply (i, function (k)
    
    k$summary [grep("n.occ",rownames(k$summary)),"mean"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)

cbind(nocc_original50[[1]],nocc_occupancy50[[1]])

######## detection probability
det_prob_original50 <-  lapply (selected.models_original50, function (i) 
  
  unlist (lapply (i, function (k)
    
    k$summary [grep("mean.p",rownames(k$summary)),"mean"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)

det_prob_occupancy50 <-  lapply (selected.models_occupancy50, function (i) 
  
  unlist (lapply (i, function (k)
    
    k$summary [grep("mean.p",rownames(k$summary)),"mean"])) ## pegar o subset com efeito significativo
  ## extrair os betas e se ou nao sobrepoe 0
)

cbind(det_prob_original50[[1]],det_prob_occupancy50[[1]])

## bayesian p-value dos selecionados
# original
bpv.selected_original50 <- lapply (selected.models_original50, function (i)
  
  unlist(lapply (i, function (k)
    
    sum (k$sims.list$Chi2repClosed > k$sims.list$Chi2Closed)/
      length(k$sims.list$Chi2repClosed)
  )
  )
)

# occupancy
bpv.selected_occupancy50 <- lapply (selected.models_occupancy50, function (i)
  
  unlist(lapply (i, function (k)
    
    sum (k$sims.list$Chi2repClosed > k$sims.list$Chi2Closed)/
      length(k$sims.list$Chi2repClosed)
  )
  )
)

cbind(bpv.selected_original50[[1]], bpv.selected_occupancy50[[1]])

## tabela com as info do efeito das covs de coral nas spp
tab_summarized_original50 <- lapply(seq(1,length(tab_efeitos_sp_original50)), function(i)
  data.frame(sp=especie, 
             effectReg1 = tab_efeitos_sp_original50[[i]][1,],
             effectReg2 = tab_efeitos_sp_original50[[i]][2,],
             coefReg1 = coefic_original50[[i]][1,],
             coefReg2 = coefic_original50[[i]][2,],
             #int = intercept[[i]],
             nocc = nocc_original50[[i]],
             det = det_prob_original50 [[i]],
             bpv = bpv.selected_original50[[i]],
             which_model=mod_sel_original50[[i]]))


## nomes das sp de coral
names(tab_summarized_original50) <- sp_coral

## occupancy
tab_summarized_occupancy50 <- lapply(seq(1,length(tab_efeitos_sp_occupancy50)), function(i)
  data.frame(sp=especie, 
             effectReg1 = tab_efeitos_sp_occupancy50[[i]][1,],
             effectReg2 = tab_efeitos_sp_occupancy50[[i]][2,],
             coefReg1 = coefic_occupancy50[[i]][1,],
             coefReg2 = coefic_occupancy50[[i]][2,],
             #int = intercept[[i]],
             nocc = nocc_occupancy50[[i]],
             det = det_prob_occupancy50 [[i]],
             bpv = bpv.selected_occupancy50[[i]],
             which_model=mod_sel_occupancy50[[i]])
)

## nomes das sp de coral
names(tab_summarized_occupancy50) <- sp_coral
## effect (regression coefficient for each region)
## original cover
efeito_para_bipartite_original50_reg1 <- sapply (tab_summarized_original50, "[[","coefReg1")
efeito_para_bipartite_original50_reg2 <- sapply (tab_summarized_original50, "[[","coefReg2")
## setting rownames
rownames(efeito_para_bipartite_original50_reg1)<- especie
rownames(efeito_para_bipartite_original50_reg2)<- especie

## site occupancy
efeito_para_bipartite_occupancy50_reg1 <-sapply (tab_summarized_occupancy50, "[[","coefReg1")
efeito_para_bipartite_occupancy50_reg2 <-sapply (tab_summarized_occupancy50, "[[","coefReg2")
## setting rownames
rownames(efeito_para_bipartite_occupancy50_reg1)<- especie
rownames(efeito_para_bipartite_occupancy50_reg2)<- especie

## significance of the effect
## original cover
significance_original50_reg1 <- sapply (tab_summarized_original50, "[[","effectReg1")
significance_original50_reg2 <- sapply (tab_summarized_original50, "[[","effectReg2")
## transforming No and yes into 1 (significant effect) and 0 (no effect)
significance_original50_reg1 <- ifelse (significance_original50_reg1 =="Yes",1,0)
significance_original50_reg2 <- ifelse (significance_original50_reg2 =="Yes",1,0)
## setting rownames
rownames(significance_original50_reg1 )<- especie
rownames(significance_original50_reg2)<- especie

## site occupancy
significance_occupancy50_reg1 <-sapply (tab_summarized_occupancy50, "[[","effectReg1")
significance_occupancy50_reg2 <-sapply (tab_summarized_occupancy50, "[[","effectReg2")
## transforming No and yes into 1 (significant effect) and 0 (no effect)
significance_occupancy50_reg1 <- ifelse (significance_occupancy50_reg1 =="Yes",1,0)
significance_occupancy50_reg2 <- ifelse (significance_occupancy50_reg2 =="Yes",1,0)
## setting rownames
rownames(significance_occupancy50_reg1)<- especie
rownames(significance_occupancy50_reg2)<- especie

## saving
save(efeito_para_bipartite_original50_reg1 ,
     efeito_para_bipartite_original50_reg2 ,
     efeito_para_bipartite_occupancy50_reg1 ,
     efeito_para_bipartite_occupancy50_reg2 ,
     significance_original50_reg1 ,
     significance_original50_reg2 ,
     significance_occupancy50_reg1 ,
     significance_occupancy50_reg2,
     file=here("output","coefficients_DISCOUNTING50.RData")) 

