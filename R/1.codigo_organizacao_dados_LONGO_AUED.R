## analises para o manuscrito "Fish reliance on live coral cover in the Brazilian Province" 

## chamar os pacotes
source("R/packages.R")
source("R/functions.R")

## CODIGO PARA ORGANIZAÇÃO DOS DADOS DE PEIXES E BENTOS
## A IDEIA EH PRIMEIRO FAZER UM ARRANJO (ARRAY) DE SITIOS (LINHAS), OCASIOES AMOSTRAIS (TRANSECCOES OU VIDEOS),
## E ESPECIE (3D). AO FIM, A IDEIA EH FAZER DATA FRAMES EM FORMATO LONGO PARA OS MODELOS DE OCUPACAO DE SITIOS, DE 
## MODO A REMOVER OS NAs E TRABALHAR SOMENTE COM AS OBSERVACOES

## AQUI, A PRIMEIRA COISA FEITA FOI ARRUMAR OS DADOS DE LONGO
## isto esta desativado porque estas mudancas foram consolidada
### dados dos peixes (Longo et al)
# L.peixes <- read.csv(here("data","detection","occ_Longo_et_al","Data_Trophic_Interactions_WAtlantic_GLongo_clean.csv"),
#                     header = T)
# 
# ## adequar os nomes das localidades e sitios da base de Longo, de acordo com Aued
# # localidades
# L.peixes$location [which (L.peixes$location == "alagoas")] <- "costa_corais"
# L.peixes$location [which (L.peixes$location == "pernambuco")] <- "costa_corais"
# L.peixes$location [which (L.peixes$location == "atol_das_rocas")] <- "rocas"
# L.peixes$location [which (L.peixes$location == "guarapari")] <- "espirito_santo"
# L.peixes$location [which (L.peixes$location == "arraial_do_cabo")] <- "arraial"
# L.peixes$location [which (L.peixes$location == "sao_paulo")] <- "ilhabela"
# L.peixes$location [which (L.peixes$location == "parcel_manoel_luis")] <- "manuel_luis"
# # separar as localidades do rio grande do norte
# L.peixes$location [which (L.peixes$site == "batente_das_agulhas")] <- "rgnor_norte"
# L.peixes$location [which (L.peixes$site == "pedra_do_silva")] <- "rgnor_norte"
# L.peixes$location [which (L.peixes$site == "barreirinha")] <- "rgnor_norte"
# L.peixes$location [which (L.peixes$site == "cabeco_do_leandro")] <- "rgnor_norte"
# L.peixes$location [which (L.peixes$site == "maracajau")] <- "rgnor_parrachos"
# L.peixes$location [which (L.peixes$site == "parrachos")] <- "rgnor_parrachos"
# # separar os sitios de santa catarina
# L.peixes$location [which (L.peixes$site == "deserta")] <- "ilhasc_norte"
# L.peixes$location [which (L.peixes$site == "saco_d'agua")] <- "ilhasc_norte"
# L.peixes$location [which (L.peixes$site == "baia_da_tartaruga")] <- "ilhasc_norte"
# L.peixes$location [which (L.peixes$site == "saco_do_vidal")] <- "ilhasc_norte"
# L.peixes$location [which (L.peixes$site == "engenho")] <- "ilhasc_norte"
# L.peixes$location [which (L.peixes$site == "xavier")] <- "ilhasc_sul"
# 
# ### organizar os sitios
# L.peixes$site [which(L.peixes$site == "anacris")] <- "ana_cristina"
# L.peixes$site [which(L.peixes$site == "parrachos")] <- 'parrachos_de_rio_do_fogo'
# L.peixes$site [which(L.peixes$site == "rocas")] <- 'piscina_das_rocas'
# L.peixes$site [which(L.peixes$site == "chapeiroes")] <- 'chapeirao'
# L.peixes$site [which(L.peixes$site == "ilha_escalvada")] <- 'escalvada'
# L.peixes$site [which(L.peixes$site == "ilha_rasa")] <- 'ilhas_rasas'
# L.peixes$site [which(L.peixes$site == "porcos")] <- 'porcos_oeste'
# L.peixes$site [which(L.peixes$site == "saco_do_sombrio_")] <- 'saco_do_sombrio'
# L.peixes$site [which(L.peixes$site == "cabras")] <- 'ilha_das_cabras'
# L.peixes$site [which(L.peixes$site == "ponta_do_diogo")] <- 'saco_do_diogo'
# L.peixes$site [which(L.peixes$site == "xavier")] <- 'xavier_ponta_sul'
# L.peixes$site [which(L.peixes$site == "deserta")] <- 'deserta_norte'
# L.peixes$site [which(L.peixes$site == "saco_d'agua")] <- 'arvoredo_saco_dagua'
# L.peixes$site [which(L.peixes$site == "barra_gales")] <- 'barra_da_gale'
# 
# ## modificar a profundidade 
# L.peixes$depth_m <- ifelse (L.peixes$depth_m >= 8, "fundo","raso")
# 
# ## add region
# L.peixes$Region <- bentos$Region [match(L.peixes$location, bentos$Locality)]
# 
# ## fazer eventID para este estudo
# L.peixes$eventID_MOD <- paste (L.peixes$Region,
#                                L.peixes$location,
#                                L.peixes$site,
#                                L.peixes$depth_m,
#                                sep=".")
# 
# ## OBTER A ID DE TODAS AS ESPECIES DE PEIXES ENCONTRADAS POR Longo
# todas_sp_Longo <-  (L.peixes$species_code)
# 
# # corresponder siglas de Longo com nomes completos de Quimbayo
# traits_peixes <- read.csv(here("data","traits","Atributos_especies_Atlantico_&_Pacifico_Oriental_2020_04_28.csv"),
#                           h=T,sep=";")
# split_names_JPQ <- lapply (strsplit(traits_peixes$Name," ",fixed=F), substr, 1,3)
# split_names_JPQ <- lapply (split_names_JPQ,tolower)
# siglas_JPQ <- unlist(lapply (split_names_JPQ, function (i) paste(i[1],i[2],sep="_")))
# 
# ## inserir uma tabela em Longo, com o nome completo das spp
# L.peixes$ScientificName <- traits_peixes$Name [match(todas_sp_Longo,siglas_JPQ)]
# 
# # encontrar quais especies estao em longo, mas nao estao em Morais
# unique (todas_sp_Longo [which(todas_sp_Longo %in% siglas_JPQ == F)])
# 
# ## ajustar spp.
# L.peixes$ScientificName [which(L.peixes$species_code == "spa_sp")] <- "Sparisoma_sp"
# L.peixes$ScientificName [which(L.peixes$species_code == "ocy_cry")] <- "Ocyurus_chrysurus"
# L.peixes$ScientificName [which(L.peixes$species_code == "ni")] <- "Not_identified"
# L.peixes$ScientificName [which(L.peixes$species_code == "kyp_sp")] <- "Kyphosus_sp"
# L.peixes$ScientificName [which(L.peixes$species_code == "euc_lef")] <- "Eucinostomus_lefroyi"
# L.peixes$ScientificName [which(L.peixes$species_code == "lut_ale")] <- "Lutjanus_alexandrei"
# L.peixes$ScientificName [which(L.peixes$species_code == "lut_sp")] <- "Lutjanus_sp"
# L.peixes$ScientificName [which(L.peixes$species_code == "aca_sp")] <- "Acanthurus_sp"
# L.peixes$ScientificName [which(L.peixes$species_code == "car_sp")] <- "Caranx_sp1"
# L.peixes$ScientificName [which(L.peixes$species_code == "acanthuridae")] <- "Not_identified"
# L.peixes$ScientificName [which(L.peixes$species_code == "dio_his")] <- "Diodon_hystrix"
# L.peixes$ScientificName [which(L.peixes$species_code == "hal_sp")] <- "Halichoeres_sp"
# L.peixes$ScientificName [which(L.peixes$species_code == "sca_sp")] <- "Scarus_sp"
# L.peixes$ScientificName [which(L.peixes$species_code == "blenideo")] <- "Not_identified"
# L.peixes$ScientificName [which(L.peixes$species_code == "das_mar")] <- "Hypanus_marianae"
# L.peixes$ScientificName [which(L.peixes$species_code == "lab_sp")] <- "Labrisomus_sp"
# L.peixes$ScientificName [which(L.peixes$species_code == "mal_sp")] <- "Malacoctenus_sp"
# L.peixes$ScientificName [which(L.peixes$species_code == "bot_sp")] <- "Bothus_sp"
# L.peixes$ScientificName [which(L.peixes$species_code == "carangideo")] <- "Caranx_sp2"
# L.peixes$ScientificName [which(L.peixes$species_code == "syn_sp")] <- "Synodus_sp"
# L.peixes$ScientificName [which(L.peixes$species_code == "manjuba")] <- "Anchoviella_lepidentostole"
# L.peixes$ScientificName [which(L.peixes$species_code == "sphyraena_borealis?")] <- "Sphyraena_borealis"
# L.peixes$ScientificName [which(L.peixes$species_code == "myc_sp")] <- "Mycteroperca_sp"
# L.peixes$ScientificName [which(L.peixes$species_code == "scomberomorus")] <- "Scomberomorus_sp"
# L.peixes$ScientificName [which(L.peixes$species_code == "sca_coel")] <- "Scarus_coelestinus"
# L.peixes$ScientificName [which(L.peixes$species_code == "carangidae")] <- "Caranx_sp3"
# L.peixes$ScientificName [which(L.peixes$species_code == "epi_niv")] <- "Epinephelus_niveatus"
# L.peixes$ScientificName [which(L.peixes$species_code == "epi_cru")] <- "Epinephelus_cruentatus"
# L.peixes$ScientificName [which(L.peixes$species_code == "neg_bre")] <- "Negaprion_brevirostris"
# L.peixes$ScientificName [which(L.peixes$species_code == "gin_cir")] <- "Ginglymostoma_cirratum"
# L.peixes$ScientificName [which(L.peixes$species_code == "par_sp")] <- "Parablennius_sp"
# L.peixes$ScientificName [which(L.peixes$species_code == "aet_nar")] <- "Aetobatus_narinari"             
# L.peixes$ScientificName [which(L.peixes$species_code == "sco_sp")] <- "Scorpaena_sp"
# L.peixes$ScientificName [which(L.peixes$species_code == "hae_sp")] <- "Haemulon_sp"
# L.peixes$ScientificName [which(L.peixes$species_code == "das_ame")] <- "Hypanus_americana"            
# L.peixes$ScientificName [which(L.peixes$species_code == "sph_sp")] <- "Sphoeroides_sp"
# L.peixes$ScientificName [which(L.peixes$species_code ==  "car_plu")] <- "Caranx_plumbeus"     
# L.peixes$ScientificName [which(L.peixes$species_code ==  "lut_moh")] <- "Lutjanus_mohogani"             
# L.peixes$ScientificName [which(L.peixes$species_code == "mir_jac")] <- "Myripristis_jacobus"
# L.peixes$ScientificName [which(L.peixes$species_code == "pem_sco")] <- "Pempheris_schomburgkii"
# L.peixes$ScientificName [which(L.peixes$species_code == "das_sp")] <- "Dasyatis_sp"
# # 
# # L.peixes$site [which(L.peixes$species_code == "het_pri")]
 
# write.csv (L.peixes, file =here("data","detection","occ_Longo_et_al","Data_Trophic_Interactions_WAtlantic_GLongo_clean_UPDATED_ALLB.csv"))
#############################################################################
#############################################################################
### lista dos locais com cobertura de coral, segundo Aued et al. 2018 PLosOne
##############################################################################
##############################################################################

locais_corais <- c("rgnor_parrachos",
                   "rgnor_norte",
                   "rgnor_sul",
                   "costa_corais",
                   "btds_santos",
                   "abrolhos",
                   "ceara",
                   "espirito_santo",
                   "arraial",
                   "ilhabela",
                   "alcatrazes",
                   "manuel_luis",
                   ## ilhas
                   "noronha",
                   "rocas",
                   "trindade")


## dados dos bentos
bentos <- read.xlsx(here("data","detection","Updated_compiled_quadrats_allsites.xlsx"),
                    sheet = 1, colNames = TRUE,detectDates=F)
## converter data em data - bug do pacote openxlsx
bentos$eventDate <-convertToDate(bentos$eventDate)

## Load new data from Longo
L.peixes<- read.xlsx(here("data","detection","occ_Longo_et_al","Data_Trophic_Interactions_WAtlantic_GLongo_clean_UPDATED_ALL.xlsx"),
                     sheet = 1, colNames = TRUE,detectDates=F)
## converter data em data - bug do pacote openxlsx
L.peixes$date <-convertToDate(L.peixes$date)
L.peixes$ScientificName <- tolower (L.peixes$ScientificName)

## subset dos sitios baseado na quantidade de coral
bentos <- bentos [which(bentos$Locality %in% locais_corais),]
L.peixes <- L.peixes [which(L.peixes$location %in% locais_corais),]

# nspp Longo et al. 

unique_spp_longo138 <- unique(L.peixes$ScientificName)
# save to further interpretation (functional space)
save (unique_spp_longo138, file=here("output","unique_spp_longo138.RData"))

## modificar o eventID removendo o ano

bentos$eventID_MOD <- substr(bentos$eventID, 1,nchar(as.character(bentos$eventID))-5) 
unique(bentos$eventID_MOD)
#################################
### filtrando as especies de peixes

## species de peixes que habitam o fundo, e sao sedentarios
traits_peixes <- read.csv(here("data","traits","Atributos_especies_Atlantico_&_Pacifico_Oriental_2020_04_28.csv"),
                          h=T,sep=";")
sp_fundo_sed <- traits_peixes [which(traits_peixes$Home_range %in% c("sed","mob") & 
                                       traits_peixes$Level_water %in% c("low","bottom") &
                                       traits_peixes$Diel_activity %in% c("day","both")),]
## filtrando familias

sp_fundo_sed_fam <- sp_fundo_sed [which(sp_fundo_sed$Family %in% c("acanthuridae", "apogonidae", "blenniidae",
                         "chaetodontidae", "gobiidae", "labridae","holocentridae",
                         "pomacentridae", "scaridae", "serranidae"
                         )),]
                                     
## ajustar nome
sp_fundo_sed_fam$Name <- gsub (" ",".",tolower (sp_fundo_sed_fam$Name))

### remover do dataset sp de peixes que nao atendem ao criterio de nivel de agua e sedentarismo

## Longo
L.peixes_subset <- L.peixes[which (L.peixes$ScientificName %in% sp_fundo_sed_fam$Name),]

## subset entre os datasets, desde que peixes ou bentos foram amostrados nos mesmos locais
## Longo
L.peixes_subset <- L.peixes_subset [which(L.peixes_subset$eventID_MOD %in% bentos$eventID_MOD),]

## da mesma forma, pegar o subset de ID de bentos que estao nos dados de peixes
L.bentos_subset <- bentos [which(bentos$eventID_MOD %in% L.peixes_subset$eventID_MOD),]

## export data.frame of fish basic data (for interpretation of size profiles)
save (L.peixes_subset, file=here("output","L.peixes_subset.RData"))

######################################################################
######################################################################
## comecar a formatar o subset de dados de peixes para os modelos   ##
## DADOS DE Longo ET AL. 2019                                      ##
######################################################################
######################################################################

## obter dados de cobertura dos corais

sitios_longo <- unique (L.peixes_subset$eventID_MOD)
sitios_longo <- gsub ("oc_isl.","",gsub ("se_reefs.","",gsub ("ne_reefs.","",sitios_longo)))
sitios_longo <- sitios_longo [order(sitios_longo)]
## editar tb na tabela original
L.peixes_subset$eventID_MOD <- gsub ("oc_isl.","",gsub ("se_reefs.","",gsub ("ne_reefs.","",L.peixes_subset$eventID_MOD)))

## substituir NAs por 1, porque teve observacao de bite
L.peixes_subset$number_of_bites [is.na(L.peixes_subset$number_of_bites)] <- 1

## todas as spp do subset de dados de longo
todas_sp_longo <- unique (L.peixes_subset$ScientificName)

## uma tabela dinamica por sitio, com a especie na linha, e o video na coluna
data_longo <- lapply (sitios_longo, function (i)
  
  cast(L.peixes_subset [which (L.peixes_subset$eventID_MOD == i),],
       formula = ScientificName  ~ video,
       value= "number_of_bites",
       fun.aggregate = sum,
       na.rm=T)
)

## ajustar os nomes das colunas, removendo a palavra "Video" (menos do primeiro nome- que eh a especie)
## e colocar as colunas da tabela em ordem de videos

data_longo_adjusted <- lapply (data_longo, function (i) {
  # salvar sp
  list.L.peixes <- i$ScientificName
  ## remover sp da tabela
  cont_preliminar <- i[,-1]
  # substituir a palavra video por nada
  colnames (cont_preliminar) <- gsub ("video","",colnames (cont_preliminar))
  ## colunas ordenadas
  cont_preliminar<-cont_preliminar [,order (as.numeric (colnames (cont_preliminar)))]
  ## agora colocar o nome das colunas como uma sequencia de 1: nvideos
  colnames (cont_preliminar) <- seq (1,length(colnames(cont_preliminar)))
  ## colocar o taxon de volta
  cont_preliminar <- cbind(list.L.peixes, cont_preliminar)
  
  ;
  cont_preliminar
  
}
)

## agora tem que inserir na tabela colunas para os videos que faltam em alguns sitios
maximo_videos <-  max(unlist (lapply (data_longo_adjusted,ncol)))+1  ## + 1 para nao dar problema para o dataset completo

imput_longo_data <- lapply (data_longo_adjusted, function (i)
  
  (matrix (NA, 
           nrow= nrow (i),
           ncol = maximo_videos - ncol (i),
           dimnames = list (i$list.L.peixes )))
)

## agora colar na tabela de cobertura

imputed_longo_data <- lapply (seq (1, length (data_longo_adjusted)), function (i)
  
  cbind (data_longo_adjusted [[i]],
         add = imput_longo_data [[i]])
)

## ajustar novamente o nome das colunas

imputed_longo_data <- lapply (imputed_longo_data, function (i) {
  
  colnames (i)[-1] <- seq (1,length (colnames (i)[-1]))
  
  ; i ## return i 
  
})

names (imputed_longo_data ) <- sitios_longo

## neste formato, a especie de peixe esta na linha, o video_number na coluna,
## e o sitio na lista; desmanchar para oragnizar no formato array
## usar do.call para tornar um DF
# spp de peixes para o subset
list_sp_longo <- unique(unlist (lapply (imputed_longo_data, '[',1)))

## orgaizando dimensoes
tabela_data_longo <- lapply (list_sp_longo, function (k)

    do.call (rbind,
      
            lapply (imputed_longo_data, function (i)
  
                  i [which(i$list.L.peixes == k),])
))

## cf dimensoes
# length(list_sp_longo)# n species
# ncol(tabela_data_longo[[1]]) ## numero videos
# nrow(tabela_data_longo[[1]]) ## numero sites

## montar uma tabela modelo para imputar sitios
nvideos_site <- unlist (lapply (data_longo_adjusted,ncol))

## sitios para imputar - 0 se sem observacao nenhuma (nenhum video)
imput_sitios <- lapply (seq(1,length(nvideos_site)), function (i)
  
  matrix(0, ncol=nvideos_site[i],nrow=1,
         dimnames = list(sitios_longo[i],
                         seq(1,nvideos_site[i]))))

## imputar sitios NAs
imput_sitios <- lapply (seq (1,length (imput_sitios)), function (i)

  cbind(imput_sitios[[i]], matrix (NA, nrow=1,ncol = maximo_videos - ncol(imput_sitios [[i]]),
        dimnames =list(rownames(imput_sitios [[i]]),
                       seq( ncol(imput_sitios [[i]])+1, maximo_videos))
        )
)
)

## sitios para imputar
imput_sitios <- do.call (rbind,imput_sitios)[,-12]

# imputar sitios
tabela_data_longo <- lapply (tabela_data_longo, function (i)

  rbind (i[,-1], imput_sitios [which(rownames(imput_sitios) %in% rownames(i) == F),])
       
        )
        

names(tabela_data_longo) <- list_sp_longo

## colocar as linhas em ordem alfabetica commum 

tabela_data_longo <- lapply(tabela_data_longo, function (i) {
  
  i [order(rownames(i)),]

  })

### transformar a lista em array

arranjo_longo_sitio_video <-   array(unlist(tabela_data_longo ), 
                                         dim = c(nrow(tabela_data_longo [[1]]), 
                                                 ncol(tabela_data_longo [[1]]), 
                                                 length(tabela_data_longo )),
                                         dimnames = list (sitios_longo [order(sitios_longo)],
                                                          NULL,
                                                          list_sp_longo))

## transformar em 0 e 1 ( deteccao e nao deteccao )
arranjo_longo_sitio_video [arranjo_longo_sitio_video > 1] <- 1
## remover o video 11
arranjo_longo_sitio_video <- arranjo_longo_sitio_video [,-11,] 

##############################################
## covariaveis de observacao

tempo<-lapply (L.peixes_subset$total_time, function (i) {
  minuto <- as.numeric (strsplit (gsub ("''","",i), "'") [[1]] [1])
  segundo <- as.numeric (strsplit (gsub ("''","",i), "'") [[1]] [2])
  tempo <- (minuto*60)+segundo
  ;
  tempo
  }
)

## bind nos dados 
L.peixes_subset$tempo <- unlist (tempo)

## covariate

## uma tabela dinamica por sitio, com a especie na linha, e o video na coluna
data_longo_tempo <- lapply (sitios_longo, function (i)
  
  as.data.frame (cast(L.peixes_subset [which (L.peixes_subset$eventID_MOD == i),],
       formula = eventID_MOD  ~ video,
       value= "tempo",
       fun.aggregate = mean,
       na.rm=T)[,-1]
))

# maximo de videos
maximo_videos_longo <- max(unlist(lapply (data_longo_tempo,ncol)))

df_tempo <- lapply (data_longo_tempo, function (i) {
  
  ## colocando colunas em ordem
  ordem <- i [,order (as.numeric(gsub ("a",".1", (gsub ("video","",colnames (i))))))]
  ##  adicionar NAs
  imput <- matrix (NA, ncol = maximo_videos_longo+1 - ncol(i),
                   nrow = nrow(i),
                   dimnames = list (NULL,
                                    paste ("video", seq (ncol(i)+1,maximo_videos_longo+1),sep="")
                                    ))
  # colar imputs
  ordem <- cbind(ordem,imput)
  # ajustar colnames 
  colnames(ordem) <- seq (1,ncol(ordem))
  ;
  ordem
  
  
})
  
## list to df
## remover a colunas 11 que nao tem nada
df_tempo <- do.call (rbind, df_tempo)[,-11]

rowSums (df_tempo>0,na.rm=T) == rowSums (arranjo_longo_sitio_video[,,1]>=0,na.rm=T)

## profundidade
data_longo_prof <- unlist(lapply (strsplit (sitios_longo, "\\."), "[[",3))

#######################################################################################
#######################################################################################
### comecar a formatar os dados dos corais para os modelos de ocupacao de sitios
### PARA OS DADOS DE LONGO ET AL. 
#######################################################################################
#######################################################################################

## obter dados de cobertura dos corais

sitios_bentos <- unique (L.bentos_subset$eventID_MOD)
sitios_bentos <- gsub ("oc_isl.","",gsub ("se_reefs.","",gsub ("ne_reefs.","",sitios_bentos)))

## fechar com os nomes dos sitios dos dados de peixes
sitios_bentos <- sitios_bentos[match(sitios_longo, sitios_bentos)]

## editar tb na tabela original
L.bentos_subset$eventID_MOD <- gsub ("oc_isl.","",gsub ("se_reefs.","",gsub ("ne_reefs.","",L.bentos_subset$eventID_MOD)))

## uma tabela dinamica por sitio, com a especie na linha, e o video na coluna
cob_bentos <- lapply (sitios_bentos, function (i)
  
  cast(L.bentos_subset [which (L.bentos_subset$eventID_MOD == i),],
       formula = Taxon  ~ Video_number,
       value= "Cover",
       fun.aggregate = max)
)

## agora pegar somente os dados dos corais que estou interessado
## lista de sp de corais nos dados de Aued
sp_corais <- c("Agaricia.fragilis", "Agaricia.humilis", "Agaricia.sp","Favia.gravida",
               "Madracis.decactis", "Meandrina.brasiliensis","Millepora.alcicornis",
               "Millepora.nitida",
               "Montastraea.cavernosa", "Mussismilia.harttii", "Mussismilia.braziliensis",
               "Mussismilia.hispida", "Mussismilia.leptophylla", "Porites.astreoides",
               "Porites.branneri", "Siderastrea.spp")

## sem considerar "Millepora.sp", "Millepora.incrusting",  "Porites.sp",

# agora pegar o subconjunto das spp. de corais 
cob_bentos <- lapply (cob_bentos, function (i)
  
  i [which (i$Taxon %in% sp_corais),]
  
)

## ajustar os nomes das colunas, removendo a palavra "Video" (menos do primeiro nome- que eh a especie)
## e colocar as colunas da tabela em ordem de videos

cob_bentos <- lapply (cob_bentos, function (i) {
  # salvar sp
  corais <- i$Taxon
  ## remover sp da tabela
  cob_preliminar <- i[,-1]
  # substituir video por nada
  colnames (cob_preliminar) <- gsub ("Video","",colnames (cob_preliminar))
  ## colunas ordenadas
  cob_preliminar<-cob_preliminar [,order (as.numeric (colnames (cob_preliminar)))]
  ## agora colocar o nome das colunas como uma sequencia de 1: nvideos
  colnames (cob_preliminar) <- seq (1,length(colnames(cob_preliminar)))
  ## colocar o taxon de volta
  cob_preliminar <- cbind(corais, cob_preliminar)
  
  ;cob_preliminar
  
}
)

## agora tem que inserir na tabela colunas para os videos que faltam em alguns sitios
maximo_videos <-  max(unlist (lapply (cob_bentos,ncol)))+1  ## + 1 para nao dar problema para o dataset completo

imput_coral_data <- lapply (cob_bentos, function (i)
  
  (matrix (NA, 
           nrow= nrow (i),
           ncol = maximo_videos - ncol (i),
           dimnames = list (i$corais)))
)

## agora colar na tabela de cobertura

imputed_coral_data <- lapply (seq (1, length (cob_bentos)), function (i)
  
  cbind (cob_bentos [[i]],
         add = imput_coral_data [[i]])
)

## ajustar novamente o nome das colunas

imputed_coral_data <- lapply (imputed_coral_data, function (i) {
  
  colnames (i)[-1] <- seq (1,length (colnames (i)[-1]))
  
  ; i ## return i 
  
})

## neste formato, a especie de coral esta na linha, o video_number na coluna,
## e o sitio na lista; desmanchar para oragnizar no formato array
## usar do.call para tornar um DF

lista_coral_sitio <- lapply (seq (1,length(sp_corais)), function (k) 
  do.call(rbind, lapply (imputed_coral_data, function (i)
    
    i [k,]
  )))

## cf dimensoes
# length(lista_coral_sitio)# n species
# ncol(lista_coral_sitio [[1]]) ## numero videos
# nrow(lista_coral_sitio [[1]]) ## numero sites

names(lista_coral_sitio) <- sp_corais

# remover a primeira coluna de cada tabela (nome da sp)

tab_completa_site_ocasiao_coral <-  lapply (lista_coral_sitio, function (i) 
  
  i[,-1]
  
)

names (tab_completa_site_ocasiao_coral ) <- sp_corais

### transformar a lista em array

arranjo_cob_coral_sitio_video <-   array(unlist(tab_completa_site_ocasiao_coral), 
                                         dim = c(nrow(tab_completa_site_ocasiao_coral[[1]]), 
                                                 ncol(tab_completa_site_ocasiao_coral[[1]]), 
                                                 length(tab_completa_site_ocasiao_coral)),
                                         dimnames = list (sitios_bentos,
                                                          NULL,
                                                          sp_corais))

## obter informacao combinada das 3 spp de agaricia
agaricia_sp_cover <- apply (arranjo_cob_coral_sitio_video [,,grep("Agaricia", sp_corais)],
                      c(1,2), sum)

## colar no array dos corais mais frequentes
## array final
arranjo_cob_coral_sitio_video <- abind(agaricia_sp_cover, arranjo_cob_coral_sitio_video )

## ajustar os nomes
dimnames(arranjo_cob_coral_sitio_video)[[3]][1] <- "Agaricia.spp"

## transformar cobertura em dado binario (deteccao e nao deteccao do coral k no sitio i, video j)
arranjo_corais <- arranjo_cob_coral_sitio_video
## remover dados do video 16 (que nao tem dados)
arranjo_corais <- arranjo_corais [,-16,]

#####################################################################
## dados de coebrtura de cada espécie de coral retiva a cobertura de coral do videoplot

## cobertura total de corais no video k, sitio i
cover_data <- apply(arranjo_corais,c(1,2),sum,na.rm=T) 

## cobertura de cada sp no video relativo ao total, using for =[

shell_array <- array (NA,dim = dim(arranjo_corais)) 

for (i in 1:dim(arranjo_corais)[3]) {
  
  shell_array[,,i] <- arranjo_corais [,,i]/cover_data
  
}

## Nan por zero, porque 0/0 = NaN
shell_array [is.nan(shell_array)] <- 0

## maximo de cobertura detectado em um video
## NA porque em alguns sitios nao foi registrado nada de coral
sp_cover_data <- apply (shell_array,c(1,3),mean,na.rm=T)
colnames(sp_cover_data) <- dimnames(arranjo_corais )[[3]]
rownames(sp_cover_data)<- dimnames(arranjo_corais )[[1]]
## remover spp que ocorreram em poucos locais (menos min_sites)

site_coral_detection <- ifelse (sp_cover_data>0,1,0)
min_sites <- round(nrow(site_coral_detection)*0.2)

## na cobertura em relacao ao total
sp_cover_data <- sp_cover_data[,which(colSums (site_coral_detection)>=min_sites)]
## desciptive statistics
round (apply (sp_cover_data,2,mean,na.rm=T),2)
round (apply (sp_cover_data,2,sd,na.rm=T),2)
round (apply (sp_cover_data,2,range,na.rm=T),2)
## no arranjo do cover
arranjo_corais <- arranjo_corais [,,which(colSums (site_coral_detection)>=min_sites)]

## numero de videos por sitio
nvideos <- rowSums(is.na(arranjo_corais[,,1])!=T)

## 
# ## finalmente, transformar em formato longo
df_coral_data <- lapply (seq(1,dim(arranjo_corais)[3]), function (sp) {
  
  ## transforma matriz em vetor
  y_long <- as.numeric(arranjo_corais [,,sp])
  
  ## ocasioes
  df_data <- data.frame(obs= seq(1,length(y_long)),
                        y= y_long,
                        M = rep (seq (1,dim(arranjo_corais)[1]), 
                                 ncol(arranjo_corais)),
                        J = unlist(
                          lapply (seq(1,ncol(arranjo_corais)), function (i) 
                            rep (i,dim(arranjo_corais)[1]))
                        )
  )
  
  ## remover NAs
  df_data <- df_data[which (is.na(df_data$y) != T),]
  df_data$n.obs <- seq (1,nrow(df_data)) ## new obs - sequence disconsidering NAs
  ## remover os zeros
  df_data <- df_data [which(df_data$y >0),]
  df_data$n.obs <- seq (1,nrow(df_data)) ## new obs - sequence disconsidering zeros
  
  ;df_data 
}
)

## coordenadas geográficas para modelo de estimativa de cobertura dos corais
## NA por que a agregacao de  fatores nao eh possivel
coordenadas <- aggregate(L.bentos_subset, 
                         by= list (L.bentos_subset$eventID_MOD), 
                         FUN=mean)[c("Group.1","Lon","Lat")]

# organizar a ordem dos nomes
coordenadas <- coordenadas [match(sitios_bentos,coordenadas$Group.1),]

##############################################
### CORRESPONDENCIA COM OS DADOS DE CORAIS

##########################################################################
##########################################################################
###  evitar mismatch espacial de ocorrencia de coral e peixe

site_peixe_detection <- apply(arranjo_longo_sitio_video,c(1,3),sum,na.rm=T)
site_coral_detection <- ifelse (sp_cover_data>0,1,0)

## quais corais ocorrem em ao menos "min_sites"

sp_cover_data <- sp_cover_data [,which(colSums (site_coral_detection)>=min_sites)]

## quais peixes sobrepoe ao menos "min_sites" de observacao de cada sp de coral
over_peixe_coral <- lapply (seq(1,ncol (site_coral_detection)), function (k) # avalie para cada sp de coral
  
  unlist( # desmanche a lista
    
    lapply (seq (1,ncol (site_peixe_detection)), function (i) # avalie para cada sp de peixe
      ## se o peixe ocorre em no minimo "min sites" com deteccao de coral
      table(which (site_peixe_detection[,i] ==1) %in% which (site_coral_detection[,k] ==1))[2] >= min_sites
      
    )))

## subset dos peixes com deteccao em ao menos "min sites" de deteccao de coral
subset_peixes <- lapply (over_peixe_coral, function (i)
  
  arranjo_longo_sitio_video [,,which (is.na(i)!=T)]
  
)

##  finalmente, transformar os dados de peixe em formato longo

df_fish_data <- lapply (subset_peixes, function (coral) ## para cada data set de coral
  lapply (seq(1,dim(coral)[3]), function (sp) { ## para cada sp de peixe
    
    ## transforma matriz em vetor
    y_long <- as.numeric(coral [,,sp])
    tabela_tempo <- as.numeric (as.matrix(df_tempo))
    
    ## ocasioes
    df_data <- data.frame(obs= seq(1,length(y_long)),
                          y= y_long,
                          time = as.numeric(tabela_tempo),
                          M = rep (seq (1,dim(coral)[1]), 
                                   ncol(df_tempo)),
                          J = unlist(
                            lapply (seq(1,ncol(df_tempo)), function (i) 
                              rep (i,dim(coral)[1]))
                          ),
                          prof = as.numeric(ifelse (data_longo_prof =="fundo", 2,1))
    )
    
    ## remover NAs
    df_data <- df_data[which (is.na(df_data$time) != T),]
    #df_data [is.na (df_data)] <- 0
    
    ; 
    df_data
    
  }
  ))

## formar uma lista de arrays de formatos longos

df_fish_data_per_coral <- lapply (seq (1,length(df_fish_data)), function (i){
  
  ## contruir um array vazio
  empty_array <- array (NA, dim=c(nrow(df_fish_data[[i]][[1]]), # aqui independe do segundo nivel, pois todos sao iguais - cf lapply(df_fish_data[[1]],nrow)
                                  ncol (df_fish_data[[i]][[1]]),
                                  length(df_fish_data[[i]])),
                        dimnames = list(NULL, 
                                        colnames(df_fish_data[[i]][[1]]),NULL))
  
  for (k in 1:dim(empty_array)[3]) {
    
    empty_array [,,k] <- as.matrix(df_fish_data[[i]][[k]])
    
  }
  filled_array <- empty_array
  ;
  filled_array
  
}
)

# lista das spp
coral_species <- colnames(site_coral_detection)
fish_species <- lapply (subset_peixes, function (i){
  
  nomes <- dimnames(i)[3]  
  ;nomes[[1]]
  
  })


## basic statistics

mean((df_fish_data_per_coral[[1]][,"time",1])/60)
sd((df_fish_data_per_coral[[1]][,"time",1])/60)
range((df_fish_data_per_coral[[1]][,"time",1])/60)

## av number of det per fish

mean (
  unlist(
    lapply (seq (1,length(df_fish_data_per_coral)), function (k)
      
      lapply (seq (1,dim(df_fish_data_per_coral[[k]])[3]), function (i)
        
        sum(df_fish_data_per_coral[[k]][,"y",i])
      )
    )
  )
)


sd (
  unlist(
    lapply (seq (1,length(df_fish_data_per_coral)), function (k)
      
      lapply (seq (1,dim(df_fish_data_per_coral[[k]])[3]), function (i)
        
        sum(df_fish_data_per_coral[[k]][,"y",i])
      )
    )
  )
)

## most detected spp
most_det <- unlist(lapply (seq (1,length(df_fish_data_per_coral)), function (k)
  lapply (seq (1,dim(df_fish_data_per_coral[[k]])[3]), function (i)
    
    sum(df_fish_data_per_coral[[k]][,"y",i])
    
  )))

most_det <- data.frame (ndet=most_det,
                        sp = unlist(fish_species))

most_det <- most_det[order(most_det$ndet,decreasing=T),]               
write.table (most_det, file = here("output","most_detVIDEO.csv"))

  
###############################################
# # # # # # # # #  SAVE # # # # # # # # # # # #
###############################################

### save data - para os modelos de coral

save (arranjo_corais, ### dados de cobertura 
      df_coral_data, ## df em formato longo para modelagem
      sp_cover_data, ## cobertura de cada coral relativo a cobertura de coral total
      nvideos, ## numero de videos por sitio
      coordenadas,# coordenadas dos sitios
      sitios_bentos,## nomes dos sitios
      coral_species,## id das especies de coral analisadas
      file=here ("output","Data_coral_detection_LONGO_AUED.RData"))

### salvar os dados para o modelo de ocupacao dos peixes

save (subset_peixes, # arranjo do subset de peixes com dados correspondendo aos corais
      df_fish_data_per_coral,## dados de peixes para a modelagem
      sitios_longo, # nome dos sitios
      fish_species,## id dos peixes analisados, para cada coral
      todas_sp_longo,## a id de todas as especies de peixes
      sp_fundo_sed_fam,## traits do subset dos peixes
      file = here("output", "Data_fish_detection_LONGO_AUED.RData"))

