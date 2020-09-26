## analises para o manuscrito "Fish reliance on live coral cover in the Brazilian Province" 

## chamar os pacotes
source("R/packages.R")
source("R/functions.R")

## CODIGO PARA ORGANIZAÇÃO DOS DADOS DE PEIXES E BENTOS
## A IDEIA EH PRIMEIRO FAZER UM ARRANJO (ARRAY) DE SITIOS (LINHAS), OCASIOES AMOSTRAIS (TRANSECCOES OU VIDEOS),
## E ESPECIE (3D). AO FIM, A IDEIA EH FAZER DATA FRAMES EM FORMATO LONGO PARA OS MODELOS DE OCUPACAO DE SITIOS, DE 
## MODO A REMOVER OS NAs E TRABALHAR SOMENTE COM AS OBSERVACOES

## dados dos bentos
bentos <- read.xlsx(here("data","detection","Updated_compiled_quadrats_allsites.xlsx"),
                    sheet = 1, colNames = TRUE,detectDates=F)
## converter data em data - bug do pacote openxlsx
bentos$eventDate <-convertToDate(bentos$eventDate)

## dados dos peixes
peixes <- read.xlsx(here("data","detection","UpdatedData_RMorais_et_al_2017.xlsx"),
                    sheet = 1, colNames = TRUE,detectDates=F)
## converter data em data - bug do pacote openxlsx
peixes$eventDate <-convertToDate(peixes$eventDate)

## OBTER A ID DE TODAS AS ESPECIES DE PEIXES ENCONTRADAS POR MORAIS
todas_sp_Morais <- (peixes$ScientificName)

## dados dos peixes (Longo et al)
L.peixes <- read.csv(here("data","detection","occ_Longo_et_al","Data_Trophic_Interactions_WAtlantic_GLongo_clean.csv"),
                    header = T)

## conferimento dos sitios com o GLongo
#write.csv (
#  data.frame (
#    unique_Longo_col.site=unique (L.peixes$site)[order(unique (L.peixes$site))],
#    unique_Aued_col.Sites= c(unique (bentos$Sites)[order(unique (bentos$Sites))], 
#                             rep(NA, length (unique (L.peixes$site)[order(unique (L.peixes$site))])-length(unique (bentos$Sites)[order(unique (bentos$Sites))]))))
#  , file ="unique_sites.csv")
#
#write.csv (
#  data.frame (
#    unique_Longo_col.loc=unique (L.peixes$location)[order(unique (L.peixes$location))],
#    unique_Aued_col.Loc= c(unique (bentos$Locality)[order(unique (bentos$Locality))], 
#                           rep(NA, length (unique (L.peixes$location)[order(unique (L.peixes$location))])-length(unique (bentos$Locality)[order(unique (bentos$Locality))]))))
#  , file ="unique_loc.csv")
#

## adequar os nomes das localidades e sitios da base de Longo, de acordo com Aued
# localidades
L.peixes$location [which (L.peixes$location == "alagoas")] <- "costa_corais"
L.peixes$location [which (L.peixes$location == "pernambuco")] <- "costa_corais"
L.peixes$location [which (L.peixes$location == "atol_das_rocas")] <- "rocas"
L.peixes$location [which (L.peixes$location == "guarapari")] <- "espirito_santo"
L.peixes$location [which (L.peixes$location == "arraial_do_cabo")] <- "arraial"
L.peixes$location [which (L.peixes$location == "sao_paulo")] <- "ilhabela"
L.peixes$location [which (L.peixes$location == "parcel_manoel_luis")] <- "manuel_luis"
# separar as localidades do rio grande do norte
L.peixes$location [which (L.peixes$site == "batente_das_agulhas")] <- "rgnor_norte"
L.peixes$location [which (L.peixes$site == "pedra_do_silva")] <- "rgnor_norte"
L.peixes$location [which (L.peixes$site == "barreirinha")] <- "rgnor_norte"
L.peixes$location [which (L.peixes$site == "cabeco_do_leandro")] <- "rgnor_norte"
L.peixes$location [which (L.peixes$site == "maracajau")] <- "rgnor_parrachos"
L.peixes$location [which (L.peixes$site == "parrachos")] <- "rgnor_parrachos"
# separar os sitios de santa catarina
L.peixes$location [which (L.peixes$site == "deserta")] <- "ilhasc_norte"
L.peixes$location [which (L.peixes$site == "saco_d'agua")] <- "ilhasc_norte"
L.peixes$location [which (L.peixes$site == "baia_da_tartaruga")] <- "ilhasc_norte"
L.peixes$location [which (L.peixes$site == "saco_do_vidal")] <- "ilhasc_norte"
L.peixes$location [which (L.peixes$site == "engenho")] <- "ilhasc_norte"
L.peixes$location [which (L.peixes$site == "xavier")] <- "ilhasc_sul"

### organizar os sitios
L.peixes$site [which(L.peixes$site == "anacris")] <- "ana_cristina"
L.peixes$site [which(L.peixes$site == "parrachos")] <- 'parrachos_de_rio_do_fogo'
L.peixes$site [which(L.peixes$site == "rocas")] <- 'piscina_das_rocas'
L.peixes$site [which(L.peixes$site == "chapeiroes")] <- 'chapeirao'
L.peixes$site [which(L.peixes$site == "ilha_escalvada")] <- 'escalvada'
L.peixes$site [which(L.peixes$site == "ilha_rasa")] <- 'ilhas_rasas'
L.peixes$site [which(L.peixes$site == "porcos")] <- 'porcos_oeste'
L.peixes$site [which(L.peixes$site == "saco_do_sombrio_")] <- 'saco_do_sombrio'
L.peixes$site [which(L.peixes$site == "cabras")] <- 'ilha_das_cabras'
L.peixes$site [which(L.peixes$site == "ponta_do_diogo")] <- 'saco_do_diogo'
L.peixes$site [which(L.peixes$site == "xavier")] <- 'xavier_ponta_sul'
L.peixes$site [which(L.peixes$site == "deserta")] <- 'deserta_norte'
L.peixes$site [which(L.peixes$site == "saco_d'agua")] <- 'arvoredo_saco_dagua'
L.peixes$site [which(L.peixes$site == "barra_gales")] <- 'barra_da_gale'

## modificar a profundidade 
L.peixes$depth_m <- ifelse (L.peixes$depth_m >= 8, "fundo","raso")

## add region
L.peixes$Region <- bentos$Region [match(L.peixes$location, bentos$Locality)]

## fazer eventID para este estudo
L.peixes$eventID_MOD <- paste (L.peixes$Region,
                               L.peixes$location,
                               L.peixes$site,
                               L.peixes$depth_m,
                               sep=".")

## OBTER A ID DE TODAS AS ESPECIES DE PEIXES ENCONTRADAS POR Longo
todas_sp_Longo <-  (L.peixes$species_code)

# corresponder siglas de Longo com nomes completos de Quimbayo
traits_peixes <- read.csv(here("data","traits","Atributos_especies_Atlantico_&_Pacifico_Oriental_2020_04_28.csv"),
                          h=T,sep=";")
split_names_JPQ <- lapply (strsplit(traits_peixes$Name," ",fixed=F), substr, 1,3)
split_names_JPQ <- lapply (split_names_JPQ,tolower)
siglas_JPQ <- unlist(lapply (split_names_JPQ, function (i) paste(i[1],i[2],sep="_")))

## inserir uma tabela em Longo, com o nome completo das spp
L.peixes$ScientificName <- traits_peixes$Name [match(todas_sp_Longo,siglas_JPQ)]

# encontrar quais especies estao em longo, mas nao estao em Morais
unique (todas_sp_Longo [which(todas_sp_Longo %in% siglas_JPQ == F)])

## ajustar spp.
L.peixes$ScientificName [which(L.peixes$species_code == "spa_sp")] <- "sparisoma_sp"
L.peixes$ScientificName [which(L.peixes$species_code == "ocy_cry")] <- "ocyurus_chrysurus"
L.peixes$ScientificName [which(L.peixes$species_code == "ni")] <- "not_identified"
L.peixes$ScientificName [which(L.peixes$species_code == "kyp_sp")] <- "kyphosus_sp"
L.peixes$ScientificName [which(L.peixes$species_code == "euc_lef")] <- "eucinostomus_lefroyi"
L.peixes$ScientificName [which(L.peixes$species_code == "lut_ale")] <- "lutjanus_alexandrei"
L.peixes$ScientificName [which(L.peixes$species_code == "lut_sp")] <- "lutjanus_sp"
L.peixes$ScientificName [which(L.peixes$species_code == "aca_sp")] <- "acanthurus_sp"
L.peixes$ScientificName [which(L.peixes$species_code == "car_sp")] <- "caranx_sp1"
L.peixes$ScientificName [which(L.peixes$species_code == "acanthuridae")] <- "not_identified"
L.peixes$ScientificName [which(L.peixes$species_code == "dio_his")] <- "diodon_hystrix"
L.peixes$ScientificName [which(L.peixes$species_code == "hal_sp")] <- "halichoeres_sp"
L.peixes$ScientificName [which(L.peixes$species_code == "sca_sp")] <- "scarus_sp"
L.peixes$ScientificName [which(L.peixes$species_code == "blenideo")] <- "not_identified"
L.peixes$ScientificName [which(L.peixes$species_code == "das_mar")] <- "Hypanus_marianae"
L.peixes$ScientificName [which(L.peixes$species_code == "lab_sp")] <- "labrisomus_sp"
L.peixes$ScientificName [which(L.peixes$species_code == "mal_sp")] <- "malacoctenus_sp"
L.peixes$ScientificName [which(L.peixes$species_code == "bot_sp")] <- "bothus_sp"
L.peixes$ScientificName [which(L.peixes$species_code == "carangideo")] <- "caranx_sp2"
L.peixes$ScientificName [which(L.peixes$species_code == "syn_sp")] <- "synodus_sp"
L.peixes$ScientificName [which(L.peixes$species_code == "manjuba")] <- "anchoviella_lepidentostole"
L.peixes$ScientificName [which(L.peixes$species_code == "sphyraena_borealis?")] <- "sphyraena_borealis"
L.peixes$ScientificName [which(L.peixes$species_code == "myc_sp")] <- "mycteroperca_sp"
L.peixes$ScientificName [which(L.peixes$species_code == "scomberomorus")] <- "scomberomorus_sp"
L.peixes$ScientificName [which(L.peixes$species_code == "sca_coel")] <- "scarus_coelestinus"
L.peixes$ScientificName [which(L.peixes$species_code == "carangidae")] <- "caranx_sp3"
L.peixes$ScientificName [which(L.peixes$species_code == "epi_niv")] <- "epinephelus_niveatus"
L.peixes$ScientificName [which(L.peixes$species_code == "epi_cru")] <- "epinephelus_cruentatus"
L.peixes$ScientificName [which(L.peixes$species_code == "neg_bre")] <- "negaprion_brevirostris"
L.peixes$ScientificName [which(L.peixes$species_code == "gin_cir")] <- "ginglymostoma_cirratum"
L.peixes$ScientificName [which(L.peixes$species_code == "par_sp")] <- "parablennius_sp"
L.peixes$ScientificName [which(L.peixes$species_code == "aet_nar")] <- "aetobatus_narinari"             
L.peixes$ScientificName [which(L.peixes$species_code == "sco_sp")] <- "scorpaena_sp"
L.peixes$ScientificName [which(L.peixes$species_code == "hae_sp")] <- "haemulon_sp"
L.peixes$ScientificName [which(L.peixes$species_code == "manjuba")] <- "anchoviella_lepidentostole"             
L.peixes$ScientificName [which(L.peixes$species_code == "das_ame")] <- "hypanus_americana"            
L.peixes$ScientificName [which(L.peixes$species_code == "sph_sp")] <- "sphoeroides_sp"
L.peixes$ScientificName [which(L.peixes$species_code ==  "car_plu")] <- "caranx_plumbeus"     
L.peixes$ScientificName [which(L.peixes$species_code ==  "lut_moh")] <- "lutjanus_mohogani"             
L.peixes$ScientificName [which(L.peixes$species_code == "mir_jac")] <- "myripristis_jacobus"
L.peixes$ScientificName [which(L.peixes$species_code == "pem_sco")] <- "pempheris_schomburgkii"
L.peixes$ScientificName [which(L.peixes$species_code == "das_sp")] <- "dasyatis_sp"
# 
L.peixes$site [which(L.peixes$species_code == "het_pri")]

  
write.csv (L.peixes, file =here("data","detection","occ_Longo_et_al","Data_Trophic_Interactions_WAtlantic_GLongo_clean_UPDATED_ALLB.csv"))

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

## Load new data from Longo

L.peixes <- read.csv(here("data","detection","occ_Longo_et_al","Data_Trophic_Interactions_WAtlantic_GLongo_clean_UPDATED_ALL.csv"),
                     header = T)

## subset dos sitios baseado na quantidade de coral
peixes <- peixes [which(peixes$Locality %in% locais_corais),]
bentos <- bentos [which(bentos$Locality %in% locais_corais),]
L.peixes <- L.peixes [which(L.peixes$location %in% locais_corais),]

## filtrar os dados de peixes de acordo com o minimo de ano de bentos
peixes <- peixes [which(peixes$eventYear >= min (bentos$eventYear) & peixes$eventYear <= max (bentos$eventYear)),]

## modificar o eventID removendo o ano, desde que escolhemos um lapso temporal que cobre 2010 a 2014, 
## de modo que possamos analisar os dados se peixes foi coletado em 2012 e bentos em 2014, por exemplo 

bentos$eventID_MOD <- substr(bentos$eventID, 1,nchar(as.character(bentos$eventID))-5) 
peixes$eventID_MOD  <- substr(peixes$eventID, 1,nchar(as.character(peixes$eventID))-5) 

## fazer um histograma pra saber o numero de eventIDS por ano

barplot_function (df1= bentos,#bentos
                  df2= peixes #peixes
                 )

### filtrando especies de peixes

## especeis de peixes que habitam o fundo, e sao sedentarios
traits_peixes <- read.csv(here("data","traits","Atributos_especies_Atlantico_&_Pacifico_Oriental_2020_04_28.csv"),
                          h=T,sep=";")
sp_fundo_sed <- traits_peixes [which(traits_peixes$Home_range %in% c("sed","mob") & 
                                       traits_peixes$Level_water %in% c("low","bottom")),]
## filtrando familias

sp_fundo_sed_fam <- sp_fundo_sed [which(sp_fundo_sed$Family %in% c("acanthuridae", "apogonidae", "blenniidae",
                         "chaetodontidae", "gobiidae", "labridae","holocentridae",
                         "pomacentridae", "scaridae", "serranidae"
                         )),]
                                     
## ajustar nome
sp_fundo_sed_fam$Name <- gsub (" ",".",tolower (sp_fundo_sed_fam$Name))

### remover do dataset sp de peixes que nao atendem ao criterio de nivel de agua e sedentarismo
## Morais
peixes_subset <- peixes[which (peixes$ScientificName %in% sp_fundo_sed_fam$Name),]
## Longo
L.peixes_subset <- L.peixes[which (L.peixes$ScientificName %in% sp_fundo_sed_fam$Name),]

## subset entre os datasets, desde que peixes ou bentos foram amostrados nos mesmos locais
## Morais
peixes_subset <- peixes_subset [which(peixes_subset$eventID_MOD %in% bentos$eventID_MOD),]
## Longo
L.peixes_subset <- L.peixes_subset [which(L.peixes_subset$eventID_MOD %in% bentos$eventID_MOD),]

## da mesma forma, pegar o subset de ID de bentos que estao nos dados de peixes
bentos_subset <- bentos [which(bentos$eventID_MOD %in% peixes_subset$eventID_MOD),]
L.bentos_subset <- bentos [which(bentos$eventID_MOD %in% L.peixes_subset$eventID_MOD),]

##  criar uma ID numerica para o observador
# somente para Morais
peixes_subset$ID.observer <- as.numeric(as.factor(peixes_subset$Observer))

### mapa para conferir a posicao dos sitios - com base no subset dos dados que correspondem
## tem um monte de pontos porque tem todos os eventIDs ai

initial_map_function (df1 = bentos_subset,# red
                      df2 = peixes_subset) # black

######################################################################
######################################################################
## comecar a formatar o subset de dados de peixes para os modelos   ##
## DADOS DE MORAIS ET AL. 2017                                      ##
######################################################################
######################################################################

### quais as localidades com dados nos subsets ???

## editar IDs e obter dados dos transectos
comb_locality_site_transect <- paste(peixes_subset$Locality, 
                                     peixes_subset$Site,
                                     peixes_subset$eventDepth, 
                                     peixes_subset$Transect_id,sep=".")

## obter os sitios unicos
comb_locality_site <- paste(peixes_subset$Locality, 
                            peixes_subset$Site,
                            peixes_subset$eventDepth,sep=".")
unique_comb_locality_site <- unique (comb_locality_site)

## o proximo passo eh criar um vetor dizendo quanto eventos amostrais (Transeccoes) cada sitio teve,
## e cole na tabela original
peixes_subset <- cbind(peixes_subset, transection_number = 
                         unlist (
                           lapply (unique_comb_locality_site, function (i)## para cada combinacao
                             as.numeric( ## cada nivel do fator como uma  contagem de repeticoes
                               factor (comb_locality_site_transect [which(comb_locality_site %in%  i )] ## transformar em fator
                                       ### o caracter vai se transformar em numero inteiro automaticamente
                               )
                             )
                           )
                         )
)

## dados basicos
## number of unique transects; number of unique localities; number of unique sites
length(unique (peixes_subset$Transect_id)); length(unique (peixes_subset$Locality)); length(unique (peixes_subset$Site))

## agora, ver quais especies de peixes restaram no subconjunto dos dados
especie <- unique (peixes_subset$ScientificName)
## criara um novo descritor de sitio, desta vez editado para ficar com formato similar aos dados de bentos
peixes_subset$locality_site <- paste(peixes_subset$Locality, 
                                     peixes_subset$Site,
                                     peixes_subset$eventDepth,sep=".")

## como o cast remove as transeccoes sem deteccao, temos que imputa-las
## primeiro tirar fora as spp que ocorreram em menos de 10 sitios

sp_dados_suficientes <- unlist(
  lapply (especie, function (i)
    length (unique (peixes_subset [which (peixes_subset$ScientificName %in% i),"locality_site"])) >= 10
  )
)

## especie com dados suficientes
## ESTE EH O POOL DE SP DA ANALISE
especie <- especie [which (sp_dados_suficientes == T)]

# colocar em ordem alfabetica
especie  <- especie [order(especie)]

## agora remover dos dados as especies que ocorrem em menos de 10 sitios
peixes_subset <- peixes_subset [which(peixes_subset$ScientificName %in% especie),]

## fazer uma tabela para cada sitio,  composta pela especie (linha) e transeccao (coluna)
tab_especies <- lapply (unique (peixes_subset$locality_site), function (i) ## para cada sitio
  
  cast(peixes_subset [peixes_subset$locality_site == i,], ## construa uma tabela dinamica de dados
       formula = ScientificName  ~ transection_number,
       value= "IndCounting",
       fun.aggregate = sum)
  
)

## adicionar as especies que nao foram detectadas em um dado sitio
## ja que o cast as remove da tabela

sp_da_coluna1 <- lapply (tab_especies, function (i) 
  
  data.frame (ScientificName= as.character(especie[especie %in% i$ScientificName == F]))
  
)

## fazer uma tabela vazia para imputacao  
## especies faltantes na linha
## transeccoes faltantes na coluna
sp_para_imputar <- lapply (seq(1,length(sp_da_coluna1)), function (i)
  cbind (sp_da_coluna1 [[i]], ## colar nos nomes as deteccoes 
         matrix(0, 
                nrow = length (especie[especie %in% tab_especies[[i]]$ScientificName == F]), ## sp que nao estao na tabela
                ncol= ncol (tab_especies[[i]])-1,## -1 pra tirar o nome das sp
                dimnames = list (NULL,
                                 colnames(tab_especies[[i]])[-1])
                
                
         )
  )
)

# imputar as sp atraves do rbind 
tab_especies_imput <- lapply (seq(1,length(sp_para_imputar)), function (i)
  
  rbind (tab_especies[[i]], sp_para_imputar[[i]])

  )

## ordenar as linhas das tabelas por ordem alfabetica
tab_especies_imput <- lapply (tab_especies_imput, function (i)
  
  i[order (i$ScientificName),]
  
)

# nomear a lista com o nome dos sitios
names(tab_especies_imput) <- unique (peixes_subset$locality_site)

# trabalhar a dimensao das ocasioes

## primeiro, colocar transeccoes que estao faltando no meio das existentes
# todas as transeccoes existentes
seq_ocasioes <- lapply (tab_especies_imput, function (i) 
  
  seq (1,max (as.numeric(colnames (i)[-1])))
)

## imputar em relacao as existentes e todas as possiveis (em relacao ao sitio com o n maximo de transeccoes -58) 
ocasioes_a_imputar <- lapply (seq(1,length(seq_ocasioes)), function (i)
  seq_ocasioes[[i]] [which(seq_ocasioes[[i]] %in% as.numeric(colnames (tab_especies_imput [[i]])[-1]) == F)]
)

## quais faltam
quais_faltam <-(which(lapply (ocasioes_a_imputar,length) >= 1))

## imputar somente nas tabelas que tem ocasioes faltando
ocasioes_a_imputar <- lapply (quais_faltam, function (i) # das que tem dados faltando
  ## construir uma matriz de zeros com o nrow igual o numero de especies
  # ncol igual ao numero de transeccoes faltantes
  matrix (0,
          nrow= length(especie),
          ncol=  length(seq_ocasioes[[i]] [which(seq_ocasioes[[i]] %in% as.numeric(colnames (tab_especies_imput [[i]])[-1]) == F)]),
          dimnames =list(NULL,
                         seq_ocasioes[[i]] [which(seq_ocasioes[[i]] %in% as.numeric(colnames (tab_especies_imput [[i]])[-1]) == F)])
  ))

# subset das tabelas para imputar
subset_imput <- tab_especies_imput [quais_faltam]
# colar as ocasioes faltantes
subset_imputadas <- lapply (seq(1,length(quais_faltam)), function (i)
  
  cbind(subset_imput [[i]],ocasioes_a_imputar[[i]]))

## ordenar as colunas por sequencia de ocasioes
subset_imputadas <- lapply (subset_imputadas, function (i) 
  i [,order(as.numeric (colnames (i)))]
) ## vai produzir NA porque uma coluna nao eh numerica

## nomear
names(subset_imputadas) <- unique (peixes_subset$locality_site) [quais_faltam]

## colar as duas listas
tabelas_ocasioes_imputadas <- c (tab_especies_imput [-quais_faltam], subset_imputadas)
tabelas_ocasioes_imputadas <- tabelas_ocasioes_imputadas[match(names(tab_especies_imput),names (tabelas_ocasioes_imputadas))]# == names(tab_especies_imput

## cf os nomes dos sitios nos dois grupos de dados (imputados e completos)
# names(tabelas_ocasioes_imputadas) == names(tab_especies_imput)

# remover a coluna do nome cientifico da sp.
tabelas_ocasioes_imputadas <- lapply (tabelas_ocasioes_imputadas, function (i)
  i [,-which(colnames(i) == "ScientificName")]
)

## agora, finalmente, colar as transeccoes inexistentes em um sitio
# encontrar o maximo de transeccoes existentes
maximo_ocasioes <- max(unlist(lapply (tabelas_ocasioes_imputadas, ncol)))

# colar
imputar_maximo_ocasioes <- lapply (tabelas_ocasioes_imputadas, function (i)
  matrix(NA, 
         nrow=length(especie),
         ncol = length (seq (max(as.numeric (colnames (i)))+1,
                             maximo_ocasioes)),
         dimnames = list(NULL,
                         seq (max(as.numeric (colnames (i)))+1,
                              maximo_ocasioes)))
)

# # # # # # IMPORTANTE!

### imputar as ocasioes
## esta eh a tabela completa, com as sp nas linhas, transeccoes colunas, e sitios na lista (3a dimensao)

tab_completa_site_ocasiao <- lapply (seq(1,length(tabelas_ocasioes_imputadas)), function (i)

    cbind (tabelas_ocasioes_imputadas [[i]], imputar_maximo_ocasioes [[i]])

    )

## um dos sitios já tinha 58 colunas, então ele add duas colunas desnecessarias
## neste caso, substituir pela tabela antiga
tab_completa_site_ocasiao [which(lapply (tab_completa_site_ocasiao,ncol) > maximo_ocasioes)] <- tabelas_ocasioes_imputadas [which(lapply (tab_completa_site_ocasiao,ncol) > maximo_ocasioes)]

## agora tenho que colocar a especie na terceira dimensao (antes era o sitio),
### e depois transformar em array
lista_sp_sitio <- lapply (seq (1,length(especie)), function (k) ## especie
  lapply (tab_completa_site_ocasiao, function (i)## para cada sitio
    
    i [k,]                   ## desmanche tudo pra colocar na ordem correta
  )
)

names(lista_sp_sitio) <- especie

# transformar em tabela novamente, de sitio na linha e ocasiao na coluna, e sp na 3a dimensao
tab_completa_site_ocasiao <- lapply (lista_sp_sitio, function (i)
  
  matrix (unlist(i),
          nrow = length(names(tab_especies_imput)),
          ncol = maximo_ocasioes,
          dimnames = list(names(tab_especies_imput)),
          byrow=T)
)

### transformar a lista em array

arranjo_deteccoes_sitio_transeccao <-   array(unlist(tab_completa_site_ocasiao), 
                                              dim = c(nrow(tab_completa_site_ocasiao[[1]]), 
                                                      ncol(tab_completa_site_ocasiao[[1]]), 
                                                      length(tab_completa_site_ocasiao)))
## transformar em 0 e 1 ( deteccao e nao deteccao )
arranjo_deteccoes_sitio_transeccao [arranjo_deteccoes_sitio_transeccao > 1] <- 1

#######################################################################################
#######################################################################################
################         covariaveis de sitio                   #######################
#######################################################################################
#######################################################################################

## de sitio (aquelas que variam por sitio)

## sitios do nordeste
nord <- cast(peixes_subset,
             formula = locality_site  ~ Region,
             value= "IndCounting",
             fun.aggregate = sum)

## colocar na ordem da tabela 
nord <- nord [match (rownames (tab_completa_site_ocasiao [[1]]),nord$locality_site),]
## regiao 
ilhas <-  ifelse(nord [,3] >0,"oc.isl",0)
nor <-  ifelse(nord [,2] >0,"nord",0)
sud <-  ifelse(nord [,4] >0,"sud",0)
regiao <-  c(ilhas[which(ilhas !=0)],
             nor[which(nor !=0)],
             sud[which(sud !=0)])

## profundidade do sitio de amostragem
## fundo ou raso
prof <- cast(peixes_subset,
             formula = locality_site  ~ eventDepth,
             value= "IndCounting",
             fun.aggregate = sum)
prof <- prof [match (rownames (tab_completa_site_ocasiao [[1]]),prof$locality_site),]
prof$fundo <- ifelse (prof$fundo > 0,2,0)
prof$raso <- ifelse (prof$raso > 0,1,0)

# cf ordem
# prof$locality_site == rownames(tab_completa_site_ocasiao[[1]])

prof <- prof [,-1] ## remover os nomes 
prof <- apply (prof,1,max) ## obter o maximo da linha (transformr em vetor onde 1=raso, 2=fundo)

## tipo de recife
tipo_recife <- cast(bentos_subset,
                    formula = eventID_MOD  ~ Reef,
                    value= "Cover",
                    fun.aggregate = max)

tipo_recife <- tipo_recife [match (rownames (tab_completa_site_ocasiao[[1]]), 
                                   gsub ("ne_reefs.","",gsub ("se_reefs.","",gsub ("oc_isl.","", tipo_recife$eventID_MOD)))),]

recife_biog <- as.factor (ifelse (tipo_recife  [,2] >0, "1","0") )

## lista das covariaveis

covariates_site <- list (biog_reef = recife_biog,
                         depth = prof, 
                         region = regiao,
                         site_names = tipo_recife$eventID_MOD)

#######################################################################################
#######################################################################################
################         covariaveis de observacao              #######################
#######################################################################################
#######################################################################################

## covariaveis que variam por sitio e por transeccao (neste caso, o observador)

trans_obs <-  lapply (unique_comb_locality_site, function (i)
  cast (peixes_subset [which(peixes_subset$locality_site == i),], 
        locality_site~transection_number,
        value="ID.observer",
        fun.aggregate = min))

## obs to add, NA nos transectos inexistentes
## 83 eh o maximo de transeccoes em uma localidade (noronha)
obs_to_add <- lapply (trans_obs, function (i) 
  matrix (NA, nrow=1,
          ncol = length (seq (1,maximo_ocasioes) [which (seq (1,maximo_ocasioes) %in% colnames (i) [-1]  == F)]),
          dimnames = list (NULL,
                           seq (1,maximo_ocasioes) [which (seq (1,maximo_ocasioes) %in% colnames (i) [-1]  == F)]
          )
  )
)

## juntar os transectos existentes e inexistentes
binded_obs <- lapply (seq(1,length(obs_to_add)), function (i)
  
  cbind(trans_obs[[i]][,-1],obs_to_add[[i]])
  
)

## botar em ordem
binded_obs <- lapply (binded_obs, function (i)
  i [order (as.numeric(colnames(i)))]
)
names(binded_obs) <- unique_comb_locality_site

## tabela final
tabela_obs <- do.call(rbind, binded_obs)

#######################################################################################
#######################################################################################
### comecar a formatar os dados dos corais para os modelos de ocupacao de sitios
#######################################################################################
#######################################################################################

## obter dados de cobertura dos corais

sitios_bentos <- unique (bentos_subset$eventID_MOD)
sitios_bentos <- gsub ("oc_isl.","",gsub ("se_reefs.","",gsub ("ne_reefs.","",sitios_bentos)))

## fechar com os nomes dos sitios dos dados de peixes
sitios_bentos <- sitios_bentos[match(rownames (tab_completa_site_ocasiao[[1]]), sitios_bentos)]

## editar tb na tabela original
bentos_subset$eventID_MOD <- gsub ("oc_isl.","",gsub ("se_reefs.","",gsub ("ne_reefs.","",bentos_subset$eventID_MOD)))

## uma tabela dinamica por sitio, com a especie na linha, e o video na coluna
cob_bentos <- lapply (sitios_bentos, function (i)
  
        cast(bentos_subset [which (bentos_subset$eventID_MOD == i),],
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
                                              dimnames = list (NULL,
                                                               NULL,
                                                               sp_corais))
## obter informacao combinada das 3 spp de agaricia
#agaricia_sp_cover <- apply (arranjo_cob_coral_sitio_video [,,grep("Agaricia", sp_corais)],
#                      c(1,2), sum)

## colar no array dos corais mais frequentes
## array final
#arranjo_cob_coral_sitio_video <- abind(agaricia_sp_cover, arranjo_cob_coral_sitio_video )

## ajustar os nomes
#dimnames(arranjo_cob_coral_sitio_video)[[3]][1] <- "Agaricia.spp"


## transformar cobertura em dado binario (deteccao e nao deteccao do coral k no sitio i, video j)
arranjo_corais <- arranjo_cob_coral_sitio_video
## remover dados do video 16 (que nao tem dados)
arranjo_corais <- arranjo_corais [,-16,]

## dados de coebrtura de cada espécie de coral retiva a cobertura de coral do videoplot
## cobertura total de coral no video k, sitio i
cover_data <- apply(arranjo_corais,c(1,2),sum,na.rm=T) 
## cobertura de cada sp no video relativo ao total, using for =[

shell_array <- array (NA,dim = dim(arranjo_corais)) 

for (i in 1:dim(arranjo_corais)[3]) {
  
  shell_array[,,i] <- arranjo_corais [,,i]/cover_data
  
}

## maximo de cobertura detectado em um video
## NA porque em alguns sitios nao foi registrado nada de coral
sp_cover_data <- apply (shell_array,c(1,3),max,na.rm=T)
# infinite  eh gerado para os locais onde nao tem  nada de coral
sp_cover_data [is.infinite (sp_cover_data)] <-  0
colnames(sp_cover_data) <- dimnames(arranjo_corais )[[3]]

## remover spp que ocorreram em poucos locais (menos min_sites)

site_coral_detection <- ifelse (sp_cover_data>0,1,0)
min_sites <- 4

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
coordenadas <- aggregate(bentos_subset, 
                         by= list (bentos_subset$eventID_MOD), 
                         FUN=mean)[c("Group.1","Lon","Lat")]

# organizar a ordem dos nomes
coordenadas <- coordenadas [match(sitios_bentos,coordenadas$Group.1),]

##########################################################################
##########################################################################
###  evitar mismatch espacial de ocorrencia de coral e peixe

site_peixe_detection <- apply(arranjo_deteccoes_sitio_transeccao,c(1,3),max,na.rm=T)
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
  
  arranjo_deteccoes_sitio_transeccao [,,which (i==T)]
  
)

##  finalmente, transformar os dados de peixe em formato longo

df_fish_data <- lapply (subset_peixes, function (coral) ## para cada data set de coral
  lapply (seq(1,dim(coral)[3]), function (sp) { ## para cada sp de peixe
  
  ## transforma matriz em vetor
  y_long <- as.numeric(coral [,,sp])
  tabela_obs_long <- as.numeric (as.matrix(tabela_obs))
  
  ## ocasioes
  df_data <- data.frame(obs= seq(1,length(y_long)),
                        y= y_long,
                        ID= tabela_obs_long,
                        M = rep (seq (1,dim(coral)[1]), 
                                 ncol(tabela_obs)),
                        J = unlist(
                          lapply (seq(1,ncol(tabela_obs)), function (i) 
                            rep (i,dim(coral)[1]))
                        ),
                        prof = prof
  )
  
  ## remover NAs
  df_data <- df_data[which (is.na(df_data$ID) != T),]
  df_data$n.obs <- seq (1,nrow(df_data)) ## new obs - sequence disconsidering NAs
  
  ##  nao existe o observador 8, ajustar isto diminuindo valores >8 por 1
  df_data$ID <- ifelse (df_data$ID >8,
                        df_data$ID-1, 
                        df_data$ID)
  
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
fish_species <- lapply (over_peixe_coral, function (i)
  
  especie[which(i == T)]
  
)
  
###############################################
# # # # # # # # #  SAVE # # # # # # # # # # # #
###############################################

### save data - para os modelos de coral

save (arranjo_corais, ### dados de cobertura 
      df_coral_data, ## df em formato longo para modelagem
      sp_cover_data, ## cobertura de cada coral relativo a cobertura de coral total
      nvideos, ## numero de videos por sitio
      coordenadas,# coordenadas dos sitios
      #sp_coral,## nomes de todas as sp de coral
      coral_species,## id das especies de coral analisadas
      sitios_bentos,# nome dos sitios
      file=here ("output","Data_coral_detection.RData"))

### salvar os dados para o modelo de ocupacao dos peixes

save (arranjo_deteccoes_sitio_transeccao,
      df_fish_data_per_coral,## dados de peixes para a modelagem
      covariates_site, ## covariaveis de sitio
      todas_sp_Morais,## a id de todas as especies de peixes
      fish_species,## id dos peixes analisados, para cada coral
      sp_fundo_sed_fam,## traits do subset dos peixes
      file = here("output", "Data_fish_detection.RData"))



######################################################################
######################################################################
## comecar a formatar o subset de dados de peixes para os modelos   ##
## DADOS DE Longo ET AL. 2019                                      ##
######################################################################
######################################################################

### quais as localidades com dados nos subsets ???

## editar IDs e obter dados dos transectos
comb_locality_site_transect <- paste(L.peixes_subset$location, 
                                     L.peixes_subset$site,
                                     L.peixes_subset$depth_m, 
                                     L.peixes_subset$,sep=".")

## obter os sitios unicos
comb_locality_site <- paste(peixes_subset$Locality, 
                            peixes_subset$Site,
                            peixes_subset$eventDepth,sep=".")
unique_comb_locality_site <- unique (comb_locality_site)

## o proximo passo eh criar um vetor dizendo quanto eventos amostrais (Transeccoes) cada sitio teve,
## e cole na tabela original
peixes_subset <- cbind(peixes_subset, transection_number = 
                         unlist (
                           lapply (unique_comb_locality_site, function (i)## para cada combinacao
                             as.numeric( ## cada nivel do fator como uma  contagem de repeticoes
                               factor (comb_locality_site_transect [which(comb_locality_site %in%  i )] ## transformar em fator
                                       ### o caracter vai se transformar em numero inteiro automaticamente
                               )
                             )
                           )
                         )
)

## dados basicos
## number of unique transects; number of unique localities; number of unique sites
length(unique (peixes_subset$Transect_id)); length(unique (peixes_subset$Locality)); length(unique (peixes_subset$Site))

## agora, ver quais especies de peixes restaram no subconjunto dos dados
especie <- unique (peixes_subset$ScientificName)
## criara um novo descritor de sitio, desta vez editado para ficar com formato similar aos dados de bentos
peixes_subset$locality_site <- paste(peixes_subset$Locality, 
                                     peixes_subset$Site,
                                     peixes_subset$eventDepth,sep=".")

## como o cast remove as transeccoes sem deteccao, temos que imputa-las
## primeiro tirar fora as spp que ocorreram em menos de 10 sitios

sp_dados_suficientes <- unlist(
  lapply (especie, function (i)
    length (unique (peixes_subset [which (peixes_subset$ScientificName %in% i),"locality_site"])) >= 10
  )
)

## especie com dados suficientes
## ESTE EH O POOL DE SP DA ANALISE
especie <- especie [which (sp_dados_suficientes == T)]

# colocar em ordem alfabetica
especie  <- especie [order(especie)]

## agora remover dos dados as especies que ocorrem em menos de 10 sitios
peixes_subset <- peixes_subset [which(peixes_subset$ScientificName %in% especie),]

## fazer uma tabela para cada sitio,  composta pela especie (linha) e transeccao (coluna)
tab_especies <- lapply (unique (peixes_subset$locality_site), function (i) ## para cada sitio
  
  cast(peixes_subset [peixes_subset$locality_site == i,], ## construa uma tabela dinamica de dados
       formula = ScientificName  ~ transection_number,
       value= "IndCounting",
       fun.aggregate = sum)
  
)

## adicionar as especies que nao foram detectadas em um dado sitio
## ja que o cast as remove da tabela

sp_da_coluna1 <- lapply (tab_especies, function (i) 
  
  data.frame (ScientificName= as.character(especie[especie %in% i$ScientificName == F]))
  
)

## fazer uma tabela vazia para imputacao  
## especies faltantes na linha
## transeccoes faltantes na coluna
sp_para_imputar <- lapply (seq(1,length(sp_da_coluna1)), function (i)
  cbind (sp_da_coluna1 [[i]], ## colar nos nomes as deteccoes 
         matrix(0, 
                nrow = length (especie[especie %in% tab_especies[[i]]$ScientificName == F]), ## sp que nao estao na tabela
                ncol= ncol (tab_especies[[i]])-1,## -1 pra tirar o nome das sp
                dimnames = list (NULL,
                                 colnames(tab_especies[[i]])[-1])
                
                
         )
  )
)

# imputar as sp atraves do rbind 
tab_especies_imput <- lapply (seq(1,length(sp_para_imputar)), function (i)
  
  rbind (tab_especies[[i]], sp_para_imputar[[i]])
  
)

## ordenar as linhas das tabelas por ordem alfabetica
tab_especies_imput <- lapply (tab_especies_imput, function (i)
  
  i[order (i$ScientificName),]
  
)

# nomear a lista com o nome dos sitios
names(tab_especies_imput) <- unique (peixes_subset$locality_site)

# trabalhar a dimensao das ocasioes

## primeiro, colocar transeccoes que estao faltando no meio das existentes
# todas as transeccoes existentes
seq_ocasioes <- lapply (tab_especies_imput, function (i) 
  
  seq (1,max (as.numeric(colnames (i)[-1])))
)

## imputar em relacao as existentes e todas as possiveis (em relacao ao sitio com o n maximo de transeccoes -58) 
ocasioes_a_imputar <- lapply (seq(1,length(seq_ocasioes)), function (i)
  seq_ocasioes[[i]] [which(seq_ocasioes[[i]] %in% as.numeric(colnames (tab_especies_imput [[i]])[-1]) == F)]
)

## quais faltam
quais_faltam <-(which(lapply (ocasioes_a_imputar,length) >= 1))

## imputar somente nas tabelas que tem ocasioes faltando
ocasioes_a_imputar <- lapply (quais_faltam, function (i) # das que tem dados faltando
  ## construir uma matriz de zeros com o nrow igual o numero de especies
  # ncol igual ao numero de transeccoes faltantes
  matrix (0,
          nrow= length(especie),
          ncol=  length(seq_ocasioes[[i]] [which(seq_ocasioes[[i]] %in% as.numeric(colnames (tab_especies_imput [[i]])[-1]) == F)]),
          dimnames =list(NULL,
                         seq_ocasioes[[i]] [which(seq_ocasioes[[i]] %in% as.numeric(colnames (tab_especies_imput [[i]])[-1]) == F)])
  ))

# subset das tabelas para imputar
subset_imput <- tab_especies_imput [quais_faltam]
# colar as ocasioes faltantes
subset_imputadas <- lapply (seq(1,length(quais_faltam)), function (i)
  
  cbind(subset_imput [[i]],ocasioes_a_imputar[[i]]))

## ordenar as colunas por sequencia de ocasioes
subset_imputadas <- lapply (subset_imputadas, function (i) 
  i [,order(as.numeric (colnames (i)))]
) ## vai produzir NA porque uma coluna nao eh numerica

## nomear
names(subset_imputadas) <- unique (peixes_subset$locality_site) [quais_faltam]

## colar as duas listas
tabelas_ocasioes_imputadas <- c (tab_especies_imput [-quais_faltam], subset_imputadas)
tabelas_ocasioes_imputadas <- tabelas_ocasioes_imputadas[match(names(tab_especies_imput),names (tabelas_ocasioes_imputadas))]# == names(tab_especies_imput

## cf os nomes dos sitios nos dois grupos de dados (imputados e completos)
# names(tabelas_ocasioes_imputadas) == names(tab_especies_imput)

# remover a coluna do nome cientifico da sp.
tabelas_ocasioes_imputadas <- lapply (tabelas_ocasioes_imputadas, function (i)
  i [,-which(colnames(i) == "ScientificName")]
)

## agora, finalmente, colar as transeccoes inexistentes em um sitio
# encontrar o maximo de transeccoes existentes
maximo_ocasioes <- max(unlist(lapply (tabelas_ocasioes_imputadas, ncol)))

# colar
imputar_maximo_ocasioes <- lapply (tabelas_ocasioes_imputadas, function (i)
  matrix(NA, 
         nrow=length(especie),
         ncol = length (seq (max(as.numeric (colnames (i)))+1,
                             maximo_ocasioes)),
         dimnames = list(NULL,
                         seq (max(as.numeric (colnames (i)))+1,
                              maximo_ocasioes)))
)

# # # # # # IMPORTANTE!

### imputar as ocasioes
## esta eh a tabela completa, com as sp nas linhas, transeccoes colunas, e sitios na lista (3a dimensao)

tab_completa_site_ocasiao <- lapply (seq(1,length(tabelas_ocasioes_imputadas)), function (i)
  
  cbind (tabelas_ocasioes_imputadas [[i]], imputar_maximo_ocasioes [[i]])
  
)

## um dos sitios já tinha 58 colunas, então ele add duas colunas desnecessarias
## neste caso, substituir pela tabela antiga
tab_completa_site_ocasiao [which(lapply (tab_completa_site_ocasiao,ncol) > maximo_ocasioes)] <- tabelas_ocasioes_imputadas [which(lapply (tab_completa_site_ocasiao,ncol) > maximo_ocasioes)]

## agora tenho que colocar a especie na terceira dimensao (antes era o sitio),
### e depois transformar em array
lista_sp_sitio <- lapply (seq (1,length(especie)), function (k) ## especie
  lapply (tab_completa_site_ocasiao, function (i)## para cada sitio
    
    i [k,]                   ## desmanche tudo pra colocar na ordem correta
  )
)

names(lista_sp_sitio) <- especie

# transformar em tabela novamente, de sitio na linha e ocasiao na coluna, e sp na 3a dimensao
tab_completa_site_ocasiao <- lapply (lista_sp_sitio, function (i)
  
  matrix (unlist(i),
          nrow = length(names(tab_especies_imput)),
          ncol = maximo_ocasioes,
          dimnames = list(names(tab_especies_imput)),
          byrow=T)
)

### transformar a lista em array

arranjo_deteccoes_sitio_transeccao <-   array(unlist(tab_completa_site_ocasiao), 
                                              dim = c(nrow(tab_completa_site_ocasiao[[1]]), 
                                                      ncol(tab_completa_site_ocasiao[[1]]), 
                                                      length(tab_completa_site_ocasiao)))
## transformar em 0 e 1 ( deteccao e nao deteccao )
arranjo_deteccoes_sitio_transeccao [arranjo_deteccoes_sitio_transeccao > 1] <- 1

#######################################################################################
#######################################################################################
################         covariaveis de sitio                   #######################
#######################################################################################
#######################################################################################

## de sitio (aquelas que variam por sitio)

## sitios do nordeste
nord <- cast(peixes_subset,
             formula = locality_site  ~ Region,
             value= "IndCounting",
             fun.aggregate = sum)

## colocar na ordem da tabela 
nord <- nord [match (rownames (tab_completa_site_ocasiao [[1]]),nord$locality_site),]
## regiao 
ilhas <-  ifelse(nord [,3] >0,"oc.isl",0)
nor <-  ifelse(nord [,2] >0,"nord",0)
sud <-  ifelse(nord [,4] >0,"sud",0)
regiao <-  c(ilhas[which(ilhas !=0)],
             nor[which(nor !=0)],
             sud[which(sud !=0)])

## profundidade do sitio de amostragem
## fundo ou raso
prof <- cast(peixes_subset,
             formula = locality_site  ~ eventDepth,
             value= "IndCounting",
             fun.aggregate = sum)
prof <- prof [match (rownames (tab_completa_site_ocasiao [[1]]),prof$locality_site),]
prof$fundo <- ifelse (prof$fundo > 0,2,0)
prof$raso <- ifelse (prof$raso > 0,1,0)

# cf ordem
# prof$locality_site == rownames(tab_completa_site_ocasiao[[1]])

prof <- prof [,-1] ## remover os nomes 
prof <- apply (prof,1,max) ## obter o maximo da linha (transformr em vetor onde 1=raso, 2=fundo)

## tipo de recife
tipo_recife <- cast(bentos_subset,
                    formula = eventID_MOD  ~ Reef,
                    value= "Cover",
                    fun.aggregate = max)

tipo_recife <- tipo_recife [match (rownames (tab_completa_site_ocasiao[[1]]), 
                                   gsub ("ne_reefs.","",gsub ("se_reefs.","",gsub ("oc_isl.","", tipo_recife$eventID_MOD)))),]

recife_biog <- as.factor (ifelse (tipo_recife  [,2] >0, "1","0") )

## lista das covariaveis

covariates_site <- list (biog_reef = recife_biog,
                         depth = prof, 
                         region = regiao,
                         site_names = tipo_recife$eventID_MOD)

#######################################################################################
#######################################################################################
################         covariaveis de observacao              #######################
#######################################################################################
#######################################################################################

## covariaveis que variam por sitio e por transeccao (neste caso, o observador)

trans_obs <-  lapply (unique_comb_locality_site, function (i)
  cast (peixes_subset [which(peixes_subset$locality_site == i),], 
        locality_site~transection_number,
        value="ID.observer",
        fun.aggregate = min))

## obs to add, NA nos transectos inexistentes
## 83 eh o maximo de transeccoes em uma localidade (noronha)
obs_to_add <- lapply (trans_obs, function (i) 
  matrix (NA, nrow=1,
          ncol = length (seq (1,maximo_ocasioes) [which (seq (1,maximo_ocasioes) %in% colnames (i) [-1]  == F)]),
          dimnames = list (NULL,
                           seq (1,maximo_ocasioes) [which (seq (1,maximo_ocasioes) %in% colnames (i) [-1]  == F)]
          )
  )
)

## juntar os transectos existentes e inexistentes
binded_obs <- lapply (seq(1,length(obs_to_add)), function (i)
  
  cbind(trans_obs[[i]][,-1],obs_to_add[[i]])
  
)

## botar em ordem
binded_obs <- lapply (binded_obs, function (i)
  i [order (as.numeric(colnames(i)))]
)
names(binded_obs) <- unique_comb_locality_site

## tabela final
tabela_obs <- do.call(rbind, binded_obs)

#######################################################################################
#######################################################################################
### comecar a formatar os dados dos corais para os modelos de ocupacao de sitios
#######################################################################################
#######################################################################################

## obter dados de cobertura dos corais

sitios_bentos <- unique (bentos_subset$eventID_MOD)
sitios_bentos <- gsub ("oc_isl.","",gsub ("se_reefs.","",gsub ("ne_reefs.","",sitios_bentos)))

## fechar com os nomes dos sitios dos dados de peixes
sitios_bentos <- sitios_bentos[match(rownames (tab_completa_site_ocasiao[[1]]), sitios_bentos)]

## editar tb na tabela original
bentos_subset$eventID_MOD <- gsub ("oc_isl.","",gsub ("se_reefs.","",gsub ("ne_reefs.","",bentos_subset$eventID_MOD)))

## uma tabela dinamica por sitio, com a especie na linha, e o video na coluna
cob_bentos <- lapply (sitios_bentos, function (i)
  
  cast(bentos_subset [which (bentos_subset$eventID_MOD == i),],
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
                                         dimnames = list (NULL,
                                                          NULL,
                                                          sp_corais))
## obter informacao combinada das 3 spp de agaricia
#agaricia_sp_cover <- apply (arranjo_cob_coral_sitio_video [,,grep("Agaricia", sp_corais)],
#                      c(1,2), sum)

## colar no array dos corais mais frequentes
## array final
#arranjo_cob_coral_sitio_video <- abind(agaricia_sp_cover, arranjo_cob_coral_sitio_video )

## ajustar os nomes
#dimnames(arranjo_cob_coral_sitio_video)[[3]][1] <- "Agaricia.spp"


## transformar cobertura em dado binario (deteccao e nao deteccao do coral k no sitio i, video j)
arranjo_corais <- arranjo_cob_coral_sitio_video
## remover dados do video 16 (que nao tem dados)
arranjo_corais <- arranjo_corais [,-16,]

## dados de coebrtura de cada espécie de coral retiva a cobertura de coral do videoplot
## cobertura total de coral no video k, sitio i
cover_data <- apply(arranjo_corais,c(1,2),sum,na.rm=T) 
## cobertura de cada sp no video relativo ao total, using for =[

shell_array <- array (NA,dim = dim(arranjo_corais)) 

for (i in 1:dim(arranjo_corais)[3]) {
  
  shell_array[,,i] <- arranjo_corais [,,i]/cover_data
  
}

## maximo de cobertura detectado em um video
## NA porque em alguns sitios nao foi registrado nada de coral
sp_cover_data <- apply (shell_array,c(1,3),max,na.rm=T)
# infinite  eh gerado para os locais onde nao tem  nada de coral
sp_cover_data [is.infinite (sp_cover_data)] <-  0
colnames(sp_cover_data) <- dimnames(arranjo_corais )[[3]]

## remover spp que ocorreram em poucos locais (menos min_sites)

site_coral_detection <- ifelse (sp_cover_data>0,1,0)
min_sites <- 4

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
coordenadas <- aggregate(bentos_subset, 
                         by= list (bentos_subset$eventID_MOD), 
                         FUN=mean)[c("Group.1","Lon","Lat")]

# organizar a ordem dos nomes
coordenadas <- coordenadas [match(sitios_bentos,coordenadas$Group.1),]

##########################################################################
##########################################################################
###  evitar mismatch espacial de ocorrencia de coral e peixe

site_peixe_detection <- apply(arranjo_deteccoes_sitio_transeccao,c(1,3),max,na.rm=T)
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
  
  arranjo_deteccoes_sitio_transeccao [,,which (i==T)]
  
)

##  finalmente, transformar os dados de peixe em formato longo

df_fish_data <- lapply (subset_peixes, function (coral) ## para cada data set de coral
  lapply (seq(1,dim(coral)[3]), function (sp) { ## para cada sp de peixe
    
    ## transforma matriz em vetor
    y_long <- as.numeric(coral [,,sp])
    tabela_obs_long <- as.numeric (as.matrix(tabela_obs))
    
    ## ocasioes
    df_data <- data.frame(obs= seq(1,length(y_long)),
                          y= y_long,
                          ID= tabela_obs_long,
                          M = rep (seq (1,dim(coral)[1]), 
                                   ncol(tabela_obs)),
                          J = unlist(
                            lapply (seq(1,ncol(tabela_obs)), function (i) 
                              rep (i,dim(coral)[1]))
                          ),
                          prof = prof
    )
    
    ## remover NAs
    df_data <- df_data[which (is.na(df_data$ID) != T),]
    df_data$n.obs <- seq (1,nrow(df_data)) ## new obs - sequence disconsidering NAs
    
    ##  nao existe o observador 8, ajustar isto diminuindo valores >8 por 1
    df_data$ID <- ifelse (df_data$ID >8,
                          df_data$ID-1, 
                          df_data$ID)
    
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
fish_species <- lapply (over_peixe_coral, function (i)
  
  especie[which(i == T)]
  
)

###############################################
# # # # # # # # #  SAVE # # # # # # # # # # # #
###############################################

### save data - para os modelos de coral

save (arranjo_corais, ### dados de cobertura 
      df_coral_data, ## df em formato longo para modelagem
      sp_cover_data, ## cobertura de cada coral relativo a cobertura de coral total
      nvideos, ## numero de videos por sitio
      coordenadas,# coordenadas dos sitios
      #sp_coral,## nomes de todas as sp de coral
      coral_species,## id das especies de coral analisadas
      sitios_bentos,# nome dos sitios
      file=here ("output","Data_coral_detection.RData"))

### salvar os dados para o modelo de ocupacao dos peixes

save (arranjo_deteccoes_sitio_transeccao,
      df_fish_data_per_coral,## dados de peixes para a modelagem
      covariates_site, ## covariaveis de sitio
      todas_sp_Morais,## a id de todas as especies de peixes
      fish_species,## id dos peixes analisados, para cada coral
      sp_fundo_sed_fam,## traits do subset dos peixes
      file = here("output", "Data_fish_detection.RData"))





