## dados formatados para os modelos

#  de peixes
Data_fish_detection.RData = dataframe em formato longo com dados de deteccao e nao-deteccao dos peixes (sp. que ocorreram em mais de 6 sitios)

# de corais
Data_coral_detection.RData = dataframe em formato longo com dados de deteccao e nao-deteccao dos corais (sp. que ocorreram em mais de 6 sitios)

## estes ultimos dados de coral suportaram os modelos para gerar este proximo dado:
coral_occupancy_data.RData = dados de coral para modelagem dos peixes, contendo a cobertura original, e a ocupação estimada pelo modelo espacial com neighborhood distance (d2) = 12 lat-long degrees
 

## resultados dos modelos

# corais
samples_coral_CARModel.RData = resultados de cada um dos oito modelos espaciais de ocupacao de coral, 
com 8 diferentes neighborhood distances (d2),

# peixes
samples_OCCcoral_PdepthObsID_gen.RData = resultado do modelo com efeito de coral na ocupacao, e da profundidade e da ID dos observadores na deteccao