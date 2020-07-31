## arquivos para analise da relacao peixe-coral
# dados
Updated_compiled_quadrats_allsites = dados bentos Aued et al. 2018
UpdatedData_RMorais_et_al_2017 = dados peixes - Morais et al. 2017

Dados_site_occ_mod = dados organizados para modelos, em formato array

## codigos R (em passos numerados)

1.codigo_organizacao_dados = organizacao dos dados para modelagem, script em r. Neste codigo, organizo os ados de peixes e corais para as analises.
2.codigo_modelagem_coral = este arquivo tem duas partes: 
 a) PARTE SUPERIOR: compilacao de um modelo CARNormal para modelagem da ocupacao de sitios dos corais, em linguagem BUGS;
 b) PARTE INFERIOR: comandos para carregar os dados e rodar os modelos, em linguagem R.
3.codigo_modelagem_peixe = este arquivo tem duas partes: 
 a) PARTE SUPERIOR: compilacao de quatro diferentes modelos para modelagem da ocupacao de sitios dos peixes, em linguagem BUGS;
 b) PARTE INFERIOR: comandos para carregar os dados e rodar os modelos, em linguagem R.

# Rproj
Manuscrito_coral_peixes.Rproj = Rproject destas analises

# modelos formato array
StaticModel_ID_obs = modelo com efeito de coral na ocupacao, e da profundidade e da ID dos observadores na deteccao
StaticModelOcc_IDobsRdmP = modelo com efeito de coral na ocupacao, e da ID dos observadores e um efeito aleatorio da transeccao na deteccao
StaticModelDepthOcc_IDobsRdmP - modelo com efeito do coral e da profundidade na ocupacao, e da ID dos observadores e um efeito aleatorio na deteccao
StaticModel_ID_obsRndmEff = modelo com efeito de coral na ocupacao, e da profundidade, ID dos observadores, e um efeito aleatorio da transeccao na deteccao

## resultados
model_effObsID_depth.RData = resultado do modelo com profundidade e ID do observador
