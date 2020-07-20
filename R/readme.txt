## arquivos para analise da relacao peixe-coral
# dados
Updated_compiled_quadrats_allsites = dados bentos Aued et al. 2018
UpdatedData_RMorais_et_al_2017 = dados peixes - Morais et al. 2017

Dados_site_occ_mod = dados organizados para modelos, em formato array

## codigos R
codigo_organizacao_dados = organizacao dos dados para modelagem, script em r
codigo_modelos = txt com a escrita dos modelos, em linguagem BUGS, script em r
codigo_modelo_long = codigos para modelo em formato long data frame, modelo em linguagem BUGS, scripts em r
codigo_interpretacao = codigos para interpretacao dos resultados, script em r

# Rproj
Manuscrito_coral_peixes.Rproj = Rproject destas analises

# modelos formato array
StaticModel_obs = modelo com efeito da profundidade e do numero de observadors - GOF inadequado
StaticModel = modelo com efeito somente da profundidade - GOF inadequado
StaticModel_GOF_agg - modelo com efeito de profundidade = GOF correto (para formato array)
StaticModel_OBS_GOF_agg = modelo com efeito de profundidade e numero de observadores (GOF correto para formato rray)

## modelo em formato long
StaticModel_ID_obs = modelo com efeito profundidade e ID obs, p variando por transecto (random factor), GOF adequado para formato long

## resultados
model_effObsID_depth.RData = resultado do modelo com profundidade e ID do observador
