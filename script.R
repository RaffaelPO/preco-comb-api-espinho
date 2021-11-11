library(tidyverse)
library(jsonlite)
library(lubridate)
library(glue)
library(stringr)

date <- Sys.Date()
date_long <- glue('{day(date)} de {month(date, label = T, abbr = F, locale = "pt_PT")} de {year(date)}')

data_daily <- fromJSON(glue('https://precoscombustiveis.dgeg.gov.pt/api/PrecoComb/PMD?dataIni=2000-01-01&dataFim={date}'))

data_daily <- data_daily[["resultado"]]

gasoleo <- data_daily %>%
  filter(TipoCombustivel == 'Gasóleo simples') %>%
  select(Data, PrecoMedio, PrecoMin, PrecoMax) %>%
  arrange(Data) %>%
  filter(Data > as.Date('2015-09-23')) %>%
  mutate(PrecoMedio = gsub(' €','',PrecoMedio)) %>%
  mutate(PrecoMedio = gsub(',','.',PrecoMedio)) %>%
  mutate(PrecoMedio = as.numeric(PrecoMedio)) %>%
  mutate(PrecoMin = gsub(' €','',PrecoMin)) %>%
  mutate(PrecoMin = gsub(',','.',PrecoMin)) %>%
  mutate(PrecoMin = as.numeric(PrecoMin)) %>%
  mutate(PrecoMax = gsub(' €','',PrecoMax)) %>%
  mutate(PrecoMax = gsub(',','.',PrecoMax)) %>%
  mutate(PrecoMax = as.numeric(PrecoMax))

gasolina <- data_daily %>%
  filter(TipoCombustivel == 'Gasolina simples 95') %>%
  select(Data, PrecoMedio, PrecoMin, PrecoMax) %>%
  arrange(Data) %>%
  filter(Data > as.Date('2015-09-23')) %>%
  mutate(PrecoMedio = gsub(' €','',PrecoMedio)) %>%
  mutate(PrecoMedio = gsub(',','.',PrecoMedio)) %>%
  mutate(PrecoMedio = as.numeric(PrecoMedio)) %>%
  mutate(PrecoMin = gsub(' €','',PrecoMin)) %>%
  mutate(PrecoMin = gsub(',','.',PrecoMin)) %>%
  mutate(PrecoMin = as.numeric(PrecoMin)) %>%
  mutate(PrecoMax = gsub(' €','',PrecoMax)) %>%
  mutate(PrecoMax = gsub(',','.',PrecoMax)) %>%
  mutate(PrecoMax = as.numeric(PrecoMax))


#GET POSTOS

postos <- fromJSON('https://precoscombustiveis.dgeg.gov.pt/api/PrecoComb/PesquisarPostos?idsTiposComb=&idMarca=&idTipoPosto=&idDistrito=&qtdPorPagina=99999')
postos <- postos[["resultado"]]

concelhosPre <- read_delim('concelhos-metadata.csv', delim = ';') %>%
  select(designacao, pre, pre2, pre3) %>%
  rename('Municipio' = 'designacao')

postos_gasolina <- postos %>%
  filter(Combustivel == 'Gasolina simples 95') %>%
  mutate(Preco = gsub(' €','',Preco)) %>%
  mutate(Preco = gsub(',','.',Preco)) %>%
  mutate(Preco = as.numeric(Preco)) %>%
  mutate(Municipio = gsub('Guimares','Guimarães',Municipio)) %>%
  mutate(Municipio = gsub('Lagoa','Lagoa (Faro)',Municipio)) %>%
  mutate(Municipio = gsub('Melgao','Melgaço',Municipio)) %>%
  select(Nome,Municipio, Preco, Morada, Localidade, Latitude,Longitude)


postos_gasoleo <- postos %>%
  filter(Combustivel == 'Gasóleo simples') %>%
  mutate(Preco = gsub(' €','',Preco)) %>%
  mutate(Preco = gsub(',','.',Preco)) %>%
  mutate(Preco = as.numeric(Preco)) %>%
  mutate(Municipio = gsub('Guimares','Guimarães',Municipio)) %>%
  mutate(Municipio = gsub('Lagoa','Lagoa (Faro)',Municipio)) %>%
  mutate(Municipio = gsub('Melgao','Melgaço',Municipio)) %>%
  select(Nome,Municipio, Preco,Morada, Localidade, Latitude,Longitude)


media_concelho_gasolina <- postos_gasolina %>%
  group_by(Municipio) %>%
  summarise(mean_gasolina = mean(Preco),
            n_gasolina = n()) %>%
  mutate(mean_gasolina = ifelse(n_gasolina == 1, NA, mean_gasolina))

media_concelho_gasoleo <- postos_gasoleo %>%
  group_by(Municipio) %>%
  summarise(mean_gasoleo = mean(Preco),
            n_gasoleo = n()) %>%
  mutate(mean_gasoleo = ifelse(n_gasoleo == 1, NA, mean_gasoleo))

concelhos <- postos_gasoleo %>%
  left_join(concelhosPre) %>%
  select(Municipio, pre, pre2, pre3) %>%
  distinct() %>%
  arrange(Municipio) %>%
  mutate(value = Municipio,
         label = Municipio) %>%
  left_join(media_concelho_gasolina) %>%
  left_join(media_concelho_gasoleo) %>%
  mutate(mean_gasolina = round(mean_gasolina, 3)) %>%
  mutate(mean_gasoleo = round(mean_gasoleo, 3))
  select(-Municipio)



data <- list(
  date_update = date_long,
  date_today = date,
  date_2years = (date -years(2)) -1,
  gasoleo = gasoleo,
  gasolina = gasolina,
  concelhos = concelhos,
  postos = list(
    gasolina = postos_gasolina,
    gasoleo = postos_gasoleo
  )
)

data <- data %>% toJSON(pretty = FALSE, auto_unbox = TRUE, na = "null")

data %>% write('data.json')
