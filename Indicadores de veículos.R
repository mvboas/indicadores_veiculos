#################################################################################################################
#GECON
#ÁREA:CRÉDITO
#PLANILHA INDICADORES DE CRÉDITO PARA VEÍCULOS
#MARCELO VILAS BOAS DE CASTRO
#DATA:26-04-2019
#################################################################################################################

#PACOTES REQUERIDOS:
#INSTALAR QUANDO NECESSÁRIO
#EXEMPLO:install.packages("pryr")
#library(xlsx)
library(RCurl)
library(XML)
library(rio)

#DEFINIR PASTAS DE RESULTADOS:
getwd()
setwd("C:\\Users\\User\\Documents")

#Criando função para coleta de séries
coleta_dados_sgs = function(series,datainicial="01/03/2011", datafinal = format(Sys.time(), "%d/%m/%Y")){
  #Argumentos: vetor de séries, datainicial que pode ser manualmente alterada e datafinal que automaticamente usa a data de hoje
  #Cria estrutura de repetição para percorrer vetor com códigos de séries e depois juntar todas em um único dataframe
  for (i in 1:length(series)){
    dados = read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",series[i],"/dados?formato=csv&dataInicial=",datainicial,"&dataFinal=",datafinal,sep="")),sep=";")
    dados[,-1] = as.numeric(gsub(",",".",dados[,-1])) #As colunas do dataframe em objetos numéricos exceto a da data
    nome_coluna = series[i] #Nomeia cada coluna do dataframe com o código da série
    colnames(dados) = c('data', nome_coluna)
    nome_arquivo = paste("dados", i, sep = "") #Nomeia os vários arquivos intermediários que são criados com cada série
    assign(nome_arquivo, dados)
    
    if(i==1)
      base = dados1 #Primeira repetição cria o dataframe
    else
      base = merge(base, dados, by = "data", all = T) #Demais repetições agregam colunas ao dataframe criado
    print(paste(i, length(series), sep = '/')) #Printa o progresso da repetição
  }
  
  base$data = as.Date(base$data, "%d/%m/%Y") #Transforma coluna de data no formato de data
  base = base[order(base$data),] #Ordena o dataframe de acordo com a data
  return(base)
}

#Coletando séries
series=c(20553, 20556, 20581, 20584, 20645, 20648, 20673, 20676, 20728, 20731, 20749, 20752, 20864, 20867, 20886, 20889, 20938, 20941, 20963, 20966, 21017, 21020, 21042, 21045, 21096, 21099, 21121, 21124, 25447, 25450, 25471, 25474, 27658, 27660, 27680, 27682, 27735)

base <- coleta_dados_sgs(series)

names(base)=c("Data", "20553 - Saldo da carteira de crédito com recursos livres - Pessoas jurídicas - Aquisição de veículos - R$ (milhões", "20556 - Saldo da carteira de crédito com recursos livres - Pessoas jurídicas - Arrendamento mercantil de veículos - R$ (milhões)", "20581 - Saldo da carteira de crédito com recursos livres - Pessoas físicas - Aquisição de veículos - R$ (milhões)", "20584 - Saldo da carteira de crédito com recursos livres - Pessoas físicas - Arrendamento mercantil de veículos - R$ (milhões)", "20645 - Concessões de crédito com recursos livres - Pessoas jurídicas - Aquisição de veículos - R$ (milhões)", "20648 - Concessões de crédito com recursos livres - Pessoas jurídicas - Arrendamento mercantil de veículos - R$ (milhões)", "20673 - Concessões de crédito com recursos livres - Pessoas físicas - Aquisição de veículos - R$ (milhões)",  "20676 - Concessões de crédito com recursos livres - Pessoas físicas - Arrendamento mercantil de veículos - R$ (milhões)" , "20728 - Taxa média de juros das operações de crédito com recursos livres - Pessoas jurídicas - Aquisição de veículos - % a.a.",
              "20731 - Taxa média de juros das operações de crédito com recursos livres - Pessoas jurídicas - Arrendamento mercantil de veículos - % a.a.", "20749 - Taxa média de juros das operações de crédito com recursos livres - Pessoas físicas - Aquisição de veículos - % a.a.", "20752 - Taxa média de juros das operações de crédito com recursos livres - Pessoas físicas - Arrendamento mercantil de veículos - % a.a.", "20864 - Prazo médio das concessões de crédito com recursos livres - Pessoas jurídicas - Aquisição de veículos - Meses", "20867 - Prazo médio das concessões de crédito com recursos livres - Pessoas jurídicas - Arrendamento mercantil de veículos - Meses", "20886 - Prazo médio das concessões de crédito com recursos livres - Pessoas físicas - Aquisição de veículos - Meses", "20889 - Prazo médio das concessões de crédito com recursos livres - Pessoas físicas - Arrendamento mercantil de veículos - Meses", "20938 - Prazo médio da carteira de crédito com recursos livres - Pessoas jurídicas - Aquisição de veículos - Meses", "20941 - Prazo médio da carteira de crédito com recursos livres - Pessoas jurídicas - Arrendamento mercantil de veículos - Meses", "20963 - Prazo médio da carteira de crédito com recursos livres - Pessoas físicas - Aquisição de veículos - Meses",
              "20966 - Prazo médio da carteira de crédito com recursos livres - Pessoas físicas - Arrendamento mercantil de veículos - Meses", "21017 - Percentual da carteira de crédito com recursos livres com atraso entre 15 e 90 dias - Pessoas jurídicas - Aquisição de veículos - %", "21020 - Percentual da carteira de crédito com recursos livres com atraso entre 15 e 90 dias - Pessoas jurídicas - Arrendamento mercantil de veículos - %", "21042 - Percentual da carteira de crédito com recursos livres com atraso entre 15 e 90 dias - Pessoas físicas - Aquisição de veículos - %", "21045 - Percentual da carteira de crédito com recursos livres com atraso entre 15 e 90 dias - Pessoas físicas - Arrendamento mercantil de veículos - %", "21096 - Inadimplência da carteira de crédito com recursos livres - Pessoas jurídicas - Aquisição de veículos - %", "21099 - Inadimplência da carteira de crédito com recursos livres - Pessoas jurídicas - Arrendamento mercantil de veículos - %", "21121 - Inadimplência da carteira de crédito com recursos livres - Pessoas físicas - Aquisição de veículos - %", "21124 - Inadimplência da carteira de crédito com recursos livres - Pessoas físicas - Arrendamento mercantil de veículos - %",
              "25447 - Taxa média mensal de juros das operações de crédito com recursos livres - Pessoas jurídicas - Aquisição de veículos - % a.m.", "25450 - Taxa média mensal de juros das operações de crédito com recursos livres - Pessoas jurídicas - Arrendamento mercantil de veículos - % a.m.	", "25471 - Taxa média mensal de juros das operações de crédito com recursos livres - Pessoas físicas - Aquisição de veículos - % a.m.", "25474 - Taxa média mensal de juros das operações de crédito com recursos livres - Pessoas físicas - Arrendamento mercantil de veículos - % a.m.", "27658 - Indicador de Custo do Crédito - ICC - Recursos Livres - Pessoas jurídicas - Aquisição de veículos - % a.a.", "27660 - Indicador de Custo do Crédito - ICC - Recursos Livres - Pessoas jurídicas - Arrendamento mercantil de veículos - % a.a.", "27680 - Indicador de Custo do Crédito - ICC - Recursos Livres - Pessoas físicas - Aquisição de veículos - % a.a.", "27682 - Indicador de Custo do Crédito - ICC - Recursos Livres - Pessoas físicas - Arrendamento mercantil de veículos - % a.a.", "27735 - Saldo das operações de crédito por atividade econômica - Geral - veículos automotores - R$ (milhões)")

#Exportando séries
write.csv2(base,"Indicadores de credito - veiculos(fonte).csv", row.names = F)
export(base, "Indicadores de credito - veiculos(fonte).xlsx")
