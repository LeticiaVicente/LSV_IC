# Carregando dados da pesquisa 2019/2020

load("Z:/Google Drive/UNIRIO/Projetos/IC/Dados_RData/bd_ic_2019_V4_19FEV20.RData")

# Pegando dados novos

library(microdatasus)

obt_2017_2019 <- fetch_datasus(year_start = 2017,year_end = 2019,information_system = "SIM-DO")

# Salvando dados novos:

save(obt_2017_2019,file="Z:/Google Drive/UNIRIO/Projetos/IC/Dados_RData/obt_2017_2019_V1_08MAR21.RData")

# Preparando os dados novos

obt_2017_2019$IDADENUM <- as.numeric(as.character(obt_2017_2019$IDADE))

obt_2017_2019$SEXOPAD <- NA

obt_2017_2019$SEXOPAD[which(obt_2017_2019$SEXO=="1")] <- "M"

obt_2017_2019$SEXOPAD[which(obt_2017_2019$SEXO=="2")] <- "F"

table(obt_2017_2019$SEXOPAD,obt_2017_2019$SEXO)

obt_2017_2019$REG <- NA

obt_2017_2019$REG[which(substr(as.character(obt_2017_2019$CODMUNRES),1,1)=="1")] <- "N"

obt_2017_2019$REG[which(substr(as.character(obt_2017_2019$CODMUNRES),1,1)=="2")] <- "NE"

obt_2017_2019$REG[which(substr(as.character(obt_2017_2019$CODMUNRES),1,1)=="3")] <- "SE"

obt_2017_2019$REG[which(substr(as.character(obt_2017_2019$CODMUNRES),1,1)=="4")] <- "S"

obt_2017_2019$REG[which(substr(as.character(obt_2017_2019$CODMUNRES),1,1)=="5")] <- "CO"

table(obt_2017_2019$REG,useNA = "always")

obt_2017_2019$IDADEPAD <- NA

obt_2017_2019$IDADEPAD[which(substr(obt_2017_2019$IDADE,1,1)<"4")] <- 0

obt_2017_2019$IDADEPAD[which(substr(obt_2017_2019$IDADE,1,1)=="4")] <- as.numeric(substr(obt_2017_2019$IDADE[which(substr(obt_2017_2019$IDADE,1,1)=="4")],2,3))

obt_2017_2019$IDADEPAD[which(substr(obt_2017_2019$IDADE,1,1)=="5")] <- as.numeric(paste0("1",substr(obt_2017_2019$IDADE[which(substr(obt_2017_2019$IDADE,1,1)=="5")],2,3)))

d1 <- table(obt_2017_2019$IDADEPAD,obt_2017_2019$IDADENUM)

View(d1)

obt_2017_2019$anoarq <- substr(obt_2017_2019$DTOBITO,5,8)

obt_2017_2019$anoarq

table(obt_2017_2019$anoarq)

obt_2017_2019$FXETARIA2 <- NA

obt_2017_2019$FXETARIA2[which(obt_2017_2019$IDADEPAD<1)] <- 0

obt_2017_2019$FXETARIA2[which(obt_2017_2019$IDADEPAD>=1 & obt_2017_2019$IDADEPAD<=4)] <- "1a4"
obt_2017_2019$FXETARIA2[which(obt_2017_2019$IDADEPAD>=5 & obt_2017_2019$IDADEPAD<=9)] <- "5a9"
obt_2017_2019$FXETARIA2[which(obt_2017_2019$IDADEPAD>=10 & obt_2017_2019$IDADEPAD<=14)] <- "10a14"
obt_2017_2019$FXETARIA2[which(obt_2017_2019$IDADEPAD>=15 & obt_2017_2019$IDADEPAD<=19)] <- "15a19"
obt_2017_2019$FXETARIA2[which(obt_2017_2019$IDADEPAD>=20 & obt_2017_2019$IDADEPAD<=24)] <- "20a24"
obt_2017_2019$FXETARIA2[which(obt_2017_2019$IDADEPAD>=25 & obt_2017_2019$IDADEPAD<=29)] <- "25a29"
obt_2017_2019$FXETARIA2[which(obt_2017_2019$IDADEPAD>=30 & obt_2017_2019$IDADEPAD<=34)] <- "30a34"
obt_2017_2019$FXETARIA2[which(obt_2017_2019$IDADEPAD>=35 & obt_2017_2019$IDADEPAD<=39)] <- "35a39"
obt_2017_2019$FXETARIA2[which(obt_2017_2019$IDADEPAD>=40 & obt_2017_2019$IDADEPAD<=44)] <- "40a44"
obt_2017_2019$FXETARIA2[which(obt_2017_2019$IDADEPAD>=45 & obt_2017_2019$IDADEPAD<=49)] <- "45a49"
obt_2017_2019$FXETARIA2[which(obt_2017_2019$IDADEPAD>=50 & obt_2017_2019$IDADEPAD<=54)] <- "50a54"
obt_2017_2019$FXETARIA2[which(obt_2017_2019$IDADEPAD>=55 & obt_2017_2019$IDADEPAD<=59)] <- "55a59"
obt_2017_2019$FXETARIA2[which(obt_2017_2019$IDADEPAD>=60 & obt_2017_2019$IDADEPAD<=64)] <- "60a64"
obt_2017_2019$FXETARIA2[which(obt_2017_2019$IDADEPAD>=65 & obt_2017_2019$IDADEPAD<=69)] <- "65a69"
obt_2017_2019$FXETARIA2[which(obt_2017_2019$IDADEPAD>=70 & obt_2017_2019$IDADEPAD<=74)] <- "70a74"
obt_2017_2019$FXETARIA2[which(obt_2017_2019$IDADEPAD>=75 & obt_2017_2019$IDADEPAD<=79)] <- "75a79"
obt_2017_2019$FXETARIA2[which(obt_2017_2019$IDADEPAD>=80)] <- "80+"

table(obt_2017_2019$FXETARIA2)

# Idade preparada conforme os dados do DataSUS!

# Carregando a tabela de coversão das microrregiões e da causa básica

load("Z:/Google Drive/UNIRIO/Projetos/IC/Dados_POP/MICRO.RData")

load("Z:/Google Drive/UNIRIO/Projetos/IC/2020-2021/Banco de Dados/CAUSABAS.RData")

# Selecionando apenas as variáveis de interesse

bd_ic_2020 <- bd_ic_2019[,c("CAUSABAS", "CODMUNRES", "anoarq", "MICROCOD", "REG", "UF", "NOMEMICRO", "IDADEPAD",
                          "SEXOPAD","causabas_cappad")]
rm(bd_ic_2019)

gc()

head(bd_ic_2020)

# Preparando a causa básica e fazendo o merge

library(dplyr)

causabas_junta <- causabas[,c(1,4)]

names(causabas_junta)

head(causabas_junta)

names(causabas_junta)[1] <- "CAUSABAS"

# Salvando

save(obt_2017_2019,file="Z:/Google Drive/UNIRIO/Projetos/IC/2020-2021/Banco de Dados/obt_2017_2019_V2_08MAR21.RData")

obt_2017_2019 <- left_join(obt_2017_2019,causabas_junta)

# Validando

table(obt_2017_2019$causabas_cappad)

# Corrigindo uma causa externa faltante

obt_2017_2019$CAUSABAS[which(is.na(obt_2017_2019$causabas_cappad))]

obt_2017_2019$causabas_cappad[which(is.na(obt_2017_2019$causabas_cappad))] <- "PAD_EXT"

# Verificando doenças respiratórias e SNV

table(obt_2017_2019$CAUSABAS[which(obt_2017_2019$causabas_cappad=="PAD_RES")],useNA = "a")

unique(obt_2017_2019$CAUSABAS[which(obt_2017_2019$causabas_cappad=="PAD_RES")])

View(causabas_junta[which(causabas_junta$causabas_cappad=="PAD_RES"),])

PAD_RES <- causabas_junta[which(causabas_junta$causabas_cappad=="PAD_RES"),]

unique(substr(PAD_RES$CAUSABAS,1,1))

PAD_SNV <- causabas_junta[which(causabas_junta$causabas_cappad=="PAD_SNV"),]

unique(substr(PAD_SNV$CAUSABAS,1,1))

table(causabas_junta$causabas_cappad[which(substr(causabas_junta$CAUSABAS,1,1)=="J")],useNA = "a")

table(causabas_junta$causabas_cappad[which(substr(causabas_junta$CAUSABAS,1,1)=="G")],useNA = "a")

table(causabas_junta$causabas_cappad[which(substr(causabas_junta$CAUSABAS,1,1)=="H")],useNA = "a")

table(obt_2017_2019$causabas_cappad[which(substr(obt_2017_2019$CAUSABAS,1,1)=="J")],useNA = "a")

table(obt_2017_2019$causabas_cappad[which(substr(obt_2017_2019$CAUSABAS,1,1)=="G")],useNA = "a")

table(obt_2017_2019$causabas_cappad[which(substr(obt_2017_2019$CAUSABAS,1,1)=="H")],useNA = "a")

# Não achamos o erro... Seria um erro de classificação do próprio SIM?

head(obt_2017_2019)

# Salvando 

save(obt_2017_2019,file="Z:/Google Drive/UNIRIO/Projetos/IC/2020-2021/Banco de Dados/obt_2017_2019_V3_08MAR21.RData")

# Preparando a Microrregião

MICRO_junta <- unique(MICRO[,c(1,5,6)])

names(MICRO_junta)

names(MICRO_junta)[1] <- "CODMUNRES"

obt_2017_2019 <- left_join(obt_2017_2019,MICRO_junta)

head(obt_2017_2019)

names(obt_2017_2019)[96]

names(obt_2017_2019)[96] <- "MICROCOD"

names(obt_2017_2019)[97]

names(obt_2017_2019)[97] <- "NOMEMICRO"

vars_temos <- names(bd_ic_2020)

cols <- which(names(obt_2017_2019) %in% vars_temos)

cols

obt_2017_2019_1 <- obt_2017_2019[,cols]

table(names(obt_2017_2019_1) %in% names(obt_2017_2019))

names(obt_2017_2019_1)

names(bd_ic_2020)

obt_2017_2019 <- obt_2017_2019_1

rm(obt_2017_2019_1)

gc()

# Avaliando NAs nas microrregiões

names(obt_2017_2019)

table(is.na(obt_2017_2019$MICROCOD))

table(is.na(obt_2017_2019$NOMEMICRO))

# Salvando a base

save(obt_2017_2019,file="Z:/Google Drive/UNIRIO/Projetos/IC/2020-2021/Banco de Dados/obt_2017_2019_V5_18MAR21.RData")

# Juntando os dados da UF

load("Z:/Google Drive/UNIRIO/Projetos/IC/Dados_POP/siglas_uf.RData")

head(uf)

obt_2017_2019$cod_uf <- as.character(substr(obt_2017_2019$CODMUNRES,1,2))

uf$cod_uf <- as.character(uf$cod_uf)

obt_2017_2019 <- left_join(obt_2017_2019,uf[,c(1,3)])

head(obt_2017_2019)

table(names(obt_2017_2019) %in% vars_temos)

names(obt_2017_2019)

names(bd_ic_2020)

names(obt_2017_2019)[11]

names(obt_2017_2019)[11] <- "UF"

obt_2017_2019$cod_uf <- NULL

table(names(obt_2017_2019) %in% vars_temos)

# Salvando a base

save(obt_2017_2019,file="Z:/Google Drive/UNIRIO/Projetos/IC/2020-2021/Banco de Dados/obt_2017_2019_V6_18MAR21.RData")

# Verificando microrregiões no arquivo antigo

names(bd_ic_2020)

table(is.na(bd_ic_2020$MICROCOD))

table(is.na(bd_ic_2020$NOMEMICRO))

# Existem NAs... Corrigindo:

bd_ic_2020$NOMEMICRO <- NULL

bd_ic_2020$MICROCOD <- NULL

bd_ic_2020_1 <- left_join(bd_ic_2020,MICRO_junta)

head(bd_ic_2020_1)

names(bd_ic_2020_1)[9]

names(bd_ic_2020_1)[9] <- "MICROCOD"

names(bd_ic_2020_1)[10]

names(bd_ic_2020_1)[10] <- "NOMEMICRO"

vars_temos <- names(bd_ic_2020_1)

# Verificando Micro

table(is.na(bd_ic_2020_1$MICROCOD))

bd_ic_2020_1[which(is.na(bd_ic_2020_1$MICROCOD)),]

table(is.na(bd_ic_2020_1$NOMEMICRO))

bd_ic_2020_1[which(is.na(bd_ic_2020_1$NOMEMICRO)),]

# Temos apenas 4 NAs:

NAANTIGOS <- bd_ic_2020_1[which(is.na(bd_ic_2020_1$NOMEMICRO)),]

NAANTIGOS

#          CAUSABAS CODMUNRES anoarq REG UF IDADEPAD SEXOPAD causabas_cappad MICROCOD NOMEMICRO
# 10519334     I251   5300000   2003  CO DF       51       M         PAD_CIR     <NA>      <NA>
# 10519339     C910   5300000   2003  CO DF       18       F         PAD_NEO     <NA>      <NA>
# 15637851     K550    530000   2008  CO DF       46       M         PAD_DIG     <NA>      <NA>
# 15645303     I339    530000   2008  CO DF       80       M         PAD_CIR     <NA>      <NA>

# Teoricamente, 53000 seria "Brasília - Ignorado", mas brasília já é um município... 

unique(grep(MICRO_junta$MICRO_LEG,pattern = "BRASILIA",value = T))

# [1] "ENTORNO DE BRASILIA" "BRASILIA" 

MICRO_junta[(grep(MICRO_junta$MICRO_LEG,pattern = "BRASILIA")),]

# Existem 2 microrregiões que podem ser referentes a brasília: "ENTORNO DE BRASILIA" "BRASILIA".
# Segundo o "CADMUN" o código 530000 e 5300000 se referem à microregião 53001 BRASILIA:
# MUNSINON,C,28	MUNSINONDV,C,32	AMAZONIA,C,1	FRONTEIRA,C,1	CAPITAL,C,1	UFCOD,C,2	MESOCOD,C,4	MICROCOD,C,5
# 530000-530009,530011-539999	,5300000-5300099,5300110-5399999	N	N	S	53	5301	53001

# Vou complear manualmente:

str(bd_ic_2020_1)

bd_ic_2020_1$MICROCOD[which(is.na(bd_ic_2020_1$MICROCOD))] <- "53000"

bd_ic_2020_1$NOMEMICRO[which(is.na(bd_ic_2020_1$NOMEMICRO))] <- "BRASILIA"

# Verificando

table(is.na(bd_ic_2020_1$MICROCOD))

table(is.na(bd_ic_2020_1$NOMEMICRO))

# Juntando as bases

base_ic_2020 <- bind_rows(bd_ic_2020_1,obt_2017_2019)

# Verificando

table(is.na(base_ic_2020$MICROCOD))

table(is.na(base_ic_2020$NOMEMICRO))

# Salvando

save(base_ic_2020,file="Z:/Google Drive/UNIRIO/Projetos/IC/2020-2021/Banco de Dados/base_ic_2020_V3_18MAR21.RData")

save(bd_ic_2020,file="Z:/Google Drive/UNIRIO/Projetos/IC/2020-2021/Banco de Dados/bd_ic_2020_V2_18MAR21.RData")

save(bd_ic_2020_1,file="Z:/Google Drive/UNIRIO/Projetos/IC/2020-2021/Banco de Dados/bd_ic_2020_1_V1_18MAR21.RData")

# Removendo arquivos que não serão usados

rm(list=ls()[-grep(ls(),pattern="base_ic_2020")])

gc()

# Carregando a expectativa de vida

ev <- read.csv2("Z:\\Google Drive\\UNIRIO\\Projetos\\IC\\Dados_POP\\EV_ANOS_CENSO.csv", header = T, sep = ";", encoding = "UTF-8", dec = ",",as.is = T)

head(ev)

names(ev)

names(ev)[1] <- "IDADE"

head(ev)

str(ev)

# Ordenando o arquivo:

library(tidyverse)

ev1 <- arrange(ev, ANO, IDADE, SEXO)

ev1

ev <- ev1

rm(ev1)

head(ev)

# Fazendo as estimativas para os anos faltantes (p1 = 1981 a 1990, p2 = 1992 a 1999 e p3 = 2001 a 2019)

evM0_p1 <- ev[which(ev$ANO <= 1991 & ev$SEXO == "M" & ev$IDADE == 0),]

evM60_p1 <- ev[which(ev$ANO <= 1991 & ev$SEXO == "M" & ev$IDADE == 60),]

evF0_p1 <- ev[which(ev$ANO <= 1991 & ev$SEXO == "F" & ev$IDADE == 0),]

evF60_p1 <- ev[which(ev$ANO <= 1991 & ev$SEXO == "F" & ev$IDADE == 60),]

evM0_p2 <- ev[which({ev$ANO == 1991 | ev$ANO == 2000} & ev$SEXO == "M" & ev$IDADE == 0),]

evM60_p2 <- ev[which({ev$ANO == 1991 | ev$ANO == 2000} & ev$SEXO == "M" & ev$IDADE == 60),]

evF0_p2 <- ev[which({ev$ANO == 1991 | ev$ANO == 2000} & ev$SEXO == "F" & ev$IDADE == 0),]

evF60_p2 <- ev[which({ev$ANO == 1991 | ev$ANO == 2000} & ev$SEXO == "F" & ev$IDADE == 60),]

evM0_p3 <- ev[which({ev$ANO == 2000 | ev$ANO == 2010} & ev$SEXO == "M" & ev$IDADE == 0),]

evM60_p3 <- ev[which({ev$ANO == 2000 | ev$ANO == 2010} & ev$SEXO == "M" & ev$IDADE == 60),]

evF0_p3 <- ev[which({ev$ANO == 2000 | ev$ANO == 2010} & ev$SEXO == "F" & ev$IDADE == 0),]

evF60_p3 <- ev[which({ev$ANO == 2000 | ev$ANO == 2010} & ev$SEXO == "F" & ev$IDADE == 60),]

lista_est <- grep(ls(), pattern = "ev[A-Z]", value = T)

for(i in 1:length(lista_est)){
  
  rl <- lm("EX~ANO", data = get(lista_est[i]))  
  
  if( sum(grep(lista_est[i],pattern = "p3")) > 0 ) tmp <- data.frame(ANO = c(min(get(lista_est[i])[,3]):2019),
                                                                     SEXO=as.character(unique(get(lista_est[i])[,2])),
                                                                     IDADE=unique(get(lista_est[i])[,1]),
                                                                     EX=NA, stringsAsFactors = F) else tmp <- 
                                                                         data.frame(ANO = c(min(get(lista_est[i])[,3]):
                                                                                              max(get(lista_est[i])[,3])),
                                                                                    SEXO=as.character(unique(get(lista_est[i])[,2])),
                                                                                    IDADE=unique(get(lista_est[i])[,1]), EX=NA, stringsAsFactors = F)
                                                                     
                                                                     tmp$EX <- predict(rl,tmp)
                                                                     
                                                                     if(i == 1) EX_EST <- tmp else EX_EST <- rbind(EX_EST, tmp)
                                                                     
                                                                     print(paste0("Estimativas de ",i," concluída"))
                                                                     
}

EX_EST

EX_EST1 <- EX_EST[-which(EX_EST$ANO %in% c(1980,1991,2000,2010)),]

table(EX_EST1$ANO)

EX <- rbind(EX_EST1,ev)

EX

EX1 <- arrange(EX,ANO, IDADE, SEXO)

EX1

EX <- EX1

EX

rm(list=lista_est)

# Conferindo

plot(EX$ANO[which(EX$SEXO == "F" & EX$IDADE == "0")], EX$EX[which(EX$SEXO == "F" & EX$IDADE == "0")],
     main = "Estimativas da experctativa de vida ao nascer - Sexo Feminino", xlab = "", ylab = "Expectativa de Vida")
points(ev[which(ev$IDADE==0 & ev$SEXO=="F"),c(3,4)],col="red",pch=19)

plot(EX$ANO[which(EX$SEXO == "F" & EX$IDADE == "60")], EX$EX[which(EX$SEXO == "F" & EX$IDADE == "60")],
     main = "Estimativas da experctativa de vida aos 60 anos - Sexo Feminino", xlab = "", ylab = "Expectativa de Vida")
points(ev[which(ev$IDADE==60 & ev$SEXO=="F"),c(3,4)],col="red",pch=19)

plot(EX$ANO[which(EX$SEXO == "M" & EX$IDADE == "0")], EX$EX[which(EX$SEXO == "M" & EX$IDADE == "0")],
     main = "Estimativas da experctativa de vida ao nascer - Sexo Masculino", xlab = "", ylab = "Expectativa de Vida")
points(ev[which(ev$IDADE==0 & ev$SEXO=="M"),c(3,4)],col="red",pch=19)

plot(EX$ANO[which(EX$SEXO == "M" & EX$IDADE == "60")], EX$EX[which(EX$SEXO == "M" & EX$IDADE == "60")],
     main = "Estimativas da experctativa de vida aos 60 anos - Sexo Masculino", xlab = "", ylab = "Expectativa de Vida")
points(ev[which(ev$IDADE==60 & ev$SEXO=="M"),c(3,4)],col="red",pch=19)

save(EX, file = "Z:/Google Drive/UNIRIO/Projetos/IC/2020-2021/Banco de Dados/EX_V6_08MAR21.RData")

save(ev, file = "Z:/Google Drive/UNIRIO/Projetos/IC/2020-2021/Banco de Dados/ev_V6_08MAR21.RData")

save(EX_EST, file = "Z:/Google Drive/UNIRIO/Projetos/IC/2020-2021/Banco de Dados/EX_EST_V6_08MAR21.RData")

save(EX_EST1, file = "Z:/Google Drive/UNIRIO/Projetos/IC/2020-2021/Banco de Dados/EX_EST1_V6_08MAR21.RData")

write.csv2(EX, file = "Z:/Google Drive/UNIRIO/Projetos/IC/2020-2021/Banco de Dados/EX_V6_08MAR21.csv", row.names = F)

write.csv2(ev, file = "Z:/Google Drive/UNIRIO/Projetos/IC/2020-2021/Banco de Dados/ev_V6_08MAR21.csv", row.names = F)

write.csv2(EX_EST, file = "Z:/Google Drive/UNIRIO/Projetos/IC/2020-2021/Banco de Dados/EX_EST_V6_08MAR21.csv", row.names = F)

write.csv2(EX_EST1, file = "Z:/Google Drive/UNIRIO/Projetos/IC/2020-2021/Banco de Dados/EX_EST1_V6_08MAR21.csv", row.names = F)

# Fazendo o merge da expecativ:

head(base_ic_2020)

head(EX)

names(EX)

names(EX)[1] <- "anoarq"

names(EX)[2] <- "SEXOPAD"

EX$anoarq <- as.character(EX$anoarq)

base_ic_2020 <- left_join(base_ic_2020,EX[EX$IDADE==60,c(1,2,4)])

head(base_ic_2020)

names(base_ic_2020)[11] 

names(base_ic_2020)[11] <- "EX60"

base_ic_2020 <- left_join(base_ic_2020,EX[EX$IDADE==0,c(1,2,4)])

head(base_ic_2020)

# Avaliandoo merge:

table(is.na(base_ic_2020$EX60))

table(is.na(base_ic_2020$EX))

# Existem NAs. Verificando

table(base_ic_2020$anoarq[which(is.na(base_ic_2020$EX60))])

table(base_ic_2020$anoarq[which(is.na(base_ic_2020$EX))])

# Tem NAs em vários anos. Eles podem ser oriundos da ausência da informação do sexo, ou referente ao ano de 1979

table(base_ic_2020$SEXOPAD[which(is.na(base_ic_2020$EX) & base_ic_2020$anoarq!="1979")],useNA = "a")

table(base_ic_2020$SEXOPAD[which(is.na(base_ic_2020$EX60) & base_ic_2020$anoarq!="1979")],useNA = "a")

# Todos os EX e EX60 que têm NA em anos diferentes de 1979 faltam informação do sexo

table(base_ic_2020$anoarq[which(is.na(base_ic_2020$EX60) & !is.na(base_ic_2020$SEXOPAD))],useNA = "a")

table(base_ic_2020$anoarq[which(is.na(base_ic_2020$EX) & !is.na(base_ic_2020$SEXOPAD))],useNA = "a")

## Todos os faltantes com SEXO preenchido se referem ao ano de 1979, o que é esperado

# Os NAs são esperados e estão corretos

str(base_ic_2020)

# Salvando a base:

save(base_ic_2020, file = "Z:/Google Drive/UNIRIO/Projetos/IC/2020-2021/Banco de Dados/base_ic_2020_V3_18MAR21.RData")

# Calculando os APVPs

## 1) Fazendo subset

### Idosos

base_ic_2020_id <- base_ic_2020[which(base_ic_2020$IDADEPAD>=60),]

base_ic_2020_id$EX <- NULL

base_ic_2020_id$APVP <- {base_ic_2020_id$EX60+60}-base_ic_2020_id$IDADEPAD-0.5

head(base_ic_2020_id)

# Filtrando aqueles com APVP:

bd_id_2020 <- base_ic_2020_id[which(base_ic_2020_id$APVP>0),]

# Verificando a consictência das informações:

table(is.na(bd_id_2020$APVP))

table(bd_id_2020$anoarq)

# Está OK!

# Salvando a base de idosos completa:

save(base_ic_2020_id, file = "Z:/Google Drive/UNIRIO/Projetos/IC/2020-2021/Banco de Dados/base_ic_2020_idoso_completo_V2_18MAR21.RData")

# Salvando a base de idosos apenas com APVP:

save(bd_id_2020, file = "Z:/Google Drive/UNIRIO/Projetos/IC/2020-2021/Banco de Dados/bd_id_2020_idoso_com_APVP_V2_18MAR21.RData")

### Adultos

base_ic_2020$APVP <- base_ic_2020$EX-base_ic_2020$IDADEPAD-0.5

head(base_ic_2020)

base_ic_2020_ad <- base_ic_2020[which(base_ic_2020$APVP > 0),]

head(base_ic_2020_ad)

# Filtrando aqueles com APVP:

bd_ad_2020 <- base_ic_2020_ad

# Verificando a consictência das informações:

table(is.na(bd_ad_2020$APVP))

table(bd_ad_2020$anoarq)

# Está OK!

# Salvando a base de adultos completa:

save(base_ic_2020_ad, file = "Z:/Google Drive/UNIRIO/Projetos/IC/2020-2021/Banco de Dados/base_ic_2020_adulto_completo_V2_18MAR21.RData")

# Salvando a base de idosos apenas com APVP:

save(bd_ad_2020, file = "Z:/Google Drive/UNIRIO/Projetos/IC/2020-2021/Banco de Dados/bd_ad_2020_adultos_com_APVP_V2_18MAR21.RData")

# Removendo os arquivos que não são de trabalho:

rm(base_ic_2020)

rm(base_ic_2020_ad)

rm(base_ic_2020_id)

rm(EX)

gc()

# Criando faixas etárias

bd_ad_2020$FXETARIA2 <- NA

bd_ad_2020$FXETARIA2[which(bd_ad_2020$IDADEPAD<=4)] <- "0a4"
bd_ad_2020$FXETARIA2[which(bd_ad_2020$IDADEPAD>=5 & bd_ad_2020$IDADEPAD<=9)] <- "5a9"
bd_ad_2020$FXETARIA2[which(bd_ad_2020$IDADEPAD>=10 & bd_ad_2020$IDADEPAD<=14)] <- "10a14"
bd_ad_2020$FXETARIA2[which(bd_ad_2020$IDADEPAD>=15 & bd_ad_2020$IDADEPAD<=19)] <- "15a19"
bd_ad_2020$FXETARIA2[which(bd_ad_2020$IDADEPAD>=20 & bd_ad_2020$IDADEPAD<=24)] <- "20a24"
bd_ad_2020$FXETARIA2[which(bd_ad_2020$IDADEPAD>=25 & bd_ad_2020$IDADEPAD<=29)] <- "25a29"
bd_ad_2020$FXETARIA2[which(bd_ad_2020$IDADEPAD>=30 & bd_ad_2020$IDADEPAD<=34)] <- "30a34"
bd_ad_2020$FXETARIA2[which(bd_ad_2020$IDADEPAD>=35 & bd_ad_2020$IDADEPAD<=39)] <- "35a39"
bd_ad_2020$FXETARIA2[which(bd_ad_2020$IDADEPAD>=40 & bd_ad_2020$IDADEPAD<=44)] <- "40a44"
bd_ad_2020$FXETARIA2[which(bd_ad_2020$IDADEPAD>=45 & bd_ad_2020$IDADEPAD<=49)] <- "45a49"
bd_ad_2020$FXETARIA2[which(bd_ad_2020$IDADEPAD>=50 & bd_ad_2020$IDADEPAD<=54)] <- "50a54"
bd_ad_2020$FXETARIA2[which(bd_ad_2020$IDADEPAD>=55 & bd_ad_2020$IDADEPAD<=59)] <- "55a59"
bd_ad_2020$FXETARIA2[which(bd_ad_2020$IDADEPAD>=60 & bd_ad_2020$IDADEPAD<=64)] <- "60a64"
bd_ad_2020$FXETARIA2[which(bd_ad_2020$IDADEPAD>=65 & bd_ad_2020$IDADEPAD<=69)] <- "65a69"
bd_ad_2020$FXETARIA2[which(bd_ad_2020$IDADEPAD>=70 & bd_ad_2020$IDADEPAD<=74)] <- "70a74"
bd_ad_2020$FXETARIA2[which(bd_ad_2020$IDADEPAD>=75 & bd_ad_2020$IDADEPAD<=79)] <- "75a79"
bd_ad_2020$FXETARIA2[which(bd_ad_2020$IDADEPAD>=80)] <- "80+"

table(bd_ad_2020$FXETARIA2,bd_ad_2020$IDADEPAD,useNA="a")

tail(table(bd_ad_2020$FXETARIA2,bd_ad_2020$IDADEPAD,useNA="a"))

summary(bd_ad_2020$EX)

bd_id_2020$FXETARIA2 <- NA
bd_id_2020$FXETARIA2[which(bd_id_2020$IDADEPAD>=60 & bd_id_2020$IDADEPAD<=64)] <- "60a64"
bd_id_2020$FXETARIA2[which(bd_id_2020$IDADEPAD>=65 & bd_id_2020$IDADEPAD<=69)] <- "65a69"
bd_id_2020$FXETARIA2[which(bd_id_2020$IDADEPAD>=70 & bd_id_2020$IDADEPAD<=74)] <- "70a74"
bd_id_2020$FXETARIA2[which(bd_id_2020$IDADEPAD>=75 & bd_id_2020$IDADEPAD<=79)] <- "75a79"
bd_id_2020$FXETARIA2[which(bd_id_2020$IDADEPAD>=80)] <- "80+"

table(bd_id_2020$FXETARIA2,bd_id_2020$IDADEPAD, useNA="a")

tail(table(bd_id_2020$FXETARIA2,bd_id_2020$IDADEPAD, useNA="a"))

# Salvando
  
save(bd_ad_2020, file = "Z:/Google Drive/UNIRIO/Projetos/IC/2020-2021/Banco de Dados/bd_ad_2020_adultos_com_APVP_V3_18MAR21.RData")

save(bd_id_2020, file = "Z:/Google Drive/UNIRIO/Projetos/IC/2020-2021/Banco de Dados/bd_id_2020_idosos_com_APVP_V3_18MAR21.RData")

##########################

# Dados Populacionais

load("Z:/Google Drive/UNIRIO/Projetos/IC/Dados_POP/MICRO.RData")

load("Z:/Google Drive/UNIRIO/Projetos/IC/Dados_POP/pop_1980_2012_V2_07JUL20.RData")

head(MICRO)

head(pop)

pop_micro1 <- left_join(pop,unique(MICRO[,c(1,5,6)]),by=c("MUNIC_RES"="MUNI_COD"))

head(pop_micro)

## Verificando

table(is.na(pop_micro$MICRO_COD))

table(is.na(pop_micro$FXETARIA1))

NAFX1 <- pop_micro[which(is.na(pop_micro$FXETARIA1)),]

table(NAFX1$FXETARIA)

# Temos um problema com PARAGOMINAS, que não tem dado por faixa etária de 1993 a 1995, apenas a população total (idade ignorada):

## Vamos pegar a distribuição da população por faixa etária em 1992 e 1996 e avaliar a evolução por faixa etária:

parago_pop_1992 <- pop_micro[which(pop_micro$MICRO_COD=="15017" & pop_micro$ANO=="1992"),]

parago_pop_1992$POP_TOT <- sum(parago_pop_1992$POP)

parago_pop_1992 %>% mutate(PROP_POP=POP/POP_TOT) -> parago_pop_1992

parago_pop_1992

# Criando dados de 1993, 1994, 1995 para PARAGOMINAS:

parago_pop_93_95 <- parago_pop_1992

parago_pop_93_95_1 <- bind_rows(parago_pop_93_95,parago_pop_93_95)

parago_pop_93_95 <- bind_rows(parago_pop_93_95_1,parago_pop_93_95)

nrow(parago_pop_93_95)/3

parago_pop_93_95$ANO <- c(rep("1993",34),rep("1994",34),rep("1995",34))

parago_pop_1993 <- pop_micro[which(pop_micro$MICRO_COD=="15017" & pop_micro$ANO=="1993"),]

parago_pop_1993 <- sum(parago_pop_1993$POP)

parago_pop_1994 <- pop_micro[which(pop_micro$MICRO_COD=="15017" & pop_micro$ANO=="1994"),]

parago_pop_1994 <- sum(parago_pop_1994$POP)

parago_pop_1995 <- pop_micro[which(pop_micro$MICRO_COD=="15017" & pop_micro$ANO=="1995"),]

parago_pop_1995 <- sum(parago_pop_1995$POP)

parago_pop_93_95$POP_TOT <- NULL

parago_pop_93_95$POP[which(parago_pop_93_95$ANO=="1993")] <- round(parago_pop_93_95$PROP_POP[which(parago_pop_93_95$ANO=="1993")]*parago_pop_1993)

parago_pop_93_95$POP[which(parago_pop_93_95$ANO=="1994")] <- round(parago_pop_93_95$PROP_POP[which(parago_pop_93_95$ANO=="1994")]*parago_pop_1994)

parago_pop_93_95$POP[which(parago_pop_93_95$ANO=="1995")] <- round(parago_pop_93_95$PROP_POP[which(parago_pop_93_95$ANO=="1995")]*parago_pop_1995)

parago_pop_93_95$PROP_POP <- NULL

sum(parago_pop_93_95$POP[parago_pop_93_95$ANO=="1993"])

sum(parago_pop_93_95$POP[parago_pop_93_95$ANO=="1994"])

sum(parago_pop_93_95$POP[parago_pop_93_95$ANO=="1995"])

# Como a varição é mínima, 

# Agregando por micro

pop_micro1 %>% group_by(MICRO_COD,MICRO_LEG,FXETARIA1,ANO,SEXOPAD) %>% summarise(POP=sum(POPULACAO)) -> pop_micro2

pop_micro2[pop_micro2$MICRO_COD=="15017" & pop_micro2$ANO =="1993",]

pop_micro2[pop_micro2$MICRO_COD=="15017" & pop_micro2$ANO =="1994",]

pop_micro2[pop_micro2$MICRO_COD=="15017" & pop_micro2$ANO =="1995",]

pop_micro2[which(pop_micro2$MICRO_COD=="15017" & pop_micro2$ANO %in% c("1993","1994","1995")),]

nrow(pop_micro2)

pop_micro3 <- pop_micro2[-which(pop_micro2$MICRO_COD=="15017" & pop_micro2$ANO %in% c("1993","1994","1995")),]

nrow(pop_micro3)

nrow(pop_micro2)-nrow(pop_micro3)

pop_micro <- bind_rows(pop_micro3,parago_pop_93_95)

nrow(pop_micro)-nrow(pop_micro3)

save(pop_micro,file="Z:\\Google Drive\\UNIRIO\\Projetos\\IC\\2020-2021\\Banco de Dados\\pop_micro_V4_22MAR21.RData")

# Verificando os dados por microrregião e ano:

sum(table(unique(pop_micro$MICRO_COD)))

table(pop_micro$FXETARIA1,useNA = "a")

table(pop_micro$FXETARIA1,pop_micro$ANO,useNA = "a")

NAFXET1 <- pop_micro1[which(is.na(pop_micro1$FXETARIA1)),]

table(NAFXET1$FXETARIA)

falt<-unique(NAFXET1$MICRO_COD)

NAFXET <- pop_micro[which(is.na(pop_micro$FXETARIA1)),]

table(NAFXET$MICRO_COD %in% falt)

# Pegando o restante dos dados populacionais

arqs <- list.files("Z:\\Google Drive\\UNIRIO\\Projetos\\IC\\2020-2021\\Banco de Dados\\Dados Populacionais 2017 - 2019")

library(reshape2)

arqs

arqs_m_csv <- grep(arqs, pattern = "pop_M",value = T)

arqs_f_csv <- grep(arqs,pattern = "pop_F",value = T)

ano <- c(2013:2019)

for(i in 1:length(arqs_m_csv)){
  
  tmp_pop <- read.csv2(file = paste0("Z:\\Google Drive\\UNIRIO\\Projetos\\IC\\2020-2021\\Banco de Dados\\Dados Populacionais 2017 - 2019\\",arqs_m_csv[i]),header = T,sep=";")
  
  tmp_pop$ANO <- ano[i]
  
  tmp_pop$SEXOPAD <- "M"
  
  tmp_pop
  
  tmp <- melt(tmp_pop,id.vars = c(1,2,20,21))
  
  tmp$FXETARIA2 <- NA
  
  for(j in 1:nrow(tmp)){
    
    if (nchar(as.character(tmp$variable[j]))==6) tmp$FXETARIA2[j] <- substr(tmp$variable[j],2,6)
        
    if (nchar(as.character(tmp$variable[j]))==4 & substr(tmp$variable[j],4,4)==".") tmp$FXETARIA2[j] <- "80+"
        
    if (nchar(as.character(tmp$variable[j]))==4 & substr(as.character(tmp$variable[j]),4,4)!=".") tmp$FXETARIA2[j] <- substr(tmp$variable[j],2,4)
    
  }
  
  names(tmp)[6] <- "POP"
  
  tmp$variable <- NULL
  
  tmp$POP <- as.numeric(tmp$POP)
  
  if(i==1) pop_micro_m_13_19 <- tmp
  
  if(i>1) pop_micro_m_13_19 <- rbind(pop_micro_m_13_19,tmp)
  
  print(paste0("O arquivo ",arqs_m_csv[i]," foi carregado e apendado"))
  
  rm(tmp)
  
  rm(tmp_pop)
  
}

pop_micro_m_13_19

table(pop_micro_m_13_19$FXETARIA2,useNA = "a")

table(is.na(pop_micro_m_13_19$ANO))

table(is.na(pop_micro_m_13_19$SEXOPAD))

table(is.na(pop_micro_m_13_19$Micro))

table(is.na(pop_micro_m_13_19$POP))

NAMIC <-  pop_micro_m_13_19[which(is.na(pop_micro_m_13_19$POP)),]

NAMIC

table(NAMIC$Micro, useNA = "a")

save(pop_micro_m_13_19,file="Z:\\Google Drive\\UNIRIO\\Projetos\\IC\\2020-2021\\Banco de Dados\\pop_micro_m_13_19_V3_15MAR21.RData")

for(i in 1:length(arqs_f_csv)){
  
  tmp_pop <- read.csv2(file = paste0("Z:\\Google Drive\\UNIRIO\\Projetos\\IC\\2020-2021\\Banco de Dados\\Dados Populacionais 2017 - 2019\\",arqs_m_csv[i]),header = T,sep=";")
  
  tmp_pop$ANO <- ano[i]
  
  tmp_pop$SEXOPAD <- "F"
  
  tmp_pop
  
  tmp <- melt(tmp_pop,id.vars = c(1,2,20,21))
  
  tmp$FXETARIA2 <- NA
  
  for(j in 1:nrow(tmp)){
    
    if (nchar(as.character(tmp$variable[j]))==6) tmp$FXETARIA2[j] <- substr(tmp$variable[j],2,6)
    
    if (nchar(as.character(tmp$variable[j]))==4 & substr(tmp$variable[j],4,4)==".") tmp$FXETARIA2[j] <- "80+"
    
    if (nchar(as.character(tmp$variable[j]))==4 & substr(as.character(tmp$variable[j]),4,4)!=".") tmp$FXETARIA2[j] <- substr(tmp$variable[j],2,4)
    
  }
  
  names(tmp)[6] <- "POP"
  
  tmp$variable <- NULL
  
  tmp$POP <- as.numeric(tmp$POP)  
  
  if(i==1) pop_micro_f_13_19 <- tmp
  
  if(i>1) pop_micro_f_13_19 <- rbind(pop_micro_f_13_19,tmp)
  
  print(paste0("O arquivo ",arqs_f_csv[i]," foi carregado e apendado"))
  
  rm(tmp)
  
  rm(tmp_pop)
  
}

pop_micro_f_13_19

table(pop_micro_f_13_19$FXETARIA2)

table(is.na(pop_micro_f_13_19$ANO))

table(is.na(pop_micro_f_13_19$SEXOPAD))

table(is.na(pop_micro_f_13_19$Micro))

table(is.na(pop_micro_f_13_19$POP))

NAMIC <-  pop_micro_f_13_19[which(is.na(pop_micro_f_13_19$POP)),]

NAMIC

table(NAMIC$Micro,useNA = "a")

save(pop_micro_f_13_19,file="Z:\\Google Drive\\UNIRIO\\Projetos\\IC\\2020-2021\\Banco de Dados\\pop_micro_f_13_19_V3_15MAR21.RData")

head(pop_micro)

head(pop_micro_f_13_19)

names(pop_micro)

names(pop_micro)[1] <- "MICROCOD"

names(pop_micro_f_13_19)

names(pop_micro_f_13_19)[1] <- "MICROCOD"

names(pop_micro_f_13_19)[6] <- "FXETARIA1"

names(pop_micro_m_13_19)

names(pop_micro_m_13_19)[1] <- "MICROCOD"

names(pop_micro_m_13_19)[6] <- "FXETARIA1"

pop_micro_f_13_19$ANO <- as.character(pop_micro_f_13_19$ANO)

pop_micro_m_13_19$ANO <- as.character(pop_micro_m_13_19$ANO)

pop_micro_f_13_19$MICROCOD <- as.character(pop_micro_f_13_19$MICROCOD)

pop_micro_m_13_19$MICROCOD <- as.character(pop_micro_m_13_19$MICROCOD)

pop_micro1 <- rbind(pop_micro[,c(1,3,4,5,6)],pop_micro_f_13_19[,c(1,3,4,5,6)])

pop_micro1 <- rbind(pop_micro1,pop_micro_m_13_19[,c(1,3,4,5,6)])

table(pop_micro1$FXETARIA1,useNA = "a")

library(stringi)

pop_micro1$FXETARIA2 <- stri_replace_all_fixed(pop_micro1$FXETARIA1,pattern =  " ",replacement = "")

table(pop_micro1$FXETARIA2,useNA = "a")

pop_micro_80_19 <- pop_micro1

rm(pop_micro1)

save(pop_micro_80_19,file="Z:\\Google Drive\\UNIRIO\\Projetos\\IC\\2020-2021\\Banco de Dados\\pop_micro_80_19_V3_22MAR21.RData")

# Agregar os bancos de dados de óbitos

# Agregando os dados:

names(bd_ad_2020)

bd_ad_2020 %>% group_by(MICROCOD,NOMEMICRO,REG,UF,anoarq,SEXOPAD,FXETARIA2,causabas_cappad) %>% summarise(APVP=sum(APVP))-> bd_ag_APVP_ad

bd_ag_APVP_ad

save(bd_ag_APVP_ad,file="Z:\\Google Drive\\UNIRIO\\Projetos\\IC\\2020-2021\\Banco de Dados\\bd_ag_APVP_ad_V2_18MAR21.RData")

bd_ad_2020 %>% group_by(MICROCOD,NOMEMICRO,REG,UF,anoarq,SEXOPAD,FXETARIA2) %>% summarise(APVP=sum(APVP))-> bd_ad_2020_geral

bd_ad_2020_geral

save(bd_ad_2020_geral,file="Z:\\Google Drive\\UNIRIO\\Projetos\\IC\\2020-2021\\Banco de Dados\\bd_ad_2020_geral_V2_18MAR20.RData")

names(bd_ad_2020)

bd_id_2020 %>% group_by(MICROCOD,NOMEMICRO,REG,UF,anoarq,SEXOPAD,FXETARIA2,causabas_cappad) %>% summarise(APVP=sum(APVP))-> bd_ag_APVP_id

bd_ag_APVP_id

save(bd_ag_APVP_id,file="Z:\\Google Drive\\UNIRIO\\Projetos\\IC\\2020-2021\\Banco de Dados\\bd_ag_APVP_id_V2_18MAR21.RData")

bd_id_2020 %>% group_by(MICROCOD,NOMEMICRO,REG,UF,anoarq,SEXOPAD,FXETARIA2) %>% summarise(APVP=sum(APVP))-> bd_id_2020_geral

bd_id_2020_geral

save(bd_id_2020_geral,file="Z:\\Google Drive\\UNIRIO\\Projetos\\IC\\2020-2021\\Banco de Dados\\bd_id_2020_geral_V2_18MAR20.RData")

# Fazendo merge do banco de APVP com o banco de População

head(pop_micro_80_19)

names(pop_micro_80_19)

names(bd_ad_2020_geral)

names(bd_id_2020_geral)

names(pop_micro_80_19)[3] <- "anoarq"

pop_micro_80_19$FXETARIA1 <- NULL

pop_micro_80_19$anoarq <- as.character(pop_micro_80_19$anoarq)

bd_APVP_ad <- left_join(bd_ad_2020_geral,pop_micro_80_19)

nrow(bd_APVP_ad)==nrow(bd_ad_2020_geral)

table(is.na(bd_APVP_ad$POP))

bd_APVP_ad[which(is.na(bd_APVP_ad$POP)),]

table(bd_APVP_ad$MICROCOD[which(is.na(bd_APVP_ad$POP))])

bd_APVP_id <- left_join(bd_id_2020_geral,pop_micro_80_19)

nrow(bd_APVP_id)==nrow(bd_id_2020_geral)

table(is.na(bd_APVP_id$POP))

bd_APVP_id[which(is.na(bd_APVP_id$POP)),]

table(bd_APVP_id$MICROCOD[which(is.na(bd_APVP_id$POP))])

# Verificando Nas nas microrregiões:

table(is.na(bd_APVP_ad$MICROCOD))

table(is.na(bd_APVP_ad$NOMEMICRO))

table(is.na(bd_APVP_id$MICROCOD))

table(is.na(bd_APVP_id$NOMEMICRO))

# Removendo municípios ignorados

bd_APVP_ad <- bd_APVP_ad[-which(substr(bd_APVP_ad$MICROCOD,3,5)=="000"),]

bd_APVP_id <- bd_APVP_id[-which(substr(bd_APVP_id$MICROCOD,3,5)=="000"),]

# Verificando a remoção dos ignorados:

table(is.na(bd_APVP_ad$POP))

bd_APVP_ad[which(is.na(bd_APVP_ad$POP)),]

table(bd_APVP_ad$NOMEMICRO[which(is.na(bd_APVP_ad$POP))])

table(bd_APVP_ad$FXETARIA2[which(is.na(bd_APVP_ad$POP))])

save(bd_APVP_ad,file="Z:\\Google Drive\\UNIRIO\\Projetos\\IC\\2020-2021\\Banco de Dados\\bd_APVP_ad_V3_22MAR21.RData")

save(bd_APVP_id,file="Z:\\Google Drive\\UNIRIO\\Projetos\\IC\\2020-2021\\Banco de Dados\\bd_APVP_id_V3_22MAR21.RData")

# Padronizando as Taxas de APVP

## Identificando a população em risco

# Como a expectativa de vida aumenta, as populações em risco são distintas ao longo dos anos, segundo sexo e grup etário:

table(bd_APVP_ad$FXETARIA2[which(bd_APVP_ad$SEXOPAD == "M")],bd_APVP_ad$anoarq[which(bd_APVP_ad$SEXOPAD == "M")])

## Sexo Masculino, população geral:
# De 1980 a 1982, de 0 a 59
# De 1983 a 1996, de 0 a 64
# De 1997 a 2010, de 0 a 69
# De 2011 a 2019, de 0 a 74

table(bd_APVP_id$FXETARIA2[which(bd_APVP_id$SEXOPAD == "M")],bd_APVP_id$anoarq[which(bd_APVP_id$SEXOPAD == "M")])

## Sexo Masculino,população idosa:
# De 1980 a 1981, de 60 a 74
# De 1982 a 2019, de 60 a 79

table(bd_APVP_ad$FXETARIA2[which(bd_APVP_ad$SEXOPAD == "F")],bd_APVP_ad$anoarq[which(bd_APVP_ad$SEXOPAD == "F")])

## Sexo Feminino, populaçãogral:
# De 1980 a 1990, de 0 a 69
# De 1991 a 2003, de 0 a 74
# De 2004 a 2019, de 0 a 79

table(bd_APVP_id$FXETARIA2[which(bd_APVP_id$SEXOPAD == "F")],bd_APVP_id$anoarq[which(bd_APVP_id$SEXOPAD == "F")])

## Sexo Feminini, população idosa:
# De 1980 a 1993, de 60 a 79
# De 1994 a 2019, de 60 a 80+

# Agregando dados do Brasil

# Populção de referência por faixa etária
names(pop_micro_80_19)

table(is.na(pop_micro_80_19$MICROCOD))

table(is.na(pop_micro_80_19$FXETARIA2))

table(is.na(pop_micro_80_19$POP))

table(is.na(pop_micro_80_19$SEXOPAD))

POPMICNA <- pop_micro_80_19[which(is.na(pop_micro_80_19$POP)),]

table(POPMICNA$MICROCOD)

pop_micro_80_19[-which(is.na(pop_micro_80_19$POP)),] %>% filter(anoarq==2019) %>%  group_by(FXETARIA2,anoarq,SEXOPAD) %>% summarise(POP=sum(POP)) -> pop_ref

pop_ref

save(pop_ref,file="Z:\\Google Drive\\UNIRIO\\Projetos\\IC\\2020-2021\\Banco de Dados\\pop_ref_ad_V3_22MAR21.RData")

# Populção de referência total

# Adulta
pop_micro_80_19

pop_micro_80_19$anoarq <- as.numeric(pop_micro_80_19$anoarq)

pop_micro_80_19[,c(2,3,5)] %>% filter({SEXOPAD == "M" & anoarq <= 1982 & FXETARIA2 %in% unique(pop_micro_80_19$FXETARIA2)[1:12]}|
                                        {SEXOPAD == "M" & anoarq >= 1983 & anoarq <= 1996 & FXETARIA2 %in% unique(pop_micro_80_19$FXETARIA2)[1:13]}|
                                        {SEXOPAD == "M" & anoarq >= 1997 & anoarq <= 2010 & FXETARIA2 %in% unique(pop_micro_80_19$FXETARIA2)[1:14]}|
                                        {SEXOPAD == "M" & anoarq >= 2011 & FXETARIA2 %in% unique(pop_micro_80_19$FXETARIA2)[1:15]}|
                                        {SEXOPAD == "F" & anoarq <= 1990 & FXETARIA2 %in% unique(pop_micro_80_19$FXETARIA2)[1:14]}|
                                        {SEXOPAD == "F" & anoarq >= 1991 & anoarq <= 2003 & FXETARIA2 %in% unique(pop_micro_80_19$FXETARIA2)[1:15]}|
                                        {SEXOPAD == "F" & anoarq >=2004 & FXETARIA2 %in% unique(pop_micro_80_19$FXETARIA2)[1:16]}) -> pop_ref_tot_filt_ad

pop_ref_tot_filt_ad

pop_ref_tot_ad <- left_join(unique(pop_ref_tot_filt_ad),pop_ref[,-2])

pop_ref_tot_ad

table(pop_ref_tot_ad$FXETARIA2[which(pop_ref_tot_ad$SEXOPAD=="M")],pop_ref_tot_ad$anoarq[which(pop_ref_tot_ad$SEXOPAD=="M")])

table(pop_ref_tot_ad$FXETARIA2[which(pop_ref_tot_ad$SEXOPAD=="F")],pop_ref_tot_ad$anoarq[which(pop_ref_tot_ad$SEXOPAD=="F")])

pop_ref_tot_ad %>% group_by(anoarq,SEXOPAD) %>% summarise(POP_REF_TOT=sum(POP)) -> pop_ref_tot_ad

# Idosa
pop_micro_80_19[,c(2,3,5)] %>% filter({SEXOPAD == "M" & anoarq <= 1981 & FXETARIA2 %in% unique(pop_micro_80_19$FXETARIA2)[13:15]}|
                                        {SEXOPAD == "M" & anoarq >= 1982 & FXETARIA2 %in% unique(pop_micro_80_19$FXETARIA2)[13:16]}|
                                        {SEXOPAD == "F" & anoarq <= 1993 & FXETARIA2 %in% unique(pop_micro_80_19$FXETARIA2)[13:16]}|
                                        {SEXOPAD == "F" & anoarq >= 1994 & FXETARIA2 %in% unique(pop_micro_80_19$FXETARIA2)[13:17]}) -> pop_ref_tot_filt_id

pop_ref_tot_filt_id

pop_ref_tot_id <- left_join(unique(pop_ref_tot_filt_id),pop_ref[,-2])

pop_ref_tot_id

table(pop_ref_tot_id$FXETARIA2[which(pop_ref_tot_id$SEXOPAD=="M")],pop_ref_tot_id$anoarq[which(pop_ref_tot_id$SEXOPAD=="M")])

table(pop_ref_tot_id$FXETARIA2[which(pop_ref_tot_id$SEXOPAD=="F")],pop_ref_tot_id$anoarq[which(pop_ref_tot_id$SEXOPAD=="F")])

pop_ref_tot_id %>% group_by(anoarq,SEXOPAD) %>% summarise(POP_REF_TOT=sum(POP)) -> pop_ref_tot_id

pop_ref_tot_id

# Salvando:

save(pop_ref_tot_ad,file="Z:\\Google Drive\\UNIRIO\\Projetos\\IC\\2020-2021\\Banco de Dados\\pop_ref_tot_ad_V4_22MAR21.RData")

save(pop_ref_tot_id,file="Z:\\Google Drive\\UNIRIO\\Projetos\\IC\\2020-2021\\Banco de Dados\\pop_ref_tot_id_V4_22MAR21.RData")

# Padronizando taxas:

## fazendo o merge com população de referência por faixa etária e total

### População Adulta

table(is.na(bd_APVP_ad))

table(is.na(bd_APVP_ad$MICROCOD))

table(is.na(bd_APVP_ad$NOMEMICRO))

table(is.na(bd_APVP_ad$APVP))

table(is.na(bd_APVP_ad$POP))

names(bd_APVP_ad)[9] <- "POP_RISCO"

pop_ref$anoarq <- NULL

bd_APVP_ad1 <- left_join(bd_APVP_ad,pop_ref)

names(bd_APVP_ad1)[10] <- "POP_REF"

names(pop_ref_tot_ad)[1] <- "anoarq"

pop_ref_tot_ad$anoarq <- as.character(pop_ref_tot_ad$anoarq)

bd_APVP_ad2 <- left_join(bd_APVP_ad1,pop_ref_tot_ad)

bd_APVP_ad2

names(bd_APVP_ad2)

bd_APVP_ad2$TX_APVP_IE <- bd_APVP_ad2$APVP*1000/bd_APVP_ad2$POP_RISCO

bd_APVP_ad2

bd_APVP_ad2$TX_APVP_ESPERADO <- {bd_APVP_ad2$TX_APVP_IE}*{bd_APVP_ad2$POP_REF/bd_APVP_ad2$POP_REF_TOT}

data.frame(bd_APVP_ad2[c(1:10),])

bd_APVP_ad2 %>% group_by(MICROCOD, UF, REG, anoarq,SEXOPAD) %>% summarise(TX_APVP_PAD = sum(TX_APVP_ESPERADO,na.rm = T)) -> bd_APVP_ad_pad

bd_APVP_ad_pad

table(is.na(bd_APVP_ad_pad))

table(is.na(bd_APVP_ad_pad$MICROCOD))

# Salvando:

save(bd_APVP_ad_pad, file = "Z:\\Google Drive\\UNIRIO\\Projetos\\IC\\2020-2021\\Banco de Dados\\bd_APVP_ad_pad_V2_22MAR21.RData")

### População Idosa
names(bd_APVP_id)[9] <- "POP_RISCO"

bd_APVP_id1 <- left_join(bd_APVP_id,pop_ref)

names(bd_APVP_id1)[10] <- "POP_REF"

names(pop_ref_tot_id)[1] <- "anoarq"

pop_ref_tot_id$anoarq <- as.character(pop_ref_tot_id$anoarq)

bd_APVP_id2 <- left_join(bd_APVP_id1,pop_ref_tot_id)

bd_APVP_id2

names(bd_APVP_id2)

bd_APVP_id2$TX_APVP_IE <- bd_APVP_id2$APVP*1000/bd_APVP_id2$POP_RISCO

bd_APVP_id2

bd_APVP_id2$TX_APVP_ESPERADO <- {bd_APVP_id2$TX_APVP_IE}*{bd_APVP_id2$POP_REF/bd_APVP_id2$POP_REF_TOT}

data.frame(bd_APVP_id2[c(1:10),])

bd_APVP_id2 %>% group_by(MICROCOD, UF, REG, anoarq,SEXOPAD) %>% summarise(TX_APVP_PAD = sum(TX_APVP_ESPERADO,na.rm = T)) -> bd_APVP_id_pad

bd_APVP_id_pad

table(is.na(bd_APVP_id_pad))

# Salvando:

save(bd_APVP_id_pad, file = "Z:\\Google Drive\\UNIRIO\\Projetos\\IC\\2020-2021\\Banco de Dados\\bd_APVP_id_pad_V2_22MAR21.RData")

bd_APVP_id_pad

load("Z:\\Google Drive\\UNIRIO\\Projetos\\IC\\2020-2021\\Banco de Dados\\bd_APVP_ad_pad_V2_22MAR21.RData")

load("Z:\\Google Drive\\UNIRIO\\Projetos\\IC\\2020-2021\\Banco de Dados\\bd_APVP_id_pad_V2_22MAR21.RData")

# Verificando "faltantes":

table(bd_APVP_ad_pad$REG,bd_APVP_ad_pad$anoarq,useNA = "a")

mic_ad <- unique(bd_APVP_ad_pad$MICROCOD)

mic_ad[-which(mic_ad %in% bd_APVP_ad_pad$MICROCOD[which(bd_APVP_ad_pad$anoarq=="1987")])]

falt_mic <- list()

for(i in 1:length(1980:2019)){
  
  falt_mic[[i]] <- mic_ad[-which(mic_ad %in% bd_APVP_ad_pad$MICROCOD[which(bd_APVP_ad_pad$anoarq==as.character(c(1980:2019)[i]))])]
  
}

falt_mic_ad <- list()

for(i in 1:length(1980:2019)){
  
  falt_mic_ad[[i]] <- mic_ad[-which(mic_ad %in% bd_APVP_ad_pad$MICROCOD[which(bd_APVP_ad_pad$anoarq==as.character(c(1980:2019)[i]))])]
  
}

falt_mic_ad

