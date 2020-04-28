require(data.table)
require(ggplot2)
require(tidyr)

setwd("C:\\Users\\UFOP\\Dropbox\\UFOP\\Grupo NAVE\\Desafio de Dados")
setwd("C:\\Users\\EricaCastilho\\Dropbox\\UFOP\\Grupo NAVE\\Desafio de Dados")

setwd('/home/ericaestatistica/Documentos/Erica/Desafio de Dados')
load("~/Documentos/Erica/Desafio de Dados/Salario_professor.Rdata")


dados_escola<-fread('TS_ESCOLA.csv')

#save(dados_escola,file='dados_escola.Rdata')

# Carregar no tableau - Arquivo de estatistica

dados_alunos<-fread('TS_ALUNO_5EF.csv')

dados_alunos<-merge(dados_alunos,dados_escola[,c('ID_ESCOLA','NIVEL_SOCIO_ECONOMICO',
                                                 'ID_DEPENDENCIA_ADM')],
                    all.x=T)
load("C:/Users/EricaCastilho/Dropbox/UFOP/Grupo NAVE/Desafio de Dados/Salario_professor.Rdata")

dados_alunos<-merge(dados_alunos,salario_professor_escola,all.x=T,by='ID_ESCOLA')

dados_alunos$TX_RESP_Q002<-factor(dados_alunos$TX_RESP_Q002)

levels(dados_alunos$TX_RESP_Q002)<-c(NA,'Branca','Preta','Parda','Amarelo',
                                     'Indigena','Nao quero declarar')

colnames(dados_alunos)[colnames(dados_alunos)=='TX_RESP_Q002']<-"Raca"

dados_alunos<-dados_alunos[ID_DEPENDENCIA_ADM!=4,]

# Relabel sexo
dados_alunos$TX_RESP_Q001<-factor(dados_alunos$TX_RESP_Q001)
levels(dados_alunos$TX_RESP_Q001)<-c(NA,'Masculino','Feminino')
colnames(dados_alunos)[colnames(dados_alunos)=="TX_RESP_Q001"]<-"Sexo"


## Media Prova por NSE

media_nse<-dados_alunos[!is.na(NIVEL_SOCIO_ECONOMICO) & NIVEL_SOCIO_ECONOMICO!="",
             .(Media_Portugues=mean(PROFICIENCIA_LP_SAEB,na.rm=T),
               Media_Matematica=mean(PROFICIENCIA_MT_SAEB,na.rm=T),
               nPortugues=length(na.omit(PROFICIENCIA_LP_SAEB)),
               nMatematica=length(na.omit(PROFICIENCIA_LP_SAEB))
               ),by=list(NIVEL_SOCIO_ECONOMICO,ID_UF)]

# Acrescenta nome
nomes_estados<-fread("Nomes_estados.csv",encoding='Latin-1')
media_nse<-merge(media_nse,nomes_estados,all.x=T)

fwrite(media_nse,file="Media_NSE_por_estado.csv")

save(media_nse,file="Media_NSE_por_estadao.Rdata")

data_long <- gather(media_nse, Disciplina, Media, Media_Portugues:Media_Matematica, factor_key=TRUE)
data_long

ggplot(data_long,aes(x=NIVEL_SOCIO_ECONOMICO,y=Media,group=Disciplina))+
  geom_point(aes(color=Disciplina))


data_long <- gather(media_nse[ID_UF==23], Disciplina, Media, Media_Portugues:Media_Matematica, factor_key=TRUE)
data_long

ggplot(data_long,aes(x=NIVEL_SOCIO_ECONOMICO,y=Media,group=Disciplina))+
  geom_point(aes(color=Disciplina))+geom_line(aes(color=Disciplina))


## Media Prova por Raca

media_raca<-dados_alunos[Raca=="Branca" | Raca=="Preta" | Raca=="Parda",
                        .(Media_Portugues=mean(PROFICIENCIA_LP_SAEB,na.rm=T),
                          Media_Matematica=mean(PROFICIENCIA_MT_SAEB,na.rm=T),
                          nPortugues=length(na.omit(PROFICIENCIA_LP_SAEB)),
                          nMatematica=length(na.omit(PROFICIENCIA_LP_SAEB))
                        ),by=list(Raca,ID_UF)]

media_raca<-merge(media_raca,nomes_estados,all.x=T)


fwrite(media_raca,file="Media_Raca_por_estado.csv")
save(media_raca,file="Media_Raca_por_estado.Rdata")


data_long <- gather(media_raca, Disciplina, Media, Media_Portugues:Media_Matematica, factor_key=TRUE)
data_long

ggplot(data_long,aes(x=Raca,y=Media,group=Disciplina))+
  geom_point(aes(color=Disciplina))


data_long <- gather(media_raca[ID_UF==23], Disciplina, Media, Media_Portugues:Media_Matematica, factor_key=TRUE)
data_long

ggplot(data_long,aes(x=Raca,y=Media,group=Disciplina))+
  geom_point(aes(color=Disciplina))+geom_line(aes(color=Disciplina))


## Media Prova por Sexo

media_sexo<-dados_alunos[!is.na(Sexo),
                         .(Media_Portugues=mean(PROFICIENCIA_LP_SAEB,na.rm=T),
                           Media_Matematica=mean(PROFICIENCIA_MT_SAEB,na.rm=T),
                           nPortugues=length(na.omit(PROFICIENCIA_LP_SAEB)),
                           nMatematica=length(na.omit(PROFICIENCIA_LP_SAEB))
                         ),by=list(Sexo,ID_UF)]

media_sexo<-merge(media_sexo,nomes_estados,all.x=T)



fwrite(media_sexo,file="Media_Sexo_por_estado.csv")

save(media_sexo,file="Media_Sexo_por_estado.Rdata")

data_long <- gather(media_raca, Disciplina, Media, Media_Portugues:Media_Matematica, factor_key=TRUE)
data_long

ggplot(data_long,aes(x=Sexo,y=Media,group=Disciplina))+
  geom_point(aes(color=Disciplina))


data_long <- gather(media_raca[ID_UF==23], Disciplina, Media, Media_Portugues:Media_Matematica, factor_key=TRUE)
data_long

ggplot(data_long,aes(x=Sexo,y=Media,group=Disciplina))+
  geom_point(aes(color=Disciplina))+geom_line(aes(color=Disciplina))




# Distribuicao de raca por grupo de NSE

prop_pretosbrancos_nse<-dados_alunos[!is.na(NIVEL_SOCIO_ECONOMICO)&
                                       NIVEL_SOCIO_ECONOMICO!="",.(Prop_brancos=prop.table(table(Raca))[1],
                                                                   Prop_pretos=prop.table(table(Raca))[2],
                                                                   Prop_pardos=prop.table(table(Raca))[3]),
                                     by=list(NIVEL_SOCIO_ECONOMICO,ID_UF)]


data_long <- gather(prop_pretosbrancos_nse[ID_UF==23], Raca, proporcao, Prop_brancos:Prop_pardos, factor_key=TRUE)
data_long

ggplot(data=data_long, aes(x=NIVEL_SOCIO_ECONOMICO, y=proporcao, fill=Raca)) +
  geom_bar(stat="identity")

# Nomes dos estados
prop_pretosbrancos_nse<-merge(prop_pretosbrancos_nse,nomes_estados,all.x=T)
# Salva base
fwrite(prop_pretosbrancos_nse,file="prop_pretosbrancos_nse_por_estado.csv")


save(prop_pretosbrancos_nse,file="prop_pretosbrancos_nse_por_estado.Rdata")

## Media Prova por Raca separado por grupo de NSE

media_raca_nse<-dados_alunos[(Raca=="Branca" | Raca=="Preta" | Raca=="Parda")
                         & !is.na(NIVEL_SOCIO_ECONOMICO) & NIVEL_SOCIO_ECONOMICO!="",
                         .(Media_Portugues=mean(PROFICIENCIA_LP_SAEB,na.rm=T),
                           Media_Matematica=mean(PROFICIENCIA_MT_SAEB,na.rm=T),
                           nPortugues=length(na.omit(PROFICIENCIA_LP_SAEB)),
                           nMatematica=length(na.omit(PROFICIENCIA_LP_SAEB))
                         ),by=list(Raca,NIVEL_SOCIO_ECONOMICO,ID_UF)]




data_long <- data.table(gather(media_raca_nse[ID_UF==23], Disciplina, Media, Media_Portugues:Media_Matematica, factor_key=TRUE))
data_long

ggplot(data_long[Disciplina == 'Media_Portugues',],aes(x=Raca,y=Media,
                                                   group=NIVEL_SOCIO_ECONOMICO))+
  geom_line(aes(color=NIVEL_SOCIO_ECONOMICO))+
  geom_point(aes(color=NIVEL_SOCIO_ECONOMICO))


# Nomes dos estados
media_raca_nse<-merge(media_raca_nse,nomes_estados,all.x=T)
# Salva base
fwrite(media_raca_nse,file="Media_Raca_NSE_por_estado.csv")


save(media_raca_nse,file="Media_Raca_NSE_por_estado.Rdata")

## Media prova por salario medio

media_escola<-dados_alunos[
                            ,.(Media_Portugues=mean(PROFICIENCIA_LP_SAEB,na.rm=T),
                               Media_Matematica=mean(PROFICIENCIA_MT_SAEB,na.rm=T)
                             ),by=ID_ESCOLA]

colnames(salario_professor_escola)[2]<-"Salario_Medio"

media_escola<-merge(media_escola,salario_professor_escola)

media_escola<-merge(media_escola,
              dados_escola[,c('ID_ESCOLA','ID_MUNICIPIO','ID_UF','NIVEL_SOCIO_ECONOMICO')],all.x=T)

data_long <- data.table(gather(media_escola[ID_UF==23], Disciplina, Media, Media_Portugues:Media_Matematica, factor_key=TRUE))
data_long

ggplot(data_long,aes(x=Salario_Medio,y=Media,group=Disciplina))+geom_line(aes(color=Disciplina))+
  geom_point(aes(color=Disciplina))

media_salario_municipio<-media_escola[,.(Media_Portugues=mean(Media_Portugues,na.rm=T),
                                        Media_Matematica=mean(Media_Matematica,na.rm=T),
                                        Salario_Medio=mean(Salario_Medio,na.rm=T),
                                        numero_escolas=length(unique(ID_ESCOLA))
                                        ),by=list(ID_MUNICIPIO,ID_UF)]

data_long <- data.table(gather(media_salario_municipio[ID_UF==23], Disciplina, Media, Media_Portugues:Media_Matematica, factor_key=TRUE))
data_long

ggplot(data_long,aes(x=Salario_Medio,y=Media,group=Disciplina))+
  geom_point(aes(color=Disciplina))


# Nomes dos estados
media_salario_municipio<-merge(media_salario_municipio,nomes_estados,all.x=T)


# Nomes dos Municipios


require(rgdal)
require(uft8)
options(encoding="utf-8")
setwd('/home/ericaestatistica/Documentos/Erica/Desafio de Dados/br_municipios')


setwd('/home/ericaestatistica/Documentos/Erica/Desafio de Dados/br_municipios')
setwd('C:\\Users\\EricaCastilho\\Dropbox\\UFOP\\Grupo NAVE\\Desafio de Dados\\br_municipios\\br_municipios')
brasil_mapa_nova = readOGR(".", "BRMUE250GC_SIR")

aux<-match(media_salario_municipio$ID_MUNICIPIO,brasil_mapa_nova$CD_GEOCMU)

iconv(brasil_mapa_nova$NM_MUNICIP[1], from="UTF-8", to="latin1")

brasil_mapa_nova$NM_MUNICIP<-iconv(brasil_mapa_nova$NM_MUNICIP, from="UTF-8", to="latin1")



media_salario_municipio$Nome_municiopio<-brasil_mapa_nova$NM_MUNICIP[aux]


setwd('/home/ericaestatistica/Documentos/Erica/Desafio de Dados')

setwd('C:\\Users\\EricaCastilho\\Dropbox\\UFOP\\Grupo NAVE\\Desafio de Dados')

# Salva base
fwrite(media_salario_municipio,file="Media_salario_municipio_por_estado.csv")
save(media_salario_municipio,file="Media_salario_municipio_por_estado.Rdata")

ggplot(media_escola[!is.na(NIVEL_SOCIO_ECONOMICO) & NIVEL_SOCIO_ECONOMICO!="" & ID_UF==23],aes(x=Salario_Medio,y=Media_Matematica))+geom_point()+
  facet_grid(.~NIVEL_SOCIO_ECONOMICO)



## salva bases

save(media_nse,file="Media_NSE_porEstado.Rdata")
save(media_raca,file="Media_Raca_porEstado.Rdata")
save(prop_pretosbrancos_nse,file="Prop_pretosbrancos_nse_porEstado.Rdata")
save(media_raca_nse,file="Media_Raca_NSE_porEstado.Rdata")
save(media_escola,file="Media_salario_escola_porEstado.Rdata")
save(media_salario_municipio,file="Media_salario_municipio_porEstado.Rdata")
