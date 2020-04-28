
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

dados_alunos<-merge(dados_alunos,salario_professor_escola,all.x=T,by='ID_ESCOLA')

dados_alunos$TX_RESP_Q002<-factor(dados_alunos$TX_RESP_Q002)
levels(dados_alunos$TX_RESP_Q002)<-c(NA,'Branca','Preta','Parda','Amarelo',
                                     'Indigena','Nao quero declarar')

colnames(dados_alunos)[colnames(dados_alunos)=='TX_RESP_Q002']<-"Raca"


# Filtra somente escolas publicas

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
               ),by=NIVEL_SOCIO_ECONOMICO]


data_long <- gather(media_nse, Disciplina, Media, Media_Portugues:Media_Matematica, factor_key=TRUE)
data_long

ggplot(data_long,aes(x=NIVEL_SOCIO_ECONOMICO,y=Media,group=Disciplina))+geom_line(aes(color=Disciplina))+
  geom_point(aes(color=Disciplina))


fwrite(media_nse,file="Media_NSE_Geral.csv")
save(media_nse,file="Media_NSE_Geral.Rdata")

## Media Prova por Raca

media_raca<-dados_alunos[Raca=="Branca" | Raca=="Preta" | Raca=="Parda",
                        .(Media_Portugues=mean(PROFICIENCIA_LP_SAEB,na.rm=T),
                          Media_Matematica=mean(PROFICIENCIA_MT_SAEB,na.rm=T),
                          nPortugues=length(na.omit(PROFICIENCIA_LP_SAEB)),
                          nMatematica=length(na.omit(PROFICIENCIA_LP_SAEB))
                        ),by=Raca]


save(media_raca,file="Media_Raca_Geral.Rdata")
fwrite(media_raca,file="Media_Raca_Geral.csv")


data_long <- gather(media_raca, Disciplina, Media, Media_Portugues:Media_Matematica, factor_key=TRUE)
data_long

ggplot(data_long,aes(x=Raca,y=Media,group=Disciplina))+geom_line(aes(color=Disciplina))+
  geom_point(aes(color=Disciplina))


## Media Prova por Sexo

media_sexo<-dados_alunos[!is.na(Sexo),
                         .(Media_Portugues=mean(PROFICIENCIA_LP_SAEB,na.rm=T),
                           Media_Matematica=mean(PROFICIENCIA_MT_SAEB,na.rm=T),
                           nPortugues=length(na.omit(PROFICIENCIA_LP_SAEB)),
                           nMatematica=length(na.omit(PROFICIENCIA_LP_SAEB))
                         ),by=Sexo]


fwrite(media_raca,file="Media_Sexo_Geral.csv")

save(media_sexo,file="Media_Sexo_Geral.Rdata")
data_long <- gather(media_sexo, Disciplina, Media, Media_Portugues:Media_Matematica, factor_key=TRUE)
data_long

ggplot(data_long,aes(x=Sexo,y=Media,group=Disciplina))+geom_line(aes(color=Disciplina))+
  geom_point(aes(color=Disciplina))

fwrite(media_raca,file="Media_Raca_por_Geral.csv")


# Limites dos graficos
range(media_nse[,c('Media_Portugues','Media_Matematica')],
  media_raca[,c('Media_Portugues','Media_Matematica')],
  media_sexo[,c('Media_Portugues','Media_Matematica')])

# Distribuicao de raca por grupo de NSE

prop_pretosbrancos_nse<-dados_alunos[!is.na(NIVEL_SOCIO_ECONOMICO)&
                                       NIVEL_SOCIO_ECONOMICO!="",.(Prop_brancos=prop.table(table(Raca))[1],
                                                                   Prop_pretos=prop.table(table(Raca))[2],
                                                                   Prop_pardos=prop.table(table(Raca))[3]),
                                     by=NIVEL_SOCIO_ECONOMICO]


data_long <- gather(prop_pretosbrancos_nse, Raca, proporcao, Prop_brancos:Prop_pardos, factor_key=TRUE)
data_long

ggplot(data=data_long, aes(x=NIVEL_SOCIO_ECONOMICO, y=proporcao, fill=Raca)) +
  geom_bar(stat="identity")

fwrite(prop_pretosbrancos_nse,file="prop_pretosbrancos_nse_Geral.csv")




# Distribuicao de raca por grupo de NSE e sexo

prop_pretosbrancos_nse_sexo<-dados_alunos[!is.na(NIVEL_SOCIO_ECONOMICO)&
                                       NIVEL_SOCIO_ECONOMICO!="",.(Prop_brancos=prop.table(table(Raca))[1],
                                                                   Prop_pretos=prop.table(table(Raca))[2],
                                                                   Prop_pardos=prop.table(table(Raca))[3]),
                                     by=list(NIVEL_SOCIO_ECONOMICO,Sexo)]


data_long <- gather(prop_pretosbrancos_nse_sexo, Raca, proporcao, Prop_brancos:Prop_pardos, factor_key=TRUE)
data_long

ggplot(data=data_long, aes(x=NIVEL_SOCIO_ECONOMICO, y=proporcao, fill=Raca)) +
  geom_bar(stat="identity")+facet_grid(.~Sexo)


total_pretosbrancos_nse_sexo<-dados_alunos[!is.na(NIVEL_SOCIO_ECONOMICO)&
                                            NIVEL_SOCIO_ECONOMICO!="" & !is.na(Sexo),.(Prop_brancos=(table(Raca))[1],
                                                                        Prop_pretos=(table(Raca))[2],
                                                                        Prop_pardos=(table(Raca))[3]),
                                          by=list(NIVEL_SOCIO_ECONOMICO,Sexo)]


data_long <- gather(total_pretosbrancos_nse_sexo, Raca, proporcao, Prop_brancos:Prop_pardos, factor_key=TRUE)
data_long

ggplot(data=data_long, aes(x=NIVEL_SOCIO_ECONOMICO, y=proporcao, fill=Raca)) +
  geom_bar(stat="identity")+facet_grid(.~Sexo)

## Media Prova por Raca separado por grupo de NSE

media_raca_nse<-dados_alunos[(Raca=="Branca" | Raca=="Preta" | Raca=="Parda")
                         & !is.na(NIVEL_SOCIO_ECONOMICO) & NIVEL_SOCIO_ECONOMICO!="",
                         .(Media_Portugues=mean(PROFICIENCIA_LP_SAEB,na.rm=T),
                           Media_Matematica=mean(PROFICIENCIA_MT_SAEB,na.rm=T),
                           nPortugues=length(na.omit(PROFICIENCIA_LP_SAEB)),
                           nMatematica=length(na.omit(PROFICIENCIA_LP_SAEB))
                         ),by=list(Raca,NIVEL_SOCIO_ECONOMICO)]

fwrite(media_raca_nse,file="Media_Raca_NSE_Geral.csv")

save(media_raca_nse,file="Media_Raca_NSE_Geral.Rdata")

data_long <- data.table(gather(media_raca_nse, Disciplina, Media, Media_Portugues:Media_Matematica, factor_key=TRUE))
data_long

ggplot(data_long[Disciplina == 'Media_Portugues',],aes(x=Raca,y=Media,
                                                   group=NIVEL_SOCIO_ECONOMICO))+
  geom_line(aes(color=NIVEL_SOCIO_ECONOMICO))+
  geom_point(aes(color=NIVEL_SOCIO_ECONOMICO))


## Media Prova por Raca separado por grupo de Sexo

media_raca_sexo<-dados_alunos[(Raca=="Branca" | Raca=="Preta" | Raca=="Parda")
                             & !is.na(Sexo) ,
                             .(Media_Portugues=mean(PROFICIENCIA_LP_SAEB,na.rm=T),
                               Media_Matematica=mean(PROFICIENCIA_MT_SAEB,na.rm=T),
                               nPortugues=length(na.omit(PROFICIENCIA_LP_SAEB)),
                               nMatematica=length(na.omit(PROFICIENCIA_LP_SAEB))
                             ),by=list(Raca,Sexo)]


fwrite(media_raca_sexo,file="Media_Raca_Sexo_Geral.csv")

data_long <- data.table(gather(media_raca_sexo, Disciplina, Media, Media_Portugues:Media_Matematica, factor_key=TRUE))
data_long

ggplot(data_long[Disciplina == 'Media_Portugues',],aes(x=Raca,y=Media,
                                                       group=Sexo))+
  geom_line(aes(color=Sexo))+
  geom_point(aes(color=Sexo))


ggplot(data_long[Disciplina == 'Media_Matematica',],aes(x=Raca,y=Media,
                                                       group=Sexo))+
  geom_line(aes(color=Sexo))+
  geom_point(aes(color=Sexo))



## Media Prova por Raca separado por grupo de NSE e Sexo


media_raca_nse_sexo<-dados_alunos[!is.na(Sexo) & (Raca=="Branca" | Raca=="Preta" | Raca=="Parda")
                             & !is.na(NIVEL_SOCIO_ECONOMICO) & NIVEL_SOCIO_ECONOMICO!="",
                             .(Media_Portugues=mean(PROFICIENCIA_LP_SAEB,na.rm=T),
                               Media_Matematica=mean(PROFICIENCIA_MT_SAEB,na.rm=T),
                               nPortugues=length(na.omit(PROFICIENCIA_LP_SAEB)),
                               nMatematica=length(na.omit(PROFICIENCIA_LP_SAEB))
                             ),by=list(Raca,NIVEL_SOCIO_ECONOMICO,Sexo)]

fwrite(media_raca_nse_sexo,file="Media_Raca_Sexo_NSE_Geral.csv")

save(media_raca_nse_sexo,file="Media_Raca_Sexo_NSE_Geral.Rdata")


data_long <- data.table(gather(media_raca_nse_sexo, Disciplina, Media, Media_Portugues:Media_Matematica, factor_key=TRUE))
data_long

ggplot(data_long[Disciplina == 'Media_Portugues',],aes(x=Raca,y=Media,
                                                       group=NIVEL_SOCIO_ECONOMICO))+
  geom_line(aes(color=NIVEL_SOCIO_ECONOMICO))+
  geom_point(aes(color=NIVEL_SOCIO_ECONOMICO))+facet_grid(.~Sexo)


## Media prova por salario medio

media_escola<-dados_alunos[
                            ,.(Media_Portugues=mean(PROFICIENCIA_LP_SAEB,na.rm=T),
                               Media_Matematica=mean(PROFICIENCIA_MT_SAEB,na.rm=T)
                             ),by=ID_ESCOLA]

colnames(salario_professor_escola)[2]<-"Salario_Medio"

media_escola<-merge(media_escola,salario_professor_escola)

media_escola<-merge(media_escola,
              dados_escola[,c('ID_ESCOLA','ID_MUNICIPIO','NIVEL_SOCIO_ECONOMICO')],all.x=T)

data_long <- data.table(gather(media_escola, Disciplina, Media, Media_Portugues:Media_Matematica, factor_key=TRUE))
data_long

ggplot(data_long,aes(x=Salario_Medio,y=Media,group=Disciplina))+geom_line(aes(color=Disciplina))+
  geom_point(aes(color=Disciplina))

media_salario_municipio<-media_escola[,.(Media_Portugues=mean(Media_Portugues,na.rm=T),
                                        Media_Matematica=mean(Media_Matematica,na.rm=T),
                                        Salario_Medio=mean(Salario_Medio,na.rm=T)
                                        ),by=ID_MUNICIPIO]

data_long <- data.table(gather(media_salario_municipio, Disciplina, Media, Media_Portugues:Media_Matematica, factor_key=TRUE))
data_long

ggplot(data_long,aes(x=Salario_Medio,y=Media,group=Disciplina))+
  geom_point(aes(color=Disciplina))


ggplot(media_escola[!is.na(NIVEL_SOCIO_ECONOMICO) & NIVEL_SOCIO_ECONOMICO!=""],aes(x=Salario_Medio,y=Media_Matematica))+geom_point()+
  facet_grid(.~NIVEL_SOCIO_ECONOMICO)



## salva bases

save(media_nse,file="Media_NSE_geral.Rdata")
save(media_raca,file="Media_Raca_geral.Rdata")
save(prop_pretosbrancos_nse,file="Prop_pretosbrancos_nse_geral.Rdata")
save(media_raca_nse,file="Media_Raca_NSE_geral.Rdata")
save(media_escola,file="Media_salario_escola_geral.Rdata")
save(media_salario_municipio,file="Media_salario_municipio_geral.Rdata")
