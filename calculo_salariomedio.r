setwd("C:\\Users\\EricaCastilho\\Dropbox\\UFOP\\Grupo NAVE\\Desafio de Dados")


setwd("C:\\Users\\UFOP\\Dropbox\\UFOP\\Grupo NAVE\\Desafio de Dados")
setwd('/home/ericaestatistica/Documentos/Erica/Desafio de Dados')
require(data.table)
require(ggplot2)
require(tidyr)



dados_professor<-fread('TS_PROFESSOR.csv')

dados_professor$TX_RESP_Q010<-factor(dados_professor$TX_RESP_Q010)

levels(dados_professor$TX_RESP_Q010)<-c(NA,levels(dados_professor$TX_RESP_Q010)[-1])

table(dados_professor$TX_RESP_Q010)

sum(table(dados_professor$TX_RESP_Q010)*c( mean(c(937)),  mean(c(937,1405)),
                                       mean(c(1405,1874)),  mean(c(1874,2342)),
                                       mean(c(2342,2811)),  mean(c(2811,3279)),
                                       mean(c(3279,3748)),  mean(c(3748,4685)),
                                       mean(c(4685,6559)),  mean(c(6559,9370)),
                                       9370))/sum(table(dados_professor$TX_RESP_Q010))

salario_professor_escola<-dados_professor[,.(sum(table(TX_RESP_Q010)*c( mean(c(937)),  mean(c(937,1405)),
                                                              mean(c(1405,1874)),  mean(c(1874,2342)),
                                                              mean(c(2342,2811)),  mean(c(2811,3279)),
                                                              mean(c(3279,3748)),  mean(c(3748,4685)),
                                                              mean(c(4685,6559)),  mean(c(6559,9370)),
                                                              9370))/sum(table(TX_RESP_Q010))),
                by=ID_ESCOLA]

mean(salario_professor_escola$V1,na.rm=T)


dados_professor[ID_ESCOLA==11024666,
                TX_RESP_Q010]


dados_professor[ID_ESCOLA==11024968,
                TX_RESP_Q010]
mean(c(3748,4685)
save(salario_professor_escola,file='Salario_professor.Rdata')
