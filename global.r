require(ggplot2)
require(shiny)
require(shinydashboard)
require(plotly)
require(shinyjs)
require(data.table)
require(DT)
require(rmarkdown)
require(png)
require(scales)

# Carrega os dados
load('Resultados_por_escola.Rdata')
load('Resultados_por_municipio.Rdata')


## Leitura dos textos explicativos em formato .md

texto_intro<- function(){
  
  includeMarkdown("introducao.md")
}

texto_contexto_linguagem<- function(){
  
  includeMarkdown("contexto_linguagem.md")
}

texto_referencia_tecnica<- function(){
  
  includeMarkdown("referencia_tecnica.md")
}
