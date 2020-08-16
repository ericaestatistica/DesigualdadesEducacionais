library(shinydashboard)
library(shiny)

# Ele nao carrega os pacotes com enconding, precisa colocar no global

# nao suar libray no shiny

#source('uiFunctions.r')

#' UI Elements
#'
#' @param buttonWidth button width
#' @param sideBarWidth (numeric) width of sidebar
#' @param ... UI elements for the box
#'
#' @export
#' @rdname uiElements
box <- function(...){
  shinydashboard::box(...)
}


#' @export
#' @rdname uiElements
panelTitle <- function(sideBarWidth) {
  dashboardHeader(
    title = "Resultados por Turma",
    titleWidth = sideBarWidth
  )
}

#' @export
#' @rdname uiElements
#' 

panelSelectInput <- function(buttonWidth) {
  wellPanel(
    selectInput("ano", "Selecione o Ano",
                choices = unique(substr(dados_filtrados$Ano ,1,4))),
    selectInput("semestre", "Selecione o Semestre",
                choices = c("Primeiro"='1'),
                selected = "Primeiro"),
    selectInput("serie", "Selecione a Serie",
                choices = unique(dados_filtrados$SÉRIE)[1:(length(unique(dados_filtrados$SÉRIE))-1)]
                ),
    uiOutput("moreControls"),
    
    #conditionalPanel(
    #  condition = "input.inputType == 'Example Dataset'"
    #)
    
    style = "color:black"
  )
 }




### Shop Level Analytics Elements ###

#' @export
#' @rdname uiElements
numeroAlunos <- function() {
  tagList(
    h4("Número de alunos:"),
    box(
      width =  12,
      infoBoxOutput("numeroalunos", width = 3),
      infoBoxOutput("percentual_nivel1", width = 3),
      infoBoxOutput("percentual_nivel2", width = 3),
      infoBoxOutput("percentual_nivel3", width = 3)
    )
  )
}



#' @export
#' @rdname uiElements
grafico_densidade <- function() {
  column(width = 10,
         h4("Comparativo Distribuições"),
         wellPanel(
           box(
             width = 10, height = "80px",
             selectInput("area_densiade", "Selecione a Área:", choices =unique(dados_filtrados$Área))
           ),
           style = "color:black"
         ),
         box(width = 10, plotlyOutput("grafico_densidade"))
  )
  
}

#' @export
#' @rdname uiElements
graficosBarrasPorcentagens <- function() {
  column(
    width = 12,
    h4("Resultados comparativos"),
    
 
    box(width = 7,plotlyOutput("grafico3",height = 200),
    )
  )
  
  
}


graficosBarrasMedias <- function() {
  column(
    width = 12,
    h4("Resultados por Área"),
    
    box(width = 5,  plotlyOutput("grafico1",height = 200)
    ),
    box(width = 5,plotlyOutput("grafico2",height = 200)
    ),
    
    )
  
}


### Individual Level Analysis Elements ###
#' @export
#' @rdname uiElements
selecaoarea<- function() {
  wellPanel(
    box(
      width = 12,
     selectInput("area", "Selecione a Área:", choices =unique(dados_filtrados$Área))
      
    ),
    
    
    style = "color:black"
  )
}

selecaoarea2<- function() {
  wellPanel(
    box(
      width = 12,  
      selectInput("area2", "Selecione a Área:", choices =unique(dados_filtrados$Área))
    ),
    style = "color:black"
  )
}


text<-'teste'

buttonWidth <- 220
sideBarWidth <- 200

fluidPage(
  
  # Application title
  uiOutput("ui")
  
  # Sidebar with a slider input for number of bins 
)
