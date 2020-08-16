
## Carrega os dados para analise
load('Resultados_por_escola.Rdata')
load('Resultados_por_municipio.Rdata')


## Carrega os pacotes
library(shinydashboard)
library(shiny)

# Funcoes de layout
box <- function(...){
  shinydashboard::box(...)
}


# Bloco de selecao no painel do aluno
panelSelectInput_aluno <- function(buttonWidth) {
    box(width=12,
    fluidRow(   
      column(4,selectInput("disciplina", "Selecione a disciplina:",
                choices = c('Língua Portuguesa','Matemática'))),
      column(4,selectInput("fator", "Selecione o fator:",
                choices = c('Gênero','Raça','Reprovação',
                            'Escolaridade da mãe','Escolaridade do pai',
                            'Incentivo dos pais'))))
    )
    
}


# Bloco de selecao no painel da escola
panelSelectInput_escola <- function(buttonWidth) {
  box(width=12,
      fluidRow(   
        column(4,selectInput("disciplina_escola", "Selecione a disciplina:",
                             choices = c('Língua Portuguesa','Matemática'))),
        column(4,selectInput("fator_escola", "Selecione o fator:",
                             choices = c('Nível Sócioeconômico','Regularidade do Corpo docente',
                                         'Distorção Idade-série'))))
  )
  
}

# Bloco de selecao no painel do municipio
panelSelectInput_municipio <- function(buttonWidth) {
  box(width=12,
      fluidRow(   
        column(4,selectInput("disciplina_municipio", "Selecione a disciplina:",
                             choices = c('Língua Portuguesa','Matemática'))),
        column(4,selectInput("fator_municipio", "Selecione o tipo de analise:",
                             choices = c('Salário vs Proficiência',
                                         'Salário vs Carga-Horária'))),
        column(4,checkboxInput("escolaridade", "Separar por escolaridade:", FALSE))
        )
  )
  
}

# Bloco de selecao no painel dos mapas
panelSelectInput_mapa<- function(buttonWidth) {
  box(width=12,
      fluidRow(   
        column(4,selectInput("disciplina_mapa", "Selecione a disciplina:",
                             choices = c('Língua Portuguesa','Matemática'))))
  )
  
}









# Parametros layout
buttonWidth <- 220



## INICIO DO SHINY SERVER

function(input, output, session) {
 
  logged <- reactiveValues(logged = FALSE, user = NULL)
  # Para criar a condicao para tela inicial
  observeEvent(input$signin, {
      logged$logged <- TRUE
      
  })
  
 
  output$ui <- renderUI({
    
    
    if(logged$logged == FALSE) {
     
    #################################
    #  Painel de Entrada
    ###################################   
    return(
    
      
     shiny::div(style = "width: 600px; max-width: 100%; margin: 0 auto; padding: 20px;",
                   shiny::wellPanel(
                     
        tagList(
          plotOutput("logo",height = "200px"),
          h4(strong("Fatores que impactam na aprendizagem do aluno")),
          HTML("Seja bem-vindo(a)! <br/>  <br/>
               Neste portal, você encontrará informações relevantes
               para a gestão do sistema escolar.
               A partir dos resultados aqui apresentados, você gestor, será capaz
               de verificar os fatores que mais influenciaram o desempenho dos
               alunos do quinto ano na Prova Brasil no ano de 2017.<br/><br/>"),
                h5("Todas as análises foram realizadas na linguagem
                R e os códigos estão todos disponíveis no", 
               a("Github", href="http://site.primeiraescolha.com.br/blog-educacao")),
               HTML(
               "<br/><br/>
               Para acessar os resultados clique em \"Acessar Portal\".
               <br/><br/>"),
             actionButton("signin", "Acessar o Portal"),
          tags$head(tags$script(src = "message-handler.js"))
          
        ))
      ))
    }
    
    #################################
    #  Painel com os resultados
    ###################################    
    
      else if(logged$logged == TRUE)
              {
      return(
        
        dashboardPage(
          
          skin = "blue",
          dashboardHeader(disable = TRUE),
          sidebar=dashboardSidebar(
            disable = TRUE
          ),
          
          body = dashboardBody(
            shinyjs::useShinyjs(),

            
            tags$head(
              
              tags$link(
                rel = "stylesheet",
                type = "text/css",
                href = "styleDefinitions.css"
              )),
            
            fluidRow(
              ## Cria cabecalho
              
              column(
                width = 8,height = "40px",
                wellPanel(height = "40px",h4(
                  strong("Análises Resultados Prova Brasil - Quinto Ano 2017"),
                  style = "text-align: center"
                )
                # h5("Análises Exploratótias", style = "text-align: center")
                )),
              box(width=3,column(
                width = 2,
                tags$a(
                  href = "http://www2.educacao.mg.gov.br/", 
                  tags$img(src = 'logo.png', title = "SEE/MG", height = "45px",width='150px', style = "margin-top: 10px; margin-left: 20px")
                ))
              )),
            div(class = "span", tabsetPanel(
              id = "Reiter",
              tabPanel(
                HTML("Descrição das  <br/>  Análises"), value = "intro",
                fluidRow(
                  tabBox(
                    width=12,
                    # The id lets us use input$tabset1 on the server to find the current tab
                    id = "tabset1", height = "800px",
                    tabPanel(width=5,"Conjunto de dados", texto_intro(),plotOutput("plot3",height = "200px")),
                    tabPanel("Contexto e linguagem",texto_referencia_tecnica(),plotOutput("rshiny",height = "200px")),
                    tabPanel(HTML("Referência técnica<br/>  sobre os indicadores"), texto_referencia_tecnica())
                   
                    
                  ),
                  
                )
              ),
              tabPanel(id='nivel_aluno',
                       HTML("Análise no nível <br/>  do aluno"), value = "tab1",
                       panelSelectInput_aluno(buttonWidth),
                     
                       #fluidRow(numeroAlunos()),
                       
                       fluidRow(
                         column(6,offset=1,plotOutput("Proficiencia_genero_MT",height = "200px")),
                         column(4,offset=1,box(width=12,textOutput('interpretacao')))
                       )
              ),
             
              tabPanel(
                HTML("Análise no nível <br/>  da escola"), value = "tab2",
                panelSelectInput_escola(buttonWidth),
                fluidRow(
                  column(6,offset=1,box(width=12,plotlyOutput("grafico_dispersao_escola",height = "400px"))),
                  column(4,offset=1,box(width=12,textOutput('interpretacao_escola')))
                )
                
                
              ),
              tabPanel(
                HTML("Análise no nível <br/>  do município"), value = "tab3",
                panelSelectInput_municipio(buttonWidth),    
                fluidRow(
                  column(7,offset=1,box(width=12,plotlyOutput("grafico_municipio",height = "400px"))),
                  column(3,offset=1,box(width=12,textOutput('interpretacao_municipio')))
                ),
                hr(),
                print("Observação: os dados sobre salário médio dos professores são referentes ao ano de 2014.")
                     
                ),
              tabPanel(id='mapa',
                       HTML("Mapa de proficiências"), value = "tab4",
                       panelSelectInput_mapa(buttonWidth),
                       
                       #fluidRow(numeroAlunos()),
                       
                       fluidRow(
                         column(5,offset=1,box(width=12,height=350,plotOutput("mapa",height = "200px"))),
                         column(4,offset=1,box(width=12,textOutput('interpretacao_mapa')))
                       )
              )
            ))
          )
        )
        
      )
      }
     ## Fecha funcao
     else{}
  })
  
  ##############################################################
  # Funcoes para layout dinamico
  ##############################################################
  
  ######################
  ## Mensagem de boas vindas
  #######################
  
  observeEvent(input$signin, {

            session$sendCustomMessage(type = 'testmessage',
                              message = 'Seja bem vindo!')
  })

  
  
  #############################
  ## Imagem pagina inicial
  #############################
  
  output$logo <- renderImage({
    # When input$n is 1, filename is ./images/image1.jpeg
    filename <- 'logo.png'
    
    # Return a list containing the filename
    list(src = filename)
  }, deleteFile = FALSE) 
  
  ###############
  ## Imagem para o texto explicativo
  ###############
  
  output$plot3 <- renderImage({
    # When input$n is 1, filename is ./images/image1.jpeg
    filename <- 'provabrasil.jpeg'
    
    # Return a list containing the filename
    list(src = filename)
  }, deleteFile = FALSE)
  
  output$rshiny <- renderImage({
    # When input$n is 1, filename is ./images/image1.jpeg
    filename <- 'rshiny.jpeg'
    
    # Return a list containing the filename
    list(src = filename)
  }, deleteFile = FALSE)
  
  
  
  
  #####################################
  ## Funcoes com os graficos
  ##################################### 
  
 # Grafico por escola
  output$grafico_dispersao_escola <- renderPlotly({
    
    if(input$disciplina_escola=='Matemática' & input$fator_escola=='Nível Sócioeconômico'){
    p<-ggplot(estatisticas_aluno_escola[!is.na(NIVEL_SOCIO_ECONOMICO) & NIVEL_SOCIO_ECONOMICO!='Grupo 6'],
              aes(x=NIVEL_SOCIO_ECONOMICO,y=media_matematica,
                  fill=NIVEL_SOCIO_ECONOMICO))+
      geom_boxplot()+theme_minimal()+ylab('Proficiência em Matemática')+
      xlab('Nível sócioeconômico da escola')+theme(legend.position = "none")
    
    }
    
    if(input$disciplina_escola=='Língua Portuguesa' & input$fator_escola=='Nível Sócioeconômico'){
      p<-ggplot(estatisticas_aluno_escola[!is.na(NIVEL_SOCIO_ECONOMICO) & NIVEL_SOCIO_ECONOMICO!='Grupo 6'],
                aes(x=NIVEL_SOCIO_ECONOMICO,y=media_portugues,
                    fill=NIVEL_SOCIO_ECONOMICO))+
        geom_boxplot()+theme_minimal()+ylab('Proficiência em Língua Portuguesa')+
        xlab('Nível sócioeconômico da escola')+theme(legend.position = "none")
      
      
    }
    
    
    
    if(input$disciplina_escola=='Matemática' & input$fator_escola=='Regularidade do Corpo docente'){
      
      p<-ggplot(estatisticas_aluno_escola,aes(x=IRD,y=media_matematica))+geom_point()+
        theme_minimal()+xlab('Indicadore de Regularidade do Corpo Docente')+
        ylab('Proficiencia em Matemática')
      
    }
    
    if(input$disciplina_escola=='Língua Portuguesa' & input$fator_escola=='Regularidade do Corpo docente'){
      p<-ggplot(estatisticas_aluno_escola,aes(x=IRD,y=media_portugues))+geom_point()+
        theme_minimal()+xlab('Indicadore de Regularidade do Corpo Docente')+
        ylab('Proficiencia em Língua Portuguesa')
      
      
    }
    
    
    if(input$disciplina_escola=='Matemática' & input$fator_escola=='Distorção Idade-série'){
      p<-ggplot(estatisticas_aluno_escola,aes(x=log(Distorcao_idade_serie),y=media_matematica))+
        geom_point()+theme_minimal()+xlab('Distorção Idade Série (escala log)')+
        ylab('Proficiencia em Matemática')
      
    }
    
    if(input$disciplina_escola=='Língua Portuguesa' & input$fator_escola=='Distorção Idade-série'){
      p<-ggplot(estatisticas_aluno_escola,aes(x=log(Distorcao_idade_serie),y=media_portugues))+
        geom_point()+theme_minimal()+xlab('Distorção Idade Série (escala log)')+
        ylab('Proficiencia em Língua Portuguesa')
      
      
    }
    
    
    ggplotly(p)
    
    
    
    
  })
  
  # Grafico por escola
  output$grafico_municipio <- renderPlotly({
    
    
    if( input$disciplina_municipio == 'Matemática' & input$fator_municipio=='Salário vs Proficiência')
      {
     if(input$escolaridade==FALSE){
      p<-ggplot(salario_professores[Escolaridade=='Total',],
              aes(x=SalarioMedio,y=media_matematica,text = paste(Nome_Municipio,'\n',
                                                                 'Salario médio:',SalarioMedio,'\n',
                                                                 'Nota média:',round(media_matematica,2))
              ))+geom_point(col='lightblue')+theme_minimal()+xlab('Salário Médio')+ylab('Proficiência em Matemática')
    
     }
      else if(input$escolaridade==TRUE){
        p<-ggplot(salario_professores[Escolaridade!='Total',],
                  aes(x=SalarioMedio,y=media_matematica,col=Escolaridade,text = paste(Nome_Municipio,'\n',
                                                                     'Salario médio:',SalarioMedio,'\n',
                                                                     'Nota média:',round(media_matematica,2))
                  ))+geom_point()+theme_minimal()+xlab('Salário Médio')+ylab('Proficiência em Matemática')
        
      }
    }
    
    if( input$disciplina_municipio == 'Língua Portuguesa' & input$fator_municipio=='Salário vs Proficiência'){
      
      if(input$escolaridade==FALSE){
        p<-ggplot(salario_professores[Escolaridade=='Total',],
                aes(x=SalarioMedio,y=media_portugues,text = paste(Nome_Municipio,'\n',
                                                                   'Salario médio:',SalarioMedio,'\n',
                                                                   'Nota média:',round(media_portugues,2))
                ))+geom_point(col='lightblue')+theme_minimal()+xlab('Salário Médio')+ylab('Proficiência em Língua Portuguesa')
      }
      else if(input$escolaridade==TRUE){
        p<-ggplot(salario_professores[Escolaridade!='Total',],
                  aes(x=SalarioMedio,y=media_portugues,col=Escolaridade,text = paste(Nome_Municipio,'\n',
                                                                    'Salario médio:',SalarioMedio,'\n',
                                                                    'Nota média:',round(media_portugues,2))
                  ))+geom_point()+theme_minimal()+xlab('Salário Médio')+ylab('Proficiência em Língua Portuguesa')
        
        
        }
    }
    
    if( input$fator_municipio == 'Salário vs Carga-Horária'){
      p<-ggplot(salario_professores[Escolaridade=='Total',],
                aes(x=CargaHorariaMedia,y=SalarioMedio,text = paste(Nome_Municipio,'\n',
                                                                    'Salario médio:',SalarioMedio,'\n',
                                                                    'Carga horária média:',round(CargaHorariaMedia,2))
                ))+geom_point(col='lightblue')+theme_minimal()+xlab('Carga Horária Média')+ylab('Salário Médio')
    }
    
    if( input$fator_municipio == 'Salário vs Carga-Horária' & input$escolaridade==TRUE){
      p<-ggplot(salario_professores[Escolaridade!='Total',],
                aes(x=CargaHorariaMedia,y=SalarioMedio,col=Escolaridade,text = paste(Nome_Municipio,'\n',
                                                                    'Salario médio:',SalarioMedio,'\n',
                                                                    'Carga horária média:',round(CargaHorariaMedia,2))
                ))+geom_point()+theme_minimal()+xlab('Carga Horária Média')+ylab('Salário Médio')
    }
    
    
    
    
    ggplotly(p, tooltip=c("text"))%>% layout(hoverlabel=list(bgcolor="white"))
    })
  
  
  ## Graficos dos alunos
  
  output$Proficiencia_genero_MT <- renderImage({
    # When input$n is 1, filename is ./images/image1.jpeg
    if(input$disciplina=='Matemática' & input$fator=='Gênero'){
    filename <- 'Graficos/Proficiencia_genero_MT.png'
    }
    
    if(input$disciplina=='Língua Portuguesa'& input$fator=='Gênero')
    {
      filename <- 'Graficos/Proficiencia_genero_LP.png'
    }
    
    
    if(input$disciplina=='Matemática' & input$fator=='Raça'){
      filename <- 'Graficos/Proficiencia_raca_MT.png'
    }
    
    if(input$disciplina=='Língua Portuguesa'& input$fator=='Raça')
    {
      filename <- 'Graficos/Proficiencia_raca_LP.png'
    }
    
    
    
    if(input$disciplina=='Matemática' & input$fator=='Reprovação'){
      filename <- 'Graficos/Proficiencia_reprovacao_MT.png'
    }
    
    if(input$disciplina=='Língua Portuguesa'& input$fator=='Reprovação')
    {
      filename <- 'Graficos/Proficiencia_reprovacao_LP.png'
    }
    
    
    if(input$disciplina=='Matemática' & input$fator=='Escolaridade da mãe'){
      filename <- 'Graficos/Proficiencia_escolaridadeMae_MT.png'
    }
    
    if(input$disciplina=='Língua Portuguesa'& input$fator=='Escolaridade da mãe')
    {
      filename <- 'Graficos/Proficiencia_escolaridadeMae_LP.png'
    }
    
    
    if(input$disciplina=='Matemática' & input$fator=='Escolaridade do pai'){
      filename <- 'Graficos/Proficiencia_escolaridadePai_MT.png'
    }
    
    if(input$disciplina=='Língua Portuguesa'& input$fator=='Escolaridade do pai')
    {
      filename <- 'Graficos/Proficiencia_escolaridadePai_LP.png'
    }
    
    
    if(input$disciplina=='Matemática' & input$fator=='Incentivo dos pais'){
      filename <- 'Graficos/Proficiencia_incentivoPais_MT.png'
    }
    
    if(input$disciplina=='Língua Portuguesa'& input$fator=='Incentivo dos pais')
    {
      filename <- 'Graficos/Proficiencia_incentivoPais_LP.png'
    }

    # Return a list containing the filename
    list(src = filename)
  }, deleteFile = FALSE) 
  
  
  # Mapa
  
  output$mapa <- renderImage({
   if(input$disciplina_mapa == 'Matemática'){
      filename <- 'Graficos/mapa_proficiencia_matematica.png'
   }
    else if (input$disciplina_mapa == 'Língua Portuguesa'){
      filename <- 'Graficos/mapa_proficiencia_portugues.png'
      
    }
    
    # Return a list containing the filename
    list(src = filename)
  }, deleteFile = FALSE) 
  

  #####################################
  ## Funcoes com textos interpretativos
  #####################################
  
  
  output$interpretacao  <- renderText({
    
    if(input$fator=='Gênero'){
      return("Para Matemática, as proficiências dos meninos e meninas são praticamente iguais.
    Em Língua Portuguesa, as proficiências das meninas são ligeiramente maiores. ")}
    
    if(input$fator=='Raça'){
      return("Em ambas disciplinas as crianças brancas aprentam os melhores desempenho, seguidas
             das pardas e por último as pretas.")}
    
    if(input$fator=='Reprovação'){
      return("As crianças que foram reprovadas pelo menos uma vez apresentam desempenho 
             pior. ")}
    
    
    if(input$fator=='Escolaridade da mãe' | input$fator=='Escolaridade do pai'){
      return("Quanto maior o nível de escolaridade do pai ou da mãe, maior o desempenho
             das crianças.")}
    
    if(input$fator=='Incentivo dos pais'){
      return("As crianças cujos pais incentivam os estudos, apresentam desempenho melhor.")}
  })
  
  output$interpretacao_escola  <- renderText({
    
    if(input$fator_escola=='Nível Sócioeconômico'){
      return("Para ambas disciplinas, quanto maior o Nível Sócioeconômico
             dos seus alunos, maior sua proficiência. ")}
    
    if(input$fator_escola=='Regularidade do Corpo docente'){
      return("Existe uma leve tendência positiva na relação entre o desempenho
             dos alunos e o indicador de Regularidade do Corpo Docente. Ela é um pouco mais 
             evidente para Matemática.")}
    
    if(input$fator_escola=='Distorção Idade-série'){
      return("Existe uma relação negativa entre o desempenho dos alunos
             e o indicador de Distorção Idade-série. Quanto maior essa distorção, menor
             o desempenho dos alunos.")}
    
    
  })
  
  
  output$interpretacao_municipio  <- renderText({
    
    if(input$fator_municipio=='Salário vs Proficiência'){
      if(input$escolaridade==FALSE){
      return("Existe uma associação postiva fraca entre o salário médio 
             dos professores no município a proficiência média dos alunos. 
             Ao passar o cursor sobre os pontos é possível identificar o nome do município.
             Um caso bem extremo é o do município de Serranópolis de Minas, que apresenta um salário
             médio alto, contrastando com uma proficiência bem baixa. E o município de Arapuá,
             que apresenta um salário médio baixo, mas uma proficiência alta se comparada com os demais.")}
    
    else if(input$escolaridade==TRUE){
      return("Ao incluirmos  a variável escolaridade do professor, nota-se que
             os professores com ensino superior tem o salário ligeiramente mais alto.
             Além disso, a relação entre o salário e a proficência segue o mesmo padrão
             em ambos os grupos. ")
     }
    }
    
    if(input$fator_municipio=='Salário vs Carga-Horária'){
      if(input$escolaridade==FALSE){
        return('O salário médio dos professores tende a ser mais alta nos municípios
               onde a carga horária média é menor. Possivelmente nos locais onde o salário é menor
               os professores acabam tendo a necessidade de trabalhar por mais tempo.')
      }
      
      else if(input$escolaridade==TRUE){
        return('A relação observada entre o salário médio e carga horário dos professores 
               não parece ser afetada por sua escolaridade.')
      }
      }
    
    
  })
  
  output$interpretacao_mapa <- renderText({
    return('O mapa ao lado mostra os municípios com cores que variam de 
           acordo com a proficiência média de seus alunos. Observa-se que existe um padrão 
           espacial nessa variável. Os municípios mais ao norte apresentam um desempenho
           pior que os municípios mais ao sul. Isso ocorre tanto para Língua Portuguesa quanto
           para Matemática.')
  })
  
}

