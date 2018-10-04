
shinyUI(
  fluidPage(
    useShinyjs(),
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     tags$img(src = 'Cargando.gif', class="loadmessage")),
    tags$head(
      tags$link(rel="stylesheet", type="text/css", href="style.css"),
      includeScript("js/iframeSizer.contentWindow.min.js"),
      includeScript("js/clara.js")
    ),
    tabsetPanel(
      tabPanel("CRUCES",
               p("La información de estos cruces corresponde únicamente a los datos de financiadores de campañas que tienen contratos registrados en SECOP."),
               div(class = "twoCols",
                   div(
                     uiOutput('varsCruces'),
                     uiOutput('varsCruces2'),
                     uiOutput('vizCrucesOpts')
                     ),
                   div(
                     highchartOutput('vizCruces', width = 650, height = 500),
                     br()
                   )
               )
      ),
      tabPanel("CANDIDATOS",
               p("En esta sección puede ver el top 10, 20, 30 y 50 de candidatos 
                 que han tenido más número de financiadores, más financiación, financiadores con más contratos, entre otras."),
               div(class = "twoCols",
                   div(
                     uiOutput('varCand'),
                     uiOutput('selCampCand'),
                     uiOutput('selVarCand')),
                   div(
                     uiOutput('typeVizCand'),
                     highchartOutput('vizCand', width = 650, height = 500)))
      ),
      tabPanel("DEPARTAMENTO",
               p("En esta sección puede observar el total de candidatos o financiadores y total o promedio de gastos o aportes en campaña por departamento."),
               div(class = "twoCols",
                   div(
                     uiOutput('deptoGeneral'),
                     uiOutput('selCampaDepto'),
                     uiOutput('selvarDepto')),
                   div(
                     uiOutput('selcViz'),
                     highchartOutput('vizDepto', width = 650, height = 500))
               ))
    )
  )
)