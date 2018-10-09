
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
    div(class = "ContratosStyle",
      div(class = "filtrosContratos",
    uiOutput('checkElec'),
    uiOutput('anioSecop'),
    uiOutput('monedaSecop'),
    uiOutput('variableInt'),
    uiOutput('slidMoney')
    ),
    div(class = "grafico",
    highchartOutput('barrasAgregadas')
    ))#,
    #verbatimTextOutput('baseoo')
   
  )
)