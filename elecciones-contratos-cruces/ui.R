
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
    HTML('<p style="font-size:15px;margin-top:3%;margin-bottom:3%;">En está sección podra conocer estadísticas generales de los financiadores que tienen o han tenido contratos con el estado.</p>'),
    div(class = "ContratosStyle",
      div(class = "filtrosContratos",
    uiOutput('checkElec'),
    uiOutput('anioSecop'),
    uiOutput('variableInt'),
    uiOutput('monedaSecop'),
    uiOutput('slidMoney')
    ),
    div(class = "grafico",
    highchartOutput('barrasAgregadas')
    ))#,
    #verbatimTextOutput('baseoo')
   
  )
)