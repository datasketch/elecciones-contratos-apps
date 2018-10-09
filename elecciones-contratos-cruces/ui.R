
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
    uiOutput('checkElec'),
    uiOutput('anioSecop'),
    uiOutput('monedaSecop'),
    uiOutput('variableInt'),
    uiOutput('slidMoney'),
    verbatimTextOutput('baseoo')
   
  )
)