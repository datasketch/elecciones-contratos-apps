financiadores <- read_csv('data/financiadores/contratos_financiadores_data.csv')
dicFinanciadores <- read_csv('data/financiadores/contratos_financiadores_dic.csv')


shinyServer(function(input, output, session){

  output$checkElec <- renderUI({
    radioButtons('elecciones', 'Elija la campaña electoral de interés:',
                 c('Regionales 2015', 'Congreso 2018'), inline = TRUE)
  })
    
  d <- reactive({
    elecId <- input$elecciones
    df <- financiadores %>% filter(campaña %in% elecId)
    df
  })
  
  output$anioSecop <- renderUI({
    selectizeInput('secopFecha', 'Elija año de la firma del contrato:',
                   sort(unique(d()$`Anno Firma del Contrato`), decreasing = TRUE))
  })

  dt <- reactive({
    annioC <- input$secopFecha
    d <- d() %>% filter(`Anno Firma del Contrato` %in% annioC)
    d
  })
  
  output$monedaSecop <- renderUI({
    moneda <- unique(dt()$Moneda)
    radioButtons('moneda', 'Elija la moneda de transacción del contrato:', 
                 moneda, inline = TRUE)
  })
  
  output$variableInt <- renderUI({
    varElg <- dicFinanciadores %>% filter(incluir == 'SI')
    varCho <- setNames(varElg$id, varElg$id_corregido)
    selectizeInput('varInteres', 'Seleccione la variable de interés', varCho)
  })
  
  
  output$slidMoney <- renderUI({
    monedaId <- input$moneda
    anioId <- input$secopFecha
    elecId <- input$elecciones
    d <- dt() %>% filter(Moneda %in% monedaId)
    sliderInput('ValueMon', 'Elija rango de la cuantía del contrato', min(d$`Cuantia Contrato`), max(d$`Cuantia Contrato`), value = c(min(d$`Cuantia Contrato`), max(d$`Cuantia Contrato`)))
  })
  
  
  baseFin <- reactive({
    minMont <- input$ValueMon[1]
    maxMont <- input$ValueMon[2]
    df <- dt() %>% filter(`Cuantia Contrato`>= minMont, `Cuantia Contrato` <= maxMont)
    df
  })
  
  output$baseoo <- renderPrint({
    baseFin()
  })
  
})