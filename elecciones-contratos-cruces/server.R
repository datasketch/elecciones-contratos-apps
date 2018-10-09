financiadores <- read_csv('data/financiadores/contratos_financiadores_data.csv')
dicFinanciadores <- read_csv('data/financiadores/contratos_financiadores_dic.csv')
nombresFin <- financiadores %>% 
                select(NombrPersona = `Nombre de la Persona`, NumerodeIdentificación = `Numero de Identificación`) %>%
                   distinct(`Numero de Identificación`, .keep_all = TRUE)

shinyServer(function(input, output, session){

  output$checkElec <- renderUI({
    radioButtons('elecciones', 'Elija la campaña electoral de interés:',
                 c('Regionales 2015', 'Congreso 2018'), inline = TRUE)
  })
    
  dElec <- reactive({
    elecId <- input$elecciones
    df <- financiadores %>% filter(campaña %in% elecId)
    df
  })
  
  output$anioSecop <- renderUI({
    selectizeInput('secopFecha', 'Elija año de la firma del contrato:',
                   c('Todos', sort(unique(dElec()$`Anno Firma del Contrato`), decreasing = TRUE)))
  })

  dt <- reactive({
    annioC <- input$secopFecha
    
    if (annioC != 'Todos') {
    d <- dElec() %>% filter(`Anno Firma del Contrato` %in% annioC)
    } else {
    d <- dElec()
    }
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
  
  basMon <- reactive({
    monedaId <- input$moneda
    d <- dt() %>% filter(Moneda %in% monedaId)
    d
  })
  
  output$slidMoney <- renderUI({
    
    d <- basMon()
    if (nrow(d) == 0) return()
    minVal <- min(d$`Cuantia Contrato`, na.rm = T)
    maxVal <- max(d$`Cuantia Contrato`, na.rm = T)
    sliderInput('ValueMon', 'Elija rango de la cuantía del contrato', 
                min = minVal, 
                max = maxVal,
                value = c(min(d$`Cuantia Contrato`), max(d$`Cuantia Contrato`)))
  })
  
  
  baseFin <- reactive({
    minMont <- input$ValueMon[1]
    maxMont <- input$ValueMon[2]
    
    df <- basMon() %>% filter(`Cuantia Contrato`>= minMont, `Cuantia Contrato` <= maxMont)
    df
  })
  
  financia <- reactive({
    varId <- input$varInteres
    if (is.null(varId)) return()
    varId <-  gsub(' ', '', varId)
    d <- baseFin()
    names(d) <- gsub(' ', '', names(d))
    
    annioC <- input$secopFecha

    if (annioC != 'Todos') {
    d <- d[!duplicated(d[c("NumerodeIdentificación",varId)]),]
    d <- d %>%
         select_(elg = varId) %>%
            group_by(elg) %>%
             summarise(totalFinanciadores = n())
    } else {
      d <- d[!duplicated(d[c("NumerodeIdentificación",varId,"AnnoFirmadelContrato")]),]
      d <- d %>%
            select_('NumerodeIdentificación', elg = varId, 'AnnoFirmadelContrato') %>%
              group_by( elg, AnnoFirmadelContrato) %>% 
                summarise(totalFinanciadores = n())
    }
    
    d[is.na(d)] <- 'Información no reportada'
    d %>%
      filter(elg != 'No Aplica')
  })
  
  
  output$barrasAgregadas <- renderHighchart({
    
    annioC <- input$secopFecha
    
    varId <- input$varInteres
    if (is.null(varId)) return()
    horLabel <- dicFinanciadores$id_corregido[dicFinanciadores$id == varId]
    title <- dicFinanciadores$titulos[dicFinanciadores$id == varId]
    
    if (annioC != 'Todos') {
    myClickFunc <-  JS("function(event) {Shiny.onInputChange('hcClicked',  {id:event.point.category.name, timestamp: new Date().getTime()});}")
    
    h <- highchart() %>%
      hc_chart(type = "column") %>%
      hc_yAxis(
        title = list(text = 'Número de financiadores con contratos en SECOP'),
        labels = list(
          style = list(
            color = '#000',
            fontSize = '13px'
          )
        )
      ) %>%
      hc_xAxis(
        title = list(text = horLabel),
        labels = list(
          style = list(
            color = '#0E0329',
            fontSize = '13px'
          )
        ),
        type = 'category',
        categories =  map(financia()[['elg']], function(x) {
          as.character(x)
        })
      ) %>%
      hc_series(list(
        data=  map(financia()[['totalFinanciadores']], function(x) {
          as.numeric(x)
        }),
        color= '#A6CEDE',
        allowPointSelect= FALSE,
        cursor = 'pointer',
        dataLabels = list(
          enabled = FALSE,
          fontFamily= 'Open Sans'),
        showInLegend = FALSE,
        events = list(
          click = myClickFunc
        )
      )) %>%
      hc_tooltip(headerFormat = 'Clikea para ver financiadores destacados<br/>',
                 pointFormat = paste0("<b>{point.category}:</b> {point.y}")) 
    } else {
      myClickFunc <-  JS("function(event) {Shiny.onInputChange('hcClicked',  {id:event.point.series.name, annio:event.point.category.name, timestamp: new Date().getTime()});}")
      h <- hgch_line_CatYeaNum(financia(), horLabel = 'Número de financiadores con contratos en SECOP', verLabel = 'Año de la firma del contrato',
                               tooltip = list(headerFormat = 'Clikea para ver financiadores destacados<br/>', 
                                              pointFormat = paste0("<b>Año firma del contrato: </b>{point.category}<br/><b>{series.name}: </b>{point.y}"))) %>% 
        hc_plotOptions(
          series = list(
            cursor = 'pointer',
            events = list(
              click = myClickFunc
            )
          )
        )
    }

    h %>%
      hc_title(text = title) %>% 
      hc_exporting(enabled = TRUE, buttons= list(
        contextButton= list(
          symbol= 'url(https://cdn1.iconfinder.com/data/icons/feather-2/24/download-32.png)',
          height= 30,
          width= 33,
          symbolSize= 24,
          symbolX= 30,
          symbolY= 30,
          menuItems = list('printChart', 'downloadJPEG', 'downloadPNG', 'downloadSVG', 'downloadPDF')
        )
      ))
  })
  
  
  baseTopFin <- reactive({
    annioC <- input$secopFecha
    varId <- input$varInteres
    if (is.null(varId)) return()
    varId <-  gsub(' ', '', varId)
    d <- baseFin()
    names(d) <- gsub(' ', '', names(d))
    
    if (annioC == 'Todos') {
    filtAni <- input$hcClicked$annio
    d <- d %>% filter(AnnoFirmadelContrato %in% filtAni)
    } else {
      d <- d
    }
    
    varFilt <- input$hcClicked$id
    if (is.null(varFilt)) return()
    varFilt <- varFilt
   
      d <- d %>%
        group_by_(varId, 'NumerodeIdentificación') %>%
        dplyr::summarise(`Cuantía total de contratos` = sum(ValorContratoconAdiciones),
                         `Total contratos` = n()) %>%
        arrange(-`Total contratos`)
      d <- d[d[[varId]] == varFilt,]
      d <- d[,-1]
      d %>% inner_join(nombresFin)
  })
  
  output$barraFinanciadores <- renderHighchart({
    df <- baseTopFin() %>% select(NombrPersona, `Total contratos`)
    if (nrow(df) > 10) {
      nF <- 10 
    } else {
      nF <- nrow(df)
    }
    hgch_bar_CatNum(df, sliceN = nF)
  })
  
  observeEvent(input$hcClicked,{
    showModal(
      modalDialog(
        title = 'Financiadores',
        footer = modalButton("Cerrar"),
        easyClose = TRUE,
        highchartOutput('barraFinanciadores')
      )
    )
  })
  
  
  
  
  output$baseoo <- renderPrint({
    baseTopFin()
    #map(basMon(), unique)
  })
  
})