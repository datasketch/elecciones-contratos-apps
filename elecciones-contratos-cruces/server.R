cruces <- read_csv("data/cruces_data.csv")
dic <- read_csv("data/cruces_dic.csv")



shinyServer(function(input, output, session){
  
# Cruces ---------------
  output$varsCruces <- renderUI({
    catVars <- dic %>% filter(ctype == "Cat") %>% pull(id)
    names(catVars) <- dic %>% filter(ctype == "Cat") %>% pull(label)
    numVars <- dic %>% filter(ctype == "Num") %>% pull(id)
    names(numVars) <- dic %>% filter(ctype == "Num") %>% pull(label)
    list(
      selectizeInput("selectedCatVar","Seleccione primera variable", choices = catVars),
      selectizeInput("selectedNumVar","Seleccione segunda variable",choices = numVars)
    )
  })
  output$varsCruces2 <- renderUI({
    catVars <- dic %>% filter(ctype == "Cat") %>% pull(id)
    selectedCatVar <- input$selectedCatVar
    grpVars <- c("Ninguna",catVars[catVars != selectedCatVar])
    names(grpVars) <- c("Ninguna",dic %>% filter(id %in% grpVars) %>% pull(label))
    list(
      selectizeInput("selectedGrpVar","Agrupar por:", choices = grpVars,
                     selected = 'Ninguna')
    )
  })
  output$vizCrucesOpts <- renderUI({
    list(
      radioButtons("vizCrucesOptsOrientation", "Orientación",
                   c("Horizontal" = "hor", "Vertical" = "ver"))
    )
  })
  output$vizCruces <- renderHighchart({
    selectedCatVar <- input$selectedCatVar
    selectedNumVar <- input$selectedNumVar
    if(any(is.null(c(selectedCatVar,selectedNumVar)))) return()
    
    if(input$selectedGrpVar == "Ninguna"){
      df <- cruces %>% select(one_of(c(selectedCatVar, selectedNumVar)))
      horLabel <- ifelse(input$vizCrucesOptsOrientation != "Horizontal",
                         selectedNumVar, selectedCatVar)
      h <- hgch_bar_CatNum(df, orientation = input$vizCrucesOptsOrientation,
                           horLabel = "", verLabel = "", export = TRUE)
    }else{
      selectedGrpVar <- input$selectedGrpVar
      df <- cruces %>% select(one_of(c(selectedGrpVar, selectedCatVar, selectedNumVar)))
      h <- hgch_bar_grouped_CatCatNum(df, orientation = input$vizCrucesOptsOrientation,
                                      horLabel = "", verLabel = "", export = TRUE)
    }
    h
  })


  
# Candidatos --------------------------------------------------------------

  output$varCand <- renderUI({
    s1 <- c('Los 10 más', 'Los 20 más', 'Los 30 más', 'Los 50 más')
    selectizeInput('varSlice', 'Top candidatos', s1)
  })
 
  output$selCampCand <- renderUI({
    selectizeInput('varCpnCdt', 'Seleccione campaña', c('Todas', 'Regionales 2015', 'Congreso 2018'))
  })  
  
  output$selVarCand <- renderUI({
    dic <- read_csv('data/candidatos_explora_dic.csv')
    dic <- dic[c(-1,-2, -8),]
    zi <- as.list(setNames(dic$id, dic$label))
    selectizeInput('vairInt', 'Seleccione variable de interés', zi)
  })
  
  dtaCts <- reactive({
    dta <- read_csv('data/candidatos_explora_data.csv')
    cmp <- input$varCpnCdt
    
    if (cmp == 'Todas') {
      dCand <- dta %>% select_('campaña', 'Candidato', input$vairInt)
    } else {
      dCand <- dta %>% filter(campaña %in% cmp)  %>% select_('Candidato', input$vairInt)
    }
    
    dtOrd <- dCand %>% arrange(desc(!!sym(input$vairInt)))
    rowsSlice <- numextract(input$varSlice)
    dtOrd %>% slice(1:rowsSlice)
    
  })
  
  output$typeVizCand <- renderUI({
    dta <- dtaCts()
    if (ncol(dta) == 3) {
      tyViz <- c('Barras', 'Stack', 'Líneas')
    } else {
      tyViz <- c('Barras', 'Pie', 'Dona')
    }
    selectizeInput('vizTypeCts', 'Seleccione visualización', tyViz)
  })
  
  output$vizCand <- renderHighchart({
    
    df <- dtaCts()
    tyviz <- input$vizTypeCts
    if (ncol(df) == 3) {
      if (tyviz == 'Barras') {
        h <- hgch_bar_grouped_CatCatNum(df, export = TRUE) }
      if (tyviz == 'Stack') {
        h <- hgch_bar_stacked_100_CatCatNum(df, export = TRUE) }
      if (tyviz == 'Líneas') {
        h <- hgch_line_CatOcaNum(df, export = TRUE)
      }
    } else {
      if (tyviz == 'Barras') {
        h <- hgch_bar_CatNum(df, export = TRUE) }
      if (tyviz == 'Pie') {
        h <- hgch_pie_CatNum(df, export = TRUE) }
      if (tyviz == 'Dona') {
        h <- hgch_donut_CatNum(df, export = TRUE)
      }
    }
    
    h
    
  })
  
  
  output$babla <- renderPrint({
    dtaCts()
  })

# Departamentos -----------------------------------------------------------

  
  output$deptoGeneral <- renderUI({
    radioButtons('depto', '', c('Departamento Candidato', 'Departamento Financiador'))
  })
  
  dicDepto <- reactive({
    idDepto <- input$depto
    if (idDepto == 'Departamento Candidato') {
      dic <- read_csv('data/departamentos_explora_dic.csv')
    } else {
      dic <- read_csv('data/departamentos_ingreso_dic.csv')
    }
    dic[c(-1, -2),]
  })
  
  output$selCampaDepto <- renderUI({
    selectizeInput('varCpn', 'Seleccione campaña', c('Todas', 'Regionales 2015', 'Congreso 2018'))
  })  
  
  output$selvarDepto <- renderUI({
    varsNu2 <- dicDepto()
    dtSelc <- as.list(setNames(varsNu2$id, varsNu2$label))
    selectizeInput('varViz', 'Variable de interés', dtSelc)
  })
  
  
  dataDepto <- reactive({
    idDepto <- input$depto
    if (idDepto == 'Departamento Candidato') {
      dta <- read_csv('data/departamentos_explora_data.csv')
    } else {
      dta <- read_csv('data/departamentos_ingreso_explora_data.csv')
    }
    
    dumFil <- input$varCpn
    if (dumFil == 'Todas') {
      d <- dta %>% select_('Departamento', 'campaña', input$varViz)
    } else { 
      d <- dta %>% filter(campaña %in% dumFil) %>% select_('Departamento', input$varViz)
    }
    
    d
  })
  

  output$selcViz <- renderUI({
    dta <- dataDepto()
    if (ncol(dta) == 3) {
      tyViz <- c('Barras', 'Stack', 'Líneas')
    } else {
      tyViz <- c('Barras', 'Pie', 'Dona')
    }
    selectizeInput('vizType', 'Seleccione visualización', tyViz)
  })
    
  output$vizDepto <- renderHighchart({

    df <- dataDepto()
    tyviz <- input$vizType
    if (ncol(df) == 3) {
      if (tyviz == 'Barras') {
        h <- hgch_bar_grouped_CatCatNum(df, export = TRUE) }
      if (tyviz == 'Stack') {
        h <- hgch_bar_stacked_100_CatCatNum(df, export = TRUE) }
      if (tyviz == 'Líneas') {
        h <- hgch_line_CatOcaNum(df, export = TRUE)
      }
    } else {
      if (tyviz == 'Barras') {
        h <- hgch_bar_CatNum(df, export = TRUE) }
      if (tyviz == 'Pie') {
        h <- hgch_pie_CatNum(df, export = TRUE) }
      if (tyviz == 'Dona') {
        h <- hgch_donut_CatNum(df, export = TRUE)
      }
    }
    
    h

  })
  

# Origen-Destino ----------------------------------------------------------


  
  
  
})