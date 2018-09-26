
nodes <- read_csv("data/clean/nodes.csv", col_types = cols(.default = "c")) %>% 
  mutate(uid = as.numeric(uid),
         db_secop = ifelse(db_secop == "No", "No","Si"))
nodes$db_registraduria_mesa <- NULL

edges <- read_csv("data/clean/edges.csv", col_types = cols(.default = "c")) %>% 
  mutate(cand_iden = as.numeric(cand_iden), fin_iden = as.numeric(fin_iden),
         fin_valor = as.numeric(fin_valor), fin_n = as.numeric(fin_n)) %>% 
  select(from = cand_iden, to = fin_iden, everything())

contratos <- read_csv("data/original/secop_data.csv")
contratos_dic <- read_csv("data/original/secop_dic.csv")

dics <- read_csv("data/clean/nodes_dic.csv")

initCands <- c("ALVARO URIBE VELEZ", "ARMANDO ALBERTO BENEDETTI VILLANEDA",
               "AURELIJUS RUTENIS ANTANAS MOCKUS SIVICKAS",
               "ENRIQUE PEÑALOSA LONDOÑO", "FEDERICO ANDRES GUTIERREZ ZULUAGA",
               "NORMAN MAURICE ARMITAGE CADAVID",
               "CESAR OMAR ROJAS AYALA",
               "ALEJANDRO CHAR CHALJUB")

shinyServer(function(input, output, session){
  
  node_id <- reactive({
    node_id <- as.numeric(parseQueryString(session$clientData$url_search)$node_id)
    if(length(node_id)==0){
      uids <- nodes %>% filter(name %in% initCands) %>% pull(uid)
      node_id <- sample(uids,1)
    }
    if(!is.null(input$clicked_cand$id))
      return(input$clicked_cand$id)
    node_id
  })
  
  output$debug <- renderPrint({
    #fin_con()
    #input$clicked_cand
    #node_id()
    nclickid <- "???"
    if(!is.null(input$clicked_cand$id))
        nclickid <- input$clicked_cand$id
    paste("node_id",node_id() %||% "??", #nodes$name[id==node_id()],
          " selectedFin",selectedFin() %||% "??", #nodes$name[id==selectedFin()]
          " Net Click Id",nclickid#, nodes$name[id==nclickid]
          )
  })
  
  node_data <- reactive({
    s <- getFinNetwork(node_id(), nodes, edges)
    s
  })
  
  selectedFin <- reactive({
    if(is.null(input$network_click)) return()
    #input$clicked_cand
    #input$clicked_cand$id
    node_id() # Add dependency for the rective to be updated when node_id changes
    input$network_click
  })
  
  output$network <- renderVisNetwork({
    selectedValue <- "cont_n_contratista"
    s <- node_data()
    networkChart(s$nodesFin, s$edgesFin, selectedValue, dics = dics) %>%
      visEvents(click = "function(nodes){
                Shiny.onInputChange('network_click', nodes.nodes[0]);
                ;}"
      )
  })
  
  output$node_profile <- renderUI({
    s <- node_data()
    selectedValue <- "cont_n_contratista"
    n <- s$n
    tagList(
      renderNode(n, dics)
      #networkChart(s$nodesFin, s$edgesFin, selectedValue)
    )
  })
  
  output$fin_profile <- renderUI({
    #input$clicked_cand$id
    fin <- selectedFin()
    node_id <- node_id()
    if(is.null(fin)){
      out <- div(id="haga-click",
                 p("Haga click en algún financiador para ver sus detalles")
      )
      return(out)
    }
    nfin <- nodes %>% filter(uid == fin) %>% as.list() %>% keep(~!is.na(.))
    other <- getOtherFinancedIden(fin, node_id, nodes, edges)
    tagList(
      div(id="haga-click2",
          renderNodeFin(nfin),
          div(class="fin-list",
              h4(class="color-red",paste("Otros Candidatos que Financió: ",length(other))),
              map(names(other),function(x){
                a(id=other[x],class="cand-link",p(x))
                #a(id=other[x],class="cand-link",p(paste(x,other[x], nodes$name[nodes$id == other[x]])))
              })
          )
      )
    )
  })
  
  output$fin_details <- renderUI({
    if(is.null(fin_con())) return()
    if(nrow(fin_con()) == 0) return()
    tagList(
      br(),
      br(),
      tabsetPanel(type = "pills",
        tabPanel("Tabla de Contratos", 
                 br(),
                 DT::dataTableOutput("fin_con_table")
                 ),
        tabPanel("Línea de Tiempo Contratos", 
                 br(),
                 timevisOutput("fin_cont_time")
                 )
      )
    )
  })
  
  fin_con <- reactive({
    fin <- selectedFin()
    if(is.null(fin)) return()
    fin_con <- contratos %>% 
      filter(cont_iden_clean == fin)
  })
  
  output$fin_con_table <- renderDataTable({
    fin_con <- fin_con()
    fin_con <- fin_con[1:50]
    names(fin_con) <- contratos_dic$label
    datatable(fin_con, class="compact", options = list(
      language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
      pageLength = 20, scrollX=TRUE
    ))
  })
  
  output$fin_cont_time <- renderTimevis({
    fin_con <- fin_con()
    getTimeline(fin_con)
  })
  
})


