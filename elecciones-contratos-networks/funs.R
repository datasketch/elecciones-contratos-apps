
renderNode <- function(n, dics, remove_na = FALSE){
  
  ndbs <- n %>% select(starts_with("db_"))
  varLabels <- map_chr(names(ndbs),~getVarLabel(.,dics))
  dt <- data_frame(val = ndbs %>% as.list() %>% flatten_chr(), 
             label = varLabels)
  if(remove_na){
    dt <- dt %>% filter(!is.na(val))
  }
  dbInfo <- HTML(paste0(
    "<div class='db-list'>
    <p style = 'font-size:15px;font-weight: 600; margin-left: 3%; margin-top: 1%;'>Reportes del candidato en otras bases de datos públicas:</p>",
                        "<ul class = 'col-dbList'>",
                        paste0(glue_data(dt, "<li>{label}:<strong> {val}</strong></li>"),collapse = "\n"),
                        "</ul></div>"))
  tagList(
    fluidRow(
      column(12,
             h2(n$name),
             p(class="color-blue", HTML(glue("<strong><span class='color-violet'>Campaña: </span>",n$campana,"</strong>",
                         " / <span class='color-violet'>Cargo: </span>",n$cargo,
                         " / <span class='color-violet'>Tipo: </span>",n$tipo)))
             ),
      column(6, offset = 6,
             dbInfo
      )
    )
  )
}

renderNodeFin <- function(n){

  tagList(
    fluidRow(
      column(12, class="color-blue",
             h4(class="color-bright-blue","Financiador"),
             h3(class="color-violet",n$name),
             p(glue("Identificación: ", n$fin_iden)),
             p(glue("Departamento del financiador: ",n$fin_departamento)),
             p(glue("Municipio del financiador: ",n$fin_municipio)),
             p(glue("Número de contratos en SECOP: ",n$cont_n_contratista)),
             p(glue("Total monto contratos en SECOP: ", paste0(format(as.numeric(n$cont_tot_contratista), big.mark = ',', small.mark = '.',  scientific = F))))
             
      )
    )
  )
}



getFinNetwork <- function(node_id, nodes, edges, 
                          top = NULL, contratos = FALSE, size_var = "cont_n_contratista"){
  n <- nodes %>% filter(uid == node_id)
  # Get node neighbors
  edg0 <- edges %>% filter(from == node_id)
  edgesFin <- edg0 %>% 
    mutate(width = fin_valor/max(fin_valor)*8,
           label = round(fin_valor/1000000,1))
  nFin <- nrow(edgesFin)
  if(!is.null(top)){
    edgesFin <- edgesFin %>% top_n(top, fin_valor)
  }
  nodesFin <- nodes %>% 
    filter((uid %in% edgesFin$from) | (uid %in% edgesFin$to)) %>% 
    mutate(id = uid, label = name, group = persona_juridica, 
           shape = ifelse(is.na(cont_n_contratista),"dot","square"))
  if(contratos){
    nodesFin <- nodesFin %>% filter(!is.na(cont_n_contratista) | uid == node_id)
    edgesFin <- edgesFin %>% filter(to %in% nodesFin$uid)
  }
  list(
    n = n,
    edgesFin = edgesFin,
    nodesFin = nodesFin,
    nFin = nFin
  )
}

getOtherFinancedIden <- function(fin, current, nodes, edges){
  other <- edges %>% filter(to == fin) %>% pull(from)
  uids <- other[other != current]
  other_cands <- nodes %>% filter(uid %in% uids)
  uids <- other_cands$uid
  names(uids) <- other_cands$cand_nombre
  uids
}

networkChart <- function(nodesFin, edgesFin, selectedValue = NULL, dics){
  # if(!is.null(selectedValue)){
  #   vals <- nodesFin[[selectedValue]]
  # }else{
  #   vals <- selectedValue
  #   if(all(is.na(vals))) vals <- 3
  # }
  # message(vals)
  # nodesFin$value <- vals/max(vals, na.rm = TRUE)*5
  # nodesFin$value[is.na(nodesFin$value)] <- 3
  nodesFin$value[nodesFin$shape == "square"] <- 0.8 * nodesFin$value[nodesFin$shape == "square"]
  label_n_contratista <- getVarLabel("cont_n_contratista", dics)
  label_tot_contratista <- getVarLabel("cont_tot_contratista", dics)
  ld <- nodesFin[c("cont_n_contratista","cont_tot_contratista")]
  #ld$label_n_contratista <- label_n_contratista
  #ld$label_tot_contratista <- label_tot_contratista
  nodesFin$color <- "#5FD3FC"
  nodesFin$color[nodesFin$persona_juridica == "Persona Natural"] <- "#E35A2A"
  str <- glue_data(ld,"{label_n_contratista}:{cont_n_contratista}<br>{label_tot_contratista}:{cont_tot_contratista}<br>")
  nodesFin$title <- str
  visNetwork(nodesFin, edgesFin, width = "100%") 
  #%>%
    #visPhysics(solver = "forceAtlas2Based",
    #           forceAtlas2Based = list(gravitationalConstant = -200))
}


getTimeline <- function(d){
  templateWC <- function(fecha, estado, proceso, ejecucion, familia, tipo, valor) {
    sprintf(
      '<table class="tg">
      <tr>
      <td class="tg-0lax"><span style = "color:#110066">%s</span></td>
      <td class="tg-0lax">Estado: <span style="color:#000000">%s</span></td>
      </tr>
      <tr>
      <td colspan="2" class="tg-0lax"><b>Proceso:</b> %s</td>
      </tr>
      <tr>
      <td colspan="2" class="tg-0lax"><b>Ejecución:</b> %s</td>
      </tr>
      <tr>
      <td colspan="2" class="tg-0lax">%s</td>
      </tr>
      <tr>
      <td class="tg-0lax">%s</td>
      <td class="tg-0lax">%s</td>
      </tr>
      </table>',
      fecha, estado, proceso, ejecucion, familia, tipo, valor
    )
  }
  lTemp <- map_df(1:nrow(d), function(z){
    df <- data.frame(content = templateWC(d$cont_ini_fecha[z], d$proc_status[z], 
                                          d$proc_tipo[z], d$muni_ejec[z], 
                                          d$familia[z], d$proc_tipo[z], 
                                          paste0('$', format(d$cont_adi_valor_tot[z], big.mark = ','))))
    df
  })
  timedata <- data.frame(
    id      = 1:nrow(d),
    content = lTemp$content,
    start   = d$cont_ini_fecha,
    end     = d$cont_fin_ejec_fecha,
    style = rep("background: #d2f2fe;", nrow(d))
  )
  timevis(timedata)
}


getVarLabel <- function(varid, dic){
  dic %>% filter(id == varid) %>% pull(label) %>% .[1]
}

