
library(tidyverse)
library(hgchmagic)
library(mop)
library(visNetwork)
library(timevis)
library(shiny)
library(glue)
source("funs.R")
#setwd("/Users/jpmarindiaz/dsrepo/elecciones-contratos-apps")

contratos <- read_csv("data/original/secop_data.csv")
contratos_dic <- read_csv("data/original/secop_dic.csv")

nodes_dic <- read_csv("data/clean/nodes_dic.csv")
dic <- bind_rows(contratos_dic, nodes_dic)

nodes <- read_csv("data/clean/nodes.csv", col_types = cols(.default = "c")) %>% 
  mutate(uid = as.numeric(uid),
         cont_n_contratista = as.numeric(cont_n_contratista),
         cont_tot_contratista = as.numeric(cont_tot_contratista))

edges <- read_csv("data/clean/edges.csv", col_types = cols(.default = "c")) %>% 
  mutate(cand_iden = as.numeric(cand_iden), fin_iden = as.numeric(fin_iden),
         fin_valor = as.numeric(fin_valor), fin_n = as.numeric(fin_n)) %>% 
  select(from = cand_iden, to = fin_iden, everything())

dics <- read_csv("data/clean/nodes_dic.csv")


node_id <- 19333686 #peñalosa
node_id <- 19164378 #mockus
cands <- nodes %>% filter(tipo %in% c("Candidato|Financiador","Candidato"))
#node_id <- sample(cands$uid,1)
initCands <- c("ALVARO URIBE VELEZ", "ARMANDO ALBERTO BENEDETTI VILLANEDA",
               "AURELIJUS RUTENIS ANTANAS MOCKUS SIVICKAS", 
               "ALEJANDRO CHAR CHALJUB",
               "ENRIQUE PEÑALOSA LONDOÑO", "FEDERICO ANDRES GUTIERREZ ZULUAGA",
               "NORMAN MAURICE ARMITAGE CADAVID","CESAR OMAR ROJAS AYALA")
uids <- nodes %>% filter(name %in% initCands) %>% pull(uid)
destacados_uids <- uids
node_id <- sample(uids,1)

s <- getFinNetwork(node_id, nodes, edges, top = 10, contratos = FALSE)
n <- s$n
nodesFin <- s$nodesFin
edgesFin <- s$edgesFin

# generate individual nets
map(destacados_uids, function(node_id){
  s <- getFinNetwork(node_id, nodes, edges, top = 10, contratos = FALSE)
  n <- s$n
  nodesFin <- s$nodesFin
  edgesFin <- s$edgesFin
  v <- networkChart(nodesFin, edgesFin, selectedValue, dics = dics)
  path <- paste0("home-data",mop::create_slug(paste0(nodes$name[nodes$uid==node_id])),".html")
  htmlwidgets::saveWidget(v, path, selfcontained = FALSE, libdir = "lib")
})




#availableValues <- c("cont_n_contratista","cont_n_rep_legal",
#                     "cont_tot_contratista","cont_tot_rep_legal")
#selectedValue <- sample(availableValues,1)
selectedValue <- "cont_n_contratista"
html_node <- renderNode(n, dics)
#nodesFin %>% select(value)
v <- networkChart(nodesFin, edgesFin, selectedValue, dics = dics)
#v <- networkChart(nodesFin, edgesFin)
v



fin <- sample(edgesFin$to,1)
nfin <- nodes %>% filter(uid == fin) %>% as.list() %>% keep(~!is.na(.))

fin_con <- contratos %>% 
  filter(cont_iden_clean == fin)
getTimeline(fin_con)

getOtherFinancedIden(fin, node_id)



