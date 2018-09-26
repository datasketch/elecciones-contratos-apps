library(tidyverse)
library(hgchmagic)
library(RSQLite)

library(mop)

cc <- read_csv("data/original/cuentas_claras_data.csv")
cc <- cc %>% mutate(fecha_reg = substr(anytime(fecha_reg),1,10))
cc_dic <- read_csv("data/original/cuentas_claras_dic.csv")

#ps <- problems(cc)
contratos <- read_csv("data/original/secop_data.csv")
contratos_dic <- read_csv("data/original/secop_dic.csv")



### NODES
can_vars <- c("cand_iden", "cand_nombre",
          "cargo","cand_departamento","org_politica",
          "cand_eleg", "campana")
candidatos <- cc %>% select(one_of(can_vars)) %>% 
  distinct(cand_iden, .keep_all = TRUE) %>% 
  mutate(
    uid = cand_iden,
    name = cand_nombre,
    tipo = "Candidato",
    tipo_iden = "Cédula de Ciudadanía",
    persona_juridica = "Persona Natural"
  )
fin_vars <- c("fin_iden","fin_nombre",
                "fin_tipo_iden", 
                "persona_juridica",
                "fin_departamento","fin_municipio",
                "campana")
financiadores <- cc %>% 
  select(one_of(fin_vars)) %>% 
  distinct(fin_iden, .keep_all = TRUE) 
financiadores <- financiadores %>% 
  mutate(
    uid = fin_iden,
    name = fin_nombre,
    tipo = "Financiador"
  )
# Candidatos que también son financiadores
which_in(candidatos$uid,financiadores$uid)
which_not_in(names(candidatos),names(financiadores))
which_not_in(names(financiadores),names(candidatos))
intersect(names(candidatos), names(financiadores))
x <- bind_rows(candidatos, financiadores) %>% select(uid, everything())
### Duplicates: Candidatos y Financiadores
# xdup <- x %>% 
#   group_by(uid) %>% 
#   filter(n()>1)
###
##### Remove duplicate
x_ <- coalesce_rows(x, uid)
##x_ <- x
### REMOVE UID NOT DEFINED
x_ <- x_ %>% 
  filter(!is.na(uid))
x2 <- x_ %>% mutate(uid = as.numeric(uid), cand_iden = as.numeric(cand_iden), fin_iden = as.numeric(fin_iden))
n_cont_contratista <- contratos %>% 
  group_by(cont_iden_clean) %>% 
  summarise(cont_n_contratista = n(), cont_tot_contratista = sum(cont_valor_tot))
n_cont_rep_legal <- contratos %>% 
  filter(!rep_legal_iden_clean %in% n_cont_contratista$cont_iden_clean) %>% # filter out contratos que ya tiene id de contratista
  group_by(rep_legal_iden_clean) %>% 
  summarise(cont_n_rep_legal = n(), cont_tot_rep_legal = sum(cont_valor_tot)) %>% 
  filter(rep_legal_iden_clean > 10000)
x3 <- left_join(x2, n_cont_contratista, by = c("uid" = "cont_iden_clean")) %>% 
  left_join(n_cont_rep_legal, by = c("uid" = "rep_legal_iden_clean"))

#x4 <- x3 %>% ungroup() %>%  filter(is.na(uid))

dbpublic <- read_csv("data/aux/db-public-elecciones-contratos.csv")
db <- dbpublic %>% select(uid = cedula, everything())
names(db)[3:10] <- paste0("db_",names(db)[3:10])
x4 <- x3 %>% left_join(db)

# nodes_var_ids <- which_in(names(x4),cc_dic$id)
# nodes_dic <- cc_dic %>% filter(id %in% nodes_var_ids)
# new_nodes_var_ids <- which_not_in(names(x4),cc_dic$id)
# new_nodes_var_ids <- data_frame(id = new_nodes_var_ids)
# nodes_dic <- bind_rows(nodes_dic,new_nodes_var_ids)
#### write_csv(nodes_dic, "data/clean/nodes_dic.csv")  ### Edit by hand

write_csv(x4, "data/clean/nodes.csv")


#### EDGES

ed <- cc %>%  select(cand_iden, fin_iden, campana,
                     fin_valor, fin_departamento, fin_municipio,
                     fin_cod,
                     fin_dona_tipo, fin_parentesco, fin_desc,
                     fecha_reg) %>% 
  arrange(campana, fecha_reg)
ed$fin_parentesco[ed$fin_parentesco == "No Aplica"] <- NA
agg_most_freq <- function(x){
  if(all(is.na(x))) return(NA)
  t <- table(na.omit(x))
  names(which.max(t))[1]
}
agg_collapse_chr <- function(x){
  paste(na.omit(x),collapse = "|")
}

edges <- ed %>% 
  group_by(cand_iden, fin_iden, campana) %>%
  summarise(fin_valor = sum(fin_valor), 
            fin_n = n(),
            fin_departamentos = agg_collapse_chr(unique(fin_departamento)),
            fin_municipios = agg_collapse_chr(unique(fin_municipio)),
            fin_cods = agg_collapse_chr(unique(fin_cod)),
            fin_dona_tipos = agg_collapse_chr(unique(fin_dona_tipo)),
            fin_parentesco = agg_collapse_chr(unique(fin_parentesco)),
            fecha_reg = agg_collapse_chr(fecha_reg)
            )
  

write_csv(ed, "data/clean/edges-full.csv")
write_csv(edges, "data/clean/edges.csv")

#####

## Prepare search file
# unique(x$persona_juridica)
# munis <- c("BOGOTA D.C.", "MEDELLIN","CALI","BARRANQUILLA","VILLAVICENCIO","CARTAGENA")
# search <- x %>% 
#   filter(campana == "Congreso 2018" | 
#            persona_juridica == "Persona Jurídica" | 
#            departamento == "BOGOTA D.C." |
#            municipio %in% munis)
# jsonlite::write_json(search, "data/clean/search.json", auto_unbox = TRUE)  

