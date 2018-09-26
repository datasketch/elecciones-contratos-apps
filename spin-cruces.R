
library(tidyverse)
library(hgchmagic)
library(mop)
library(shiny)
library(glue)


contratos <- read_csv("data/original/secop_data.csv")
contratos_dic <- read_csv("data/original/secop_dic.csv")

nodes_dic <- read_csv("data/clean/nodes_dic.csv")
dics <- bind_rows(contratos_dic, nodes_dic)

nodes <- read_csv("data/clean/nodes.csv", col_types = cols(.default = "c")) %>% 
  mutate(uid = as.numeric(uid),
         cont_n_contratista = as.numeric(cont_n_contratista),
         cont_tot_contratista = as.numeric(cont_tot_contratista))

edges <- read_csv("data/clean/edges.csv", col_types = cols(.default = "c")) %>% 
  mutate(cand_iden = as.numeric(cand_iden), fin_iden = as.numeric(fin_iden),
         fin_valor = as.numeric(fin_valor), fin_n = as.numeric(fin_n)) %>% 
  select(from = cand_iden, to = fin_iden, everything())

### 

# Para los candidatos/financiadores/que tienen contratos en SECOP

selectedCampana <- sample(c("Regionales 2015", "Congreso 2018"),1)
selectedCampana
campana_init <- mop::extract_numbers(selectedCampana)

nconts <- nodes %>% filter(cont_n_contratista > 1) %>% 
  discard_all_na_cols()

econts <- edges %>% 
  group_by(to) %>% 
  summarise(aportes_n = sum(fin_n),
            aportes_tot = sum(fin_valor),
            aportes_cand_n = length(unique(from))
            ) %>% 
  select(uid = to, everything())
  
cruces <- left_join(nconts, econts)

dic <- dics %>% filter(id %in% names(cruces))
dic <- bind_rows(dic, data_frame(
  id = c("aportes_n","aportes_tot","aportes_cand_n"),
  label = c("Número de aportes a campañas", "Total aportes en pesos", "Número candidatos financiados"),
  ctype = c("Num","Num","Num")
))

# write_csv(dic, "data/clean/cruces_dic")
# write_csv(cruces, "data/clean/cruces_data.csv")
# 

########

cruces <- read_csv("data/clean/cruces_data.csv")
dic <- read_csv("data/clean/cruces_dic.csv")




# Cat vars
catVars <- dic %>% filter(ctype == "Cat") %>% pull(id)
selectedCatVar <- sample(catVars, 1)

# Num vars
numVars <- dic %>% filter(ctype == "Num") %>% pull(id)
selectedNumVar <- sample(numVars, 1)

df <- cruces %>% select(one_of(c(selectedCatVar, selectedNumVar)))
hgch_bar_CatNum(df)

# Group Var
catVars2 <- catVars[catVars != selectedCatVar]
selectedGrpVar <- sample(catVars2, 1)
df <- cruces %>% select(one_of(c(selectedGrpVar, selectedCatVar, selectedNumVar)))
hgch_bar_grouped_CatCatNum(df)


# selectedVars <- sample(dic$id, 2)
# vars <- dic %>% filter(id %in% selectedVars) %>% arrange(ctype)
# ctypes <- vars$ctype
# vars <- vars %>% pull(id)
# df <- cruces %>% select(one_of(vars))
#gftype <- datafringe::getFtype(df)
#if(grepl("Cat",gftype)) gftype <- c(gftype, sort(gsub("Cat","Oca", gftype)))
#meta <- hgchmagic::hgchMeta()
#meta %>% filter(ftype %in% gftype) %>% filter(group %in% c("pie","bars","donut"))


h <- hgch_bar_CatNum(df)
h




### 
# # Regionales 2015
# 
# finan <- nodes %>% 
#   filter(tipo %in% c("Financiador","Candidato|Financiador")) %>% 
#   filter(campana %in% c("Regionales 2015","Congreso 2018|Regionales 2015","Regionales 2015|Congreso 2018"))
# conts <- contratos %>% 
#   filter(contratista_id %in% finan$uid) %>% 
#   filter(cont_firma_ano >= campana_init, cont_firma_ano <= (cont_firma_ano + 3))

## Número de empresas financiadores que tienen contratos

# emp <- nodes %>% 
#   filter(persona_juridica == "Persona Jurídica") %>% 
#   discard_all_na_cols()

  
  