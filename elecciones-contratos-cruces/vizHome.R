library(hgchmagic)

financiadores <- read_csv('data/financiadores/contratos_financiadores_data.csv')
d <- financiadores %>% filter(campaña == 'Congreso 2018')
d <- d %>% filter(Moneda == 'Pesos (COP)')
f1 <- d[!duplicated(d[c("Numero de Identificación", "Tipo Persona", "Anno Firma del Contrato")]),]
f1 <- f1 %>% group_by(`Tipo Persona`, `Anno Firma del Contrato`) %>% summarise(total = n())
h <- hgch_line_CatYeaNum(f1, 
                    #thema = tma(symbLine = FALSE, width = 450, height = 300),
                    horLabel = 'Número de financiadores con contratos en SECOP', verLabel = 'Año de la firma del contrato',
                    tooltip = list(headerFormat = 'Clikea para ver financiadores destacados<br/>', 
                                   pointFormat = paste0("<b>Año firma del contrato: </b>{point.category}<br/><b>{series.name}: </b>{point.y}")))

saveWidget(h, 'congresoLinea.html')
d <- financiadores %>% filter(campaña == 'Congreso 2018')
d <- d %>% filter(`Anno Firma del Contrato` == 2017)
ani <- d %>% filter(Moneda == 'Pesos (COP)')
ani$`Municipios Ejecucion` <- trimws(gsub('-.*', '', ani$`Municipios Ejecucion`))
ani <- ani[!duplicated(ani[c("Numero de Identificación", "Municipios Ejecucion")]),]

f1 <- ani %>%  group_by(`Municipios Ejecucion`) %>% summarise(total = n())
h <- hgch_bar_CatNum(f1, horLabel = '', verLabel = 'Total financiadores con contratos',
                thema = tma(colores = c('#A6CEDE')), sort = 'desc',tooltip = list(pointFormat ="<b>{point.name}:</b> {point.y}")) 
saveWidget(h, 'congresoBarras.html')                        

