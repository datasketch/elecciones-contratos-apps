library(shiny)
library(shinyjs)
library(tidyverse)
library(hgchmagic)
library(stringr)
library(RSQLite)

numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 

hcoptslang <- getOption("highcharter.lang")
hcoptslang$contextButtonTitle <- 'Descargar Imagen'
hcoptslang$printChart <- "Imprimir Gráfico"
hcoptslang$downloadJPEG <- "Descarga en JPEG"
hcoptslang$downloadPNG <- "Descarga en PNG"
hcoptslang$downloadPDF <- "Descarga en PDF"
hcoptslang$downloadSVG <- "Descarga en SVG"
hcoptslang$thousandsSep <- "."
hcoptslang$decimalPoint <- ","
options(highcharter.lang = hcoptslang)