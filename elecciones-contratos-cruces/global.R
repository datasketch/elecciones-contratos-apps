library(shiny)
library(shinyjs)
library(tidyverse)
library(hgchmagic)
library(stringr)

numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 