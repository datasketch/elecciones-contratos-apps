library(tidyverse)
library(hgchmagic)
library(mop)
library(visNetwork)
library(timevis)
library(shiny)
library(DT)
library(V8)
library(glue)
source("funs.R")

library(shinyjs)

jscode <- "
shinyjs.init = function() {
  $(document).on('click', '.cand-link', function () {
    Shiny.onInputChange('clicked_cand', {id: this.id, timestamp:Math.random()});
    console.log('clicked: ' + this.id); 
  });
}"

css <- '

'


shinyUI(
  fluidPage(
    useShinyjs(),
    tags$head(
      #tags$link(rel="stylesheet", type="text/css", href="style.css"),
      includeScript("js/iframeSizer.contentWindow.min.js")
    ),
    includeCSS("style.css"),
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     tags$img(src = 'Cargando.gif', class="loadmessage")),
    inlineCSS(css),
    fluidRow(
             uiOutput("node_profile"),
             #verbatimTextOutput("debug"),
             br()
    ),
    fluidRow(
      column(6,class = "bg-white",
        div(id="network-legend",
            div(class="square"),span("Tiene contratos en SECOP"),
            div(id="circle"), span("No tiene contratos"),
            div(class="square square-blue"),span("Empresa"),
            div(class="square square-red"),span("Persona")
             ),
        visNetworkOutput("network")
      ),
      column(6, class = "bg-white",
        uiOutput("fin_profile")
             )
    ),
    fluidRow(class = "bg-light-blue",
      uiOutput("fin_details")
    ),
    extendShinyjs(text = jscode)
))