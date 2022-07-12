library (readxl)

y1 <- read_excel("assassinatos_10y_CPT.xls", na = "-")

library (dplyr)

y2 <- y1 %>%
  select(Ano,Bioma,Categoria,Sexo,Lideranca) %>% 
  count(Ano,Bioma,Categoria,Sexo,Lideranca, sort=T)
str(y2)
head(y2)

library(networkD3)

links <- data.frame(source = c(y2$Ano,y2$Bioma,y2$Categoria,y2$Sexo),
                    target = c(y2$Bioma,y2$Categoria,y2$Sexo,y2$Lideranca),
                    value = y2$n)

# now convert as character
links$source <- as.character(links$source)
links$target<- as.character(links$target)

nodes <- data.frame(name = unique(c(links$source, links$target)))

links$source <- match(links$source, nodes$name) - 1
links$target <- match(links$target, nodes$name) - 1

# Remove cyclical links (source node is downstream from target node)
links <- links[links$source < links$target, ]#para excluir o n? que d? volta

library(htmlwidgets) #to run custom JavaScript 
#library(htmltools)

# Make the Network
network <- sankeyNetwork(Links = links, Nodes = nodes,
                         Source = 'source', Target = 'target',
                         Value = 'value', NodeID = 'name', 
                         sinksRight=FALSE, fontSize = 18,
                         nodeWidth = 70, height = 800, width = 1600)

network <- htmlwidgets::prependContent(network, htmltools::tags$h1
                ("Assassinatos causados por conflitos no campo (2012-2021) - Dados da Comissão Pastoral da Terra", 
                  style = "font-size: 24px"))
network <- htmlwidgets::appendContent(network, htmltools::tags$p
                ("Fonte: Dados da Comissão Pastoral da Terra  (https://www.cptnacional.org.br) | Visualização gráfica produzida por Francesca Palmeira (@fblpalmeira)"))

network <- htmlwidgets::onRender(network, '
  function(el) { 
    var cols_x = this.sankey.nodes().map(d => d.x).filter((v, i, a) => a.indexOf(v) === i).sort(function(a, b){return a - b});
    var labels = ["Ano", "Bioma", "Vítima", "Sexo", "Liderança"];
    cols_x.forEach((d, i) => {
      d3.select(el).select("svg")
        .append("text")
        .attr("x", d)
        .attr("y", 12)
        .text(labels[i])
        .style("font-size", "18px");
    })
  }
')

network <- htmlwidgets::onRender(
  network,
  'function(el, x) { 
    d3.selectAll(".legend text").style("fill", "white");
    d3.select("body").style("background-color", "white");
    d3.select("h1").style("color", "gray").style("font-family", "sans-serif");
    d3.select("body")
      .style("background-image", "url(https://pbs.twimg.com/profile_images/781261591543242752/ANOrZfwm_400x400.jpg)")
      .style("background-repeat", "no-repeat")
      .style("background-position", "top right")
      .style("background-size", "150px")
;
  }'
)

network

# you save it as an html
saveNetwork(network, "network.html")

library(jsonlite)
library(webshot2)

# you convert it as png
webshot("network.html","network.png",  vheight = 800, vwidth = 1600)

#####

#library(networkD3)

links <- data.frame(source = c(paste0(y2$Ano, "_1"), paste0(y2$Sexo, "_2"), paste0(y2$Lideranca, "_3")),
                    target = c(paste0(y2$Sexo, "_2"), paste0(y2$Lideranca, "_3"), paste0(y2$Categoria, "_4")))
                    #value = y2$n)

links <- data.frame(source = c(paste0(y2$Ano,'_1'),paste0(y2$Sexo,'_2'),paste0(y2$Lideranca,'_3')),
                    target = c(paste0(y2$Sexo,'_2'),paste0(y2$Lideranca,'_3'),paste0(y2$Categoria,'_4')))

# now convert as character
links$source <- as.character(links$source)
links$target<- as.character(links$target)

nodes <- data.frame(name = unique(c(links$source, links$target)))

links$source <- match(links$source, nodes$name) - 1
links$target <- match(links$target, nodes$name) - 1

#library(sfo)
#library(plotly)
#library(magrittr)
#library(htmlwidgets)
#library(htmltools)

# Make the Network
network <- sankeyNetwork(Links = links, Nodes = nodes,
                         Source = 'source', Target = 'target',
                         Value = 'value', NodeID = 'name', 
                         sinksRight=FALSE, fontSize = 18, nodeWidth = 70, 
                         height = 800, width = 1300)

network <- htmlwidgets::prependContent(network, htmltools::tags$h1("", style = "font-size: 23px"))
network <- htmlwidgets::appendContent(network, htmltools::tags$p(""))

network <- htmlwidgets::onRender(network, '
  function(el) { 
    var cols_x = this.sankey.nodes().map(d => d.x).filter((v, i, a) => a.indexOf(v) === i).sort(function(a, b){return a - b});
    var labels = ["Nome dos indiv?duos", "Esta??o", "Unidade de Conserva??o"];
    cols_x.forEach((d, i) => {
      d3.select(el).select("svg")
        .append("text")
        .attr("x", d)
        .attr("y", 12)
        .text(labels[i])
        .style("font-size", "18px");
    })
  }
')

network <- htmlwidgets::onRender(
  network,
  'function(el, x) { 
    d3.selectAll(".legend text").style("fill", "white");
    d3.select("body").style("background-color", "white");
    d3.select("h1").style("color", "red").style("font-family", "sans-serif");
    d3.select("body")
      .style("background-image", "url()")
      .style("background-repeat", "no-repeat")
      .style("background-position", "top right")
      .style("background-size", "500px")
;
  }'
)

network

# you save it as an html
saveNetwork(network, "network2.html")

#library(jsonlite)
#library(webshot2)

# you convert it as png
webshot("network2.html","network2.png", vheight = 800, vwidth = 1300)

###

name <- c('Enrolled', 'Opted-Out', 'Invited', 'Activated')
xpos <- c(0, 1, 1, 2)
nodes <- data.frame(name, xpos)

source <- c(0, 0, 2)
target <- c(1, 2, 3)
value <- c(20, 80, 60)
links <- data.frame(source, target, value)

sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
              Target = "target", Value = "value", NodeID = "name",
              units = "TWh", fontSize = 12, nodeWidth = 30, sinksRight = FALSE)

####
library("shiny")
library("networkD3")

ui <- fluidPage(
  column(3),
  column(6, sankeyNetworkOutput("mySankeyD")),
  column(3)
)

server <- function(input, output) {
  output$mySankeyD <- renderSankeyNetwork({
    myDf <- list(
      nodes=data.frame(name=c( "A", "B", "C", "D", "E",
                               "V", "W", "X", "Y", "Z")),
      links=data.frame(source=as.integer(c(0, 1, 2, 3, 3, 4, 4)),
                       target=as.integer(c(7, 6, 7, 8, 7, 5, 9)),
                       value =           c(1, 4, 1, 5, 1, 5, 3)
      )
    )
    
    sankeyNetwork(Links = myDf$links, Nodes = myDf$nodes, Source = "source",
                  Target = "target", Value = "value", NodeID = "name",
                  units = "TWh", fontSize = 25, nodeWidth = 30, fontFamily = "sans-serif", iterations = 30)
  })
}

shinyApp(ui, server)_UI <- function(id) {
  ns <- NS(id)
  tagList(
  
  )
}

library("shiny")
library("networkD3")

ui <- fluidPage(
  column(3),
  column(6, sankeyNetworkOutput("mySankeyD")),
  column(3)
)

server <- function(input, output) {
  output$mySankeyD <- renderSankeyNetwork({
    myDf <- list(
      links <- data.frame(source = c(y2$Ano,y2$Bioma,y2$Estado,y2$Categoria,y2$Sexo),
                          target = c(y2$Bioma,y2$Estado,y2$Categoria,y2$Sexo,y2$Lideranca),
                          value = y2$n)
      
      nodes <- data.frame(name = unique(c(links$source, links$target)))
      
      )
    )

  # Make the Network
  sankeyNetwork(Links = links, Nodes = nodes,
                           Source = 'source', Target = 'target',
                           Value = 'value', NodeID = 'name', 
                           sinksRight=FALSE, fontSize = 18,
                           nodeWidth = 70, height = 800, width = 1300)
  
  })
}

shinyApp(ui, server) <- function(input, output, session) {
  
}

