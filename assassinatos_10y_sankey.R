library(readxl)

y1 <- read_excel("assassinatos_10y_CPT.xls", na = "-")

library(dplyr)

y2 <- y1 %>%
  select(Ano,Bioma,Estado,Categoria,Sexo,Lideranca) %>% 
  count(Ano,Bioma,Estado,Categoria,Sexo,Lideranca, sort=T)
str(y2)
head(y2)

library(networkD3)

links <- data.frame(source = c(y2$Bioma,y2$Ano,y2$Estado,y2$Categoria,y2$Sexo),
                    target = c(y2$Ano,y2$Estado,y2$Categoria,y2$Sexo,y2$Lideranca),
                    value = y2$n)

# now convert as character
links$source <- as.character(links$source)
links$target<- as.character(links$target)

nodes <- data.frame(name = unique(c(links$source, links$target)))

links$source <- match(links$source, nodes$name) - 1
links$target <- match(links$target, nodes$name) - 1

# Remove cyclical links (source node is downstream from target node)
links <- links[links$source < links$target, ]#para excluir o node que da volta

library(htmlwidgets) #to run custom JavaScript 
library(htmltools)

# Make the Network
network <- sankeyNetwork(Links = links, Nodes = nodes,
                         Source = 'source', Target = 'target',
                         Value = 'value', NodeID = 'name', 
                         sinksRight=FALSE, fontSize = 18,
                         nodeWidth = 70, height = 800, width = 1700)

network <- htmlwidgets::prependContent(network, htmltools::tags$h1
                ("Assassinatos causados por conflitos no campo (2012-2021) - Dados da Comissão Pastoral da Terra", 
                  style = "font-size: 24px"))
network <- htmlwidgets::appendContent(network, htmltools::tags$p
                ("Fonte: Dados da Comissão Pastoral da Terra  (https://www.cptnacional.org.br) | Visualização gráfica produzida por Francesca Palmeira (@fblpalmeira)"))

network <- htmlwidgets::onRender(network, '
  function(el) { 
    var cols_x = this.sankey.nodes().map(d => d.x).filter((v, i, a) => a.indexOf(v) === i).sort(function(a, b){return a - b});
    var labels = ["Bioma", "Ano", "Estado", "Vítima", "Sexo", "Liderança"];
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
webshot("network.html","network.png",  vheight = 800, vwidth = 1700)

