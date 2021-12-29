install.packages('plotly')
library(plotly)
fig <- plot_ly(
    type = "sankey",
    orientation = "h",

    node = list(
      label = c("A1", "A2", "B1", "B2", "C1", "C2"),
      color = c("blue", "blue", "red", "red", "blue", "blue"),
      pad = 15,
      thickness = 20,
      line = list(
        color = "purple",
        width = 0.5
      )
    ),

    link = list(
      source = c(0,1,0,2,3,3),
      target = c(2,3,3,4,4,5),
      value =  c(8,4,2,8,4,2)
    )
  )
fig <- fig %>% layout(
    title = "Basic Sankey Diagram",
    font = list(
      size = 10
    )
)

fig

## networkD3 library
install.packages('networkD3')
library(networkD3)
library(dplyr)

links <- data.frame(
    source = c('a','a','b','c','c','e'),
    target = c('b','c','d','f','g','h'),
    value = c(10,5,3,8,2,8)

)

nodes2 <- data.frame(
    name = c(as.character(links$source),
    as.character(links$target)) %>% unique()
)


links$IDsource <- match(links$source, nodes2$name)-1
links$IDtarget <- match(links$target, nodes2$name)-1

p <- sankeyNetwork(
    Links = links,
    Nodes = nodes2,
    Source = 'IDsource',
    Target = 'IDtarget',
    Value = 'value',
    NodeID = 'name',
    sinksRight = FALSE
    )

 
p