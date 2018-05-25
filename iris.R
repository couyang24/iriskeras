if (!require("pacman")) install.packages("pacman")
pacman::p_load(knitr, tidyverse, data.table, DT, caret, viridis, 
               plotly, DescTools, GGally, corrplot, skimr, corrgram,
               highcharter)

iris %>% 
  datatable(filter = 'top', options = list(
    pageLength = 15, autoWidth = TRUE
  ))


tran_iris <- iris %>% 
  gather(type, value, -5) %>% 
  unite(Species, c(type, Species), sep = '_')

hcboxplot(tran_iris$value, tran_iris$Species, color = 'firebrick') %>% 
  hc_add_theme(hc_theme_monokai()) %>% 
  hc_chart(type = 'column')

hcboxplot(iris$Sepal.Length, iris$Species, color = 'firebrick') %>% 
  hc_add_theme(hc_theme_google()) %>% 
  hc_chart(type = "column")
hcboxplot(iris$Sepal.Width, iris$Species, color = 'firebrick') %>% 
  hc_add_theme(hc_theme_google()) %>% 
  hc_chart(type = "column")
hcboxplot(iris$Petal.Length, iris$Species, color = 'firebrick') %>% 
  hc_add_theme(hc_theme_google()) %>% 
  hc_chart(type = "column")



plot_ly(iris, x=~Petal.Length, y=~Petal.Width, size = ~Sepal.Length,
       type="scatter",mode="markers", color = ~Species, text=~Species)

graph <- list()
i=1
graph[[i]] <- iris %>%
  GGally::ggpairs(aes(colour = Species, alpha = .4)) %>%
  ggplotly()

graph[[1]]
