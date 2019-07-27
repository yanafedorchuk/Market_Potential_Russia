library(rio)
library(shiny)
library(plotly)
library(ggplot2)
df <- rio::import("pop_data.xlsx") %>% select(-c(1,3:5,9:14))%>% 
  melt(id.vars = c("ID89", "oblast_center", "CityStatus", "RaionCenter", "URAL", "YUZHNY" ,"DALNEV" , "PRIVOL", "SEVER_ZAPAD", "SIBIRSKII", "TSENTRALNY", "HBR", "LBR")) %>% #, "URAL", "YUZHNY" ,"DALNEV" , "PRIVOL", "SEVER_ZAPAD", "SIBIRSKII", "Tcentralny" 
  mutate(variable = as.character(variable), 
         ratio = ifelse(substring(variable, 1, 3) == "pop", "pop", 
                        ifelse(substring(variable, 1, 4) == "spop", "spop", 
                               ifelse(substring(variable, 1, 3) == "smp", "smp","mp"))), 
         year = substring(variable, nchar(variable)-3, nchar(variable))) %>% 
  select(-variable)%>%
  group_by(ID89, ratio) %>%
  arrange((year)) %>%
  mutate(value = as.numeric(value))%>% 
  mutate(value = diff.xts(value, log = TRUE))


ui <- fluidPage(
  titlePanel("Рыночный потенциал в России"),
  inputPanel(selectInput("method",label = 
                           "Выберите способ расчета рыночного потенциала",
                         choices = c("Расстояняие", "Квадрат расстояния")),
             selectInput('year', 'Выберите год',
                         choices = unique(df$year))),
  mainPanel(plotlyOutput('main'))
) 


 
server <- function(input, output){
  output$main <- renderPlotly({
    p <- df %>%
     filter(year == input$year) %>%
      dcast(ID89+oblast_center+
              CityStatus+ RaionCenter+
              year+URAL+YUZHNY+DALNEV+PRIVOL+
              SEVER_ZAPAD+SIBIRSKII+TSENTRALNY+
              HBR+LBR~ratio, value.var = "value")  %>%
      melt(id.vars = c("ID89","oblast_center",
                         "CityStatus", "RaionCenter",
                         "year",
                         "HBR","LBR", "mp",
                       "pop", "smp", "spop")) %>%
      filter(value ==1 ) %>%
      ggplot() +
      geom_point(aes(x = mp, y = pop, color = variable))+
      geom_text(aes(x = mp, y = pop, color = variable, label = ID89))
    ggplotly(p) %>% layout(height = 600, width = 1000)
      
  })
}
shinyApp(ui,server)
