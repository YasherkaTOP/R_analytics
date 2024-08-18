library(shiny)
library(readxl)
library(tidyverse)
library(ggplot2)
data <- read_xlsx("Данные 2020.xlsx")
numeric_columns <- names(data)[sapply(data, is.numeric)]
data$Месяц <- factor(data$Месяц, levels = c("Январь", "Февраль", "Март", "Апрель", "Май", "Июнь", "Июль", "Август", "Сентябрь", "Октябрь", "Ноябрь", "Декабрь"))

summary_df <- data %>%
  group_by(Квартал, Месяц) %>%
  summarise(across(c(numeric_columns), sum)) %>%
  mutate(Регион = "Россия")

data <- bind_rows(data, summary_df)

ui <- fluidPage(
  
  titlePanel("Ваш заголовок дашборда"),

  sidebarPanel(
    
    # Выпадающее меню для выбора региона
    selectInput("region", "Выберите регион:",
                unique(data$Регион), selected = "Россия"),
    
    # Выпадающее меню для выбора показателя
    selectInput("indicator", "Выберите показатель:",
                sort(numeric_columns), selected = sort(numeric_columns)[1]),
    
    # Радиокнопки для выбора типа графика
    radioButtons("plot_type", "Тип графика:",
                 c("Распределение показателя", "Динамика показателя")),
    
    # Ввод цвета графика
    textInput("plot_color", "Цвет графика:", value = "#808080")
  ),
  
  mainPanel(
    plotOutput("plot")
  )
)

server <- function(input, output) {
  
  # Функция для отрисовки ящика с усами
  draw_boxplot <- function(data, color) {
    output$plot <- renderPlot({
      boxplot(data[,input$indicator], col = color, main = "Boxplot")
    })
  }
  
  # Функция для отрисовки линейного графика
  draw_lineplot <- function(data, color) {
    avg_data <- aggregate(data[,input$indicator], by = list(data$Месяц), FUN = mean)
    output$plot <- renderPlot({
      modified_string <- paste0("`", input$indicator, "`")
      ggplot(avg_data, aes_string(x = "Group.1", y = modified_string, group = 1)) +
        geom_line(size = 1.5, color = color) +
        labs(title = sprintf("Динамика %s по месяцам", input$indicator),
             x = "Месяц",
             y = "Параметр") +
        theme(axis.text = element_text(size = 12))
    })
  }
  
  # Выбор данных в зависимости от выбора региона
  observe({
    data_filtered <- data[data$Регион == input$region, ]
    if (input$plot_type == "Распределение показателя") {
      draw_boxplot(data_filtered, input$plot_color)
    } else if (input$plot_type == "Динамика показателя") {
      draw_lineplot(data_filtered, input$plot_color)
    }
  })
}

shinyApp(ui = ui, server = server)
