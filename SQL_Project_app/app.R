#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(tidymodels)
library(readr)
library(readxl)
library(shinyWidgets)

test_cl = read_excel("test_cl.xlsx")
factors = c("Gender", "HasCrCard", "Exited")
test_cl = test_cl %>% mutate_at(factors, factor)

log_reg_model = readRDS("log_reg_model.rds")
tree_model = readRDS("tree_model.rds")
rf_model = readRDS("rf_model.rds")
rf_tunned_model = readRDS("rf_tunned_model.rds")
xgb_model = readRDS("xgb_model.rds")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Рекомендации по увеличению метрики удержания клиентов банком"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          prettyRadioButtons(
            inputId = "Script",
            label = "Выберете сценарий для увеличения количества, удержанных клиентов", 
            choices = c("1: увеличение количества продуктов у клиента", "2: величение баланса клиентов"),
            icon = icon("fa-solid fa-bookmark"), 
            bigger = TRUE,
            status = "info",
            animation = "smooth"
          ),
          
          actionButton(
            inputId = "Info_script",
            label = HTML("Узнать про сценарии подробнее")
          ),
          
          pickerInput(
            inputId = "Model",
            label = "Выберете модель для симуляции", 
            choices = c("Логистическая регрессия", "Дерево решений", "Случайный лес", "Случайный лес с тьюнингом", "XGboost"),
            options =  list(showIcon = TRUE))
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tableOutput("Metrics"),
          plotOutput("Result")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  observeEvent(input$Info_script, {
    sendSweetAlert(
      session = session,
      title = "О сценариях",
      text = HTML("<b> 1 сценарий: </b> Увеличение количества продуктов до двух у клиентов, которые пользуются одним продуктом. 
      Данная стратегия может быть реализована при помощи персонализированной рекламы продуктов в приложении банка. <br> 
      <b> 2 сценарий: </b> Увеличение баланса клиентов на 10%. Данная стратегия может быть реализована вводом премиальных программ 
      с условием по балансу клиента илли вводом периодов повышенных процентов по счетам."),
      type = "info",
      html = TRUE)
  })
  
  Simulation_data <- reactive({
    if (input$Script == "1: увеличение количества продуктов у клиента"){
 
      test_prod = test_cl %>% 
        mutate(NumOfProducts = ifelse(NumOfProducts == 1, NumOfProducts + 1, NumOfProducts))
      
      if (input$Model == "Логистическая регрессия") {
        pred_prod = predict(log_reg_model, test_prod)
      } 
      else {
        if (input$Model == "Дерево решений"){
          pred_prod = predict(tree_model, test_prod)
        }
        else{
          if(input$Model == "Случайный лес"){
            pred_prod = predict(rf_model, test_prod)
          }
          else{
            if(input$Model == "Случайный лес с тьюнингом"){
              pred_prod = predict(rf_tunned_model, test_prod)
            }
            else{
              pred_prod = predict(xgb_model, test_prod)
            }
          }
        }
      }
    }
    else{
      
      test_balance = test_cl %>% 
        mutate(Balance = Balance*1.1)
      
      if (input$Model == "Логистическая регрессия") {
        pred_prod = predict(log_reg_model, test_balance)
      } 
      else {
        if (input$Model == "Дерево решений"){
          pred_prod = predict(tree_model, test_balance)
        }
        else{
          if(input$Model == "Случайный лес"){
            pred_prod = predict(rf_model, test_balance)
          }
          else{
            if(input$Model == "Случайный лес с тьюнингом"){
              pred_prod = predict(rf_tunned_model, test_balance)
            }
            else{
              pred_prod = predict(xgb_model, test_balance)
            }
          }
        }
      }
    }
    pred_prod
  })
  
  Pred_test <- reactive({
    if (input$Model == "Логистическая регрессия") {
      pred_test = predict(log_reg_model, test_cl)
    } 
    else {
      if (input$Model == "Дерево решений"){
        pred_test = predict(tree_model, test_cl)
      }
      else{
        if(input$Model == "Случайный лес"){
          pred_test = predict(rf_model, test_cl)
        }
        else{
          if(input$Model == "Случайный лес с тьюнингом"){
            pred_test = predict(rf_tunned_model, test_cl)
          }
          else{
            pred_test = predict(xgb_model, test_cl)
          }
        }
      }
    }
    pred_test
  })
  
  Data_metrics <- reactive({
    Pred_test() %>% 
      cbind(test_cl) %>% 
      conf_mat(truth = Exited, estimate = .pred_class) %>% 
      summary() %>% 
      select(-.estimator) %>% 
      filter(.metric == "accuracy" | .metric == "sens" | .metric == "spec" | .metric == "precision" | .metric == "recall") %>% 
      rename( Метрика = ".metric", Значение = ".estimate" ) 
      
  })
  
  output$Metrics = renderTable({
    Data_metrics()
  })
  
  output$Result = renderPlot({
    ggplot(Pred_test()) + geom_bar(aes(x = factor(.pred_class)), alpha = 0.5) +
      geom_bar(data = data.frame(Simulation_data()), aes(x = .pred_class), alpha = 0.5, fill = "royalblue")+
      scale_x_discrete(name = "Ушел ли клиент?", labels = c("Остался","Ушел"))+
      labs(title = "Распределение оттока клиентов", y = "Количество клиентов")+
      theme_light() 
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
