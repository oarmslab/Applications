library(shinydashboard)
library(tidyverse)

ui <- dashboardPage(
  dashboardHeader(title = "GPA Calculator"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("GPA Calculator", tabName = "gpa_calculator")
    ),
    sidebarMenu(
      menuItem("About", tabName = "about")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "gpa_calculator",
              fluidRow(
                box(width = 8,
                    uiOutput("input_fields"),
                    actionButton("save_s", "Save Subjects"),
                    verbatimTextOutput("output_list")
                ),
                box(width = 2,
                    uiOutput("grade_fields"),
                    verbatimTextOutput("output_1")
                ),
                box(width = 2,
                    uiOutput("credit_val_fields"),
                    verbatimTextOutput("output_2")
                )
              ),
              box(tableOutput("table_complete")),
              box("Your GPA is", textOutput("your_GPA"), ifelse(input$t_input == "GPA on 4", "/ 4", "/ 5"))
      ),
      tabItem(tabName = "about",
              h2("About GPA Calculator"),
              p("This app is used to calculate GPA on a scale of 4 or 5. In order to get your GPA, first select the number of courses. Then enter the respective courses, the earned grades, and the credit values. Once that is done, hit the 'Save Subjects' button and a summary table will be returned alongside your GPA")
      )
    )
  )
)

server <- function(input, output) {

  user_inputs <- eventReactive(input$save_s, {
    inputs <- data.frame(
      subject = sapply(1:input$n_input, function(i) input[[paste0("input_", i)]]),
      grade = sapply(1:input$n_input, function(i) input[[paste0("grade_", i)]]),
      credit_value = sapply(1:input$n_input, function(i) input[[paste0("credit_val_", i)]])
    )
    inputs
  })

  output$input_fields <- renderUI({
    lapply(1:input$n_input, function(i) {
      textInput(paste0("input_", i), label = paste0("Subject ", i))
    })
  })

  output$grade_fields <- renderUI({
    lapply(1:input$n_input, function(i) {
      if (input$t_input == "GPA on 4") {
        selectInput(inputId = paste0("grade_", i), label = paste0("Grade ", i),
                    choices = c("A", "B+", "B", "C+", "C", "D+", "D", "F"))
      } else {
        selectInput(inputId = paste0("grade_", i), label = paste0("Grade ", i),
                    choices = c("A", "B", "C", "D", "E", "F"))
      }
    })
  })

  output$credit_val_fields <- renderUI({
    lapply(1:input$n_input, function(i) {
      numericInput(paste0("credit_val_", i), label = paste0("Credit Value ", i), value = 0, min = 0)
    })
  })

  output$output_list <- renderTable({
    as.data.frame(user_inputs())
  })

  output$table_complete <- renderTable({
    scores_data <- as.data.frame(user_inputs())
    if (input$t_input == "GPA on 4") {
      scores_data <- scores_data %>%
        mutate(grade_point
