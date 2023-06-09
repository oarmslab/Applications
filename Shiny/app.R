library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "GPA Calculator"),
  dashboardSidebar(
    numericInput("n_input", "Number of Subjects:", value = 1, min = 1),
    actionButton("submit", "Submit")
  ),
  dashboardBody(
    fluidRow(
      box(width = 8,
          uiOutput("input_fields"),
          actionButton("save_s", "Save Subjects"),
          verbatimTextOutput("output")
      ),
      box(width = 2,
          uiOutput("grade_fields"),
          actionButton("save_g", "Save"),
          verbatimTextOutput("output_1")
      ),
      box(width = 2,
          uiOutput("credit_val_fields"),
          actionButton("save_cv", "Save"),
          verbatimTextOutput("output_2")
      )
    ),
    box(tableOutput("output_list")),
    box(textOutput("your_GPA"))
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
      selectInput(inputId = paste0("grade_", i), label = paste0("Grade ", i),
                  choices = c("A", "B+", "B", "C+", "C", "D+", "D", "F"))
    })
  })

  output$credit_val_fields <- renderUI({
    lapply(1:input$n_input, function(i) {
      numericInput(paste0("credit_val_", i), label = paste0("Credit Value ", i), value = 0, min = 0)
    })
  })

  output$output_list <- renderTable({
    user_inputs()
  })

  output$your_GPA <- renderText({

    # Convert the output list to a data frame
    scores_data <- as.data.frame("output_list")

    # create a section for grade point

    scores_data <- scores_data %>%
     mutate(grade_point = if_else(grade == "A", 4,
                                  if_else(grade == "B+", 3.5,
                                   if_else(grade == "B", 3,
                                           if_else(grade == "C+", 2.5,
                                                   if_else(grade == "C", 2,
                                                           if_else(grade == "D+", 1.5,
                                                                   if_else(grade == "D", 1, 0))))))),



            gradeP_credit_V = gade_point*credit_value)
    GPA = sum(gradeP_credit_V)/sum(credit_value)

    # Return GPA

    GPA
  })

}

shinyApp(ui, server)
