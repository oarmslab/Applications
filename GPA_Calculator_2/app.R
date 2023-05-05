library(shinydashboard)
library(tidyverse)
library(shinythemes)

################################################################################ User Interface
ui <- dashboardPage(
  dashboardHeader(title = "GPA Calculator"), # title
  dashboardSidebar(h5("This app is used to calculate GPA for most higher institutions of learning in Cameroon. In order to get your GPA, first select the number of courses as well as the GPA scale. Then enter the respective courses, the earned grades, and the credit values. Once that is done, hit the 'Save Subjects' button and a summary table of results will be returned alongside your GPA"),
                   numericInput("n_input", "Number of Subjects:", value = 1, min = 1),
                   selectInput("t_input", "GPA scale", choices = c("GPA on 4", "GPA on 5"))
  ),
  dashboardBody(
    fluidRow(
      box(width = 8, title = strong("Courses"),
          uiOutput("input_fields"),
          actionButton("save_s", "Save Subjects"),
          verbatimTextOutput("output")
      ),
      box(width = 2, title = strong("Grades"),
          uiOutput("grade_fields"),
          verbatimTextOutput("output_1")
      ),
      box(width = 2, title = strong("Credit Values"),
          uiOutput("credit_val_fields"),
          verbatimTextOutput("output_2")
      )
    ),
    box(width = 8, title = strong("Results"),
        tableOutput("table_complete")
        ),
    box(width = 4, title = strong("Grade Point Average (GPA)"),
      "Your GPA is", textOutput("your_GPA"))

  )
)

################################################################################ Server

server <- function(input, output) {

  user_inputs <- eventReactive(input$save_s, { # This ensures that the number of subjects, associated grades and credit values tie with the number of courses selected
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

      if (input$t_input == "GPA on 4"){
      selectInput(inputId = paste0("grade_", i), label = paste0("Grade ", i),
                  choices = c("A", "B+", "B", "C+", "C", "D+", "D", "F"))
      }else{

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

    if (input$t_input == "GPA on 4"){

    # Convert the output list to a data frame
    scores_data <- as.data.frame(user_inputs())

    # create a section for grade point

    scores_data <- scores_data %>%
      mutate(grade_point = if_else(grade == "A", 4,
                                   if_else(grade == "B+", 3.5,
                                           if_else(grade == "B", 3,
                                                   if_else(grade == "C+", 2.5,
                                                           if_else(grade == "C", 2,
                                                                   if_else(grade == "D+", 1.5,
                                                                           if_else(grade == "D", 1, 0))))))))


    scores_data <- scores_data %>%
      mutate(grade_point_credit_value = grade_point*credit_value)

    }else{

      # Convert the output list to a data frame
      scores_data <- as.data.frame(user_inputs())

      # create a section for grade point

      scores_data <- scores_data %>%
        mutate(grade_point = if_else(grade == "A", 5,
                                     if_else(grade == "B", 4,
                                             if_else(grade == "C", 3,
                                                     if_else(grade == "D", 2.5,
                                                             if_else(grade == "E", 1,0))))))



      scores_data <- scores_data %>%
        mutate(grade_point_credit_value = grade_point*credit_value)

    }

  })

  output$your_GPA <- renderText({

    if (input$t_input == "GPA on 4"){

    # Convert the output list to a data frame
    scores_data <- as.data.frame(user_inputs())

    # create a section for grade point

    scores_data <- scores_data %>%
      mutate(grade_point = if_else(grade == "A", 4,
                                   if_else(grade == "B+", 3.5,
                                           if_else(grade == "B", 3,
                                                   if_else(grade == "C+", 2.5,
                                                           if_else(grade == "C", 2,
                                                                   if_else(grade == "D+", 1.5,
                                                                           if_else(grade == "D", 1, 0))))))))


    scores_data <- scores_data %>%
      mutate(grade_point_credit_value = grade_point*credit_value)


    GPA =round(sum(scores_data$grade_point_credit_value)/sum(scores_data$credit_value), 3)

    #Return GPA

    GPA = paste0(GPA, "/ 4.000")
    }else{

      # Convert the output list to a data frame
      scores_data <- as.data.frame(user_inputs())

      # create a section for grade point

      scores_data <- scores_data %>%
        mutate(grade_point = if_else(grade == "A", 5,
                                     if_else(grade == "B", 4,
                                             if_else(grade == "C", 3,
                                                     if_else(grade == "D", 2.5,
                                                             if_else(grade == "E", 1,0))))))


      scores_data <- scores_data %>%
        mutate(grade_point_credit_value = grade_point*credit_value)


      GPA = round(sum(scores_data$grade_point_credit_value)/sum(scores_data$credit_value), 3)

      #Return GPA

      GPA = paste0(GPA, "/ 5.000")

    }
  })

}

shinyApp(ui, server)
