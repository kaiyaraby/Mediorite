#libraries----
library(shiny)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyverse)




# Layout setup----
ui <- fluidPage(
  
  # App title
  titlePanel("Statistics"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      
      # Input: Select a file 
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line 
      tags$hr(),
      
      
      # Input: Select Output
      checkboxGroupInput("choice","Analysis",choices = c("Raw Data","Box Plot","Paired T-test","Wilcox Test"))),
    
    # Horizontal line
    tags$hr(),
    
  ),
  
  # Main panel for displaying outputs
  mainPanel(
    
    # Output: Data file
    tableOutput("tablehead"),
    
    tableOutput("t_test_table"),
    
    tableOutput("wilcox_table"),
    
    plotOutput(
      "boxplot",
      width = "100%",
      height = "400px",
      click = NULL,
      dblclick = NULL,
      hover = NULL,
      brush = NULL,
      inline = FALSE
    )
    
  )
  
)




# Server ----

server <- function(input, output) {
  
  # Conclusion func ----
  conclusion <- function(pvals,alpha){
    conclusions <- c()
    for(pval in pvals){
      if(pval<=alpha){
        conc <- "Reject"
      }
      else{
        conc <- "Accept"
      }
      conclusions <- append(conclusions, conc)
    }
    return(conclusions)
  }
  
  
  
  # Raw Data Output ----
  output$tablehead <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    req(input$choice)
    tryCatch(
      { raw_data <- read.csv(input$file1$datapath)
      data1 <- raw_data[,1]
      data2 <- raw_data[,2]
      if(input$choice=="Raw Data"){
        print(raw_data)
      }
      
      }
      ,
      error = function(e) {
        stop(safeError(e))
      }
    )
    
    
  })
  
  
  
  # Paired t-test output ----
  output$t_test_table <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    req(input$choice)
    tryCatch(
      {  raw_data <- read.csv(input$file1$datapath)
      data1 <- raw_data[,1]
      data2 <- raw_data[,2]
      two_sided <- t.test(data1, data2, paired = TRUE, alternative = "two.sided")$p.value
      less <- t.test(data1, data2, paired = TRUE, alternative = "less")$p.value
      greater <- t.test(data1, data2, paired = TRUE, alternative = "greater")$ p.value
      alpha <- 0.05
      p_values <- c(two_sided, less, greater)
      
      
      results <- data.frame("Alternative"=c("Two-sided", "Improvement", "Deterioration"), "P-values"=p_values, "Conclusion"=conclusion(p_values, alpha))
      if(input$choice=="Paired T-test"){
        print(results)
      }
      
      }
      ,
      error = function(e) {
        stop(safeError(e))
      }
    )
    
    
  })
  
  
  
  # Box-Plot Output ----
  output$boxplot<- renderPlot({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    req(input$choice)
    tryCatch(
      {   raw_data <- read.csv(input$file1$datapath)
      data1 <- raw_data[,1]
      data2 <- raw_data[,2]
      data_frame <- melt(data.frame(data1,data2))
        if(input$choice=="Box Plot"){ggboxplot(data_frame,  x = "variable", y = "value",
                                               color = "variable", palette = c("#00AFBB", "#E7B800"), fill = "variable",
                                               order = c(colnames(data)[1], colnames(data)[2]),
                                               ylab = "Value", xlab = "Groups")}
        
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
    
  })
  
  
  # Wilcox Test Output ----
  output$wilcox_table <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    req(input$choice)
    tryCatch(
      {  raw_data <- read.csv(input$file1$datapath)
      data1 <- raw_data[,1]
      data2 <- raw_data[,2]
      two_sided <- wilcox.test(data1, data2, paired = TRUE, alternative = "two.sided")$p.value
      less <- wilcox.test(data1,data2, paired = TRUE, alternative = "less")$p.value
      greater <- wilcox.test(data1, data2, paired = TRUE, alternative = "greater")$ p.value
      alpha <- 0.05
      p_values <- c(two_sided, less, greater)
      
      
      results <- data.frame("Alternative"=c("Two-sided", "Improvement", "Deterioration"), "P-values"=p_values, "Conclusion"=conclusion(p_values, alpha))
      if(input$choice=="Wilcox Test"){
        print(results)
      }
      
      }
      ,
      error = function(e) {
        stop(safeError(e))
      }
    )
    
    
  })
  

  
}


# Create Shiny app ----
shinyApp(ui, server)

