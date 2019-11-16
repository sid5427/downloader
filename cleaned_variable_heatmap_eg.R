library(d3heatmap)
library(RColorBrewer)
library(shiny)
library(shinythemes)
library(reprex)
library(dplyr)


data<-read.csv("example_matrix_for_heatmap.txt", header=TRUE, row.names = 1, sep="\t")
rownames(data)
nrow(data)
dim(data)

new_data_matrix <- data.frame(rownames(data))

colnames <- c("col_group_1","col_group_2","col_group_3","col_group_4","col_group_5")

####ui####
ui<-fluidPage(
  titlePanel("example_heatmap"), 
  theme=shinytheme("cerulean"),
  
  sidebarPanel(
    sliderInput("obs",
                "Number of observations:",
                min = 1,
                max = nrow(data),
                value = nrow(data)),
    tableOutput("values"),
    
    #group of checkboxes
    checkboxGroupInput("checkGroup", 
                       label = h3("columns to select"),
                       choices = colnames,
                       selected = colnames)
  ),
  
  mainPanel(
    d3heatmapOutput("heatmap", 
                    height="1200px", width="80%")
  ),
  
  
  fluidRow(column(3, verbatimTextOutput("value")))
)

####server####
server <- function(input, output) 
{
  output$value <- renderPrint({ input$checkGroup })
  
  observeEvent(input$checkGroup,{
    if("col_group_1" %in% input$checkGroup){
      print("col_group_1") ##debuging
      new_data_matrix <- cbind(new_data_matrix,data[,1:4])
    }
    if("col_group_2" %in% input$checkGroup ){
      print("col_group_2") ##debuging
      new_data_matrix <- cbind(new_data_matrix,data[,5:8])
    }
    if("col_group_3" %in% input$checkGroup ){
      print("col_group_3") ##debuging
      new_data_matrix <- cbind(new_data_matrix,data[,9:12])
    }
    if("col_group_4" %in% input$checkGroup ){
      print("col_group_4") ##debuging
      new_data_matrix <- cbind(new_data_matrix,data[,13:16])
    }
    if("col_group_5" %in% input$checkGroup ){
      print("col_group_5") ##debuging
      new_data_matrix <- cbind(new_data_matrix,data[,17:20])
    }
    dim(new_data_matrix) ##debuging
  })
  
  output$heatmap <- renderD3heatmap({
    d3heatmap(new_data_matrix[1:input$obs,2:ncol(new_data_matrix)],
              col=brewer.pal(9,"Reds"),
              scale="none")}
  )
}

shinyApp(ui, server)
