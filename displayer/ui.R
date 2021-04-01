library(shiny)

fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("csvFile", "Choose CSV File", accept = ".csv"),
      checkboxInput("header", "Header", TRUE),
      checkboxInput("graph", "Graph", TRUE)
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
        tabPanel("Plot", imageOutput("image")),
        tabPanel("Summary", tableOutput("table"))
      )
    )
  )
)