library(shiny)

fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("csvFile", "Choose a CSV File", accept = ".csv"),
      selectInput("method", "Méthode:", 
        choices=list("ACP", "AFC", "CAH", "k-means")),
      checkboxInput("header", "Header", TRUE),
      checkboxInput("graph", "Graph", TRUE),
      width=2
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
        tabPanel("Plot", splitLayout( 
          plotOutput("plot0"),
          plotOutput("plot1")
        )),
        tabPanel("Summary", tableOutput("table"))
      )
    )
  )
)