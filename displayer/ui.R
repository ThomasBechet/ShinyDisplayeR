library(shiny)

fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("csvFile", "Choose a CSV File", accept = ".csv"),
      selectInput("method", "Méthode:", 
        choices=list("ACP", "CAH")),
      checkboxInput("header", "Header", TRUE),
      #checkboxGroupInput(inputId="columns", label="Colonnes sélectionnées", choices=uiOutput("columns_list")),
      uiOutput("sel_vars"),
      width=2
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
        tabPanel(title=uiOutput("plot0_name"), 
          plotOutput("plot0", height="600px")
        ),
        tabPanel(title=uiOutput("plot1_name"),
          plotOutput("plot1", height = "600px")
        ),
        tabPanel("Summary",
          tableOutput("table")
        )
      )
    )
  )
)