library(shiny)

fluidPage(
  # Sidebar + Main Panel layout
  sidebarLayout(
    # Side panel
    sidebarPanel(
      # File selection
      fileInput("csvFile", "Choix du fichier CSV", accept = ".csv"),
      # Select with or without header
      checkboxInput("header", "Fichier avec en-tête ?", TRUE),
      # Method selection
      selectInput("method", "Méthode:", 
        choices=list("ACP", "CAH")),
      # Column selection
      uiOutput("sel_vars"),
      width=2
    ),
    # Main panel
    mainPanel(
      # Tab layout
      tabsetPanel(type = "tabs",
        # Data tab
        tabPanel("Données",
          tableOutput("table")
        ),
        # First plot tab
        tabPanel(title=uiOutput("plot0_name"), 
          plotOutput("plot0", height="600px")
        ),
        # First plot tab
        tabPanel(title=uiOutput("plot1_name"),
          plotOutput("plot1", height = "600px")
        )
      )
    )
  )
)