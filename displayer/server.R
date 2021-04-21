library(shiny)
library(FactoMineR)
library(factoextra)
data(tea)

function(input, output, session) {
  # Load table data
  table <- reactive({
    file <- input$csvFile
    ext <- tools::file_ext(file$datapath)

    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))

    read.table(file$datapath, header=input$header, sep=',')
  })

  pca <- reactive({
    PCA(table()[,2:10], scale.unit=TRUE, ncp=5, graph=F)
  })
  hcpc <- reactive({
    HCPC(pca(), nb.clust=-1 , order=TRUE, graph=F)
  })

  # Select columns
  output$checkboxes <- renderUI({
    tab = table()
    checkboxGroupInput("checkboxes", "Selected columns", choices=tab[1,2:length(tab[0,])])
  })

  # Displayed table 
  output$table <- renderTable({
    table()
  })

  # Plot0
  output$plot0 <- renderPlot({
    if (input$method == "ACP") {
      plot.PCA(pca(), choix="ind")
    } else if (input$method == "CAH") {
      plot.HCPC(hcpc(), choice="tree")
    }
  })

  # Plot 0 title
  output$plot0_name <- renderText({
    if (input$method == "ACP") {
      "Individus"
    } else if (input$method == "CAH") {
      "Arbre hiéarchique"
    }
  })

  # Plot1
  output$plot1 <- renderPlot({
    if (input$method == "ACP") {
      plot.PCA(pca(), choix="var")
    } else if (input$method == "CAH") {
      plot.HCPC(hcpc(), choice="map")
    }
  })

  # Plot 1 title
  output$plot1_name <- renderText({
    if (input$method == "ACP") {
      "Variables"
    } else if (input$method == "CAH") {
      "Carte des facteurs"
    }
  })
}