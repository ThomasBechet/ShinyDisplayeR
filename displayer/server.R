library(shiny)
library(FactoMineR)

function(input, output, session) {

  ###############################
  #        DATA SECTION         #
  ###############################

  # Load table data
  table <- reactive({
    # Retrieve the file name
    file <- input$csvFile
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))

    # Load the table
    dat <- read.table(file$datapath, header=input$header, sep=',')
    dat
  })

  # Compute the Principal Component Analysis (ACP)
  pca <- reactive({
    # Get selected columns
    listIndexes <- c()
    dat <- table()
    for(i in 1:length(input$sel_vars)) {
      for(j in 1:length(colnames(dat))) {
        if (!is.null(colnames(dat)[j]) & !is.null(input$sel_vars[i])) {
          if (colnames(dat)[j] == input$sel_vars[i]) {
            listIndexes <- append(listIndexes, j)
          }
        }
      }
    }

    # Compute the PCA from selected columns
    PCA(dat[,listIndexes], scale.unit=TRUE, ncp=5, graph=F)
  })

  # Compute the hierarchical ascendant clustering
  hcpc <- reactive({
    HCPC(pca(), nb.clust=-1 , order=TRUE, graph=F) # Use the PCA
  })

  ###############################
  #       DISPLAY SECTION       #
  ###############################

  # Select columns
  output$sel_vars <- renderUI({
    selected <- c()
    dat <- table()
    for(colname in colnames(dat)) {
      # Remove useless column by default (better alternative ?)
      if (colname != 'X' & colname != 'Rank' & colname != 'Points' & colname != 'Competition') {
        selected <- append(selected, colname)
      }
    }
  
    # Return selector component
	  selectizeInput(
		  "sel_vars",
		  "Colonnes sélectionnées :",
		  choices = colnames(dat),
		  multiple = TRUE,
		  selected = selected
		)
  })

  # Displayed table ('Données' tab)
  output$table <- renderTable({
    dat <- table()
    dat
  })

  # First plot
  output$plot0 <- renderPlot({
    if (input$method == "ACP") {
      plot.PCA(pca(), choix="ind")
    } else if (input$method == "CAH") {
      plot.HCPC(hcpc(), choice="tree")
    }
  })

  # First plot title ('Individus' or 'Arbre hiérarchique')
  output$plot0_name <- renderText({
    if (input$method == "ACP") {
      "Individus"
    } else if (input$method == "CAH") {
      "Arbre hiérarchique"
    }
  })

  # Second plot
  output$plot1 <- renderPlot({
    if (input$method == "ACP") {
      plot.PCA(pca(), choix="var")
    } else if (input$method == "CAH") {
      plot.HCPC(hcpc(), choice="map")
    }
  })

  # Second plot title ('Variables' or 'Carte des facteurs')
  output$plot1_name <- renderText({
    if (input$method == "ACP") {
      "Variables"
    } else if (input$method == "CAH") {
      "Carte des facteurs"
    }
  })
}