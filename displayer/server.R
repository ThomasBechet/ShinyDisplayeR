library(shiny)
library(FactoMineR)
# library(factoextra)
# data(tea)

function(input, output, session) {
  # Load table data
  table <- reactive({
    file <- input$csvFile
    ext <- tools::file_ext(file$datapath)

    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))

    dat$dat <- read.table(file$datapath, header=input$header, sep=',')
	
	if (is.null(input$sel_vars)) {
		dat$dat
    } else {
		dat$dat[, input$sel_vars]
    }
  })
  
  #d <- reactive({
   # data
  #})
  
  dat <- reactiveValues(dat=NULL)
  observe({
    #dat$dat <- d()
  })

  pca <- reactive({
	listIndexes <- c()
	for(i in 1:length(input$sel_vars)) {
		for(j in 1:length(colnames(dat$dat))) {
			if (!is.null(colnames(dat$dat)[j]) & !is.null(input$sel_vars[i])) {
				if (colnames(dat$dat)[j] == input$sel_vars[i]) {
					listIndexes <- append(listIndexes, j)
				}
			}
		}
	}
	if (is.null(dat$dat)) {
		PCA(table()[,2:10], scale.unit=TRUE, ncp=5, graph=F)
	} else {
		PCA(dat$dat[,listIndexes], scale.unit=TRUE, ncp=5, graph=F)
	}
  })
  hcpc <- reactive({
    HCPC(pca(), nb.clust=-1 , order=TRUE, graph=F)
  })

  # Select columns
  output$sel_vars <- renderUI({
	selected <- c()
	for(colname in colnames(dat$dat)) {
		if (colname != 'X' & colname != 'Rank' & colname != 'Points' & colname != 'Competition') {
			selected <- append(selected, colname)
		}
	}
  
	selectizeInput(
		  "sel_vars",
		  "Colonnes pour le calcul :",
		  choices = colnames(dat$dat),
		  multiple = TRUE,
		  selected = selected
		)
   # checkboxGroupInput("checkboxes", "Selected columns", choices=tab[1,2:length(tab[0,])])
  })

  # Displayed table 
  output$table <- renderTable({
    dat$dat
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