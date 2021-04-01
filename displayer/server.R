library(shiny)
library(FactoMineR)
library(factoextra)

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
  ca <- reactive({
    CA(table()[,2:10], graph=F)
  })
  hcpc <- reactive({
    res.mfa = MFA(table()[,2:10], graph=F)
    HCPC(res.mfa)
  })

  # Displayed table 
  output$table <- renderTable({
    table()
  })

  # Plot0
  output$plot0 <- renderPlot({
    if (input$method == "ACP") {
      plot.PCA(pca(), choix="ind")
    } else if (input$method == "AFC") {
      plot.CA(ca())
    } else if (input$method == "CAH") {
      plot.HCPC(hcpc())
    }
  })

  # Plot1
  output$plot1 <- renderPlot({
    if (input$method == "ACP") {
      plot.PCA(pca(), choix="var")
    } else if (input$method == "AFC") {
      # plot.CA(ca())
    } else if (input$method == "CAH") {
      # plot.HCPC(hcpc())
    }
  })
}