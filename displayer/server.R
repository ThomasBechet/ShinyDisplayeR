library(shiny)
library(FactoMineR)
library(factoextra)

function(input, output, session) {
  output$table <- renderTable({
    file <- input$csvFile
    ext <- tools::file_ext(file$datapath)

    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))

    read.csv(file$datapath, header = input$header)
  })

  output$image <- renderImage({
    file <- input$csvFile
    ext <- tools::file_ext(file$datapath)

    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))

    tab <- read.table(file$datapath, header=TRUE, sep=',')
    res.pca = PCA(tab[,2:10], scale.unit=TRUE, ncp=5, graph=F)

    size = session$clientData$output_image_width / 2
    filename = "output.jpg"
    jpeg(filename, width=size, height=size)
    if (input$graph == TRUE) {
      print(fviz_pca_var(res.pca))
    } else {
      print(fviz_pca_ind(res.pca))
    }
    dev.off()

    list(src = filename)
  }, deleteFile=TRUE)
}