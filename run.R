library(shiny)

data.input <- data.frame(
  Category1 = rep(letters[1:3], each = 15),
  Info = paste("Text info", 1:45),
  Category2 = sample(letters[15:20], 45, replace = T),
  Size = sample(1:100, 45),
  MoreStuff = paste("More Stuff", 1:45)
)
shiny::runApp("displayer")