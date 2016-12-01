#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("DT")
#install.packages("rpart")
#install.packages("reshape2")
library(shiny)
library(shinydashboard)

runApp("Ts65DnApp")

#rsconnect::setAccountInfo(name='colbytylerford', token='324E96B5C29C12415746640C89856527', secret='m6yGX8HHBHTHJcqDJcPjqBPRFVsighQiLDwQxTx3')
#library(rsconnect)
#rsconnect::deployApp("Ts65DnApp")
#runApp("Ts65DnApp",display.mode = "showcase")