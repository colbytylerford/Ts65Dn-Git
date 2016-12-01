##server.r##
#library(shiny)
#library(DT)
library(shinydashboard)
library(reshape)
library(reshape2)


#Load All Data
#RawData <- read.csv("../RawData.csv")
#BehaviorData <- read.csv("../BehaviorData.csv")
#GenotypeData <- read.csv("../GenotypeData.csv")
#ClassData <- read.csv("../ClassData.csv")
#TreatmentData <- read.csv("../TreatmentData.csv")
library(RCurl)
RawData <- read.csv(url("http://www.colbyford.com/research/RawData.csv"))
BehaviorData <- read.csv(url("http://www.colbyford.com/research/BehaviorData.csv"))
GenotypeData <- read.csv(url("http://www.colbyford.com/research/GenotypeData.csv"))
ClassData <- read.csv(url("http://www.colbyford.com/research/ClassData.csv"))
TreatmentData <- read.csv(url("http://www.colbyford.com/research/TreatmentData.csv"))

datasets <- c("ClassData","BehaviorData","GenotypeData","TreatmentData","RawData")
assumptions <- c("Normal","Not Normal")

function(input, output) {
  # Drop-down selection box for which data set
  output$choosedataset <- renderUI({
    selectInput("dataset", "Choose Your Dataset:", as.list(datasets))
  })

  
  ###########ONE VARIABLE STATISTICS###########
  
  # Check boxes
  output$choosecolumns <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(input$dataset))
      return()
    
    # Get the data set with the appropriate name
    dat <- get(input$dataset)
    colnames <- names(dat)
    
    # Create the checkboxes and select them all by default
    radioButtons("columns", "Choose a column:", 
                       choices  = colnames,
                       selected = colnames[2])
  })
  

  
  # Output the data
  output$datatable <- renderTable({
    # If missing input, return to avoid error later in function
    if(is.null(input$dataset))
      return()
    
    # Get the data set
    dat <- get(input$dataset)
    
    # Make sure columns are correct for data set (when data set changes, the
    # columns will initially be for the previous data set)
    if (is.null(input$columns) || !(input$columns %in% names(dat)))
      return()
    
    # Keep the selected columns
    selectedcol <- dat[, input$columns, drop = TRUE]

    if(is.numeric(selectedcol)){
      output$distplot <- renderPlot({
        dens <- density(na.omit(selectedcol))
        plot(dens, main="Distribution of dataset")
        polygon(dens, col="lightseagreen", border="cadetblue4")
      })
      output$shapiroout <- renderPrint({
        if(shapiro.test(selectedcol)$p.value>0.05){
          paste("Shapiro-Wilks Test for Normality: With a p-value of ",shapiro.test(selectedcol)$p.value,", this variable appears to be normally distributed.")
        } else {
          paste("Shapiro-Wilks Test for Normality: With a p-value of ",shapiro.test(selectedcol)$p.value,", this variable deviates from the normal distribution.")
        }
      })
      output$boxplot1 <- renderPlot({
        boxplot(selectedcol,notch=TRUE,col="gold",main="Boxplot",xlab=colnames(selectedcol),horizontal = TRUE)
      })
    } else {
      output$distplot <- renderPlot({
      })
      output$shapiroout <- renderPrint({
        paste("Please select column with numeric values.")
      })
      output$boxplot1 <- renderPrint({
        paste("Please select column with numeric values.")
      })
    }
    
    # Return first n rows
    head(dat, input$previewrows)
  })
  
  ###########TWO VARIABLE STATISTICS###########
  output$chooseassumption <- renderUI({
    selectInput("assumption","Assume the data is:", as.list(assumptions))
  })
  
  # Check boxes
  output$choosevariable1 <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(input$dataset))
      return()
    
    # Get the data set with the appropriate name
    dat <- get(input$dataset)
    colnames <- names(dat)
    
    # Create the checkboxes and select them all by default
    radioButtons("selvar1", "Choose your 1st variable:", 
                 choices  = colnames,
                 selected = colnames[2])
  })
  
  # Check boxes
  output$choosevariable2 <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(input$dataset))
      return()
    
    # Get the data set with the appropriate name
    dat <- get(input$dataset)
    colnames <- names(dat)
    
    # Create the checkboxes and select them all by default
    radioButtons("selvar2", "Choose your 2nd variable:", 
                 choices  = colnames,
                 selected = colnames[3])
  })
  
  output$meandifferences <- renderPrint({
    if(input$assumption == "Normal"){
      dat <- as.data.frame(get(input$dataset))
      colnames <- names(dat)
      if(t.test(dat[, input$selvar1],dat[, input$selvar2],paired=TRUE,alternative="two.sided")$p.value > 0.05){
        paste("Student's t-Test: At the 0.05 significance level, we FAIL TO REJECT the null hypothesis that there is no difference between the means of",input$selvar1," and ",input$selvar2," . (p-value:",t.test(dat[, input$selvar1],dat[, input$selvar2],paired=TRUE,alternative="two.sided")$p.value,")")
      }else{
        paste("Student's t-Test: At the 0.05 significance level, we REJECT the null hypothesis that there is no difference between the means of",input$selvar1," and ",input$selvar2,". (p-value:",t.test(dat[, input$selvar1],dat[, input$selvar2],paired=TRUE,alternative="two.sided")$p.value,")")
      }
    }else{
      dat <- as.data.frame(get(input$dataset))
      colnames <- names(dat)
      if(wilcox.test(dat[, input$selvar1],dat[, input$selvar2],paired=TRUE,alternative="two.sided")$p.value > 0.05){
        paste("Wilcoxon Signed-Rank Test: At the 0.05 significance level, we FAIL TO REJECT the null hypothesis that there is no difference between the means of",input$selvar1," and ",input$selvar2," . (p-value:",wilcox.test(dat[, input$selvar1],dat[, input$selvar2],paired=TRUE,alternative="two.sided")$p.value,")")
      }else{
        paste("Wilcoxon Signed-Rank Test: At the 0.05 significance level, we REJECT the null hypothesis that there is no difference between the means of ",input$selvar1," and ",input$selvar2," . (p-value:",wilcox.test(dat[, input$selvar1],dat[, input$selvar2],paired=TRUE,alternative="two.sided")$p.value,")")
      }
    }
  })
  
  output$anovaoutput <- renderPrint({
      dat <- as.data.frame(get(input$dataset))
      colnames <- names(dat)
      if(input$dataset == "RawData"){
        paste("Please select a pivoted dataset.")
      }else{
        shapeddata <- melt(dat, id="Protein")
        listdata <- as.list(dat[c(2:ncol(dat))])
      
        if(input$assumption == "Normal"){
          barttest <- bartlett.test(shapeddata$value,shapeddata$variable)
          if(barttest$p.value > 0.05){
            paste("Bartlett Test: With a p-value of ",barttest$p.value,", variances are NOT significantly different from each other.")
          }else{
            paste("Bartlett Test: With a p-value of ",barttest$p.value,", variances ARE significantly different from each other.")
          }
        }else{
          krustest <- kruskal.test(listdata)
          if(krustest$p.value > 0.05){
            paste("Kruskal-Wallis Test: With a p-value of ",krustest$p.value,", we FAIL TO REJECT the null hypothesis that the mean ranks are equal among groups.")
          }else{
            paste("Kruskal-Wallis Test: With a p-value of ",krustest$p.value,", we REJECT the null hypothesis that the mean ranks are equal among groups.")
          }
      }

    }
  })

  
  ###########PREDICTIVE ANALYTICS###########
  #Linear Regression
  output$choosegene <- renderUI({
    genes <- Filter(is.numeric, RawData)
    selectInput("geneselection", "Choose the Gene to Predict:", colnames(genes))
  })
  
  require(graphics)
  output$lmplot <- renderPlot({
    gene <- subset(RawData,select=(input$geneselection))
    categories <- Filter(is.factor, RawData)
    categories$MouseID <- NULL
    categories$class <- NULL
    genename <- gsub(" ","",paste("RawData$",colnames(gene[1])))
    categorynames <- gsub(" ","",paste("RawData$",colnames(categories)))
    categoryplus <- paste(categorynames,collapse="+")
    formula <- as.formula(paste(genename,"~",paste(categorynames,collapse="+")))
    fit <- lm(formula,data=RawData)
    pred <- predict(fit,gene)
    cordata <- as.data.frame(cbind(gene,pred))
    colnames(cordata) <- c("Actual","Prediction")
    cortest <- cor.test(cordata$Actual,cordata$Prediction)
    output$lmformula <- renderPrint({
      paste("Formula: ",formula[2],"=",formula[3])
    })
    output$lmstats <- renderPrint({
      paste("R-Squared: ",summary(fit)$r.squared)
    })
    output$coeftable <- renderTable({
      #Variable <- rownames(as.data.frame(fit$coefficients))
      Variable <- c("(Intercept)","Genotype","Treatment","Behavior")
      Coefficients <- summary(fit)$coefficients
      cbind(Variable,Coefficients)
    })
    output$cortest <- renderPrint({
      if(cortest$p.value > 0.05){
        paste("Pearson Correlation: With a p-value of ",cortest$p.value, "and a correlation coefficient of ", cortest$estimate,". The prediction and actual values for the gene are NOT correlated and we FAIL TO REJECT the null hypothesis that the true correlation is equal to zero.")
      }else{
        paste("Pearson Correlation: With a p-value of ",cortest$p.value, "and a correlation coefficient of ", cortest$estimate,". The prediction and actual values for the gene ARE correlated and we REJECT the null hypothesis that the true correlation is equal to zero.")
      }
    })
    plot(cordata$Actual,cordata$Prediction,main="Correlation Scatterplot", xlab = input$geneselection, ylab = "Prediction")
    abline(lm(cordata$Actual~cordata$Prediction),col="red")
  })
  
  #Decision Tree
  library(rpart)
  output$choosecategory <- renderUI({
    categories <- Filter(is.factor, RawData)
    categories$MouseID <- NULL
    selectInput("categoryselection", "Choose the Class to Predict:", colnames(categories))
  })
  

  
  output$dtplot <- renderPlot({
    genes <- Filter(is.numeric, RawData)
    category <- subset(RawData,select=(input$categoryselection))
    categoryname <- gsub(" ","",paste("RawData$",colnames(category[1])))
    genenames <- gsub(" ","",paste("RawData$",colnames(genes)))
    formula <- as.formula(paste(categoryname,"~",paste(genenames,collapse="+")))
    tree <- rpart(formula,data=RawData,method="class")
    title <- paste("Classification Tree for Predicting",input$categoryselection)
    plot(tree,uniform=TRUE, 
         main=title)
    text(tree,use.n=TRUE, all=TRUE, cex=.8)
    output$dtcvplot <- renderPlot({
      plotcp(tree)
    })
  })
  
}