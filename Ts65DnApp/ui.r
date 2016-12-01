## ui.R ##
library(shinydashboard)
#library(DT)

dashboardPage(skin="yellow",
  dashboardHeader(title = "Analysis of Memantine on the Ts65Dn Mouse Model", titleWidth = 450),
  dashboardSidebar(
    sidebarMenu(
      uiOutput("choosedataset"),
      menuItem("Introduction", tabName = "intro", icon=icon("info")),
      menuItem("Single-Variable Statistics", tabName = "singvardash", icon = icon("area-chart")),
      menuItem("Two-Variable Statistics", tabName = "twovardash", icon = icon("th")),
      menuItem("Predictive Analytics", tabName = "preddash", icon = icon("line-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "intro",
              fluidRow(
                box(title="Introduction", status = "info",
                    p("When beginning to assess the efficacy of a new pharmacological product, animal models are used as the first phase,", em("in vivo"), "stage."),
                    img(src="https://upload.wikimedia.org/wikipedia/commons/0/0d/%D0%9C%D1%8B%D1%88%D1%8C_2.jpg",width=250),"[3]",
                    p("In this data, ", strong("Memantine"), ", a neurological medication, is being tested on mice to determine its effectiveness against Down Syndrome. Given that Down Syndrome is a human disease, the mouse model Ts65Dn is used. In the Ts65Dn model, imbalances in small chromosome fragments of mouse chromosome 16 are found to be orthologous to about half of the human chromosome 21.[1]"),
                    p("This data was then pivoted (using the melt and cast functions in R) by Class, Behavior, Genotype, and Treatment for anaysis. These pivoted datasets as well as the original, raw data can be selected from the sidebar.")
                    ),
                
                box(title="About the Data", status = "primary",
                    p("The data used in this tool comes from the ", a("UCI Machine Learning Repository",href = "http://archive.ics.uci.edu/ml/datasets/Mice+Protein+Expression"), ", where it was donated by Higuera et. al.[2]"),
                    p("The data set consists of the expression levels of 77 proteins/protein modifications that produced detectable signals in the nuclear fraction of cortex. There are 38 control mice and 34 trisomic mice (Down syndrome), for a total of 72 mice. In the experiments, 15 measurements were registered of each protein per sample/mouse. Therefore, for control mice, there are 38x15, or 570 measurements, and for trisomic mice, there are 34x15, or 510 measurements. The dataset contains a total of 1080 measurements per protein. Each measurement can be considered as an independent sample/mouse."),
                    p("The eight classes of mice are described based on features such as genotype, behavior and treatment. According to genotype, mice can be control or trisomic. According to behavior, some mice have been stimulated to learn (context-shock) and others have not (shock-context) and in order to assess the effect of the drug memantine in recovering the ability to learn in trisomic mice, some mice have been injected with the drug and others have not.")
                )
              ),
              fluidRow(
                box(title="Memantine", status = "warning", img(src="https://upload.wikimedia.org/wikipedia/commons/2/2d/Memantine_ball-and-stick_model.png",width=250),"[4]",
                    br(),
                    p("Memantine is a compound that acts on the glutaminergic system as an N-Methyl-D-aspartate (NMDA) receptor antagonist."),
                    p("This helps to prevent neuronal excitotoxicity, basically preventing burnout due to excessive stimulation.[5]"),
                    p("Memantine is currently marketed by Allergan under the brand Namenda for the treatment of moderate-to severe-Alzheimer's disease.[6]"),
                    img(src="http://www.namendaxr.com/Content/images/logo/namendaxr.png",width=250),"[7]"
                    
                    ),
              
                box(title="References", background = "black", solidHeader = TRUE,
                    p("[1] Olson, L.E., Roper, R.J., Baxter, L.L., Carlson, E.J., Epstein, C.J. and Reeves, R.H. (2004), Down syndrome mouse models Ts65Dn, Ts1Cje, and Ms1Cje/Ts65Dn exhibit variable severity of cerebellar phenotypes. Dev. Dyn., 230: 581-589."),
                    p("[2] Higuera C, Gardiner KJ, Cios KJ (2015) Self-Organizing Feature Maps Identify Proteins Critical to Learning in a Mouse Model of Down Syndrome. PLoS ONE 10(6): e0129126. journal.pone.0129126"),
                    p("[3] By George Shuklin (Own work) [CC BY-SA 1.0 (http://creativecommons.org/licenses/by-sa/1.0)], via Wikimedia Commons"),
                    p("[4] By Vaccinationist (Memantine on PubChem) [CC BY-SA 4.0 (http://creativecommons.org/licenses/by-sa/4.0)], via Wikimedia Commons"),
                    p("[5] Memantine - DrugBank. (n.d.). Retrieved November 20, 2016, from https://www.drugbank.ca/drugs/DB01043"),
                    p("[6] Reisberg, B. et al. N Engl J Med 2003;348:1333-1341"),
                    p("[7] NAMENDA XR. (2016, February). Retrieved November 20, 2016, from http://www.namendaxr.com/")
                )
              )
          ),
              
      tabItem(tabName = "singvardash",
        fluidRow(
          box(solidHeader = TRUE, status = "primary",
              uiOutput("choosecolumns"),
          box(width = NULL,
              title = "Data Preview",
              numericInput("previewrows", "Number of rows to preview:", 10),
              
              div(style = 'overflow: scroll', tableOutput('datatable'))
              #tableOutput("datatable")
          )
          ),
          
          box(title = "Outliers", background = "navy",
              plotOutput("boxplot1",height = 250)
              ),
    
          box(title = "Normality", background = "blue",
          plotOutput("distplot", height = 250),
          tableOutput("shapiroout")
          )
        ),
        fluidRow(

        )
      ),
      
      tabItem(tabName = "twovardash",
              #Differences: Wilcoxon,t-Test
              #Variances: Kruskall-Wallis, Bartlett
              
              fluidRow(
                box(solidHeader = TRUE, status = "primary",
                    uiOutput("chooseassumption"),
                    uiOutput("choosevariable1"),
                    uiOutput("choosevariable2")
                ),
                box(title = "Mean Differences",
                  tableOutput("meandifferences")
                ),
                box(title = "Analysis of Variance",
                    tableOutput("anovaoutput"))
                )
              ),
      tabItem(tabName = "preddash",
              #Linear Regression
              #Decision Tree
            fluidRow(
              box(title = "Linear Regression and Correlation", width = NULL, solidHeader=TRUE, background = "red",
                  uiOutput("choosegene"),
                  tableOutput("lmformula"),
                  tableOutput("lmstats"),
                  tableOutput("coeftable"),
                  tableOutput("cortest"),
                  plotOutput("lmplot",height=500)
              )
            ),
            fluidRow(
              box(title = "Decision Tree", width = NULL, solidHeader=TRUE, background = "green",
                  uiOutput("choosecategory"),
                  plotOutput("dtplot",height=1000),
                  plotOutput("dtcvplot")
              )
            )
      )  
    )
  )
)  
