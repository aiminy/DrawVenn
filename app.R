  library(shiny)
  library(Vennerable)

    ui <- fluidPage(

      sliderInput("obs", "total number of peaks",
                  min = 3000, max = 40000, value = 35000
      ),

       sliderInput("x", "RAD21_x",
                   min = -90, max = 90, value = -80
       ),

       sliderInput("xx", "6025_x",
                   min = -90, max = 90, value = 90
       ),

       sliderInput("yy", "6025_y",
                   min = -90, max = 90, value = -35
       ),

       sliderInput("x2", "3803_x",
                   min = -90, max = 90, value =-90
       ),

      sliderInput("y2", "3803_y",
                  min = -50, max = 50, value = -35
      ),

      sliderInput("x3", "SMC1A_x",
                  min = -120, max = 120, value =110
      ),

      sliderInput("y3", "SMC1A_y",
                  min = -120, max = 120, value = -100
      ),

      mainPanel(
        plotOutput("distPlot")
    )


    )

    # Server logic
    server <- function(input, output) {
      output$distPlot <- renderPlot({

        BB=c(1:6153,6154:25736)

        CC=c(6154:25736,25737:29579)

        AA=c(6026:6153,6154:8462,25737:25776,29650:29719)

        Vstem <- Venn(list(ASXL1=AA,SMC1A=BB,RAD21=CC))

        #SetLabels <- VennGetSetLabels(Vstem)

        Cstem3 <- compute.Venn(Vstem,doWeights=TRUE)

        SetLabels <- VennGetSetLabels(Cstem3)
        # FaceLabels <- VennGetFaceLabels(Cstem3)
        #
        # FaceLabels[FaceLabels$FaceName=="010","x"] <- input$obs
        # FaceLabels[FaceLabels$FaceName=="010","y"] <- 3
        #
        # SetLabels[SetLabels$Label=="SMC1A","hjust"] <- "right"
         SetLabels[SetLabels$Label=="SMC1A","x"] <- input$x3
         SetLabels[SetLabels$Label=="SMC1A","y"] <- input$y3
        #
          SetLabels[SetLabels$Label=="RAD21","x"] <- input$x
        # SetLabels[SetLabels$Label=="RAD21","y"] <- 58.78653
        #
        # SetLabels[SetLabels$Label=="ASXL1","y"] <-80
        #
          Cstem3 <- VennSetSetLabels(Cstem3,SetLabels)
        #
        # Cstem3<-VennSetFaceLabels(Cstem3,FaceLabels)
        #
         Cstem3@FaceLabels[which(Cstem3@FaceLabels$FaceName=="010"),]$x<-input$xx
         Cstem3@FaceLabels[which(Cstem3@FaceLabels$FaceName=="010"),]$y<-input$yy
        #
         Cstem3@FaceLabels[which(Cstem3@FaceLabels$FaceName=="001"),]$x<-input$x2
         Cstem3@FaceLabels[which(Cstem3@FaceLabels$FaceName=="001"),]$y<-input$y2
        #
        # Cstem3@FaceLabels[which(Cstem3@FaceLabels$FaceName=="101"),]$x<--18
        # #Cstem3@FaceLabels[which(Cstem3@FaceLabels$FaceName=="101"),]$y<--
        # Cstem3@FaceLabels[which(Cstem3@FaceLabels$FaceName=="101"),]$vjust<-"center"
        #
        # Cstem3@FaceLabels[which(Cstem3@FaceLabels$FaceName=="110"),]$x<-19
        # Cstem3@FaceLabels[which(Cstem3@FaceLabels$FaceName=="110"),]$y<-49.86767
        #
         c.3.set=sum(Cstem3@IndicatorWeight[,4])
         Cstem3@IndicatorWeight[which(row.names(Cstem3@IndicatorWeight)=="000"),4]=input$obs-c.3.set
        #
        # Cstem3@FaceLabels[which(Cstem3@FaceLabels$FaceName=="DarkMatter"),]$y<--130
        #
        # temp<-ol$venn_cnt
        #
        # index.A=grep(colnames(Cstem3@IndicatorWeight)[1],colnames(temp))
        # index.B=grep("SMC1",colnames(temp))
        # index.C=grep("Rad21",colnames(temp))
        #
        # temp2<-as.data.frame(temp[,c(index.A,index.B,index.C,4)])
        # colnames(temp2)[1:3]=colnames(Cstem3@IndicatorWeight)[1:3]
        #
        # row.names(temp2)<-with(temp2,paste0(temp2$ASXL1,temp2$SMC1A,temp2$RAD21))
        #
        # row.names(temp2)<-row.names(Cstem3@IndicatorWeight)
        #
        # temp3<-merge(Cstem3@IndicatorWeight,temp2,by=0,sort=FALSE)
        #
        # Cstem3@IndicatorWeight[,4]<-temp3$Counts
        #
        # c.3.set=sum(Cstem3@IndicatorWeight[,4])
        #
        # Cstem3@IndicatorWeight[which(row.names(Cstem3@IndicatorWeight)=="000"),4]=35000-c.3.set
        #
        #grid.newpage()

        plot(Cstem3)





        #hist(rnorm(input$obs))
      })
    }

    # Complete app with UI and server components
    shinyApp(ui, server)




  #
  # This is a Shiny web application. You can run the application by clicking
  # the 'Run App' button above.
  #
  # Find out more about building applications with Shiny here:
  #
  #    http://shiny.rstudio.com/
  #



  # # Define UI for application that draws a histogram
  # ui <- shinyUI(fluidPage(
  #
  #    # Application title
  #    titlePanel("Draw venn"),
  #
  #    # Sidebar with a slider input for number of bins
  #    sidebarLayout(
  #       sidebarPanel(
  #          sliderInput("bins",
  #                      "Number of bins:",
  #                      min = 1,
  #                      max = 50,
  #                      value = 30)
  #       ),
  #
  #       # Show a plot of the generated distribution
  #       mainPanel(
  #          plotOutput("distPlot")
  #       )
  #    )
  # ))
  #
  # # Define server logic required to draw a histogram
  # server <- shinyServer(function(input, output) {
  #
  #    output$distPlot <- renderPlot({
  #       # generate bins based on input$bins from ui.R
  #       x    <- faithful[, 2]
  #       bins <- seq(min(x), max(x), length.out = input$bins + 1)
  #
  #       # draw the histogram with the specified number of bins
  #       hist(x, breaks = bins, col = 'darkgray', border = 'white')
  #    })
  # })
  #
  # # Run the application
  # shinyApp(ui = ui, server = server)
