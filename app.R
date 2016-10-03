  library(shiny)
  library(Vennerable)

    ui <- fluidPage(

    titlePanel("ChipSeq peak overlap test"),
    
      sidebarLayout(
        
        sidebarPanel(
          helpText("Peak overlap test"),
          
      sliderInput("obs", "total number of peaks",
                  min = 3000, max = 40000, value = 32044
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
                  min = -120, max = 120, value = -100)
      ),
      mainPanel(
        textOutput("text1"),
        plotOutput("distPlot")
    )
))
    # Server logic
    server <- function(input, output) {
  
      pvalue<-reactive({
        
        BB=c(1:6153,6154:25736)
        
        CC=c(6154:25736,25737:29579)
        
        AA=c(6026:6153,6154:8462,25737:25776,29650:29719)
        
        Vstem <- Venn(list(ASXL1=AA,SMC1A=BB,RAD21=CC))
        
        #SetLabels <- VennGetSetLabels(Vstem)
        
        Cstem3 <- compute.Venn(Vstem,doWeights=TRUE)  
        
        a=Cstem3@IndicatorWeight[,4][8]+Cstem3@IndicatorWeight[,4][7]
        b=Cstem3@IndicatorWeight[,4][3]
        c=Cstem3@IndicatorWeight[,4][5]
        d=input$obs-(a+b+c)
        
        
        ctb<-matrix(c(a,b,c,d),nrow = 2,dimnames =list(c("In", "Out"),c("In", "Out")))
        print(ctb)
        
        if(d>0){
        re<-fisher.test(ctb, alternative='greater')[c("p.value","estimate")]
        re<-re$p.value
        }else
        {
        re="Total number of peaks is too small"  
        }
        
        return(re)
        
      }
      )
      
      output$text1 <- renderText({
       #cat("Fisher exact test:\n") 
       paste("Fisher exact test","\n",pvalue()) 
      })
      
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
         
         #print(Cstem3@IndicatorWeight[,4])
         #print(Cstem3@IndicatorWeight[,4][8])
         #print(input$obs)
         
         a=Cstem3@IndicatorWeight[,4][8]+Cstem3@IndicatorWeight[,4][7]
         b=Cstem3@IndicatorWeight[,4][3]
         c=Cstem3@IndicatorWeight[,4][5]
         d=input$obs-(a+b+c)
         
         ctb<-matrix(c(a,b,c,d),nrow = 2,dimnames =list(c("In", "Out"),c("In", "Out")))
         
         #re<-fisher.test(ctb)
         #ctb3
         #fisher.test(ctb2, alternative='greater')[c("p.value","estimate")]
         
         re<-fisher.test(ctb, alternative='greater')[c("p.value","estimate")]
         
         print(re)
         
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
        #grid.text("test",-80,58)

        #hist(rnorm(input$obs))
      })
    }

    # Run the application
    shinyApp(ui, server)
