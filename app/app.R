library("shiny")
library("ggplot2")
library("extrafont")
library("deSolve")

ui <- fluidPage(
  titlePanel("A Primer of Ecology With Shiny ;p", "Nathalia's shiny app"),
  br(),
  sidebarLayout(
    sidebarPanel(sliderInput("N0GeoIn", "N0",
                             min = 1, max = 100, value = 5),
                 sliderInput("RGeoIn", "R",
                             min = -1, max = 3, value = 0.1, step = 0.01),
                 sliderInput("tGeoIn", "t",
                             min = 0, max = 100, value = 50)
    ),
    mainPanel(plotOutput("geoGplot"))
  ),
  
  sidebarLayout(
    sidebarPanel(sliderInput("N0ExpIn", "N0",
                           min = 1, max = 100, value = 1),
               sliderInput("rExpIn", "r",
                           min = -3, max = 3, value = 0.1, step = 0.01),
               sliderInput("tExpIn", "t",
                           min = 0, max = 100, value = 50)),
    mainPanel(plotOutput("expGPlot")) #default is expGPlot
  ),
  
  br(),
  sidebarLayout(
    sidebarPanel(sliderInput("N0LogIn", "N0",
                             min = 1, max = 100, value = 1),
                 sliderInput("rLogIn", "r",
                             min = -3, max = 3, value = 0.2, step = 0.01),
                 sliderInput("tLogIn", "t",
                             min = 0, max = 100, value = 80),
                 sliderInput("kLogIn", "K", min = 0, max = 100, value = 50)
                 ),
    mainPanel(plotOutput("logGPlot")) #default is logGPlot
  ),
  
  br(),
  sidebarLayout(
    sidebarPanel(sliderInput("N01Comp", "N01",
                             min = 1, max = 100, value = 2),
                 sliderInput("N02Comp", "N02",
                                          min = 1, max = 100, value = 1),
                 numericInput("r1Comp", "r1",
                             min = -5, max = 5, value = 1, step = 0.01),
                 numericInput("r2Comp", "r2",
                             min = -5, max = 5, value = 0.1, step = 0.01),
                 numericInput("K1Comp", "K1", min = 1, max = 500, value = 5, step = 1),
                 numericInput("a12Comp", "alpha", min = 0, max = 3, value = 0.01, step = 0.005),
                 numericInput("K2Comp", "K2", min = 0, max = 3, value = 50, step = 1),
                 numericInput("a21Comp", "beta", min = 0, max = 0.1, value = 0.1, step = 0.005),
                 sliderInput("tComp", "t",
                             min = 1, max = 300, value = 100)
                 
    ),
    mainPanel(plotOutput("compPlot"))
    )

)#end of UI


server <- function(input, output){
  #----- GEOMETRIC GROWTH
  
  # You can't use 0:input$x. A solution is to create a vector with the possible
  # input values and filter it inside of the reactive data frame creation.
  t <- c(0:100)
  n <- c(0:100)
  df <- data.frame(t,n)
  
  geoGdf <- reactive({
    df <- df[df$t %in% seq(from = 0, to = input$tGeoIn, by = 1),] # Filters for the
    # actual input.
    df$n <- input$N0GeoIn * (1 + input$RGeoIn) ^ df$t
    #print(df) #for debugging
    
#Problem: in real life, when N gets to 0 it sticks to 0 and never changes. The 
#mathematical function alone can go below 0 and back indefinitely.
#Solution:
    hit_zero = FALSE
    for (i in df$n){
      #print(df$n[i])
      #print(i)
      if (i > 0 & hit_zero == F){
        #do nothing
        #print(i)
        #print(df$n[df$n %in% i])
      }else{
        #print(which(df$n %in% i))
        #print(df$n[df$n %in% i])
        #df$n[df$n %in% i] <- 0 #Gets the indexes of values < 0 and uses the
#index to change the values to 0.
        hit_zero = TRUE
      }
      #print(i)
      print(hit_zero)
    }
    print(df)
    df
  })
  
  #renders the GEOMETRIC GROWTH plot
  
  output$geoGplot <- renderPlot({
    ggplot(geoGdf(), aes(x = t, y = n)) +
    geom_line(linetype = "dashed") +
    geom_point(colour='lightsteelblue4', size = 3) +
    theme_bw() +
    ggtitle("Crescimento Geométrico (Em Passos)") +
    xlab("t") +
    ylab("N(t)") +
    theme(plot.title = element_text(size = 24, hjust = 0.5,
                                      family = "Calibri", face = "bold"),
            axis.title = element_text(size = 20,
                                      family = "Calibri", face = "bold"),
            axis.text = element_text(size = 16,
                                     family = "Calibri", face = "bold")
    )
    
  })
  
  
  
  #-----CONTINUOUS EXPONENTIAL GROWTH
  t1 <- c(0:100)
  N1 <- c(0:100)
  df1 <- data.frame(t1,N1)
  
  expGdf <- reactive({
    df1 <- df1[df1$t1 %in% seq(from=0,to=input$tExpIn,by=1),]
    df1$N1 <-  input$N0ExpIn * exp((input$rExpIn * df1$t1)) #exp(r*t1) = e^(rt)
    #print(df1) #only for debugging
    df1
  })

  # renders the EXPONENTIAL GROWTH PLOT
  output$expGPlot<-renderPlot({
    ggplot(expGdf(),aes(x=t1,y=N1)) +
    geom_line(color = "lightsteelblue4") +
    geom_point(colour='lightsteelblue4', size = 3) +
    theme_bw() +
    ggtitle("Crescimento Exponencial Contínuo") +
    xlab("t") +
    ylab("N(t)") +
    theme(plot.title = element_text(size = 24, hjust = 0.5,
                                    family = "Calibri", face = "bold"),
          axis.title = element_text(size = 20,
                                    family = "Calibri", face = "bold"),
          axis.text = element_text(size = 16,
                                   family = "Calibri", face = "bold")
    )
    })

  #-----LOGISTIC (DENSITY-DEPENDENT) GROWTH
  t2 <- c(0:100)
  n2 <- c(0:100)
  df2 <- data.frame(t2,n2)

  
  logGdf <- reactive({
    df2 <- df2[df2$t2 %in% seq(from=0,to=input$tLogIn, by=1),]
    df2$n2 <- input$kLogIn / (1 + ((input$kLogIn - input$N0LogIn) / input$N0LogIn) * exp(-input$rLogIn * df2$t2))
    #print(df2) #only for debugging
    df2
  })
  
  # formula: Nt = ( k / (1 + ((k - n0) / n0) * exp(-r * t)))

  #renders the LOGISTIC GROWTH PLOT
  output$logGPlot<-renderPlot({
    ggplot(logGdf(),aes(x=t2,y=n2)) +
      geom_point(colour='lightsteelblue4', size = 3) +
      geom_hline(yintercept = input$kLogIn, linetype = "dashed", color = "red1") +
      annotate("text", x = input$tLogIn, y = (input$kLogIn - input$kLogIn/20 ),
               label = "K", size = 6, color = "red1") + # o y desce o "K" em 20% do valor para arrumar
      theme_bw() +
      ggtitle("Crescimento Logístico") +
      xlab("t") +
      ylab("N(t)") +
      theme(plot.title = element_text(size = 24, hjust = 0.5,
                                      family = "Calibri", face = "bold"),
            axis.title = element_text(size = 20,
                                      family = "Calibri", face = "bold"),
            axis.text = element_text(size = 16,
                                     family = "Calibri", face = "bold")
      )
  })  

  
  #--------------- INTERESPECIFIC COMPETITION  (with continuous logistic growth)
  t3 <- c(0:300)
  df3 <- data.frame(t3)
  
  Compdf <- reactive({
    df3 <- df3[df3$t3 %in% seq(from=0,to=input$tComp, by=1),]
    a <- as.vector(df3)
    #print(df3) #debugging
    #se quiser trocar para K no input, calcular a11 com 1/K
    
    parms <- c(r1 = input$r1Comp, r2 = input$r2Comp, a11 = 1/input$K1Comp,
               a21 = input$a21Comp, a22 = 1/input$K2Comp, a12 = input$a12Comp) #named vector of doubles
    
    initialN <- c(input$N01Comp,input$N02Comp)
    
    lvcomp2 <- function(t, n, parms) {
      with(as.list(parms), {
        dn1dt <- r1 * n[1] * (1 - a11 * n[1] - a12 * n[2])
        dn2dt <- r2 * n[2] * (1 - a22 * n[2] - a21 * n[1])
        list(c(dn1dt, dn2dt))
      })
    }
    
    out <- ode(y = initialN, times = a, func = lvcomp2, parms = parms)
    
    N1 = rep("N1",length(a))
    N2 = rep("N2",length(a))
    
    #print(data.frame(out, N1, N2))
    result <- data.frame(out, N1, N2) #N1 and N2 are used by the aes function to colour the lines
    #print(head(result)) #debugging
    result
    
    #df3$n3 <- NA
    #print(df3) #only for debugging
    #df3
  })
  
  
  
  
  # renders the COMPETITION PLOT
  output$compPlot<-renderPlot({
    ggplot(Compdf(),aes(x=time,y=X1)) +
      geom_point(aes(colour=N1), size = 2) +
      geom_point(size = 2, aes(x = time, y =X2, colour = N2)) +
      geom_hline(yintercept = input$K1Comp, linetype = "dashed", color = "black") +
      annotate("text", x = input$tLogIn, y = (input$K1Comp - (input$K1Comp)/20 ),
               label = "K1", size = 6, color = "black") +
      geom_hline(yintercept = input$K2Comp, linetype = "dashed", color = "black") +
      annotate("text", x = input$tLogIn, y = (input$K2Comp - (input$K2Comp)/20 ),
               label = "K2", size = 6, color = "black") +
      theme_bw() +
      ggtitle("Competição Interespecífica") +
      xlab("t") +
      ylab("N(t)") +
      theme(plot.title = element_text(size = 24, hjust = 0.5,
                                      family = "Calibri", face = "bold"),
            axis.title = element_text(size = 20,
                                      family = "Calibri", face = "bold"),
            axis.text = element_text(size = 16,
                                     family = "Calibri", face = "bold"),
            legend.text = element_text(size = 16,
                                       family = "Calibri", face = "bold"),
            legend.title = element_blank()
      )
  })
  
  

  
#----------------------------------------------------------------------
  
  #--------------------OBSERVE----------------
  #observe({ print(input$priceInput[1]) }) 
  
  #un-comment the last line for testing
  # by using observe you can access
  #an reactive variable (like an input) outside a reactive context (that could be
  #a render function)
  #this is not mapped to the ui and just prints the value in the console
  
  #-------------------REACTIVE----------------
  #
  #priceDiff <- reactive({
  #  diff(input$priceInput)
  #})
  #observe({print( priceDiff() ) })
  #}
  
  #un-comment the last section for testing
  #By using reactive you can create a reactive variable that does not come
  #directly from input.
  #When printing, you must use observe({}) and refer to the variable name as it
  #was a function with () 
  
  
  
  
  
  
  
  
} #end of server, don't exclude (again)

shinyApp(ui = ui, server = server)