library("shiny")
library("ggplot2")
library("extrafont")
library("deSolve")

# Must be saved in UTC-8 to show accents correctly.

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
                             min = 1, max = 100, value = 50),
                 sliderInput("N02Comp", "N02",
                                          min = 1, max = 100, value = 50),
                 numericInput("r1Comp", "r1",
                             min = -5, max = 5, value = 1, step = 0.01),
                 numericInput("r2Comp", "r2",
                             min = -5, max = 5, value = 0.1, step = 0.01),
                 numericInput("K1Comp", "K1", min = 1, max = 500, value = 100, step = 1),
                 numericInput("K2Comp", "K2", min = 0, max = 3, value = 50, step = 1),
                 numericInput("alphaComp", "alpha", min = 0, max = 3, value = 0.5, step = 0.005),
                 numericInput("betaComp", "beta", min = 0, max = 0.1, value = 0.5, step = 0.005),
                 numericInput("a11Comp", "a11", min = 0, max = 0.1, value = 0.01, step = 0.005),
                 numericInput("a22Comp", "a22", min = 0, max = 0.1, value = 0.01, step = 0.005),
                 sliderInput("tComp", "t",
                             min = 1, max = 300, value = 100)
                 
    ),
    mainPanel(plotOutput("compPlot"))

  ),
  
  br(),
  sidebarLayout(
    sidebarPanel("Usa os mesmos valores do gráfico de competição."),
    mainPanel(plotOutput("compIsoPlot"))
    
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
    df
    #note: the minimum possible R value in real life is -1
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
    ggtitle("Crescimento Exponencial Cont??nuo") +
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
      ggtitle("Crescimento Log??stico") +
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
  #using deSolve package
  #code taken from A Primer of Ecology With R
  t3 <- c(0:300)
  df3 <- data.frame(t3)
  
  Compdf <- reactive({
    df3 <- df3[df3$t3 %in% seq(from=0,to=input$tComp, by=1),]
    a <- as.vector(df3)
    #print(df3) #debugging
    
    # a11 and a22 aren't given directly by the input, but calculated from K
    # TODO See how to interchange easily between using K and a values
    parms <- c(r1 = input$r1Comp, r2 = input$r2Comp, a11 = input$a11Comp,
               beta = input$betaComp, a22 = input$a22Comp, alpha = input$alphaComp) #named vector of doubles
    
    initialN <- c(input$N01Comp,input$N02Comp)
    
    lvcomp2 <- function(t, n, parms) {
      with(as.list(parms), {
        dn1dt <- r1 * n[1] * (1 - a11 * n[1] - alpha * n[2])
        dn2dt <- r2 * n[2] * (1 - a22 * n[2] - beta * n[1])
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
      ggtitle("Competição Interespec??fica") +
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
  
 #--------- COMPETITION ISOCLINES
  # Uses the same input from the Competition Plot to generate the correspondent isoclines.

  xy <- reactiveValues() # This is used to create reactive values from anywhere that can be accessed
  # through reactive functions. Ex:
  # xy$a <- 1 (from anywhere inside the server)
  # print(xy$a) (from a reactive context)
 
  Isodf <- reactive({
    
    k1 <- input$K1Comp
    k2 <- input$K2Comp
    
    # xy$lim gets the max number of points any isocline will need (as the greatest value on
    # either x or y axis and uses it both as the number of points to be calculed by the function
    # and as the plot area limits.
    
    xy$lim <- max(c(input$K1Comp/input$alphaComp, input$K2Comp, input$K2Comp/input$betaComp,
                  input$K1Comp))
    print("xy test:")
    print(xy$lim)
    
    npoints <- 0:xy$lim


    # Uses it to create the appropriately sized dataframe.
    df4 <- data.frame(Nmax = npoints)

    
    N1Iso <- input$K1Comp - npoints * input$alphaComp  #would be N2 in the formula
    N2Iso <- input$K2Comp - npoints * input$betaComp   #would be N1 in the formula
    #print(N1Iso)
    df4$Iso1 <- N1Iso
    df4$Iso2 <- N2Iso
    
    df4$color1 <- rep("N1", times = np + 1)
    df4$color2 <- rep("N2", times = np + 1)
    
    #xylim <- tail(npoints,1)
    #print("xylim:")
    #print(xylim)
    
    print(head(df4[,1:3])) 
    print(tail(df4[,1:3])) 
    #print(head(df4[,1:3])) #prints N1Iso and N2Iso cols
    
    
    df4
  })
  
  # renders the COMPETITION ISOCLINES plot
  output$compIsoPlot<-renderPlot({
    ggplot(Isodf(), aes(x = Iso1, y = Nmax,
                        color = color1)) + #N1 iso
      geom_line(size = 1.2) +
      geom_line(aes(x = Nmax, y = Iso2,
                    color = color2), #N2 iso
                size = 1.2) + 
      theme_bw() +
      ggtitle("Isoclinas") +
      xlab("N1") +
      theme_bw() +
      ylab("N2") +
      xlim(0,xy$lim) +
      ylim(0, xy$lim) +
      theme(plot.title = element_text(size = 24, hjust = 0.5,
                                      family = "Calibri", face = "bold"),
            axis.title = element_text(size = 20,
                                      family = "Calibri", face = "bold"),
            axis.text = element_text(size = 16,
                                     family = "Calibri", face = "bold"),
            legend.text = element_text(size = 16,
                                       family = "Calibri", face = "bold"),
            legend.title = element_blank(),
            axis.line.x = element_line(colour = 'black', size = 0.5, linetype='solid')
      ) #+
      #annotate("text", label = "K1", x = input$K1Comp + xy$lim/20, y = xy$lim/20, size = 6, color = "red") +
      #annotate("text", label = "K1/a", y = (input$K1Comp/input$alphaComp), x = xy$lim/20, size = 6, color = "red") +
      #annotate("text", label = "K2", y = input$K2Comp + xy$lim/20, x = xy$lim/20, size = 6, color = "blue") +
      #annotate("text", label = "K2/B", x = ( (input$K2Comp/input$betaComp) + xy$lim/20), y = xy$lim/20, size = 6, color = "blue")
    
        
    
    # TODO DEIXAR GRAFICO MAIS BONITO
    # COLOCAR ETIQUETAS NAS ISOCLINAS
    # MUDAR OS VALORES NOS EIXOS
    # MARCAR O PONTO DE EQUILIBRIO
    
  })
  

  
#----------------------------------------------------------------------
  
  #--------------------OBSERVE----------------
  #observe({ print(input$priceInput[1]) }) 
  
  #un-comment the last line for testing
  # by using observe you can access
  #a reactive variable (like an input) outside of reactive context (that could be
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

# rtsudio Version 1.1.442
# R version 3.4.3 (2017-11-30) Kite-Eating Tree 