library(mosaicData)
library(shiny)
library(ggplot2)
library(lattice)
library(latticeExtra)
library(shiny)
library(shinythemes)
library(corrplot)
library(gridExtra)
library(grid)
#install.packages("hrbrthemes")
library(hrbrthemes)

data <- SaratogaHouses
colnames(data)

data$price_milions <- data$price/1000
data$landValue_milions <- data$landValue/1000



####################################################


ui <- tagList(shinythemes::themeSelector(),
  navbarPage("Wykresy zmiennych ze zbioru SaratogaHouses",
                 
                 navbarMenu("Histogramy",
                   tabPanel("Zmiennych iloœciowych", 
                     sidebarLayout(
                     sidebarPanel(
                       selectInput(inputId = "zmienna9",
                                   label="Narysuj histogram dla zmiennej:",
                                   choices = names(data)[c(2:3,5:10,17:18)], selected = "price_milions"),
                       sliderInput("bins9",
                                   "Liczba przedzia³ów:",
                                   min = 1,
                                   max = 50,
                                   value = 30),
                       checkboxInput("show_xlab9", "Poka¿ etykietê osi X", value = TRUE),
                       checkboxInput("show_ylab9", "Poka¿ etykietê osi Y", value=TRUE),
                       checkboxInput("show_title9", "Poka¿ tytu³", value = TRUE),
                       checkboxInput("show_line9", "Poka¿ liniê dla wartoœci œredniej", value = TRUE),
                       textInput("kolor_linii9", "Kolor linii wyznaczaj¹cej œredni¹ wartoœæ:", value = "red"),
                       checkboxInput("show_mean9", "Wypisz na wykresie wartoœæ œredni¹", value = TRUE),
                       numericInput("rozmiar_czcionki9", "Rozmiar czcionki:", value = 1, step=0.2, min=0),
                       numericInput("polozenie_czcionki9", "Wysokoœæ po³o¿enia napisu:", value = 200),
                       textInput("wypelnienie9", "Kolor wype³nienia s³upków:", value = "cadetblue3"),
                       textInput("obramowanie9", "Kolor obramowania s³upków:", value = "cadetblue")
                       
                       
                     ),
                     
                     mainPanel(
                       plotOutput("distPlot9", height = "600px")))),
                   tabPanel("Zmiennych kategoryzuj¹cych",   
                       sidebarLayout(
                       sidebarPanel(
                       selectInput(inputId = "zmienna10",
                                   label="Narysuj histogram dla zmiennej:",
                                   choices = names(data)[c(7:16)]),
                       textInput("wypelnienie10", "Kolor wype³nienia s³upków:", value = "cadetblue3"),
                       sliderInput("transparentnosc10","Poziom krycia wype³nienia s³upków:", value = 1, min=0, max=1),
                       checkboxInput("show_xlab10", "Poka¿ etykietê osi X", value = TRUE),
                       checkboxInput("show_ylab10", "Poka¿ etykietê osi Y", value=TRUE),
                       checkboxInput("show_title10", "Poka¿ tytu³", value = TRUE)
                       
                       
                     ),
                     
                     mainPanel(
                       plotOutput("distPlot10", height = "600px")
                     )
                   )),
                   tabPanel("Dwóch zmiennych", 
                            sidebarLayout(
                              sidebarPanel(
                                selectInput(inputId = "zmienna1_11",
                                            label="Wybierz pierwsz¹ zmienn¹:",
                                            choices = names(data)[c(2:3,5:18)], selected = "age"),
                                
                                selectInput(inputId = "zmienna2_11",
                                            label="Wybierz drug¹ zmienn¹:",
                                            choices = names(data)[c(2:3,5:18)], selected = "fuel"),
                                checkboxInput("show_xlab11", "Poka¿ etykietê osi X", value = TRUE),
                                checkboxInput("show_ylab11", "Poka¿ etykietê osi Y", value=TRUE),
                                checkboxInput("show_title11", "Poka¿ tytu³", value = TRUE),
                                radioButtons("wypelnienie11", "Kolor wype³nienia s³upków:", choices = c("cadetblue", "cadetblue3",  
                                                                                                        "darksalmon", "mistyrose", "darkseagreen1", "ivory3", "grey"))
                              ),
                              
                              
                              mainPanel(
                                plotOutput("distPlot11")
                              )
                            )
                  )),
                 tabPanel("Wykresy pude³kowe",
                            sidebarLayout(
                              sidebarPanel(
                                selectInput(inputId = "zmienna1_13",
                                            label="Wybierz zmienn¹ iloœciow¹:",
                                            choices = names(data)[c(2:3,5:10,17:18)], selected = "price"),

                                selectInput(inputId = "zmienna2_13",
                                            label="Wybierz zmienn¹ kategoryzuj¹c¹:",
                                            choices = names(data)[c(11:16)], selected = ""),
                                checkboxInput("show_xlab13", "Poka¿ etykietê osi X", value = TRUE),
                                checkboxInput("show_ylab13", "Poka¿ etykietê osi Y", value=TRUE),
                                checkboxInput("show_title13", "Poka¿ tytu³", value = TRUE),
                                numericInput("shape13", "Wybierz kszta³t oznaczenia obserwacji odstaj¹cych", min = 0, max = 25, value = 1),
                                radioButtons("kolor_odstajacych13", "Kolor oznaczenia obserwacji odstaj¹cych:", choices = c("red", "black", "blue", "green", "cadetblue", "cadetblue3",  
                                                                                                                            "darksalmon", "mistyrose", "darkseagreen1", "ivory3", "grey"
                                ))
                              ),
                              
                              
                              mainPanel(
                                tabsetPanel(tabPanel("Wykres pude³kowy jednej zmiennej", plotOutput("distPlot2_13")),
                                            tabPanel("Wykres pude³kowy dwóch zmiennych", plotOutput("distPlot13"))
                                )
                              )
                            )
                   
                 ),
                 tabPanel("Wykres gêstoœci zmiennych iloœciowych",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = "zmienna12",
                                          label="Narysuj wykres czêstoœci dla zmiennej:",
                                          choices = names(data)[c(2:3,5:10,17:18)], selected = "price"),
                              sliderInput("bins12",
                                          "Liczba przedzia³ów:",
                                          min = 1,
                                          max = 100,
                                          value = 50),
                              checkboxInput("show_xlab12", "Poka¿ etykietê osi X", value = TRUE),
                              checkboxInput("show_ylab12", "Poka¿ etykietê osi Y", value=TRUE),
                              checkboxInput("show_title12", "Poka¿ tytu³", value = TRUE)
                              
                              
                            ),
                            
                            
                            mainPanel(
                              plotOutput("distPlot12")
                            )
                          )),
                 tabPanel("Wykres f. gêstoœci i sk. f. gêstoœci",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = "zmienna1_14",
                                          label="Narysuj funkcjê gêstoœci zmiennej:",
                                          choices = names(data)[c(2:3,5:10,17:18)], selected = "price_milions"),
                              checkboxInput("show_xlab14", "Poka¿ etykietê osi X", value = TRUE),
                              checkboxInput("show_ylab14", "Poka¿ etykietê osi Y", value=TRUE),
                              checkboxInput("show_title14", "Poka¿ tytu³ dla wykresu jednej zmiennej", value = TRUE),
                              selectInput("punkty14", "Poka¿ obserwacje na wykresie", choices = c(TRUE, FALSE)),
                              selectInput(inputId = "zmienna2_14",
                                          label="Dodaj zmienn¹ kategoryzuj¹c¹:",
                                          choices = names(data)[c(11:16)], selected = "heating"),
                              checkboxInput("show_title214", "Poka¿ tytu³ dla wykresu dwóch zmiennych", value = TRUE),
                              selectInput("typ14", "Wybierz typ wykresu sk. f. gêstoœci", choices = c("l", "p", "s"))
                              #selectInput("leg14", "Poka¿ legendê dla wykresu dwóch zmiennych", choices = c(TRUE, FALSE))
                            ),
                            
                            
                            mainPanel(
                              tabsetPanel(tabPanel("Wykres f. gêstoœci jednej zmiennej", plotOutput("distPlot14")),
                                          tabPanel("Wykres f. gêstoœci dwóch zmiennych", plotOutput("distPlot214")),
                                          tabPanel("Wykres f. gêstoœci dwóch zmiennych - grupy na jednym wykresie", plotOutput("distPlot314")),
                                          tabPanel("Wykres sk. f. gêstoœci jednej zmiennej", plotOutput("distPlot414")),
                                          tabPanel("Wykres sk. f. gêstoœci dwóch zmiennych", plotOutput("distPlot514")),
                                          tabPanel("Wykres sl. f. gêstoœci dwóch zmiennych - grupy na jednym wykresie", plotOutput("distPlot614"))
                              )
                            )
                          )),
                 tabPanel("Wykres korelacji",
                          sidebarLayout(
                            sidebarPanel(

                              
                              h3("Modyfikuj wykres"),
                              checkboxInput("show_title15", "Poka¿ tytu³ wykresu", value = TRUE),
                              selectInput("upper15", "Wybierz metodê prezentacji danych górnego trójk¹ta", 
                                          choices = c("pie", "circle", "square", "ellipse", "number", "shade", "color")),
                              sliderInput("poziom_istotnosci15", "Wybierz posiom istotnoœci", max = 1, min = 0, value = 0.05),
                              numericInput("rozmiar_numery15", "Wybierz rozmiar wartoœci w macierzy", min = 0, value = 0.7, step = 0.05),
                              textInput("kolor_numery15", "Podaj kolor wartoœci w dolnym trójk¹cie macierzy", value = "black"),
                              numericInput("rozmiar_naglowki15", "Wybierz rozmiar nazw zmiennych", min = 0, value = 0.7, step = 0.05),
                              selectInput("polozenie15", "Wybierz po³o¿enie nazw zmiennych", choices = c("z lewej strony macierzy i nad ni¹" = "lt",  "w przek¹tnej macierzy" = "d",
                                                                                                    "brak nag³ówków" = "n"))
                              
                              
                              
                            ),
                            
                            
                            mainPanel(
                              plotOutput("distPlot15", height = "600px")
                            )
                          )
                 ),
                 navbarMenu("Wykresy zale¿noœci dwóch zmiennych",
                            tabPanel("Wykres pairs",
                                     
                                     plotOutput("distPlot2", height = "600px")
                                     
                                     
                            ),
                            
                            tabPanel("Wykres scatterPlot",
                                     
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput(inputId = "zmienna1_7",
                                                     label="Wybierz zmiann¹ x:",
                                                     choices = names(data)[c(2:3,5:18)], selected = "age"),
                                         
                                         selectInput(inputId = "zmienna2_7",
                                                     label="Wybierz zmienn¹ y:",
                                                     choices = names(data)[c(2:3,5:18)], selected = "price_milions"),
                                         checkboxInput("show_xlab7", "Poka¿ etykietê osi X", value = TRUE),
                                         checkboxInput("show_ylab7", "Poka¿ etykietê osi Y", value=TRUE),
                                         checkboxInput("show_main7", "Poka¿ tytu³", value = TRUE)
                                       ),
                                       mainPanel(plotOutput("distPlot7", height = "600px"))
                                     )
                            ) 
                 ,
                            tabPanel("Wykresy plot",
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput(inputId = "zmienna116",
                                                     label="Wybierz zmienn¹ x:",
                                                     choices = names(data)[c(2:3,5:10,17:18)], selected = "price_milions"),
                                         selectInput(inputId = "zmienna216",
                                                     label="Wybierz zmienn¹ y:",
                                                     choices = names(data)[c(2:3,5:10,17:18)], selected = "age"),
                                         selectInput("typ_plot16", "Wybierz typ wykresu;", choices = c("punktowy" = "p", "liniowy" = "l", "punktowo-liniowy" = "b",
                                                                                                       "histogram" = "h")),
                                         sliderInput("pch16", "Wybierz typ punktów wykresu punktowego", min = 1, max = 25, value = NULL),
                                         textInput("kolor_plot16", "Podaj kolor wykresu", value = "black"),
                                         
                                         checkboxInput("show_xlab16", "Poka¿ etykietê osi X", value = TRUE),
                                         checkboxInput("show_ylab16", "Poka¿ etykietê osi Y", value=TRUE),
                                         checkboxInput("show_main16", "Poka¿ tytu³", value = TRUE),
                                         numericInput("jitterx16", "Zastosuj funkcjê jitter dla zmiennej x:", min = 0, value = 0, step = 0.02),
                                         numericInput("jittery16", "Zastosuj funkcjê jitter dla zmiennej y:", min = 0, value = 0, step = 0.02),
                                         checkboxInput("lty_plot16", "Wyznacz liniê regresji dla danych", value=TRUE),
                                         textInput("kolor_abline16", "Podaj kolor linii regresji", value = "steelblue3")
                                         
                                       ),
                                       mainPanel(
                                         plotOutput("distPlot3", height = "600px")
                                         
                                       )
                                       
                                     )
                                     
                            ),
                            tabPanel("Dynamiczny wykres zale¿noœci dwóch zmiennych",
                                     sidebarLayout(
                                       sidebarPanel(
                                         helpText("Aby wyznaczyæ liniê regresji zaznacz wybrane punkty na wykresie."),
                                         h4("Nachylenie linii regresji:"),
                                         textOutput("nachylenie"),
                                         selectInput(inputId = "zmienna_117",
                                                     label="Wybierz zmienn¹ x:",
                                                     choices = names(data)[c(2:3, 5:10,17:18)], selected = "price_milions"),
                                         selectInput(inputId = "zmienna_217",
                                                     label="Wybierz zmienn¹ y:",
                                                     choices = names(data)[c(2:3, 5:10,17:18)], selected = "age"),
                                         checkboxInput("xlab17", "Poka¿ etykietê osi X", value = TRUE),
                                         checkboxInput("ylab17", "Poka¿ etykietê osi Y", value=TRUE),
                                         checkboxInput("main17", "Poka¿ tytu³", value = TRUE)
                                       ),
                                       mainPanel(
                                         plotOutput("distPlot4", height = "600px",
                                                    brush = brushOpts(
                                                      id = "brush1")
                                         )
                                         
                                       )
                                     )
                            ))
                 ,
                 navbarMenu("Wykresy trzech lub czterech zmiennych",
                            tabPanel("Wykres z trzeci¹ zmienn¹ oznaczon¹ kolorem",
                                     
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput(inputId = "zmienna1_5",
                                                     label="Wybierz pierwsz¹ zmienn¹:",
                                                     choices = names(data)[c(2:3,5:18)], selected = "age"),
                                         
                                         selectInput(inputId = "zmienna2_5",
                                                     label="Wybierz drug¹ zmienn¹:",
                                                     choices = names(data)[c(2:3,5:18)], selected = "price_milions"),
                                         
                                         selectInput(inputId = "zmienna3_5",
                                                     label="Wybierz trzeci¹ zmienn¹:",
                                                     choices = names(data)[c(2:3,5:18)], selected = "fuel"),
                                         checkboxInput("show_xlab5", "Poka¿ etykietê osi X", value = TRUE),
                                         checkboxInput("show_ylab5", "Poka¿ etykietê osi Y", value=TRUE),
                                         checkboxInput("show_main5", "Poka¿ tytu³", value = TRUE)
                                       ),
                                       mainPanel(plotOutput("distPlot5", height = "600px"))
                                     )
                            ),
                            
                            tabPanel("Wykres panelowy z trzeci¹ zmienn¹ kategoryzuj¹c¹",
                                     
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput(inputId = "zmienna1_6",
                                                     label="Wybierz pierwsz¹ zmienn¹:",
                                                     choices = names(data)[c(2:3,5:18)], selected = "age"),
                                         
                                         selectInput(inputId = "zmienna2_6",
                                                     label="Wybierz drug¹ zmienn¹:",
                                                     choices = names(data)[c(2:3,5:18)], selected = "price_milions"),
                                         
                                         selectInput(inputId = "zmienna3_6",
                                                     label="Wybierz zmienn¹ kategoryzuj¹c¹:",
                                                     choices = names(data)[c(6:16)], selected = "fuel"),
                                         checkboxInput("show_xlab6", "Poka¿ etykietê osi X", value = TRUE),
                                         checkboxInput("show_ylab6", "Poka¿ etykietê osi Y", value=TRUE),
                                         checkboxInput("show_main6", "Poka¿ tytu³", value = TRUE)
                                       ),
                                       mainPanel(plotOutput("distPlot6", height = "600px"))
                                     )
                            ),
                            
                            tabPanel("Wykres b¹belkowy",
                                     
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput(inputId = "zmienna1_8",
                                                     label="Wybierz pierwsz¹ zmienn¹:",
                                                     choices = names(data)[c(2:3,5:18)], selected = "age"),
                                         
                                         selectInput(inputId = "zmienna2_8",
                                                     label="Wybierz drug¹ zmienn¹:",
                                                     choices = names(data)[c(2:3,5:18)], selected = "price_milions"),
                                         
                                         selectInput(inputId = "zmienna3_8",
                                                     label="Wybierz zmienn¹ oznaczon¹ rozmiarem b¹belka:",
                                                     choices = names(data)[c(2:3,5:18)], selected = "rooms"),
                                         selectInput(inputId = "zmienna4_8",
                                                     label="Wybierz zmienn¹ oznaczon¹ kolorem b¹belka:",
                                                     choices = names(data)[c(2:3,5:18)], selected = "rooms"),
                                         sliderInput("shape8", "Wybierz kszta³t punktów:", min=1, max=25, step = 1, value=21),
                                         numericInput("size8", "Wybierz rozmiar punktów:", min = 0, step = 0.2, value = 3),
                                         checkboxInput("show_xlab8", "Poka¿ etykietê osi X", value = TRUE),
                                         checkboxInput("show_ylab8", "Poka¿ etykietê osi Y", value=TRUE),
                                         textInput("tytul18", "Podaj tytu³ wykresu", value = "Wykres b¹belkowy")
                                       ),
                                       mainPanel(plotOutput("distPlot8", height = "600px"))
                                     )
                            ),  
                            tabPanel("Wykres b¹belkowy z mo¿liwoœci¹ przybli¿ania punktów",
                                     
                                     sidebarLayout(
                                       sidebarPanel(
                                         helpText("Aby przybli¿yæ zaznacz wybrany zakres punktów i kliknij na niego dwa razy."),
                                         selectInput(inputId = "zmienna1_82",
                                                     label="Wybierz pierwsz¹ zmienn¹:",
                                                     choices = names(data)[c(2:3,5:18)], selected = "age"),
                                         
                                         selectInput(inputId = "zmienna2_82",
                                                     label="Wybierz drug¹ zmienn¹:",
                                                     choices = names(data)[c(2:3,5:18)], selected = "price_milions"),
                                         
                                         selectInput(inputId = "zmienna3_82",
                                                     label="Wybierz zmienn¹ oznaczon¹ rozmiarem b¹belka:",
                                                     choices = names(data)[c(6:16)], selected = "rooms"),
                                         selectInput(inputId = "zmienna4_82",
                                                     label="Wybierz zmienn¹ oznaczon¹ kolorem b¹belka:",
                                                     choices = names(data)[c(2:3,5:18)], selected = "rooms"),
                                         sliderInput("shape82", "Wybierz kszta³t punktów:", min=1, max=25, step = 1, value=21),
                                         numericInput("size82", "Wybierz rozmiar punktów:", min = 0, step = 0.2, value = 3),
                                         checkboxInput("show_xlab82", "Poka¿ etykietê osi X", value = TRUE),
                                         checkboxInput("show_ylab82", "Poka¿ etykietê osi Y", value=TRUE),
                                         textInput("tytul28", "Podaj tytu³ wykresu", value = "Wykres b¹belkowy")
                                       ),
                                       mainPanel(plotOutput("distPlot82", height = "600px",
                                                            dblclick = "plot1_dblclick",
                                                            brush=brushOpts(
                                                              id="plot1_brush",
                                                              resetOnNew = TRUE
                                                            )))
                                     )
                            )
                            
                            
                 )
                
                           

                 
                 
))


server <- function(input, output) {
  
  
  ############# HISTOGRAM ZMIENNEJ ILOŒCIOWEJ
  output$distPlot9 <- renderPlot({
    # generate bins based on input$bins from ui.R
    x9    <- data[[input$zmienna9]]
    bins9 <- seq(min(x9), max(x9), length.out = input$bins9 + 1)
    srednia9 <- round(mean(x9),2)
    # draw the histogram with the specified number of bins
    
    xlab9 <- ifelse(input$show_xlab9, input$zmienna9, "")
    ylab9 <- ifelse(input$show_ylab9, "Frequency", "")
    main9 <- ifelse(input$show_title9, paste("Histogram zmiennej", input$zmienna9), "")
    hist(x9, breaks = bins9, col = input$wypelnienie9, border = input$obramowanie9, main = main9, xlab = xlab9, ylab = ylab9)
    
    lty9 <- ifelse(input$show_line9, 2, 0)
    abline(v = srednia9, col=input$kolor_linii9, lwd=2, lty=lty9)
    
    labels9 <- ifelse(input$show_mean9, paste("œrednia =", srednia9), "")
    text(x= srednia9, y = input$polozenie_czcionki9, labels = labels9, pos = 2, srt = 270, cex = input$rozmiar_czcionki9)
  })
  
  ############## HISTOGRAM ZMIENNEJ JAKOŒCIOWEJ
  output$distPlot10 <- renderPlot({
    x10    <- data[[input$zmienna10]]
    fill10 <- input$wypelnienie10
    xlab10 <- ifelse(input$show_xlab10, input$zmienna10, "")
    ylab10 <- ifelse(input$show_ylab10, "Frequency", "")
    title10 <- ifelse(input$show_title10, paste("Histogram zmiennej", input$zmienna10), "")
    
    ggplot(data=data)+geom_bar(aes(x=x10),fill=fill10, alpha=input$transparentnosc10)+
      labs(title= title10 ,x = xlab10, y = ylab10)+
      theme_light()
    
  })
 
  ############# HISTOGRAM DWÓCH ZMIENNYCH
  output$distPlot11 <- renderPlot({
    x11    <- data[[input$zmienna1_11]]
    y11    <- data[[input$zmienna2_11]]
    
    
    xlab11 <- ifelse(input$show_xlab11, input$zmienna1_11, "")
    ylab11 <- ifelse(input$show_ylab11, "Procent ca³oœci", "")
    main11 <- ifelse(input$show_title11, paste("Histogram zmiennych:", input$zmienna1_11, "i", input$zmienna2_11), "")
    histogram( ~ x11 | y11, main=main11, xlab=xlab11, ylab=ylab11, col=input$wypelnienie11)
    
  })
  
  ############# WYKRESY PUDE£KOWE
  
  output$distPlot2_13 <- renderPlot({
    # generate bins based on input$bins from ui.R
    y13   <- data[[input$zmienna1_13]]
    
    ylab13 <- ifelse(input$show_ylab13, input$zmienna1_13, "")
    main13 <- ifelse(input$show_title13, paste("Wykres pude³kowy zmiennej", input$zmienna1_13), "")
    ggplot(aes(y = y13), data = data) +
      geom_boxplot(outlier.colour = input$kolor_odstajacych13, outlier.shape = input$shape13) + ggtitle(main13) + 
      #labs(xlab = xlab, ylab = ylab) +
      ylab(ylab13) +
      theme_light()
  })
  
  
  
  
  
  output$distPlot13 <- renderPlot({
    # generate bins based on input$bins from ui.R
    x13    <- data[[input$zmienna2_13]]
    y13    <- data[[input$zmienna1_13]]
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #srednia <- round(mean(x),2)
    # draw the histogram with the specified number of bins
    
    xlab13 <- ifelse(input$show_xlab13, input$zmienna2_13, "")
    ylab13 <- ifelse(input$show_ylab13, input$zmienna1_13, "")
    main13 <- ifelse(input$show_title13, paste("Wykresy pude³kowe zmiennych:", input$zmienna1_13, "i", input$zmienna2_13), "")
    ggplot(aes(x = x13, y = y13), data = data) +
      geom_boxplot(outlier.colour = input$kolor_odstajacych13, outlier.shape = input$shape13) + ggtitle(main13) + 
      #labs(xlab = xlab, ylab = ylab) +
      xlab(xlab13) + ylab(ylab13) +
      theme_light()
  })
  

################## WYKRES CZÊSTOŒCI
  
  
  output$distPlot12 <- renderPlot({
    # generate bins based on input$bins from ui.R
    x12    <- data[[input$zmienna12]]
    bins12 <- seq(min(x12), max(x12), length.out = input$bins12 + 1)
    # draw the histogram with the specified number of bins
    fill12 <- input$wypelnienie12
    xlab12 <- ifelse(input$show_xlab12, input$zmienna12, "")
    ylab12 <- ifelse(input$show_ylab12, "Frequency", "")
    title12 <- ifelse(input$show_title12, paste("Wykres gêstoœci zmiennej", input$zmienna12), "")
    
    ggplot(data=data)+geom_freqpoly(aes(x=x12), breaks=bins12)+
      labs(title= title12 ,x = xlab12, y = ylab12)+
      theme_light()
    
  })
     

  ############## WYKRES (SK.) F. GÊSTOŒCI
  
  output$distPlot14 <- renderPlot({
    # generate bins based on input$bins from ui.R
    x14    <- data[[input$zmienna1_14]]
    y14    <- data[[input$zmienna2_14]]
    # draw the histogram with the specified number of bins
    xlab14 <- ifelse(input$show_xlab14, input$zmienna1_14, "")
    ylab14 <- ifelse(input$show_ylab14, "Gêstoœæ", "")
    title14 <- ifelse(input$show_title14, paste("Funkcja gêstoœci zmiennej", input$zmienna1_14), "")
    
    
    densityplot(~ x14, xlab = xlab14, ylab = ylab14,
                main=title14, plot.points = input$punkty14)
  })
  
  
  output$distPlot214 <- renderPlot({
    # generate bins based on input$bins from ui.R
    x14    <- data[[input$zmienna1_14]]
    y14    <- data[[input$zmienna2_14]]
    # draw the histogram with the specified number of bins
    fill14 <- input$wypelnienie14
    xlab14 <- ifelse(input$show_xlab14, input$zmienna1_14, "")
    ylab14 <- ifelse(input$show_ylab14, "Gêstoœæ", "")
    title214 <- ifelse(input$show_title214, paste("Funkcja gêstoœci zmiennych", input$zmienna1_14, "i", input$zmienna2_14), "")
    
    
    densityplot(~ x14 | y14, xlab = xlab14, ylab = ylab14, 
                main= title214, plot.points=input$punkty14)
  })
  
  output$distPlot314 <- renderPlot({
    # generate bins based on input$bins from ui.R
    x14    <- data[[input$zmienna1_14]]
    y14    <- data[[input$zmienna2_14]]
    
    fill14 <- input$wypelnienie14
    xlab14 <- ifelse(input$show_xlab14, input$zmienna1_14, "")
    ylab14 <- ifelse(input$show_ylab14, "Gêstoœæ", "")
    title214 <- ifelse(input$show_title214, paste("Funkcja gêstoœci zmiennych", input$zmienna1_14, "i", input$zmienna2_14), "")
    
    auto.key <- input$leg14 
    densityplot(~ x14, group = y14, xlab = xlab14, ylab = ylab14, 
                main= title214, plot.points=input$punkty14, auto.key = TRUE)
  })
  
  output$distPlot414 <- renderPlot({
    # generate bins based on input$bins from ui.R
    x14    <- data[[input$zmienna1_14]]
    
    xlab14 <- ifelse(input$show_xlab14, input$zmienna1_14, "")
    ylab14 <- ifelse(input$show_ylab14, "Skumulowana gêstoœæ", "")
    title14 <- ifelse(input$show_title14, paste("Skumulowana funkcja gêstoœci zmiennej", input$zmienna1_14), "")
    
    ecdfplot(~ x14, xlab = xlab14,
             ylab= ylab14, main = title14, plot.points = input$punkty14, type = input$typ14)
  })
  
  
  output$distPlot514 <- renderPlot({
    # generate bins based on input$bins from ui.R
    x14    <- data[[input$zmienna1_14]]
    y14    <- data[[input$zmienna2_14]]
    
    fill14 <- input$wypelnienie14
    xlab14 <- ifelse(input$show_xlab14, input$zmienna1_14, "")
    ylab14 <- ifelse(input$show_ylab14, "Skumulowana gêstoœæ", "")
    title214 <- ifelse(input$show_title214, paste("Skumulowana funkcja gêstoœci zmiennych", input$zmienna1_14, "i", input$zmienna2_14), "")
    
    ecdfplot(~ x14 | y14, xlab = xlab14,
             ylab= ylab14, main = title214, plot.points = input$punkty14, type = input$typ14)
    
  })
  
  output$distPlot614 <- renderPlot({
    # generate bins based on input$bins from ui.R
    x14    <- data[[input$zmienna1_14]]
    y14    <- data[[input$zmienna2_14]]
    
    fill14 <- input$wypelnienie14
    xlab14 <- ifelse(input$show_xlab14, input$zmienna1_14, "")
    ylab14 <- ifelse(input$show_ylab14, "Skumulowana gêstoœæ", "")
    title214 <- ifelse(input$show_title214, paste("Skumulowana funkcja gêstoœci zmiennych", input$zmienna1_14, "i", input$zmienna2_14), "")
    
    ecdfplot(~ x14, group = y14, xlab = xlab14,
             ylab= ylab14, main = title214, discrete = TRUE, plot.points = input$punkty14, auto.key = TRUE, type = input$typ14)
    
  })
  
  ############# WYKRES KORELACJI
    
  output$distPlot15 <- renderPlot({
    
    title15 <- ifelse(input$show_title15, "Wykres korelacji miêdzy zmiennymi", "")
    
    res1 <- cor.mtest((data)[c(2:3,5:10,17:18)], conf.level = .95)
    a<-data.frame((data)[c(2:3,5:10,17:18)])
    b<-cor(a)
    colnames(b) <- colnames((data)[c(2:3,5:10,17:18)])
    rownames(b) <- colnames((data)[c(2:3,5:10,17:18)])
    
    grid.newpage()
    pushViewport(viewport(width=0.7, height=0.7,
                          name="vp1"))
    print(corrplot.mixed(b, lower.col = input$kolor_numery15, tl.cex= input$rozmiar_naglowki15, number.cex = input$rozmiar_numery15, upper=input$upper15,
                         p.mat = res1$p, sig.level = input$poziom_istotnosci15, tl.pos = input$polozenie15,
                         main = title15, mar = c(2, 1, 2, 1)))
    
    
    
    
  })
  
  output$distPlot2 <- renderPlot({
    
    title <- ifelse(input$show_title, "Wykres zale¿noœci miêdzy zmiennymi", "")
    
    grid.newpage()
    pushViewport(viewport(width=0.7, height=0.7,
                          name="vp1"))
    print(pairs((data)[c(2:3,5:10,17:18)], panel = panel.smooth,
                main = "Wykres zale¿noœci miêdzy zmiennymi iloœciowymi", mar = c(2, 1, 2, 1)))
    
    
    
    
  })
  
  output$distPlot3 <- renderPlot({
    
    xlab16 <- ifelse(input$show_xlab16, input$zmienna116, "")
    ylab16 <- ifelse(input$show_ylab16, input$zmienna216, "")
    main16 <- ifelse(input$show_main16, paste("Wykres zale¿noœci miêdzy zmiennymi:", input$zmienna116, "i", input$zmienna216), "")
    lty_plot16 <- ifelse(input$lty_plot16, 1, 0)
    x16 <- data[[input$zmienna116]]
    y16 <- data[[input$zmienna216]]
    
    plot(jitter(x16, input$jitterx16), jitter(y16, input$jittery16), xlab=xlab16, 
         ylab=ylab16, main=main16, mar=c(2,1,2,1), type = input$typ_plot16, col = input$kolor_plot16, pch = input$pch16)
    
    mod1 <- lm(y16 ~ x16)
    abline(mod1, lty= lty_plot16, col = input$kolor_abline16)
    
  })
  
  
  model <- reactive({
    brushed_data <- brushedPoints(data, input$brush1,
                                  xvar = input$zmienna_117, yvar = input$zmienna_217)
    if(nrow(brushed_data) < 2){
      return(NULL)
    }
    lm(brushed_data[[input$zmienna_217]] ~ brushed_data[[input$zmienna_117]])
  })
  output$nachylenie <- renderText({
    if(is.null(model())){
      "No model found"
    }else {
      model()[[1]][2]
    }})
  
  
  output$distPlot4 <- renderPlot({
    xlabd <- ifelse(input$xlab17, input$zmienna_117, "")
    ylabd <- ifelse(input$ylab17, input$zmienna_217, "")
    maind <- ifelse(input$main17, paste("Wykres zale¿noœci miêdzy zmiennymi:", input$zmienna_117, "i", input$zmienna_217), "")
    plot(data[[input$zmienna_117]], data[[input$zmienna_217]], xlab=xlabd,
         ylab = ylabd, main=maind,
         cex=1.5, pch=16, bty="n")
    if(!is.null(model())){
      abline(model(), col="blue", lwd=2) 
    }
  })
  
  
  
  output$distPlot5 <- renderPlot({
    xlab5 <- ifelse(input$show_xlab5, input$zmienna1_5, "")
    ylab5 <- ifelse(input$show_ylab5, input$zmienna2_5, "")
    main5 <- ifelse(input$show_main5, paste("Wykres zale¿noœci miêdzy zmiennymi:", input$zmienna1_5, ",", input$zmienna2_5, "i",
                                            input$zmienna3_5), "")
    
    
    qplot(data[[input$zmienna1_5]], data[[input$zmienna2_5]], color=data[[input$zmienna3_5]],
          main=main5, ylab = ylab5, xlab =  xlab5) +
      labs(fill = input$zmienna3_5)+
      theme_bw() 
  })
  
  
  
  output$distPlot6 <- renderPlot({
    xlab6 <- ifelse(input$show_xlab6, input$zmienna1_6, "")
    ylab6 <- ifelse(input$show_ylab6, input$zmienna2_6, "")
    main6 <- ifelse(input$show_main6, paste("Wykres zale¿noœci miêdzy zmiennymi:", input$zmienna1_6, ",", input$zmienna2_6, "i",
                                            input$zmienna3_6), "")
    #zmienna1_6 <- input$zmienna1_6
    #zmienna2_6 <- input$zmienna2_6
    #zmienna3_6 <- input$zmienna3_6
    #qplot(zmienna1_6, zmienna2_6, data=data,
    #      main=main6, ylab = ylab6, xlab =  xlab6) +
    #facet_wrap(~ zmienna3_6)+#, nrow=input$wiersze6) 
    #  theme_bw() 
    
    xyplot(data[[input$zmienna2_6]] ~ data[[input$zmienna1_6]] | factor(data[[input$zmienna3_6]]),
           main = main6,
           xlab=xlab6, ylab=ylab6, panel=function(x, y) {
             panel.lmline(x, y)
             panel.xyplot(x, y)
           })
    
    
  })
  
  output$distPlot7 <- renderPlot({
    xlab7 <- ifelse(input$show_xlab7, input$zmienna1_7, "")
    ylab7 <- ifelse(input$show_ylab7, input$zmienna2_7, "")
    main7 <- ifelse(input$show_main7, paste("Wykres zale¿noœci miêdzy zmiennymi:", input$zmienna1_7, "i", input$zmienna2_7), "")
    
    smoothScatter(x=data[[input$zmienna1_7]], y=data[[input$zmienna2_7]],
                  xlab=xlab7, ylab=ylab7, main=main7)
    
    
  })
  
  
  output$distPlot8 <- renderPlot({
    xlab8 <- ifelse(input$show_xlab8, input$zmienna1_8, "")
    ylab8 <- ifelse(input$show_ylab8, input$zmienna2_8, "")

    
    ggplot(data = data,aes(x=data[[input$zmienna1_8]], y=data[[input$zmienna2_8]], size=data[[input$zmienna3_8]], 
                           label=data[[input$zmienna3_8]], fill = as.factor(data[[input$zmienna4_8]])),guide=FALSE)+
      geom_point(alpha=0.5, color="black", shape=input$shape8)+ scale_size_area(max_size = input$size8)+
      ggtitle(input$tytul18)+
      scale_x_continuous(name=xlab8)+
      scale_y_continuous(name=ylab8)+
      theme_ipsum()
    
    
  })
  

  

  
  ranges <- reactiveValues(x = NULL, y = NULL) 
  
  output$distPlot82 <- renderPlot({
    xlab82 <- ifelse(input$show_xlab82, input$zmienna1_82, "")
    ylab82 <- ifelse(input$show_ylab82, input$zmienna2_82, "")

    ggplot(data = data,aes(x=data[[input$zmienna1_82]], y=data[[input$zmienna2_82]], size=data[[input$zmienna3_82]], 
                           label=data[[input$zmienna3_82]], fill = as.factor(data[[input$zmienna4_82]])),guide=FALSE)+
      geom_point(alpha=0.5, color="black", shape=input$shape82)+ scale_size_area(max_size = input$size82)+
      ggtitle(input$tytul28)+
      scale_x_continuous(name=xlab82)+
      scale_y_continuous(name=ylab82)+
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)+
      theme_ipsum()
    
    
  })
  
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }})
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

