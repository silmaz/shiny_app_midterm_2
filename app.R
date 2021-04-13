# https://github.com/pcm-dpc/COVID-19

library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(shinycssloaders)
library(shinyjs)
library(shinythemes)
library(plyr)
library(stringr)
library(maptools)
library(ggalt)
library(ggthemes)
library(tibble)
library(viridis)
library(RColorBrewer)
library(htmlwidgets)

library(reshape2)
library(data.table)
library(stringr)

library(tidyr)
library (readr)

library(maps)
library(mapproj)
library(cartography)
library(devtools)

library(mapIT)

source("helpers_2.R") # table and map
source("helpers_3.R") # preparation data set
source("helpers_4.R") # moving average

################# PROVINCE ----
url_province <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province-latest.csv"
data_province<-read_csv(url(url_province))
data_province <- as.data.frame(data_province)

DATE_province <- substring(as.character(data_province$data[1]), 1, 10)   
new_data_province <- preparation_data_prov(data_province)


rem <- c("Barletta", "Crotone", "Fermo", "Lecco", "Lodi", "Monza e Brianza",
         "Prato", "Rimini", "Sud Sardegna", "Verbano", "Vibo Valentina")
fin <- c("Bari", "Catanzaro", "Ascoli-Piceno", "Bergamo", "Milano", "Milano",
         "Firenze", "Forli'", "Cagliari", "Novara", "Catanzaro")
prov_removed <- data.frame(originali = rem, prov_mappa = fin)



############### REGIONI ----
url_regioni <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni-latest.csv"
data_regioni<-read_csv(url(url_regioni))
data_regioni<-as.data.frame(data_regioni)

DATE_regioni <- substring(as.character(data_regioni$data[1]), 1, 10)   

data_regioni <-preparation_data_regioni(data_regioni)


gp <- list(low="yellow", high="red3")







################ NAZIONE ----
url_nazione <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv"
data_nazione <- read_csv(url(url_nazione))
data_nazione<-as.data.frame(data_nazione)
head(data_nazione)

day <- c(NA, length(data_nazione$data))
for (i in 1:length(data_nazione$data)){
    day[i] <- substring(as.character(data_nazione$data[i]), 1, 10)   
}
data_nazione<-data.frame(day,data_nazione)
data_nazione<-data_nazione[,-c(2)]

data_nazione[is.na(data_nazione)] = 0

deceduti_oggi <- rep(NA, length(day))
deceduti_oggi[1]<- data_nazione$deceduti[1]
guariti_oggi <- rep(NA,length(day))
guariti_oggi[1] <- data_nazione$dimessi_guariti[1]
isolamento_oggi <- rep(NA,length(day))
isolamento_oggi[1] <- data_nazione$isolamento_domiciliare[1]
ospedalizzati_non_gravi <- rep(NA, length(day))
for (i in 1:(length(day)-1)){
    deceduti_oggi[i+1] <- data_nazione$deceduti[i+1]-data_nazione$deceduti[i]
    guariti_oggi[i+1] <- data_nazione$dimessi_guariti[i+1]-data_nazione$dimessi_guariti[i]
    isolamento_oggi[i+1] <- data_nazione$isolamento_domiciliare[i+1]-data_nazione$isolamento_domiciliare[i]
    ospedalizzati_non_gravi[i] <- data_nazione$totale_ospedalizzati[i]-data_nazione$terapia_intensiva[i]
}

data_nazione<-data.frame(data_nazione, deceduti_oggi,guariti_oggi,isolamento_oggi,ospedalizzati_non_gravi)








#################### APP ----


ui <- fluidPage(theme = shinytheme("superhero"),
                useShinyjs(),
 
            navbarPage("COVID-19 Data Visualizer",
                
                tabPanel("Introduction",
                        fluidPage(
                             h4("This App plots some graphs abaut coronavirus cases in Italy and
                                the condition of the Hospital due to the number of infected people."),
                             h3("Plots"),
                             h4("The App is divided in 4 different areas with different types of plots."),
                             h4("1.1. The number of the cases each days and also the number of healted each day
                                in two different layout;"),
                             h4("1.2. the moving average of dayly new cases and healted with the possibility 
                                of changing the number of days for the mean;"),
                             h4("1.3. the moving average and also the true value of dayly cases and healted."),
                             h4("2.1. The number of dayly cases, the number of hospitalized people,
                                of the people in intensica care and those in quarantine;"),
                             h4("2.2. the proportion of people hospitalized (minor cases) and those in intensive 
                                care with respect the total number of hospitalized people;"),
                             h4("2.3. the number of hospitalized and quarantine cases over the total number of cases."),
                             h4("3. The number of cases each day in each provinces and I give them a color between yellow and red
                                 with respect the daily highest number of cases."),
                             h4("4. Some other variables for each regions each day with an analogous color scale."),
                             h3("Structure"),
                             h4("Each of these four arguments are gouped with tabs and, if necessary, thay 
                                are also divided in subtabs."),
                             h4("In some cases I report some details about how I modify the data before visualize them.")
                         )
                 ),
                
                tabPanel("Cases and Recovered", 
                    sidebarPanel(
                        dateRangeInput("dates", label = h3("Date range"),min = data_nazione$day[1],
                                       max = data_nazione$day[length(data_nazione$day)],
                                       start = data_nazione$day[250],
                                       end = data_nazione$day[length(data_nazione$day)],
                                       format = "yyyy-mm-dd"),
                        checkboxInput("casi", "Daily new cases",value = TRUE),
                        checkboxInput("guariti", "Daily Healed",value = FALSE),
                        selectInput("select", label = h3("Select box"), 
                                    choices = list("Lines" = 1, "Bars" = 2), 
                                    selected = 2)
                    ),
                    mainPanel(
                        tabsetPanel(
                        tabPanel("Cases and Healed",
                           plotOutput("plot_1") %>% withSpinner(color="#0dc5c1")
                       ),
                       tabPanel("Moving Average",
                            numericInput("k_val", label="Moving average k variable",
                                         value = 7, min = 1,
                                         max = length(data_nazione)-1, step = 1),
                           plotOutput("plot_2") %>% withSpinner(color="#0dc5c1")
                        ),
                       tabPanel("Cases and Moving Average",
                                numericInput("k_val_2", label="Moving average k variable",
                                             value = 7, min = 1,
                                             max = length(data_nazione)-1, step = 1),
                                plotOutput("plot_3") %>% withSpinner(color="#0dc5c1"),
                                textOutput("text_bar")
                       
                       )
                    )
                    )
                   ), 
                
                
                tabPanel("Hospitals",
                         sidebarPanel(
                             dateRangeInput("dates", label = h3("Date range"),min = data_nazione$day[1],
                                            max = data_nazione$day[length(data_nazione$day)],
                                            start = data_nazione$day[250],
                                            end = data_nazione$day[length(data_nazione$day)],
                                            format = "yyyy-mm-dd")
                             
                         ),
                         mainPanel(
                             tabsetPanel(
                                    tabPanel("Cases distribution",plotOutput("plot_osp_1") %>% withSpinner(color="#0dc5c1")),
                                    tabPanel("Cases in hospital",plotOutput("plot_osp_2") %>% withSpinner(color="#0dc5c1")),
                                    tabPanel("Hospital and quarantine",plotOutput("plot_osp_3") %>% withSpinner(color="#0dc5c1"))
                            )
                          )
                        ),
            
                tabPanel("Provinces", 
                                 fluidPage(
                                     column(6,
                                            plotOutput("plot_prov") %>% withSpinner(color="#0dc5c1"),
                                            textOutput("text_prov_4"),
                                            textOutput("text_prov_1"),
                                            textOutput("text_prov_2"),
                                            textOutput("text_prov_3"),
                                            tableOutput("table_prov_remove") %>% withSpinner(color="#0dc5c1")
                                     ),
                                     column(6,
                                            dateInput("date_prov",
                                                      label = paste('Note: i dati sia aggiornano alle',
                                                                    '18:00 di ogni giorno, quindi per vedere',
                                                                    'i dati di oggi devi attendere quell ora.'),
                                                      min = "2020-03-18",
                                                      max = format(Sys.Date(),"%Y-%m-%d"),
                                                      value = DATE_province),
                                            dataTableOutput("table_prov") %>% withSpinner(color="#0dc5c1")
                                     )
                                ),
                            
                ),
                
                tabPanel("Regions", 
                         fluidPage(
                           column(7,
                                  plotOutput("plot_reg") %>% withSpinner(color="#0dc5c1"),
                                  textOutput("text_reg"),
                                  
                           ),
                           column(5,
                                  dateInput("date_reg",
                                            label = paste('Note: i dati sia aggiornano alle',
                                                          '18:00 di ogni giorno, quindi per vedere',
                                                          'i dati di oggi devi attendere quell ora.'),
                                            min = "2020-03-18",
                                            max = format(Sys.Date(),"%Y-%m-%d"),
                                            value = DATE_regioni),
                                  #dataTableOutput("table_reg") %>% withSpinner(color="#0dc5c1")
                                  selectInput("select_reg", label = h3("Select variable"), 
                                              choices = list("Cases" = 1, "Hospedalized with symptoms" = 2,
                                                             "Intensive care" = 3, "Hospedalized" = 4,
                                                             "Quarantine" = 5, "New daily cases" = 6,
                                                             "Dismissed heled" = 7, "Deaths" = 8,
                                                             "New daily intensive care" = 9,
                                                             "Cases over 10.000 people" = 10), 
                                              selected = 1),
                                  dataTableOutput("table_reg") %>% withSpinner(color="#0dc5c1"),
                                  
                           )
                         ),
                         
                ) 
        ) 
)




######## SERVER ----


######## CASES ----

server <- function(input, output) {


    values <- reactiveValues()
    
    output$plot_1 = renderPlot({
        values$DT <- data.frame(giorno = data_nazione$day[match(format(input$dates[1]),data_nazione$day) : match(format(input$dates[2]),data_nazione$day)],
                                positivi = data_nazione$nuovi_positivi[match(format(input$dates[1]),data_nazione$day) : match(format(input$dates[2]),data_nazione$day)],
                                guariti = data_nazione$guariti_oggi[match(format(input$dates[1]),data_nazione$day) : match(format(input$dates[2]),data_nazione$day)])
        
        if(input$select == 2){         
            if(input$casi==TRUE & input$guariti==FALSE){
                ggplot(values$DT,aes(x = as.Date(giorno), y = positivi)) + geom_bar(color = "#FC4E07", alpha=0.3, stat = "identity") +
                    scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") +
                    labs(x = "Days") +
                    labs(y = "Cases")
            }
            else if(input$casi==FALSE & input$guariti==TRUE){
                ggplot(values$DT,aes(x = as.Date(giorno), y = guariti)) + geom_bar(color="#00AFBB", alpha=0.3, stat = "identity") +
                    scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") +
                    labs(x = "Days") +
                    labs(y = "Healed")
            }
            else if(input$casi==TRUE & input$guariti==TRUE){
                ggplot() + 
                    geom_col(data=values$DT, aes(as.Date(giorno), y = positivi, colour = "Cases"), alpha = 0.2) + 
                    geom_col(data=values$DT, aes(as.Date(giorno), y = guariti, colour = "Healed"), alpha = 0.2) +
                    labs(x = "Days") + labs(y = "") +
                    theme(legend.position="bottom") + theme(legend.title = element_blank())
              
            }
            else if(input$casi==FALSE & input$guariti==FALSE){
                ggplot(values$DT)
            }
        }
        else if(input$select ==1){
            if(input$casi==TRUE & input$guariti==FALSE){
                ggplot(values$DT,aes(x = as.Date(giorno), y = positivi, color="#FC4E07")) +
                    geom_line() +
                    scale_x_date(date_breaks = "1 month",
                                 date_labels = "%b-%y") +
                    labs(x = "Days") +
                    labs(y = "Cases")
            }
            else if(input$casi==FALSE & input$guariti==TRUE){
                ggplot(values$DT,aes(x = as.Date(giorno), y = guariti, color="#00AFBB")) +
                    geom_line() +
                    scale_x_date(date_breaks = "1 month",
                                 date_labels = "%b-%y") + labs(x = "Days") +
                    labs(x = "Days") +
                    labs(y = "Healed")
            }
            else if(input$casi==TRUE & input$guariti==TRUE){
                ggplot() + 
                    labs(x = "Days") + labs(y = "") +
                    geom_line(data=values$DT, aes(as.Date(giorno), y = guariti, colour = "Recovered")) +
                    geom_line(data=values$DT, aes(as.Date(giorno), y = positivi, colour = "Cases")) + 
                    theme(legend.position="bottom") + theme(legend.title = element_blank())
              
            }
            else if(input$casi==FALSE & input$guariti==FALSE){
                ggplot(values$DT)
            }
        }
        
    })
    
    output$plot_2 = renderPlot({
        
        pos = data_nazione$nuovi_positivi[match(format(input$dates[1]),data_nazione$day) : match(format(input$dates[2]),data_nazione$day)]
        guar = data_nazione$guariti_oggi[match(format(input$dates[1]),data_nazione$day) : match(format(input$dates[2]),data_nazione$day)]
        
        pos <- media_mobile(pos, input$k_val)
        guar <- media_mobile(guar, input$k_val)
        
        values$DT <- data.frame(giorno = data_nazione$day[match(format(input$dates[1]),data_nazione$day) : match(format(input$dates[2]),data_nazione$day)],
                                positivi = pos,
                                guariti = guar)
        
        if(input$select == 2){         
            if(input$casi==TRUE & input$guariti==FALSE){
                ggplot(values$DT,aes(x = as.Date(giorno), y = positivi)) + geom_bar(color = "#FC4E07", alpha=0.3, stat = "identity") +
                    scale_x_date(date_breaks = "1 month",
                                 date_labels = "%b-%y") +
                    labs(x = "Days") +
                    labs(y = "Cases")
            }
            else if(input$casi==FALSE & input$guariti==TRUE){
                ggplot(values$DT,aes(x = as.Date(giorno), y = guariti)) + geom_bar(color="#00AFBB", alpha=0.3, stat = "identity") +
                    scale_x_date(date_breaks = "1 month",
                                 date_labels = "%b-%y") +
                    labs(x = "Days") + labs(y = "Healed")
            }
            else if(input$casi==TRUE & input$guariti==TRUE){
                ggplot() + 
                    geom_col(data=values$DT, aes(as.Date(giorno), y = positivi, colour = "Cases"), alpha = 0.2) + 
                    geom_col(data=values$DT, aes(as.Date(giorno), y = guariti, colour = "Recovered"), alpha = 0.2) +
                    labs(x = "Days") + labs(y = "") + 
                    theme(legend.position="bottom") + theme(legend.title = element_blank())
            }
            else if(input$casi==FALSE & input$guariti==FALSE){
                ggplot(values$DT)
            }
        }
        else if(input$select ==1){
            if(input$casi==TRUE & input$guariti==FALSE){
                ggplot(values$DT,aes(x = as.Date(giorno))) +
                    geom_line(aes( y = positivi, color="positivi"), color="#FC4E07", position = 'identity') +
                    scale_x_date(date_breaks = "1 month",
                                 date_labels = "%b-%y") +
                    labs(x = "Days") +
                    labs(y = "Cases")
            }
            else if(input$casi==FALSE & input$guariti==TRUE){
                ggplot(values$DT,aes(x = as.Date(giorno))) + 
                    geom_line(aes( y = guariti, color = "guariti"), color="#00AFBB", position = 'identity') +
                    scale_x_date(date_breaks = "1 month",
                                 date_labels = "%b-%y") +
                    labs(x = "Days") + labs(y = "Healed")
            }
            else if(input$casi==TRUE & input$guariti==TRUE){
                ggplot() + 
                    geom_line(data=values$DT, aes(as.Date(giorno), y = guariti, color = "Healed")) +
                    geom_line(data=values$DT, aes(as.Date(giorno), y = positivi, color = "Cases")) +
                    theme(legend.position="bottom") + theme(legend.title = element_blank()) +
                    labs(x = "Days") + labs(y ="")
            }
            else if(input$casi==FALSE & input$guariti==FALSE){
                ggplot(values$DT)
            }
        }
        
    })
    
    output$plot_3 = renderPlot({
      
        pos = data_nazione$nuovi_positivi[match(format(input$dates[1]),data_nazione$day) : match(format(input$dates[2]),data_nazione$day)]
        guar = data_nazione$guariti_oggi[match(format(input$dates[1]),data_nazione$day) : match(format(input$dates[2]),data_nazione$day)]
        
        pos <- media_mobile(pos, input$k_val)
        guar <- media_mobile(guar, input$k_val)
        
        values$DT <- data.frame(giorno = data_nazione$day[match(format(input$dates[1]),data_nazione$day) : match(format(input$dates[2]),data_nazione$day)],
                                positivi = data_nazione$nuovi_positivi[match(format(input$dates[1]),data_nazione$day) : match(format(input$dates[2]),data_nazione$day)],
                                positivi_k = pos,
                                guariti = data_nazione$guariti_oggi[match(format(input$dates[1]),data_nazione$day) : match(format(input$dates[2]),data_nazione$day)],
                                guariti_k = guar)
        
            if(input$casi==TRUE & input$guariti==FALSE){ # positivi
                ggplot() +
                    geom_col(data=values$DT, aes(x = as.Date(giorno), y=positivi, color = "positivi"),alpha=0.3) + 
                    geom_line(data=values$DT, aes(x = as.Date(giorno), y=positivi_k, color="media mobile positivi")) + 
                    scale_x_date(date_breaks = "1 month",
                                 date_labels = "%b-%y") +
                    labs(x = "Days") +
                    labs(y = "Cases") +
                scale_colour_manual("", values = c("#CC0000","#FC4E07")) + 
                theme(legend.position="bottom")
              
            }
            else if(input$casi==FALSE & input$guariti==TRUE){ # guariti
                ggplot(values$DT,aes(x = as.Date(giorno))) +
                    geom_col(aes(y = guariti, color="guariti"), alpha=0.3) +
                    geom_line(aes(y=guariti_k,color="media mobile positivi")) + 
                    scale_x_date(date_breaks = "1 month",
                                 date_labels = "%b-%y") + labs(x = "days") +
                labs(x = "Days") +
                labs(y = "Healed") +
                scale_colour_manual("", values = c("#00AFBB","blue")) + 
                theme(legend.position="bottom")
            }
            else if(input$casi==TRUE & input$guariti==TRUE){
                ggplot(values$DT, aes(as.Date(giorno))) + 
                    geom_col(aes(y = positivi, colour = "Cases"), alpha = 0.2) + 
                    geom_col(aes(y = guariti, colour = "Healed"), alpha = 0.2) +
                    geom_line(aes(y=positivi_k, color="Moving average cases")) + 
                    geom_line(aes(y=guariti_k, color="Moving average healed")) +
                labs(x = "Days") +
                labs(y = "") +
                scale_colour_manual("", values = c("#FC4E07","#00AFBB","#CC0000","blue")) + 
                theme(legend.position="bottom")
                
            }
            else if(input$casi==FALSE & input$guariti==FALSE){
                ggplot(values$DT)
            }
            
    })
    
    output$text_bar = renderText({
      paste("In this case the Select Box for the layout of the plot is usefull.
             This is the only way in which you can plot these graphs, because lines and bars are both present.")
    })
    
    
    
######### OSPEDALI ----
    
    output$plot_osp_1 = renderPlot({
        
        positivi = data_nazione$totale_positivi[length(data_nazione$day)-1:length(data_nazione$day)]
        ospedale = data_nazione$totale_ospedalizzati[length(data_nazione$day)-1:length(data_nazione$day)]
        
        positivi_non_osp <- rep(NA, length(data_regioni$totale_positivi))
        for(i in 1:length(positivi)){
            positivi_non_osp[i] <- positivi[i]-ospedale[i]
        }
        
        values$DT <- data.frame( giorno = data_nazione$day[length(data_nazione$day)-1:length(data_nazione$day)],
                                 tot_positivi = data_nazione$totale_positivi[length(data_nazione$day)-1:length(data_nazione$day)],
                                 terapia_intensiva = data_nazione$terapia_intensiva[length(data_nazione$day)-1:length(data_nazione$day)],
                                 ospedalizzati_non_gravi = data_nazione$ospedalizzati_non_gravi[length(data_nazione$day)-1:length(data_nazione$day)],
                                 positivi_non_ospedalizzati = positivi_non_osp)
        
        ggplot() +
            geom_line(data=values$DT, aes(x=as.Date(giorno), y=tot_positivi, color = "Number of ases")) +
            geom_line(data=values$DT, aes(x=as.Date(giorno), y=terapia_intensiva, color = "Intensive care")) +
            geom_line(data=values$DT, aes(x=as.Date(giorno), y=ospedalizzati_non_gravi, color = "Hospitalized (minor cases)")) +
            geom_line(data=values$DT, aes(x=as.Date(giorno), y=positivi_non_ospedalizzati, color = "Quarantine cases")) +
            theme(legend.position="bottom") + theme(legend.title = element_blank()) +
            labs(y = "") + labs(x = "Days")
            
    })
    
    output$plot_osp_2 = renderPlot({
        
        ospedale = data_nazione$totale_ospedalizzati[length(data_nazione$day)-1:length(data_nazione$day)]
        terapia_intensiva = data_nazione$terapia_intensiva[length(data_nazione$day)-1:length(data_nazione$day)]
        ospedalizzati_non_gravi = data_nazione$ospedalizzati_non_gravi[length(data_nazione$day)-1:length(data_nazione$day)]
        
        osp_non_gr <- rep(NA,length(ospedale))
        osp_ter_int <- rep(NA,length(ospedale))
        
        for(i in 1:length(ospedale)){
            osp_non_gr[i] <- ospedalizzati_non_gravi[i]/ospedale[i]
            osp_ter_int[i] <- terapia_intensiva[i]/ospedale[i]
        }
        
        values$DT <- data.frame( giorno = data_nazione$day[length(data_nazione$day)-1:length(data_nazione$day)],
                                 osp_non_gravi = osp_non_gr,
                                 osp_terapia_intensiva = osp_ter_int)
        ggplot() +
            geom_line(data=values$DT, aes(x=as.Date(giorno), y=osp_non_gravi, color = "Hospitalized (minor cases) over hospitalized")) +
            geom_line(data=values$DT, aes(x=as.Date(giorno), y=osp_terapia_intensiva, color = "Intensive care over hospitalized")) +
            theme(legend.position="bottom") + theme(legend.title = element_blank()) +
            labs(y = "") + labs(x = "Days")

    })

    
    output$plot_osp_3 = renderPlot({
        
        ospedale = data_nazione$totale_ospedalizzati[length(data_nazione$day)-1:length(data_nazione$day)]
        positivi = data_nazione$totale_positivi[length(data_nazione$day)-1:length(data_nazione$day)]
        positivi_non_osp <- rep(NA, length(data_regioni$totale_positivi))
        for(i in 1:length(positivi)){
            positivi_non_osp[i] <- positivi[i]-ospedale[i]
        }
        
        pos_non_osp <- rep(NA,length(ospedale))
        pos_osp <- rep(NA,length(ospedale))
        
        for(i in 1:length(ospedale)){
            pos_non_osp[i] <- positivi_non_osp[i]/positivi[i]
            pos_osp[i] <- ospedale[i]/positivi[i]
        }
        
        values$DT <- data.frame( giorno = data_nazione$day[length(data_nazione$day)-1:length(data_nazione$day)],
                                 positivi_in_ospedale = pos_osp,
                                 positivi_a_casa = pos_non_osp)
        ggplot() +
            geom_line(data=values$DT, aes(x=as.Date(giorno), y=positivi_in_ospedale, color = "Hospetalised over cases")) +
            geom_line(data=values$DT, aes(x=as.Date(giorno), y=positivi_a_casa, color = "Quarantine cases over cases")) +
            theme(legend.position="bottom") + theme(legend.title = element_blank()) + 
            labs(y = "") + labs(x = "Days")
        
    })
    
    
    
    
    
######### PROVINCE ----
    
    output$plot_prov <- renderPlot({
        
        full_date <- as.character(input$date_prov)
        yeas_date <- substring(as.character(full_date), 1, 4)
        month_date <- substring(as.character(full_date), 6, 7)
        day_date <- substring(as.character(full_date), 9, 10)
        
        url_now <- paste('https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province-',
                     yeas_date,month_date,day_date,'.csv', sep = "")
        data_now<-read_csv(url(url_now))
        
        data_now <- as.data.frame(data_now)
        data_now_new <- preparation_data_prov(data_now)
        data_now_new <- as.data.frame(data_now_new)
        
        map_reg(data_now_new$totale_casi)

    })
    
    
    # tab
    output$table_prov <- renderDataTable({
        full_date <- as.character(input$date_prov)
        yeas_date <- substring(as.character(full_date), 1, 4)
        month_date <- substring(as.character(full_date), 6, 7)
        day_date <- substring(as.character(full_date), 9, 10)
        
        url_now <- paste('https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province-',
                     yeas_date,month_date,day_date,'.csv', sep = "")
        data_now<-read_csv(url(url_now))
        
        data_now <- as.data.frame(data_now)
        data_now_new <- preparation_data_prov(data_now)
        data_now_new <- as.data.frame(data_now_new)
        
        generate_tab(data_now_new$totale_casi)
        
    })
    
    # text
    
    output$text_prov_4 <- renderText({
      paste("The color in the map are defined using the highest number of daily cases.
             I divided the dayly range of number of cases in 5 intervals and assigned an
             index from 1 to 5 to define in which interval each provinces was.")
    })
    
    output$text_prov_1 <- renderText({
        paste("I merged some province to fit the data with the names of provinces of the map data set.")
    })
    output$text_prov_2 <- renderText({
        paste("I marge the first column provinces in the second column provinces.")
    })
    output$text_prov_3 <- renderText({
        paste("I add date about provinces that appears named as em('Fuori Regione / 
                Provincia Autonoma') or em('In fase di definizione/aggiornamento') to the 
                provincial capital.")
    })
    
    # table
    output$table_prov_remove = renderTable(prov_removed)
    
   
    
    
########## REGIONI ----
    
    output$plot_reg <- renderPlot({
      
      full_date <- as.character(input$date_reg)
      yeas_date <- substring(as.character(full_date), 1, 4)
      month_date <- substring(as.character(full_date), 6, 7)
      day_date <- substring(as.character(full_date), 9, 10)
      
      url_now <- paste('https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni-',
                       yeas_date,month_date,day_date,'.csv', sep = "")
      data_now<-read_csv(url(url_now))
      
      data_now <- as.data.frame(data_now)
      data_now_new <- preparation_data_regioni(data_now)
      data_now_new <- as.data.frame(data_now_new)
      
      if(input$select_reg == 1){
        mapIT(totale_positivi,denominazione_regione, data = data_now_new) +
          scale_fill_gradient(low="yellow", high="red", name="Cases")
      }else if(input$select_reg == 2){
        mapIT(ricoverati_con_sintomi,denominazione_regione, data = data_now_new) +
          scale_fill_gradient(low="yellow", high="red", name="Hospedalized \nwith \nsymptoms")
      }else if(input$select_reg == 3){
        mapIT(terapia_intensiva,denominazione_regione, data = data_now_new) +
          scale_fill_gradient(low="yellow", high="red", name="Intensive \ncare")
      }else if(input$select_reg == 4){
        mapIT(totale_ospedalizzati,denominazione_regione, data = data_now_new) +
          scale_fill_gradient(low="yellow", high="red", name="Hospedalized")
      }else if(input$select_reg == 5){
        mapIT(isolamento_domiciliare,denominazione_regione, data = data_now_new) +
          scale_fill_gradient(low="yellow", high="red", name="Quarantine")
      }else if(input$select_reg == 6){
        mapIT(nuovi_positivi,denominazione_regione, data = data_now_new) +
          scale_fill_gradient(low="yellow", high="red", name="New \ndaily \ncases")
      }else if(input$select_reg == 7){
        mapIT(dimessi_guariti,denominazione_regione, data = data_now_new) +
          scale_fill_gradient(low="yellow", high="red", name="Dismissed \nhealed")
      }else if(input$select_reg == 8){
        mapIT(deceduti,denominazione_regione, data = data_now_new) +
          scale_fill_gradient(low="yellow", high="red", name="Deaths")
      }else if(input$select_reg == 9){
        mapIT(ingresso_terapia_intensiva,denominazione_regione, data = data_now_new) +
          scale_fill_gradient(low="yellow", high="red", name="New daily \nintensive \ncare")
      }else if(input$select_reg == 10){
        mapIT(caso_per_10m,denominazione_regione, data = data_now_new) +
          scale_fill_gradient(low="yellow", high="red", name="Cases over \n10000 \npeople")
      }
      
      
      
     
      
    })
    
    output$table_reg <- renderDataTable({
      
      full_date <- as.character(input$date_reg)
      yeas_date <- substring(as.character(full_date), 1, 4)
      month_date <- substring(as.character(full_date), 6, 7)
      day_date <- substring(as.character(full_date), 9, 10)
      
      url_now <- paste('https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni-',
                       yeas_date,month_date,day_date,'.csv', sep = "")
      data_now<-read_csv(url(url_now))
      
      data_now <- as.data.frame(data_now)
      data_now_new <- preparation_data_regioni(data_now)
      data_now_new <- as.data.frame(data_now_new)
      
      if(input$select_reg == 1){
        tab_reg <- data_frame(Region = data_now_new$denominazione_regione,
                                Cases = data_now_new$totale_positivi)
      }else if(input$select_reg == 2){
        tab_reg <- data_frame(Region = data_now_new$denominazione_regione,
                                Hospedalized_with_symptoms = data_now_new$ricoverati_con_sintomi)
      }else if(input$select_reg == 3){
        tab_reg <- data_frame(Region = data_now_new$denominazione_regione,
                                Intensive_care = data_now_new$terapia_intensiva)
      }else if(input$select_reg == 4){
        tab_reg <- data_frame(Region = data_now_new$denominazione_regione,
                                Hospedalized = data_now_new$totale_ospedalizzati)
      }else if(input$select_reg == 5){
        tab_reg <- data_frame(Region = data_now_new$denominazione_regione,
                                Quarantine = data_now_new$isolamento_domiciliare)
      }else if(input$select_reg == 6){
        tab_reg <- data_frame(Region = data_now_new$denominazione_regione,
                                New_daily_cases = data_now_new$nuovi_positivi)
      }else if(input$select_reg == 7){
        tab_reg <- data_frame(Region = data_now_new$denominazione_regione,
                                Dismissed_healted = data_now_new$dimessi_guariti)
      }else if(input$select_reg == 8){
        tab_reg <- data_frame(Region = data_now_new$denominazione_regione,
                                Deaths = data_now_new$deceduti)
      }else if(input$select_reg == 9){
        tab_reg <- data_frame(Region = data_now_new$denominazione_regione,
                                New_daily_intensive_care = data_now_new$ingresso_terapia_intensiva)
      }else if(input$select_reg == 10){
        tab_reg <- data_frame(Region = data_now_new$denominazione_regione,
                                Cases_over_10.000_people = data_now_new$caso_per_10m)
      }
      
    })
    
    output$text_reg <- renderText({
      paste("The color scale in this plot is calculated as in the case of provinces.")
    })

}





shinyApp(ui = ui, server = server)


