library(shiny)
library(htmltools)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(ggplot2)
library(tidyverse)
library(sqldf)
library(lubridate)
library(foreign)
library(survminer)
library(survival)

rm(list=ls())

Header = dashboardHeader(
  
  
)

Sidebar = dashboardSidebar(
  
  sidebarMenu(
        menuItem(
        "Kaplan Meyer",
        tabName = "KME"
        )
        )
)

Body    = dashboardBody(
  
  tabItems(
    #Pagina do KME
    tabItem(tabName = "KME",
      fluidPage(
            column(width = 12,          
                     offset = 0,          
                     align = 'center',    
                     h1('Estimativa não paramétrica geral') 
                  ),
            tags$br(),
                #Primeira caixa
                box(title = "Função de sobrevivência",
                    width = 12,
                    height = "500px",
                    solidHeader = FALSE,
                    collapsible = TRUE,
                    #Saida-grafico
                    plotOutput("Grafico1"),
                    sidebar =  boxSidebar(
                      id = "Boxside01",
                      pickerInput(
                        inputId = "Id01",
                        label = "CID", 
                        choices = c("C00", "C01", "C02", "C03", "C04", 
                                    "C05", "C06", "C07", "C08", "C09", "C10", "C11", "C12", "C13", 
                                    "C14", "C15", "C16", "C17", "C18", "C19", "C20", "C21", "C22", 
                                    "C23", "C24", "C25", "C26", "C30", "C31", "C32", "C33", "C34", 
                                    "C37", "C38", "C39", "C40", "C41", "C42", "C44", "C47", "C48", 
                                    "C49", "C50", "C51", "C52", "C53", "C54", "C55", "C56", "C57", 
                                    "C58", "C60", "C61", "C62", "C63", "C64", "C65", "C66", "C67", 
                                    "C68", "C69", "C70", "C71", "C72", "C73", "C74", "C75", "C76", 
                                    "C77", "C80")
                      ),
                      materialSwitch(
                        inputId = "Id02",
                        label = "N° de individuos em risco", 
                        status = "primary",
                        right = TRUE
                      ),
                      sliderTextInput(
                        inputId = "Id03",
                        label = "Ano de diagnostico/Ano para censura:", 
                        choices = c("2000","2001","2002","2003","2004","2005","2006",
                                    "2007","2008","2009","2010","2011","2012","2013",
                                    "2014","2015","2016","2017","2018","2019","2020",
                                    "2021","2022"),
                        selected = c("2014", "2019"),
                        from_min = "2000", 
                        from_max = "2021",
                        to_min = "2001",
                        to_max = "2022"
                      )
                      
                    )
                      
                  )
                )
     )
 )
)

UI = dashboardPage(skin = "blue-light",header = Header, sidebar = Sidebar, body = Body)

Server = function(input, output) {
  
    # Carregando dados
    df = read.dbf("pacigeral.dbf")
    #Classificando dados de Censura - 0 e falha - 1
    df = df %>% mutate(`CENS` = case_when(`ULTINFO` == 3 ~ 1,
                                          TRUE ~ 0),
                       TEMPCENS = (ymd(DTDIAG) %--% ymd(DTULTINFO))/ddays(1)) %>%
                filter(TEMPCENS > 0)
    
    
    output$Grafico1 = renderPlot({
      
      CB = df %>% filter(ANODIAG == as.numeric(input$Id03[1]) & year(DTULTINFO) <= as.numeric(input$Id03[2]),
                         TOPOGRUP == input$Id01)
      
      EKM = survfit(Surv(TEMPCENS, CENS)~1 , data = CB)
      
      g = ggsurvplot(EKM,
                     data = CB,
                     ylab="S(t)",
                     xlab="Tempo (dias)",
                     title="Figura 1: Estimação não paramétrica de S(t)",
                     censor.shape=" ",
                     risk.table = input$Id02, 
                     risk.table.title="Individuos em risco",
                     pval = F
      )
      
      g
    })
    
    }

shinyApp(UI, Server)
