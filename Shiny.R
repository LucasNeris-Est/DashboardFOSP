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
setwd("input")
#dados = read.dbf("pacigeral_mod.dbf")

anos = c("2000","2001","2002","2003","2004","2005","2006",
         "2007","2008","2009","2010","2011","2012","2013",
         "2014","2015","2016","2017","2018","2019","2020",
         "2021","2022")
variaveis = c("Sexo","Faixa Etária","Cirurgia", "Radioterapia", "Quimioterapia", "Hormonioterapia"
              ,"Imunoterapia","Categoria de Atendimento", "Transplante de Medula Óssea")
faixa_etaria = c("00-09", "10-19", "20-29", "30-39", 
                 "40-49", "50-59", "60-69", "70+")

Header = dashboardHeader(
  title = "Análise de Sobrevivêcia com dados da FOSP",
  titleWidth = 600
  
)

Sidebar = dashboardSidebar(
  
  sidebarMenu(
        menuItem(
        "Configurações",
        tabName = "config"
        ),
        menuItem(
          "Resumo",
          tabName = "resum"
        ),
        menuItem(
          "EKM",
          tabName = "EKM"
        ),
        menuItem(
          "Modelo Weibull",
          tabName = "modweibull"
        ),
        menuItem(
          "Modelo de Cox",
          tabName = "Cox"
        )
        )
)

Body    = dashboardBody(
  
  tabItems(
    #Pagina de configurações
    tabItem(tabName = "config",
            fluidPage(
      box(title = 'Configurações desejadas para construção do dashboard',
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          height = "500px",
          fluidRow(column(3,
                            
                            # Selecionar CID
                            pickerInput(
                              inputId = "lucas01",
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
                            )
                            
                            
                            
                
                  ),
                  column(4, checkboxGroupInput(
                    inputId = "lucas02",
                    label = "Variáveis", 
                    choices = variaveis,
                    selected = "Sexo"
                  )),
                  column(5,sliderTextInput(
                    inputId = "lucas03",
                    label = "Anos de diagnostico", 
                    choices = anos,
                    selected = c("2009", "2013"),
                    from_min = "2000", 
                    from_max = "2021",
                    to_min = "2001",
                    to_max = "2022"
                  ),
                  pickerInput(
                    inputId = "lucas04",
                    label = "Ano máximo de acompanhamento", 
                    choices = anos,
                    selected = c("2018")
                  ),
                  conditionalPanel(
                  condition = "input.lucas02.includes('Faixa Etária')",
                  checkboxGroupInput(
                    inputId = "lucas05",
                    label = "Faixa Etária (Caso marcado)", 
                    choices = faixa_etaria)
                                   ))
                  
                         )
                
                       ),
                     )

                )
   
   
  )
 )

UI = dashboardPage(skin = "blue-light",header = Header, sidebar = Sidebar, body = Body)

Server = function(input, output, session) {
  
  observeEvent(
      input$lucas03,{
      input$lucas04
      aux = as.numeric(anos) 
      aux = aux[aux>=as.numeric(input$lucas03[2])]
      aux = as.character(aux)
      updatePickerInput(session,"lucas04",choices = aux)
    }
  )

  
}

shinyApp(UI, Server)
