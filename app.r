#carpeta p-Proy6 app
library(dplyr)
library(class)
library(stringr)
library(sf)
library(shiny)
library(shinydashboard)
library(ggplot2)

#l--------------------data reading

#setwd("~/R/ejerc/shuny app")

#dir()
#getwd()

viofamyearstate<-read.csv("viofamyearstate.csv")

viofamyearstate<-select(viofamyearstate, -X)

options(shiny.sanitize.errors = FALSE)
#-------------sidebar menu
sidebar <- dashboardSidebar(
    sidebarMenu(id="menu1",
        #Top por Anio
        menuItem("Top por Anio", tabName = "top", icon = icon("line-chart")),
        #Comparativa de anios
        menuItem("Contraste entre anios", tabName = "contra", icon = icon("area-chart")),
        #mapas coroplecticos
        menuItem("Mapas Coroplecticos", tabName = "mapa", icon = icon("map-marker")),
        #Histogramas por Por Anio
        menuItem("Histogramas", tabName = "histo", icon = icon("bar-chart"))
        
        
    )
)
#------------cuerpo
#Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
body <- dashboardBody(
    tabItems(
        #Top por anio
        tabItem(tabName = "top", 
                fluidRow(
                    box(solidHeader = TRUE, background = "navy",
                    conditionalPanel(condition="input.menu1=='top'",
 
                                     p("Elige el anio para ver el top"),
                                     selectInput("year", "Seleccione el anio",
                                                 choices = unique(viofamyearstate$Anio)),
                                     p("Elige el mes para ver el top"),
                                     selectInput("mes", "Seleccione el mes",
                                                 choices = c("Enero", "Febrero", "Marzo",
                                                             "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")),
                                     radioButtons("choice","Choose an option",
                                                  choices=c("Menor incidencia" = 1,
                                                            "Mayor incidencia" = 2))
                    )
                ),
                #fluidRow(#h3("Tabla de Contraste", align = "center" ),
                         
                         box(solidHeader = TRUE, background = "teal",
                            h3("Top 10 de Estados", align = "center"),
                             conditionalPanel(condition="input.choice==1", 
                                              tableOutput("table")),
                             conditionalPanel(condition="input.choice==2",
                                              tableOutput("table2"))
                         )
                         
                )
        )

    )#close tabItems()
)#close body

#----------------interfaz UI------------------
ui <- dashboardPage(
    dashboardHeader(title = "Proyecto final"),
    sidebar,
    body
)
#-------------------server------------------------
server <- function(input, output) {
    
    
    
    
    output$table <- renderTable({ 
        table1<- switch (input$mes,
                         "Enero" = arrange(filter(viofamyearstate,  Anio%in%input$year), Enerosum )[1:10,c(1:3,4)],
                         "Febrero"=arrange(filter(viofamyearstate, Anio%in%input$year), Febrerosum )[1:10,c(1:3,6)],
                         "Marzo"=arrange(filter(viofamyearstate,  Anio%in%input$year), Marzosum )[1:10,c(1:3,8)],
                         "Abril"=arrange(filter(viofamyearstate,  Anio%in%input$year), Abrilsum )[1:10,c(1:3,10)],
                         "Mayo"=arrange(filter(viofamyearstate,  Anio%in%input$year), Mayosum )[1:10,c(1:3,12)],
                         "Junio"=arrange(filter(viofamyearstate,  Anio%in%input$year), Juniosum )[1:10,c(1:3,14)],
                         "Julio"=arrange(filter(viofamyearstate,  Anio%in%input$year), Juliosum )[1:10,c(1:3,16)],
                         "Agosto"=arrange(filter(viofamyearstate,  Anio%in%input$year), Agostosum )[1:10,c(1:3,18)],
                         "Semptiembre"=arrange(filter(viofamyearstate,  Anio%in%input$year), Semptiembresum )[1:10,c(1:3,20)],
                         "Octubre"=arrange(filter(viofamyearstate,  Anio%in%input$year), Octubresum )[1:10,c(1:3,22)],
                         "Noviembre"=arrange(filter(viofamyearstate,  Anio%in%input$year), Noviembresum )[1:10,c(1:3,24)],
                         "Diciembre"=arrange(filter(viofamyearstate,  Anio%in%input$year), Diciembresum )[1:10,c(1:3,26)]
                         
        )
    })
    
    output$table2 <- renderTable({
        switch (input$mes,
                "Enero" = arrange(filter(viofamyearstate,  Anio%in%input$year), desc(Enerosum) )[1:10,c(1:3,4)],
                "Febrero"=arrange(filter(viofamyearstate,  Anio%in%input$year), desc(Febrerosum) )[1:10,c(1:3,6)],
                "Marzo"=arrange(filter(viofamyearstate,  Anio%in%input$year), desc(Marzosum) )[1:10,c(1:3,8)],
                "Abril"=arrange(filter(viofamyearstate,  Anio%in%input$year), desc(Abrilsum) )[1:10,c(1:3,10)],
                "Mayo"=arrange(filter(viofamyearstate,  Anio%in%input$year), desc(Mayosum) )[1:10,c(1:3,12)],
                "Junio"=arrange(filter(viofamyearstate,  Anio%in%input$year), desc(Juniosum) )[1:10,c(1:3,14)],
                "Julio"=arrange(filter(viofamyearstate,  Anio%in%input$year), desc(Juliosum) )[1:10,c(1:3,16)],
                "Agosto"=arrange(filter(viofamyearstate,  Anio%in%input$year), desc(Agostosum) )[1:10,c(1:3,18)],
                "Semptiembre"=arrange(filter(viofamyearstate,  Anio%in%input$year), desc(Semptiembresum) )[1:10,c(1:3,20)],
                "Octubre"=arrange(filter(viofamyearstate,  Anio%in%input$year), desc(Octubresum) )[1:10,c(1:3,22)],
                "Noviembre"=arrange(filter(viofamyearstate,  Anio%in%input$year), desc(Noviembresum) )[1:10,c(1:3,24)],
                "Diciembre"=arrange(filter(viofamyearstate,  Anio%in%input$year), desc(Diciembresum) )[1:10,c(1:3,26)]
                
        )
    })
    
    
} #termina server

shinyApp(ui, server)

