library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)

rprog_evs_dat <- read.csv("https://raw.githubusercontent.com/alkrasnokutskaya/RProg/master/rprog_evs_dat.csv")

ui <- fluidPage(
    
    titlePanel("Immigration Attitudes in Europe"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("cnt",
                        "Country:",
                        choices = sort(unique(rprog_evs_dat$country))),
            checkboxGroupInput("qs",
                        "Issues:",
                        c("Jobs", "Crime",
                          "Welfare", "Multiculturalism")),
            radioButtons("graph",
                         "Graph:", 
                         choices = c("Density plot", "Histogram"),
                         selected = "Density plot"),
            uiOutput("special")
        ),
        
        mainPanel(
            h5("This page presents data on Immigration 
               Attitudes based on European Values Study 2018."),
            h5("Survey questions behind Jobs, Crime, Welfare 
               and Multiculturalism ask respondentsto asses 
               statements from -10 to 10, most argeed to the least."),
            h5("The statements are the following:"),
            h5("Immigrants take away jobs from [nationality]."),
            h5("Immigrants increase crime problems."),
            h5("Immigrants are a strain on welfare system."),
            h5("Better if immigrants not maintain own customs."),
            fluidRow(DT::dataTableOutput("table")),
            fluidRow(column(6, plotOutput("dp1")),
                     column(6, plotOutput("dp2")),
                     column(6, plotOutput("dp3")),
                     column(6, plotOutput("dp4")),
                     column(12, plotOutput("dp5")),
                     column(6, plotOutput("h1")),
                     column(6, plotOutput("h2")),
                     column(6, plotOutput("h3")),
                     column(6, plotOutput("h4")))
        )
    )
)

server <- function(input, output) {
    
    output$special <- renderUI({
        switch(input$graph,
               "Density plot" = checkboxGroupInput("add", "Additional:", 
                                                   c("Compare all variables")),
               "Histogram" = checkboxGroupInput("add", "Additional:", 
                                       c("Show country average",
                                      "Show European average")))
    })
    
    output$dp1 <- renderPlot({
        if("Jobs" %in% input$qs & "Density plot" %in% input$graph){
        dat <- rprog_evs_dat %>% filter(country == req(input$cnt))
        
        ggplot(dat, aes(x = v185)) + geom_density(fill = "chartreuse4",
                                                  alpha = 0.7)+
            labs(x = "Jobs", y = "Density") +
            xlim(-10, 10)
        }
    })
    output$dp2 <- renderPlot({
        if("Crime" %in% input$qs & "Density plot" %in% input$graph){
            dat <- rprog_evs_dat %>% filter(country == req(input$cnt))
            
            ggplot(dat, aes(x = v186)) + geom_density(fill = "salmon",
                                                      alpha = 0.7) +
                labs(x = "Crime", y = "Density") +
                xlim(-10, 10)
        }
    })
    output$dp3 <- renderPlot({
        if("Welfare" %in% input$qs & "Density plot" %in% input$graph){
            dat <- rprog_evs_dat %>% filter(country == req(input$cnt))
            
            ggplot(dat, aes(x = v187)) + geom_density(fill = "mediumorchid1",
                                                      alpha = 0.7) +
                labs(x = "Welfare", y = "Density") +
                xlim(-10, 10)
        }
    })
    output$dp4 <- renderPlot({
        if("Multiculturalism" %in% input$qs & "Density plot" %in% input$graph){
            dat <- rprog_evs_dat %>% filter(country == req(input$cnt))
            
            ggplot(dat, aes(x = v188)) + geom_density(fill = "turquoise3",
                                                      alpha = 0.7) +
                labs(x = "Multiculturalism", y = "Density") +
                xlim(-10, 10)
        }
    })
    output$dp5 <- renderPlot({
        if("Compare all variables" %in% input$add){
            dat <- rprog_evs_dat %>% filter(country == req(input$cnt))
            
            ggplot(dat, aes(x = v185, fill = "Jobs")) +
                geom_density(alpha = 0.5) +
                geom_density(aes(x=v186, fill = "Crime"), alpha = 0.6) +
                geom_density(aes(x=v187, fill = "Welfare"), alpha = 0.4) +
                geom_density(aes(x=v188, fill = "Multiculturalism"), alpha = 0.5) +
                scale_fill_discrete("Issues") +
                labs(x = "Value", y = "Density")
            
        }
    })
    output$h1 <- renderPlot({
        if("Jobs" %in% input$qs & "Histogram" %in% input$graph){
            dat <- rprog_evs_dat %>% filter(country == req(input$cnt))
            
            h <- ggplot(dat, aes(x = v185)) + geom_histogram(fill = "chartreuse4",
                                                             alpha = 0.7) +
                labs(x = "Jobs", y = "Frequency") +
                if("Show country average" %in% input$add){
                    geom_vline(aes(xintercept = mean(v185)),
                               col='darkgreen',
                               size=1)
                }
                if("Show European average" %in% input$add){ h <-
                    h + geom_vline(aes(xintercept = mean(rprog_evs_dat$v185)),
                                   col='royalblue4',
                                   size=1) +
                    geom_text(aes(x=mean(rprog_evs_dat$v185)+0.5,
                                  label=round(mean(rprog_evs_dat$v185), 1), y = 500),
                              colour="royalblue4")
                }
            h
        }
    })
    output$h2 <- renderPlot({
        if("Crime" %in% input$qs & "Histogram" %in% input$graph){
            dat <- rprog_evs_dat %>% filter(country == req(input$cnt))
            
            h <- ggplot(dat, aes(x = v186)) + geom_histogram(fill = "salmon",
                                                             alpha = 0.7)+
                labs(x = "Crime", y = "Frequency") +
                if("Show country average" %in% input$add){
                    geom_vline(aes(xintercept = mean(v186)),col='tomato3',size=1)
                }
            if("Show European average" %in% input$add){ h <-
                h + geom_vline(aes(xintercept = mean(rprog_evs_dat$v186)),
                               col='royalblue4',
                               size=1) +
                geom_text(aes(x=mean(rprog_evs_dat$v186)+0.5,
                              label=round(mean(rprog_evs_dat$v186), 1), y = 500),
                          colour="royalblue4")
            }
            h
        }
    })
    output$h3 <- renderPlot({
        if("Welfare" %in% input$qs & "Histogram" %in% input$graph){
            dat <- rprog_evs_dat %>% filter(country == req(input$cnt))
            
            h <- ggplot(dat, aes(x = v187)) + geom_histogram(fill = "mediumorchid1",
                                                             alpha = 0.7)+
                labs(x = "Welfare", y = "Frequency") +
                if("Show country average" %in% input$add){
                    geom_vline(aes(xintercept = mean(v187)),col='darkorchid3',size=1)
                }
            if("Show European average" %in% input$add){ h <-
                h + geom_vline(aes(xintercept = mean(rprog_evs_dat$v187)),
                               col='royalblue4',
                               size=1) +
                geom_text(aes(x=mean(rprog_evs_dat$v187)+0.5,
                              label=round(mean(rprog_evs_dat$v187), 1), y = 500),
                          colour="royalblue4")
            }
            h
        }
    })
    output$h4 <- renderPlot({
        if("Multiculturalism" %in% input$qs & "Histogram" %in% input$graph){
            dat <- rprog_evs_dat %>% filter(country == req(input$cnt))
            
            h <- ggplot(dat, aes(x = v188)) + geom_histogram(fill = "turquoise3",
                                                             alpha = 0.7)+
                labs(x = "Multiculturalism", y = "Frequency") +
                if("Show country average" %in% input$add){
                    geom_vline(aes(xintercept = mean(v188)),col='darkslategray4',size=1)
                }
            if("Show European average" %in% input$add){ h <-
                h + geom_vline(aes(xintercept = mean(rprog_evs_dat$v188)),
                               col='royalblue4',
                               size=1) +
                geom_text(aes(x=mean(rprog_evs_dat$v188)+0.5,
                              label=round(mean(rprog_evs_dat$v188), 1), y = 500),
                          colour="royalblue4")
            }
            h
        }
    })
    
}

shinyApp(ui = ui, server = server)
