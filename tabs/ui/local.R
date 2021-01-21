tab_files <- list.files(path = "tabs/ui/local", full.names = T)
suppressMessages(lapply(tab_files, source))

local <- tabPanel(title = "Nivel distrital", 
                      value = "local",
                      hr(),
                  
                  
                  fluidRow(
                    box(title = span(icon("magic"), " Seleccione un distrito"),
                        width = 12, uiOutput("selector_dis"), status = "info")
                  ),
                  
                  fluidRow(
                    tabBox(width = 12,
                           title = h4(strong("Semáforo COVID")),
                           id = "tab_semaforo_dis",
                           tabPanel("Casos",dygraphOutput("dygraph_dis_new_cases")),
                           tabPanel("Defunciones",dygraphOutput("dygraph_dis_new_deaths")))
                  ),
                  
                  fluidRow(
                    box(title = "Casos acumulados de Covid-19 (I)", dygraphOutput("plot3_dis"), textOutput("legend_plot3_dis")),
                    box(title = "Casos acumulados de Covid-19 (II)", dygraphOutput("plot4_dis"), textOutput("legend_plot4_dis"))
                    
                  )
                  
                  
                  
                  
                  
                  
                  )