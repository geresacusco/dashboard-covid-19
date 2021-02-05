tab_files <- list.files(path = "tabs/ui/provincial", full.names = T)
suppressMessages(lapply(tab_files, source))

provincial <- tabPanel(title = "Nivel Provincial", 
                     value = "provincial",

                     fluidRow(
                       box(title = span(icon("magic"), " Seleccione una provincia"),
                         width = 12, uiOutput("selector_prov"), status = "info")
                     ),
                     
                     fluidRow(
                       tabBox(width = 12,
                         title = h4(strong("Semáforo COVID")),
                         id = "tab_semaforo_prov",
                         tabPanel("Tasa de positividad molecular",dygraphOutput("dygraph_prov_positividad_molecular")),
                         tabPanel("Casos",dygraphOutput("dygraph_prov_new_cases")),
                         tabPanel("Defunciones",dygraphOutput("dygraph_prov_new_deaths")),
                         tabPanel("Comparativo Casos",dygraphOutput("dygraph_prov_comparativo_casos")),
                         tabPanel("Comparativo Defunciones",dygraphOutput("dygraph_prov_comparativo_defunciones")),
                         tabPanel("Comparativo Positividad Molecular",dygraphOutput("dygraph_prov_comparativo_posimolecular")))
                     ),
                     
                     fluidRow(
                       box(title = "Casos acumulados de Covid-19 (I)", dygraphOutput("plot3_prov"), textOutput("legend_plot3_prov")),
                       box(title = "Casos acumulados de Covid-19 (II)", dygraphOutput("plot4_prov"), textOutput("legend_plot4_prov"))
                       
                     )
)