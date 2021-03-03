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
                         tabPanel("Tasa de positividad molecular",
                                  tabsetPanel(
                                    tabPanel("Gráfico", dygraphOutput("dygraph_prov_positividad_molecular"),
                                             h4(strong("Descripción de los ejes")),
                                             p(div(strong("Eje Y: "), em("Tasa de positividad de pruebas moleculares (promedio de 7 días)."), style = "color:blue")),
                                             p(div(strong("Eje X: "), em("Días."), style = "color:blue"), "El primer día de la serie corresponde al 13/03/2020, fecha en la cual se reportó
                                          el primer caso confirmado por COVID-19 en la región.")
                                          ),
                                    tabPanel("Resumen", "El semáforo COVID-19 de tasa de positividad molecular muestra
                               el nivel de riesgo respecto al número total de personas infectadas por muestras moleculares procesadas
                               por COVID-19.", br(), br(),
                                             p("El color", strong("rojo"), "representa un nivel elevado de riesgo, en esta zona
las precauciones aumentan. En esta zona el nivel y velocidad de contagio por muestras moleculares es mucho más elevada.
Se recomienda salir de casa solo en casos excepcionales y tomando muy en cuenta las medidas
de seguridad sanitaria. ", br(), br(),
                                               "El color", strong("amarillo"), "representa un nivel de riesgo moderado. Aunque el riesgo aún se mantiene, se pueden realizar más
actividades, siempre tomando en consideración las medidas de seguridad sanitaria.", br(), br(),
                                               "El color", strong("verde"), "representa que el nivel de riesgo no es tan elevado respecto a los
otros colores. En todo momento se deberían tomar en cuenta las medidas de seguridad sanitaria. 
"))
                                  )),
                         
                         
#                          tabPanel("Tasa de positividad antigenica",
#                                   tabsetPanel(
#                                     tabPanel("Gráfico", dygraphOutput("dygraph_prov_positividad_antigenica"),
#                                              h4(strong("Descripción de los ejes")),
#                                              p(div(strong("Eje Y: "), em("Tasa de positividad de pruebas antigenicas (promedio de 7 días)."), style = "color:blue")),
#                                              p(div(strong("Eje X: "), em("Días."), style = "color:blue"), "El primer día de la serie corresponde al 13/03/2020, fecha en la cual se reportó
#                                           el primer caso confirmado por COVID-19 en la región.")),
#                                     tabPanel("Resumen", "El semáforo COVID-19 de tasa de positividad antigenica muestra
#                                el nivel de riesgo respecto al número total de personas infectadas por muestras moleculares procesadas
#                                por COVID-19.", br(), br(),
#                                              p("El color", strong("rojo"), "representa un nivel elevado de riesgo, en esta zona
# las precauciones aumentan. En esta zona el nivel y velocidad de contagio por muestras moleculares es mucho más elevada.
# Se recomienda salir de casa solo en casos excepcionales y tomando muy en cuenta las medidas
# de seguridad sanitaria. ", br(), br(),
#                                                "El color", strong("amarillo"), "representa un nivel de riesgo moderado. Aunque el riesgo aún se mantiene, se pueden realizar más
# actividades, siempre tomando en consideración las medidas de seguridad sanitaria.", br(), br(),
#                                                "El color", strong("verde"), "representa que el nivel de riesgo no es tan elevado respecto a los
# otros colores. En todo momento se deberían tomar en cuenta las medidas de seguridad sanitaria. 
# "))
#                                   )),
#                          
                         
                         tabPanel("Casos",
                                  tabsetPanel(
                                    tabPanel("Gráfico", dygraphOutput("dygraph_prov_new_cases"),
                                             h4(strong("Descripción de los ejes")),
                                             p(div(strong("Eje Y: "), em("Tasa de positividad de pruebas moleculares (promedio de 7 días)."), style = "color:blue")),
                                             p(div(strong("Eje X: "), em("Días."), style = "color:blue"), "El primer día de la serie corresponde al 13/03/2020, fecha en la cual se reportó
                                          el primer caso confirmado por COVID-19 en la región.")
                                    ),
                                    tabPanel("Resumen", "El semáforo COVID-19 de casos muestra
                               el nivel de riesgo respecto al número total de contagiados
                               por COVID-19.", br(), br(),
                                             p("El color", strong("rojo"), "representa un nivel elevado de riesgo, en esta zona
las precauciones aumentan. En esta zona el nivel y velocidad de contagio es mucho más elevada.
Se recomienda salir de casa solo en casos excepcionales y tomando muy en cuenta las medidas
de seguridad sanitaria. ", br(), br(),
                                               "El color", strong("amarillo"), "representa un nivel de riesgo moderado. Aunque el riesgo aún se mantiene, se pueden realizar más
actividades, siempre tomando en consideración las medidas de seguridad sanitaria.", br(), br(),
                                               "El color", strong("verde"), "representa que el nivel de riesgo no es tan elevado respecto a los
otros colores. En todo momento se deberían tomar en cuenta las medidas de seguridad sanitaria. 
"))
                                    )),
                         tabPanel("Defunciones",
                                  tabsetPanel(
                                    tabPanel("Gráfico", dygraphOutput("dygraph_prov_new_deaths"),
                                             h4(strong("Descripción de los ejes")),
                                             p(div(strong("Eje Y: "), em("Tasa de positividad de pruebas moleculares (promedio de 7 días)."), style = "color:blue")),
                                             p(div(strong("Eje X: "), em("Días."), style = "color:blue"), "El primer día de la serie corresponde al 13/03/2020, fecha en la cual se reportó
                                          el primer caso confirmado por COVID-19 en la región.")
                                    ),
                                    tabPanel("Resumen", "El semáforo COVID-19 de casos muestra
                               el nivel de riesgo respecto al número total de contagiados
                               por COVID-19.", br(), br(),
                                             p("El color", strong("rojo"), "representa un nivel elevado de riesgo, en esta zona
las precauciones aumentan. En esta zona el nivel y velocidad de contagio es mucho más elevada.
Se recomienda salir de casa solo en casos excepcionales y tomando muy en cuenta las medidas
de seguridad sanitaria. ", br(), br(),
                                               "El color", strong("amarillo"), "representa un nivel de riesgo moderado. Aunque el riesgo aún se mantiene, se pueden realizar más
actividades, siempre tomando en consideración las medidas de seguridad sanitaria.", br(), br(),
                                               "El color", strong("verde"), "representa que el nivel de riesgo no es tan elevado respecto a los
otros colores. En todo momento se deberían tomar en cuenta las medidas de seguridad sanitaria. 
"))
                                  )))
                         # tabPanel("Comparativo Casos",dygraphOutput("dygraph_prov_comparativo_casos")),
                         # tabPanel("Comparativo Defunciones",dygraphOutput("dygraph_prov_comparativo_defunciones")),
                         # tabPanel("Comparativo Positividad Molecular",dygraphOutput("dygraph_prov_comparativo_posimolecular")))
                     ),
                     
                     fluidRow(
                       box(title = "Casos acumulados de Covid-19", dygraphOutput("plot3_prov"), textOutput("legend_plot3_prov")))
                       # box(title = "Casos acumulados de Covid-19 (II)", dygraphOutput("plot4_prov"), textOutput("legend_plot4_prov"))
                       

)