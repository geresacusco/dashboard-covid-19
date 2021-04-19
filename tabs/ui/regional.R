regional <- tabPanel(title = "Nivel Regional", 
                 value = "regional",
                 br(),
                 HTML("<h1><center>Análisis de la Pandemia por COVID-19</center></h1>"),
                 column(width = 12,
                        br(), br(),
                        wellPanel(
                HTML("El Análisis de la Pandemia por COVID-19 muestra, de forma clara y directa,
                          información relevante sobre la situación COVID en la region Cusco,
                          para esto se presentan gráficos interactivos a nivel regional,
                          provincial y distrital.</h4>")
                        ),
                        br(),
                   # downloadButton("reporte", "Generar reporte"),
                   fluidRow(
                   box(width = 12, 
                   valueBox(h2(strong("35,700")), "Casos totales 2021", icon = icon("virus", "fa-2x")),
                   valueBox(h2(strong("194,642")), "Pruebas procesadas 2021 (moleculares, rápidas y antigénicas)", icon = icon("vial", "fa-2x")),
                   valueBox(h2(strong("1,488")), "Total Fallecidos 2021", icon = icon("skull-crossbones","fa-2x")))
                 ),

                # Fila 1

                                
                fluidRow(
                  tabBox(title = h4(strong("Mapa Temático")),
                    tabPanel("Total Positivo",
                             tabsetPanel(
                               tabPanel("Gráfico",highchartOutput("map_total_positivo")),
                               tabPanel("Resumen","Este mapa muestra el número de
                                        casos positivos acumulados por", strong("pruebas: molecular, rápida y antigénica"), ", esta útlima desde este 2021. Para COVID-19 desde los
                                        lugares menos afectados hacia los más afectados
                                        de la Región Cusco. El rango comprende los
                                        valores desde el primer día en que se reportó
                                        el primer caso en la Región, hasta la fecha.
                                        Asimismo, el rango continuo de colores muestra el color más claro para las zonas menos
                                        afectadas y el más oscuro para las
                                        más afectadas.", br(), br(),
                                        "Los colores intermedios cambian según las zonas
                                        se encuentren más o menos afectadas.")
                             )),
                    tabPanel("Positivo Rápida",
                             tabsetPanel(
                               tabPanel("Gráfico",highchartOutput("map_pr_positivo")),
                               tabPanel("Resumen","Este mapa muestra el número
                                        de casos positivos acumulados por", strong("pruebas
                                        rápidas"), "para COVID-19 desde los lugares menos
                                        afectados hacia los más afectados de la
                                        Región Cusco. El rango comprende los valores
                                        desde el primer día en que se reportó el
                                        primer caso en la Región, hasta la fecha.
                                        Asimismo, el rango continuo de colores muestra el color más claro para las zonas menos
                                        afectadas y el más oscuro para las
                                        más afectadas.", br(), br(),
                                        "Los colores intermedios cambian según las zonas
                                        se encuentren más o menos afectadas.")
                             )),
                    
            
                    tabPanel("Positivo Molecular",
                             tabsetPanel(
                               tabPanel("Gráfico", highchartOutput("map_pm_positivo")),
                               tabPanel("Resumen", "Este mapa muestra el número
                                        de casos positivos acumulados por", strong("pruebas
                                        moleculares"), "para COVID-19 desde los lugares
                                        menos afectados hacia los más afectados de
                                        la Región. El rango comprende los valores
                                        desde el primer día en que se reportó el primer
                                        caso en la Región, hasta la fecha.
                                        Asimismo, el rango continuo de colores muestra el color más claro para las zonas menos
                                        afectadas y el más oscuro para las
                                        más afectadas.", br(), br(),
                                        "Los colores intermedios cambian según las zonas
                                        se encuentren más o menos afectadas.")
                             ))
                  ),
                  
                  tabBox(title = h4(strong("Semáforo COVID")),
                         
                         
                         
                         tabPanel("Tasa de positividad molecular",
                                  tabsetPanel(
                                    tabPanel("Gráfico", dygraphOutput("dygraph_region_positividad_molecular"),
                                             h4(strong("Descripción de los ejes")),
                                             p(div(strong("Eje Y: "), em("Tasa de positividad de pruebas moleculares (promedio de 7 días)."), style = "color:blue")),
                                             p(div(strong("Eje X: "), em("Días."), style = "color:blue"), "El primer día de la serie corresponde al 13/03/2020, fecha en la cual se reportó
                                          el primer caso confirmado por COVID-19 en la región.")),
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


                                                                           
                         tabPanel("Tasa de positividad antigenica",
                                  tabsetPanel(
                                    tabPanel("Gráfico", dygraphOutput("dygraph_region_positividad_antigenica"),
                                             h4(strong("Descripción de los ejes")),
                                             p(div(strong("Eje Y: "), em("Tasa de positividad de pruebas antigenicas (promedio de 7 días)."), style = "color:blue")),
                                             p(div(strong("Eje X: "), em("Días."), style = "color:blue"), "El primer día de la serie corresponde al 13/03/2020, fecha en la cual se reportó
                                          el primer caso confirmado por COVID-19 en la región.")),
                                    tabPanel("Resumen", "El semáforo COVID-19 de tasa de positividad antigenica muestra
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

                         
                                                  
                      
                    tabPanel("Casos",
                             tabsetPanel(
                               tabPanel("Gráfico",dygraphOutput("dygraph_region_casos"),
                                        h4(strong("Descripción de los ejes")),
                                        p(div(strong("Eje Y: "), em("Casos positivos acumulados por COVID-19 en la Región Cusco."), style = "color:blue")),
                                        p(div(strong("Eje X: "), em("Días."), style = "color:blue"), "El primer día de la serie corresponde al 13/03/2020, fecha en la cual se reportó
                                          el primer caso confirmado por COVID-19 en la región.")),
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






tabPanel("Casos antigenica",
         tabsetPanel(
           tabPanel("Gráfico",dygraphOutput("dygraph_region_casos_antigenica"),
                    h4(strong("Descripción de los ejes")),
                    p(div(strong("Eje Y: "), em("Casos positivos por prueba antigenica por COVID-19 en la Región Cusco."), style = "color:blue")),
                    p(div(strong("Eje X: "), em("Días."), style = "color:blue"), "El primer día de la serie corresponde al 13/03/2020, fecha en la cual se reportó
                                          el primer caso confirmado por COVID-19 en la región.")),
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
                               tabPanel("Gráfico",dygraphOutput("dygraph_region_defunciones"),
                                        h4(strong("Descripción de los ejes")),
                                        p(div(strong("Eje Y: "), em("Defunciones acumuladas por COVID-19 en la Región Cusco."), style = "color:blue")),
                                        p(div(strong("Eje X: "), em("Días."), style = "color:blue"), "El primer día de la serie corresponde al 13/03/2020, fecha en la cual se reportó
                                          el primer caso confirmado por COVID-19 en la región.")),
                               tabPanel("Resumen","El semáforo COVID-19 de defunciones muestra
                               el nivel de riesgo respecto al número total de fallecidos por COVID-19.", br(), br(),
"El color", strong("rojo"), "representa un nivel elevado de riesgo, en esta zona las precauciones aumentan. Hay una
alerta máxima por las defunciones que se presentan día a día. En todo tiempo deberíamos no exponernos
al COVID-19, necesitamos cuidarnos y cuidar a los nuestros.", br(), br(),
"El color", strong("amarillo"), "representa un nivel de riesgo moderado. Aunque el riesgo aún se mantiene, la alerta no es máxima debido a
que el número de defunciones por día ha bajado respecto al color rojo. En todo tiempo deberíamos no
exponernos al COVID-19, necesitamos cuidarnos y cuidar a los nuestros.", br(), br(),
"El color", strong("verde"), "representa que el nivel de riesgo es bajo, la alerta por número de defunciones está
presente, pero no es tan elevada respecto a los otros colores. En todo tiempo deberíamos no
exponernos al COVID-19, necesitamos cuidarnos y cuidar a los nuestros.
")
                             )),


                    tabPanel("Camas",
                             tabsetPanel(
                               tabPanel("Gráfico", dygraphOutput("dygraph_region_camas"),
                                        h4(strong("Descripción de los ejes")),
                                        p(div(strong("Eje Y: "), em("Total de camas disponibles para COVID-19 (UCI, NO UCI, NIVEL II), en la Región Cusco."), style = "color:blue")),
                                        p(div(strong("Eje X: "), em("Días."), style = "color:blue"))),
                               tabPanel("Resumen", "El semáforo COVID-19 de camas muestra
                               el nivel de riesgo respecto al número total de camas UCI,
                               NO UCI y camas de Nivel II por COVID-19.", br(), br(),
"El color", strong("rojo"), "representa un nivel elevado de riesgo, en esta zona las camas se ocupan
por sobre el 60%. Hay una alerta máxima por el uso de las mismas en el día a día.
En todo tiempo deberíamos no exponernos al COVID-19, necesitamos cuidarnos y
cuidar a los nuestros.", br(), br(),
"El color", strong("amarillo"), "representa un nivel de riesgo moderado, en esta zona las camas se ocupan
entre el 30 y 60%. Aunque el riesgo aún se mantiene, la alerta no es
máxima debido a que el número de camas por día ha bajado respecto al color rojo.
En todo tiempo deberíamos no exponernos al COVID-19,
necesitamos cuidarnos y cuidar a los nuestros.", br(), br(),
"El color", strong("verde"), "representa un nivel de riesgo bajo, la alerta por número de
camas usadas para COVID-19 está presente, pero no es tan elevada respecto a los
otros colores, a saber, el uso de estas se encuentra por debajo del 30%. En todo tiempo deberíamos no
exponernos al COVID-19, necesitamos cuidarnos y cuidar a los nuestros.
")
                             ))
                  )                
                ),                
                                
                # Fila 2

                fluidRow(
                  tabBox(title = h4(strong("Casos acumulados de Covid-19: Región Cusco (I)")),
                         tabPanel("Lineal",
                                  tabsetPanel(
                                    tabPanel("Gráfico",dygraphOutput("plot3"),
                                             h4(strong("Descripción de los ejes")),
                                             p(div(strong("Eje Y: "), em("Número total de casos positivos, recuperados, sintomáticos, activos y defunciones por COVID-19, en la región."), style = "color:blue")),
                                             p(div(strong("Eje X: "), em("Días."), style = "color:blue"), "El primer día de la serie corresponde al 13/03/2020, fecha en la cual se reportó
                                          el primer caso confirmado por COVID-19 en la región.")),
                                             
                                    tabPanel("Resumen","Las curvas de esta gráfica muestran
                                    la evolución diaria del acumulado de
                                    casos positivos, recuperados, defunciones, sintomáticos
                                    y activos, para la Región Cusco.", br(), br(),
"Usted puede elegir apreciar el panorama más amplio (acumulado histórico) o mover el
deslizador para apreciar la evolución de las curvas acumuladas en los días más próximos a hoy.
")
                                  ))
                  ),

                  tabBox(title = h4(strong("Casos acumulados de Covid-19: Región Cusco (II)")),
                         tabPanel("Lineal",
                                  tabsetPanel(
                                    tabPanel("Gráfico",dygraphOutput("plot4"),
                                             h4(strong("Descripción de los ejes")),
                                             p(div(strong("Eje Y: "), em("Número total de casos positivos a COVID-19 y número total de habitantes con inicio de síntomas en la región."), style = "color:blue")),
                                             p(div(strong("Eje X: "), em("Días."), style = "color:blue"), "El primer día de la serie corresponde al 13/03/2020, fecha en la cual se reportó
                                          el primer caso confirmado por COVID-19 en la región.")),
                                    tabPanel("Resumen","Las curvas de esta
                                    gráfica muestran la evolución diaria
                                    del número acumulado, hasta la fecha,
                                    de personas en inicio de síntomas para
                                    COVID-19 y el número total de casos
                                    positivos, para la Región Cusco..", br(), br(),
                                    "Usted puede elegir apreciar el panorama más amplio (acumulado histórico)
o mover el deslizador para apreciar la evolución de las curvas acumuladas
en los días más próximos a hoy.")
                                  )),
                         tabPanel("Log",
                                  tabsetPanel(
                                    tabPanel("Gráfico", dygraphOutput("plot5"),
                                             h4(strong("Descripción de los ejes")),
                                             p(div(strong("Eje Y: "), em("Log10 del número total de casos positivos a COVID-19 y log10 del número total de habitantes con inicio de síntomas en la región."), style = "color:blue")),
                                             p(div(strong("Eje X: "), em("Días."), style = "color:blue"), "El primer día de la serie corresponde al 13/03/2020, fecha en la cual se reportó
                                          el primer caso confirmado por COVID-19 en la región.")),
                                    tabPanel("Resumen", "Las curvas de esta
                                    gráfica muestran la evolución diaria
                                    del número acumulado, hasta la fecha,
                                    de personas en inicio de síntomas para
                                    COVID-19 y el número total de casos
                                    positivos. En esa línea, se puede
                                    apreciar un versus entre las
                                    evoluciones de ambas curvas,
                                    se puede ver como se comporta
                                    la curva de aquellas personas
                                    que iniciaron síntomas respecto
                                    a aquellas que ya fueron
                                    confirmadas como contagiadas con el virus.", br(), br(),
"Usted puede elegir apreciar el panorama más amplio (acumulado histórico)
o mover el deslizador para apreciar la evolución de las curvas acumuladas
en los días más próximos a hoy.", br(), br(),
p(strong("Nota: "), "Se aplicó", em("log10"), "a las curvas para suavizar la serie en el tiempo. 
"))
                                  ))
                  )
                )

                
                 # fluidRow(
                 #   box(title = "Casos acumulados de Covid-19: Region Cusco (I)", dygraphOutput("plot3"), textOutput("legend_plot3")),
                 #   box(title = "Casos acumulados de Covid-19: Region Cusco (II)", dygraphOutput("plot4"), textOutput("legend_plot4")),
                 #   



                   

)
)