shinyServer(function(input, output, session){
  #################################################### Set up loading screen ----
  Sys.sleep(3) # plots
  waiter_hide()
  
  #################################################### Leer data ----
  source("01_scripts/read_data.R")
  
  # Leer data para bubble plot (experimental)
  # data_prov <- fread("https://raw.githubusercontent.com/geresacusco/dashboard-covid-19/main/data/data_provincial.csv", keepLeadingZeros = TRUE)
  # data_prov$fecha <- as.Date(data_prov$fecha)
  # data_prov <- subset(data_prov, fecha > as.Date("2020-03-12") & fecha < Sys.Date() - 1)
  # data_densidad_prov <- fread("https://raw.githubusercontent.com/geresacusco/dashboard-covid-19/main/data/densidad/densidad_provincia.csv")
  # bubble_data <- merge(data_prov, data_densidad_prov, by = "provincia")
  # bubble_data <- mutate(bubble_data, incidencia = total_positivo/poblacion )
  
  #################################################### Hacer data reactiva y subset por provincia y distrito ----
  
  ### Make data reactive ----
  
  # Map data
  
    map_district <- read_data_map_district()
    data_dis <- read_data_dis()
  # Regional (semaforo V2)
  
  data_dpto_r <- reactive({
    data_dpto <- read_data_dpto()
  })

  data_dpto_r2 <- reactive({
    data_dpto <- read_data_dpto()
    data_dpto <- filter(data_dpto, fecha > "2021-01-01")
  })
    
  # Provincial (semaforo V2)
  
  data_prov_r <- reactive({
    data_prov <- read_data_prov()
  })
  
  data_prov_r2 <- reactive({
    data_prov <- read_data_prov()
    data_prov <- filter(data_prov, fecha > "2021-01-01")
    
  })
  
  # Distrital (semaforo v2)
  
  data_dis_r <- reactive({
    data_dis <- read_data_dis()
  })  
  
  data_dis_r2 <- reactive({
    data_dis <- read_data_dis()
    data_dis <- filter(data_dis, fecha > "2021-01-01")
    
  })  
  
  # Cusco (semaforo)
  data_cusco_r <- reactive({
    data_cusco <- read_data_cusco()
  })

  # Valores del semáforo provincial
  data_semaforo_r <- reactive({
    data_semaforo <- read_semaforo()
  })
  
  # Valores del semáforo distrital
  data_semaforo_dis_r <- reactive({
    data_semaforo_dis <- read_semaforo_dis()
  })
  
  # Camas
  data_beds <- reactive({
    data_camas <- read_data_beds()
  })
  
  # Provincial (test)
  data_corona <- reactive({
    data_res <- read_data_corona()
  })

  ##### Selectores ----
  
  ## Provincia
  
  # Province selector -----
  output$selector_prov <- renderUI({
    
    pickerInput(
      inputId = "prov",
      label = "Elige una provincia:", 
      choices = data_prov_r()[, unique(provincia)],
      selected = "CUSCO",
      options = list(
        `live-search` = TRUE,
        style = "btn-info",
        maxOptions = 7
      )
    )
    
  })
  
  # Subset data by province ----
  data_prov_subset <- reactive({
    shiny::req(input$prov)
    data_res_prov <- copy(data_prov_r()[.(input$prov), on = .(provincia)])
    data_res_prov
  })
  
  # Subset semaforo por provincia ----
  data_semaforo_subset <- reactive({
    shiny::req(input$prov)
    data_trat <- copy(data_semaforo_r()[.(input$prov), on = .(provincia)])
    data_trat
  })

  ## Distrito
  
  # District selector -----
  output$selector_dis <- renderUI({
    
    pickerInput(
      inputId = "dis",
      label = "Elige un distrito:", 
      choices = data_dis_r()[, unique(distrito)],
      selected = "CUSCO",
      options = list(
        `live-search` = TRUE,
        style = "btn-info",
        maxOptions = 7
      )
    )
    
  })
  
  # Subset data by district ----
  data_dis_subset <- reactive({
    shiny::req(input$dis)
    data_res_dis <- copy(data_dis_r()[.(input$dis), on = .(distrito)])
    data_res_dis
  })

  # Subset semaforo por distrito ----
  data_semaforo_dis_subset <- reactive({
    shiny::req(input$dis)
    data_trat_dis <- copy(data_semaforo_dis_r()[.(input$dis), on = .(distrito)])
    data_trat_dis
  })
  
  # Colores ----
  
  myPal1 <- c(
    rgb(3, 4, 94, maxColorValue = 255))
  print(myPal1)
  
  myPal2 <- c(
    rgb(255, 134, 0, maxColorValue = 255),
    rgb(3, 4, 94, maxColorValue = 255))
  print(myPal2)
  
  myPal3 <- c(
    rgb(255, 134, 0, maxColorValue = 255),
    rgb(251, 54, 64, maxColorValue = 255),
    rgb(3, 4, 94, maxColorValue = 255))
  print(myPal3)
  
  myPal4 <- c(
    rgb(255, 134, 0, maxColorValue = 255),
    rgb(251, 54, 64, maxColorValue = 255),
    rgb(3, 4, 94, maxColorValue = 255),
    rgb(115, 113, 252, maxColorValue = 255))
  print(myPal4)
  
  myPal5 <- c(
    rgb(255, 134, 0, maxColorValue = 255),
    rgb(251, 54, 64, maxColorValue = 255),
    rgb(6, 214, 160, maxColorValue = 255),
    rgb(115, 113, 252, maxColorValue = 255),
    rgb(3, 4, 94, maxColorValue = 255))
  print(myPal5)
  
  
  ######################################## Código de gráficos ----
  
  ### 1) Código para graficar el semáforo COVID ----

  # Formato de legenda  
 valueFormatter_rounded = "function(y) {
    return y.toFixed(0) ;}"
  
 valueFormatter_percent = "function(y) {
    return y.toFixed(0) + '%';}"

  ## Casos
  output$dygraph_region_casos <- renderDygraph({
    
    dygraph(data_dpto_r()[, .(fecha, positivo)]) %>%
      dyAxis("x", label = "Fecha") %>%
      dySeries("positivo",label = "Número de casos") %>%
      dyAxis("y", label = "Número de casos",valueFormatter = JS(valueFormatter_rounded) ) %>%
      dyRangeSelector(dateWindow = c(data_dpto_r()[, max(fecha) - 80], data_dpto_r()[, max(fecha) + 1]),
                      fillColor = "#003169", strokeColor = "00909e") %>%
      dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
                fillGraph = FALSE, fillAlpha = 0.4,
                colors = c("#003169", "", "")) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
      dyLegend(width = 150, show = "follow", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE) %>%
      dyRoller(showRoller = FALSE, rollPeriod = 7) %>%
      dyShading(from = "0", to = "372.85", color = "rgb(116, 199, 184, 0.7)", axis = "y") %>%
      dyShading(from = "372.85", to = "1118.355", color = "rgb(255, 205, 163, 0.7)", axis = "y") %>%
      dyShading(from = "1118.355", to = "1491.14", color = "rgb(239, 79, 79, 0.7)", axis = "y")
    
  })
  
  
  ## Casos antigenica
  output$dygraph_region_casos_antigenica <- renderDygraph({
    
    dygraph(data_dpto_r2()[, .(fecha, positivo_antigenica)]) %>%
      dyAxis("x", label = "Fecha") %>%
      dySeries("positivo_antigenica",label = "Número de casos") %>%
      dyAxis("y", label = "Número de casos",valueFormatter = JS(valueFormatter_rounded) ) %>%
      dyRangeSelector(dateWindow = c(data_dpto_r2()[, max(fecha) - 80], data_dpto_r2()[, max(fecha) + 1]),
                      fillColor = "#003169", strokeColor = "00909e") %>%
      dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
                fillGraph = FALSE, fillAlpha = 0.4,
                colors = c("#003169", "", "")) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
      dyLegend(width = 150, show = "follow", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE) %>%
      dyRoller(showRoller = FALSE, rollPeriod = 7)
      # dyShading(from = "0", to = "40.5", color = "rgb(116, 199, 184, 0.7)", axis = "y") %>%
      # dyShading(from = "40.5", to = "121.5", color = "rgb(255, 205, 163, 0.7)", axis = "y") %>%
      # dyShading(from = "121.5", to = "162", color = "rgb(239, 79, 79, 0.7)", axis = "y")
    
  })

  
  ## Defunciones
  output$dygraph_region_defunciones <- renderDygraph({
    
    dygraph(data_dpto_r()[, .(fecha, defunciones)]) %>%
      dySeries("defunciones", label = "Número de defunciones") %>%
      dyAxis("x", label = "Fecha") %>%
      dyAxis("y", label = "Número de defunciones",valueFormatter = JS(valueFormatter_rounded) ) %>%
      dyRangeSelector(dateWindow = c(data_dpto_r()[, max(fecha) - 80], data_dpto_r()[, max(fecha) + 1]),
                      fillColor = "#142850", strokeColor = "#222d32") %>%
      dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
                fillGraph = FALSE, fillAlpha = 0.4,
                colors = c("#142850", "", "")) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
      dyLegend(width = 150, show = "follow", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)  %>%
      dyRoller(showRoller = FALSE, rollPeriod = 7) %>%
      dyShading(from = "0", to = "6.965", color = "rgb(116, 199, 184, 0.7)", axis = "y") %>%
      dyShading(from = "6.965", to = "20.895", color = "rgb(255, 205, 163, 0.7)", axis = "y") %>%
      dyShading(from = "20.895", to = "27.86", color = "rgb(239, 79, 79, 0.7)", axis = "y")
    
  })
  
  ## Camas
  output$dygraph_region_camas <- renderDygraph({
    
    dygraph(data_beds()[, .(DateRep, UCI_percent, NOUCI_percent, NIVELII_percent)]) %>%
      dyAxis("x", label = "Fecha") %>%
      dyAxis("y", label = "Porcentaje de ocupación UCI",valueFormatter = JS(valueFormatter_rounded) ) %>%
      dySeries("UCI_percent", label = "% Ocupacion UCI") %>%
      dySeries("NOUCI_percent", label = "% Ocupacion No UCI") %>%
      dySeries("NIVELII_percent", label = "% Ocupacion Nivel II") %>%
      dyRangeSelector(dateWindow = c(data_beds()[, max(DateRep) - 80], data_beds()[, max(DateRep) + 1]),
                      fillColor = c("#03045e", "#3a0ca3","#7371fc"), strokeColor = "#03045e") %>%
      dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
                fillGraph = FALSE, fillAlpha = 0.4,
                colors = c("#03045e", "#3a0ca3","#7371fc")) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
      dyLegend(show = "follow", showZeroValues = TRUE, labelsDiv = NULL,
               labelsSeparateLines = FALSE, hideOnMouseOut = TRUE) %>%
      dyCSS(textConnection("
                  .dygraph-legend {
                  width: 150 !important;
                  min-width: 150px;
                  color: #000445;
                  background-color: rgb(250, 250, 250, 0.4) !important;
                  padding-left:5px;
                  border-color:#000445;
                  border-style:solid;
                  border-width:3px;
                  transition:0s 2s;
                  z-index: 80 !important;
                  box-shadow: 2px 2px 5px rgba(0, 0, 0, .3);
                  border-radius: 0px;
                  }
                  .dygraph-legend:hover{
                  transform: translate(-50%);
                  transition: 3s;
                  }
                
                  .dygraph-legend > span {
                    color: #000445;
                    padding-left:3px;
                    padding-right:3px;
                    margin-left:-3px;
                    background-color: rgb(250, 250, 250, 0.4) !important;
                    display: block;
                  }
                
                  .dygraph-legend > span:first-child {
                    margin-top:3px;
                  }

                  .dygraph-legend > span > span{
                    display: inline;
                  }
                  
                  .highlight {
                    border-left: 3px solid #000445;
                    padding-left:3px !important;
                  }
                ")
      ) %>%
      dyShading(from = "0", to = "25", color = "rgb(116, 199, 184, 0.7)", axis = "y") %>%
      dyShading(from = "25", to = "65", color = "rgb(255, 205, 163, 0.7)", axis = "y") %>%
      dyShading(from = "65", to = "150", color = "rgb(239, 79, 79, 0.7)", axis = "y")
    
  })
  
  ## Positividad molecular
  
  output$dygraph_region_positividad_molecular <- renderDygraph({
    
    dygraph(data_dpto_r()[, .(fecha, posi_molecular_percent)]) %>%
      dySeries("posi_molecular_percent", label = "Tasa de positividad") %>%
      dyAxis("x", label = "Fecha") %>%
      dyAxis("y", label = "Tasa de positividad", valueFormatter = JS(valueFormatter_percent) ) %>%
      dyRangeSelector(dateWindow = c(data_dpto_r()[, max(fecha) - 80], data_dpto_r()[, max(fecha) + 1]),
                      fillColor = "#142850", strokeColor = "#222d32") %>%
      dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
                fillGraph = FALSE, fillAlpha = 0.4,
                colors = c("#142850", "", "")) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
      dyLegend(width = 150, show = "follow", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)  %>%
      dyRoller(showRoller = FALSE, rollPeriod = 7) %>%
      dyShading(from = "0", to = "15", color = "rgb(116, 199, 184, 0.7)", axis = "y") %>%
      dyShading(from = "15", to = "30", color = "rgb(255, 205, 163, 0.7)", axis = "y") %>%
      dyShading(from = "30", to = "100", color = "rgb(239, 79, 79, 0.7)", axis = "y")
    
  })
  
  ## Positividad antigenica
  
  output$dygraph_region_positividad_antigenica <- renderDygraph({
    
    dygraph(data_dpto_r2()[, .(fecha, posi_antigenica_percent)]) %>%
      dySeries("posi_antigenica_percent", label = "Tasa de positividad") %>%
      dyAxis("x", label = "Fecha") %>%
      dyAxis("y", label = "Tasa de positividad", valueFormatter = JS(valueFormatter_percent), valueRange = c(0, 40) ) %>%
      dyRangeSelector(dateWindow = c(data_dpto_r2()[, max(fecha) - 30], data_dpto_r2()[, max(fecha) + 1]),
                      fillColor = "#142850", strokeColor = "#222d32") %>%
      dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
                fillGraph = FALSE, fillAlpha = 0.4,
                colors = c("#142850", "", "")) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
      dyLegend(width = 150, show = "follow", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)  %>%
      dyRoller(showRoller = FALSE, rollPeriod = 7) %>%
      dyShading(from = "0", to = "15", color = "rgb(116, 199, 184, 0.7)", axis = "y") %>%
      dyShading(from = "15", to = "30", color = "rgb(255, 205, 163, 0.7)", axis = "y") %>%
      dyShading(from = "30", to = "100", color = "rgb(239, 79, 79, 0.7)", axis = "y")
    
  })
  

  ### 2) Código para graficar el mapa del cusco ----
  
  # Casos totales
  
  data_positivo <- data_dis %>% 
    group_by(IDDIST) %>% 
    do(item = list(
      IDDIST = first(.$IDDIST),
      sequence = .$total_positivo,
      total_positivo = first(.$total_positivo))) %>% 
    .$item
  
  output$map_total_positivo <- renderHighchart ({  
  highchart(type = "map") %>%
    hc_add_series(
      data = data_positivo,
      name = "Casos totales",
      mapData = map_district,
      joinBy = 'IDDIST',
      borderWidth = 0.01
    ) %>% 
    hc_mapNavigation(enabled = TRUE) %>%
    hc_colorAxis(minColor = "#06d6a0", maxColor = "#03045e")  %>%
    hc_legend(
      layout = "vertical",
      reversed = TRUE,
      floating = TRUE,
      align = "right"
    ) %>% 
    hc_motion(
      enabled = TRUE,
      autoPlay = TRUE,
      axisLabel = "fecha",
      labels = sort(unique(data_dis$fecha)),
      series = 0,
      updateIterval = 50,
      magnet = list(
        round = "floor",
        step = 0.1
      )
    ) %>% 
    hc_chart(marginBottom  = 100)
  })

  # Casos prueba rapida
  
  data_positivo_rapida <- data_dis %>% 
    group_by(IDDIST) %>% 
    do(item = list(
      IDDIST = first(.$IDDIST),
      sequence = .$total_positivo_rapida,
      total_positivo = first(.$total_positivo_rapida))) %>% 
    .$item
  
  
  output$map_pr_positivo <- renderHighchart ({  
    highchart(type = "map") %>%
      hc_add_series(
        data = data_positivo_rapida,
        name = "Casos totales",
        mapData = map_district,
        joinBy = 'IDDIST',
        borderWidth = 0.01
      ) %>% 
      hc_mapNavigation(enabled = TRUE) %>%
      hc_colorAxis(minColor = "#ff8600", maxColor = "#03045e")  %>%
      hc_legend(
        layout = "vertical",
        reversed = TRUE,
        floating = TRUE,
        align = "right"
      ) %>% 
      hc_motion(
        enabled = TRUE,
        autoPlay = TRUE,
        axisLabel = "fecha",
        labels = sort(unique(data_dis$fecha)),
        series = 0,
        updateIterval = 50,
        magnet = list(
          round = "floor",
          step = 0.1
        )
      ) %>% 
      hc_chart(marginBottom  = 100)
  })
  
  # Casos moleculares
  
  data_positivo_molecular <- data_dis %>% 
    group_by(IDDIST) %>% 
    do(item = list(
      IDDIST = first(.$IDDIST),
      sequence = .$total_positivo_molecular,
      total_positivo = first(.$total_positivo_molecular))) %>% 
    .$item
  
  output$map_pm_positivo <- renderHighchart ({  
    highchart(type = "map") %>%
      hc_add_series(
        data = data_positivo_molecular,
        name = "Casos totales",
        mapData = map_district,
        joinBy = 'IDDIST',
        borderWidth = 0.01
      ) %>% 
      hc_mapNavigation(enabled = TRUE) %>%
      hc_colorAxis(minColor = "#7371fc", maxColor = "#03045e")  %>%
      hc_legend(
        layout = "vertical",
        reversed = TRUE,
        floating = TRUE,
        align = "right"
      ) %>% 
      hc_motion(
        enabled = TRUE,
        autoPlay = TRUE,
        axisLabel = "fecha",
        labels = sort(unique(data_dis$fecha)),
        series = 0,
        updateIterval = 50,
        magnet = list(
          round = "floor",
          step = 0.1
        )
      ) %>% 
      hc_chart(marginBottom  = 100)
  })
        
  ### 3) Código para graficar el bubble plot ----
  
  # datacusco_str <- distinct(bubble_data, provincia, .keep_all = TRUE) %>%
  #   mutate(x = densidad_poblacional, y = incidencia, z = poblacion)
  # 
  # datacusco_seq <- bubble_data %>%
  #   arrange(provincia, fecha) %>%
  #   group_by(provincia) %>%
  #   do(sequence = list_parse(select(., x = densidad_poblacional, y = incidencia, z = poblacion)))
  # 
  # 
  # data_cusco <- left_join(datacusco_str, datacusco_seq)
  # 
  # # summarise_if(data_raw, is.numeric, funs(min, max)) %>%
  # #   tidyr::gather(key, value) %>%
  # #   arrange(key)
  # 
  # output$bubble1 <- renderHighchart ({
  # highchart() %>%
  #   hc_add_series(data_cusco, type = "bubble",
  #                 minSize = 0, maxSize = 30) %>%
  #   hc_motion(enabled = TRUE, series = 0, labels = unique(bubble_data$fecha),
  #             loop = TRUE, autoPlay = TRUE,
  #             updateInterval = 50, magnet = list(step =  0.1)) %>%
  #   hc_plotOptions(series = list(showInLegend = FALSE)) %>%
  #   hc_xAxis(type = "logarithmic", min = 12, max = 1000) %>%
  #   hc_yAxis(min = 0, max = 0.15) %>%
  #   hc_add_theme(hc_theme_smpl())
  # })

  
  ## 3)  Codigo gráfico 3 (Paquete Dygraph)
  
  output$plot3 <- renderDygraph({
    dygraph(data_dpto_r()[, .(fecha, total_positivo, total_recuperado, total_sintomaticos, total_defunciones)],) %>%
      dySeries("total_positivo", label = "Positivos") %>%
      dySeries("total_recuperado", label = "Recuperados") %>%
      dySeries("total_sintomaticos", label = "Sintomáticos") %>%
      dySeries("total_defunciones", label = "Defunciones") %>%
      dyLegend(show = "follow", showZeroValues = TRUE, labelsDiv = NULL,
               labelsSeparateLines = FALSE, hideOnMouseOut = TRUE) %>%
      dyCSS(textConnection("
                  .dygraph-legend {
                  width: 150 !important;
                  min-width: 150px;
                  color: #000445;
                  background-color: rgb(250, 250, 250, 0.4) !important;
                  padding-left:5px;
                  border-color:#000445;
                  border-style:solid;
                  border-width:3px;
                  transition:0s 2s;
                  z-index: 80 !important;
                  box-shadow: 2px 2px 5px rgba(0, 0, 0, .3);
                  border-radius: 0px;
                  }
                  .dygraph-legend:hover{
                  transform: translate(-50%);
                  transition: 3s;
                  }
                
                  .dygraph-legend > span {
                    color: #000445;
                    padding-left:3px;
                    padding-right:3px;
                    margin-left:-3px;
                    background-color: rgb(250, 250, 250, 0.4) !important;
                    display: block;
                  }
                
                  .dygraph-legend > span:first-child {
                    margin-top:3px;
                  }

                  .dygraph-legend > span > span{
                    display: inline;
                  }
                  
                  .highlight {
                    border-left: 3px solid #000445;
                    padding-left:3px !important;
                  }
                ")
      ) %>%
      dyRangeSelector() %>%
      dyOptions(colors = myPal5)
  })


  ## 4)  Codigo gráfico 4 (lineal) (Paquete Dygraph)

  output$plot4 <- renderDygraph({
    dygraph(data_dpto_r()[, .(fecha, total_positivo, total_inicio)],) %>%
      dySeries("total_positivo", label = "Total de casos positivos por covid-19") %>%
      dySeries("total_inicio", label = "Total de casos de inicio de síntomas por covid-19") %>%
      dyLegend(show = "follow", showZeroValues = TRUE, labelsDiv = NULL,
               labelsSeparateLines = FALSE, hideOnMouseOut = TRUE) %>%
      dyCSS(textConnection("
                  .dygraph-legend {
                  width: 150 !important;
                  min-width: 150px;
                  color: #000445;
                  background-color: rgb(250, 250, 250, 0.4) !important;
                  padding-left:5px;
                  border-color:#000445;
                  border-style:solid;
                  border-width:3px;
                  transition:0s 2s;
                  z-index: 80 !important;
                  box-shadow: 2px 2px 5px rgba(0, 0, 0, .3);
                  border-radius: 0px;
                  }
                  .dygraph-legend:hover{
                  transform: translate(-110%);
                  transition: 3s;
                  }
                
                  .dygraph-legend > span {
                    color: black;
                    padding-left:5px;
                    padding-right:2px;
                    margin-left:-5px;
                    background-color: rgb(250, 250, 250, 0.4) !important;
                    display: block;
                  }
                
                  .dygraph-legend > span:first-child {
                    margin-top:2px;
                  }

                  .dygraph-legend > span > span{
                    display: inline;
                  }
                  
                  .highlight {
                    border-left: 3px solid #000445;
                    padding-left:3px !important;
                  }
                ")
      ) %>%
      dyRangeSelector() %>%
      dyOptions(colors = myPal2)
  })

  ## 5)  Codigo gráfico 5 (logaritmo) (Paquete Dygraph)
  
  output$plot5 <- renderDygraph({
    dygraph(data_dpto_r()[, .(fecha, xposi, xini)],) %>%
      dySeries("xposi", label = "Total de casos positivos por covid-19") %>%
      dySeries("xini", label = "Total de casos de inicio de síntomas por covid-19") %>%
      dyLegend(show = "follow", showZeroValues = TRUE, labelsDiv = NULL,
               labelsSeparateLines = FALSE, hideOnMouseOut = TRUE) %>%
      dyCSS(textConnection("
                  .dygraph-legend {
                  width: 150 !important;
                  min-width: 150px;
                  color: #000445;
                  background-color: rgb(250, 250, 250, 0.4) !important;
                  padding-left:5px;
                  border-color:#000445;
                  border-style:solid;
                  border-width:3px;
                  transition:0s 2s;
                  z-index: 80 !important;
                  box-shadow: 2px 2px 5px rgba(0, 0, 0, .3);
                  border-radius: 0px;
                  }
                  .dygraph-legend:hover{
                  transform: translate(-50%);
                  transition: 3s;
                  }
                
                  .dygraph-legend > span {
                    color: #000445;
                    padding-left:3px;
                    padding-right:3px;
                    margin-left:-3px;
                    background-color: rgb(250, 250, 250, 0.4) !important;
                    display: block;
                  }
                
                  .dygraph-legend > span:first-child {
                    margin-top:3px;
                  }

                  .dygraph-legend > span > span{
                    display: inline;
                  }
                  
                  .highlight {
                    border-left: 3px solid #000445;
                    padding-left:3px !important;
                  }
                ")
      ) %>%
      dyRangeSelector() %>%
      dyOptions(colors = myPal2)
  })
  
  ############################ Código para graficar la data provincial ----
  
  # Grafico provincias del Cusco
  
  
  ## Semaforo Provincial: Casos
  output$dygraph_prov_new_cases <- renderDygraph({
    
    shiny::req(input$prov)
    
    dygraph(data_prov_subset()[, .(fecha, positivo)], main = input$prov) %>%
      dyAxis("x", label = "Fecha") %>%
      # dyAxis("y", label = "Número de casos", valueFormatter = JS(valueFormatter_rounded) ) %>%
      dySeries("positivo", label = "Número de casos") %>%
      dyRangeSelector(dateWindow = c(data_prov_subset()[, max(fecha) - 50], data_prov_subset()[, max(fecha) + 1]),
                      fillColor = "#003169", strokeColor = "00909e") %>%
      dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
                fillGraph = FALSE, fillAlpha = 0.4,
                drawPoints = FALSE, pointSize = 3,
                pointShape = "circle",
                colors = c("#003169")) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
      dyLegend(width = 150, show = "follow", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)  %>%
      dyRoller(showRoller = FALSE, rollPeriod = 7) %>%
      dyShading(from = data_semaforo_subset()[, .(cases_q0)], to = data_semaforo_subset()[, .(cases_q1)], color = "rgb(116, 199, 184, 0.7)", axis = "y") %>%
      dyShading(from = data_semaforo_subset()[, .(cases_q1)], to = data_semaforo_subset()[, .(cases_q2)], color = "rgb(255, 205, 163, 0.7)", axis = "y") %>%
      dyShading(from = data_semaforo_subset()[, .(cases_q2)], to = data_semaforo_subset()[, .(cases_q3)], color = "rgb(239, 79, 79, 0.7)", axis = "y") 
  })

  ## Semaforo Provincial: Defunciones
  output$dygraph_prov_new_deaths <- renderDygraph({
    
    shiny::req(input$prov)
    
    dygraph(data_prov_subset()[, .(fecha, defunciones)],
            main = input$prov) %>%
      dyAxis("y", label = "Número de defunciones",  valueFormatter = JS(valueFormatter_rounded) ) %>%
      dyAxis("x", label = "Fecha") %>%
      dySeries("defunciones", label = "Número de defunciones") %>%
      dyRangeSelector(dateWindow = c(data_prov_subset()[, max(fecha) - 50], data_prov_subset()[, max(fecha) + 1]),
                      fillColor = "#003169", strokeColor = "00909e") %>%
      dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
                fillGraph = FALSE, fillAlpha = 0.4,
                drawPoints = FALSE, pointSize = 3,
                pointShape = "circle",
                colors = c("#003169")) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
      dyLegend(width = 150, show = "follow", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)  %>%
      dyRoller(showRoller = FALSE, rollPeriod = 7) %>%
      dyShading(from = data_semaforo_subset()[, .(deaths_q0)], to = data_semaforo_subset()[, .(deaths_q1)], color = "rgb(116, 199, 184, 0.7)", axis = "y") %>%
      dyShading(from = data_semaforo_subset()[, .(deaths_q1)], to = data_semaforo_subset()[, .(deaths_q2)], color = "rgb(255, 205, 163, 0.7)", axis = "y") %>%
      dyShading(from = data_semaforo_subset()[, .(deaths_q2)], to = data_semaforo_subset()[, .(deaths_q3)], color = "rgb(239, 79, 79, 0.7)", axis = "y") 
  })  
  
  ## Semaforo provincial, positividad molecular

  output$dygraph_prov_positividad_molecular <- renderDygraph({
    
    shiny::req(input$prov)
    
    dygraph(data_prov_subset()[, .(fecha, posi_molecular_percent)],
            main = input$prov) %>%
      dySeries("posi_molecular_percent", label = "Tasa de positividad") %>%
      dyAxis("x", label = "Fecha") %>%
      dyAxis("y", label = "Tasa de positividad", valueFormatter = JS(valueFormatter_percent) ) %>%
      dyRangeSelector(dateWindow = c(data_prov_subset()[, max(fecha) - 50], data_prov_subset()[, max(fecha) + 1]),
                      fillColor = "#003169", strokeColor = "00909e") %>%
      dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
                fillGraph = FALSE, fillAlpha = 0.4,
                drawPoints = FALSE, pointSize = 3,
                pointShape = "circle",
                colors = c("#003169")) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
      dyLegend(width = 150, show = "follow", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)  %>%
      dyRoller(showRoller = FALSE, rollPeriod = 7) %>%
      dyShading(from = "0", to = "15", color = "rgb(116, 199, 184, 0.7)", axis = "y") %>%
      dyShading(from = "15", to = "30", color = "rgb(255, 205, 163, 0.7)", axis = "y") %>%
      dyShading(from = "30", to = "100", color = "rgb(239, 79, 79, 0.7)", axis = "y")
  })  
  
  
  
  ## Positividad antigenica
  
  output$dygraph_prov_positividad_antigenica <- renderDygraph({
    
    shiny::req(input$prov)
    
    dygraph(data_prov_r2()[, .(fecha, posi_antigenica_percent)]) %>%
      dySeries("posi_antigenica_percent", label = "Tasa de positividad") %>%
      dyAxis("x", label = "Fecha") %>%
      dyAxis("y", label = "Tasa de positividad", valueFormatter = JS(valueFormatter_percent), valueRange = c(0, 40) ) %>%
      dyRangeSelector(dateWindow = c(data_prov_r2()[, max(fecha) - 30], data_prov_r2()[, max(fecha) + 1]),
                      fillColor = "#142850", strokeColor = "#222d32") %>%
      dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
                fillGraph = FALSE, fillAlpha = 0.4,
                colors = c("#142850", "", "")) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
      dyLegend(width = 150, show = "follow", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)  %>%
      dyRoller(showRoller = FALSE, rollPeriod = 7) %>%
      dyShading(from = "0", to = "15", color = "rgb(116, 199, 184, 0.7)", axis = "y") %>%
      dyShading(from = "15", to = "30", color = "rgb(255, 205, 163, 0.7)", axis = "y") %>%
      dyShading(from = "30", to = "100", color = "rgb(239, 79, 79, 0.7)", axis = "y")
    
  })
  
  
  
  # ## 3)  Semaforos comparativos (Paquete Dygraph)
  # 
  # ## Casos
  # 
  # output$dygraph_prov_comparativo_casos <- renderDygraph({
  #   
  #   shiny::req(input$prov)
  #   
  #   dygraph(data_prov_subset()[, .(dias, primera_ola_positivo, segunda_ola_positivo)],
  #           main = input$prov) %>%
  #     # dyAxis("y", label = "Cases") %>%
  #     dySeries("primera_ola_positivo", label = "Primera Ola") %>%
  #     dySeries("segunda_ola_positivo", label = "Segunda Ola") %>%
  #     dyRangeSelector(dateWindow = c(data_prov_subset()[, max(dias) - 50], data_prov_subset()[, max(dias) + 1]),
  #                     fillColor = "#003169", strokeColor = "00909e") %>%
  #     dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
  #               fillGraph = FALSE, fillAlpha = 0.4,
  #               drawPoints = FALSE, pointSize = 3,
  #               pointShape = "circle",
  #               colors = c("#03045e", "#3a0ca3")) %>%
  #     dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
  #     dyLegend(width = 150, show = "follow", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)  %>%
  #     dyRoller(showRoller = FALSE, rollPeriod = 7) %>%
  #     dyShading(from = data_semaforo_subset()[, .(cases_q0)], to = data_semaforo_subset()[, .(cases_q1)], color = "rgb(116, 199, 184, 0.7)", axis = "y") %>%
  #     dyShading(from = data_semaforo_subset()[, .(cases_q1)], to = data_semaforo_subset()[, .(cases_q2)], color = "rgb(255, 205, 163, 0.7)", axis = "y") %>%
  #     dyShading(from = data_semaforo_subset()[, .(cases_q2)], to = data_semaforo_subset()[, .(cases_q3)], color = "rgb(239, 79, 79, 0.7)", axis = "y") 
  # })  
  # 
  # ## Defunciones
  # 
  # output$dygraph_prov_comparativo_defunciones <- renderDygraph({
  #   
  #   shiny::req(input$prov)
  #   
  #   dygraph(data_prov_subset()[, .(dias, primera_ola_defunciones, segunda_ola_defunciones)],
  #           main = input$prov) %>%
  #     # dyAxis("y", label = "Cases") %>%
  #     dySeries("primera_ola_defunciones", label = "Primera Ola") %>%
  #     dySeries("segunda_ola_defunciones", label = "Segunda Ola") %>%
  #     dyRangeSelector(dateWindow = c(data_prov_subset()[, max(dias) - 50], data_prov_subset()[, max(dias) + 1]),
  #                     fillColor = "#003169", strokeColor = "00909e") %>%
  #     dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
  #               fillGraph = FALSE, fillAlpha = 0.4,
  #               drawPoints = FALSE, pointSize = 3,
  #               pointShape = "circle",
  #               colors = c("#03045e", "#3a0ca3")) %>%
  #     dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
  #     dyLegend(width = 150, show = "follow", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)  %>%
  #     dyRoller(showRoller = FALSE, rollPeriod = 7) %>%
  #     dyShading(from = data_semaforo_subset()[, .(deaths_q0)], to = data_semaforo_subset()[, .(deaths_q1)], color = "rgb(116, 199, 184, 0.7)", axis = "y") %>%
  #     dyShading(from = data_semaforo_subset()[, .(deaths_q1)], to = data_semaforo_subset()[, .(deaths_q2)], color = "rgb(255, 205, 163, 0.7)", axis = "y") %>%
  #     dyShading(from = data_semaforo_subset()[, .(deaths_q2)], to = data_semaforo_subset()[, .(deaths_q3)], color = "rgb(239, 79, 79, 0.7)", axis = "y") 
  # })  
  # 
  # ## Tasa de positividad molecular
  # 
  # output$dygraph_prov_comparativo_posimolecular <- renderDygraph({
  #   
  #   shiny::req(input$prov)
  #   
  #   dygraph(data_prov_subset()[, .(dias, primera_ola_tasamolecular, segunda_ola_tasamolecular)],
  #           main = input$prov) %>%
  #     # dyAxis("y", label = "Cases") %>%
  #     dySeries("primera_ola_tasamolecular", label = "Primera Ola") %>%
  #     dySeries("segunda_ola_tasamolecular", label = "Segunda Ola") %>%
  #     dyRangeSelector(dateWindow = c(data_prov_subset()[, max(dias) - 50], data_prov_subset()[, max(dias) + 1]),
  #                     fillColor = "#003169", strokeColor = "00909e") %>%
  #     dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
  #               fillGraph = FALSE, fillAlpha = 0.4,
  #               drawPoints = FALSE, pointSize = 3,
  #               pointShape = "circle",
  #               colors = c("#03045e", "#3a0ca3")) %>%
  #     dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
  #     dyLegend(width = 150, show = "follow", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)  %>%
  #     dyRoller(showRoller = FALSE, rollPeriod = 7) %>%
  #     dyShading(from = "0", to = "0.15", color = "rgb(116, 199, 184, 0.7)", axis = "y") %>%
  #     dyShading(from = "0.15", to = "0.30", color = "rgb(255, 205, 163, 0.7)", axis = "y") %>%
  #     dyShading(from = "0.30", to = "0.74", color = "rgb(239, 79, 79, 0.7)", axis = "y")
  # })  

  
  ## 3)  Codigo gráfico 3 a nivel provincial (Paquete Dygraph)
  output$plot3_prov <- renderDygraph({

    shiny::req(input$prov)
    
      dygraph(data_prov_subset()[, .(fecha, total_positivo, total_recuperado, total_sintomaticos, total_defunciones,total_inicio)],) %>%
        dySeries("total_positivo", label = "Total de casos positivos") %>%
        dySeries("total_recuperado", label = "Total de casos recuperados") %>%
        dySeries("total_sintomaticos", label = "Total de casos sintomáticos") %>%
        dySeries("total_inicio", label = "Total de casos por inicio de síntomas") %>%
        dySeries("total_defunciones", label = "Total de defunciones") %>%
        dyLegend(show = "follow", showZeroValues = TRUE, labelsDiv = NULL,
                 labelsSeparateLines = FALSE, hideOnMouseOut = TRUE) %>%
        dyCSS(textConnection("
                  .dygraph-legend {
                  width: 150 !important;
                  min-width: 150px;
                  color: #000445;
                  background-color: rgb(250, 250, 250, 0.4) !important;
                  padding-left:5px;
                  border-color:#000445;
                  border-style:solid;
                  border-width:3px;
                  transition:0s 2s;
                  z-index: 80 !important;
                  box-shadow: 2px 2px 5px rgba(0, 0, 0, .3);
                  border-radius: 0px;
                  }
                  .dygraph-legend:hover{
                  transform: translate(-50%);
                  transition: 3s;
                  }
                
                  .dygraph-legend > span {
                    color: #000445;
                    padding-left:3px;
                    padding-right:3px;
                    margin-left:-3px;
                    background-color: rgb(250, 250, 250, 0.4) !important;
                    display: block;
                  }
                
                  .dygraph-legend > span:first-child {
                    margin-top:3px;
                  }

                  .dygraph-legend > span > span{
                    display: inline;
                  }
                  
                  .highlight {
                    border-left: 3px solid #000445;
                    padding-left:3px !important;
                  }
                ")
      ) %>%
      dyRangeSelector() %>%
        dyOptions(colors = myPal5)
  })
    
  ## 3)  Codigo gráfico 4 a nivel provincial (Paquete Dygraph)
  # output$plot4_prov <- renderDygraph({
  #   
  #   shiny::req(input$prov)
  #   
  #   dygraph(data_prov_subset()[, .(fecha, total_positivo, total_inicio)],) %>%
  #     dySeries("total_positivo", label = "Total de casos positivos por covid-19") %>%
  #     dySeries("total_inicio", label = "Total de casos de inicio de síntomas por covid-19") %>%
  #     dyLegend(show = "follow", showZeroValues = TRUE, labelsDiv = NULL,
  #              labelsSeparateLines = FALSE, hideOnMouseOut = TRUE) %>%
  #     dyCSS(textConnection("
  #                 .dygraph-legend {
  #                 width: auto !important;
  #                 min-width: 150px;
  #                 color: #000445;
  #                 background-color: rgb(250, 250, 250, 0.4) !important;
  #                 padding-left:5px;
  #                 border-color:#000445;
  #                 border-style:solid;
  #                 border-width:3px;
  #                 transition:0s 2s;
  #                 z-index: 80 !important;
  #                 box-shadow: 2px 2px 5px rgba(0, 0, 0, .3);
  #                 border-radius: 0px;
  #                 }
  #                 .dygraph-legend:hover{
  #                 transform: translate(-110%);
  #                 transition: 3s;
  #                 }
  #               
  #                 .dygraph-legend > span {
  #                   color: black;
  #                   padding-left:5px;
  #                   padding-right:2px;
  #                   margin-left:-5px;
  #                   background-color: rgb(250, 250, 250, 0.4) !important;
  #                   display: block;
  #                 }
  #               
  #                 .dygraph-legend > span:first-child {
  #                   margin-top:2px;
  #                 }
  # 
  #                 .dygraph-legend > span > span{
  #                   display: inline;
  #                 }
  #                 
  #                 .highlight {
  #                   border-left: 3px solid #000445;
  #                   padding-left:3px !important;
  #                 }
  #               ")
  #     ) %>%
  #     dyRangeSelector() %>%
  #     dyOptions(colors = myPal2)
  # })
  
  
  ############################ Código para graficar la data distrital ----
  
  
  ## Semaforo Distrital: Casos
  
  output$dygraph_dis_new_cases <- renderDygraph({
    
    shiny::req(input$dis)
    
    dygraph(data_dis_subset()[, .(fecha, positivo)],
            main = input$dis) %>%
      dyRangeSelector(dateWindow = c(data_dis_subset()[, max(fecha) - 50], data_dis_subset()[, max(fecha) + 1]),
                      fillColor = "#003169", strokeColor = "00909e") %>%
      dySeries("positivo", label = "Número de casos") %>%
      dyAxis("x", label = "Fecha") %>%
      dyAxis("y",label = "Número de casos", valueFormatter = JS(valueFormatter_rounded) ) %>%
      dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
                fillGraph = FALSE, fillAlpha = 0.4,
                drawPoints = FALSE, pointSize = 3,
                pointShape = "circle",
                colors = c("#003169")) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
      dyLegend(width = 150, show = "follow", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)  %>%
      # dyRoller(showRoller = FALSE, rollPeriod = 7) %>%
      dyShading(from = data_semaforo_dis_subset()[, .(cases_q0)], to = data_semaforo_dis_subset()[, .(cases_q1)], color = "#74c7b8", axis = "y") %>%
      dyShading(from = data_semaforo_dis_subset()[, .(cases_q1)], to = data_semaforo_dis_subset()[, .(cases_q2)], color = "#ffcda3", axis = "y") %>%
      dyShading(from = data_semaforo_dis_subset()[, .(cases_q2)], to = data_semaforo_dis_subset()[, .(cases_q3)], color = "#ef4f4f", axis = "y")
  })
  
  ## Semaforo distrital: Defunciones
  
  output$dygraph_dis_new_deaths <- renderDygraph({
    
    shiny::req(input$dis)
    
    dygraph(data_dis_subset()[, .(fecha, defunciones)],
            main = input$dis) %>%
      dyAxis("x", label = "Fecha") %>%
      dyAxis("y", label = "Número de defunciones" , valueFormatter = JS(valueFormatter_rounded) ) %>%
      dySeries("defunciones", label = "Número de defunciones") %>%
      dyRangeSelector(dateWindow = c(data_dis_subset()[, max(fecha) - 50], data_dis_subset()[, max(fecha) + 1]),
                      fillColor = "#003169", strokeColor = "00909e") %>%
      dyAxis("y", valueFormatter = JS(valueFormatter_rounded) ) %>%
      dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
                fillGraph = FALSE, fillAlpha = 0.4,
                drawPoints = FALSE, pointSize = 3,
                pointShape = "circle",
                colors = c("#003169")) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
      dyLegend(width = 150, show = "follow", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)  %>%
      # dyRoller(showRoller = FALSE, rollPeriod = 7) %>%
      dyShading(from = data_semaforo_dis_subset()[, .(deaths_q0)], to = data_semaforo_dis_subset()[, .(deaths_q1)], color = "#74c7b8", axis = "y") %>%
      dyShading(from = data_semaforo_dis_subset()[, .(deaths_q1)], to = data_semaforo_dis_subset()[, .(deaths_q2)], color = "#ffcda3", axis = "y") %>%
      dyShading(from = data_semaforo_dis_subset()[, .(deaths_q2)], to = data_semaforo_dis_subset()[, .(deaths_q3)], color = "#ef4f4f", axis = "y")
  })  
  
  ## Semaforo distrital: Positividad molecular
  
  output$dygraph_dis_positividad_molecular <- renderDygraph({
    
    shiny::req(input$dis)
    
    dygraph(data_dis_subset()[, .(fecha, posi_molecular_percent)],
            main = input$dis) %>%
      dyAxis("x", label = "Fecha") %>%
      dyAxis("y", label = "Tasa de positividad",valueFormatter = JS(valueFormatter_percent) ) %>%
      dySeries("posi_molecular_percent", label = "Tasa de positividad molecular") %>%
      dyRangeSelector(dateWindow = c(data_dis_subset()[, max(fecha) - 50], data_dis_subset()[, max(fecha) + 1]),
                      fillColor = "#003169", strokeColor = "00909e") %>%
      dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
                fillGraph = FALSE, fillAlpha = 0.4,
                drawPoints = FALSE, pointSize = 3,
                pointShape = "circle",
                colors = c("#003169")) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
      dyLegend(width = 150, show = "follow", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)  %>%
      # dyRoller(showRoller = FALSE, rollPeriod = 7) %>%
      dyShading(from = "0", to = "15", color = "rgb(116, 199, 184, 0.7)", axis = "y") %>%
      dyShading(from = "15", to = "30", color = "rgb(255, 205, 163, 0.7)", axis = "y") %>%
      dyShading(from = "30", to = "100", color = "rgb(239, 79, 79, 0.7)", axis = "y")
  })  
  
  
  ## Positividad antigenica
  
  output$dygraph_dis_positividad_antigenica <- renderDygraph({
    
    shiny::req(input$dis)
    
    dygraph(data_dis_r2()[, .(fecha, posi_antigenica_percent)]) %>%
      dySeries("posi_antigenica_percent", label = "Tasa de positividad") %>%
      dyAxis("x", label = "Fecha") %>%
      dyAxis("y", label = "Tasa de positividad", valueFormatter = JS(valueFormatter_percent), valueRange = c(0, 40) ) %>%
      dyRangeSelector(dateWindow = c(data_dis_r2()[, max(fecha) - 30], data_dis_r2()[, max(fecha) + 1]),
                      fillColor = "#142850", strokeColor = "#222d32") %>%
      dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
                fillGraph = FALSE, fillAlpha = 0.4,
                colors = c("#142850", "", "")) %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
      dyLegend(width = 150, show = "follow", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)  %>%
      dyRoller(showRoller = FALSE, rollPeriod = 7) %>%
      dyShading(from = "0", to = "15", color = "rgb(116, 199, 184, 0.7)", axis = "y") %>%
      dyShading(from = "15", to = "30", color = "rgb(255, 205, 163, 0.7)", axis = "y") %>%
      dyShading(from = "30", to = "100", color = "rgb(239, 79, 79, 0.7)", axis = "y")
    
  })
  
  
  # ## 3)  Semaforos comparativos (Paquete Dygraph)
  # 
  # ## Casos
  # 
  # output$dygraph_dis_comparativo_casos <- renderDygraph({
  #   
  #   shiny::req(input$dis)
  #   
  #   dygraph(data_dis_subset()[, .(dias, primera_ola_positivo, segunda_ola_positivo)],
  #           main = input$dis) %>%
  #     # dyAxis("y", label = "Cases") %>%
  #     dySeries("primera_ola_positivo", label = "Primera Ola") %>%
  #     dySeries("segunda_ola_positivo", label = "Segunda Ola") %>%
  #     dyRangeSelector(dateWindow = c(data_dis_subset()[, max(dias) - 50], data_dis_subset()[, max(dias) + 1]),
  #                     fillColor = "#003169", strokeColor = "00909e") %>%
  #     dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
  #               fillGraph = FALSE, fillAlpha = 0.4,
  #               drawPoints = FALSE, pointSize = 3,
  #               pointShape = "circle",
  #               colors = c("#03045e", "#3a0ca3")) %>%
  #     dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
  #     dyLegend(width = 150, show = "follow", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)  %>%
  #     dyRoller(showRoller = FALSE, rollPeriod = 7) %>%
  #     dyShading(from = data_semaforo_dis_subset()[, .(cases_q0)], to = data_semaforo_dis_subset()[, .(cases_q1)], color = "#74c7b8", axis = "y") %>%
  #     dyShading(from = data_semaforo_dis_subset()[, .(cases_q1)], to = data_semaforo_dis_subset()[, .(cases_q2)], color = "#ffcda3", axis = "y") %>%
  #     dyShading(from = data_semaforo_dis_subset()[, .(cases_q2)], to = data_semaforo_dis_subset()[, .(cases_q3)], color = "#ef4f4f", axis = "y")
  # })  
  # 
  # ## Defunciones
  # 
  # output$dygraph_dis_comparativo_defunciones <- renderDygraph({
  #   
  #   shiny::req(input$dis)
  #   
  #   dygraph(data_dis_subset()[, .(dias, primera_ola_defunciones, segunda_ola_defunciones)],
  #           main = input$dis) %>%
  #     # dyAxis("y", label = "Cases") %>%
  #     dySeries("primera_ola_defunciones", label = "Primera Ola") %>%
  #     dySeries("segunda_ola_defunciones", label = "Segunda Ola") %>%
  #     dyRangeSelector(dateWindow = c(data_dis_subset()[, max(dias) - 50], data_dis_subset()[, max(dias) + 1]),
  #                     fillColor = "#003169", strokeColor = "00909e") %>%
  #     dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
  #               fillGraph = FALSE, fillAlpha = 0.4,
  #               drawPoints = FALSE, pointSize = 3,
  #               pointShape = "circle",
  #               colors = c("#03045e", "#3a0ca3")) %>%
  #     dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
  #     dyLegend(width = 150, show = "follow", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)  %>%
  #     dyRoller(showRoller = FALSE, rollPeriod = 7) %>%
  #     dyShading(from = data_semaforo_dis_subset()[, .(deaths_q0)], to = data_semaforo_dis_subset()[, .(deaths_q1)], color = "#74c7b8", axis = "y") %>%
  #     dyShading(from = data_semaforo_dis_subset()[, .(deaths_q1)], to = data_semaforo_dis_subset()[, .(deaths_q2)], color = "#ffcda3", axis = "y") %>%
  #     dyShading(from = data_semaforo_dis_subset()[, .(deaths_q2)], to = data_semaforo_dis_subset()[, .(deaths_q3)], color = "#ef4f4f", axis = "y")
  # })  
  # 
  #  ## Tasa de positividad molecular
  # 
  # output$dygraph_dis_comparativo_posimolecular <- renderDygraph({
  #   
  #   shiny::req(input$dis)
  #   
  #   dygraph(data_dis_subset()[, .(dias, primera_ola_tasamolecular, segunda_ola_tasamolecular)],
  #           main = input$dis) %>%
  #     # dyAxis("y", label = "Cases") %>%
  #     dySeries("primera_ola_tasamolecular", label = "Primera Ola") %>%
  #     dySeries("segunda_ola_tasamolecular", label = "Segunda Ola") %>%
  #     dyRangeSelector(dateWindow = c(data_dis_subset()[, max(dias) - 50], data_dis_subset()[, max(dias) + 1]),
  #                     fillColor = "#003169", strokeColor = "00909e") %>%
  #     dyOptions(useDataTimezone = TRUE, strokeWidth = 2,
  #               fillGraph = FALSE, fillAlpha = 0.4,
  #               drawPoints = FALSE, pointSize = 3,
  #               pointShape = "circle",
  #               colors = c("#03045e", "#3a0ca3")) %>%
  #     dyHighlight(highlightSeriesOpts = list(strokeWidth = 2.5, pointSize = 4)) %>%
  #     dyLegend(width = 150, show = "follow", hideOnMouseOut = TRUE, labelsSeparateLines = TRUE)  %>%
  #     dyRoller(showRoller = FALSE, rollPeriod = 7) %>%
  #     dyShading(from = "0", to = "0.15", color = "rgb(116, 199, 184, 0.7)", axis = "y") %>%
  #     dyShading(from = "0.15", to = "0.30", color = "rgb(255, 205, 163, 0.7)", axis = "y") %>%
  #     dyShading(from = "0.30", to = "0.74", color = "rgb(239, 79, 79, 0.7)", axis = "y")
  # })  
  
  
  ## 3)  Codigo gráfico 3 a nivel DISTRITAL (Paquete Dygraph)
  
  output$plot3_dis <- renderDygraph({
    
    shiny::req(input$dis)
    
    dygraph(data_dis_subset()[, .(fecha, total_positivo, total_recuperado, total_sintomaticos, total_defunciones, total_inicio)],) %>%
      dySeries("total_positivo", label = "Total de casos positivos") %>%
      dySeries("total_recuperado", label = "Total de casos recuperados") %>%
      dySeries("total_sintomaticos", label = "Total de casos sintomáticos") %>% 
      dySeries("total_inicio", label = "Total de casos por inicio de síntomas") %>%
      dySeries("total_defunciones", label = "Total de defunciones") %>%
      dyLegend(show = "follow", showZeroValues = TRUE, labelsDiv = NULL,
               labelsSeparateLines = FALSE, hideOnMouseOut = TRUE) %>%
      dyCSS(textConnection("
                  .dygraph-legend {
                  width: 150 !important;
                  min-width: 150px;
                  color: #000445;
                  background-color: rgb(250, 250, 250, 0.4) !important;
                  padding-left:5px;
                  border-color:#000445;
                  border-style:solid;
                  border-width:3px;
                  transition:0s 2s;
                  z-index: 80 !important;
                  box-shadow: 2px 2px 5px rgba(0, 0, 0, .3);
                  border-radius: 0px;
                  }
                  .dygraph-legend:hover{
                  transform: translate(-50%);
                  transition: 3s;
                  }
                
                  .dygraph-legend > span {
                    color: #000445;
                    padding-left:3px;
                    padding-right:3px;
                    margin-left:-3px;
                    background-color: rgb(250, 250, 250, 0.4) !important;
                    display: block;
                  }
                
                  .dygraph-legend > span:first-child {
                    margin-top:3px;
                  }

                  .dygraph-legend > span > span{
                    display: inline;
                  }
                  
                  .highlight {
                    border-left: 3px solid #000445;
                    padding-left:3px !important;
                  }
                ")
      ) %>%
      dyRangeSelector() %>%
      dyOptions(colors = myPal5)
  })
  
  
  ## 3)  Codigo gráfico 4 a nivel DISTRITAL (Paquete Dygraph)
  
  # output$plot4_dis <- renderDygraph({
  #   
  #   shiny::req(input$dis)
  #   
  #   dygraph(data_dis_subset()[, .(fecha, total_positivo, total_inicio)],) %>%
  #     dySeries("total_positivo", label = "Total de casos positivos por covid-19") %>%
  #     dySeries("total_inicio", label = "Total de casos de inicio de síntomas por covid-19") %>%
  #     dyLegend(show = "follow", showZeroValues = TRUE, labelsDiv = NULL,
  #              labelsSeparateLines = FALSE, hideOnMouseOut = TRUE) %>%
  #     dyCSS(textConnection("
  #                 .dygraph-legend {
  #                 width: 150 !important;
  #                 min-width: 150px;
  #                 color: #000445;
  #                 background-color: rgb(250, 250, 250, 0.4) !important;
  #                 padding-left:5px;
  #                 border-color:#000445;
  #                 border-style:solid;
  #                 border-width:3px;
  #                 transition:0s 2s;
  #                 z-index: 80 !important;
  #                 box-shadow: 2px 2px 5px rgba(0, 0, 0, .3);
  #                 border-radius: 0px;
  #                 }
  #                 .dygraph-legend:hover{
  #                 transform: translate(-50%);
  #                 transition: 3s;
  #                 }
  #               
  #                 .dygraph-legend > span {
  #                   color: #000445;
  #                   padding-left:3px;
  #                   padding-right:3px;
  #                   margin-left:-3px;
  #                   background-color: rgb(250, 250, 250, 0.4) !important;
  #                   display: block;
  #                 }
  #               
  #                 .dygraph-legend > span:first-child {
  #                   margin-top:3px;
  #                 }
  # 
  #                 .dygraph-legend > span > span{
  #                   display: inline;
  #                 }
  #                 
  #                 .highlight {
  #                   border-left: 3px solid #000445;
  #                   padding-left:3px !important;
  #                 }
  #               ")
  #     ) %>%
  #     dyRangeSelector() %>%
  #     dyOptions(colors = myPal2)
  # })
  
  
  # Reporte en R markdown
  
  # output$reporte <- downloadHandler(
  #   filename = "reporte.pdf",
  #   content = function(file) {
  #     tempReport <- file.path(tempdir(), "reporte.Rmd")
  #     file.copy("reporte.Rmd", tempReport, overwrite = TRUE)
  #     
  #     params <- list()
  #     
  #     rmarkdown::render(tempReport, output_file = file,
  #                       params = params,
  #                       envir = new.env(parent = globalenv())
  #     )
  #   }
  # )
  
  
})