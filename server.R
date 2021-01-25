shinyServer(function(input, output, session){
  ######################################## Data de github

  # # Leer data para el bubble plot
  # data_raw <- fread("https://raw.githubusercontent.com/branmora/diresacusco/main/Cusco_data.csv")

  # #  Leer data para Gráfico 2
  # data2_1 <- fread("https://raw.githubusercontent.com/branmora/diresacusco/main/Cusco_data.csv")
  # data2_2 <- fread("https://raw.githubusercontent.com/branmora/diresacusco/main/provincial-incidencia-densidad-8-12.csv")
  
  #################################################### Set up loading screen ----
  
  Sys.sleep(3) # plots
  waiter_hide()
  
  #################################################### Leer data ----
  
  # Leer data
  source("01_scripts/read_data.R")
  
  #################################################### Hacer data reactiva y subset por provincia y distrito ----
  
  ### Make data reactive ----
  
  # Map data
  
    map_district <- read_data_map_district()
    data_dis <- read_data_dis()
  # Regional (semaforo V2)
  
  data_dpto_r <- reactive({
    data_dpto <- read_data_dpto()
  })
  
  # Provincial (semaforo V2)
  
  data_prov_r <- reactive({
    data_prov <- read_data_prov()
  })
  
  # Distrital (semaforo v2)
  
  data_dis_r <- reactive({
    data_dis <- read_data_dis()
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
  
  myPal5 <- c(
    rgb(255, 134, 0, maxColorValue = 255),
    rgb(251, 54, 64, maxColorValue = 255),
    rgb(6, 214, 160, maxColorValue = 255),
    rgb(115, 113, 252, maxColorValue = 255),
    rgb(3, 4, 94, maxColorValue = 255))
  print(myPal5)
  
  ######################################## Código de gráficos ----
  
  ### 1) Código para graficar el semáforo COVID ----
  
  ## Casos
  output$dygraph_region_casos <- renderDygraph({
    
    dygraph(data_dpto_r()[, .(fecha, positivo)]) %>%
      dySeries("positivo", label = "Promedio de 7 dias") %>%
      # dyAxis("y", label = "Cases") %>%
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
  
  
  # dyLegend(width = 380, showZeroValues = TRUE, labelsDiv = "legend_plot3",
  #          labelsSeparateLines = FALSE, hideOnMouseOut = TRUE) %>%
  
  ## Defunciones
  output$dygraph_region_defunciones <- renderDygraph({
    
    dygraph(data_dpto_r()[, .(fecha, defunciones)]) %>%
      dySeries("defunciones", label = "Promedio de 7 dias") %>%
      # dyAxis("y", label = "Deaths") %>%
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
    
    dygraph(data_beds()[, .(DateRep, UCI, NOUCI, NIVELII)]) %>%
      dySeries("UCI", label = "Ocupacion UCI") %>%
      dySeries("NOUCI", label = "Ocupacion No UCI") %>%
      dySeries("NIVELII", label = "Ocupacion Nivel II") %>%
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
      dyShading(from = "0", to = "0.25", color = "rgb(116, 199, 184, 0.7)", axis = "y") %>%
      dyShading(from = "0.25", to = "0.65", color = "rgb(255, 205, 163, 0.7)", axis = "y") %>%
      dyShading(from = "0.65", to = "1.5", color = "rgb(239, 79, 79, 0.7)", axis = "y")
    
  })
  
  # 2) Código para graficar el mapa del cusco ----
  
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
        
  # 3) Código para graficar el bubble plot ----
  
  # data_raw$Fecha <- as.Date(data_raw$Fecha, "%d/%m/%Y")
  # 
  # datacusco_str <- distinct(data_raw, Distrito, .keep_all = TRUE) %>% 
  #   mutate(x = Densidad, y = Incidencia, z = Poblacion) 
  # 
  # datacusco_seq <- data_raw %>% 
  #   arrange(Distrito, Fecha) %>% 
  #   group_by(Distrito) %>% 
  #   do(sequence = list_parse(select(., x = Densidad, y = Incidencia, z = Poblacion)))
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
  #   hc_motion(enabled = TRUE, series = 0, labels = unique(data_raw$Fecha),
  #             loop = TRUE, autoPlay = TRUE, 
  #             updateInterval = 1000, magnet = list(step =  20)) %>% 
  #   hc_plotOptions(series = list(showInLegend = FALSE)) %>% 
  #   hc_xAxis(type = "logarithmic", min = 12, max = 15000) %>% 
  #   hc_yAxis(min = 0, max = 129.6) %>% 
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
    
    dygraph(data_prov_subset()[, .(fecha, positivo)],
            main = input$prov) %>%
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
      # dyAxis("y", label = "Cases") %>%
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
  
  
  ## 3)  Codigo gráfico 3 a nivel provincial (Paquete Dygraph)
  output$plot3_prov <- renderDygraph({

    shiny::req(input$prov)
    
      dygraph(data_prov_subset()[, .(fecha, total_positivo, total_recuperado, total_sintomaticos, total_defunciones)],) %>%
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
        dyOptions(colors = myPal3)
  })
    
  ## 3)  Codigo gráfico 4 a nivel provincial (Paquete Dygraph)
  output$plot4_prov <- renderDygraph({
    
    shiny::req(input$prov)
    
    dygraph(data_prov_subset()[, .(fecha, total_positivo, total_inicio)],) %>%
      dySeries("total_positivo", label = "Total de casos positivos por covid-19") %>%
      dySeries("total_inicio", label = "Total de casos de inicio de síntomas por covid-19") %>%
      dyLegend(show = "follow", showZeroValues = TRUE, labelsDiv = NULL,
               labelsSeparateLines = FALSE, hideOnMouseOut = TRUE) %>%
      dyCSS(textConnection("
                  .dygraph-legend {
                  width: auto !important;
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
      dyOptions(colors = myPal1)
  })
  
  
  ############################ Código para graficar la data distrital ----
  
  
  ## Semaforo Distrital: Casos
  
  output$dygraph_dis_new_cases <- renderDygraph({
    
    shiny::req(input$dis)
    
    dygraph(data_dis_subset()[, .(fecha, positivo)],
            main = input$prov) %>%
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
      dyShading(from = data_semaforo_dis_subset()[, .(cases_q0)], to = data_semaforo_dis_subset()[, .(cases_q1)], color = "#74c7b8", axis = "y") %>%
      dyShading(from = data_semaforo_dis_subset()[, .(cases_q1)], to = data_semaforo_dis_subset()[, .(cases_q2)], color = "#ffcda3", axis = "y") %>%
      dyShading(from = data_semaforo_dis_subset()[, .(cases_q2)], to = data_semaforo_dis_subset()[, .(cases_q3)], color = "#ef4f4f", axis = "y")
  })
  
  ## Semaforo distrital: Defunciones
  
  output$dygraph_dis_new_deaths <- renderDygraph({
    
    shiny::req(input$dis)
    
    dygraph(data_dis_subset()[, .(fecha, defunciones)],
            main = input$prov) %>%
      # dyAxis("y", label = "Cases") %>%
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
      dyShading(from = data_semaforo_dis_subset()[, .(deaths_q0)], to = data_semaforo_dis_subset()[, .(deaths_q1)], color = "#74c7b8", axis = "y") %>%
      dyShading(from = data_semaforo_dis_subset()[, .(deaths_q1)], to = data_semaforo_dis_subset()[, .(deaths_q2)], color = "#ffcda3", axis = "y") %>%
      dyShading(from = data_semaforo_dis_subset()[, .(deaths_q2)], to = data_semaforo_dis_subset()[, .(deaths_q3)], color = "#ef4f4f", axis = "y")
  })  
  
  
  ## 3)  Codigo gráfico 3 a nivel DISTRITAL (Paquete Dygraph)
  
  output$plot3_dis <- renderDygraph({
    
    shiny::req(input$dis)
    
    dygraph(data_dis_subset()[, .(fecha, total_positivo, total_recuperado, total_sintomaticos, total_defunciones)],) %>%
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
      dyOptions(colors = myPal3)
  })
  
  
  ## 3)  Codigo gráfico 4 a nivel DISTRITAL (Paquete Dygraph)
  
  output$plot4_dis <- renderDygraph({
    
    shiny::req(input$dis)
    
    dygraph(data_dis_subset()[, .(fecha, total_positivo, total_inicio)],) %>%
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
      dyOptions(colors = myPal1)
  })
  
  
  
})