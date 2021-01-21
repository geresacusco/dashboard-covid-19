shinyUI(
  
  fluidPage(
    use_waiter(),
    use_steward(),
    waiter_show_on_load(html = spin_wandering_cubes(), color = "#333e48"),
    includeCSS("styles.css"),
    tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);"),
    ##-- Favicon -- --
    tags$head(
      tags$link(rel = "shortcut icon", href = "img/logo.ico"),
      #-- biblio js ----
      tags$link(rel="stylesheet", type = "text/css",
                href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css"),
      tags$link(rel="stylesheet", type = "text/css",
                href = "https://fonts.googleapis.com/css?family=Open+Sans|Source+Sans+Pro")
    ),
    ##-- Logo ----
    list(tags$head(HTML('<link rel="icon", href="img/logo.png",
                        type="image/png" />'))),
    div(style="padding: 1px 0px; width: '100%'",
        titlePanel(
          title="", windowTitle = "GERESA Dashboard"
        )
    ),
    ##-- Header ----
    navbarPage(title = div(img(src="img/logo.png",
                               height = "110px"), style = "padding-left:50px;"),
               id = "navbar",
               selected = "regional",
               theme = "styles.css", 
               fluid = T,
               ##-- Tablas ----
               regional,
               provincial,
               local
    ),
    ##-- Footer ----
    div(class = "footer",
        includeHTML("html/footer.html")
    )
  )
)