# Leer data de Github -----

# Data departamental
read_data_dpto <- function() {
  data_dpto <- fread("https://raw.githubusercontent.com/branmora/diresacusco/main/R_new_design/data/data_regional.csv", keepLeadingZeros = TRUE)
  data_dpto$fecha <- as.Date(data_dpto$fecha)
  data_dpto <- subset(data_dpto, fecha > as.Date("2020-03-12") & fecha < Sys.Date() - 1)
  data_dpto <- mutate(data_dpto, xposi=log10(total_positivo), xini = log10(total_inicio))
  return(data_dpto)
}

# Data provincial
read_data_prov <- function() {
  data_prov <- fread("https://raw.githubusercontent.com/branmora/diresacusco/main/R_new_design/data/data_provincial.csv", keepLeadingZeros = TRUE)
  data_prov$fecha <- as.Date(data_prov$fecha)
  data_prov$fecha <- as.Date(data_prov$fecha)
  data_prov <- subset(data_prov, fecha > as.Date("2020-03-12") & fecha < Sys.Date() - 1)
  return(data_prov)
}

# Data distrital
read_data_dis <- function() {
  data_dis <- fread("https://raw.githubusercontent.com/branmora/diresacusco/main/R_new_design/data/data_distrital.csv", keepLeadingZeros = TRUE)
  data_dis$fecha <- as.Date(data_dis$fecha)
  data_dis$fecha <- as.Date(data_dis$fecha)
  data_dis <- subset(data_dis, fecha > as.Date("2020-03-12") & fecha < Sys.Date() -1)
  return(data_dis)
}

# Data camas
read_data_beds <- function() {
  
  data_beds_melt <- fread("https://raw.githubusercontent.com/branmora/diresacusco/main/R_new_design/data/camas/camas.csv", sep2 = ";")
  data_beds_melt[, DateRep := lubridate::mdy(DateRep)]
  
  return(data_beds_melt)
  
}

# Data valores semáforo
read_semaforo <- function() {
  
  # data_confirmed <- fread("02_data/time_series_19-covid-Confirmed.csv")
  data_semaforo <- fread("https://raw.githubusercontent.com/branmora/covid-19-cusco/master/data/traffic_light.csv")
}



read_data_cusco <- function() {
  
  data_cusco_melt <- fread("https://raw.githubusercontent.com/branmora/covid-19-cusco/master/data/data_cusco.csv", sep2 = ";")
  
  data_cusco_melt[, fecha_resultado := lubridate::mdy(fecha_resultado)]
  
  return(data_cusco_melt)
  
}

read_data_corona <- function() {
  
  data_confirmed_melt <- fread("https://raw.githubusercontent.com/branmora/covid-19-cusco/master/data/cases.csv", sep2 = ";")
  
  data_confirmed_melt[, DateRep := lubridate::mdy(DateRep)]
  
  data_confirmed_melt_agg <- copy(data_confirmed_melt[,
                                                      .(Cases_cumsum = sum(Cases_cumsum, na.rm = TRUE)),
                                                      by = .(Country, DateRep)])
  
  # New Cases per day 
  setorder(data_confirmed_melt_agg, Country, DateRep)
  data_confirmed_melt_agg[, Cases := c(.SD[1, Cases_cumsum],
                                       diff(Cases_cumsum,
                                            lag = 1,
                                            differences = 1)
  ),
  by = .(Country)]
  
  data_confirmed_melt_agg[Cases < 0, Cases := 0]
  
  
  return(data_confirmed_melt_agg)
  
}