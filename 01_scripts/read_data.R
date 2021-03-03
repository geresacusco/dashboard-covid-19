# Leer data de Github -----

# Data departamental
read_data_dpto <- function() {
  data_dpto <- fread("https://raw.githubusercontent.com/geresacusco/dashboard-covid-19/main/data/data_regional.csv", keepLeadingZeros = TRUE)
  data_dpto$fecha <- as.Date(data_dpto$fecha)
  data_dpto <- subset(data_dpto, fecha > as.Date("2020-03-12") & fecha < Sys.Date() - 1)
  data_dpto <- mutate(data_dpto, xposi=log10(total_positivo), xini = log10(total_inicio))
  data_dpto <- mutate(data_dpto, posi_molecular_percent = posi_molecular*100)  
  data_dpto <- mutate(data_dpto, posi_antigenica_percent = posi_antigenica*100)  
  
  return(data_dpto)
}

# Data provincial
read_data_prov <- function() {
  data_prov <- fread("https://raw.githubusercontent.com/geresacusco/dashboard-covid-19/main/data/data_provincial.csv", keepLeadingZeros = TRUE)
  data_prov$fecha <- as.Date(data_prov$fecha)
  data_prov <- subset(data_prov, fecha > as.Date("2020-03-12") & fecha < Sys.Date() - 1)
  data_prov <- mutate(data_prov, posi_molecular_percent = posi_molecular*100)
  data_prov <- mutate(data_prov, posi_antigenica_percent = posi_antigenica*100)  
  return(data_prov)
}

# Data distrital
read_data_dis <- function() {
  data_dis <- fread("https://raw.githubusercontent.com/geresacusco/dashboard-covid-19/main/data/data_distrital.csv", keepLeadingZeros = TRUE)
  data_dis$fecha <- as.Date(data_dis$fecha)
  data_dis <- subset(data_dis, fecha > as.Date("2020-03-12") & fecha < Sys.Date() -1)
  data_dis <- mutate(data_dis, IDDIST = ubigeo)
  data_dis <- mutate(data_dis, posi_molecular_percent = posi_molecular*100)  
  data_dis <- mutate(data_dis, posi_antigenica_percent = posi_antigenica*100)  
  
  return(data_dis)

}

# Data de mapa distrital

read_data_map_district <- function() {
  cusco_map_district <- jsonlite::fromJSON("https://raw.githubusercontent.com/geresacusco/dashboard-covid-19/main/data/mapas/districts.geojson", simplifyVector = FALSE)
}


# Data camas
read_data_beds <- function() {
  
  data_beds_melt <- fread("https://raw.githubusercontent.com/geresacusco/dashboard-covid-19/main/data/camas/camas.csv", sep2 = ";")
  data_beds_melt <- mutate(data_beds_melt, UCI_percent = UCI*100)  
  data_beds_melt <- mutate(data_beds_melt, NOUCI_percent = NOUCI*100)  
  data_beds_melt <- mutate(data_beds_melt, NIVELII_percent = NIVELII*100)  
  data_beds_melt[, DateRep := lubridate::mdy(DateRep)]
  
  return(data_beds_melt)
  
}

# Data valores semáforo provincial,
read_semaforo <- function() {
  data_semaforo <- fread("https://raw.githubusercontent.com/geresacusco/dashboard-covid-19/main/data/semaforo/traffic_light.csv")
}


# Data valores semáforo distrital,
read_semaforo_dis <- function() {
  data_semaforo_dis <- fread("https://raw.githubusercontent.com/geresacusco/dashboard-covid-19/main/data/semaforo/traffic_light_distrital.csv")
}

