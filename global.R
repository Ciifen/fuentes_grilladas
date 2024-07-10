library(shiny)
library(DT)
library(shinyjs)
library(utils)

library(stringi)

library(shinyAce)



lugar_map <- list(
  "Bolivia" = 'bol',
  "Chile"= 'chi',
  'Ecuador'='ecu',
  'Venezuela'='ven',
  'Per\u00FA'='per',
  'Colombia'='col'
)

parameter_map <- list(
  "CHIRPS" = list(
    "Precipitaci\u00F3n" = "pcp"
  ),
  "NASA-POWER" = list(
    "Temperatura m\u00E1xima" = "TMAX",
    "Temperatura m\u00EDnima" = "TMIN",
    "humedad relativa" = "HRELATIVA",
    "Precipitaci\u00F3n" = "PCP",
    "Humedad del suelo (\u003C5cm)" = "HSOILTOP",
    "Humedad del suelo (\u003C100cm)" = "HSOILROOT"
  ),
  "CPC" = list(
    "Precipitaci\u00F3n" = "PCP",
    "Temperatura m\u00E1xima" = "TMAX",
    "Temperatura m\u00EDnima" = "TMIN"
  ),
  "ERA5" = list(
    "Precipitaci\u00F3n" ="tp", 
    "Temperatura m\u00E1xima" = "mx2t", 
    "Temperatura m\u00EDnima"= "mn2t", 
    "Vientos u" = "u100", 
    "Vientos v" = "v100", 
    "Humedad del suelo (0 - 7cm)" = "swvl1", 
    "Humedad del suelo  (7 - 28cm)" = "swvl2", 
    "Humedad del suelo  (28 - 100cm)" = "swvl3"
  ),
  "GPCC-ALL PRODUCTS" = list(
    "Precipitaci\u00F3n" = "pcp"
  ),
  "NCAR/UCAR Reanalysis" = list(
    "Vientos u"="UWND", 
    "Vientos v"="VWND", 
    "Presi\u00F3n atmosf\u00E9rica a nivel del mar"="SLP", 
    "Altura Geopotencial 200"="HGT200", "Altura Geopotencial 500"="HGT500", 
    "Altura Geopotencial 850"="HGT850"
  ),
  "PERSIANN-CCS" = list("Precipitaci\u00F3n" = "pcp"),
  "GPM (GPM_3IMERGDE)" = list("Precipitaci\u00F3n" = "pcp")
)


main_categories <- data.frame(
  category =  c(
    rep("ERA5", 8),
    "CHIRPS", 
    "GPCC-ALL PRODUCTS", 
    rep("NASA-POWER", 6), 
    rep("CPC", 3), 
    rep("NCAR/UCAR Reanalysis", 6),
    "PERSIANN-CCS",
    "GPM (GPM_3IMERGDE)"
  ),
  subcategory = c(
    "Precipitaci\u00F3n", "Temperatura m\u00E1xima", "Temperatura m\u00EDnima", "Vientos u", "Vientos v", "Humedad del suelo (0 - 7cm)", "Humedad del suelo  (7 - 28cm)", "Humedad del suelo  (28 - 100cm)",
    "Precipitaci\u00F3n",
    "Precipitaci\u00F3n", 
    "Temperatura m\u00E1xima", "Temperatura m\u00EDnima", "humedad relativa", "Precipitaci\u00F3n","Humedad del suelo (\u003C5cm)", "Humedad del suelo (\u003C100cm)",
    "Precipitaci\u00F3n", "Temperatura m\u00E1xima", "Temperatura m\u00EDnima",
    "Vientos u", "Vientos v", "Presi\u00F3n atmosf\u00E9rica a nivel del mar", "Altura Geopotencial 200", "Altura Geopotencial 500", "Altura Geopotencial 850",
    "Precipitaci\u00F3n", 
    "Precipitaci\u00F3n"
  )
)


fecha_today <- Sys.Date()
main_limites_fechas <- list(
  "CHIRPS" = list(
    "Precipitaci\u00F3n" = list("1981-01-01",format(fecha_today-10, format("%Y-%m-%d")))
  ),
  "NASA-POWER" = list(
    "Temperatura m\u00E1xima" = list('1981-01-01',format(fecha_today-47)),
    "Temperatura m\u00EDnima" = list('1981-01-01',format(fecha_today-47)),
    "humedad relativa" = list('1981-01-01',format(fecha_today-47)),
    "Precipitaci\u00F3n" = list('1981-01-01',format(fecha_today-47)),
    "Humedad del suelo (<5cm)" = list('1981-01-01',format(fecha_today-47)),
    "Humedad del suelo (<5cm<100cm)" = list('1981-01-01',format(fecha_today-47))
  ),
  "CPC" = list(
    "Precipitaci\u00F3n" = list('1979-01-01',format(fecha_today-2)),
    "Temperatura m\u00E1xima" = list('1979-01-01',format(fecha_today-2)),
    "Temperatura m\u00EDnima" = list('1979-01-01',format(fecha_today-2))
  ),
  "ERA5" = list(
    "Precipitaci\u00F3n" = list('1940-01-01',format(fecha_today-9)),
    "Temperatura m\u00E1xima" = list('1940-01-01',format(fecha_today-9)), 
    "Temperatura m\u00EDnima"=  list('1940-01-01',format(fecha_today-9)), 
    "Vientos u" =  list('1940-01-01',format(fecha_today-9)), 
    "Vientos v" =  list('1940-01-01',format(fecha_today-9)),  
    "Humedad del suelo (0 - 7cm)" =  list('1940-01-01',format(fecha_today-9)),  
    "Humedad del suelo  (7 - 28cm)" =  list('1940-01-01',format(fecha_today-9)),  
    "Humedad del suelo  (28 - 100cm)" =  list('1940-01-01',format(fecha_today-9))
  ),
  "GPCC-ALL PRODUCTS" = list("Precipitaci\u00F3n" =  list('2009-01-01',format(fecha_today-32))
  ),
  "NCAR/UCAR Reanalysis" = list(
    "Vientos u"= list('1948-01-01',format(fecha_today-2)), 
    "Vientos v"=list('1948-01-01',format(fecha_today-2)), 
    "Presi\u00F3n atmosf\u00E9rica a nivel del mar"=list('1948-01-01',format(fecha_today-2)), 
    "Altura Geopotencial 200"=list('1948-01-01',format(fecha_today-2)),  
    "Altura Geopotencial 500"=list('1948-01-01',format(fecha_today-2)), 
    "Altura Geopotencial 850"=list('1948-01-01',format(fecha_today-2))
  ),
  "PERSIANN-CCS" = list("Precipitaci\u00F3n" =  list('2003-01-01',format(fecha_today-1))), 
  "GPM (GPM_3IMERGDE)" = list("Precipitaci\u00F3n" =  list('2000-06-01',format(fecha_today-30)))
)



data <- data.frame(
  `Nombre de la base` = c(
    rep("ERA5 hourly data on single levels from 1940 to present", 8),
    "CHIRPS", 
    "GPCC-ALL PRODUCTS", 
    rep("NASA-POWER", 6), 
    "CPC Global Unified Gauge-Based Analysis of Daily Precipitation", 
    rep("CPC Global Daily Temperature", 2),
    rep("NCAR/UCAR Reanalysis", 6),
    "PERSIANN-CCS",
    "GPM (GPM_3IMERGDE)"
  ),
  Tipo = c(
    rep("Reanalysis-Monitoreo", 8),
    "Monitoreo", 
    "Reanalysis-Monitoreo",
    rep("Monitoreo", 6), 
    rep("Reanalysis-Monitoreo", 3),
    rep("Reanalysis", 6),
    "Monitoreo",  
    "Monitoreo"
  ),
  Periodo = c(
    rep("1940-Presente", 8),
    "1981-presente",
    "2009-presente",
    rep("1981-presente", 6),
    rep("1979-presente", 3),
    rep("1948-presente", 6),
    "2003-presente", 
    "06/2000-presente"
  ),
  `Dias de retraso` = c(
    rep("5-8 d\u00EDas", 8),
    "5-6 d\u00EDas", 
    "4 d\u00EDas", 
    rep("1 d\u00EDa", 5), 
    "2 d\u00EDas",
    rep("2 d\u00EDas", 3),
    rep("2-3 d\u00EDas", 6),
    "1 d\u00EDa", 
    "1 d\u00EDa"
  ),
  Parametro = c(
    "Precipitaci\u00F3n", "Temperatura m\u00E1xima", "Temperatura m\u00EDnima", "Vientos u", "Vientos v", "Humedad del suelo (0 - 7cm)", "Humedad del suelo  (7 - 28cm)", "Humedad del suelo  (28 - 100cm)",
    "Precipitaci\u00F3n",
    "Precipitaci\u00F3n", 
    "Temperatura m\u00E1xima", "Temperatura m\u00EDnima", "humedad relativa", "Precipitaci\u00F3n", "Humedad del suelo (<5cm)", "Humedad del suelo (<100cm)",
    "Precipitaci\u00F3n", "Temperatura m\u00E1xima", "Temperatura m\u00EDnima",
    "Vientos u", "Vientos v", "Presi\u00F3n atmosf\u00E9rica a nivel del mar", "Altura Geopotencial 200", "Altura Geopotencial 500", "Altura Geopotencial 850",
    "Precipitaci\u00F3n", 
    "Precipitaci\u00F3n"
  ),
  Unidad = c("m", "°C", "°C", "m/s", "m/s", "m³/m³", "m³/m³", "m³/m³", "mm", "mm", "°C", "°C", "%", "kg m⁻² s⁻¹", "1", "1", "mm", "°C", "°C", "m/s", "m/s", "Pascals", "millibar", "millibar", "millibar", "mm/day", "mm"),
  
  Precision = c("4", "1", "1", "1", "1", "6", "6", "6", "1", "1", "1", "1", "1", "6", "-", "-", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1"),
  
  `Resolucion espacial` = c(
    rep("0.25x0.25", 8),
    "0.05x0.05",
    "1x1", 
    rep("0.5x0.625", 6),
    rep("0.5x0.5",  3),
    rep("2.5x2.5", 6),
    "0.04x0.04", 
    "0.1x0.1"
  ),
  `Resolucion temporal` = c(
    rep("Horario", 8),
    "diario", 
    "diario", 
    rep("diario", 6), 
    rep("diario", 3),
    rep("diario", 6),
    "diario", 
    "diario"
  ),
  Acceso = c(
    rep("https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels?tab=overview", 8),
    "https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_daily/tifs/p05/ https://data.chc.ucsb.edu/products/CHIRPS-2.0/prelim/global_daily/tifs/p05/",
    "https://opendata.dwd.de/climate_environment/GPCC/first_guess_daily/",
    rep("https://power.larc.nasa.gov/data/", 6),
    rep("https://downloads.psl.noaa.gov/Datasets/cpc_global_precip/", 3),
    rep("https://psl.noaa.gov/data/gridded/data.ncep.reanalysis.html", 6),
    "https://chrsdata.eng.uci.edu/",
    "https://gpm1.gesdisc.eosdis.nasa.gov/data/GPM_L3/GPM_3IMERGDE.06/"
  )
)


# Convertir los enlaces en HTML
data$Acceso <- ifelse(data$Acceso != "", paste0('<a href="', data$Acceso, '" target="_blank">', data$Acceso, '</a>'), data$Acceso)


# Invocar funciones

source("/opt/shiny-server/samples/sample-apps/app_bases/Scripts_database/CHIRPS_GENERATOR.R")
source("/opt/shiny-server/samples/sample-apps/app_bases/Scripts_database/NASA_POWER_GENERATOR.R")
source("/opt/shiny-server/samples/sample-apps/app_bases/Scripts_database/CPC_GENERATOR.R")
source("/opt/shiny-server/samples/sample-apps/app_bases/Scripts_database/PERSIANN_GENERATOR.R")
source("/opt/shiny-server/samples/sample-apps/app_bases/Scripts_database/GPCC_generator.R")
source("/opt/shiny-server/samples/sample-apps/app_bases/Scripts_database/ERA5_GENERATOR.R")
source("/opt/shiny-server/samples/sample-apps/app_bases/Scripts_database/NCEP_NCAR_GENERATOR.R")
source("/opt/shiny-server/samples/sample-apps/app_bases/Scripts_database/GPM_GENERATOR.R")

