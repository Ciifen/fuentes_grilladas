library(R.utils)
library(curl)
library(raster)
library(parallel)
library(stringr)
library(RCurl)
library(ncdf4)

ERA5 <- new.env()

#Funcion para crear subdirectorios
ERA5$crear_subdirectorios <- function(base, ...) {
  subdirs <- c(base, list(...))
  ruta <- Reduce(file.path, subdirs)
  
  if (!dir.exists(ruta)) {
    dir.create(ruta, recursive = TRUE, showWarnings = FALSE)
    Sys.chmod(ruta, mode = "7777", use_umask = FALSE)
    print(paste("La carpeta", ruta, "no existe."))
  } else {
    #print(paste("La carpeta", ruta, "ya existe."))
  }
  return(ruta)
}


#Crear carpetas
ERA5$url_base <- '/opt/shiny-server/samples/sample-apps/app_bases/ERA5'
ERA5$parametros <- c('u100', 'v100', 'mx2t', 'mn2t', 'tp','swvl1','swvl2','swvl3')

for (parametro in ERA5$parametros){
  ERA5$crear_subdirectorios(ERA5$url_base, parametro)
  ERA5$crear_subdirectorios(ERA5$url_base, parametro, 'diario')
  ERA5$crear_subdirectorios(ERA5$url_base, parametro, 'diario','PAIS')
  ERA5$crear_subdirectorios(ERA5$url_base, 'temp')
  url_bolivia <- ERA5$crear_subdirectorios(ERA5$url_base, parametro, 'diario','PAIS', 'BOLIVIA')
  url_chile <- ERA5$crear_subdirectorios(ERA5$url_base, parametro, 'diario','PAIS','CHILE')
  url_colombia <- ERA5$crear_subdirectorios(ERA5$url_base, parametro, 'diario','PAIS','COLOMBIA')
  url_ecuador <- ERA5$crear_subdirectorios(ERA5$url_base, parametro, 'diario','PAIS','ECUADOR')
  url_venezuela <- ERA5$crear_subdirectorios(ERA5$url_base, parametro, 'diario','PAIS','VENEZUELA')
  url_peru <- ERA5$crear_subdirectorios(ERA5$url_base, parametro, 'diario','PAIS','PERU')
  directorio_temp <- ERA5$crear_subdirectorios(ERA5$url_base, parametro, 'diario', 'temp')
  directorio_tif <- ERA5$crear_subdirectorios(ERA5$url_base, parametro, 'diario', 'tif')
}

ERA5$downloads_transformar_a_tif <- function(Rango_fecha, pais_abreviado, parametro) {
  
  name_paises <- list(
    ven = 'VENEZUELA',
    col = 'COLOMBIA',
    ecu = 'ECUADOR',
    per = 'PERU',
    bol = 'BOLIVIA',
    chi = 'CHILE'
  )
  
  lugar <- name_paises[[pais_abreviado]]
  
  # Lista con urls
  
  fecha_inicial <- Rango_fecha[[1]]
  fecha_final <- Rango_fecha[[2]]
  
  
  #Ruta del directorio de trabajo
  url_base <- '/opt/shiny-server/samples/sample-apps/app_bases/ERA5'
  directorio_tif <- ERA5$crear_subdirectorios(url_base, parametro, 'diario', 'tif')#Ruta archivo tif para descarga
  
  #Ruta del directorio donde se encuentra archivos solicitdos considera el lugar y paraemtro
  url <- file.path(ERA5$url_base, parametro, 'diario', 'PAIS',lugar)
  
  #Obtiene la ruta del rango de fecha solicitado
  rango <- seq.Date(as.Date(fecha_inicial), as.Date(fecha_final), by = "day")
  rango <- format(rango, "%Y%m%d") 
  
  urls_tif_filename <- paste0(url, '/',rango,'.tif')
  
  #Ruta del directorio donde se guarda el archivo solicitado por el usuario
  name_outputFile <- paste0(lugar, '_',parametro,'_',fecha_inicial,'_',fecha_final,'.tif')
  outputFile <- file.path(directorio_tif, name_outputFile)
  
  if (file.exists(outputFile)){
    message("Ya se encuentra el archivo generado")
    # Detenemos la ejecución del script
    #return(NULL)
  }else{
    raster_data <- stack(urls_tif_filename)
    writeRaster(raster_data, filename=outputFile, overwrite=TRUE)
    print(paste0("Archivo generado: ", outputFile))
  }
  
  
  setwd(dirname(outputFile))
  # Crear el nombre del archivo zip con la misma basename pero extensión .zip
  zipfile <- sub("\\.tif$", ".zip", basename(outputFile))
  # Comprimir el archivo raster en un archivo zip
  zip(zipfile, files = basename(outputFile))
  
  
  return(zipfile)
}


