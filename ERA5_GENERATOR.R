library(R.utils)
library(curl)
library(raster)
library(parallel)
library(stringr)
library(ncdf4)
#library(RCurl)


library(future)
plan(multisession)

ERA5 <- new.env()
#La funcion valida si el archivo existe y es mayor a 0 el tama;o
ERA5$existe <- function(file) {
  if (file.exists(file)) {
    if (file.info(file)$size > 0) {
      return(TRUE)
    } else {
      file.remove(file)
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}

ERA5$generate_raster_info_txt <- function(raster_path) {
  #closeAllConnections()
  setwd(dirname(raster_path))
  # Verificar si el archivo existe
  if (!ERA5$existe(raster_path)) {
    stop("El archivo especificado no existe.")
  }
  
  # Leer el raster
  r <- stack(raster_path)
  
  # Extraer información
  raster_info <- list(
    class = class(r),
    band = nlayers(r),
    dimensions = dim(r),
    resolution = res(r),
    extent = extent(r),
    crs = projection(r),
    names = names(r)
 
  )
  
  # Extraer las fechas del nombre del archivo
  raster_dir<- dirname(raster_path)
  name_without_ext <-  substring(basename(raster_path),1, nchar(basename(raster_path)) - 4 )
  file_name_parts <- unlist(strsplit(name_without_ext, "_"))
  fecha_inicial <- file_name_parts[length(file_name_parts) - 1]
  fecha_final <- file_name_parts[length(file_name_parts)]
  
  # Convertir la lista de información a un formato legible
  base_text <- paste(
    "Class: ", raster_info$class, "\n",
    "Band: ", raster_info$band, "\n",
    "Dimensions: ", paste(raster_info$dimensions, collapse = " x "), "\n",
    "Resolution: ", paste(raster_info$resolution, collapse = ", "), "\n",
    "Extent: ", paste(raster_info$extent[], collapse = ", "), "\n",
    "CRS: ", raster_info$crs, "\n",
    sep = ""
  )
  
  # Crear la ruta para el archivo de texto con el mismo nombre pero extensión .txt
  name_file <- file.path(raster_dir, paste0(name_without_ext, ".txt"))
  output_file <- file(name_file, "w")
  writeLines(base_text, output_file)

  # Convertir las fechas a formato POSIXct para permitir la secuencia por hora
  fecha_inicial <- as.POSIXct(paste0(fecha_inicial," 00:00:00"), format = "%Y-%m-%d %H:%M:%S")
  fecha_final <- as.POSIXct(paste0(fecha_final," 23:00:00"), format = "%Y-%m-%d %H:%M:%S")
  
  # Generar la secuencia de fechas
  fechas <- seq(fecha_inicial, fecha_final, by = "hour")
  
  # Añadir la información de las bandas y las fechas
  for (i in 1:length(fechas)) {
    linea <- paste("Banda ", sprintf("%03d", i), ": ", fechas[i], sep = "")
    writeLines(linea, output_file)
  }
  close(output_file)
  #closeAllConnections()
  return(basename(name_file))
}




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
ERA5$url_base <- '/srv/shiny-server/datosgrillados/ERA5'
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
  url_base <- '/srv/shiny-server/datosgrillados/ERA5'
  directorio_tif <- ERA5$crear_subdirectorios(url_base, parametro, 'diario', 'tif')#Ruta archivo tif para descarga
  
  #Ruta del directorio donde se encuentra archivos solicitdos considera el lugar y paraemtro
  url <- file.path(ERA5$url_base, parametro, 'diario', 'PAIS',lugar)
  
  #Obtiene la ruta del rango de fecha solicitado
  rango <- seq.Date(as.Date(fecha_inicial), as.Date(fecha_final), by = "day")
  rango <- format(rango, "%Y%m%d") 
  
  urls_tif_filename <- paste0(url, '/',rango,'.tif')
  
  #Ruta del directorio donde se guarda el archivo solicitado por el usuario
  name_outputFile <- paste0("ERA5_",lugar, '_',parametro,'_',fecha_inicial,'_',fecha_final,'.tif')
  name_outputFile_zip <- paste0("ERA5_",lugar, '_',parametro,'_',fecha_inicial,'_',fecha_final,'.zip')
  outputFile <- file.path(directorio_tif, name_outputFile)
  outputFile_zip <- file.path(directorio_tif, name_outputFile_zip)
  
  if (ERA5$existe(outputFile_zip)){
    message("Ya se encuentra el archivo generado")
    # Detenemos la ejecución del script
    #return(NULL)
  }else{
    

    future({
      
      library(R.utils)
      library(curl)
      library(raster)
      library(parallel)
      library(stringr)
      library(ncdf4)
      
      raster_data <- stack(urls_tif_filename)
      writeRaster(raster_data, filename=outputFile, overwrite=TRUE)
      print(paste0("Archivo generado: ", outputFile))
      
      setwd(dirname(outputFile))
      out_txt <- ERA5$generate_raster_info_txt(outputFile)
      # Crear el nombre del archivo zip con la misma basename pero extensión .zip
      zipfile <- sub("\\.tif$", ".zip", basename(outputFile))
      # Comprimir el archivo raster en un archivo zip
      zip(zipfile, files = c(basename(outputFile),out_txt))
      
    }, globals = list(
      urls_tif_filename = urls_tif_filename,
      outputFile = outputFile,
      directorio_tif = directorio_tif,
      ERA5 = ERA5,
      name_paises = name_paises,
      fecha_inicial = fecha_inicial,
      fecha_final = fecha_final
    ))

    
  }
  


  
  return(outputFile_zip)
}



