# =============================================================================
# fuentes_grilladas — Descarga de datos climáticos e hidrológicos en formato ráster
# -----------------------------------------------------------------------------
# Copyright (C) 2025 CIIFEN — Centro Internacional para la Investigación
# del Fenómeno de El Niño (https://ciifen.org)
# Author       : Iliana Salazar
#                Desarrolladora de Servicios Climáticos
#                CIIFEN
# Project      : Proyecto ENANDES
# System       : Plataforma web de productos y servicios climáticos
# Repository   : https://github.com/Ciifen/fuentes_grilladas
# Contact      : Pier Maquilón — p.maquilon@ciifen.org
#
# All products resulting from this work are the exclusive property of CIIFEN.
# Attribution to the author must be preserved in all copies and
# derivative works.
# =============================================================================

#install.packages("R.methodsS3", dependencies=TRUE);install.packages("sp", dependencies=TRUE);install.packages("R.oo", dependencies=TRUE)
#install.packages("Rcpp", dependencies=TRUE);install.packages("terra", dependencies=TRUE);install.packages("raster", dependencies=TRUE)
#install.packages("R.utils");install.packages("curl");install.packages("raster");install.packages("parallel");install.packages("stringr")
#install.packages("stringr");install.packages("doParallel");install.packages("foreach");install.packages("future")
#install.packages("httr")
library(R.utils)
library(curl)
library(sp)
library(raster)
library(parallel)
library(stringr)
library(ncdf4)
library(doParallel)
library(foreach)
library(future)
library(httr)


CHIRPS <- new.env()


CHIRPS$existe_validado <- function(file, fecha_final) {
  # Validar si el archivo existe y es mayor a 0 utilizando la función 'existe'
  if (CHIRPS$existe(file)) { 
    # Calcular la fecha límite de dos meses antes
    fecha_limite <- Sys.Date() %m-% months(2)
    
    # Si 'fecha_final' es posterior a la fecha límite, retornar FALSE (hacer como si no existiera)
    if (!is.null(fecha_final) && fecha_final >= fecha_limite) {
      return(FALSE)
    }
    # Si el archivo existe y la fecha es válida, retornar TRUE
    return(TRUE)
  }
  # Si no existe, retornar FALSE
  return(FALSE)
}

#La funcion valida si el archivo existe y es mayor a 0 el tama;o
CHIRPS$existe <- function(file) {
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

#La funcion valida si el archivo existe y es mayor a 0 el tama;o
CHIRPS$existe_size <- function(file) {
  if (file.exists(file)) {
    if (file.info(file)$size > 0  & file.info(file)$size < 2000000) {
      return(TRUE)
    } else {
      file.remove(file)
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}




CHIRPS$generate_raster_info_txt <- function(raster_path) {
  #closeAllConnections()
  setwd(dirname(raster_path))
  # Verificar si el archivo existe
  if (!file.exists(raster_path)) {
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
  # Convertir las fechas a formato Date
  fecha_inicial <- as.Date(fecha_inicial, format = "%Y-%m-%d")
  fecha_final <- as.Date(fecha_final, format = "%Y-%m-%d")
  
  # Generar la secuencia de fechas
  fechas <- seq(fecha_inicial, fecha_final, by = "day")
  
  # Añadir la información de las bandas y las fechas
  for (i in 1:length(fechas)) {
    linea <- paste("Banda ", sprintf("%03d", i), ": ", fechas[i], sep = "")
    writeLines(linea, output_file)
  }
  close(output_file)
  #closeAllConnections()
  return(basename(name_file))
}



CHIRPS$generar_urls <- function(fecha_inicial, fecha_final){
  
  # URLs de las páginas
  urlbase <- "https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_daily/tifs/p05/"
  
  # Listas para almacenar las URLs
  urls_definitivo <- list()
  urls_preliminar <- list()
  urls_tif_file_definitivo <- list() 
  urls_tif_file_preliminar <- list()
  
  # Iterar sobre el rango de fechas y construir las URLs
  rango <- seq.Date(fecha_inicial, fecha_final, by = "day")
  for (fecha in as.character(rango)) {
    year <- format(as.Date(fecha), "%Y")
    ymd <- format(as.Date(fecha), "%Y.%m.%d")
    
    url_definitivo <- paste0("https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_daily/tifs/p05/", year, "/chirps-v2.0.", ymd, ".tif.gz")
    url_preliminar <- paste0("https://data.chc.ucsb.edu/products/CHIRPS-2.0/prelim/global_daily/tifs/p05/", year, "/chirps-v2.0.", ymd, ".tif.gz")
    archivo_definitivo <- paste0("chirps_", ymd, "_def.tif")
    archivo_preliminar <- paste0("chirps_", ymd, "_prelim.tif")
    
    urls_definitivo <- c(urls_definitivo, url_definitivo)
    urls_preliminar <- c(urls_preliminar, url_preliminar)
    urls_tif_file_definitivo <- c(urls_tif_file_definitivo, archivo_definitivo)
    urls_tif_file_preliminar <- c(urls_tif_file_preliminar, archivo_preliminar)
  }
  return(list(urls_definitivo = urls_definitivo, urls_preliminar = urls_preliminar, urls_tif_file_definitivo=urls_tif_file_definitivo, urls_tif_file_preliminar=urls_tif_file_preliminar))
}

#Funcion para crear subdirectorios
CHIRPS$crear_subdirectorios <- function(base, ...) {
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


CHIRPS$downloads_transformar_a_tif <- function(Rango_fecha, lugar) {
  
  parametro <- 'pcp'
  tryCatch({    
    fecha_inicial <- as.Date(Rango_fecha[[1]])
    fecha_final <- as.Date(Rango_fecha[[2]])
   
    # Lista con urls
    
    output_urls <- CHIRPS$generar_urls(fecha_inicial, fecha_final)
    urls_definitivo <- output_urls$urls_definitivo
    urls_preliminar <- output_urls$urls_preliminar
    urls_tif_file_definitivo <- output_urls$urls_tif_file_definitivo
    urls_tif_file_preliminar <- output_urls$urls_tif_file_preliminar
    
    url_base <- '/opt/shiny-server/samples/sample-apps/datosgrillados/CHIRPS'
    CHIRPS$crear_subdirectorios(url_base, 'PREC')
    CHIRPS$crear_subdirectorios(url_base, 'PREC', 'diario')
    CHIRPS$crear_subdirectorios(url_base, 'PREC', 'diario','PAIS')
    CHIRPS$crear_subdirectorios(url_base, 'PREC', 'diario','tif')
    url_bolivia <- CHIRPS$crear_subdirectorios(url_base, 'PREC', 'diario','PAIS', 'BOLIVIA')
    url_chile <- CHIRPS$crear_subdirectorios(url_base, 'PREC', 'diario','PAIS','CHILE')
    url_colombia <- CHIRPS$crear_subdirectorios(url_base, 'PREC', 'diario','PAIS','COLOMBIA')
    url_ecuador <- CHIRPS$crear_subdirectorios(url_base, 'PREC', 'diario','PAIS','ECUADOR')
    url_venezuela <- CHIRPS$crear_subdirectorios(url_base, 'PREC', 'diario','PAIS','VENEZUELA')
    url_peru <- CHIRPS$crear_subdirectorios(url_base, 'PREC', 'diario','PAIS','PERU')
    directorio_temp <- CHIRPS$crear_subdirectorios(url_base, 'PREC', 'diario', 'temp')
    directorio_tif <- CHIRPS$crear_subdirectorios(url_base, 'PREC', 'diario', 'tif')
    
    name_outputFile <- paste0("CHIRPS_",lugar, '_',parametro,'_',fecha_inicial,'_',fecha_final,'.tif')
    outputFile <- file.path(directorio_tif, name_outputFile)
    name_outputFile_zip <- paste0("CHIRPS_",lugar, '_',parametro,'_',fecha_inicial,'_',fecha_final,'.zip')
    outputFile_zip <- file.path(directorio_tif, name_outputFile_zip)
    
    
    coordenadas_list <- list(
      ven = c(lonL = -73.378, lonR = -59.8035,latB = 0.6499, latT = 12.8011 ),
      col = c(lonL = -81, lonR = -66.8694, latB = -4.2271,latT = 13.5),
      ecu = c(lonL = -91.38, lonR = -75.1867,  latB = -5.014, latT = 1.6742),
      per = c(lonL = -81.3269, lonR = -68.6651, latB = -18.3496, latT = 0.012),
      bol = c(lonL = -69.6409, lonR = -57.453,  latB = -22.8969,latT = -9.6805),
      chi = c(lonL = -109.5, lonR = -66.4173, latB = -60, latT = -17.5065)
      
    )
    
    urls_paises <- list(
      ven = url_venezuela,
      col = url_colombia,
      ecu = url_ecuador,
      per = url_peru,
      bol = url_bolivia,
      chi = url_chile
    )
    if (lugar %in% names(coordenadas_list)) {
      coordenadas_pais <- coordenadas_list[[lugar]]
      url_pais <- urls_paises[[lugar]]
    } else {
      stop("Lugar no válido")
    }
    
  }, error = function(e) {
    message("Ocurrió un error en el proceso en segundo plano: ", e)

  }, finally = {
    
  })
  
  print(outputFile_zip)
  print(fecha_final)
  
  if (CHIRPS$existe_validado(outputFile_zip, fecha_final)){
    message("Ya se encuentra el archivo generado")

  } else{

    tryCatch({
      
      library(R.utils)
      library(curl)
      library(raster)
      library(parallel)
      library(stringr)
      library(ncdf4)
      library(doParallel)
      library(foreach)

      
      urls_tif_filename <- list() #Lista con los archivos para el stack
      
      for (i in seq_along(urls_definitivo)) {
        #foreach(
        #  i = seq_along(urls_tif_file_definitivo),
        #  .combine = c,
        #  .packages = c("raster", "curl", "stringr", "R.utils", "ncdf4", "stringr"),
        #  .export = c("urls_definitivo", "urls_preliminar","urls_tif_file_definitivo","urls_tif_file_preliminar", "url_pais", "coordenadas_pais", "directorio_temp")
        #) %dopar% {
        url_definitivo <- urls_definitivo[i][[1]]
        url_preliminar <- urls_preliminar[i][[1]]
        url_tif_file_definitivo <-file.path(url_pais, urls_tif_file_definitivo[i]) 
        url_tif_file_preliminar <-file.path(url_pais, urls_tif_file_preliminar[i]) 
        
        print(paste("Processing url: ", url_tif_file_definitivo))
        
        existe <- function(file) {
          if (file.exists(file)) {
            if (file.info(file)$size > 0  & file.info(file)$size < 2000000) {
              return(TRUE)
            } else {
              file.remove(file)
              return(FALSE)
            }
          } else {
            return(FALSE)
          }
        }
        
        if (!CHIRPS$existe_size(url_tif_file_definitivo)) {
          
          # El archivo .tif no existe, descárgalo y procesa
          tryCatch(
            {
              gestionar_descarga_archivos <- function(url_definitivo, url_preliminar, url_tif_file_definitivo, url_tif_file_preliminar) {
                # Función para descargar y descomprimir si es necesario
                download_and_unzip <- function(url, output_file) {
                  output_filegz <- paste0(output_file, ".gz")
                  download.file(url, output_filegz, mode = "wb")
                  cat("Archivo descargado:", output_filegz, "\n")
                  # Descomprimir si el archivo tiene extensión .gz
                  if (grepl("\\.gz$", output_filegz)) {
                    gunzip(output_filegz, remove = TRUE)
                    cat("Archivo descomprimido:", output_file, "\n")
                  }
                  return(output_file)
                }
                
                # Verificar si existe el archivo preliminar
                if (file.exists(url_tif_file_preliminar)) {
                  respuesta <- HEAD(url_definitivo)
                  if (status_code(respuesta) == 200) {
                    # Descargar y descomprimir el archivo definitivo
                    final_file <- download_and_unzip(url_definitivo, url_tif_file_definitivo)
                    cat("Descargado archivo definitivo y eliminado archivo preliminar para", url_definitivo, "\n")
                    file.remove(url_tif_file_preliminar)
                    return(final_file)
                  } else {
                    cat("Archivo preliminar ya descargado para", url_tif_file_preliminar, "\n")
                    return(url_tif_file_preliminar)
                  }
                } 
                else {
                  respuesta <- HEAD(url_definitivo)
                  if (status_code(respuesta) == 200) {
                    # Descargar y descomprimir el archivo definitivo
                    print("archivo definitivo descargandose")
                    final_file <- download_and_unzip(url_definitivo, url_tif_file_definitivo)
                    cat("Descargado archivo definitivo para", url_definitivo, "\n")
                    return(final_file)
                  } else {
                    respuesta <- HEAD(url_preliminar)
                    if (status_code(respuesta) == 200) {
                      print("Archivo preliminar descargandose")
                      # Descargar y descomprimir el archivo preliminar
                      prelim_file <- download_and_unzip(url_preliminar, url_tif_file_preliminar)
                      cat("Descargado archivo preliminar para", url_preliminar, "\n")
                      return(prelim_file)
                    } else {
                      cat("No se encontraron datos para :", url_definitivo, "\n")
                      return(NULL)
                    }
                  }
                }
              }
              
              url_tif_filename <- gestionar_descarga_archivos(url_definitivo, url_preliminar, url_tif_file_definitivo, url_tif_file_preliminar)
              # Crea el objeto raster con los datos y la resolución
              raster_precipitacion <- raster(url_tif_filename)
              # Definir la extensión (ajusta las coordenadas según tu necesidad)
              extension_pais <- raster::extent(coordenadas_pais)
              # Recortar el RasterStack
              r <- raster::crop(raster_precipitacion, extension_pais)
              r[r<0] <- NA
              r <- round(r, 1)
              # Escribir el objeto Raster como un archivo GeoTIFF
              writeRaster(r, filename = url_tif_filename, format = "GTiff", overwrite = TRUE)
              print(paste("Archivo TIF temporal guardado en:", url_tif_filename))
              urls_tif_filename <- c(urls_tif_filename, url_tif_filename)
            },
            error = function(cond) {
              # Si se produce una excepción, se maneja aquí
              print(paste("Error:", cond$message))
            }
          )
        } else {
          # El archivo .tif ya existe en la carpeta, omite la descarga y procesamiento
          print(paste("El archivo TIF definitivo ya existe en:", url_tif_file_definitivo))
          urls_tif_filename <- c(urls_tif_filename, url_tif_file_definitivo)
        }
        
      }
      
      #stopImplicitCluster()
      setwd(url_pais)
      raster_data <- stack(urls_tif_filename)
      writeRaster(raster_data, filename=outputFile, overwrite=TRUE)
      
      rm(raster_data, urls_tif_filename)#Eliminar variable en memoria
      gc()#recolectar basura
      
      setwd(dirname(outputFile))
      out_txt <- CHIRPS$generate_raster_info_txt(outputFile)
      # Crear el nombre del archivo zip con la misma basename pero extensión .zip
      zipfile <- sub("\\.tif$", ".zip", basename(outputFile))
      # Comprimir el archivo raster en un archivo zip
      zip(zipfile, files = c(basename(outputFile),out_txt))

      
    }, error = function(e) {
      message("Ocurrió un error en el proceso en segundo plano: ", e)

    }, finally = {

    })
    
    
  }
  
  
  
  
  return(outputFile_zip)
}




