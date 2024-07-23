library(R.utils)
library(curl)
library(raster)
library(parallel)
library(stringr)
library(ncdf4)
library(lubridate)
library(doParallel)
library(foreach)
#library(RCurl)

library(future)
plan(multisession)

GPCC <- new.env()

#La funcion valida si el archivo existe y es mayor a 0 el tama;o
GPCC$existe <- function(file) {
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

GPCC$generate_raster_info_txt <- function(raster_path) {
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





GPCC$generar_urls <- function(fecha_inicial, fecha_final){
  
  
  seqfechas <- seq(fecha_inicial, fecha_final, by='month')
  
  # URLs de las páginas
  urlbase <- "https://opendata.dwd.de/climate_environment/GPCC/first_guess_daily/"
  
  # Listas para almacenar las URLs
  urls <- list()
  urls_tif_filename <- list()  # Nueva lista para las URLs de archivos TIF
  
  # Iterar sobre el rango de fechas y construir las URLs
  rango <- seq.Date(fecha_inicial, fecha_final, by = "month")
  for (fecha in as.character(rango)) {
    anio_curr <- format(as.Date(fecha), "%Y")
    date_curr <- format(as.Date(fecha), "%Y%m")
    
    url_directorio <- paste0(anio_curr,"/")
    filename <- paste0("first_guess_daily_",date_curr ,".nc.gz")
    
    url <- paste0(urlbase,url_directorio, filename)
    urls <- c(urls, url)
  }
  
  rango <- seq.Date(fecha_inicial, fecha_final, by = "day")
  for (fecha in as.character(rango)) {
    
    # Construir el nombre del archivo TIF
    YMD <- format(as.Date(fecha), "%Y%m%d")
    url_tif_filename <- paste0("gpcc", YMD, ".tif")
    urls_tif_filename <- c(urls_tif_filename, url_tif_filename)
    
  }
  return(list(lista_urls = urls, urls_tif_filename = urls_tif_filename))
}

#Funcion para crear subdirectorios
GPCC$crear_subdirectorios <- function(base, ...) {
  subdirs <- c(base, list(...))
  ruta <- Reduce(file.path, subdirs)
  
  if (!dir.exists(ruta)) {
    dir.create(ruta, recursive = TRUE, showWarnings = FALSE)
    Sys.chmod(ruta, mode = "7777", use_umask = FALSE)
    print(paste("La carpeta", ruta, "no existía y fue creada."))
  } else {
    #print(paste("La carpeta", ruta, "ya existe."))
  }
  return(ruta)
}

GPCC$downloads_transformar_a_tif <- function(Rango_fecha, lugar, parametro) {
  
  # Definir el número de núcleos a utilizar
  num_cores <- 2
  
  # Registrar el backend paralelo con el número de núcleos especificado
  registerDoParallel(num_cores)
  
  
  fecha_inicial <- Rango_fecha[[1]]
  fecha_final <- Rango_fecha[[2]]
  
  
  # Lista con urls
  output_urls <- GPCC$generar_urls(fecha_inicial, fecha_final)
  urls <- output_urls$lista_urls
  urls_tif_filename <- output_urls$urls_tif_filename
  
  url_base <- '/srv/shiny-server/datosgrillados/GPCC'
  GPCC$crear_subdirectorios(url_base, 'PREC')
  GPCC$crear_subdirectorios(url_base, 'PREC', 'diario')
  GPCC$crear_subdirectorios(url_base, 'PREC', 'diario','PAIS')
  GPCC$crear_subdirectorios(url_base, 'PREC', 'diario','tif')
  url_bolivia <- GPCC$crear_subdirectorios(url_base, 'PREC', 'diario','PAIS', 'BOLIVIA')
  url_chile <- GPCC$crear_subdirectorios(url_base, 'PREC', 'diario','PAIS','CHILE')
  url_colombia <- GPCC$crear_subdirectorios(url_base, 'PREC', 'diario','PAIS','COLOMBIA')
  url_ecuador <- GPCC$crear_subdirectorios(url_base, 'PREC', 'diario','PAIS','ECUADOR')
  url_venezuela <- GPCC$crear_subdirectorios(url_base, 'PREC', 'diario','PAIS','VENEZUELA')
  url_peru <- GPCC$crear_subdirectorios(url_base, 'PREC', 'diario','PAIS','PERU')
  directorio_temp <- GPCC$crear_subdirectorios(url_base, 'PREC', 'diario', 'temp')
  directorio_tif <- GPCC$crear_subdirectorios(url_base, 'PREC', 'diario', 'tif')
  
  name_outputFile <- paste0("GPCC_",lugar, '_',parametro,'_',fecha_inicial,'_',fecha_final,'.tif')
  outputFile <- file.path(directorio_tif, name_outputFile)
  name_outputFile_zip <- paste0("GPCC_",lugar, '_',parametro,'_',fecha_inicial,'_',fecha_final,'.zip')
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
  
  
  if (GPCC$existe(outputFile_zip)){
    message("Ya se encuentra el archivo generado")
    # Detenemos la ejecución del script
  } else{
    future({
      
      library(R.utils)
      library(curl)
      library(raster)
      library(parallel)
      library(stringr)
      library(ncdf4)
      library(lubridate)
      library(doParallel)
      library(foreach)
    
    #Descargar archivos nc.gz
    for (i in seq_along(urls)) {
      url <- urls[i][[1]]
      print(paste("Processing URL:", url))
      file_temp <- paste0(directorio_temp, '/', basename(url))
      setwd(directorio_temp)
      
      if (!GPCC$existe(paste0(file_temp))) {
        print('No se encuentra descargado')
        tryCatch(
          {
            system(paste0("wget --no-check-certificate --load-cookies ~/.urs_cookies --save-cookies ~/.urs_cookies --keep-session-cookies --content-disposition ", url))
          },
          error = function(cond) {
            # Si se produce una excepción, se maneja aquí
            print(paste("Error:", cond$message))
          }
        )
      }else{
        print('Ya se encuentra descargado')
      }
      
      
    }
    
    #Procesar archivo nc.gz mensual a tif diario
    #for (i in seq_along(urls_tif_filename)) {
    
    foreach(
      i = seq_along(urls_tif_filename),
      .combine = c,
      .packages = c("raster", "curl", "stringr", "R.utils", "ncdf4", "stringr"),
      .export = c("urls", "urls_tif_filename", "url_pais", "coordenadas_pais", "directorio_temp")
    ) %dopar% {
      
      url_tif_filename <-urls_tif_filename[i][[1]]
      url_tif_pais <- file.path(url_pais, url_tif_filename)
      
      setwd(directorio_temp)
      print(paste("Processing URL:", url, ' tif:', url_tif_filename))
      
      if (!GPCC$existe(url_tif_pais)) {
        # El archivo .tif no existe, descárgalo y procesa
        tryCatch(
          {
            #Extraer la fecha de la ruta url_tif_filename
            fecha <- substr(url_tif_filename, 5, 12)
            year_mes <- substr(url_tif_filename, 5, 10)
            url <- paste0(directorio_temp,"/","first_guess_daily_", year_mes,".nc.gz")
            archivo_temporal <- file.path(directorio_temp, basename(url))
            file_nc <- substr(archivo_temporal, 1, nchar(archivo_temporal) - 3)
            
            #gunzip archivo temporal first.....nc.gz
            if (!file.exists(file_nc)) {
              R.utils::gunzip(archivo_temporal)
            } else {
              print(paste("El archivo", archivo_temporal, "ya esta descomprimido."))
            }
            
            #el nombre del archivo sin gz
            raster_stack <- raster::stack(file_nc, varname="p")
            #extraer fechas de todo el nc
            dates <- as.Date(names(raster_stack), format = "X%Y.%m.%d")
            #extraer la fecha del tif que quiero extraer
            date= as.Date(fecha,  format = "%Y%m%d")
            layer_indices <- which(dates == date) #Extra el indice que la fecha seleccionada
            raster <- raster_stack[[layer_indices]]#Extrae la fecha seleccionada
            # Definir la extensión (ajusta las coordenadas según tu necesidad)
            extension_pais <- raster::extent(coordenadas_pais)
            # Recortar el RasterStack
            r <- raster::crop(raster, extension_pais)
            r[r<0] <- NA
            r <- round(r, 1)
            # Construir el nombre de salida en el formato deseado
            
            raster::writeRaster(r, url_tif_pais, format = "GTiff", overwrite = TRUE)
            
            print(paste("Archivo TIF guardado en:", url_tif_filename))
          },
          error = function(cond) {
            # Si se produce una excepción, se maneja aquí
            print(paste("Error:", cond$message))
          }
        )
      } else {
        # El archivo .tif ya existe en la carpeta, omite la descarga y procesamiento
        print(paste("El archivo TIF ya existe en:", url_tif_filename))
      }
    }
    stopImplicitCluster()
    setwd(url_pais)
    raster_data <- stack(urls_tif_filename)
    writeRaster(raster_data, filename=outputFile, overwrite=TRUE)
    setwd(dirname(outputFile))
    out_txt <- GPCC$generate_raster_info_txt(outputFile)
    # Crear el nombre del archivo zip con la misma basename pero extensión .zip
    #zipfile <- sub("\\.tif$", ".zip", basename(outputFile))
    # Comprimir el archivo raster en un archivo zip
    zip(outputFile_zip, files = c(basename(outputFile),out_txt))
    
    }, globals = list(
      urls = urls,
      url_pais=url_pais,
      urls_tif_filename=urls_tif_filename,
      directorio_temp = directorio_temp,
      outputFile = outputFile,
      outputFile_zip=outputFile_zip,
      coordenadas_list = coordenadas_list,
      coordenadas_pais=coordenadas_pais,
      GPCC = GPCC,
      fecha_inicial = fecha_inicial,
      fecha_final = fecha_final
    ))
    
  }
  return(outputFile_zip)
}


