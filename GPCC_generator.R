library(R.utils)
library(curl)
library(raster)
library(parallel)
library(stringr)
library(RCurl)
library(ncdf4)
library(lubridate)
library(doParallel)
library(foreach)

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
  
  url_base <- '/opt/shiny-server/samples/sample-apps/app_bases/GPCC'
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
  
  name_outputFile <- paste0(lugar, '_',parametro,'_',fecha_inicial,'_',fecha_final,'.tif')
  outputFile <- file.path(directorio_tif, name_outputFile)
  
  
  
  
  
  coordenadas_list <- list(
    ven = c(lonL = -73.378, lonR = -59.8035,latB = 0.6499, latT = 12.2011 ),
    col = c(lonL = -79.3667, lonR = -66.8694, latB = -4.2271,latT = 12.4583),
    ecu = c(lonL = -81.0833, lonR = -75.1867,  latB = -5.014, latT = 1.6742),
    per = c(lonL = -81.3269, lonR = -68.6651, latB = -18.3496, latT = 0.012),
    bol = c(lonL = -69.6409, lonR = -57.453,  latB = -22.8969,latT = -9.6805),
    chi = c(lonL = -75.6445, lonR = -66.4173, latB = -45, latT = -17.5065)
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
  
  
  if (file.exists(outputFile)){
    message("Ya se encuentra el archivo generado")
    # Detenemos la ejecución del script
    #return(NULL)
  } else{
    
    
    #Descargar archivos nc.gz
    for (i in seq_along(urls)) {
      url <- urls[i][[1]]
      print(paste("Processing URL:", url))
      file_temp <- paste0(directorio_temp, '/', basename(url))
      setwd(directorio_temp)
      

      
      
      if (!existe(paste0(file_temp))) {
        print('No se encuentra descargado')
        
        system(paste0("wget --no-check-certificate --load-cookies ~/.urs_cookies --save-cookies ~/.urs_cookies --keep-session-cookies --content-disposition ", url))
      }else{
        print('Ya se encuentra descargado')
      }
    }
    
    #Procesar archivo nc.gz mensual a tif diario
    #for (i in seq_along(urls_tif_filename)) {
    
    foreach(
      i = seq_along(urls_tif_filename),
      .combine = c,
      .packages = c("raster", "curl", "stringr", "R.utils", "ncdf4", "RCurl", "stringr"),
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
    
  }

  
  setwd(dirname(outputFile))
  # Crear el nombre del archivo zip con la misma basename pero extensión .zip
  zipfile <- sub("\\.tif$", ".zip", basename(outputFile))
  # Comprimir el archivo raster en un archivo zip
  zip(zipfile, files = basename(outputFile))
  
  return(zipfile)
}



GPCC$Rango_fecha <- list(as.Date("2023-04-15"), as.Date("2023-04-15"))
GPCC$lugar <- 'ecu'
#GPCC$downloads_transformar_a_tif(GPCC$Rango_fecha, GPCC$lugar)



