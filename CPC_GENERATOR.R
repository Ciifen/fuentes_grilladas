library(doParallel)
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

CPC <- new.env()

CPC$obtener_nombre_variable <- function(parametro) {
  switch(parametro,
         'TMIN' = 'tmin',
         'TMAX' = 'tmax',
         'PCP' = 'precip',
         stop("Parámetro no válido")
  )
}

CPC$generar_urls <- function(Rango_fecha, parametro){
  
  fecha_inicial <- Rango_fecha[[1]]
  fecha_final <- Rango_fecha[[2]]
  
  seqfechas <- seq(fecha_inicial, fecha_final, by='year')
  
  # Listas para almacenar las URLs
  urls <- list()
  urls_tif_filename <- list()  # Nueva lista para las URLs de archivos TIF
  
  # Iterar sobre el rango de fechas y construir las URLs
  rango <- seq.Date(fecha_inicial, fecha_final, by = "year")
  
  #definir url de descarga
  if (parametro == 'TMIN' ){
    urlbase <- 'https://downloads.psl.noaa.gov/Datasets/cpc_global_temp/'
  }else if (parametro == 'TMAX'){
    urlbase <- 'https://downloads.psl.noaa.gov/Datasets/cpc_global_temp/'
  }else if (parametro == 'PCP'){
    urlbase <- 'https://downloads.psl.noaa.gov/Datasets/cpc_global_precip/'
  }
  
  for (fecha in as.character(rango)) {
    anio_curr <- format(as.Date(fecha), "%Y")
    filename <- paste0(CPC$obtener_nombre_variable(parametro),'.',anio_curr ,".nc")
    url <- paste0(urlbase, filename)
    urls <- c(urls, url)
  }
  
  rango <- seq.Date(fecha_inicial, fecha_final, by = "day")
  
  for (fecha in as.character(rango)) {
    
    # Construir el nombre del archivo TIF
    YMD <- format(as.Date(fecha), "%Y%m%d")
    url_tif_filename <- paste0(parametro, YMD, ".tif")
    urls_tif_filename <- c(urls_tif_filename, url_tif_filename)
  }
  return(list(lista_urls = urls, urls_tif_filename = urls_tif_filename, fecha_inicial=fecha_inicial, fecha_final=fecha_final))
}

#Funcion para crear subdirectorios
CPC$crear_subdirectorios <- function(base, ...) {
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



CPC$downloads_transformar_a_tif <- function(Rango_fecha, lugar, parametro) {
  
  nombre_variable <- CPC$obtener_nombre_variable(parametro)
  
  # Definir el número de núcleos a utilizar
  #num_cores <- detectCores()-4
  #num_cores <- 2
  
  # Registrar el backend paralelo con el número de núcleos especificado
  #registerDoParallel(num_cores)
  
  
  # Lista con urls
  output_urls <- CPC$generar_urls(Rango_fecha, parametro)
  urls <- output_urls$lista_urls
  urls_tif_filename <- output_urls$urls_tif_filename
  
  url_base <- '/opt/shiny-server/samples/sample-apps/app_bases/CPC'
  
  CPC$crear_subdirectorios(url_base, parametro)
  CPC$crear_subdirectorios(url_base, parametro, 'diario')
  CPC$crear_subdirectorios(url_base, parametro, 'diario','PAIS')
  
  url_bolivia <- CPC$crear_subdirectorios(url_base, parametro, 'diario','PAIS', 'BOLIVIA')
  url_chile <- CPC$crear_subdirectorios(url_base, parametro, 'diario','PAIS','CHILE')
  url_colombia <- CPC$crear_subdirectorios(url_base, parametro, 'diario','PAIS','COLOMBIA')
  url_ecuador <- CPC$crear_subdirectorios(url_base, parametro, 'diario','PAIS','ECUADOR')
  url_venezuela <- CPC$crear_subdirectorios(url_base, parametro, 'diario','PAIS','VENEZUELA')
  url_peru <- CPC$crear_subdirectorios(url_base, parametro, 'diario','PAIS','PERU')
  directorio_temp <- CPC$crear_subdirectorios(url_base, parametro, 'diario', 'temp')
  directorio_tif <- CPC$crear_subdirectorios(url_base, parametro, 'diario', 'tif')
  
  name_outputFile <- paste0(lugar, '_',parametro,'_',output_urls$fecha_inicial,'_',output_urls$fecha_final,'.tif')
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
    message(paste0("Ya se encuentra el archivo generado"), outputFile)
    # Detenemos la ejecución del script
    #return(NULL)
    
    
    
  }else{
    
    #Descargar archivos nc
    for (i in seq_along(urls)) {
      url <- urls[i][[1]]
      print(paste("Processing URL:", url))
      file_temp <- paste0(directorio_temp, '/', basename(url))
      setwd(directorio_temp)
      if (file.exists(file_temp)) {
        if (as.Date(file.info(file_temp)$ctime) != Sys.Date()){
          file.remove(file_temp)
          system(paste0("wget --no-check-certificate --load-cookies ~/.urs_cookies --save-cookies ~/.urs_cookies --keep-session-cookies --content-disposition ", url))
        }
        
      }else{
        system(paste0("wget --no-check-certificate --load-cookies ~/.urs_cookies --save-cookies ~/.urs_cookies --keep-session-cookies --content-disposition ", url))
      }
    }
    
    #Procesar archivo nc anual a tif diario
    for (i in seq_along(urls_tif_filename)) {
      
      #foreach(
      #  i = seq_along(urls_tif_filename),
      #  .combine = c,
      #  .packages = c("raster", "curl", "stringr", "R.utils", "ncdf4", "RCurl", "stringr"),
      #  .export = c("urls", "urls_tif_filename", "url_pais", "coordenadas_pais", "directorio_temp")
      #) %dopar% {
      
      url_tif_filename <-urls_tif_filename[i][[1]]
      url_tif_pais <- file.path(url_pais, url_tif_filename)
      
      
      existe <- function(file) {
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
      
      
      if (!existe(url_tif_pais)) {
        
        # El archivo .tif no existe, descárgalo y procesa
        tryCatch(
          {
            
            #Extraer la fecha de la ruta url_tif_filename
            fecha <- substr(url_tif_filename, nchar(parametro)+1, nchar(parametro)+8)
            year <- substr(url_tif_filename, nchar(parametro)+1, nchar(parametro)+4)
            archivo_temporal <- paste0(directorio_temp,"/",nombre_variable,'.', year,".nc")
            
            # Convertir la fecha a formato Date
            fecha_seleccionada <- as.Date(fecha, format = "%Y%m%d")
            #Leer archivo nc
            raster_stack <- raster::stack(archivo_temporal)
            
            #extraer fechas de todo el nc
            dates <- as.Date(names(raster_stack), format = "X%Y.%m.%d")
            #extraer la fecha del tif que quiero extraer
            date= as.Date(fecha,  format = "%Y%m%d")
            layer_indices <- which(dates == fecha_seleccionada) #Extra el indice que la fecha seleccionada
            raster_seleccionado <- raster_stack[[layer_indices]]#Extrae la fecha seleccionada
            #rotar coordenadaas
            raster <- raster::rotate(raster_seleccionado)
            
            # Definir la extensión (ajusta las coordenadas según la necesidad)
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
    
    #stopImplicitCluster()
    
    
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








