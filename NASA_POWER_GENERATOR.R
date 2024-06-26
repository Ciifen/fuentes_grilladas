library(R.utils)
library(curl)
library(raster)
library(parallel)
library(stringr)
library(RCurl)
library(ncdf4)
library(doParallel)
library(foreach)

NASA_POWER <- new.env()

NASA_POWER$obtener_nombre_variable <- function(parametro) {
  switch(parametro,
         'TMAX' = 'T2M_MAX',
         'TMIN' = 'T2M_MIN',
         'PCP' = 'PRECTOTCORR',
         'HRELATIVA' = 'RH2M',
         #'HSOIL'='HSOIL',
         'HSOILROOT'='GWETROOT',
         'HSOILTOP'='GWETTOP',
         stop("Parámetro no válido")
  )
}



# Definir el número de núcleos a utilizar
NASA_POWER$num_cores <- 2
# Registrar el backend paralelo con el número de núcleos especificado
registerDoParallel(NASA_POWER$num_cores)

NASA_POWER$generar_urls <- function(fecha_inicial, fecha_final){
  
  # URLs de las páginas
  urlbase <- "https://power-datastore.s3.amazonaws.com/v9/daily/"
  
  # Listas para almacenar las URLs
  urls <- list()
  urls_tif_filename <- list()  # Nueva lista para las URLs de archivos TIF
  
  # Iterar sobre el rango de fechas y construir las URLs
  rango <- seq.Date(fecha_inicial, fecha_final, by = "day")
  for (fecha in as.character(rango)) {
    
    anio_curr <- format(as.Date(fecha), "%Y")
    month_curr <- format(as.Date(fecha), "%m")
    date_curr <- format(as.Date(fecha), "%Y%m%d")
    url_directorio <- paste0(anio_curr,"/",month_curr,"/")
    filename <- paste0("power_901_daily_",date_curr ,"_merra2_utc.nc")
    
    url <- paste0(urlbase,url_directorio, filename)
    urls <- c(urls, url)
    
    # Construir el nombre del archivo TIF
    YMD <- format(as.Date(fecha), "%Y%m%d")
    filename_tif <- paste0(YMD, ".tif")
    url_tif_filename <- paste0(filename_tif)
    urls_tif_filename <- c(urls_tif_filename, url_tif_filename)
  }
  return(list(lista_urls = urls, urls_tif_filename = urls_tif_filename))
}

#Funcion para crear subdirectorios
NASA_POWER$crear_subdirectorios <- function(base, ...) {
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


NASA_POWER$downloads_transformar_a_tif <- function(Rango_fecha, lugar, parametro) {
  
  nombre_variable <- NASA_POWER$obtener_nombre_variable(parametro)
  
  fecha_inicial <- Rango_fecha[[1]]
  fecha_final <- Rango_fecha[[2]]
  
  output_urls <- NASA_POWER$generar_urls(fecha_inicial, fecha_final)
  urls <- output_urls$lista_urls
  urls_tif_filename <- output_urls$urls_tif_filename
  
  url_base <- '/opt/shiny-server/samples/sample-apps/app_bases/NASA_POWER'
  
  NASA_POWER$crear_subdirectorios(url_base, parametro)
  NASA_POWER$crear_subdirectorios(url_base, parametro, 'diario')
  NASA_POWER$crear_subdirectorios(url_base, parametro, 'diario','PAIS')
  
  url_bolivia <- NASA_POWER$crear_subdirectorios(url_base, parametro, 'diario','PAIS', 'BOLIVIA')
  url_chile <- NASA_POWER$crear_subdirectorios(url_base, parametro, 'diario','PAIS','CHILE')
  url_colombia <- NASA_POWER$crear_subdirectorios(url_base, parametro, 'diario','PAIS','COLOMBIA')
  url_ecuador <- NASA_POWER$crear_subdirectorios(url_base, parametro, 'diario','PAIS','ECUADOR')
  url_venezuela <- NASA_POWER$crear_subdirectorios(url_base, parametro, 'diario','PAIS','VENEZUELA')
  url_peru <- NASA_POWER$crear_subdirectorios(url_base, parametro, 'diario','PAIS','PERU')
  directorio_temp <- NASA_POWER$crear_subdirectorios(url_base, 'temp')
  directorio_tif <- NASA_POWER$crear_subdirectorios(url_base, parametro, 'diario', 'tif')
  
  name_outputFile <- paste0(lugar, '_',parametro,'_',fecha_inicial,'_',fecha_final,'.tif')
  outputFile <- file.path(directorio_tif, name_outputFile)
  
  
  coordenadas_list <- list(
    VENEZUELA = c(lonL = -73.378, lonR = -59.8035,latB = 0.6499, latT = 12.8011 ),
    COLOMBIA = c(lonL = -81, lonR = -66.8694, latB = -4.2271,latT = 13.5),
    ECUADOR = c(lonL = -91.38, lonR = -75.1867,  latB = -5.014, latT = 1.6742),
    PERU = c(lonL = -81.3269, lonR = -68.6651, latB = -18.3496, latT = 0.012),
    BOLIVIA = c(lonL = -69.6409, lonR = -57.453,  latB = -22.8969,latT = -9.6805),
    CHILE = c(lonL = -109.5, lonR = -66.4173, latB = -60, latT = -17.5065)
  )
  
  urls_paises <- list(
    ven = url_venezuela,
    col = url_colombia,
    ecu = url_ecuador,
    per = url_peru,
    bol = url_bolivia,
    chi = url_chile
  )
  if (lugar %in% names(urls_paises)) {
    url_pais <- urls_paises[[lugar]]
  } else {
    stop("Lugar no válido")
  }
  
  
  if (file.exists(outputFile)){
    message("Ya se encuentra el archivo generado")
    # Detenemos la ejecución del script
    return(NULL)
  } else{
    
    
    
    #for (i in seq_along(urls)) {
    
    foreach(
      i = seq_along(urls),
      .combine = c,
      .packages = c("raster", "curl", "stringr", "R.utils", "ncdf4", "RCurl", "stringr"),
      .export = c("urls", "urls_tif_filename", "url_pais", "directorio_temp")
    ) %dopar% {
      url <- urls[i]
      url_tif_filename <-paste0(url_pais,'/' ,urls_tif_filename[i]) 
      print(paste("Processing URL:", url, ' tif:', url_tif_filename))
      
      
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
      
      
      if (!existe(url_tif_filename)) {
        
        # El archivo .tif no existe, descárgalo y procesa
        tryCatch(
          {
            #Extraer la fecha de la ruta url_tif_filename
            fecha <- substr(basename(url_tif_filename), 1, 8)
            setwd(directorio_temp)
            # Especifica la ruta al archivo .nc
            ruta_archivo <- paste0("power_901_daily_",fecha ,"_merra2_utc.nc")
            file_temp <- paste0(directorio_temp,'/',ruta_archivo)
            
            if (file.exists(file_temp)) {
              if (as.Date(file.info(file_temp)$ctime) != Sys.Date()){
                file.remove(file_temp)
                system(paste0("wget --no-check-certificate --load-cookies ~/.urs_cookies --save-cookies ~/.urs_cookies --keep-session-cookies --content-disposition ", url))
              }
            }else{
              print('No se encuentra descargado')
              system(paste0("wget --no-check-certificate --load-cookies ~/.urs_cookies --save-cookies ~/.urs_cookies --keep-session-cookies --content-disposition ", url))
            }
            
            # Abre el archivo NetCDF
            
            tryCatch({
              archivo_nc <- nc_open(ruta_archivo)
              nc_close(archivo_nc)
            }, error = function(cond) {
              print(paste("Error:", cond$message))
              print("Volviendo a descargar...")
              system(paste0("wget --no-check-certificate --load-cookies ~/.urs_cookies --save-cookies ~/.urs_cookies --keep-session-cookies --content-disposition ", url))
            })
            
            archivo_nc <- nc_open(ruta_archivo)
            lon <- ncvar_get(archivo_nc, "lon")
            lat <- ncvar_get(archivo_nc, "lat")
            # Cierra el archivo NetCDF
            nc_close(archivo_nc)
            # Define la extensión geográfica del raster
            lon_min <- min(lon)
            lon_max <- max(lon)
            lat_min <- min(lat)
            lat_max <- max(lat)
            # Define la extensión geográfica del raster
            extension_correcta <- raster::extent(lon_min, lon_max, lat_min, lat_max)
            
            #Abrir archivo como raster
            # Crea el objeto raster con los datos y la resolución
            r <- raster(ruta_archivo, varname=nombre_variable)
            # Define la extensión geográfica del raster
            extent(r) <- extension_correcta
            r[r<0] <- NA
            r <- round(r, 1)
            #Recortar raster para cada pais
            for (num in 1:length(coordenadas_list)){
              raster_recortado <- raster::crop(r,  raster::extent(coordenadas_list[[num]]))
              file <- paste0(fecha,'.tif')
              output <- file.path(url_base, parametro, 'diario','PAIS',names(coordenadas_list)[num],file )
              if (!file.exists(output)){
                raster::writeRaster(raster_recortado, output, format = "GTiff", overwrite = TRUE)
                print(paste("Archivo TIF guardado en:", output))
              }
              
            }
            rm(r)
            gc()
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


