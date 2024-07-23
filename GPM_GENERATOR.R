library(R.utils)
library(curl)
library(raster)
library(parallel)
library(stringr)
library(ncdf4)
library(doParallel)
library(foreach)
#library(RCurl)

library(future)
plan(multisession)

GPM <- new.env()


#La funcion valida si el archivo existe y es mayor a 0 el tama;o
GPM$existe <- function(file) {
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

# Definir el número de núcleos a utilizar
GPM$num_cores <- 2

# Registrar el backend paralelo con el número de núcleos especificado
registerDoParallel(GPM$num_cores)

GPM$generate_raster_info_txt <- function(raster_path) {
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






GPM$generar_urls <- function(fecha_inicial, fecha_final){
  
  # URLs de las páginas
  urlbase <- "https://gpm1.gesdisc.eosdis.nasa.gov/data/GPM_L3/GPM_3IMERGDE.06/"
  
  
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
    filename <- paste0("3B-DAY-E.MS.MRG.3IMERG.",date_curr ,"-S000000-E235959.V06.nc4")
    
    url <- paste0(urlbase,url_directorio, filename)
    urls <- c(urls, url)
    
    # Construir el nombre del archivo TIF
    YMD <- format(as.Date(fecha), "%Y%m%d")
    filename_tif <- paste0("gpm", YMD, ".tif")
    url_tif_filename <- paste0(filename_tif)
    urls_tif_filename <- c(urls_tif_filename, url_tif_filename)
    
  }
  return(list(lista_urls = urls, urls_tif_filename = urls_tif_filename))
}

#Funcion para crear subdirectorios
GPM$crear_subdirectorios <- function(base, ...) {
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


GPM$downloads_transformar_a_tif <- function(Rango_fecha, lugar, parametro) {
  # Lista con urls
  
  fecha_inicial <- Rango_fecha[[1]]
  fecha_final <- Rango_fecha[[2]]
  
  output_urls <- GPM$generar_urls(fecha_inicial, fecha_final)
  urls <- output_urls$lista_urls
  urls_tif_filename <- output_urls$urls_tif_filename
  
  url_base <- '/srv/shiny-server/datosgrillados/GPM'
  GPM$crear_subdirectorios(url_base, 'PREC')
  GPM$crear_subdirectorios(url_base, 'PREC', 'diario')
  GPM$crear_subdirectorios(url_base, 'PREC', 'diario','PAIS')
  GPM$crear_subdirectorios(url_base, 'PREC', 'diario','tif')
  url_bolivia <- GPM$crear_subdirectorios(url_base, 'PREC', 'diario','PAIS', 'BOLIVIA')
  url_chile <- GPM$crear_subdirectorios(url_base, 'PREC', 'diario','PAIS','CHILE')
  url_colombia <- GPM$crear_subdirectorios(url_base, 'PREC', 'diario','PAIS','COLOMBIA')
  url_ecuador <- GPM$crear_subdirectorios(url_base, 'PREC', 'diario','PAIS','ECUADOR')
  url_venezuela <- GPM$crear_subdirectorios(url_base, 'PREC', 'diario','PAIS','VENEZUELA')
  url_peru <- GPM$crear_subdirectorios(url_base, 'PREC', 'diario','PAIS','PERU')
  directorio_temp <- GPM$crear_subdirectorios(url_base, 'PREC', 'diario', 'temp')
  directorio_tif <- GPM$crear_subdirectorios(url_base, 'PREC', 'diario', 'tif')
  
  name_outputFile <- paste0("GPM_",lugar, '_',parametro,'_',fecha_inicial,'_',fecha_final,'.tif')
  outputFile <- file.path(directorio_tif, name_outputFile)
  name_outputFile_zip <- paste0("GPM_",lugar, '_',parametro,'_',fecha_inicial,'_',fecha_final,'.zip')
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
  
  if (GPM$existe(outputFile_zip)){
    message("Ya se encuentra el archivo generado")
    # Detenemos la ejecución del script
    #return(NULL)
  } else{
    future({
      library(R.utils)
      library(curl)
      library(raster)
      library(parallel)
      library(stringr)
      library(ncdf4)
      library(doParallel)
      library(foreach)
      
      #for (i in seq_along(urls)) {
      foreach(
        i = seq_along(urls),
        .combine = c,
        .packages = c("raster", "curl", "stringr", "R.utils", "ncdf4", "stringr"),
        .export = c("urls", "urls_tif_filename", "url_pais", "coordenadas_pais", "directorio_temp")
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
              fecha <- substr(basename(url_tif_filename), 4, 11)
              setwd(directorio_temp)
              # Especifica la ruta al archivo .nc4
              ruta_archivo <- paste0("3B-DAY-E.MS.MRG.3IMERG.",fecha,"-S000000-E235959.V06.nc4")
              
              
              if (!existe(paste0(directorio_temp,'/',ruta_archivo))) {
                print('No se encuentra descargado')
                system(paste0("wget --no-check-certificate --load-cookies ~/.urs_cookies --save-cookies ~/.urs_cookies --keep-session-cookies --content-disposition ", url))
              }else{
                print('Ya se encuentra descargado')
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
              # Lee la variable precipitationCal
              variable_precipitacion <- ncvar_get(archivo_nc, "precipitationCal")
              # Cierra el archivo NetCDF
              nc_close(archivo_nc)
              # Define la extensión geográfica del raster
              lon_min <- min(lon)
              lon_max <- max(lon)
              lat_min <- min(lat)
              lat_max <- max(lat)
              # Define la extensión geográfica del raster
              extension_correcta <- raster::extent(lon_min, lon_max, lat_min, lat_max)
              # Crea el objeto raster con los datos y la resolución
              raster_precipitacion <- raster(variable_precipitacion)
              # Define la extensión geográfica del raster
              extent(raster_precipitacion) <- extension_correcta
              # Construir el nombre de salida en el formato deseado
              nombre_salida <- file.path(url_pais, paste0("gpm", fecha, ".tif"))
              # Definir la extensión (ajusta las coordenadas según tu necesidad)
              extension_pais <- raster::extent(coordenadas_pais)
              # Recortar el RasterStack
              r <- raster::crop(raster_precipitacion, extension_pais)
              r[r<0] <- NA
              r <- round(r, 1)
              print(r)
              # Escribir el objeto Raster como un archivo GeoTIFF
              writeRaster(r, filename = nombre_salida, format = "GTiff", overwrite = TRUE)
              print(paste("Archivo TIF guardado en:", url_tif_filename))
            },
            error = function(cond) {
              # Si se produce una excepción, se maneja aquí
              print(paste("Error:", cond$message))
              print(paste("Cambiando a otro link de descarga..."))
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
      out_txt <- GPM$generate_raster_info_txt(outputFile)
      # Crear el nombre del archivo zip con la misma basename pero extensión .zip
      zipfile <- sub("\\.tif$", ".zip", basename(outputFile))
      # Comprimir el archivo raster en un archivo zip
      zip(zipfile, files = c(basename(outputFile),out_txt))
      
      
    }, globals = list(
      urls = urls,
      urls_tif_filename=urls_tif_filename,
      url_base=url_base,
      url_pais=url_pais,
      directorio_temp = directorio_temp,
      outputFile = outputFile,
      coordenadas_list = coordenadas_list,
      GPM = GPM,
      fecha_inicial = fecha_inicial,
      fecha_final = fecha_final
      
    ))
    
    
  }
  
  return(outputFile_zip)
}



#GPM$Rango_fecha <- list(as.Date("2024-04-06"), as.Date("2024-04-06"))
#GPM$lugar <- 'chi'
#GPM$parametro <- 'PCP'

#GPM$downloads_transformar_a_tif(GPM$Rango_fecha, 'ecu', GPM$parametro)
#GPM$downloads_transformar_a_tif(GPM$Rango_fecha, 'per', GPM$parametro)
#GPM$downloads_transformar_a_tif(GPM$Rango_fecha, 'chi', GPM$parametro)
#GPM$downloads_transformar_a_tif(GPM$Rango_fecha, 'ven', GPM$parametro)
#GPM$downloads_transformar_a_tif(GPM$Rango_fecha, 'bol', GPM$parametro)
#GPM$downloads_transformar_a_tif(GPM$Rango_fecha, 'col', GPM$parametro)





