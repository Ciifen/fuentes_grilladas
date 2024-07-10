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


NCEP_NCAR <- new.env()


NCEP_NCAR$generate_raster_info_txt <- function(raster_path) {
  closeAllConnections()
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
  closeAllConnections()
  return(basename(name_file))
}




NCEP_NCAR$obtener_nombre_variable <- function(parametro) {
  switch(parametro,
         'UWND' = 'uwnd',
         'VWND' = 'vwnd',
         'HGT200' = 'hgt',
         'HGT500' = 'hgt',
         'HGT850' = 'hgt',
         'SLP' = 'slp',
         stop("Parámetro no válido")
  )
}

NCEP_NCAR$generar_urls <- function(Rango_fecha, parametro){
  
  fecha_inicial <- Rango_fecha[[1]]
  fecha_final <- Rango_fecha[[2]]
  
  
  seqfechas <- seq(fecha_inicial, fecha_final, by='year')
  
  # Listas para almacenar las URLs
  urls <- list()
  urls_tif_filename <- list()  # Nueva lista para las URLs de archivos TIF
  
  # Iterar sobre el rango de fechas y construir las URLs
  rango <- seq.Date(fecha_inicial, fecha_final, by = "year")
  
  #definir url de descarga
  if (parametro == 'UWND' | parametro =='VWND' | parametro == 'HGT200' | parametro =='HGT500' | parametro =='HGT850' ){
    urlbase <- "https://psl.noaa.gov/thredds/fileServer/Datasets/ncep.reanalysis/Dailies/pressure/"
  }else if (parametro == 'SLP'){
    urlbase <- "https://psl.noaa.gov/thredds/fileServer/Datasets/ncep.reanalysis/Dailies/surface/"
  }
  
  if (parametro == 'UWND'){
    name_variable <- 'uwnd'
  }
  else if (parametro =='VWND'){
    name_variable <- 'vwnd'
  }
  else if (parametro =='HGT200'){
    name_variable <- 'hgt'
  }
  else if (parametro =='HGT500'){
    name_variable <- 'hgt'
  }
  else if (parametro =='HGT850'){
    name_variable <- 'hgt'
    
  }else if (parametro == 'SLP'){
    name_variable <- 'slp'
  }
  
  for (fecha in as.character(rango)) {
    anio_curr <- format(as.Date(fecha), "%Y")
    filename <- paste0(name_variable,'.',anio_curr ,".nc")
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
NCEP_NCAR$crear_subdirectorios <- function(base, ...) {
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


NCEP_NCAR$downloads_transformar_a_tif <- function(Rango_fecha, lugar, parametro) {
  
  # Definir el número de núcleos a utilizar
  #num_cores <- detectCores()
  num_cores <- 2
  
  # Registrar el backend paralelo con el número de núcleos especificado
  registerDoParallel(num_cores)

  
  # Lista con urls
  output_urls <- NCEP_NCAR$generar_urls(Rango_fecha, parametro)
  urls <- output_urls$lista_urls
  urls_tif_filename <- output_urls$urls_tif_filename
  
  url_base <- '/opt/shiny-server/samples/sample-apps/app_bases/NCEP_NCAR'
  
  NCEP_NCAR$crear_subdirectorios(url_base, parametro)
  NCEP_NCAR$crear_subdirectorios(url_base, parametro, 'diario')
  NCEP_NCAR$crear_subdirectorios(url_base, parametro, 'diario','PAIS')
  
  url_bolivia <- NCEP_NCAR$crear_subdirectorios(url_base, parametro, 'diario','PAIS', 'BOLIVIA')
  url_chile <- NCEP_NCAR$crear_subdirectorios(url_base, parametro, 'diario','PAIS','CHILE')
  url_colombia <- NCEP_NCAR$crear_subdirectorios(url_base, parametro, 'diario','PAIS','COLOMBIA')
  url_ecuador <- NCEP_NCAR$crear_subdirectorios(url_base, parametro, 'diario','PAIS','ECUADOR')
  url_venezuela <- NCEP_NCAR$crear_subdirectorios(url_base, parametro, 'diario','PAIS','VENEZUELA')
  url_peru <- NCEP_NCAR$crear_subdirectorios(url_base, parametro, 'diario','PAIS','PERU')
  directorio_temp <- NCEP_NCAR$crear_subdirectorios(url_base, parametro, 'diario', 'temp')
  directorio_tif <- NCEP_NCAR$crear_subdirectorios(url_base, parametro, 'diario', 'tif')
  
  name_outputFile <- paste0("NCEP_",lugar, '_',parametro,'_',output_urls$fecha_inicial,'_',output_urls$fecha_final,'.tif')
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
    message(paste0("Ya se encuentra el archivo generado"), outputFile)
    # Detenemos la ejecución del script
    return(NULL)
    
  }else{
    
    
    #Descargar archivos nc
    for (i in seq_along(urls)) {
      url <- urls[i][[1]]
      print(paste("Processing URL:", url))
      file_temp <- paste0(directorio_temp, '/', basename(url))
      setwd(directorio_temp)
      if (file.exists(file_temp)) {
        if (file.info(file_temp)$size>0) {
          if (as.Date(file.info(file_temp)$ctime) != Sys.Date()){
            file.remove(file_temp)
            system(paste0("wget --no-check-certificate --load-cookies ~/.urs_cookies --save-cookies ~/.urs_cookies --keep-session-cookies --content-disposition ", url))
          }
        }else{
          file.remove(file_temp)
          system(paste0("wget --no-check-certificate --load-cookies ~/.urs_cookies --save-cookies ~/.urs_cookies --keep-session-cookies --content-disposition ", url))
        }
        
      }else{
        system(paste0("wget --no-check-certificate --load-cookies ~/.urs_cookies --save-cookies ~/.urs_cookies --keep-session-cookies --content-disposition ", url))
      }
      
    }
    
    #Procesar archivo nc anual a tif diario
    #for (i in seq_along(urls_tif_filename)) {
      
      foreach(
        i = seq_along(urls_tif_filename),
        .combine = c,
        .packages = c("raster", "curl", "stringr", "R.utils", "ncdf4", "RCurl", "stringr"),
        .export = c("urls", "urls_tif_filename", "url_pais", "coordenadas_pais", "directorio_temp")
      ) %dopar% {
      
      url_tif_filename <-urls_tif_filename[i][[1]]
      url_tif_pais <- file.path(url_pais, url_tif_filename)
   
      
      #Validar si el archivo existe y esta creado correctamente
      existe <- file.exists(url_tif_pais)
      
      #La funcion valida si el archivo existe y es mayor a 0 el tama;o
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
            nombre_variable <- NCEP_NCAR$obtener_nombre_variable(parametro)
            #Extraer la fecha de la ruta url_tif_filename
            fecha <- substr(url_tif_filename, nchar(parametro)+1, nchar(parametro)+8)
            year <- substr(url_tif_filename, nchar(parametro)+1, nchar(parametro)+4)
            archivo_temporal <- paste0(directorio_temp,"/",nombre_variable,'.', year,".nc")
            
          
            if (parametro == 'UWND' | parametro =='VWND'){
              nc <- nc_open(archivo_temporal)  # Abrir el archivo NetCDF
              # Abrir el archivo NetCDF como un objeto RasterBrick
              lvl <- match(850,nc$dim$level$vals)
              nc_close(nc)
              brick <- brick(archivo_temporal, level=lvl)
              #extent(r) <- extent(0, 360, -90, 90)
              #projection(brick) <- CRS("+proj=longlat +datum=WGS84")
              r <- raster::rotate(brick)
              raster_stack <- stack(r)
              
            }
            
            else if (parametro =='HGT200'){
              nc <- nc_open(archivo_temporal)  # Abrir el archivo NetCDF
              # Abrir el archivo NetCDF como un objeto RasterBrick
              lvl <- match(200,nc$dim$level$vals)
              nc_close(nc)
              brick <- brick(archivo_temporal, level=lvl)
              #projection(brick) <- CRS("+proj=longlat +datum=WGS84")
              #extent(r) <- extent(0, 360, -90, 90)
              r <- stack(brick)
              # Aplicar rotación
              raster_stack <- raster::rotate(r)
              
            }
            else if (parametro =='HGT500'){
              nc <- nc_open(archivo_temporal)  # Abrir el archivo NetCDF
              # Abrir el archivo NetCDF como un objeto RasterBrick
              lvl <- match(500,nc$dim$level$vals)
              nc_close(nc)
              brick <- brick(archivo_temporal, level=lvl)
              r <- stack(brick)
              # Aplicar rotación
              raster_stack <- raster::rotate(r)
              
            }
            else if (parametro =='HGT850'){
              nc <- nc_open(archivo_temporal)  # Abrir el archivo NetCDF
              # Abrir el archivo NetCDF como un objeto RasterBrick
              lvl <- match(850,nc$dim$level$vals)
              nc_close(nc)
              brick <- brick(archivo_temporal, level=lvl)
              #projection(brick) <- CRS("+proj=longlat +datum=WGS84")
              r <- stack(brick)
              # Aplicar rotación
              raster_stack <- raster::rotate(r)
              
            }else if (parametro == 'SLP'){
              # Carga el raster stack
              raster_stack <- raster::stack(archivo_temporal, varname='slp')
              # Asigna la extensión correcta al raster stack
              # Aplicar rotación
              raster_stack <- raster::rotate(raster_stack)
            }

            # Convertir la fecha a formato Date
            fecha_seleccionada <- as.Date(fecha, format = "%Y%m%d")
            
            # Extraer las fechas de todo el archivo nc
            fechas <- as.Date(names(raster_stack), format = "X%Y.%m.%d")
            
            # Encontrar el índice correspondiente a la fecha seleccionada
            indice_fecha <- which(fechas == fecha_seleccionada)
            
            # Seleccionar la capa raster correspondiente a la fecha seleccionada
            r <- raster_stack[[indice_fecha]]
            r[r<0] <- NA
            r <- round(r, 1)
            
            
            #Recortar raster para cada pais
            for (num in 1:length(coordenadas_list)){
              raster_recortado <- raster::crop(r,  raster::extent(coordenadas_list[[num]]))
              file <- paste0(parametro, fecha,'.tif')
              output <- file.path(url_base, parametro, 'diario','PAIS',names(coordenadas_list)[num],file )
              
              
              if (!existe(output)){
                raster::writeRaster(raster_recortado, output, format = "GTiff", overwrite = TRUE)
                print(paste("Archivo TIF guardado en:", output))
              }
              
              
              
            }
            
            # Elimina variables intermedias y ejecuta garbage collection
            rm(brick, r, raster_stack)
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
  out_txt <- NCEP_NCAR$generate_raster_info_txt(outputFile)
  # Crear el nombre del archivo zip con la misma basename pero extensión .zip
  zipfile <- sub("\\.tif$", ".zip", basename(outputFile))
  # Comprimir el archivo raster en un archivo zip
  zip(zipfile, files = c(basename(outputFile), out_txt))
  return(zipfile)
  
}


