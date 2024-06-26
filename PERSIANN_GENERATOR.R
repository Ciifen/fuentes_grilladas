library(R.utils)
library(curl)
library(raster)
library(parallel)
library(stringr)
library(RCurl)
library(doParallel)
library(foreach)

PERSIANN <- new.env()

#Funcion para crear subdirectorios
PERSIANN$crear_subdirectorios <- function(base, ...) {
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

PERSIANN$generar_urls <- function(Rango_fecha){
  
  fecha_inicial <- Rango_fecha[[1]]
  fecha_final <- Rango_fecha[[2]]
  
  # URLs de las páginas
  urlbase_ccs <- "ftp://persiann.eng.uci.edu/CHRSdata/PERSIANN-CCS/daily/"
  
  # Listas para almacenar las URLs
  urls <- list()
  urls_tif_filename <- list()  # Nueva lista para las URLs de archivos TIF
  
  # Iterar sobre el rango de fechas y construir las URLs
  rango <- seq.Date(fecha_inicial, fecha_final, by = "day")
  for (fecha in as.character(rango)) {
    yyddd <- format(as.Date(fecha), "%y%j")
    filename <- paste0("rgccs1d", yyddd, ".bin.gz")
    url <- paste0(urlbase_ccs, filename)
    urls <- c(urls, url)
    
    # Construir el nombre del archivo TIF
    YMD <- format(as.Date(fecha), "%Y%m%d")
    filename_tif <- paste0("persiann", YMD, ".tif")
    url_tif_filename <- paste0(filename_tif)
    urls_tif_filename <- c(urls_tif_filename, url_tif_filename)
  }
  return(list(lista_urls = urls, urls_tif_filename = urls_tif_filename, fecha_inicial=fecha_inicial, fecha_final=fecha_final))
}



PERSIANN$downloads_transformar_a_tif <- function(Rango_fecha, lugar) {
  # Lista con urls
  output_urls <- PERSIANN$generar_urls(Rango_fecha)
  urls <- output_urls$lista_urls
  urls_tif_filename <- output_urls$urls_tif_filename
  
  url_base <- '/opt/shiny-server/samples/sample-apps/app_bases/PERSIANN'
  PERSIANN$crear_subdirectorios(url_base, 'PREC')
  PERSIANN$crear_subdirectorios(url_base, 'PREC', 'diario')
  PERSIANN$crear_subdirectorios(url_base, 'PREC', 'diario','PAIS')
  url_bolivia <- PERSIANN$crear_subdirectorios(url_base, 'PREC', 'diario','PAIS', 'BOLIVIA')
  url_chile <- PERSIANN$crear_subdirectorios(url_base, 'PREC', 'diario','PAIS','CHILE')
  url_colombia <- PERSIANN$crear_subdirectorios(url_base, 'PREC', 'diario','PAIS','COLOMBIA')
  url_ecuador <- PERSIANN$crear_subdirectorios(url_base, 'PREC', 'diario','PAIS','ECUADOR')
  url_venezuela <- PERSIANN$crear_subdirectorios(url_base, 'PREC', 'diario','PAIS','VENEZUELA')
  url_peru <- PERSIANN$crear_subdirectorios(url_base, 'PREC', 'diario','PAIS','PERU')
  directorio_temp <- PERSIANN$crear_subdirectorios(url_base, 'PREC', 'diario', 'temp')
  directorio_tif <- PERSIANN$crear_subdirectorios(url_base, 'PREC', 'diario', 'tif')
  
  name_outputFile <- paste0(lugar,'_',output_urls$fecha_inicial,'_',output_urls$fecha_final,'.tif')
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
    
    for (i in seq_along(urls)) {
      url <- urls[i][[1]]
      url_tif_filename <-paste0(url_pais,'/' ,urls_tif_filename[i][[1]]) 
      print(paste("Processing URL:", url, ' tif:', url_tif_filename))
      
      
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
      
      #process_url <- function(url, url_tif_filename) {
      if (!existe(url_tif_filename)) {
        # El archivo .tif no existe, descárgalo y procesa
        tryCatch(
          {
            base <- file.path(directorio_temp, paste0("base_", gsub("\\.tif$", "",basename(url_tif_filename)), ".grd"))
            # generar mascara 0.04
            r <- raster(xmn=0, xmx=360, ymn=-60, ymx=60, nrow=3000, ncol=9000, crs="+proj=longlat +datum=WGS84")
            values(r) <- 1
            r <- writeRaster(r, base, datatype="FLT4S", overwrite=TRUE)
            x <- readLines(base)
            x[grep("byteorder", x)] <- "byteorder=big"
            x[grep("nodatavalue", x)] <- "nodatavalue=-9999"
            writeLines(x, base)
            archivo_temporal <- file.path(directorio_temp, basename(url))
            curl_download(url, archivo_temporal)
            R.utils::gunzip(archivo_temporal)
            f <- gsub("\\.gz$", "", archivo_temporal)
            file.rename(f, extension(f, "gri"))
            fg <- extension(f, "grd") 
            file.copy(base, fg)
            # Limitar los valores a un decimal
            r <- raster(fg) * 1
            r <- rotate(r)
            # Extraer la fecha del nombre del archivo
            yyddd <- str_extract(basename(url), "(\\d{5})")
            fecha <- as.Date(yyddd, format = "%y%j")
            # Construir el nombre de salida en el formato deseado
            nombre_salida <- file.path(
              url_pais,
              paste0("persiann", format(fecha, "%Y%m%d"), ".tif")
            )
            # Definir la extensión (ajusta las coordenadas según tu necesidad)
            extension_pais <- extent(coordenadas_pais)
            
            
            
            # Recortar el RasterStack
            r <- crop(r, extension_pais)
            
            writeRaster(r, nombre_salida, format = "GTiff", overwrite = TRUE)
            # Elimina el archivo temporal .gri
            #file.remove(extension(f, "gri"))
            #file.remove(extension(fg, "grd"))
            #return("Descarga completada exitosamente.")
            print(paste("Archivo TIF guardado en:", nombre_salida))
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
      
      #}
    }
    
    
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


#PERSIANN$Rango_fecha <- list(as.Date("2023-09-02"), as.Date("2023-09-04"))
#PERSIANN$lugar <- 'ven'

#PERSIANN$downloads_transformar_a_tif(PERSIANN$Rango_fecha, 'ecu')
#PERSIANN$downloads_transformar_a_tif(PERSIANN$Rango_fecha, 'per')
#PERSIANN$downloads_transformar_a_tif(PERSIANN$Rango_fecha, 'chi')
#PERSIANN$downloads_transformar_a_tif(PERSIANN$Rango_fecha, 'ven')
#PERSIANN$downloads_transformar_a_tif(PERSIANN$Rango_fecha, 'bol')
#PERSIANN$downloads_transformar_a_tif(PERSIANN$Rango_fecha, 'col')


