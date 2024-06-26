library(lubridate)
library(raster)
library(parallel)
library(doParallel)
library(httr)


CHIRPS <- new.env()


#Funcion para crear subdirectorios
CHIRPS$crear_subdirectorios <- function(base, ...) {
  subdirs <- c(base, list(...))
  ruta <- Reduce(file.path, subdirs)
  
  if (!dir.exists(ruta)) {
    dir.create(ruta, recursive = TRUE, showWarnings = FALSE)
    Sys.chmod(ruta, mode = "777", use_umask = FALSE)
    print(paste("La carpeta", ruta, "no existía y fue creada."))
  } else {
    #print(paste("La carpeta", ruta, "ya existe."))
  }
  return(ruta)
}

CHIRPS$procesar_archivo <- function(archivo, lonL, lonR, latB, latT) {
  
  # Cargar raster en un objeto de pila
  pila_tif <- raster::stack(archivo)
  
  # Definir las coordenadas para recortar
  cord <- raster::extent(lonL, lonR, latB, latT)
  
  # Realizar el recorte
  raster_crop <- raster::crop(pila_tif, cord)
  
  raster_crop[raster_crop<0] <- NA
  raster_crop <- round(raster_crop,1)
  return(raster_crop)
}

CHIRPS$download_from_link <- function(links,name.nc) {
  for (link in links) {
    print(paste("Trying to download from", link))
    tryCatch(
      {
        # Intentamos descargar el archivo desde el link actual
        #download.file(link, destfile = name.nc, mode = "wb")
        system(paste0("wget --no-check-certificate -O '", name.nc, "' '", link, "'"))
        return("Descarga completada exitosamente.")
      },
      error = function(cond) {
        # Si se produce una excepción, cambiamos al siguiente link
        print(paste("Error:", cond$message))
        print(paste("Cambiando a otro link de descarga..."))
      }
    )
  }
  # Si no se pudo descargar desde ninguno de los links, retornamos un mensaje de error.
  return("No se pudo descargar el archivo desde ninguno de los links disponibles.")
}

CHIRPS$write_raster_parallel <- function(raster, filename) {
  raster::writeRaster(raster, filename, format = "GTiff", overwrite = TRUE)
  #system(paste("chmod 7777", filename))
}

CHIRPS$downloads_transformar_a_tif <- function(Rango_fecha, lugar) {

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
  
  
  
  fecha_inicial <- Rango_fecha[[1]]
  fecha_final <- Rango_fecha[[2]]

  MainDir='/opt/shiny-server/samples/sample-apps/app_bases'
  DirPath <- paste0(MainDir,'/','CHIRPS')
  dir.create(path = paste0(MainDir,'/','CHIRPS'),showWarnings = F)
  system(paste("chmod 7777", DirPath))
  
  DirPath_daily <- paste0(MainDir,'/','CHIRPS','/diario')
  dir.create(path = paste0(MainDir,'/','CHIRPS','/diario'),showWarnings = F)
  Sys.chmod(DirPath_daily, mode = "7777", use_umask = FALSE)
  
  DirPath_daily_pais<- paste0(MainDir,'/','CHIRPS','/diario/PAIS')
  dir.create(path = paste0(MainDir,'/','CHIRPS','/diario/PAIS'),showWarnings = F)
  Sys.chmod(DirPath_daily_pais, mode = "7777", use_umask = FALSE)
  
  temp_dir <- paste0(DirPath_daily_pais,'/temp')
  dir.create(path = temp_dir,showWarnings = F)
  Sys.chmod(temp_dir, mode = "7777", use_umask = FALSE)
  
  url_base <- '/opt/shiny-server/samples/sample-apps/app_bases/CHIRPS'
  url_bolivia <- CHIRPS$crear_subdirectorios(url_base, 'diario','PAIS', 'BOLIVIA')
  url_chile <- CHIRPS$crear_subdirectorios(url_base, 'diario','PAIS','CHILE')
  url_colombia <- CHIRPS$crear_subdirectorios(url_base, 'diario','PAIS','COLOMBIA')
  url_ecuador <- CHIRPS$crear_subdirectorios(url_base, 'diario','PAIS','ECUADOR')
  url_venezuela <- CHIRPS$crear_subdirectorios(url_base, 'diario','PAIS','VENEZUELA')
  url_peru <- CHIRPS$crear_subdirectorios(url_base, 'diario','PAIS','PERU')
  
  print('Cargando funciones completo')
  cores=4
  coordenadas_list <- list(
    ven = c(lonL = -73.378, lonR = -59.8035, latT = 12.2011, latB = 0.6499 ),
    col = c(lonL = -79.3667, lonR = -66.8694, latT = 12.4583, latB = -4.2271),
    ecu = c(lonL = -81.0833, lonR = -75.1867, latT = 1.6742, latB = -5.014 ),
    per = c(lonL = -81.3269, lonR = -68.6651, latT = 0.012, latB = -18.3496),
    bol = c(lonL = -69.6409, lonR = -57.453, latT = -9.6805, latB = -22.8969),
    chi = c(lonL = -75.6445, lonR = -66.4173, latT = -17.5065, latB = -50)
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

  coordenadas_aux <- coordenadas_pais
  setwd(url_pais)
  archivos <- list.files()

  fecha_data_fool <- gsub(archivos,pattern = 'chirps-v2.0.',replacement = '')
  fecha_data <- substr(fecha_data_fool,start = 1,stop = 10)
  fechas_necesarias <-  seq(fecha_inicial, fecha_final, by = "day")

  c=0
  c1=0
  CHIRPS$name_file_faltantes <- NULL
  CHIRPS$name_file_existentes <- NULL
  CHIRPS$url_faltantes <- NULL
  
  #k=2
  for(k in 1:length(fechas_necesarias)){
    if(lugar=='chi'){
      name_file_tiff1 <- paste0(url_pais, "/chirps-v2.0.", fechas_necesarias[k], "_", coordenadas_aux[1], "_", coordenadas_aux[2], "_",
                                '-56.4966', "_", coordenadas_aux[3], ".tif")
      name_file_tiff1_prel <- paste0(url_pais, "/chirps-v2.0.", fechas_necesarias[k],'_','preliminar',"_", coordenadas_aux[1], "_", coordenadas_aux[2], "_",
                                     '-56.4966', "_", coordenadas_aux[3], ".tif")
    }else{
      name_file_tiff1 <- paste0(url_pais, "/chirps-v2.0.", fechas_necesarias[k], "_", coordenadas_aux[1], "_", coordenadas_aux[2], "_",
                                coordenadas_aux[4], "_", coordenadas_aux[3], ".tif")
      name_file_tiff1_prel <- paste0(url_pais, "/chirps-v2.0.", fechas_necesarias[k],'_','preliminar',"_", coordenadas_aux[1], "_", coordenadas_aux[2], "_",
                                     coordenadas_aux[4], "_", coordenadas_aux[3], ".tif")
    }
    
    
    if(existe(name_file_tiff1)==T){
      c1=c1+1
      CHIRPS$name_file_existentes[c1] <- name_file_tiff1
      print(paste('El archivo:',name_file_tiff1,'ya existe'))
    }else{
      
      c=c+1
      Year_st <- substr(fechas_necesarias[k],1,4)
      Month_st <- substr(fechas_necesarias[k],6,7)
      Day_st <- substr(fechas_necesarias[k],9,10)
      st_date <- fechas_necesarias[k]
      link_st_date<-paste0('https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_daily/tifs/p05/',Year_st,'/','chirps-v2.0.',Year_st,'.',Month_st,'.',Day_st,'.tif.gz')
      link_st_date2 <- paste0('https://data.chc.ucsb.edu/products/CHIRPS-2.0/prelim/global_daily/tifs/p05/',Year_st,'/','chirps-v2.0.',Year_st,'.',Month_st,'.',Day_st,'.tif.gz')
      #Preliminar link
      # URL de la página web
      url_prelim <-  paste0('https://data.chc.ucsb.edu/products/CHIRPS-2.0/prelim/global_daily/tifs/p05/',Year_st,'/')
      # Descargar el contenido de la página web
      response <- GET(url_prelim, config(ssl_verifypeer = FALSE))
      print(paste0("Descargando:",st_date ))
      # Verificar si la descarga fue exitosa (código de estado 200)
      if (status_code(response) == 200) {
        
        # Leer el contenido de la página
        contenido <- content(response, "text")
        
        # Dividir el contenido en líneas
        lineas <- strsplit(contenido, "\n")[[1]]
        
        #Buscar primera linea que contiene chirps
        
        #primera_linea_chirps <- head(grep("chirps-v2.0", lineas, value = TRUE), n = 1)
        
        # Buscar la última línea que contiene "chirps-v2.0"

        ultima_linea_chirps <- tail(grep("chirps-v2.0", lineas, value = TRUE), n = 1)
        posicion_chirps <- regexpr("chirps-v2.0.", ultima_linea_chirps)
        fecha_end_html_fool1 <- substr(ultima_linea_chirps,start =posicion_chirps[1]+12,stop = posicion_chirps[1]+21 )

        fecha_end_html <- as.Date(gsub(fecha_end_html_fool1,pattern = '[.]',replacement = '-'))
  
        # Imprimir la última línea que contiene "chirps-v2.0"
      } else {
        #cat("Error al descargar la página..")
      }
  
      # Real Link
      # URL de la página web
      url_real<-  paste0('https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_daily/tifs/p05/',Year_st,'/')
      # Descargar el contenido de la página web
      response <- GET(url_real, config(ssl_verifypeer = FALSE))
      # Verificar si la descarga fue exitosa (código de estado 200)
      if (status_code(response) == 200) {
        # Leer el contenido de la página
        contenido <- content(response, "text")
        # Dividir el contenido en líneas
        lineas <- strsplit(contenido, "\n")[[1]]
        
        ultima_linea_chirps <- tail(grep("chirps-v2.0", lineas, value = TRUE), n = 1)
        posicion_chirps <- regexpr("chirps-v2.0.", ultima_linea_chirps)
        fecha_end_html_fool1 <- substr(ultima_linea_chirps,start =posicion_chirps[1]+12,stop = posicion_chirps[1]+21 )
        
        fecha_end_html_real <- as.Date(gsub(fecha_end_html_fool1,pattern = '[.]',replacement = '-'))
    
        num_flag <- length(fecha_end_html_real)
        
        if(num_flag==0){
          
          url_real<-  paste0('https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_daily/tifs/p05/',as.numeric(Year_st)-1,'/')
   
          response <- GET(url_real, config(ssl_verifypeer = FALSE))
          
          contenido <- content(response, "text")
          
          # Dividir el contenido en líneas
          lineas <- strsplit(contenido, "\n")[[1]]
          
          ultima_linea_chirps <- tail(grep("chirps-v2.0", lineas, value = TRUE), n = 1)
          posicion_chirps <- regexpr("chirps-v2.0.", ultima_linea_chirps)
          fecha_end_html_fool1 <- substr(ultima_linea_chirps,start =posicion_chirps[1]+12,stop = posicion_chirps[1]+21 )
          
          fecha_end_html_real <- as.Date(gsub(fecha_end_html_fool1,pattern = '[.]',replacement = '-'))
        }else{
          
          fecha_end_html_real <- as.Date(gsub(fecha_end_html_fool1,pattern = '[.]',replacement = '-'))
        }
        
      } else {
        cat("Error al descargar la páginaa.")
      }
      #fecha end html real corresponde al real y fecha end html al preliminar
      
      #Usando link si es Preliminar u Real
      #if(st_date>fecha_end_html_real&st_date<=fecha_end_html){
      if(st_date>fecha_end_html_real){
        links_st <- c(link_st_date2)
        name_file_tiff1 <- paste0(url_pais, "/chirps-v2.0.preliminar_", st_date, "_", coordenadas_aux[1], "_", coordenadas_aux[2], "_",
                                  coordenadas_aux[4], "_", coordenadas_aux[3], ".tif")
        print(paste('Se descargará archivo madre',name_file_tiff1,'pero es preliminar'))
      }else{
        links_st <- c(link_st_date)
        name_file_tiff1 <- paste0(url_pais, "/chirps-v2.0.", st_date, "_",coordenadas_aux[1], "_", coordenadas_aux[2], "_",
                                  coordenadas_aux[4], "_", coordenadas_aux[3], ".tif")
      }
      
      
      #if (st_date == fecha_end_html+1) {
      #  print(paste("Solo hay datos hasta la fecha:", as.Date(fecha_end_html), "y son preliminares a partir de:",fecha_end_html_real+1))
      #  break  # Salir del bucle si se encuentra la fecha
      #}
      CHIRPS$name_file_faltantes[c] <- name_file_tiff1
      CHIRPS$url_faltantes[c] <- links_st
      
      
      
    }
    
  }
  # CHIRPS$name_file_faltantes <- name_file_faltantes 
  
  #Si todos los archivo existen
  if(length(CHIRPS$name_file_faltantes)==0&length(CHIRPS$url_faltantes)==0){
    print('Los archivos ya existen no es necesario descarga nuevos')
    
    
  }else{
    CHIRPS$name_file_faltantes2 <- paste0(CHIRPS$name_file_faltantes,'.gz')
    
    #Descargando datos en paralelo
    num_cores <- 2
    # Calcular el número de archivos por núcleo
    # Iniciar el clúster para ejecutar en paralelo
    cl <- makeCluster(num_cores)
    # Exporta los objetos necesarios al clúster
    clusterExport(cl, "CHIRPS") 
    
    #i=1
    # Aplica la función en paralelo para escribir los rasters en archivos individuales
    parLapply(cl, 1:length(CHIRPS$url_faltantes), function(i) {
      CHIRPS$download_from_link(links =  CHIRPS$url_faltantes[i], name.nc = CHIRPS$name_file_faltantes2[i])
    })
    
    # Detener el clúster
    stopCluster(cl)
    
    print('ya se descargaron los archivos faltantes')
    #------------------------------------------------------------
    #Extrayendo archivos
    for(k in 1:length( CHIRPS$name_file_faltantes2)){
      R.utils::gunzip( CHIRPS$name_file_faltantes2[k],overwrite = TRUE)
    }
    #-------------------------------------------------------
    #Cortando archivos 
    num_cores <- 2
    
    # Calcular el n?mero de archivos por n?cleo
    num_archivos <- length( CHIRPS$name_file_faltantes)
    archivos_por_core <- ceiling(num_archivos / num_cores)
    
    # Dividir la lista de nombres de archivos en partes para cada n?cleo
    archivos_por_core <- split( CHIRPS$name_file_faltantes, rep(1:num_cores, each = archivos_por_core, length.out = num_archivos))
    
    # Iniciar el cl?ster para ejecutar en paralelo
    cl <- makeCluster(num_cores)
    
    # Aplicar la funci?n en paralelo a cada parte de la lista de nombres de archivos
    raster_crop_total <- parLapply(cl, archivos_por_core,  CHIRPS$procesar_archivo,lonL = coordenadas_aux[1], lonR = coordenadas_aux[2], 
                                   latB = coordenadas_aux[4], latT = coordenadas_aux[3])
    
    
    # Detener el cl?ster
    stopCluster(cl)
    #---------------------------------------------------------------------
    #Grabando raster
    CHIRPS$ec_prec_total <- raster::stack(raster_crop_total)
    #extent(ec_prec_total) <- c(-180,180,-90,90)
    num_date <- length(names( CHIRPS$ec_prec_total))
    # Configuración de paralelización para cortar datos
    num_cores <- 2 # Utiliza el número de núcleos disponibles
    cl <- makeCluster(num_cores) # Crea el clúster de núcleos
    # Exporta los objetos necesarios al clúster
    clusterExport(cl, "CHIRPS") 
    
    
    # Aplica la función en paralelo para escribir los rasters en archivos individuales
    parLapply(cl, 1:nlayers( CHIRPS$ec_prec_total), function(i) {
      CHIRPS$write_raster_parallel(raster =  CHIRPS$ec_prec_total[[i]], filename =  CHIRPS$name_file_faltantes[i])
    })
    
    # Cierra el clúster de núcleos
    stopCluster(cl)
    #-----------------------------------------------------------------------------
    
  }
  
  
  #Uniendo los archivo que si están con los que no
  todas_necesarias <- c( CHIRPS$name_file_existentes, CHIRPS$name_file_faltantes)
  
  if(length(Rango_fecha)==2){
    file_selected <- todas_necesarias
    fecha_inicial_str <- format(fecha_inicial, format='%Y-%m-%d')
    fecha_final_str <- format(fecha_final, format='%Y-%m-%d')
    name_zip_file2 <- paste0(temp_dir,'/CHIRPS-V2.0.',fecha_inicial_str,'_',fecha_final_str,'_',lugar,'.tif')
    archivo_nombre <- paste0('CHIRPS-V2.0.',fecha_inicial_str,'_',fecha_final_str,'_',lugar,'.tif')
  }else{
    file_selected <- todas_necesarias
    name_zip_file2 <- paste0(temp_dir,'/CHIRPS-V2.0.',fecha_inicial_str,'_',lugar,'.tif')
    archivo_nombre <- paste0('CHIRPS-V2.0.',fecha_inicial_str,'_',lugar,'.tif')
  }
  print('Generando nombres.... completo')
  gc()
  
  num_cores <- 2
  
  # Calcular el n?mero de archivos por n?cleo
  num_archivos <- length(file_selected)
  archivos_por_core <- ceiling(num_archivos / num_cores)
  
  # Dividir la lista de nombres de archivos en partes para cada n?cleo
  archivos_por_core <- split(file_selected, rep(1:num_cores, each = archivos_por_core, length.out = num_archivos))
  
  # Iniciar el cl?ster para ejecutar en paralelo
  cl <- makeCluster(num_cores)
  
  # Aplicar la funci?n en paralelo a cada parte de la lista de nombres de archivos
  raster_crop_total <- parLapply(cl, archivos_por_core,  CHIRPS$procesar_archivo,lonL = coordenadas_aux[1], lonR = coordenadas_aux[2], 
                                 latB = coordenadas_aux[4], latT = coordenadas_aux[3])

  
  # Detener el cl?ster
  stopCluster(cl)
  print('Corriendo en paralelo.... completo')
  
  FINAL_RASTER <- raster::stack(raster_crop_total)
  
  gc()
  
  raster::writeRaster(FINAL_RASTER,name_zip_file2,overwrite=T)
  print('Exportando resultados.... completo')
  
  setwd(dirname(name_zip_file2))
  # Crear el nombre del archivo zip con la misma base pero extensión .zip
  zipfile <- sub("\\.tif$", ".zip", basename(name_zip_file2))
  
  # Comprimir el archivo raster en un archivo zip
  zip(zipfile, files = basename(name_zip_file2))

  
  return(zipfile)
  
}

