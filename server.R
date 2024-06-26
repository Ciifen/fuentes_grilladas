library(shinyvalidate)
library(shinyFiles)




server <- function(input, output, session){
  
  colnames(data) <- c("Fuente", "Tipo","Periodo", "Delay","Parametro", "Resoluci\u00F3n espacial","Resoluci\u00F3n temporal", "Acceso")
  
  output$tabla <- renderDataTable({
    datatable(
      data,
      options = list(
        pageLength = 30,
        #buttons = c('copy', 'csv', 'excel')
        dom = 'lBfrtip'
        
      ),
      rownames = FALSE,
      #extensions = 'Buttons',
      escape = FALSE
    )
  })
  
  #Validar campo fecha
  iv <- InputValidator$new()
  iv$add_rule("date", sv_required())
  iv$enable()
  
  output$downloadData <- downloadHandler(
    
    filename <- function() {
      req(input$category)
      req(input$subcategory)
      req(input$pais)
      req(input$date)
      
      category <- input$category
      subcategory <- input$subcategory
      lugar <- lugar_map[[input$pais]]
      
      parametro <- parameter_map[[category]][[subcategory]]
      fecha_inicial <- format(input$date[1], "%Y%m%d")
      fecha_final <- format(input$date[2], "%Y%m%d")
      #Name output
      #print(paste0(lugar, '_',parametro,'_',fecha_inicial,'_',fecha_final,'.zip'))
      paste0(lugar, '_',parametro,'_',fecha_inicial,'_',fecha_final,'.zip')
      
    },
    
    content <- function(file) {
      req(input$category)
      req(input$subcategory)
      req(input$pais)
      req(input$date)
      
      category <- input$category
      subcategory <- input$subcategory
      lugar <- lugar_map[[input$pais]]
      parametro <- parameter_map[[category]][[subcategory]]
      rango_fecha <- list(as.Date(input$date[1]), as.Date(input$date[2]))
      
      showNotification("La descarga está en progreso, por favor espere...", type = "message", duration = NULL)
      
      
      file_to_copy <- tryCatch({
        if (category == "CHIRPS") {
          file_to_copy <- CHIRPS$downloads_transformar_a_tif(lugar = lugar, Rango_fecha = rango_fecha)
        } else if (category == "NASA- POWER") {
          file_to_copy <- NASA_POWER$downloads_transformar_a_tif(Rango_fecha = rango_fecha, lugar = lugar, parametro = parametro)
        } else if (category == "CPC") {
          file_to_copy <- CPC$downloads_transformar_a_tif(Rango_fecha = rango_fecha, lugar = lugar, parametro = parametro)
        }  else if (category == "ERA5") {
          file_to_copy <- ERA5$downloads_transformar_a_tif(Rango_fecha = rango_fecha, pais_abreviado = lugar, parametro = parametro)
        }  else if (category == "GPCC-ALL PRODUCTS") {
          file_to_copy <- GPCC$downloads_transformar_a_tif(Rango_fecha = rango_fecha, lugar = lugar, parametro = parametro)
        }  else if (category == "NCAR/UCAR Reanalysis") {
          file_to_copy <- NCEP_NCAR$downloads_transformar_a_tif(Rango_fecha = rango_fecha, lugar = lugar, parametro = parametro)
        }  else if (category == "PERSIANN-CCS") {
          file_to_copy <- PERSIANN$downloads_transformar_a_tif(Rango_fecha = rango_fecha, lugar = lugar)
        }  else if (category == "GPM (GPM_3IMERGDE)") {
          file_to_copy <- GPM$downloads_transformar_a_tif(Rango_fecha = rango_fecha, lugar = lugar, parametro = parametro)
        } else {
          NULL
        }
        
        if (is.null(file_to_copy) || !file.exists(file_to_copy)) {
          showNotification("El archivo no está disponible en las fechas solicitadas. Por favor ignore el archivo html", type = "error")
          return(NULL)
        }
        file.copy(file_to_copy, file) 
        
      }, error = function(e) {
        #showNotification(paste("Error:", e$message), type = "error")
        showNotification("El archivo no está disponible en las fechas solicitadas. Por favor ignore el archivo html", type = "error")
        
      })
      
      
    },
    contentType = "application/zip"
  )
  
  
  observeEvent(input$category, {
    subcategories <- main_categories$subcategory[main_categories$category == input$category]
    updateSelectInput(session, "subcategory", choices = subcategories)
  })
  
  observeEvent(list(input$category, input$subcategory), {
    req(input$category, input$subcategory) # Ensure both inputs are available
    
    limites_date <- main_limites_fechas[[input$category]][[input$subcategory]]
    
    if (!is.null(limites_date)) {
      updateDateRangeInput(session, "date", min = limites_date[[1]], max = limites_date[[2]])
      
      
      
      # Validate the date range input
      observeEvent(input$date, {
        start_date <- input$date[1]
        end_date <- input$date[2]
        
        
        if (!is.na(start_date) && is.na(end_date)) {
          updateDateRangeInput(session, "date", start =input$date[1], end = input$date[1])
        }
        
        # Check for NA values
        if (!is.na(start_date) && !is.na(end_date)) {
          if (start_date > end_date) {
            # If the start date is greater than the end date, show an error message
            
            showNotification("La fecha de inicio debe ser menor que la fecha final.", type = "error")
            
            # Optionally, you can reset the date range to a valid state
            updateDateRangeInput(session, "date", start =input$date[1], end = input$date[1])
          }
        } else {
          # Handle the case where start_date or end_date is NA
          #showNotification("Las fechas seleccionadas no son válidas.", type = "error")
        }
      })
      
      
    } else {
      updateDateRangeInput(session, "date", min = NULL, max = NULL)
    }
    
  })
  
  
}




