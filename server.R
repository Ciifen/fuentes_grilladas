library(shinyvalidate)
library(shinyFiles)
library(shinyCopy2clipboard)

server <- function(input, output, session){
  
  
  
  output$text <- renderUI({
    HTML('
  <div style="display: flex; justify-content: left; width: 100vw;">
    <div style="border: 1px solid #ddd; padding: 10px; margin-top: 10px; width: 90vw; box-sizing: border-box; display: flex; justify-content: space-between;">
      <!-- Bloque izquierdo -->
      <div style="flex: 1; margin-right: 10px;">
        <p><strong>Consideraciones:</strong></p>
        <p><li>Los valores de temperatura de NASA POWER y ERA5 se convirtieron de K a °C.</li></p>
        <p><li>La base de PERSIANN no contiene las siguientes fechas:</li></p>
        <div style="display: flex; flex-wrap: wrap;">
          <div style="flex: 1; padding: 2px;">
            <ul style="list-style-type: none; padding: 0;">
              <li>20240305</li>
              <li>20240306</li>
              <li>20240307</li>
              <li>20240308</li>
              <li>20240309</li>
            </ul>
          </div>
          <div style="flex: 1; padding: 2px;">
            <ul style="list-style-type: none; padding: 0;">
              <li>20240414</li>
              <li>20240415</li>
              <li>20240416</li>
              <li>20240417</li>
              <li>20240418</li>
              <li>20240419</li>
              <li>20240420</li>
            </ul>
          </div>
          <div style="flex: 1; padding: 2px;">
            <ul style="list-style-type: none; padding: 0;">
              <li>20240319</li>
            </ul>
          </div>
        </div>
      </div>
      <!-- Bloque derecho -->
      <div style="flex: 1;">
        <p><strong>Coordenadas de los países:</strong></p>
        <ul style="list-style-type: none; padding: 0;">
          <li>Venezuela: lonL = -73.378, lonR = -59.8035, latB = 0.6499, latT = 12.2011</li>
          <li>Colombia: lonL = -79.3667, lonR = -66.8694, latB = -4.2271, latT = 12.4583</li>
          <li>Ecuador: lonL = -81.0833, lonR = -75.1867, latB = -5.014, latT = 1.6742</li>
          <li>Peru: lonL = -81.3269, lonR = -68.6651, latB = -18.3496, latT = 0.012</li>
          <li>Bolivia: lonL = -69.6409, lonR = -57.453, latB = -22.8969, latT = -9.6805</li>
          <li>Chile: lonL = -75.6445, lonR = -66.4173, latB = -45, latT = -17.5065</li>
        </ul>
        <p>Excepción, en CHIRPS Chile tiene el límite latB = -50</p>
      </div>
    </div>
  </div>
  ')
  })
  

  
  
  
  coordenadas_list <- list(
    ven = c(lonL = -73.378, lonR = -59.8035,latB = 0.6499, latT = 12.2011 ),
    col = c(lonL = -79.3667, lonR = -66.8694, latB = -4.2271,latT = 12.4583),
    ecu = c(lonL = -81.0833, lonR = -75.1867,  latB = -5.014, latT = 1.6742),
    per = c(lonL = -81.3269, lonR = -68.6651, latB = -18.3496, latT = 0.012),
    bol = c(lonL = -69.6409, lonR = -57.453,  latB = -22.8969,latT = -9.6805),
    chi = c(lonL = -75.6445, lonR = -66.4173, latB = -45, latT = -17.5065)
  )
  
  
  
  output$link_git <- renderUI({
    HTML('<p>Para acceder a los códigos usados para descargar las bases, revise el siguiente GitHub: <a href="https://github.com/Ciifen/fuentes_grilladas" target="_blank">Fuentes grilladas</a></p>')
  })
  

  output$link_buzon <- renderUI({
    HTML('<p>Sus opiniones son importantes para nosotros, pueden enviarnos sus comentarios usando este <a href="https://forms.office.com/Pages/ResponsePage.aspx?id=4RiERL8vEEyYeAO8jUvi4zn0Ued0nDpOt_6uRBkXO09URTZBVDdHTDhDOE8yWFlFVktQSTM2NTI5Ty4u" target="_blank">enlace.</a></p>')
  })
 
  colnames(data) <- c("Fuente", "Tipo","Periodo", "Delay","Parametro","Unidad","Precisi\u00F3n (decimales)", "Resoluci\u00F3n espacial","Resoluci\u00F3n temporal", "Acceso")
  
  output$tabla <- renderDataTable({
    datatable(
      data,
      options = list(
        pageLength = 30,
        #buttons = c('copy', 'csv', 'excel')
        buttons = c('copy'),
        dom = 'lBfrtip',
        
        rowCallback = JS("
          function(row, data) {
            var valueColumnIndex = 5; // Índice de la columna 'Value'
          $('td:eq(' + valueColumnIndex + ')', row).attr('title', 'Los valores de temperatura de NASA POWER y ERA5 se convirtieron de K a °C.');
          }
        ")
        
        #rowCallback = JS("
        #  function(row, data, displayNum, index) {
        #    var valueColumnIndex = 1; // Índice de la columna 'Value'
        #    if (index === 2 || index === 3) { // Fila 3 y 4 (índices 2 y 3)
        #      $('td:eq(' + valueColumnIndex + ')', row).attr('title', 'Este es el valor: ' + data[valueColumnIndex]);
        #    }
        #  }
        #")
        
        
      ),
      rownames = FALSE,
      extensions = 'Buttons',
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
      
      
      obtener_nombre <- function(name) {
        switch(name,
               'ERA5' = 'ERA5',
               'CHIRPS' = 'CHIRPS',
               'GPCC-ALL PRODUCTS' = 'GPCC',
               'NASA-POWER' = 'NASAPOWER',
               'CPC'='CPC',
               'NCAR/UCAR Reanalysis'='NCEP',
               'PERSIANN-CCS'='PERSIANN',
               'GPM (GPM_3IMERGDE)'='GPM',
               stop("No válido")
        )
      }

      paste0(obtener_nombre(category),'_',lugar, '_',parametro,'_',fecha_inicial,'_',fecha_final,'.zip')
      
    },
    
    content <- function(file) {
      showNotification("La descarga está en progreso, por favor espere...", type = "message", duration = 3)
      
      req(input$category)
      req(input$subcategory)
      req(input$pais)
      req(input$date)
      
      category <- input$category
      subcategory <- input$subcategory
      lugar <- lugar_map[[input$pais]]
      parametro <- parameter_map[[category]][[subcategory]]
      rango_fecha <- list(as.Date(input$date[1]), as.Date(input$date[2]))

      
      file_to_copy <- tryCatch({
        if (category == "CHIRPS") {
          file_to_copy <- CHIRPS$downloads_transformar_a_tif(lugar = lugar, Rango_fecha = rango_fecha)
        } else if (category == "NASA-POWER") {
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
          showNotification("El archivo no está disponible en las fechas solicitadas. Por favor ignore el archivo html", type = "error", duration = 7)
          return(NULL)
        }
        file.copy(file_to_copy, file) 
        
      }, error = function(e) {
        #showNotification(paste("Error:", e$message), type = "error")
        showNotification("El archivo no está disponible en las fechas solicitadas. Por favor ignore el archivo html", type = "error", duration = 7)
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




