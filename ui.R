library(shinyjs)


#revisar
html_title <-
  '<span class="logo-container">
      <a href=""><img src="AdaptationFundLogo.jpg" height="60"/></a>
      <a href=""><img src="WMO_Spanish_Log-cst.png" height="60"/></a>
      <a href=""><img src="1-ENANDES-COLOR-2.png" height="60"/></a>
      <a href=""><img src="ciifencst.png" height="60"/></a>
      <a href=""><img src="crcosacst.png" height="60"/></a>
      
    </div>
  </span>'

html_mapa <-
  '<span class="mapa-container">
      <a href=""><img src="paises_crc_osa.jpg" /></a>
    </div>
  </span>'

custom_css <-
  "
  
  #DataTables_Table_0_wrapper{
  margin-top:50px;
  }
  
  
.logo-container {
  display: flex;
  justify-content: center;
  align-items: center;
  padding: 20px 0;
}

.mapa-container {
  display: flex;
  justify-content: center;
  align-items: center;
}

.logo-container a img {
  margin: 0 70px;
}

.nav-underline{
  padding-top: 1px;
  display:inline;

}
.row {
  display: flex;
  flex-wrap: nowrap;
  justify-content: flex-start;
  margin: 5px;
}

.columna {
  flex: 0 0 auto;
  padding: 2px;
  margin-right: 5px;
}

.columna ul {
  list-style-type: none;
  padding: 0;
}



@media screen and (max-width: 1024px) {
  .new-sidebar {
    display: none !important;
  }

  .new-main {
    width: 100%;
  }
}

@media screen and (max-width: 768px) {
  .navbar-nav {
    float: right;
    width: 100%;
  }
  
}

"


ui <- fluidPage(
  
  
  
  # Inicializar shinyjs
  shinyjs::useShinyjs(),
  
  tags$head(tags$style(HTML(custom_css))),
  
  HTML(html_title),
  
  navbarPage(
    collapsible = TRUE,
    title = '',
    tabPanel(
      'Descarga',
      fluidPage(
        
        titlePanel(
          fluidRow(
            column(12, "Herramienta para descarga de bases de datos grilladas"), 
          )
        ),
        
        sidebarLayout(
          sidebarPanel(
            selectInput("category", "Seleccione una base:", choices = unique(main_categories$category)),
            selectInput("subcategory", "Seleccione una variable:", choices=NULL),
            selectInput("pais", "Seleccione un país:",choices = c("Bolivia","Chile","Colombia","Ecuador","Per\u00FA","Venezuela")),
            dateRangeInput(inputId = 'date',
                           label = 'Seleccionar rango de fecha:',
                           min= NULL, max= NULL),
            downloadButton("downloadData", label = "Descargar")
            
          ),
          mainPanel(
            #HTML(html_mapa)
            tags$img(src = "paises_crc_osa.jpg", style="width: 250px; margin-left: 40%;"),
            #h6("Sample download", align = "center")
            
          )
        )
      )
      
    ), 
    
    
    
    
    tabPanel(
      'Información sobre las bases',
      fluidPage(
        #titlePanel('Detalles sobre las bases'),
        
        mainPanel(
          htmlOutput("text"),
          DTOutput("tabla"),
          
        )
        
        
      )
    ),
    
    
    
    


    
    tabPanel(
      'Códigos',
      fluidPage(
       
          tabPanel(
            'Códigos',
            fluidPage(
              # Primera línea con htmlOutput
              fluidRow(
                column(12, htmlOutput("link_git"))
              ),
              
              
              tags$br(),
              
      
              fluidRow(
                column(12, tags$p(  tags$b("Ejemplos de códigos para leer los archivos descargados del programa"),   " "))
              ),
              
              fluidRow(
                column(12, tags$p(  "Ejemplo en R ",   " "))
              ),
              
              
              # Segunda línea con el cuadro de código y botón de copiar
              fluidRow(
                column(12,
                       div(style = "position: relative;",
                           actionButton("copyBtn", "Copiar código", class = "copy-btn", style = "position: absolute; top: 10px; right: 10px; z-index: 10;"),
                           aceEditor("code", 
                                     value = "library(maps)
                                     
#Funcion obtiene la posicion de una fecha especifica entre un rango de fecha                         
fecha_posicion <- function(fecha_inicio, fecha_fin, fecha_consulta) {
# Convertir las fechas a formato Date
fecha_inicio <- ymd(fecha_inicio)
fecha_fin <- ymd(fecha_fin)
fecha_consulta <- ymd(fecha_consulta)
# Comprobar si la fecha de consulta está dentro del rango
if (fecha_consulta < fecha_inicio || fecha_consulta > fecha_fin) {
  return(\"La fecha de consulta no está dentro del rango.\")}
# Calcular la posición de la fecha de consulta dentro del rango
posicion <- as.numeric(fecha_consulta - fecha_inicio + 1)
return(posicion)
                                     }

raster_path <- \"/ejemplo/ruta/NASAPOWER_col_TMAX_2024-05-08_2024-05-15.tif\" 

# Extraer las fechas del nombre del archivo
raster_dir<- dirname(raster_path)
name_without_ext <-  substring(basename(raster_path),1, nchar(basename(raster_path)) - 4 )
file_name_parts <- unlist(strsplit(name_without_ext, \"_\"))
fecha_inicial <- file_name_parts[length(file_name_parts) - 1]
fecha_final <- file_name_parts[length(file_name_parts)]
print(paste0('Los datos se encuentran entre los dias ',fecha_inicial,\" y \", fecha_final))


# Graficar un dia especifico
date_selec <- \"2024-05-05\"

num_band <- fecha_posicion(fecha_inicial, fecha_final, date_selec)
Band <- raster(raster_path, band=num_band)

#Estos datos pueden convertirse a data frame y plotear la banda seleccionada
Band_df  <- raster::as.data.frame(Band, xy = TRUE)
colnames(Band_df)[3] <- \"Value\"
Band_df <- na.omit(Band_df)

#Graficar
ggplot() +
  geom_raster(data = Band_df , aes(x = x, y = y, fill= Value )) +
  scale_fill_viridis_c() + 
  labs(title = \"Mapa de Temperatura\",
       x = \"Longitud\",
       y = \"Latitud\",
       fill = \"Temperatura\") +

  theme_minimal()"
                                     
                                     
                                     ,
                                     
                                     
                                     mode = "r", 
                                     theme = "chrome",
                                     readOnly = TRUE,
                                     height = "400px",
                                     fontSize = 14
                           )
                       )
                )
              )
            )
          ),
        
          tags$br(),
        
        tags$script(HTML("
    $(document).ready(function() {
      $('#copyBtn').on('click', function() {
        var copyText = document.getElementById('code').env.document.doc.getValue();
        var textArea = document.createElement('textarea');
        textArea.value = copyText;
        document.body.appendChild(textArea);
        textArea.select();
        document.execCommand('copy');
        document.body.removeChild(textArea);
        #alert('Código copiado al portapapeles');
      });
    });
  "))
      )
      

    ),
    
    
    
    
    
    
    
    
    
    
    
    
    
    tabPanel(
      'Comentarios',
      fluidPage(
        titlePanel('Buzón de sugerencias'),
        
        fluidRow(
          column(12, htmlOutput("link_buzon"))
        ),
        
        
        
      )
    ),
    
    
    
  )
)









