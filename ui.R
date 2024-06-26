library(shinyjs)

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


@media screen and (max-width: 1024px) {
    .new-sidebar {
        display: none !important;
    }
    
    .new-main {
        width: 100%;
    }
}


@media screen and (max-width: 768px){
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
          DTOutput("tabla")
          
          
        )
      )
    )
    
    
    
)
)
  








