"Datos/Localidades elegibles Diconsa.xlsx" |> openxlsx::read.xlsx()->Localidades_Elegibles
"Datos/Localidades elegibles Diconsa.xlsx" |> openxlsx::read.xlsx(sheet = "Marginación")->Localidades_Elegibles_Marginacion
"Datos/Localidades elegibles Diconsa.xlsx" |> openxlsx::read.xlsx(sheet = "resago alto")->Localidades_Elegibles_Rezago
Localidades_Elegibles_Rezago[(nrow(Localidades_Elegibles_Rezago)+1),]=colnames(Localidades_Elegibles_Rezago)
colnames(Localidades_Elegibles_Rezago)=colnames(Localidades_Elegibles_Marginacion)
colnames(Localidades_Elegibles_Rezago)[4]='IRS_2020'
Localidades_Elegibles=Localidades_Elegibles |> 
  dplyr::filter(Indice.de.rezago.alto=='Elegible'&Marginación.alta.y.muy.alta=='Elegible')
Localidades_Elegibles=Localidades_Elegibles |> merge(Localidades_Elegibles_Marginacion |> dplyr::select(CVE_LOC,GM_2020),by.x='cve',by.y='CVE_LOC',all.x = T)
Localidades_Elegibles=Localidades_Elegibles |> merge(Localidades_Elegibles_Rezago |> dplyr::select(CVE_LOC,IRS_2020),by.x='cve',by.y='CVE_LOC',all.x = T)
Localidades_Elegibles$IRS_2020='Alto'
library(leaflet)
library(leaflet.extras)
library(sf)
library(leafem)
library(htmlwidgets)
library(htmltools)
source("../../../Reutilizables/Postgres_BUIG/conexion_buig.R")
municipios=st_read(buig,"limite_municipal")
localidades=st_read(buig,"localidades")
localidades_c_pob=rbind(st_read(buig,"poblacion_scince_2020localidad rural") |> dplyr::select(cvegeo,pob1,nomgeo),
                        st_read(buig,"poblacion_scince_2020localidad_urbana") |> dplyr::select(cvegeo,pob1,nomgeo))

localidades_c_pob=localidades_c_pob |> 
  dplyr::filter(pob1>=200 & pob1<15000)
##Vamos a revisar el de chapulhuacan. Sí hay una localidad que coincide en nombre y población. La Reforma de Palo Semita

Localidades_Elegibles$cve[Localidades_Elegibles$NOM_LOC=='La Reforma de Palo Semita']='130180100'

#######################
Localidades_Elegibles_c_geom_puntual=Localidades_Elegibles |> merge(localidades_c_pob |> 
                                                                      dplyr::mutate(cvegeo=substr(cvegeo,1,9)),
                                                                    by.x='cve', by.y='cvegeo') |> 
  dplyr::filter(st_geometry_type(geom)=='POINT') |> st_as_sf()
Localidades_Elegibles_c_geom_polygon=Localidades_Elegibles |> merge(localidades_c_pob |> 
                                                                      dplyr::mutate(cvegeo=substr(cvegeo,1,9)),
                                                                    by.x='cve', by.y='cvegeo') |> 
  dplyr::filter(st_geometry_type(geom)=='MULTIPOLYGON') |> st_as_sf()



leaflet()  |> 
  addTiles(options = leaflet::tileOptions(opacity =0.6))|>
  setView(lng =-98.88704 ,lat =20.47901,zoom=9) |> 
  addPolygons(data=municipios,
              label = municipios$nomgeo,fillColor = "gray",fillOpacity = 0.1,color = "white",weight = 3,group = "Municipios",
              popup ="A" #generarPopupMunicipal()
                ) |> 
  addPolygons(data=Localidades_Elegibles_c_geom_polygon |> st_transform(st_crs("EPSG:4326")),
              fillColor ="brown" #colorear_rojos((localidades_urbanas_c_pobreza$valor_pobreza))
                ,color = "black",weight = 0.1,opacity = 1,fillOpacity = 1,
              label = paste0(Localidades_Elegibles_c_geom_polygon$NOM_MUN,"-",Localidades_Elegibles_c_geom_polygon$NOM_LOC),
              
              popup ="B" #generarPopup()
              # paste0("Municipio: ",localidades_urbanas_c_pobreza$Municipio," <br>",
              #              "Localidad: ",localidades_urbanas_c_pobreza$NOMGEO,"<br>",
              #              "pobtot: ",localidades_urbanas_c_pobreza$`Población del ITER**`,"<br>",
              #              "intervalo_pobreza: ",localidades_urbanas_c_pobreza$`Rango de pobreza (%)`,"<br>",
              #              "porcentaje municipal: ",localidades_urbanas_c_pobreza$`Pobr%`,"---",localidades_urbanas_c_pobreza$`Pobr_mode%`,"---",localidades_urbanas_c_pobreza$`Pobr_ext%`,"<br>"
              #              )
              ,group = "Localidades urbanas"
  ) |> 
  addCircleMarkers(data=Localidades_Elegibles_c_geom_puntual |> st_transform(st_crs("EPSG:4326")),
              fillColor ="lightblue" #colorear_rojos(as.numeric((localidades_rurales_poligonos_c_pobreza$`Pobr%`)))
                ,color = "black",weight = 0.1,opacity = 1,fillOpacity = 1,radius = 100,
              label = paste0(Localidades_Elegibles_c_geom_puntual$NOM_MUN,"-",Localidades_Elegibles_c_geom_puntual$NOM_LOC),
              popup = "C"#generarPopupRural()
              #   paste0("Municipio: ",localidades_rurales_poligonos_c_pobreza$NOM_MUN," <br>",
              #                "Localidad: ",localidades_rurales_poligonos_c_pobreza$NOMGEO,"<br>",
              #                "pobtot: ",localidades_rurales_poligonos_c_pobreza$POBTOT,"<br>",
              #                "porcentaje municipal: ",localidades_urbanas_c_pobreza$`Pobr%`,"---",localidades_urbanas_c_pobreza$`Pobr_mode%`,"---",localidades_urbanas_c_pobreza$`Pobr_ext%`,"<br>",
              #                "absoluto municipal: ",as.numeric(localidades_rurales_poligonos_c_pobreza$POBTOT)*as.numeric(localidades_rurales_poligonos_c_pobreza$`Pobr%`)/100,"<br>",
              #                "absoluto municipal moderada: ",as.numeric(localidades_rurales_poligonos_c_pobreza$POBTOT)*as.numeric(localidades_rurales_poligonos_c_pobreza$`Pobr_mode%`)/100,"<br>",
              #                "absoluto municipal extrema: ",as.numeric(localidades_rurales_poligonos_c_pobreza$POBTOT)*as.numeric(localidades_rurales_poligonos_c_pobreza$`Pobr_ext%`)/100,"<br>"
              # )
              ,group = "Localidades Rurales"
  ) |> 
  addLayersControl(overlayGroups = c("Localidades urbanas","Localidades Rurales"),options = layersControlOptions(collapsed = F)) |> 
  #addLegend(title = "Porcentaje de Pobreza",,position = "bottomright",pal = colorear_rojos_factor,values =c("[0-20)", "[20,40)", "[40,60)", "[60,80)", "[80-100]"), opacity = 1) |> 
  addSearchFeatures(targetGroups = c("Localidades urbanas","Localidades Rurales"),
                    options = searchFeaturesOptions(
                      zoom = 12,
                      openPopup = F,
                      firstTipSubmit =F,initial = F,
                      hideMarkerOnCollapse =T)) |> 
  addEasyButton(
    easyButton(
      icon = "fa-info-circle",
      title = "Información",
      onClick = JS("function(btn, map){ 
      var modal = document.getElementById('infoModal');
      if (modal) modal.style.display = 'block';
    }")
    )
  ) |> 
  prependContent(
    tags$div(
      id = "infoModal",
      class = "modal",
      style = "display:none; position:fixed; top:20%; left:20%; width:60%; background:white; padding:20px; border:2px solid black; z-index:1000; overflow-y: auto;", # Added overflow-y: auto for scroll if content is long
      
      tags$h4("Metodología"),
      tags$p(
        "La elaboración de este mapa se basó en dos fuentes principales de datos y abordó la estimación de la pobreza a dos niveles geográficos distintos:"
      ),
      tags$ul(
        tags$li(
          tags$strong("Localidades Urbanas (268 polígonos): "),
          "Por primera vez,",tags$a(href = "https://www.coneval.org.mx/Medicion/Paginas/pobreza_localidad_urbana.aspx", "CONEVAL") ," publicó datos de pobreza a nivel de localidad urbana en 2020. Para estas localidades, filtramos la información para el estado de Hidalgo. Dado que CONEVAL presenta la información como un intervalo del porcentaje de pobreza, tomamos el punto medio de dicho intervalo. Combinamos este valor con los datos de población a nivel de localidad del",tags$a(href = "https://www.inegi.org.mx/programas/ccpv/2020/#tabulados", "INEGI 2020") ," para estimar la ",
          tags$strong("población en pobreza"),
          " en cada una de estas 268 localidades urbanas."
        ),
        tags$li(
          tags$strong("Otras Localidades (rurales y urbanas no cubiertas por CONEVAL a nivel localidad): "),
          "Para el resto de las más de 5,000 localidades (entre urbanas y rurales) de Hidalgo que están georreferenciadas, se utilizó el ",
          tags$strong("porcentaje de pobreza del municipio"),
          " al que pertenecen. Con los datos de población de estas localidades reportados por INEGI, se estimó la población en pobreza utilizando los indicadores de pobreza a nivel municipal (pobreza, pobreza moderada y pobreza extrema)."
        )
      ),
      tags$p(
        "Adicionalmente, el mapa también presenta los datos de ",
        tags$strong("población en pobreza"),
        ", ",
        tags$strong("pobreza extrema"),
        " y ",
        tags$strong("pobreza moderada"),
        ", así como sus respectivos porcentajes, a ",
        tags$strong("nivel municipal"),
        "."
      ),
      
      tags$button("Cerrar", onclick = "document.getElementById('infoModal').style.display='none'")
    )
  ) |> 
  onRender("function() {
    var map = this; 
    function resizeMarkers() {
      var zoom = map.getZoom();
      if(zoom>=10){
        map.eachLayer(function(layer) {
          if (layer.options) {
            var newSize;
            if (layer.options.group === 'Localidades Rurales' ) {
              newSize = zoom*2-15+2
              layer.setRadius(newSize);
            }
          }
        });
      }
      else{
      map.eachLayer(function(layer) {
          if (layer.options) {
            var newSize;
            if (layer.options.group === 'Localidades Rurales' ) {
              newSize = zoom*2-15+2
              layer.setRadius(newSize);
            }
          }
        });
      }
    }

    // Llamar a resizeMarkers en eventos de zoom y al cargar el mapa
    map.on('zoomend', resizeMarkers);
    map.whenReady(resizeMarkers);
    }")|> addLogo(img = "https://raw.githubusercontent.com/JairEsc/Gob/main/Otros_archivos/imagenes/Planeacion_sigeh.png",src = "remote",width = "400px",height='71px',position = "bottomleft") 
mapa_web
