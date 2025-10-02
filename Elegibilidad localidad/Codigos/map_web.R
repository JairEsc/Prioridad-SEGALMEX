library(openxlsx)
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

################################################################################
###Contribución Enrique
################################################################################

Localidades_Elegibles_c_geom_puntual=Localidades_Elegibles |> merge(localidades_c_pob |> 
                                                                      dplyr::mutate(cvegeo=substr(cvegeo,1,9)),
                                                                    by.x='cve', by.y='cvegeo') |> 
  dplyr::filter(st_geometry_type(geom)=='POINT') |> st_as_sf()

Localidades_Elegibles_c_geom_polygon=Localidades_Elegibles |> merge(localidades_c_pob |> 
                                                                      dplyr::mutate(cvegeo=substr(cvegeo,1,9)),
                                                                    by.x='cve', by.y='cvegeo') |> 
  dplyr::filter(st_geometry_type(geom)=='MULTIPOLYGON') |> st_as_sf()

###Hagamos el receunto para los municipios####
Localidades_Elegibles_c_geom_puntual$MUN=substr(Localidades_Elegibles_c_geom_puntual$cve,1,5)
Localidades_Elegibles_c_geom_polygon$MUN=substr(Localidades_Elegibles_c_geom_polygon$cve,1,5)
municipios$N_Rezago=numeric(84)
municipios$N_Pobreza_A=numeric(84)
municipios$N_Pobreza_MA=numeric(84)
for(i in 1:84){
  municipios$N_Rezago[i]=nrow(Localidades_Elegibles_c_geom_puntual[Localidades_Elegibles_c_geom_puntual$Indice.de.rezago.alto=="Elegible" &
                                                                    Localidades_Elegibles_c_geom_puntual$MUN==municipios$cvegeo[i],])+
    nrow(Localidades_Elegibles_c_geom_polygon[Localidades_Elegibles_c_geom_polygon$Indice.de.rezago.alto=="Elegible" &
                                                Localidades_Elegibles_c_geom_polygon$MUN==municipios$cvegeo[i],])
  
  municipios$N_Pobreza_A[i]=nrow(Localidades_Elegibles_c_geom_puntual[Localidades_Elegibles_c_geom_puntual$GM_2020=="Alto" &
                                                                        Localidades_Elegibles_c_geom_puntual$MUN==municipios$cvegeo[i],])+
    nrow(Localidades_Elegibles_c_geom_polygon[Localidades_Elegibles_c_geom_polygon$GM_2020=="Alto" &
                                                Localidades_Elegibles_c_geom_polygon$MUN==municipios$cvegeo[i],])
  
  municipios$N_Pobreza_MA[i]=nrow(Localidades_Elegibles_c_geom_puntual[Localidades_Elegibles_c_geom_puntual$GM_2020=="Muy alto" &
                                                                      Localidades_Elegibles_c_geom_puntual$MUN==municipios$cvegeo[i],])+
    nrow(Localidades_Elegibles_c_geom_polygon[Localidades_Elegibles_c_geom_polygon$GM_2020=="Muy alto" &
                                                Localidades_Elegibles_c_geom_polygon$MUN==municipios$cvegeo[i],])
}
municipios$TieneLocs=ifelse(municipios$N_Rezago==0 & municipios$N_Pobreza_A==0 & municipios$N_Pobreza_MA==0,
                         NA,1)
paleta_categorias=colorFactor(palette = c("#BDBDBD"),
                              domain =c(T),
                              alpha = T, na.color = rgb(0,0,0,0)
)
A=read_sf("../../../Reutilizables/regiones/Banco de datos infografias _Eduardo.xlsx")
B=dplyr::select(A,Field1,Field8)
B=B[!is.na(B$Field1) & B$Field1!="Municipio",]
B=B[B$Field1!="Estatal",]
municipios=merge(x = municipios,y = B,by.x="nomgeo",by.y="Field1",all.x=T)
colnames(municipios)[colnames(municipios)=="Field8"]="POB"
################################################################################
mapa_web=leaflet()  |> 
  addTiles(options = leaflet::tileOptions(opacity =1))|>
  setView(lng =-98.88704 ,lat =20.47901,zoom=9) |> 
  addPolygons(data=municipios,
              label = municipios$nomgeo,fillColor = paleta_categorias(is.na(municipios$TieneLocs)),fillOpacity = 0.6,color = "white",weight = 2,group = "Municipios",
              popup =generarPopup_Municipio() #"A" #generarPopupMunicipal()
                ) |> 
  addPolygons(data=Localidades_Elegibles_c_geom_polygon |> st_transform(st_crs("EPSG:4326")),
              fillColor ="#621132" #colorear_rojos((localidades_urbanas_c_pobreza$valor_pobreza))
                ,color = "black",weight = 0.1,opacity = 1,fillOpacity = 1,
              label = paste0(Localidades_Elegibles_c_geom_polygon$NOM_MUN,"-",Localidades_Elegibles_c_geom_polygon$NOM_LOC),
              
              popup =generarPopup_Localidad_Poly() #"B" #generarPopup()
              # paste0("Municipio: ",localidades_urbanas_c_pobreza$Municipio," <br>",
              #              "Localidad: ",localidades_urbanas_c_pobreza$NOMGEO,"<br>",
              #              "pobtot: ",localidades_urbanas_c_pobreza$`Población del ITER**`,"<br>",
              #              "intervalo_pobreza: ",localidades_urbanas_c_pobreza$`Rango de pobreza (%)`,"<br>",
              #              "porcentaje municipal: ",localidades_urbanas_c_pobreza$`Pobr%`,"---",localidades_urbanas_c_pobreza$`Pobr_mode%`,"---",localidades_urbanas_c_pobreza$`Pobr_ext%`,"<br>"
              #              )
              ,group = "Localidades urbanas"
  ) |> 
  addCircleMarkers(data=Localidades_Elegibles_c_geom_puntual |> st_transform(st_crs("EPSG:4326")),
              fillColor ="#D4C29C" #colorear_rojos(as.numeric((localidades_rurales_poligonos_c_pobreza$`Pobr%`)))
                ,color = "black",weight = 0.1,opacity = 1,fillOpacity = 1,radius = 100,
              label = paste0(Localidades_Elegibles_c_geom_puntual$NOM_MUN,"-",Localidades_Elegibles_c_geom_puntual$NOM_LOC),
              popup = generarPopup_Localidad_Point()#"C"#generarPopupRural()
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
        "Este mapa web muestra la elegibilidad a nivel de localidad para el programa social SEGALMEX. Se consideran elegibles aquellas localidades que cumplan cada uno de los siguientes criterios
        "
        
      ),tags$ul(
        tags$li(
          "Población total mayor o igual a 200 y menor a 15,000 habitantes"
        ),
        tags$li(
          "Grado de Marginación Alta o Muy Alta"
        ),
        tags$li(
          "Grado de Rezago Social Alto o Muy Alto"
        )
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
    }")|> 
  addLogo(img = "https://raw.githubusercontent.com/JairEsc/Gob/main/Otros_archivos/imagenes/Planeacion_sigeh.png",src = "remote",width = "400px",height='71px',position = "bottomleft") |> 
  addLegend(
    position = "bottomright",
    colors = c("#D4C29C", "#621132","#BDBDBD"), # 'transparent' for the circle icon
    labels = c("Localidades rurales elegibles","Localidades urbanas elegibles","Municipios SIN localidades elegibles"),
    title = "Simbología", # More descriptive title
    opacity = 0.7,
  )
#mapa_web |> htmlwidgets::saveWidget("Mapa Web/Elegibilidad SEGALMEX.html",title = "Elegibilidad SEGALMEX",selfcontained = T)
htmlwidgets::saveWidget(widget = mapa_web, file = "tmp.html",title = "Elegibilidad SEGALMEX", selfcontained = TRUE)
file.rename("tmp.html", "Mapa Web/Elegibilidad SEGALMEX.html")
