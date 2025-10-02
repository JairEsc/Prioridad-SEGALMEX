###LAs funciones para los popup

#Estilos

css_popup = {"<style>
    .leaflet-popup-content{
      width:1200px
    }
    .card {
            width: 100% !important; /* Ancho fijo para la card */
            background-color: #fff;
            border-radius: 8px;
            text-align: center;
            padding: 0px;
            box-sizing: border-box; /* Incluir padding en el ancho */
        }

        .card h2 {
            margin-top: 0;
            color: #333;
            font-size: 1.6em;
            margin-bottom: 1px;
        }

        .card h3 {
            color: #555;
            font-size: 1.2em;
            margin-top: 0;
            margin-bottom: 5px;
        }

        .coords {
            font-size: 1.1em;
            font-weight: bold;
            color: #777;
            margin-bottom: 0.5em;
        }

        .poverty-description {
            font-size: 0.9em;
            color: #666;
            line-height: 1.4;
            margin-bottom: 20px;
        }

        .poverty-number {
            font-size: 1.1em;
            font-weight: bold;
            color: #333;
            margin-bottom: 20px;
        }

        .divider {
            border-bottom: 1px solid #eee;
            margin: 20px 0;
        }

        .indicators-title {
            font-size: 1.3em;
            color: #333;
            margin-bottom: 15px;
        }

        .indicators-grid {
            display: grid;
            grid-template-columns: repeat(3, 1fr);
            gap: 0.5rem;
            text-align: center;
        }

        .indicator-item {
            display: flex;
            flex-direction: column;
            align-items: center;
        }

        .indicator-item p {
            margin: 0;
            font-size: 0.9em;
            color: #444;
            text-wrap:nowrap
        }

        .indicator-item .percentage {
            font-weight: bold;
            font-size: 1.1em;
            color: #e44d26; /* Color para el porcentaje */
            margin-top: 5px;
        }

        .indicator-bar {
            width: 80%;
            height: 10px;
            background-color: #ccc; /* Placeholder para la barra */
            margin-top: 8px;
            border-radius: 5px;
        }

        .indicator-value {
            font-size: 0.85em;
            color: #777;
            margin-top: 5px;
        }
  </style>"}

css_popup2 = {"<style>
    .leaflet-popup-content{
      width:1200px
    }
    .card {
            width: 100% !important; /* Ancho fijo para la card */
            background-color: #fff;
            border-radius: 8px;
            text-align: center;
            padding: 10px;
            box-sizing: border-box; /* Incluir padding en el ancho */
        }

        .card h2 {
            margin-top: 0;
            color: #333;
            font-size: 1.8em;
            margin-bottom: 5px;
        }

        .card h3 {
            color: #555;
            font-size: 1.2em;
            margin-top: 0;
            margin-bottom: 15px;
        }

        .coords {
            font-size: 1.1em;
            font-weight: bold;
            color: #777;
            margin-bottom: 0.5em;
        }

        .poverty-description {
            font-size: 0.9em;
            color: #666;
            line-height: 1.4;
            margin-bottom: 20px;
        }

        .poverty-number {
            font-size: 1.1em;
            font-weight: bold;
            color: #333;
            margin-bottom: 20px;
        }

        .divider {
            border-bottom: 1px solid #eee;
            margin: 20px 0;
        }

        .indicators-title {
            font-size: 1.3em;
            color: #333;
            margin-bottom: 15px;
        }

        .indicators-grid {
            display: grid;
            grid-template-columns: repeat(2, 1fr);
            gap: 30px;
            text-align: center;
        }

        .indicator-item {
            display: flex;
            flex-direction: column;
            align-items: center;
        }

        .indicator-item p {
            margin: 0;
            font-size: 0.9em;
            color: #444;
        }

        .indicator-item .percentage {
            font-weight: bold;
            font-size: 1.1em;
            color: #e44d26; /* Color para el porcentaje */
            margin-top: 5px;
        }

        .indicator-bar {
            width: 80%;
            height: 10px;
            background-color: #ccc; /* Placeholder para la barra */
            margin-top: 8px;
            border-radius: 5px;
        }

        .indicator-value {
            font-size: 0.85em;
            color: #777;
            margin-top: 5px;
        }
  </style>"}


generarPopup_Municipio=function(){
  return(paste0(css_popup,
'<div class="card" style="background-color:rgba(128,128,128,0.1);">',
                  '<h2 id="nomgeo-val">',municipios$nomgeo,'</h2>',
                  ifelse(!is.na(municipios$TieneLocs),paste0('<h3 class="indicators-title">Localidades con: </h3>',
                                                        '<div class="indicators-grid">
                  
                <div class="indicator-item">
                  <p style=\'line-height: 3\'>Rezago Social Alto </p>
                  <p class="percentage">',municipios$N_Rezago,'</p>',
                                                        '</div>
                
                <div class="indicator-item">
                  <p style=\'line-height: 3\'>Marginación Alta </p>
                  <p class="percentage">',municipios$N_Pobreza_A,'</p>',
                                                        '</div>
                
                <div class="indicator-item">
                  <p style=\'line-height: 3\'>Marginación Muy Alta </p>
                  <p class="percentage">',municipios$N_Pobreza_MA,'</p>',
                                                        '</div>

            </div>
            <h3 id="Pob" style="text-align: left;margin-top:5%">',paste0("Población Total:", formatC( municipios$POB |> as.numeric(),format = "d",big.mark = ",")),'</h3>
    </div>'),paste0('<h3 class="indicators-title">Sin localidades elegibles</h3>
                    <h3 id="Pob" style="text-align: left;margin-top:5%">',paste0("Población Total:   ",formatC( municipios$POB |> as.numeric(),format = "d",big.mark = ",")),'</h3>'))))
}


generarPopup_Localidad_Point=function(){
  return(paste0(css_popup2,
'<div class="card" style="background-color:rgba(128,128,128,0.1);">',
'<h2 id="nomgeo-val">',Localidades_Elegibles_c_geom_puntual$NOM_LOC,'</h2>',
'<h3 class="indicators-title">',Localidades_Elegibles_c_geom_puntual$NOM_MUN,'</h3>',

'<div class="indicators-grid"> 
                <div class="indicator-item">
                  <p style=\'line-height: 3\'>Nivel de Rezago Social</p>
                  <p class="percentage">',Localidades_Elegibles_c_geom_puntual$IRS_2020,'</p>',
                '</div>
                
                <div class="indicator-item">
                  <p style=\'line-height: 3\'>Nivel de Marginación</p>
                  <p class="percentage">',Localidades_Elegibles_c_geom_puntual$GM_2020,'</p>',
                '</div>
            </div><h3 id="Pob" style="text-align: left;">',paste0("Población :   ",formatC( Localidades_Elegibles_c_geom_puntual$POBTOT |> as.numeric(),format = "d",big.mark = ",")),'</h3>'))
}

generarPopup_Localidad_Poly=function(){
  return(paste0(css_popup2,
    '<div class="card" style="background-color:rgba(128,128,128,0.1);">',
    '<h2 id="nomgeo-val">',Localidades_Elegibles_c_geom_polygon$NOM_LOC,'</h2>',
    '<h3 class="indicators-title">',Localidades_Elegibles_c_geom_polygon$NOM_MUN,'</h3>',
    
    '<div class="indicators-grid">
                <div class="indicator-item">
                  <p style=\'line-height: 3\'>Nivel de Rezago Social</p>
                  <p class="percentage">',Localidades_Elegibles_c_geom_polygon$IRS_2020,'</p>',
    '</div>
                
                <div class="indicator-item">
                  <p style=\'line-height: 3\'>Nivel de Marginación</p>
                  <p class="percentage">',Localidades_Elegibles_c_geom_polygon$GM_2020,'</p>',
    '</div>
            </div><h3 id="Pob" style="text-align: left;">',paste0("Población :   ",formatC( Localidades_Elegibles_c_geom_polygon$POBTOT |> as.numeric(),format = "d",big.mark = ",")),'</h3>'))
}