generarPopupMunicipal=function(ambito='urbana',df=municipios |> st_drop_geometry()
                               #nomgeo_loc,municipio, intervalo,pob_localidad,ind_mun_pob,ind_mun_pob_extr,ind_mun_pob_mod,pob_mun_tot
){
  df=df |> 
    dplyr::mutate(
      pobr_pob=round(as.numeric(Pob_tot_2020) *as.numeric(`Pobr%`)/100,0),
      pobr_mode_pob=round(as.numeric(Pob_tot_2020) *as.numeric(`Pobr_mode%`)/100,0),
      pobr_extr_pob=round(as.numeric(Pob_tot_2020) *as.numeric(`Pobr_ext%`)/100,0),
    )
  ##HTML##
  ########################
  #######Municipio_nombre#######
  ###Con Localidades Elegibles: 0//N###
  #Población total
  #Población elegible por programa
  #Porcentaje de poblacion en rezago social alto o muy alto
  #Porcentaje de poblacion en marginacion alta o muy alta
  
  #Num. localidades urbanas en rezago social alto o muy alto
  #Num. localidades rurales en rezago social alto o muy alto
  
  #Num. localidades urbanas en marginacion social alto o muy alto
  #Num. localidades rurales en marginacion social alto o muy alto

  
  return(paste0(css_popup,'<div class="card" style="background-color:rgba(128,128,128,0.1)">',
                '<h2 id="nomgeo-val">',df$NOM_MUN,'</h2>',
                '<h3 class="indicators-title">Indicadores municipales</h3>',
                '<div class="indicators-grid">
            <div class="indicator-item">
                <p style=\'line-height: 3\'>Pobreza</p>
                <p class="percentage" id="p1-val">',paste0(round(as.numeric(df$`Pobr%`),1),"%"),'</p>
                <div class="indicator-bar" style="background-color:',colorear_rojos(round(as.numeric(df$`Pobr%`),1)),'"></div>',
                '<p class="indicator-value" id="val2-val">',formatC(big.mark = ",",x = df$pobr_pob,format = "d"),'<br>personas','</p>',
                '</div>
            <div class="indicator-item">
                <p>Pobreza<br>Moderada</p>
                <p class="percentage" id="p2-val">',paste0(round(as.numeric(df$`Pobr_mode%`),1),"%"),'</p>
                <div class="indicator-bar" style="background-color:',colorear_rojos(round(as.numeric(df$`Pobr_mode%`),1)),'"></div>',
                '<p class="indicator-value" id="val2-val">',formatC(big.mark = ",",x = df$pobr_mode_pob,format = "d"),'<br>personas','</p>',
                '</div>
            <div class="indicator-item">
                <p>Pobreza<br>Extrema</p>
                <p class="percentage" id="p3-val">',paste0(round(as.numeric(df$`Pobr_ext%`),1),"%"),'</p>
                <div class="indicator-bar" style="background-color:',colorear_rojos(round(as.numeric(df$`Pobr_ext%`),1)),'"></div>',
                '<p class="indicator-value" id="val3-val">',formatC(big.mark = ",",x = df$pobr_extr_pob,format = "d"),'<br>personas','</p>',
                '</div>
        </div>
    </div>'))
}
