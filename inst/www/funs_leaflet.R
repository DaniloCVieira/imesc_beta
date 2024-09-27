#' @export
mode_function <- function(x, na.rm = F) {
  levels_data <- levels(x)
  level_codes <- 1:length(levels_data)
  names(level_codes) <- levels_data
  if(isTRUE(na.rm)){
    x<-na.omit(x)
  }
  x <- level_codes[as.character(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


rst0_nsample<-function(data,nsample){
  coords<-attr(data,"coords")
  daco<-cbind(coords,data)
  spp<-to_spatial(daco)

  spsp<-spsample(spp,nsample,"regular")
  coo<-coordinates(spsp)
  xn<-length(unique(coo[,1]))
  yn<-length(unique(coo[,2]))
  cells<-min(c(xn,yn))
  remp<-raster(spsp, nrows=cells,ncols=cells)
  remp

}
#' @export
rst0<-function(data,resolution=0.01){
  res<-resolution
  coords<-attr(data,"coords")
  x_min <- min(coords[,1])
  x_max <- max(coords[,1])
  y_min <-min(coords[,2])
  y_max <- max(coords[,2])
  ncol <- ceiling((x_max - x_min) / res)
  nrow <- ceiling((y_max - y_min) / res)
  spp<-to_spatial(coords)
  remp<-raster(spp, nrows=nrow,ncols=ncol)
  remp
}


#' @export
rst0_back<-function(data){
  coords<-attr(data,"coords")
  ymin=length(unique(coords[,2]))
  xmin=length(unique(coords[,1]))
  ncol=round(sqrt(xmin))
  nrow=round(sqrt(ymin))
  prop<-ncol/nrow
  ncol*nrow
  len<-length(data[,1])
  x<-round(sqrt(len)*prop)
  y<-round(len/round(sqrt(len)*prop))
  # set up an 'empty' raster, here via an extent object derived from your data
  e <- extent(to_spatial(coords))
  my_rst <- raster(e, ncol=x, nrow=y)
  my_rst
}
#' @export
rst_num<-function(my_rst,data){
  coords<-attr(data,"coords")
  my_rst <- rasterize(coords, my_rst, data[,1], fun=mean)
  my_rst<-raster::ratify(my_rst)
  my_rst
}
#' @export
rst_factor<-function(my_rst,data){

  levels_data <- levels(data[,1])

  coords<-attr(data,"coords")
  my_rst <- rasterize(coords, my_rst, data[,1], fun=mode_function)

  llevs<-levels(data[,1])[as.numeric(  levels(factor(values(my_rst))))]

  values(my_rst)<-factor(values(my_rst), labels=llevs)

  my_rst
}
#' @export


#' @export
get_chart_data <- function(data,factor_chart=2, distance = 50, fun="sum") {
  coords<-attr(data,"coords")
  colnames(coords)<-c("x","y")
  factors<-attr(data,"factors")[factor_chart]
  distance_km<-distance*1000
  di<-geodist(coords,measure="geodesic")
  coords<-do.call(rbind,lapply(1:nrow(di), function(i){
    pic<-unique(which(di[i,]<distance_km),
                which(di[,i]<distance_km))
    apply(coords[pic,],2,mean)
  }))
  df<-data.frame(coords,fac=factors[,1],z=data[,1])
  if(is.factor(df$z)){
    df$factor_new<- apply(df[-c(1:2)],1,function(x) paste(x, collapse = "_split_"))
  } else{
    df$factor_new<- df$fac
  }
  dfk<-cbind(df[,1:2],kohonen::classvec2classmat( df$factor_new))
  if(!is.factor(df$z)){
    dfk<-fill_z_chart(dfk,df)
  }
  if(fun=="sum"){
    resultado <- dfk  |>
      group_by(x, y) |>
      summarise(across(everything(), sum, na.rm = TRUE), .groups = 'drop')
  } else if(fun=="mean"){
    resultado <- dfk |>
      group_by(x, y) |>
      summarise(across(everything(), mean, na.rm = TRUE), .groups = 'drop')}


  #colnames(resultado)<-gsub("_split_",".",colnames(resultado))
  return(resultado)

}
#' @export
get_df_chart2<-function(resultado,result="combined"){
  class_factor<-gsub("_split_*.","",colnames(resultado))[-c(1:2)]
  z_factor<-gsub(".*_split_","",colnames(resultado))[-c(1:2)]
  resultado_new<-data.frame(resultado[-c(1:2)])
  for(i in 1:nrow(resultado_new)){
    vec<-unlist(resultado_new[i,])

    vals<-z_factor[vec>0]
    cols_index<-which(vec>0)
    newvec<-if(result=="combined"){
      paste(class_factor[vec>0],vals, sep=":")
    } else{
      vals
    }
    resultado_new[i,cols_index]<-    newvec
  }
  colnames(resultado_new)<-gsub("_split_",".",colnames(resultado_new))
  resultado_new

}
#' @export
fill_z_chart<-function(dfk,df){
  dfk2<-dfk[-c(1:2)]
  i=1
  for(i in 1:nrow(dfk2)){
    pic<-which(dfk2[i,]>0)
    dfk2[i,pic]<-df$z[i]

  }
  cbind(dfk[c(1:2)],dfk2)
}
#' @export
get_factor_names<-function(df){
  class_factor<-gsub("_split_*.","",colnames(df))[-c(1:2)]
  z_factor<-gsub(".*_split_","",colnames(df))[-c(1:2)]
  list(class_factor=class_factor,z_factor=z_factor)
}
#' @export
get_cols_factor_chart<-function(df,data, pal){
  names_factors<-get_factor_names(df)
  class_factor<-names_factors$class_factor
  z_factor<-names_factors$z_factor

  min_opacity_factor<-0.6
  alphas<-seq(1,min_opacity_factor,len=length(unique(class_factor)))[as.factor(class_factor)]
  cols<-colorRampPalette(pal)(nlevels(data[,1]))
  cols<-cols[levels(data[,1])%in%data[,1]]
  cols<-cols[as.factor(z_factor)]
  cols<-sapply(seq_along(cols),function(i) adjustcolor(cols[i],alphas[i]))
  cols
}
#' @export
get_popup_chart<-function(df){
  facnames<-get_factor_names(df)
  aa<-get_df_chart2(df, "not")
  aa<-data.frame(t(aa))
  do.call(rbind,lapply(aa,function(x){
    pic<-x!=0
    xpic<-x[pic]
    clapic<-facnames[[1]][pic]
    sapply(unique(clapic), function(x){
      paste(xpic[clapic==x],collapse  =";")
    })
  }))
}
#' @export

#' @export

#' @export
add_shapes1<-function(map,data,layer_shape_args,base_shape_args){

  if(isTRUE(base_shape_args$shape)){
    base_shape_args$map<-map
    base_shape_args$data<-data
    base_shape_args$shape_attr<-"base_shape"

    map<-do.call(add_base_shape,base_shape_args)

  }
  if(isTRUE(layer_shape_args$shape)){
    layer_shape_args$shape_attr<-"layer_shape"
    layer_shape_args$map<-map
    layer_shape_args$data<-data
    map<-do.call(add_base_shape,layer_shape_args)
  }

  map

}
#' @export
control_layers<-function(addCircles,addMinicharts,rst,base_shape_args,layer_shape_args,args_labels=NULL){
  layers_data<-c()
  if(isTRUE(addCircles)){
    layers_data[length(layers_data)+1]<-"circles"
  }

  if(!is.null(rst)){
    layers_data[length(layers_data)+1]<-"raster"
  }
  if(!is.null(args_labels)){
    if(isTRUE(args_labels$show_labels)){
      layers_data[length(layers_data)+1]<-"labels"
    }

  }

  layers_shape<-c()
  if(is.null(rst)){
    if(isTRUE(base_shape_args$shape)){
      layers_shape[length(layers_shape)+1]<-"base_shape"
    }
    if(isTRUE(layer_shape_args$shape)){
      layers_shape[length(layers_shape)+1]<-"layer_shape"
    }
  }

  c(layers_shape,layers_data)

}
#' @export
add_extraLL<-function(map,data,args_extra_shape){
  if(is.null(args_extra_shape)){
    return(map)
  }
  map2<-map
  ea<-data.frame(args_extra_shape)
  ea[,1]<-as.logical(ea[,1])
  ea[,3]<-as.numeric(ea[,3])
  ea[,5]<-as.numeric(ea[,5])

  for(k in 1:nrow(args_extra_shape)){
    e<-attr(data,"extra_shape")[[k]]
    ei<-ea[k,]
    if(!is.null(e)){
      if(ei$layers){
        extra<-st_as_sf(e)[,"geometry"]
        if(!is.null(extra)){
          for(i in 1:nrow(extra)){
            geom<-sf::st_geometry_type(extra[i,])
            base_temp<-st_cast(extra[i,],"LINESTRING")
            for(j in 1:nrow(base_temp)){
              co<-st_coordinates(base_temp[j,])[,1:2]
              map2<-map2 |> leaflet::addPolylines(co[,1],co[,2],color =ei$colors ,opacity=1,weight =ei$sizes,group="extra_shape")
            }
          }
        }
      }
    }
  }
  return(map2)
}
#' @export

lab_nmax<-span("nmax:", tiphelp("for local kriging, the number of nearest observations that should be used for a kriging prediction or simulation, where nearest is defined in terms of the space of the spatial locations. By default (empty), all observations are used"))
lab_nmin<-span( "nmin:",tiphelp("for local kriging, if the number of nearest observations within distance maxdist is less than nmin, a missing value will be generated- see maxdist"))
lab_omax<-span("omax:",tiphelp("maximum number of observations to select per octant (3D) or quadrant (2D); only relevant if maxdist has been defined as well"))
lab_maxdist<-span("maxdist:",tiphelp("for local kriging, only observations within a distance of maxdist from the prediction location are used for prediction; if combined with nmax, both criteria apply"))
lab_idp<-span("idp:",tiphelp("numeric; specify the inverse distance weighting power"))
lab_k<-span("k:",tiphelp("number of neighbours considered."))
lab_resolution<-span("resolution:",tiphelp("Interpolation resolution"))

interp_leaflet<-function(data, cut_shape=T, resolution=3000,crs.info="+proj=longlat +datum=WGS84 +no_defs",k=5,...){
  if(is.factor(data[,1])){
    get_knn_leaf(data,k,cut_shape,resolution)
  } else{
    get_idw_leaf(data, cut_shape, resolution,crs.info="+proj=longlat +datum=WGS84 +no_defs",...)

  }
}


#' @export

#' @export


#' @export
migrate_rst<-function(rst,data){
  attr(rst,"coords")<-attr(data,"coords")
  attr(rst,"factors")<-attr(data,"factors")
  attr(rst,"base_shape")<-attr(data,"base_shape")
  attr(rst,"layer_shape")<-attr(data,"layer_shape")
  attr(rst,"extra_shape")<-attr(data,"extra_shape")
  attr(rst,"levels")<-attr(rst,"data_levels")
  attr(rst,'z_name')<-colnames(data[1])
  rst
}
#' @export
get_raster_template<-function(kc,resolution, sample=F){
  cd<-coordinates(kc)


  res1<-dist(sort( cd[,1])[1:2])
  res2<-dist(abs(sort( cd[,2])[1:2]))
  resol<-round(sqrt(resolution/2))
  resol<-c(resol[1],resol[2])


  example_raster <- raster(crs = crs(kc), vals = 0, nrows=resol[1],ncols=resol[2], ext = extent(kc))
  example_raster
}
#' @export
variogram_parameters<-function(df,model="Gau",var=1, log=F, g=NULL){


  log_params=NULL
  if(isTRUE(log)){
    log_var<-decostand(df[,var],"log")
    log_params<-attr(log_var,"parameters")
    df[,var]<-log_var
  }
  coords<-attr(df,"coords")[rownames(df),]
  colnames(coords)<-c("x","y")
  z<-df[var]
  if(!is.null(g)){
    if(length(g$data)>0){
      newnames<-make.unique(c(names(g$data),var))
      var<-newnames[length(newnames)]
      colnames(z)<-var
    }
  }
  dd<-cbind(coords,z)
  formula<-as.formula(paste(var,"~ 1"))
  dsp<-to_spatial(dd)



  if(nrow(sp::zerodist(dsp))>0){
    dsp <- dsp[-sp::zerodist(dsp)[,1],]
  }
  TheVariogram=NULL


  auto_z1<-automap::autofitVariogram(formula, dsp,model=model)
  vgm_z1<-auto_z1[[1]]
  model_z1<-auto_z1[[2]]
  return(list(variogram=vgm_z1,auto_z1=auto_z1, formula=formula,dsp=dsp, id=var, log=log,log_params=log_params))
}
#' @export
predict_cokrige<-function(g,newdata=NULL, crs.info="+proj=longlat +datum=WGS84 +no_defs"){


  coords<-attr(g,"coords")
  base_shape<-attr(g,"base_shape")
  layer_shape<-attr(g,"layer_shape")
  limits<-get_limits(limits=NULL,base_shape,layer_shape,coords)
  if(is.null(newdata)){
      #newgrid<-get_grid(coords,limits,crs.info,res=resolution)
    newgrid<-data.frame(coords,lapply(g$data,function(x) data.frame(x$data)[3]))
  } else{
    newgrid<-newdata
  }

  grd<-to_spatial(data.frame(newgrid),crs.info=crs.info)
  k.c <- predict(g, grd)
  #pred1<-predict(g, newdata =newgrid )
  result<-data.frame(k.c)
  result["optional"]<-NULL
  islog=attr(g,"log")

  if(!is.null(islog)){
    names(islog)<-paste0(names(g$data),".pred")
    for(i in 1:length(islog)){
      if(!is.null(islog[[i]])){
        pred<-result[,names(islog)[i]]
        parameters<-islog[[i]]
        result[,names(islog)[i]]<-(2^pred) * parameters$minpos
      }
    }
  }
  atts<-attributes(result)
  result<-predict_cokrige_factors(result,g)

  attr(result,"k.c")<-k.c
  attr(result,"coords")<-coords
  attr(result,"base_shape")<-base_shape
  attr(result,"layer_shape")<-layer_shape
  result
}


#' @export
predict_cokrige_factors<-function(pred,g){
  isfac<-attr(g,"is_factor")
  if(!any(isfac)){
    return(pred)
  }
  factor_column<-attr(g,"factor_column")
  factor_level<-attr(g,"factor_level")
  preds0<-preds<-pred[grep("\\.pred",colnames(pred))]
  colnames(preds0)<-gsub("gstat_level_","",colnames(preds0))
  predfac_results<-list()
  for(var in unique(factor_column)){
    preds<-preds0
    pic_var<-which(factor_column==var)
    preds<-preds[pic_var]
    isfac2<-isfac[pic_var]
    if(all(isfac2)){
      pred_fac<-preds[which(isfac2)]
      pred_tab<-decostand(pred_fac,"range")
      probs<-pred_tab
      colnames(probs)<-colnames(pred_fac)
      predictions<-kohonen::classmat2classvec(probs)
      df_pred<-data.frame(pred=factor(predictions, labels=unique(factor_level[pic_var])))
      colnames(df_pred)<-var
      predfac_results[[var]]<-df_pred
    }
  }

  pred_class<-data.frame(do.call(cbind,predfac_results))
  colnames(pred_class)<-paste0(names(predfac_results),".pred")
  rownames(pred_class)<-rownames(pred)
  pred_result<-cbind(pred,pred_class)
  return(pred_result)

}
#' @export
df_gpred<-function(pred, var){
  kc<-attr(pred,"k.c")
  coords<-coordinates(kc)
  z<-pred[var]
  attr(z,"coords")<-coords
 z
}
#' @export
raster_gpred<-function(pred, var, resolution,cut_shape,sample){
  kc<-attr(pred,"k.c")
  raster_empty<-get_raster_template(kc,resolution,sample)
  coords<-coordinates(kc)
  z<-pred[var]
  attr(z,"coords")<-coords
  rz<-rst_num(raster_empty,z)
  base_shape<-attr(pred,"base_shape")
  rz<-cut_shape_fun(rz,cut_shape,base_shape)
  rz
}
#' @export
cut_shape_fun<-function(my_rst,cut_shape=F,base_shape){
  if(isTRUE(cut_shape)){
    if(!is.null(base_shape)) {
      my_rst<-raster::mask(my_rst, base_shape)}
  }
  my_rst
}
#' @export
cutted_to_numeric<-function(x,return=1){
  xlev<-levels(x)
  xlev<-gsub("\\[|\\)|\\]|\\(","",xlev)
  bb<-do.call(rbind,strsplit(xlev,","))
  c(bb[,1],bb[nrow(bb),2])
}
#' @export
gcv_df<-function(out){
  obs<-attr(out,"obs")
  result<-list()

  for(i in 1:ncol(obs)){

  }
  ME<-mean(out$residual)
  MSE<-mean(out$residual^2)
  MSNE<-mean(out$zscore^2)
  COP<-cor(out$observed, out$observed - out$residual)
  CPR<- cor(out$observed - out$residual, out$residual)
  TSS = sum((out$observed - mean(out$observed))^2)
  RSS = sum(out$residual^2)
  R2 = 1 - (RSS/TSS)
  df<-data.frame(ME,MSE,MSNE,COP,CPR,R2)
  df<-round(df,5)
  df
}
#' @export
gcv_df_col<-function(out, round=5){
  obs<-attr(out,"obs")
  result<-list()
  for(i in 1:ncol(obs)){
    residual<-out[,i]
    observed<-obs[,i]
    ME<-mean(residual)
    MSE<-mean(residual^2)
    #MSNE<-mean(out$zscore^2)
    COP<-cor(observed, observed - residual)
    CPR<- cor(observed - residual, residual)
    TSS = sum((observed - mean(observed))^2)
    RSS = sum(residual^2)
    R2 = 1 - (RSS/TSS)
    df<-data.frame(ME,MSE,COP,CPR,R2)
    df<-round(df,round)
    result[[i]] <-df
  }
  names(result)<-colnames(obs)
  result<-do.call(rbind,result)
  result
}
content = paste0("<p>Waiting time between ",
                 "eruptions and the duration of the eruption for the Old Faithful geyser ",
                 "in Yellowstone National Park, Wyoming, USA.</p><p>Azzalini, A. and ",
                 "Bowman, A. W. (1990). A look at some data on the Old Faithful geyser. ",
                 "Applied Statistics 39, 357-365.</p>")
#' @export
gcv_summary<-function(out){
 df<-gcv_df(out)
  labels<-paste0(
    p("ME: mean error, ideally 0"),
    p("ME: Mean square error, ideally small"),
    #p("MSNE:: Mean square normalized error, ideally close to 1"),
    p("COP: correlation observed and predicted, ideally 1"),
    p("CPR:correlation predicted and residual, ideally 0"),
    p("R²:R-squared")
  )
  labels
}
#' @export
help_autofit<-function(){
  popify(a(icon("fas fa-question-circle",style="color:  #3c8dbc")),"autofitVariogram {automap}",
         paste0(
           p(
             HTML(paste0("iMESc uses the function ",code('autofitVariogram')," from ",code('automap')," package to automatically fit a experimental variogram."))
           ),
           p("The initial sill is estimated as the mean of the max and the median of the semi-variance. The inital range is defined as 0.10 times the diagonal of the bounding box of the data. The initial nugget is defined as the min of the the semi-variance"),
           p("Users can also adjust the parameters manually (nugget, psill, range and kappa).")
         ),"right"

  )
}
#' @export
to_ordinal <- function(vec) {
  # Função auxiliar para determinar o sufixo correto
  get_suffix <- function(number) {
    if (number %% 100 %in% c(11, 12, 13)) {
      return("th")
    }
    switch(as.character(number %% 10),
           `1` = "st",
           `2` = "nd",
           `3` = "rd",
           "th")
  }

  # Aplicar a função auxiliar a cada elemento do vetor
  sapply(vec, function(x) paste0(x, get_suffix(x)))
}
pie_chart_help <- div(
  p("This function utilizes the ", code('geodist'), " package to calculate geodesic distances between each pair of points in your dataset."),
  p("Using the ", strong("buffer (km)"), " parameter, the function defines a circular buffer zone around each point with a radius equal to the specified distance. Points within this buffer are identified and considered for analysis, based on their proximity to each other."),
  p("The function then uses a selected ",strong("factor")," to categorize and aggregate data within each buffer zone. This aggregation is visually represented through pie charts on the map, where each pie chart corresponds to a buffer zone and displays the summarized data by factor levels."),
  p("Essentially, the resulting map showcases spatial pie charts, with each chart summarizing data within its respective buffer zone.")
)


get_data_rst<-function(rst){
  rater_points<-raster::rasterToPoints(rst)
  data<-data.frame(rater_points[,3])
  attr(data,"coords")<-data.frame(rater_points[,1:2])
  data
}
add_base_shape<-function(map,data,shape_attr="base_shape",color="blue",fillOpacity =1,stroke =T,weight =1, shape=T, fill=T,border_col=NULL,...){
  if(isTRUE(shape)){
    if(!is.null(attr(data,shape_attr))){
      base_shape<-st_as_sf(attr(data,shape_attr))[,"geometry"]
      if(!is.null(base_shape)){
        for(i in 1:nrow(base_shape)){
          geom<-sf::st_geometry_type(base_shape[i,])
          base_temp<-st_cast(base_shape[i,],"POLYGON")
          for(j in 1:nrow(base_temp)){
            co<-st_coordinates(base_temp[j,])[,1:2]
            map<-map |> leaflet::addPolygons(co[,1],co[,2],fill=fill,color=color,opacity=1,fillOpacity =fillOpacity,stroke=stroke,weight =weight,group="base_shape")
          }
        }


      }

    }}
  map
}








get_bathy_lines<-function(lines){

  lines_coords <- st_coordinates(lines)
  contour_ids <- lines$Contour

  # Combine the coordinates with their corresponding Contour ID
  # This assumes that 'lines_coords' and 'contour_ids' have corresponding rows
  # Extract the number of coordinates for each MULTILINESTRING
  coord_lengths <- sapply(st_geometry(lines), function(geom) sum(lengths(st_coordinates(geom)[, 1])))

  # Now, repeat the 'Contour' IDs according to the number of coordinates in each geometry
  contour_ids_expanded <- rep(contour_ids, coord_lengths)

  # Ensure the length matches
  if (length(contour_ids_expanded) != nrow(lines_coords)) {
    stop("The number of coordinates does not match the number of expanded Contour IDs.")
  }

  # Combine the coordinates with the corresponding Contour ID
  lines_coords <- cbind(lines_coords, Contour = contour_ids_expanded)

  # Proceed with the rest of your code
  lines_list <- lapply(unique(lines_coords[, "L1"]), function(line_id) {
    lapply(unique(lines_coords[lines_coords[, "L1"] == line_id, "L2"]), function(subline_id) {
      line_coords <- lines_coords[lines_coords[, "L1"] == line_id & lines_coords[, "L2"] == subline_id, c("X", "Y", "Contour")]
      df<-data.frame(line_coords)
      colnames(df)<-c("x","y","z")
      df
    })
  })
  batdf<-unlist(lines_list,recursive = F)
  batdf
}
rect_angle<-function(co){

  middle<-round(nrow(co)/2)
  x1<-min(co[,1])
  x2<-max(co[,1])
  y1<-min(co[,2])
  y2<-max(co[,2])
  dx <- x2 - x1
  dy <- y2 - y1

  # Calcula o ângulo em radianos
  angle_radians <- atan2(dy, dx)

  # Converte para graus
  angle_degrees <- angle_radians * (180 / pi)

  # xx<-max(co[,1])-min(co[,2])/2
  cbind(  data.frame(as.list(colMeans(co))),angle_degrees)
}





help_interp <- function(method){
  res<-switch(method,
         "krige" = "Kriging uses variograms to model the spatial correlation between data points (gstat package).",
         "idw" = "Inverse Distance Weighting (IDW) assigns weights inversely proportional to the distance between sample points and the interpolation point. The closer the sample points, the higher the weight assigned (gstat package).",
         "knn" = "K-Nearest Neighbors (KNN) estimates the value of an unknown point based on the values of the k nearest neighbors in feature space.",
         "svmRadial" = "The radial kernel is a common choice for SVM as it maps the data into a higher-dimensional feature space. It works by transforming the input data into a higher-dimensional space where it can find optimal decision boundaries to separate different classes or approximate nonlinear functions effectively",
         "gaussprRadial" = "Gaussian Process (GP) with a Radial Basis Function (RBF) kernel. This method models the underlying process as a Gaussian distribution over functions, where the RBF kernel controls the smoothness of the functions sampled from the Gaussian process. Smaller values of the kernel width parameter result in more wiggly functions, while larger values produce smoother functions. Gaussian processes are often used for interpolation and regression tasks where uncertainty estimation is important. ",
         "svmRadialCost" = "This is a radial SVM model where the cost parameter controls the penalty for SVM margin violations"
  )
  tiphelp(res)
}



