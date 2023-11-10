predcomb_table<-function(modelist,newdata, progress=T){
  res_compre<-lapply(modelist,newdata=newdata,function(x,newdata){
    trainingData<-x$trainingData
    trainingData<-trainingData[-which(colnames(trainingData)==".outcome")]
    res<-suppressWarnings(predict(x,newdata= newdata[which(colnames(newdata)%in%colnames(trainingData))]))
    res

  })
  res_compre<-data.frame(res_compre)
  rownames(res_compre)<-rownames(newdata)
  colnames(res_compre)<-names(modelist)
  attr(res_compre,"rownames")<-rownames(newdata)
  res_compre
}



get_metrics_ensemble<-function(modelist,pred,obs){




  if(modelist[[1]]$modelType=="Classification"){


    pred_tab<-data.frame(pred=factor(pred,levels(obs)),obs=obs)
    metric1<-caret::multiClassSummary(pred_tab, lev=levels(obs))
    metric<-metric1
  } else{


    metric_reg<-c(
      F1_Score=MLmetrics::F1_Score(pred,obs),
      FBeta_Score=MLmetrics::FBeta_Score(pred,obs),
      Gini=MLmetrics::Gini(pred,obs),
      MAE=MLmetrics::MAE(pred,obs),
      MAPE=MLmetrics::MAPE(pred,obs),
      MedianAE=MLmetrics::MedianAE(pred,obs),
      MedianAPE=MLmetrics::MedianAPE(pred,obs),
      MSE=MLmetrics::MSE(pred,obs),
      NormalizedGini=MLmetrics::NormalizedGini(pred,obs),
      Poisson_LogLoss=MLmetrics::Poisson_LogLoss(pred,obs),
      R2_Score=MLmetrics::R2_Score(pred,obs),
      RAE=MLmetrics::RAE(pred,obs),
      RMSE=MLmetrics::RMSE(pred,obs),
      RMSLE=MLmetrics::RMSLE(pred,obs),
      RMSPE=MLmetrics::RMSPE(pred,obs),
      RRSE=MLmetrics::RRSE(pred,obs)

    )

    metric<-metric_reg

  }
  metric
}




calculate_weights <- function(preds, true_labels) {
  if (nrow(preds) != length(true_labels)) {
    stop("O número de instâncias nas previsões e nos rótulos verdadeiros deve ser igual.")
  }

  num_classifiers <- ncol(preds)
  num_instances <- nrow(preds)

  # Inicializar pesos dos classificadores
  weights <- rep(1, num_classifiers)

  for (i in 1:num_instances) {
    # Calcula o número de previsões incorretas por instância
    incorrect_preds <- sum(preds[i, ] != true_labels[i])

    # Atualiza pesos para classificadores corretos
    correct_classifiers <- which(preds[i, ] == true_labels[i])
    if (length(correct_classifiers) > 0) {
      alpha_i <- incorrect_preds / num_classifiers
      weights[correct_classifiers] <- weights[correct_classifiers] + alpha_i
    }
  }

  # Normalizar os pesos para que a soma seja igual a um
  final_weights <- weights / sum(weights)
  names(final_weights) <- colnames(preds)

  return(final_weights)
}
matches_with <- function(df, match) {
  # Garante que a função funcione mesmo se o usuário não especifique nomes em match
  names(match) <- NULL

  # Aplica uma função em cada linha do dataframe
  rows_to_select <- apply(df, 1, function(row) {
    # Compara cada elemento da linha com o elemento correspondente em match
    all(row == match)
  })

  # Retorna apenas as linhas do dataframe que correspondem a todas as condições
  rows_to_select
}
get_preds_finalmodel<-function(modelist){
  bests<-lapply(modelist,function(m) m$bestTune)
  lapply(1:length(modelist),function(i){
    m<-modelist[[i]]
    tune_names<-colnames(m$pred)[ which(!colnames(m$pred)%in%c("pred","obs","rowIndex","Resample"))]
    be<-bests[[i]]
    pic<-matches_with(m$pred[,colnames(be), drop=F],unlist(be))
    m$pred[pic,]

  })
}
get_weighs_from_finalmodel_cv<-function(modelist){

  preds_cv_finalmodel<-get_preds_finalmodel(modelist)
  predtab2<-do.call(cbind,lapply(preds_cv_finalmodel,function(x) x[order(x$rowIndex),'pred']))
  colnames(predtab2)<-names(modelist)
  obstab2<-do.call(cbind,lapply(preds_cv_finalmodel,function(x) x[order(x$rowIndex),'obs']))
  calculate_weights(predtab2,obstab2[,1])
}

ensemble_find_new<-function(models_grid,modelist,weis,predtab,newdata,obc,  en_method,show_progress=T, pararell=F,session=MockShinySession$new(), weitype){
  models<-colnames(predtab)
  models_grid<-do.call("c", lapply(seq_along(models), function(i) combn(models, i, FUN = list)))
  rep=length(models_grid)
  picprogress<-function(n) paste0(round((n/rep),2)*100,"%")
  if(isTRUE(show_progress)){
    if(isTRUE(pararell)){
      progress <- shiny::Progress$new()
      progress$set(message = "Parallelizing the run...", value = 0)
      prog<-function(n) progress$set(message =  picprogress(n), value = (n/rep))
    } else{
      prog<-function(n) message(picprogress(n))
    }

  } else{
    prog<-function(n) message(picprogress(n))
  }
  opts <- list(progress=prog)
  if(isTRUE(pararell)){
    res<-foreach(i = 1:length(models_grid), .combine = data.frame, .packages ="shiny",.options.snow = opts) %do% {
      modl<-modelist[models_grid[[i]]]
      w<- if(weitype=="iv"){
        get_weighs_from_finalmodel_cv(modl)
      } else{
        weis[models_grid[[i]]]
      }
      get_ensemble_pred(
      predtab=predtab[,models_grid[[i]],drop=F],
      modelist=modelist[models_grid[[i]]],
      weis=w,
      newdata=newdata,weitype=NULL,obc=NULL,en_method=en_method)[,1]}
    if(isTRUE(show_progress)){
      progress$close()
    }
    res<-t(res)
  } else{
    withProgress(session=session,message="Running", max=rep,{
      res<-lapply(1:length(models_grid),function(i){
        prog(i)
        modl<-modelist[models_grid[[i]]]
        w<- if(weitype=="iv"){
          get_weighs_from_finalmodel_cv(modl)
        } else{
          weis[models_grid[[i]]]
        }
        pred<-get_ensemble_pred(
          predtab=predtab[,models_grid[[i]],drop=F],
          modelist=modelist[models_grid[[i]]],
          weis=w,
          newdata=newdata,weitype=NULL,obc=NULL,en_method=en_method)[,1]
        incProgress(1,message=paste0("Making predictions...",round(i/length(models_grid),2)*100,"%"), session=session)
        pred

      })

      res<- do.call(cbind,res)

    })

  }



  res


}

ensemble_find_probs_new<-function(models_grid,modelist,weis,predtab,  show_progress=T, pararell=F, session=MockShinySession$new(),weitype){

  rep=length(models_grid)
  picprogress<-function(n) paste0(round((n/rep),2)*100,"%")
  if(isTRUE(show_progress)){
    if(isTRUE(pararell)){
      progress <- shiny::Progress$new()
      progress$set(message = "Parallelizing the run...", value = 0)
      prog<-function(n) progress$set(message =  picprogress(n), value = (n/rep))
    } else{
      prog<-function(n) message(picprogress(n))
    }
  } else{
    prog<-function(n) message(picprogress(n))
  }

  opts <- list(progress=prog)
  if(isTRUE(pararell)){
    res<-foreach(i = 1:length(models_grid), .packages ="shiny",.options.snow = opts) %do% {
      modl<-modelist[models_grid[[i]]]
      w<- if(weitype=="iv"){
        get_weighs_from_finalmodel_cv(modl)
      } else{
        weis[models_grid[[i]]]
      }
      get_ensemble_pred(predtab[,models_grid[[i]],drop=F],modelist[models_grid[[i]]],weis=w, prob=T)
    }


    if(isTRUE(show_progress)){
      progress$close()
    }

  } else{
    withProgress(session=session,message="Gathering prob tables for AUCs...", max=rep,{
      i=1
      res<-lapply(1:length(models_grid),function(i){
        prog(i)
        modl<-modelist[models_grid[[i]]]

        w<- if(weitype=="iv"){
          get_weighs_from_finalmodel_cv(modl)
        } else{
          weis[models_grid[[i]]]
        }

        pred<-get_ensemble_pred(predtab[,models_grid[[i]],drop=F],modelist[models_grid[[i]]],weis=w, prob=T)
        incProgress(1,message=paste0("Gathering prob tables...",round(i/length(models_grid),2)*100,"%"), session=session)
        pred

      })

    })
  }

  res
}


