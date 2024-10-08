#' @export


fixed_dt_con<-list()
#' @export
fixed_dt_con$ui<-function(id,data,max_length=100, label="Show columns:"){
  if(is.null(data)){
    return(NULL)
  }
  vecs<-split_vector_max_elements(1:ncol(data),max_length)
  choices_containers<-sapply(vecs,function(x) paste(range(x),collapse="-"))
  choices_names<-names(choices_containers)
  names(choices_names)<-choices_containers
  ns<-NS(id)
  div(
    pickerInput_fromtop(ns("data_container"),label , choices_names)

  )
}
#' @export
fixed_dt_con$server<-function(id,data,max_length=100){
  moduleServer(id,function(input,output,session){

    if(is.null(data)){
      return(NULL)
    }
    vecs<-split_vector_max_elements(1:ncol(data),max_length)
    data_containers<-lapply(vecs,function(x) data[,x])
    output$data_render<-renderUI({
      div(
        class="half-drop-inline",
        style="max-width: 100%; overflow-x: auto",
        fixed_dt(data_containers[[input$data_container]],scrollY = "300px",scrollX=T)
      )
    })
  })
}
fixed_dt_con$server2<-function(id,data,max_length=100){
  moduleServer(id,function(input,output,session){

    if(is.null(data)){
      return(NULL)
    }
    vecs<-split_vector_max_elements(1:ncol(data),max_length)
    data_containers<-lapply(vecs,function(x) data[,x])

    if(!is.null(input$data_container))
      return(data_containers[[input$data_container]])
  })
}
split_vector_max_elements <- function(vec, max_length) {
  split(vec, ceiling(seq_along(vec) / max_length))
}
#' @export
databank_module<-list()
#' @export
databank_module$ui<-function(id){
  ns<-NS(id)
  choices<-c(paste0("tab",1:7))

  choices_names<-list(
    div(id = ns("tab1"), tipify(icon("fas fa-archive",style="color: #333;"),
                                "Numeric-Attribute")),
    div(id = ns("tab2"), icon("fas fa-boxes")),
    div(id = ns("tab3"), icon("fas fa-map-marker-alt")),
    div(id = ns("tab4"), icon("far fa-map")),
    div(id = ns("tab5"), icon("fas fa-braille")),
    div(id = ns("tab6"), img(src = sup_icon2, height = '17', width = '20')),
    div(id = ns("tab7"), icon("fas fa-comment"))
  )

  div(
    div(

      box_caret(ns("bank_tools"),
                title="Datalist Attributes",
                color="#374061ff",
                inline=F,
                div(class="bank_attr",
                    div(style="display: flex",

                        pickerInput_fromtop_live(ns("data_bank"), NULL, choices=NULL)
                        ,
                        radioGroupButtons(
                          ns("view_datalist"), NULL,
                          choiceNames = choices_names,

                          choiceValues = choices, status = "view_datalist"
                        )
                    )
                )),
      tags$style(HTML(
        "
    .icon_summary{
    padding-left: 5px;
 padding-right: 10px;
 padding-top: 10px;
    box-sizing: content-box;
    border: 0px solid;
    border-radius: 0px;

    font-size: 10px;
    color: #696969;
 display: flex;
align-items: flex-end;
 gap: 3px
    }
.text_summary{
color:RoyalBlue
}
    "
      )),

box_caret(ns("box_bank"),
          button_title2=div(uiOutput(ns('summary_attr'))
            ),
          title=inline(
            uiOutput(ns("selected_attr"))
          ),
          button_title = div(
            actionLink(ns("download_table"),"Download",icon("download")),
            actionLink(ns("download_plot"),"Download",icon("download")),
            downloadLink(ns('comments_downs'), label = "Download",icon("download"))
          ),


          div(
            class="half-drop-inline",
            div(
              style="margin-top: -10px;padding-left: 20px;padding-top: 5px",
              tabsetPanel(
                id=ns("tab_bank"),
                type="hidden",
                header=uiOutput(ns("summary")),
                tabPanel(
                  'tab1',
                  div(
                    style = " background: white;",
                    div(style="font-size: 11px; display: none",
                        id=ns("split_columns"),
                        render_warning(
                          title=NULL,point_icon=F,icon=NULL,
                          fluidRow(
                            column(12,

                                   column(4,class="mp0",
                                          div("The selected 'Datalist' has many columns, which may slow rendering. Columns were splitted based on the 'Max Nº of Columns'."),

                                   ),
                                   column(8,class="mp0",
                                          class="half-drop picker25",
                                          div(style="display: flex;
                                    flex-wrap:wrap; margin-left: 15px",
                                    numericInput(ns('col_interval'),"Max Nº of Columns:",200),
                                    uiOutput(ns('split_data_out'))
                                          )

                                   )
                            )
                          )
                        )
                    ),

                    DT::dataTableOutput(ns("DT_data"))

                  )

                ),
                tabPanel('tab2',
                         div(style = "background: white;",

                             DT::dataTableOutput(ns("DT_factors"))

                         )),
                tabPanel('tab3',
                         div(style = " background: white;",
                             uiOutput(ns("viewcoords")))
                ),
                tabPanel('tab4',uiOutput(ns("viewshapes"))),
                tabPanel('tab5',uiOutput(ns("viewsom"))),
                tabPanel('tab6',div(
                  tabsetPanel(
                    id=ns("sl_type"),
                    header=div(class='inline_pickers',
                               numericInput(ns("round_sl"),"Round",3)),
                    tabPanel("Classification Models",value="tab1",
                             div(style="overflow: auto",
                                 uiOutput(ns("viewsl_class"))
                             )


                    ),
                    tabPanel("Regression Models",value="tab2",
                             div(style="overflow: auto",
                                 uiOutput(ns("viewsl_reg")))

                    )
                  )
                )),
                tabPanel('tab7',uiOutput(ns("comments")))
              )

            )
          ))

    )
  )
}

#' @export
databank_module$server<-function(id, vals){
  moduleServer(id,function(input, output, session) {



    ns<-session$ns
    available_models<-SL_models$models




    output$summary_attr<-renderUI({
      req(get_download_data())
      div(

        id=ns("summary_attr"),
        style="display: flex;gap: 1px; align-items: flex-end",
        div(class='icon_summary',

            span('rows:'),
            uiOutput(ns("nrows")),
        ),
        div(class='icon_summary',

            span("columns:"),
            uiOutput(ns("ncols")),
        ),
        div(class='icon_summary',

            span('NAs:'),
            uiOutput(ns("nas")),
        )


      )
    })


    output$summary<-renderUI({

      #basic_summary(get_download_data())

    })

    observe({
      shinyjs::toggle('summary_attr',condition=input$view_datalist%in%c("tab1",'tab2',"tab3"))
    })

    output$nrows<-renderUI({
      div(
        class='text_summary',
        nrow(get_download_data())
      )
    })
    output$ncols<-renderUI({
      div(
        class='text_summary',
        ncol(get_download_data())
      )
    })
    output$nas<-renderUI({
      div(
        class='text_summary',
        sum(is.na(get_download_data()))
      )
    })

    get_title_attr<-reactive({
      switch(input$view_datalist,
             "tab1"={"Numeric-Attribute"},
             "tab2"={"Factor-Attribute"},
             "tab3"={"Coords-Attribute"} ,
             "tab4"={"Shapes-Attribute"},
             "tab5"={"SOM-Attribute"} ,
             "tab6"={"Supervised Models"},
             "tab7"={"Comments"} )
    })

    observe({
      shinyjs::toggle('download_table',condition=input$view_datalist%in%c("tab1","tab2","tab3","tab5","tab6"))
      shinyjs::toggle('download_plot',condition=input$view_datalist%in%c("tab4"))
      shinyjs::toggle('comments_downs',condition=input$view_datalist%in%c("tab7"))

    })

    summary_som<-reactive({
      mlist<-attr(getdata_bank(), "som")
      res<-lapply(names(mlist),function(i){
        m=mlist[[i]]
        supersom_summaries3(m, i)
      })
      res2<-data.frame(res)
      colnames(res2)<-NULL
      res2
    })
    get_download_data<-reactive({
      switch(input$view_datalist,
             "tab1"={getdata_bank()},
             "tab2"={attr(getdata_bank(),"factors")},
             "tab3"={attr(getdata_bank(),"coords")},
             "tab4"={NULL},
             "tab5"={summary_som()},
             "tab6"={
               if(input$sl_type=='tab1'){
                 get_metrics()$class
               } else{
                 get_metrics()$reg
               }
             },
             "tab7"={NULL}
      )
    })
    get_name_download<-reactive({
      name<-paste0(input$data_bank,"-",get_title_attr())
      if(input$view_datalist=="tab6"){
        if(input$sl_type=='tab1'){
          name<-paste0(input$data_bank,"-classification-summary")
        } else{
          name<-paste0(input$data_bank,"-regression-summary")
        }
      }
      name
    })
    observeEvent(ignoreInit = T,input$download_table,{
      vals$hand_down<-"generic"
      module_ui_downcenter("downcenter")
      data<-get_download_data()
      name<-get_name_download()
      mod_downcenter <- callModule(module_server_downcenter, "downcenter",  vals=vals, data=data, name=name)
    })
    observeEvent(ignoreInit = T,input$download_plot,{
      vals$hand_plot<-"generic_gg"
      generic=plot_shape()
      message<-name_c<-"Shape"
      module_ui_figs("downfigs")
      mod_downcenter<-callModule(module_server_figs, "downfigs",  vals=vals,message=message, name_c=name_c,generic=generic)
    })

    output$comments_downs<-{
      downloadHandler(
        filename = function() {
          paste(input$data_bank, "_comment", Sys.Date(), ".txt", sep = "")

        },
        content = function(file) {
          res<-attr(getdata_bank(), "notes")
          res<-gsub("<br/>", "\n", res)
          writeLines(res, file, sep = '\n')
          beep(10)
        }
      )
    }

    output$selected_attr<-renderUI({
      get_title_attr()

    })











    observeEvent(input$view_datalist,{
      vals$cur_view_datalist<-input$view_datalist
    })




    update_imesc_models<-reactive({
      req(input$data_bank)
      data_x<-input$data_bank

      saved_models<-lapply(as.character(available_models),function(model){
        attr(vals$saved_data[[data_x]],model)
      })
      models_in<-available_models[sapply(saved_models,length)>0]
      if(length(models_in)>0){
        saved_models<-saved_models[sapply(saved_models,length)>0]
        names(saved_models)<-models_in
        for(i in seq_along(saved_models)){
          for(j in names(saved_models[[i]])){
            if(inherits(saved_models[[i]][[j]],"train")){
              saved_models[[i]][[j]]<-list(m= saved_models[[i]][[j]])
            }

          }
        }
        for(i in seq_along(saved_models)) {
          attr(vals$saved_data[[data_x]],names(saved_models)[i])<-saved_models[[i]]
        }

      }

      return(NULL)
    })



    get_metrics<-reactive({
      update_imesc_models()
      get_datalist_model_metrics(vals$saved_data,input$data_bank)
    })

    render_metrics<-function(table,round){


      ncol_base<-max(attr(table,"ncol_train"))
      container<-container_global_caret(table)
      pic<-which(unlist(lapply(table,function(x) is.numeric(x))))
      table[,pic]<-round(table[,pic],round)
      table
      tables<-DT::formatStyle(
        DT::formatStyle(
          DT::datatable(as.matrix(table),
                        escape=F,
                        rownames=T,
                        container =container,
                        options=list(lengthMenu = list(c(-1), c("All")),

                                     info=FALSE,autoWidth=F,dom = 't')),
          c(1), `border-left` = "solid 1px"
        ),       c(ncol_base,ncol(table)), `border-right` = "solid 1px"
      )
      div(class="half-drop-inline",
          DT::renderDataTable(tables))


    }
    output$viewsl_class<-renderUI({
      validate(need(get_metrics()$class,"No classification model found"))
      div(
        render_metrics(get_metrics()$class,input$round_sl)
      )
    })

    output$viewsl_reg<-renderUI({
      validate(need(get_metrics()$reg,"No Regression model found"))
      div(
        render_metrics(get_metrics()$reg,input$round_sl)
      )
    })

    observeEvent(input$view_datalist,{
      updateTabsetPanel(session,"tab_bank",selected=input$view_datalist)
    })


    observeEvent(vals$saved_data,{
      choices=names(vals$saved_data)
      selected=vals$cur_data
      selected=get_selected_from_choices(selected,choices)
      updatePickerInput(session,'data_bank',choices=choices,selected=selected)
    })




    getdata_bank<-reactive({
      req(length(vals$saved_data)>0)
      req(input$data_bank)
      vals$saved_data[[input$data_bank]]
    })




    output$bank_input1<-renderUI({
      div()
    })


    output$viewshapes<-renderUI({
      choices<-layers_choices()
      div(
        span(
          inline(
            pickerInput(ns("pick_elayers"), "Shapes:",  choices, width = "250px")
          ),
          actionButton(ns("delete_elayer_shape"), icon("fas fa-trash-alt"))
        ),
        uiOutput(ns('viewbase')),
        uiOutput(ns('viewlayer')),
        uiOutput(ns("elayers")),
        plotOutput(ns("shape_plot"))
      )
    })

    plot_shape<-reactive({
      if(input$pick_elayers%in%c('base_shape','layer_shape')){
        shape<-attr(getdata_bank(), input$pick_elayers)
      } else{
        shape<-attr(getdata_bank(), "extra_shape")[[input$pick_elayers]]
      }
      req(shape)
      ggplot(st_as_sf(shape)) + geom_sf() +
        theme(panel.background = element_rect(fill = "white"),
              panel.border = element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"))

    })

    output$shape_plot<-renderPlot({
      plot_shape()
    })

    output$ddcogs3<-renderUI({
      div(class = "ddlinks",
          actionLink(ns("ddcogs3"), "Edit changes")
      )
    })
    output$viewsom<-renderUI({
      validate(need(length(attr(getdata_bank(), "som")) > 0,"No SOM model found"))
      div(
        uiOutput(ns("supersom_summary"))
      )
    })

    output$supersom_summary<-renderUI({
      mlist<-attr(getdata_bank(), "som")
      res_som<-lapply(names(mlist),function(i){
        m=mlist[[i]]
        supersom_summaries2(m, i)
      })


      do.call(splitLayout, res_som)
    })

    output$comments<-renderUI({
      div(
        h4(strong("Comments"),
           inline(uiOutput(ns('comments_edit'))),
           inline(uiOutput(ns('comments_remove'))),
           inline(uiOutput(ns('comments_down')))
        ),
        div(uiOutput(ns('comment_data')))
      )
    })

    output$comments_remove<-renderUI({
      div(
        popify(bsButton(ns('comments_remove'), icon("fas fa-trash"), style = "button_active"), NULL, "Delete comment")
      )
    })
    output$comments_edit<-renderUI({
      actionButton(ns('comments_edit'), icon("fas fa-edit"))
    })

    output$viewcoords<-renderUI({

      if (is.null(attr(getdata_bank(), "coords"))) {
        div(

          div(
            div(strong("No coords found in Datalist:", style = "color: red"), em(input$data_bank))),
          uiOutput(ns("add_coords_intru")),
          div(style = "margin-top: 20px",
              splitLayout(
                cellWidths = c("30%", "20%"),
                column(12,
                       fileInput(inputId = ns("add_coords_file"), label = NULL)),
                column(12,
                       uiOutput(ns("add_coords_button"))
                )

              ))

        )
      } else {
        div(
          inline(
            DT::dataTableOutput(ns("DTcoords"))
          )
        )
      }

    })


    output$viewbase<-renderUI({
      req(input$pick_elayers)
      req(input$pick_elayers=="base_shape")

      column(12,

             if (is.null(attr(getdata_bank(), "base_shape"))) {
               fluidRow(

                 column(12,
                        p(strong("No base_shape found in Datalist:", style = "color: red"), em(input$data_bank))),
                 uiOutput(ns("add_base_intru")),
                 column(12, style = "margin-top: 20px",
                        splitLayout(
                          cellWidths = c("30%", "20%"),
                          column(12,
                                 fileInput(inputId = ns("add_base_file"), label = NULL)),
                          column(12,
                                 uiOutput(ns("add_base_button"))
                          )

                        ))
               )
             }

      )
    })



    output$viewlayer<-renderUI({

      req(input$pick_elayers)
      req(input$pick_elayers=="layer_shape")
      div(
        if (is.null(attr(getdata_bank(), "layer_shape"))) {
          fluidRow(

            column(12,
                   p(strong("No layer_shape found in Datalist:", style = "color: red"), em(input$data_bank))),
            uiOutput(ns("add_layer_intru")),
            column(12, style = "margin-top: 20px",
                   splitLayout(
                     cellWidths = c("30%", "20%"),
                     column(12,
                            fileInput(inputId = ns("add_layer_file"), label = NULL)),
                     column(12,
                            uiOutput(ns("add_layer_button"))
                     )

                   ))

          )
        }


      )
    })


    output$comment_data<-renderUI({
      HTML(attr(getdata_bank(), "notes"))
    })

    output$ddcogs1_pick<-renderUI({
      pickerInput(ns("ddcogs1_pick"), div("Select a variable"), choices = colnames(getdata_bank()), selected = vals$ddcogs1_pick, width = "200px")
    })
    output$dfcogs1_pick<-renderUI({
      pickerInput(ns("dfcogs1_pick"), div("Select a factor"), choices = colnames(attr(getdata_bank(), "factors")), selected = vals$dfcogs1_pick, width = "200px")
    })
    output$ddcogs3_pick<-renderUI({
      span(strong("Datalist:"), em(input$data_bank, style = 'color: SeaGreen'))
    })
    output$ddcogs3_edit<-renderUI({
      pickerInput(ns("ddcogs3_edit"), div("Edit Changes:"), colnames(attr(getdata_bank(), "transf")), width = "200px", selected = vals$ddcogs3_edit)
    })
    output$ddcogs1_targ<-renderUI({
      input$ddcogs1_pick
    })



    output$add_coords_button<-renderUI({
      req(length(input$add_coords_file$datapath) > 0)
      actionButton(ns("add_coords"), icon = icon("fas fa-arrow-right"),
                   strong(tipify(icon("fas fa-warehouse"), "Click to save to the Datalist"),
                          "Add", style = "button_active"))
    })
    output$add_coords_intru<-renderUI({
      div(
        div( style = "margin-top: 20px",
             strong("1. Use the button below to upload one.")),
        if (length(input$add_coords_file$datapath) > 0) {
          div(
            div( style = "margin-top: 20px",
                 strong("2. Click ", strong("Add", style = "color: SeaGreen"), " to insert it to the Datalist:", em(input$data_bank, style = "color: SeaGreen"))),
            div(
              uiOutput(ns("error_coords")))
          )
        }
      )
    })
    output$add_base_button<-renderUI({
      req(length(input$add_base_file$datapath) > 0)
      actionButton(ns("add_base"), icon = icon("fas fa-arrow-right"),
                   strong(tipify(icon("fas fa-warehouse"), "Click to save to the Datalist"),
                          "Add", style = "button_active"))
    })
    output$add_base_intru<-renderUI({
      column(12,
             column(12, style = "margin-top: 20px",
                    strong("1. Use the button below to upload one.")),
             if (length(input$add_base_file$datapath) > 0) {
               column(12, style = "margin-top: 20px",
                      strong("2. Click ", strong("Add", style = "color: SeaGreen"), " to insert it to the Datalist:", em(input$data_bank, style = "color: SeaGreen")))
             }
      )
    })
    output$add_layer_button<-renderUI({
      req(length(input$add_layer_file$datapath) > 0)
      actionButton(ns("add_layer"), icon = icon("fas fa-arrow-right"),
                   strong(tipify(icon("fas fa-warehouse"), "Click to save to the Datalist"),
                          "Add", style = "button_active"))
    })
    output$add_layer_intru<-renderUI({
      column(12,
             column(12, style = "margin-top: 20px",
                    strong("1. Use the button below to upload one.")),
             if (length(input$add_layer_file$datapath) > 0) {
               column(12, style = "margin-top: 20px",
                      strong("2. Click ", strong("Add", style = "color: SeaGreen"), " to insert it to the Datalist:", em(input$data_bank, style = "color: SeaGreen")))
             }
      )
    })

    observeEvent(getdata_bank(),{
      data<-getdata_bank()
      shinyjs::toggle("split_columns",condition=ncol(data)>300)
    })
    output$split_data_out<-renderUI({
      data<-getdata_bank()

      req(input$col_interval)
      div(class="picker25",
          fixed_dt_con$ui(ns("numeric"),data,input$col_interval,label="Show Group:")
      )

    })

    get_data_split<-reactive({
      req(input$col_interval)
      fixed_dt_con$server2("numeric",getdata_bank(),input$col_interval)
    })

    output$DT_data<-{

      DT::renderDataTable({
        data<-getdata_bank()

        req(data)
        req(is.data.frame(data))
        if(ncol(data)>input$col_interval){
          data<-get_data_split()
        }
        data},
        extensions = c('FixedColumns',"FixedHeader"),
        options = list(
          pageLength = 15,
          info = FALSE,
          lengthMenu = list(c(15, -1), c( "15","All")),
          autoWidth=F,
          scrollX = TRUE,
          scrollY = '280px',
          fixedColumns = list(leftColumns = 1, rightColumns = 0)),
        rownames = TRUE,
        class ='cell-border compact stripe',
        editable=T)}
    observeEvent(ignoreInit = T,input$DT_data_cell_edit, {
      row <-input$DT_data_cell_edit$row
      clmn<-input$DT_data_cell_edit$col
      value<-if(input$DT_data_cell_edit$value==""){NA}else{as.numeric(input$DT_data_cell_edit$value)}
      if(ncol(vals$saved_data[[input$data_bank]])>input$col_interval){
        vals$saved_data[[input$data_bank]][colnames(get_data_split())][row, clmn]<-value
      } else{         vals$saved_data[[input$data_bank]][row, clmn]<-value
      }
    })

    output$DT_factors<-{

      DT::renderDataTable({
        req(getdata_bank())
        data<-attr(getdata_bank(),"factors")
        validate(need(ncol(data) < 1000, "Preview not available for data with more than 1000 columns"))
        data},
        extensions = c('FixedColumns',"FixedHeader"),
        options = list(
          pageLength = 15,
          info = FALSE,
          lengthMenu = list(c(15, -1), c( "15","All")),
          autoWidth=F,
          scrollX = TRUE,
          scrollY = '280px',
          fixedColumns = list(leftColumns = 1, rightColumns = 0)),
        rownames = TRUE,
        class ='cell-border compact stripe',
        editable=T)}
    output$DTcoords<-{DT::renderDataTable(data.frame(attr(getdata_bank(),"coords")),options = list(
      pageLength = 15, info = FALSE,lengthMenu = list(c(15, -1), c( "15","All")), autoWidth=T,dom = 'lt',scrollX = TRUE, scrollY = "280px"), rownames = TRUE,class ='cell-border compact stripe')}


    removechange<-reactive({

      modalDialog(size="m",easyClose = T,footer =NULL,
                  div(
                    inline(uiOutput(ns('ddcogs3_pick'))),
                    span(
                      inline(uiOutput(ns('ddcogs3_edit'))),
                      bsButton(ns("ddcogs1_rename_remove"),icon(verify_fa = FALSE,name=NULL,class="fas fa-trash-alt")), modalButton("Dismiss")
                    ),
                    div(
                      renderPrint(attr(getdata_bank(),"transf"))
                    )
                  )
      )
    })
    bag_name_new<-reactive({
      bag<-1
      name0<-input$data_bank
      name1<-paste(name0,bag)
      if(name1%in%names(vals$saved_data))
      {
        repeat{
          bag<-bag+1
          name1<-paste(name0,bag)
          if(!name1%in%names(vals$saved_data)) break
        }
      }
      paste(name0,bag)

    })
    getdown<-reactive({
      switch(vals$hand_down,
             "data"=getdata_bank(),
             "factors"=attr(getdata_bank(),"factors"),
             "coords"=attr(getdata_bank(),"coords"),

             "som"=combsom_down()
      )
    })

    combsom<-reactive({
      soms<-attr(getdata_bank(),"som")
      req(length(soms)>0)
      combsom<-do.call("cbind",lapply(soms,train.summary_fun))
      colnames(combsom)<-names(soms)
      combsom
    })
    combsom_down<-reactive({
      combsom_down<-do.call("cbind",lapply(attr(getdata_bank(),"som"),train.summary_fun))
      colnames(combsom_down)<-names(attr(getdata_bank(),"som"))
      data.frame(combsom_down)
    })
    layers_choices<-reactive({
      base_shape<-attr(getdata_bank(),"base_shape")
      layer_shape<-attr(getdata_bank(),"layer_shape")
      eshape<-attr(getdata_bank(),"extra_shape")

      pic<-which(unlist(lapply(list(base_shape,layer_shape),function(x)length(x)>0)))
      choices=c(c("Base Shape"="base_shape","Layer Shape"="layer_shape"),names(eshape))
      choices
    })


    observeEvent(ignoreInit = T,input$delete_coords,{
      showModal(
        modalDialog(
          title="Are you sure?",
          column(12, style="margin-top: 20px; margin-bottom: 20px",
                 h5(strong("Remove coords from",style="color: red"),span("Datalist:"),em(input$data_bank,style="color: gray")),
                 bsButton(ns("delete_coords_yes"),icon=icon(verify_fa = FALSE,name=NULL,class="far fa-trash-alt"),"Confirm", style="button_active",block=T))
          ,
          easyClose = T,
          size = "s"

        )
      )
    })




    observeEvent(input$data_bank,{

      choices0=choices<-c(paste0("tab",1:7))

      choices_names<-list(
        div(id = ns("tab1"), icon("fas fa-archive"),cogs_title="Numeric-Attribute"),
        div(id = ns("tab2"), icon("fas fa-boxes"),cogs_title="Factor-Attribute"),
        div(id = ns("tab3"), icon("fas fa-map-marker-alt"),cogs_title="Coords-Attribute"),
        div(id = ns("tab4"), icon("far fa-map"),cogs_title="Shapes-Attribute"),
        div(id = ns("tab5"), icon("fas fa-braille"),cogs_title="SOM-Attribute"),
        div(id = ns("tab6"), img(src = sup_icon2, height = '17', width = '20'),cogs_title="Supervised Models"),
        div(id = ns("tab7"), icon("fas fa-comment"),cogs_title="Comments")
      )

      somyes<-length(attr(getdata_bank(), "som")) > 0
      supyes<-any(sapply(get_metrics(),length)>0)

      if(isFALSE(somyes)){
        rem<-which(choices=='tab5')
        choices<-choices[-rem]
        choices_names[rem]<-NULL
      }
      if(isFALSE(supyes)){
        rem<-which(choices=='tab6')
        choices<-choices[-rem]
        choices_names[rem]<-NULL
      }


      selected=get_selected_from_choices(vals$cur_view_datalist,choices)

      updateRadioGroupButtons(session,'view_datalist',choiceValues =choices,choiceNames =choices_names,selected=selected)
      shinyBS::addPopover(session,'tab1', NULL,"Numeric-Attribute")
      shinyBS::addPopover(session,'tab2', NULL,"Factor-Attribute")
      shinyBS::addPopover(session,'tab3', NULL,"Coords-Attribute")
      shinyBS::addPopover(session,'tab4', NULL,"Shapes-Attribute")
      shinyBS::addPopover(session,'tab5', NULL,"SOM-Attribute")
      shinyBS::addPopover(session,'tab6', NULL,"Supervised Models")
      shinyBS::addPopover(session,'tab7', NULL,"Comments")

    })
    observeEvent(ignoreInit = T,input$delete_coords_yes,{
      attr(vals$saved_data[[input$data_bank]],"coords")<-NULL
      removeModal()
      updateRadioGroupButtons(session,"view_datalist", selected="coords")


    })
    observeEvent(ignoreInit = T,input$delete_som,{
      showModal(
        modalDialog(
          title="Are you sure?",
          column(12, style="margin-top: 20px; margin-bottom: 20px",
                 h5(strong("Remove som",style="color: red"),em(paste0(input$remove_som,collapse="; "),style="color: gray")),
                 bsButton(ns("delete_som_yes"),icon=icon(verify_fa = FALSE,name=NULL,class="far fa-trash-alt"),"Confirm", style="button_active",block=T)),
          easyClose = T,
          size = "s"

        )
      )
    })
    observeEvent(ignoreInit = T,input$delete_som_yes,{

      attr(vals$saved_data[[input$data_bank]],"som")[input$remove_som]<-NULL
      removeModal()

    })
    observeEvent(ignoreInit = T,input$add_base,{

      t<-try({get(gsub(" ", "", capture.output(
        load(input$add_base_file$datapath, verbose = T)
      )[2]))})
      if("try-error" %in% class(t)) {t<-readRDS(input$add_base_file$datapath) }

      attr(vals$saved_data[[input$data_bank]],"base_shape")<-t
      updateRadioGroupButtons(
        session,"view_datalist", selected="extra_shape"
      )

    })
    observeEvent(ignoreInit = T,input$add_layer,{

      t<-try({get(gsub(" ", "", capture.output(
        load(input$add_layer_file$datapath, verbose = T)
      )[2]))})
      if("try-error" %in% class(t)) {t<-readRDS(input$add_layer_file$datapath) }
      attr(vals$saved_data[[input$data_bank]],"layer_shape")<-t

      updateRadioGroupButtons(
        session,"view_datalist", selected="extra_shape"
      )
      t
    })
    observeEvent(ignoreInit = T,input$add_coords_file,{
      output$error_coords<-renderUI(NULL)
    })
    observeEvent(ignoreInit = T,input$add_coords,{

      coords<-data.frame(fread(input$add_coords_file$datapath))
      rownames(coords)<-coords[, 1]
      coords[, 1]<-NULL
      if(ncol(coords)!=2){ output$error_coords<-
        renderUI({
          div(strong(
            "Invalid Entries. The first column must contain the name of the observations. The second and third columns must contain the logitude and latitude respectively", style="color: red"))
        })}

      if(any(rownames(coords)%in%rownames(getdata_bank()))==F) {
        output$error_coords<-
          renderUI({
            div(strong(
              "None of the IDs of the banked coordinates are compatible with the ids of the selected datalist. Please bank coodinates with valid IDs", style="color: red"))
          })
      }

      req(any(rownames(coords)%in%rownames(getdata_bank())))

      attr(vals$saved_data[[input$data_bank]],"coords")<-na.omit(
        coords[rownames(getdata_bank()),]
      )
      updateRadioGroupButtons(
        session,"view_datalist", selected="coords"
      )

    })
    observeEvent(ignoreInit = T,input$DT_factors_cell_edit, {
      factors<-attr(getdata_bank(),"factors")
      row <-input$DT_factors_cell_edit$row
      clmn<-input$DT_factors_cell_edit$col
      value<-input$DT_factors_cell_edit$value
      if(!value%in%levels(factors[,clmn])){
        levels(factors[,clmn])<-c(levels(factors[,clmn]), value)
        factors[row, clmn]<-value
        attr(vals$saved_data[[input$data_bank]],"factors")<-factors

      } else{
        attr(vals$saved_data[[input$data_bank]],"factors")[row, clmn]<-input$DT_factors_cell_edit$value
      }

    })
    observeEvent(ignoreInit = T,input$delete_elayer_shape,{
      req(input$pick_elayers=="base_shape")
      attr(vals$saved_data[[input$data_bank]],"base_shape")<-NULL
      removeModal()
      updateRadioGroupButtons(session,"view_datalist", selected="extra_shape")
    })

    observeEvent(ignoreInit = T,input$trash_som ,{
      showModal(
        modalDialog(easyClose = T,
                    title=strong("Remove Som-Attribute"),
                    div(
                      div(
                        div(style="overflow-y: scroll;height: 200px;overflow-x: scroll; padding-top: 10px",
                            checkboxGroupInput(ns("remove_som"), NULL, choices=names(attr(getdata_bank(),"som")))
                        )
                      ),
                      bsButton(ns("delete_som"),icon=icon(verify_fa = FALSE,name=NULL,class="far fa-trash-alt"),"Delete", style="button_active", block=T, value=F)
                    )
        )
      )

    })
    observeEvent(ignoreInit = T,input$delete_elayer_shape,{
      req(input$pick_elayers=="layer_shape")
      attr(vals$saved_data[[input$data_bank]],"layer_shape")<-NULL
      removeModal()
      updateRadioGroupButtons(session,"view_datalist", selected="extra_shape")
    })







    observeEvent(ignoreInit = T,input$data_bank,{
      vals$cur_data<-input$data_bank
    })

    observeEvent(ignoreInit = T,input$view_datalist,
                 {vals$curview_databank<-input$view_datalist})

    observeEvent(ignoreInit = T,input$ddcogs1_pick,{
      vals$ddcogs1_pick<-input$ddcogs1_pick
    })
    observeEvent(ignoreInit = T,input$dfcogs1_pick,{
      vals$dfcogs1_pick<-input$dfcogs1_pick
    })
    observeEvent(ignoreInit = T,input$ddcogs3_edit,{
      vals$ddcogs3_edit<-input$ddcogs3_edit
    })
    observeEvent(ignoreInit = T,input$ddcogs3,{

      showModal(
        removechange()
      )
    })
    observeEvent(ignoreInit = T,input$ddcogs1_rename_remove,{
      #pic<-colnames(attr(getdata_bank(),"transf"))==input$ddcogs3_edit
      attr(vals$saved_data[[input$data_bank]],"transf")<-data.frame(attr(vals$saved_data[[input$data_bank]],"transf"))
      attr(vals$saved_data[[input$data_bank]],"transf")[input$ddcogs3_edit]<-NULL
    })
    observeEvent(ignoreInit = T,input$ddcogs1,{
      showModal(
        modalDialog(size="m",easyClose = T,footer =NULL,
                    div(
                      inline(uiOutput(ns('ddcogs1_pick'))),
                      span(
                        inline(textInput(ns("ddcogs1_rename"), div("Rename Variable:",em(inline(uiOutput(ns('ddcogs1_targ'))), style="color: SeaGreen")),placeholder = "New name", width="200px")),
                        bsButton(ns("ddcogs1_rename_go"),"Rename"), modalButton("Dismiss")
                      )
                    )
        )
      )
    })
    observeEvent(ignoreInit = T,input$dfcogs1,{
      showModal(
        modalDialog(size="m",easyClose = T,footer =NULL,
                    div(
                      inline(uiOutput(ns('dfcogs1_pick'))),
                      span(
                        inline(textInput(ns("dfcogs1_rename"), div("Rename Factors:",em(inline(uiOutput(ns('dfcogs1_targ'))), style="color: SeaGreen")),placeholder = "New name", width="200px")),
                        bsButton(ns("dfcogs1_rename_go"),"Rename"), modalButton("Dismiss")
                      )
                    )
        )
      )
    })
    observeEvent(ignoreInit = T,input$ddcogs1_rename_go,{

      updateTextInput(session,'view_datalist', value="data")
      req(input$ddcogs1_rename!="")
      pic<- which( colnames(getdata_bank())==input$ddcogs1_pick)
      colnames(vals$saved_data[[input$data_bank]])[pic]<-input$ddcogs1_rename
    })
    observeEvent(ignoreInit = T,input$dfcogs1_rename_go,{
      req(input$dfcogs1_rename!="")
      pic<- which( colnames(attr(getdata_bank(),"factors"))==input$dfcogs1_pick)
      colnames(attr(vals$saved_data[[input$data_bank]],"factors"))[pic]<-input$dfcogs1_rename
    })
    observeEvent(ignoreInit = T,input$comments_remove,{
      attr(vals$saved_data[[input$data_bank]],"notes")<-NULL
    })
    observeEvent(ignoreInit = T,input$delete_elayer_shape,{
      req(input$pick_elayers!='base_shape')
      req(input$pick_elayers!='layer_shape')
      attr(vals$saved_data[[input$data_bank]],"extra_shape")[[input$pick_elayers]]<-NULL
      removeModal()
      updateRadioGroupButtons(session,"view_datalist", selected="extra_shape")
    })

    modal_comment<-reactive({
      modalDialog(
        title="Create a comment",
        easyClose = T,
        footer=div(modalButton("Cancel"), inline(uiOutput("confirm_note"))),
        div(
          uiOutput("note_target"),
          uiOutput("note_create")
        )
      )
    })

    observeEvent(ignoreInit = T,input$comments_edit,{
      showModal( modal_comment())
    })


    observe({
      req(vals$update_state)
      update_state<-vals$update_state
      ids<-names(update_state)
      update_on<-grepl(id,ids)
      names(update_on)<-ids
      to_loop<-names(which(update_on))
      withProgress(min=1,max=length(to_loop),message="Restoring",{
        for(i in to_loop) {
          idi<-gsub(paste0(id,"-"),"",i)
          incProgress(1)
          restored<-restoreInputs2(session, idi, update_state[[i]])

          if(isTRUE(restored)){
            vals$update_state[[i]]<-NULL
          }

        }
      })

    })

  })



}

