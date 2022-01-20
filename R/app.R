#' Calculate and Plot ICER
#'
#' The app will calculate the ICER from the mean costs and quality-adjusted
#' life years (QALY) for a set of treatment options, and draw the efficiency
#' frontier in the costs-effectiveness plane. The app automatically identifies
#' and excludes dominated and extended-dominated options from the ICER
#' calculation.
#' How to operate:
#' Step 1: Open the interface typing 'icertool()'
#' Step 2: Introduce mean costs and quality-adjusted life years for each
#' treatment option in the 'Data Input' tab
#' Step 3: Adjust (if necessary) the position of the text labels on the
#' graph in the 'Results' tab
#' Step 4: To copy an image of the graph to a word-processor document,
#' right-click with your mouse on the graph and select 'Copy image', then go
#' to your word-processor document and select 'Paste Special' and 'Bitmap image'
#'
#'
#' @export
#'
icertool <- function(){

requireNamespace("shiny")
requireNamespace("shinythemes")
requireNamespace("purrr")
requireNamespace("DT")
requireNamespace("tidyverse")
requireNamespace("readxl")
requireNamespace("ggplot2")
requireNamespace("ggrepel")
requireNamespace("digest")
requireNamespace("shinyhelper")



#data frame creation
data <- data.frame(
  treat = character(), #treatment name
  q = as.numeric(),     #treatment qalys
  c = as.numeric()      #treatment costs
)

data.0 <- data #save old data

#define the name of the treatment, the cost, and the qalys

ui <- shiny::fluidPage(theme=shinythemes::shinytheme("simplex"),
                       shiny::navbarPage("ICER tool",
  shiny::tabPanel( shiny::icon("fas fa-igloo"),
                  shiny::fluidRow(shiny::column(width=2),
                                  shiny::column(
                                    shiny::p("Welcome to the incremental cost-effectiveness ratio (ICER) graphics tool.",style="color:black;text-align:center"),
                      width=8,style="background-color:papayawhip;border-radius: 10px")
           ),
           shiny::hr(),
           shiny::p("The app will calculate the ICER from the mean costs and quality-adjusted life years (QALY) for a set of treatment options, and draw the efficiency frontier in the costs-effectiveness plane. The app automatically identifies and excludes dominated and extended-dominated options from the ICER calculation."),
           shiny::hr(),
           shiny::p("How to operate:"),
           shiny::tags$div(
             shiny::tags$ul(
               shiny::tags$li("Step 1: Introduce  mean costs and quality-adjusted life years for each treatment option in the 'Data Input' tab"),
               shiny::tags$li("Step 2: Adjust (if necessary) the position of the text labels on the graph in the 'Results' tab"),
               shiny::tags$li("Step 3: To copy an image of the graph to a word-processor document, right-click with your mouse on the graph and select 'Copy image', then go to your word-processor document and select 'Paste Special' and 'Bitmap image'")
             )
           ),
           shiny::hr(),
           shiny::HTML("<p>The app was developed in R by <a href='http://www.ugr.es/~davidepstein/'>David Epstein</a> and <a href='http://danielpereztr.es/'>Daniel Perez Troncoso</a> at the Department of Applied Economics at the University of Granada.
           Contact: <a href = 'mailto:davidepstein@ugr.es'>davidepstein@ugr.es</a></p>"),
           shiny::hr(),
           shiny::HTML("<p>Please, cite as <i>David Epstein, Daniel Perez Troncoso</i> (2021) ICER tool. <a href='https://ICERtool.shinyapps.io/ICERtool/'>https://ICERtool.shinyapps.io/ICERtool/</a>"),
           shiny::hr(),
           shiny::HTML("<p><b>Changelog</b><p>"),
           shiny::HTML("<ul>
                <li>22/03/2020 (v 0.3): 1) Now data can be imported from a .xlsx file. 2) An example dataset has been included. 3) Minor bugs fixed. </li>
                <li>22/03/2020 (V 0.2): 1) Now the theme of the chart can be changed in the 'Results' tab. 2) Minor bugs fixed.</li>
                <li>22/03/2020 (V 0.1): 1) Sliders have been introduced to modify the position of the labels on the chart. The range of the sliders is automatically calculated according to the data. 2) Minor bugs fixed.</li>
                <li>20/03/2020 (V 0.0): Release</li>
                </ul>")


  ),
  shiny::tabPanel("Data input",
  #sidebar for data input and outputs definition
  shiny::sidebarLayout(
    #inputs
    shiny::sidebarPanel(
      shiny::textInput("treat", "Name of the treatment", ""),
      shiny::numericInput("q", "QALYS", ""),
      shiny::numericInput("c", "Costs", ""),
      #add and remove button
      shiny::actionButton(inputId = "add.button", label = "Add", icon =  shiny::icon("plus")),
      shiny::actionButton(inputId = "delete.button", label = "Delete", icon =  shiny::icon("minus")),
      shiny::fileInput("excel", "Choose .xlsx file", accept = c(".xlsx")),
      shiny::actionButton(inputId ="load.button", label = "Load data", icon =  shiny::icon("upload")) |> shinyhelper::helper(type = "markdown", content = "upload_help"),
      shiny::hr(),
      shiny::actionButton(inputId = "example.button", label = "Load example data", icon = shiny::icon("database"))

    ), #end of the sidebarPanel

    shiny::mainPanel(
    DT::DTOutput('table')

    ) #mainPanel end
  ) #sidebarLayout end
  ),
  shiny::tabPanel("Results",
                  shiny::sidebarPanel(
                    shiny::h5("Adjust the position of the treatment option labels"),
                    shiny::sliderInput("rry", min=-5, max = +5, value = 0, label="Label 'y' axis", step=0.05),
                    shiny::sliderInput("rrx", min=-5, max = +5, value = 0, label= "Label 'x' axis", step=0.05),
                    shiny::h5("Adjust the position of the ICER"),
                    shiny::sliderInput("ryo", min=-5, max = +5, value = 0, label="Number 'y' axis", step=0.05),
                    shiny::sliderInput("rxo", min=-5, max = +5, value = 0, label="Number 'x' axis", step=0.05),
                    shiny::h5("Graphical settings"),
                    shiny::selectInput("fontsize", "Font size:",
                  c("3" = "3",
                    "4" = "4",
                    "5" = "5",
                    "6" = "6",
                    "7" = "7",
                    "8" = "8",
                    "9" = "9",
                    "10" = "10")),
                  shiny::textInput("xtitle", "x-axis title", "Mean QALY"),
                  shiny::textInput("ytitle", "y-axis title", "Mean Cost"),
                  shiny::selectInput("titlesize", "Title size:",
                 c("9" = "9",
                   "10" = "10",
                   "11" = "11",
                   "12" = "12",
                   "13" = "13",
                   "14" = "14",
                   "15" = "15",
                   "16" = "16")),
                 shiny::selectInput("graphtheme", "Graph theme:",
                 c("Gray" = "theme_gray",
                   "B/W" = "theme_bw",
                   "LineDraw" = "theme_linedraw",
                   "Light" = "theme_light",
                   "Dark" = "theme_dark",
                   "Minimal" = "theme_minimal",
                   "Classic" = "theme_classic",
                   "Void" = "theme_void",
                   "Test" = "theme_test"))

    ),
    shiny::mainPanel(
      shiny::h4("Options on the efficiency frontier"),
      DT::dataTableOutput('resultable'),
      shiny::h4("Options and efficiency frontier"),
      shiny::plotOutput(outputId = "plane")
    )
  )
)
)
 #ui end

server <- function(input, output, session) {

  #table
  values <- shiny::reactiveValues()
  values$df <- data

  shiny::observeEvent(input$add.button,{
    cat("add Entry\n")
    print(input$treatment)
    print(input$costs)
    print(input$qalys)
    newRow <- data.frame(input$treat, input$q, input$c)
    colnames(newRow)<-c("treat", "q", "c")
    values$df <- rbind(values$df,newRow)
    data.0 <- rbind(data.0,newRow)
    print(nrow(values$df))
  })

  shiny::observeEvent(input$load.button,{
    inFile <- input$excel
    if (!is.null(inFile)){
      exc <- readxl::read_excel(inFile$datapath, sheet=1)
      colnames(exc) <- c("treat", "q", "c")
      values$df <- rbind(values$df, exc)
    }
  })

  shiny::observeEvent(input$delete.button,{
    cat("deleteEntry\n")
    values$df <- values$df[-nrow(values$df), ]
    tablex <- values$df
  })


  output$table = DT::renderDataTable({
    tablex=as.data.frame(values$df)
    colnames(tablex) <- c("Treatment", "QALYs", "Costs")
    tablex
  })


  output$resultable = DT::renderDataTable({
    dom <- function(data) {
      #exclude dominated options
      data<- data[order(data$q,data$c),]
      N<-nrow(data)
      if (N<3) return(data.frame(data))
      else {
        icer<-(data$c[2:N]-data$c[1:(N-1)])/(data$q[2:N]-data$q[1:(N-1)])
        # first, exclude dominated options
        # exclude  option j if option j+1 has greater qaly and lower cost (negative ICER)
        select.1<- (icer >=0 )
        select.1<-c(select.1,TRUE)
        # exclude  option j if option j-1 has same qaly and greater cost (infinite ICER)
        select.2<-!(icer==Inf)
        select.2<-c(TRUE,select.2)
        select<-select.1 & select.2
        data<-data[select,]
        #the function will call itself recursively until all the dominated options are excluded
        if (N>nrow(data)) return(dom(data))
        else return(data.frame(data))
      }
    }

    myicer <- function(data) {
      #exclude dominated options
      data<-dom(data)
      # sort by q from smallest to greatest
      data<- data[order(data$q,data$c),]
      N<-nrow(data)
      icer<-(data$c[2:N]-data$c[1:(N-1)])/(data$q[2:N]-data$q[1:(N-1)])
      icer.final<-c(NA, icer)
      if (N<3) return(data.frame(data,icer=icer.final))
      else {
        #now calculate and exclude extended dominated options
        icer.diff<-icer[2:length(icer)]-icer[1:(length(icer)-1)]
        select.3<-c(TRUE,icer.diff>0,TRUE)
        data<-data[select.3,]
        #the function will call itself recursively until all the dominated options are excluded
        if (N>nrow(data)) return(myicer(data))
        else return(data.frame(data,icer=icer.final))
      }
    }


   data <- values$df
   tabler <- myicer(data)
   colnames(tabler) <- c("Treatment", "QALYs", "Costs", "ICER")
   tabler
  })

  output$plane <- shiny::renderPlot({

    dom <- function(data) {
      #exclude dominated options
      data<- data[order(data$q,data$c),]
      N<-nrow(data)
      if (N<3) return(data.frame(data))
      else {
        icer<-(data$c[2:N]-data$c[1:(N-1)])/(data$q[2:N]-data$q[1:(N-1)])
        # first, exclude dominated options
        # exclude  option j if option j+1 has greater qaly and lower cost (negative ICER)
        select.1<- (icer >=0 )
        select.1<-c(select.1,TRUE)
        # exclude  option j if option j-1 has same qaly and greater cost (infinite ICER)
        select.2<-!(icer==Inf)
        select.2<-c(TRUE,select.2)
        select<-select.1 & select.2
        data<-data[select,]
        #the function will call itself recursively until all the dominated options are excluded
        if (N>nrow(data)) return(dom(data))
        else return(data.frame(data))
      }
    }

    myicer <- function(data) {
      #exclude dominated options
      data<-dom(data)
      # sort by q from smallest to greatest
      data<- data[order(data$q,data$c),]
      N<-nrow(data)
      icer<-(data$c[2:N]-data$c[1:(N-1)])/(data$q[2:N]-data$q[1:(N-1)])
      icer.final<-c(NA, icer)
      if (N<3) return(data.frame(data,icer=icer.final))
      else {
        #now calculate and exclude extended dominated options
        icer.diff<-icer[2:length(icer)]-icer[1:(length(icer)-1)]
        select.3<-c(TRUE,icer.diff>0,TRUE)
        data<-data[select.3,]
        #the function will call itself recursively until all the dominated options are excluded
        if (N>nrow(data)) return(myicer(data))
        else return(data.frame(data,icer=icer.final))
      }
    }

    py <- input$pnudge_y
    px <- input$pnudge_x
    size <- input$fontsize
    size <- as.numeric(size)
    ytitle <- input$ytitle
    xtitle <- input$xtitle
    titlesize <- input$titlesize
    titlesize <- as.numeric(titlesize)

    ce_plot<-function(data.0,cq) {
      range_y <- max(data.0$c) - min(data.0$c)
      range_x <- max(data.0$q) - min(data.0$q)
      ry <- -(range_y/40)+as.numeric(input$rry)
      rx <- -(range_x/40)+as.numeric(input$rrx)
      ryo <- -(range_y/40)+ as.numeric(input$ryo)
      rxo <- 0 + as.numeric(input$rxo)
      #graph options , cost-effectiveness efficiency boundary and ICERs along the boundary
      ggplot2::ggplot() +
        ggplot2::geom_point(data=data.0,ggplot2::aes(y=c,x=q)) +
        ggplot2::geom_line(data=cq,ggplot2::aes(y=c,x=q)) +
        ggplot2::geom_text(data=data.0,size=size,ggplot2::aes(y=c,x=q),label = data.0$treat, nudge_y=ry, nudge_x=rx) +
        ggplot2::geom_label(data=cq[2:nrow(cq),],ggplot2::aes(y=c,x=q), size=size,label = round(cq$icer[2:nrow(cq)],0), nudge_y=ryo, nudge_x=rxo)+
        ggplot2::xlab(xtitle) + ggplot2::ylab(ytitle)+
        ggplot2::theme(text = ggplot2::element_text(size = titlesize)) + ggrepel::geom_text_repel()+
        if (input$graphtheme == "theme_gray"){
          ggplot2::theme_gray()
        }
        else if (input$graphtheme == "theme_bw"){
          ggplot2::theme_bw()
        }
        else if (input$graphtheme == "theme_linedraw"){
          ggplot2::theme_linedraw()
        }
        else if (input$graphtheme == "theme_light"){
          ggplot2::theme_light()
        }
        else if (input$graphtheme == "theme_dark"){
          ggplot2::theme_dark()
        }
        else if (input$graphtheme == "theme_minimal"){
          ggplot2::theme_minimal()
        }
        else if (input$graphtheme == "theme_classic"){
          ggplot2::theme_classic()
        }
        else if (input$graphtheme == "theme_void"){
          ggplot2::theme_void()
        }
        else if (input$graphtheme == "theme_test"){
          ggplot2::theme_test()
        }

    }

    data.0 <- values$df
    cq <- myicer(data.0)
    ce_plot(data.0,cq)



  })

  shiny::observe({
    data.0 <- values$df
    rangy <- max(data.0$c) - min(data.0$c)
    rangx <- max(data.0$q) - min(data.0$q)
    shiny::updateSliderInput(session, "rry", min=format(round(-rangy/10, 4), nsmall = 4), max=format(round(+rangy/10, 4), nsmall = 4), step = rangy/50, value = 0)
    shiny::updateSliderInput(session, "rrx", min=format(round(-rangx/10, 4), nsmall = 4), max=format(round(+rangx/10, 4), nsmall = 4), step = rangx/50, value = 0)
    shiny::updateSliderInput(session, "ryo", min=format(round(-rangy/10, 4), nsmall = 4), max=format(round(+rangy/10, 4), nsmall = 4), step = rangy/50, value = 0)
    shiny::updateSliderInput(session, "rxo", min=format(round(-rangx/10, 4), nsmall = 4), max=format(round(+rangx/10, 4), nsmall = 4), step = rangx/50, value = 0)
  })

  shinyhelper::observe_helpers(help_dir = "R/helpfile")

  shiny::observeEvent(input$example.button,{
    exampledata <- data.frame(treat=c("T1", "T2", "T3", "T4"), q=c(4, 5.7, 6, 6.5), c=c(650, 655, 655, 658))
    values$df <- rbind(values$df, exampledata)
    data.0 <- rbind(data.0, exampledata)
  })

}


shiny::shinyApp(ui , server) }
