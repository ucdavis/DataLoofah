server <- function(input, output, session) {
  ###################
  # data import tab #
  ###################
  # extract sheet names from Excel files
  sheetNames <- reactive({
    req(input$upload)
    ext <- tools::file_ext(input$upload$name)
    if(ext == 'xls' | ext == "xlsx"){
      readxl::excel_sheets(input$upload$datapath)
    } else {
      "No Sheets"
    }
  })

  # update dropdown menu after data is imported
  observe({
    updateSelectInput(
      session, "sheet", choices = sheetNames()
    )
  })


  # import data using appropriate function
  data <- reactive({
    if((!is.null(input$upload)) && (input$sheet != "")){
      ext <- tools::file_ext(input$upload$name)
      # import data using appropriate function based on file type
      switch(
        ext,
        csv = read.csv(input$upload$datapath),
        xls = read_xls(input$upload$datapath, sheet = input$sheet),
        xlsx = read.xlsx(input$upload$datapath, sheet = input$sheet),
        sas7bdat = read_sas(input$upload$datapath),
        sav = read_spss(input$upload$datapath),
        dta = read_dta(input$upload$datapath),
        rds = readRDS(input$upload$datapath),
        validate(paste0("Invalid file; Please upload a file of the following",
                        " types: .csv, .xls, .xlsx, .sas7bdat, .sav, .dta, .rds"))
      )
    } else {
      NULL
    }
  })

  # Display message to import data file or summary of data file (nrow, ncol)
  data_info <- reactive({
    if((!is.null(data()))){
      text <- paste0("The data has ", nrow(data()), " rows and ", ncol(data()),
                     " columns. The variable types are displayed below.",
                     " Please review each variable and check that its class",
                     " (numeric or character) is as expected.")
    } else {
      text <- paste0("Import your data file.
                     Accepted file types are xls, xlsx",
                     ", csv, R (rds), SAS (sas7bdat), Stata (dta), or SPSS (sav).")
    }
    text
  })

  output$dataInfo <- renderText(data_info())


  # get class of each variable
  data_class <- reactive({
    if((!is.null(data()))){
      data.frame(
        "Variable" = colnames(data()),
        "Class" = sapply(1:ncol(data()), function(x){class(data()[[x]])})
      )
    }
  })

  # display variable names and classes in table
  output$info <- DT::renderDataTable({
    DT::datatable(data_class(),
                  options = list(ordering = FALSE))
  })

  ####################################
  # Categorical Variables Table  tab #
  ####################################
  # get list of variables stored as character/factor
  chr_vars <- reactive({
    if((!is.null(data()))){
      data_class()[data_class()$Class == 'character' |
                     data_class()$Class == 'factor', ]$Variable
    }
  })

  # subset data to character/factor variables
  data_chr <- reactive({
    if((!is.null(data()))){
      data() %>%
        dplyr::select(any_of(chr_vars()))
    }
  })

  # get number of distinct entries for each character/factor variable
  data_chr_distinct <- reactive({
    if((!is.null(data_chr()))){
      data_chr() %>%
        summarise_all(n_distinct) %>%
        t() %>%
        as.data.frame() %>%
        rownames_to_column(var = "Variable") %>%
        rename("N_Distinct" = V1)
    }
  })

  # subset categories to those with <= 20 unique entries
  data_chr_summarize <- reactive({
    if((!is.null(data_chr_distinct()))){
      data_chr_distinct() %>%
        filter(N_Distinct <= 20)
    }
  })

  # get a list of variables with <= 20 unique entries
  chr_vars_summ <- reactive({
    if((!is.null(data_chr_summarize()))){
      unlist(data_chr_summarize()$Variable)
    }
  })

  # subset data with <= 20 unique categories to summarize
  data_chr_summ <- reactive({
    if((!is.null(data_chr_summarize()))){
      data() %>%
        dplyr::select(any_of(chr_vars_summ()))
    }
  })

  # use arsenal::tableby() to get summary statistics for all variables selected
  # see https://cran.r-project.org/web/packages/arsenal/vignettes/tableby.html
  # for more details on arsenal::tableby
  dat_chr_table <- reactive({
    if((!is.null(data_chr_summ())) && ncol(data_chr_summ()) > 0){
      tableby(~., data = data_chr_summ(), test = FALSE,
              cat.stats = c('countpct', 'Nmiss2'),
              stats.labels = list(Nmiss2 = "Missing"))
    }
  })

  # need to modify the arsenal::tableby object to use in DT::datatable()

  # create overall label
  overall_label <- reactive({
    if((!is.null(data_chr_summ()))){
      paste0("Overall (N=", nrow(data()), ")")
    }
  })
  # extract relevant summary info from arsenal::tableby object
  chr_table <- reactive({
    if((!is.null(data_chr_summ())) && ncol(data_chr_summ()) > 0){
      tmp <- summary(dat_chr_table())$object$Overall %>%
        filter(Overall != "") %>%
        rowwise() %>%
        mutate(
          Test = case_when(
            label != "Missing" ~ paste0(Overall[1], " (", round(Overall[2], 2),
                                        "%)"),
            label == "Missing" ~ paste0(Overall[1])
          )
        ) %>%
        ungroup() %>%
        rename("Category" = "label") %>%
        dplyr::select(variable, Category, Test)

      colnames(tmp)[colnames(tmp) == "Test"] <- overall_label()
      tmp
    }
  })

  # subset data to those with >20 distinct entries
  dat_chr_distinct_check <- reactive({
    if((!is.null(data_chr_distinct()))){
      data_chr_distinct() %>%
        filter(N_Distinct > 20)
    }
  })
  # display a warning message for variables with >20 unique entries or display
  # message if there are no character variables in the data set.
  chr_Message <- reactive({
    if((!is.null(dat_chr_distinct_check()))){
      chr_vars_check <- unlist(dat_chr_distinct_check()$Variable)
      if(length(chr_vars_check) > 0){
        text <- paste0(
          "The following variables are stored as a character/factor with more",
          " than 20 unique responses: ", paste(chr_vars_check, collapse = ", "),
          ".", tags$br(),
          "Consider checking if these are numeric values stored as text",
          " or character values with typos/spelling differences between similar",
          " responses (e.g. Male, male, m, M)."
        )
      } else if(length(chr_vars()) > 0 & length(chr_vars_check) == 0){
        text <- paste0(
          "There are no additional character variables to review."
        )
      } else if((!is.null(chr_vars())) && length(chr_vars()) == 0){
        text <-  paste0("There are no character variables.")
      }
    } else{
      text <- paste0("")
    }
    text
  })

  # create summary table using DT::datatable()
  # see https://rstudio.github.io/DT/ for more details on DT::datatable
  # options: set page length to 20, add button to download summary table,
  #   disable ordering of table, group rows by variable
  output$chrTable <- DT::renderDataTable(server = FALSE, {
    if((!is.null(chr_table()))){
      DT::datatable(
        chr_table(), rownames = FALSE,
        extensions = list('RowGroup' = NULL, "Buttons" = NULL),
        options=list(dom = 'Bfrtip',
                     columnDefs = list(list(visible=FALSE,
                                            targets=c(0))),
                     rowGroup = list(dataSrc = 0),
                     pageLength = 20,
                     ordering = FALSE,
                     buttons = list(
                       list(
                         extend = 'collection',
                         buttons = c('csv', 'excel'),
                         text = "Download"
                       )
                     )
        )
      ) %>%
        formatStyle(names(chr_table()), textAlign = 'center')
    }
  })

  output$chrMessage <- renderText(chr_Message())

  #####################################
  # Categorical Variables Figures tab #
  #####################################
  # create bar plots for all character/factor variables that were summarized
  chr_plots <- reactive({
    if((!is.null(data_chr_summ())) && ncol(data_chr_summ()) > 0){
      tmp_chr_plots <- vector(mode = 'list', length = ncol(data_chr_summ()))

      # include progress bar so user has an indicator
      withProgress(message = "Creating Figures", value = 0, {
        n_chr <- length(tmp_chr_plots)

        for(i in 1:n_chr){
          # create plot and convert to SVG image
          tmp_chr_plots[[i]] <- xmlSVG({
            show(ggplot(data_chr_summ(), aes(x = .data[[chr_vars_summ()[i]]])) +
                   geom_bar() +
                   theme_bw())
          }, standalone = TRUE)

          incProgress(1/n_chr, detail = paste("Figure", i))

          Sys.sleep(0.1)
        }
      })


      tmp_chr_plots
    }
  })

  # following code updates slickR dots with index number
  # see https://cran.r-project.org/web/packages/slickR/vignettes/basics.html
  # for more on slickR
  cP1 <- htmlwidgets::JS("function(slick, index) {
    return '<a>'+(index+1)+'</a>';
    }")

  opts_dot_number1 <- settings(
    initialSlide = 0,
    slidesToShow = 1,
    slidesToScroll = 1,
    focusOnSelect = TRUE,
    dots = TRUE,
    customPaging = cP1
  )

  # render figure images in carousel using slickR
  output$ChrSlickR <- renderSlickR({
    if((!is.null(chr_plots()))){
      slickR(chr_plots(), height = 550, width = "95%", slideId = 'chr') +
        settings(slidesToShow = 1, slidesToScroll = 1) +
        opts_dot_number1
    }

  })

  ###############################
  # Numeric Variables Table tab #
  ###############################
  # create vector of numeric variables
  num_vars <- reactive({
    if((!is.null(data()))){
      data_class()[data_class()$Class == 'numeric' |
                     data_class()$Class == 'integer', ]$Variable
    }
  })

  # subset data to numeric variables
  dat_num <- reactive({
    if((!is.null(data()))){
      data() %>%
        dplyr::select(any_of(num_vars()))
    }
  })

  # summary table of numeric variables using arsenal::tableby
  dat_num_table <- reactive({
    if((!is.null(dat_num())) && ncol(dat_num()) > 0){
      tableby(~., data = dat_num(), test = FALSE,
              numeric.stats = c('meansd', 'medianq1q3', 'range', 'Nmiss2'),
              stats.labels = list(Nmiss2 = "(Missing)"))
    }
  })

  # extract relevant data from tableby object
  num_table <- reactive({
    if((!is.null(dat_num_table()))){
      tmp2 <- summary(dat_num_table())$object$Overall %>%
        filter(Overall != "") %>%
        rowwise() %>%
        mutate(
          Test = case_when(
            label == "Mean (SD)" ~ paste0(format(round(Overall[1], 2), nsmall = 2),
                                          " (", format(round(Overall[2], 2),
                                                       nsmall = 2), ")"),
            label == "Median (Q1, Q3)" ~ paste0(format(round(unname(Overall[1]), 2),
                                                       nsmall = 2),
                                                " (",
                                                format(round(unname(Overall[2]), 2),
                                                       nsmall = 2),
                                                ", ",
                                                format(round(unname(Overall[3]), 2),
                                                       nsmall = 2),
                                                ")"),
            label == "Range" ~ paste0(format(round(Overall[1], 2), nsmall = 2),
                                      " \U2012 ",
                                      format(round(Overall[2], 2), nsmall = 2)),
            label == "(Missing)" ~ paste0(Overall[1])
          )
        ) %>%
        ungroup() %>%
        rename("Summary" = "label") %>%
        dplyr::select(variable, Summary, Test)

      colnames(tmp2)[colnames(tmp2) == 'Test'] <- overall_label()
      tmp2
    }
  })

  # create summary table using DT::datatable()
  # see https://rstudio.github.io/DT/ for more details on DT::datatable
  # options: set page length to 20, add button to download summary table,
  #   disable ordering of table, group rows by variable
  output$numTable <- DT::renderDataTable(server = FALSE, {
    if((!is.null(num_table()))){
      DT::datatable(
        num_table(), rownames = FALSE,
        extensions = list('RowGroup' = NULL, 'Buttons' = NULL),
        options=list(dom = 'Bfrtip',
                     columnDefs = list(list(visible=FALSE,
                                            targets=c(0))),
                     rowGroup = list(dataSrc = 0),
                     pageLength = 12,
                     ordering = FALSE,
                     buttons = list(
                       list(
                         extend = 'collection',
                         buttons = c('csv', 'excel'),
                         text = "Download"
                       )
                     ))
      ) %>%
        formatStyle(names(num_table()), textAlign = 'center')
    }
  })


  #################################
  # Numeric Variables Figures tab #
  #################################
  num_plots <- reactive({
    if((!is.null(dat_num())) && ncol(dat_num()) > 0){
      tmp_num_plots <- vector(mode = 'list', length = ncol(dat_num()))

      withProgress(message = "Creating Figures", value = 0, {
        n_num <- length(tmp_num_plots)

        for(i in 1:n_num){
          tmp_num_plots[[i]] <- xmlSVG({
            show(ggplot(dat_num(), aes(x = .data[[num_vars()[i]]])) +
                   geom_histogram() +
                   theme_bw())
          }, standalone = TRUE)

          incProgress(1/n_num, detail = paste("Figure", i))

          Sys.sleep(0.1)
        }

      })
      tmp_num_plots
    }
  })

  # following code updates slickR dots with index number
  # see https://cran.r-project.org/web/packages/slickR/vignettes/basics.html
  # for more on slickR
  cP2 <- htmlwidgets::JS("function(slick, index) {
      return '<a>'+(index+1)+'</a>';
                             }")

  opts_dot_number2 <- settings(
    initialSlide = 0,
    slidesToShow = 1,
    slidesToScroll = 1,
    focusOnSelect = TRUE,
    dots = TRUE,
    customPaging = cP2
  )

  output$NumSlickR <- renderSlickR({
    if((!is.null(num_plots()))){
      slickR(num_plots(), height = 550, width = "95%", slideId = 'num') +
        settings(slidesToShow = 1, slidesToScroll = 1) +
        opts_dot_number2
    }
  })

  ###############################
  # Numeric Variables: Outliers #
  ###############################
  # update var selection lists in outliers tab #
  # update list of variables to select ID
  observe({
    if (!is.null(num_vars())) updateSelectInput(session, "id_var", choices=data_class()$Variable)
  })
  # update list of variables to select variable of interest
  observe({
    if (!is.null(num_vars())) updateSelectInput(session, "var_outlier", choices=num_vars())
  })


  # subset data to those below stated minimum, above stated max, or missing
  # can select if you want to include records with missing values
  dat_outliers <- reactive({
    if((!is.null(data())) & input$includeMiss == "no"){
      data() %>%
        dplyr::select(input$id_var, input$var_outlier) %>%
        filter(!!(as.name(input$var_outlier)) < input$Minimum |
                 !!(as.name(input$var_outlier)) > input$Maximum)
    } else if((!is.null(data())) & input$includeMiss == "yes"){
      data() %>%
        dplyr::select(input$id_var, input$var_outlier) %>%
        filter(!!(as.name(input$var_outlier)) < input$Minimum |
                 !!(as.name(input$var_outlier)) > input$Maximum |
                 is.na(!!(as.name(input$var_outlier))))
    }
  })

  # create table of records outside of range using DT::datatable
  # see https://rstudio.github.io/DT/ for more details on DT::datatable
  # options: set page length to 10, add button to download table,
  #   disable ordering of table
  output$outlierTable <- DT::renderDataTable(server = FALSE, {
    if((!is.null(dat_outliers()))){
      DT::datatable(
        dat_outliers(), rownames = FALSE,
        extensions = list('Buttons' = NULL),
        options=list(dom = 'Bfrtip',
                     pageLength = 10,
                     ordering = FALSE,
                     buttons = list(
                       list(
                         extend = 'collection',
                         buttons = c('csv', 'excel'),
                         text = "Download"
                       )
                     ))
      ) %>%
        formatStyle(names(dat_outliers()), textAlign = 'center')
    }
  })

}
