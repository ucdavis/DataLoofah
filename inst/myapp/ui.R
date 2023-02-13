pacman::p_load(dplyr, tibble, ggplot2, shiny, DT, arsenal, openxlsx, readxl,
               svglite, slickR, haven, fresh)

ui <- navbarPage(
  # add favicon to browser tab
  tags$head(tags$link(rel="shortcut icon", href="favicon.png")),
  # tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),
  # replace title with logo on navbar
  title = div(img(src="CTSC_Data_Loofah_Icon.png",
                  width = "90px", height = "60px")),
  # title in browser tab
  windowTitle = "CTSC Data Loofah",
  # adjust dimensions of navbar to accommodate logo
  header = use_theme(
    create_theme(
      theme = "default",
      bs_vars_navbar(
        height = "90px",
        margin_bottom = "15px",
        padding_vertical = "15px"

      )
    )
  ),

  tabPanel(
    "Introduction",

    fluidPage(
      fluidRow(
        h3("Purpose"),
        p("The purpose of this tool is to ",
          "investigate the data and its quality prior to analysis. The ",
          "goal is to catch data issues such as:"),

        tags$ul(
          tags$li("numeric variables stored as text (e.g. numbers with a",
                  " white space at the end, commas, etc.),"),
          tags$li("categorical variables that have inconsistent values ",
                  "(e.g. 'male' vs 'Male' vs 'm' vs 'M'),"),
          tags$li("numeric variables with extreme or nonsensical values",
                  " (e.g. BMI of 203.4),"),
          tags$li("or highlight the use of certain values that are often used",
                  " to code for missing values (e.g. -9, 888, 999)."),
          tags$li("Note that some categorical/factor variables may be stored",
                  " as numeric values. For the purposes of this tool they will",
                  " be treated as numeric values, but this is not a data",
                  " quality issue as ",
                  "they can easily be converted to factors in statistical ",
                  "software prior to analysis.")
        ),

        p("This tool is not meant to ",
          "create final summary statistics! Rather, this tool is to ",
          "help the user identify potential data errors that then ",
          "need to be corrected prior to conducting statistical analyses."),
        h3("Instructions"),
        tags$ul(
          tags$li("To import your data go to the ", tags$b("Data Import"),
                  " tab and click ", tags$b("Browse"),
                  " to select your data file. If your data is stored",
                  " in an Excel file you can select the sheet to import from",
                  " the ", tags$b("Sheet")," dropdown."),
          tags$li("Once your data is imported, a summary sentence and table ",
                  "will appear. The sentence summarizes the number of records",
                  " and variables in the data file. The table shows each ",
                  "variable in the data file and their class (",
                  "how the data are stored). Check that the variables are stored",
                  " as expected."),
          tags$li("The data are automatically split into ",
                  tags$b("Categorical"), " and ", tags$b("Numeric"),
                  " variables and summarized."),
          tags$li("The ", tags$b("Categorical"), " variables are summarized in",
                  " the ", tags$b("Categorical Variables: Summary Table"),
                  " and ", tags$b("Categorical Variables: Figures"),
                  " tabs.", "The summary table will show the number and percent",
                  " of responses in each category", "The number of missing",
                  " responses are shown as well.", "Below the table is a",
                  " warning message listing the categorical variables that are",
                  " not summarized due to there being more than 20 unique",
                  " categories.", " These variables are visualized in the ",
                  " barplots under the ",
                  tags$b("Categorical Variables: Figures"), " tab."),
          tags$li("The ", tags$b("Numeric"), " variables are summarized in the ",
                  tags$b("Numeric Variables: Summary Table"), "and ",
                  tags$b("Numeric Variables: Figures"), " tabs.", " The summary",
                  " table shows the mean (standard deviation),",
                  " median (1st quartile, 3rd quartile),",
                  " range (minimum - maximum),", " and number of missing records.",
                  " These variables are visualized in the histograms under the ",
                  tags$b("Numeric Variables: Figures"), " tab."),
          tags$li("Review the summary tables and figures to assess the",
                  " cleanliness of your data file and correct your data",
                  " as necessary. If you wish to review the summaries at a",
                  " later point in time, the summary tables can be downloaded",
                  " to a CSV or Excel file by clicking the ", tags$b("Download"),
                  " button at the top of each summary table."),
          tags$li("A more detailed manual and demonstration videos are availbe ",
                  "on our ",
                  tags$a(href="https://health.ucdavis.edu/ctsc/area/biostatistics/data-loofah-tool.html",
                         "webpage"), ".")
        )
      )
    )
  ),

  tabPanel(
    "Data Import",
    sidebarLayout(
      sidebarPanel(
        fileInput(
          "upload", NULL, accept = c(".csv", ".xlsx", ".xls",
                                     ".sas7bdat", ".sav",
                                     ".dta", ".rds")
        ),
        selectInput('sheet', "Choose Sheet",  NULL)
      ),
      mainPanel(
        fluidRow(
          column(
            11,
            p(textOutput("dataInfo"))
          )
        ),
        fluidRow(
          column(
            11,
            DT::dataTableOutput("info")
          )
        ),
      )
    )
  ),

  tabPanel(
    "Categorical Variables: Summary Table",
    fluidPage(

      fluidRow(
        column(
          12,
          DT::dataTableOutput("chrTable")
        )

      ),

      fluidRow(
        column(
          11,
          htmlOutput("chrMessage")
        )
      )
    )
  ),

  tabPanel(
    "Categorical Variables: Figures",
    fluidPage(
      slickROutput("ChrSlickR")
    )
  ),

  tabPanel(
    "Numeric Variables: Summary Table",
    fluidPage(
      fluidRow(

        column(
          12,
          DT::dataTableOutput("numTable")
        )

      )
    )
  ),

  tabPanel(
    "Numeric Variables: Figures",
    fluidPage(
      slickROutput("NumSlickR")
    )
  ),

  tabPanel(
    "Numeric Variables: Outliers",
    sidebarPanel(
      p("This page helps you identify records that have data errors you",
        " noticed while reviewing the summary table or histograms. Select the",
        " ID variable and numeric variable of interest in the dropdown menus",
        " below. Next, input the minimum and maximum values your variable of",
        " interest could have. Finally, select if you would like the table to",
        " include records with missing values. Once all options are selected",
        " you will be able to download the table consisting of the ID and ",
        "variable of interest."),
      selectInput(
        inputId = "id_var",
        label = "ID variable:",
        choices = c(""), selected = NULL, multiple = FALSE
      ),
      selectInput(
        inputId = "var_outlier",
        label = "Variable:",
        choices = c(""), selected = NULL, multiple = FALSE
      ),
      numericInput(
        "Minimum",
        label = "Select the minimum value",
        value = NULL
      ),
      numericInput(
        "Maximum",
        label = "Select the maximum value",
        value = NULL
      ),
      radioButtons(
        "includeMiss",
        label = "Include missing values?",
        choices = c("Yes" = "yes",
                    "No" = "no")
      )
    ),

    mainPanel(
      DT::dataTableOutput("outlierTable")
    )
  )

)
