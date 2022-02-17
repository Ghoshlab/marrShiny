source("global.R", local = TRUE)

# Script for timeout feature
timeoutSeconds <- 60*15 # 15'
inactivity <- sprintf("function idleTimer() {
var t = setTimeout(logout, %s);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions
function logout() {
Shiny.setInputValue('timeOut', '%ss')
}
function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, %s);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();", timeoutSeconds*1000, timeoutSeconds, timeoutSeconds*1000)

# Define UI
ui <- navbarPage("MaRR - Maximum Rank Reproducibility",
    header=tags$script(inactivity), 
    # Introduction Tab UI ------------------------------------------------------
    tabPanel("Introduction",
          withMathJax(),
          column(1),
          column(10, includeMarkdown("www/introduction.md")),
          column(1)),
    
    # Example Tab UI -----------------------------------------------------------
    tabPanel("Example",
          column(1),
          sidebarLayout(
              sidebarPanel(
                  radioButtons("exampleDataSelect", "Choose CSV File",
                               choices = list("Without Features" = FALSE,
                                              "With Features" = TRUE)),
                  tags$hr(),
                  bsTooltip("exampleDataSelect",
                            paste0("Two example data sets are provided to d",
                                   "emonstrate MaRRs functionality. This f",
                                   "irst contains only abundance columns an",
                                   "d does not require any modification to the data options below. Th",
                                   "e second has two feature identifying ",
                                   "columns: mass-to-charge ratio (mz) and ",
                                   "retention time (rt). When using this da",
                                   "ta set, specify the mz and rt colum",
                                   "ns using the Feature Identifying Colum",
                                   "ns selection tool before clicking Run Analysis")),
                  actionButton("exampleRunAnalysis", "Run Analysis", 
                               width = "100%", 
                               style="color: #228B22; border-color: #228B22; font-weight: bold;"),
                  bsTooltip("exampleRunAnalysis",
                            paste0("Once all the MaRR Options and Data Options have been set according to your needs, click Run Analysis to apply MaRR to your data set.")),
                  tags$hr(),
                  h4("MaRR Options"),
                  sliderInput("examplePSamplepairs", "P Sample Pairs",
                              min = 0, max = 1, value = 0.75),
                  bsTooltip("examplePSamplepairs",
                            paste0("A threshold value that lies between 0 a",
                                   "nd 1, used to assign a feature to be re",
                                   "producible based on the reproducibility",
                                   "output of the sample pairs per feature"),
                            "right"),
                  sliderInput("examplePFeatures", "P Features",
                              min = 0, max = 1, value = 0.75),
                  bsTooltip("examplePFeatures",
                            paste0("A threshold value that lies between 0 a",
                                   "nd 1, used to assign a sample pair to b",
                                   "e reproducible based on the reproducibi",
                                   "lity output of the features per sample ",
                                   "pair"),
                            "right"),
                  sliderInput("exampleAlpha", "Alpha", 
                              min = 0.01, max = 1, value = 0.05),
                  bsTooltip("exampleAlpha",
                            paste0("Level of significance to control the Fa",
                                   "lse Discovery Rate (FDR)"),
                            "right"),
                  tags$hr(),
                  h4("Data Options"), 
                  selectInput("exampleTranspose", "Data Format", 
                              choices = c("Samples as Columns", 
                                          "Samples as Rows")),
                  bsTooltip("exampleTranspose",
                            paste0("Here you may specify whether your data set has samples as columns or samples as rows. Both example data sets have samples as rows so no change is needed, but you may find this feature useful when working with your data."),
                            "top"),
                  selectInput("exampleRowNames", label = NULL, 
                              choices = c("No Row Names", 
                                          "Has Row Names")),
                  bsTooltip("exampleRowNames",
                            paste0("Some data sets may have a redundant leading column labeling each row, such as numbering column. This option is not needed for the example data sets, but you may choose Has Rows to exclude the first column."),
                            "top"),
                  varSelectInput("exampleFeatureVars", 
                                 "Feature Identifying Columns",
                                 data = data.frame(),
                                 multiple = TRUE), 
                  bsTooltip("exampleFeatureVars",
                            paste0("Here you may specify variables which identify features, such as mass-to-charge ratio, retention time, or feature name. These variables will be excluded from the analysis but will remain as identifiers of each feature. The example data set With Features has two feature identifying columns, mz and rt, which should be specified here."),
                            "top"),
                  varSelectInput("exampleExcludedVars", 
                                 "Exclude Samples",
                                 data = data.frame(), 
                                 multiple = TRUE), 
                  bsTooltip("exampleExcludedVars",
                            paste0("Here you may specify samples which you wish to exclude from analysis. When you select a sample name, it will be removed from the Data tab to the right. Use the backspace key to unselect columns."),
                            "top"),
                  width = 3
              ),
              
              mainPanel(
                  tabsetPanel(
                      tabPanel("Data", 
                               h1(), 
                               DT::dataTableOutput("exampleInData")),
                      tabPanel("Sample Pairs",
                               h4("Summary"),
                               htmlOutput("exampleSummaryHTMLfPerS"),
                               tags$hr(),
                               h4("Distribution"),
                               plotOutput("exampleFPerSP"),
                               tags$hr(),
                               h4("% Reproducible by Sample Pair"),
                               DT::dataTableOutput("exampleSamplePairs")),
                      tabPanel("Features",
                               h4("Summary"),
                               htmlOutput("exampleSummaryHTMLsPerF"),
                               tags$hr(),
                               h4("Distribution"),
                               plotOutput("exampleSPerF"),
                               tags$hr(),
                               h4("% Reproducible by Feature"),
                               DT::dataTableOutput("exampleFeatures")),
                      tabPanel("Filtered Data",
                               h4("Filtered by Features and Sample Pairs"),
                               DT::dataTableOutput("exampleFilteredByBoth"),
                               h1(),
                               disabled(downloadButton("exampleDownloadFilteredByBoth",
                                                       paste0("Download data filtered by Sample Pairs and Features"))),
                               tags$hr(),
                               h4("Filtered by only Sample Pairs"),
                               DT::dataTableOutput("exampleFilteredBySP"),
                               h1(),
                               disabled(downloadButton("exampleDownloadFilteredBySP",
                                                       paste0("Download data filtered by only Sample Pairs"))),
                               disabled(downloadButton("exampleDownloadRemovedS",
                                                       paste0("Download removed samples"))),
                               tags$hr(),
                               h4("Filtered by only Features"),
                               DT::dataTableOutput("exampleFilteredByF"),
                               h1(),
                               disabled(downloadButton("exampleDownloadFilteredByF",
                                                       paste0("Download data filtered by only Features"))),
                               disabled(downloadButton("exampleDownloadRemovedF",
                                                       paste0("Download removed features"))),
                               tags$hr())
                  ),
                  width = 7
              )
          ),
          column(1)
    ),
    
    # Analysis Tab UI ----------------------------------------------------------
    tabPanel("Analysis",
          column(1),
          sidebarLayout(
              sidebarPanel(
                  shinyjs::useShinyjs(), 
                  fileInput("file1", "Choose CSV File",
                            multiple = FALSE,
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv")),
                  tags$hr(),
                  actionButton("runAnalysis", "Run Analysis", width = "100%", 
                               style="color: #228B22; border-color: #228B22; font-weight: bold;"),
                  tags$hr(),
                  h4("MaRR Options"),
                  sliderInput("pSamplepairs", "P Sample Pairs",
                              min = 0, max = 1, value = 0.75),
                  sliderInput("pFeatures", "P Features",
                              min = 0, max = 1, value = 0.75),
                  sliderInput("alpha", "Alpha", 
                              min = 0.01, max = 1, value = 0.05),
                  tags$hr(),
                  h4("Data Options"),
                  selectInput("transpose", "Data Format", 
                              choices = c("Samples as Columns", 
                                          "Samples as Rows")),
                  selectInput("rowNames", label = NULL, 
                              choices = c("No Row Names", 
                                          "Has Row Names")),
                  varSelectInput("featureVars", 
                                 "Feature Identifying Columns",
                                 data = data.frame(),
                                 multiple = TRUE), 
                  varSelectInput("excludedVars", 
                                 "Exclude Samples",
                                 data = data.frame(), 
                                 multiple = TRUE),
                  
                  width = 3
              ),
              
              mainPanel(
                  tabsetPanel(
                      tabPanel("Data", 
                               h1(), 
                               DT::dataTableOutput("inData")),
                      tabPanel("Sample Pairs",
                               h4("Summary"),
                               htmlOutput("summaryHTMLfPerS"),
                               tags$hr(),
                               h4("Distribution"),
                               plotOutput("fPerSP"),
                               tags$hr(),
                               h4("% Reproducible by Sample Pair"),
                               DT::dataTableOutput("samplePairs")),
                      tabPanel("Features",
                               h4("Summary"),
                               htmlOutput("summaryHTMLsPerF"),
                               tags$hr(),
                               h4("Distribution"),
                               plotOutput("sPerF"),
                               tags$hr(),
                               h4("% Reproducible by Feature"),
                               DT::dataTableOutput("features")),
                      tabPanel("Filtered Data",
                               h4("Filtered by Features and Sample Pairs"),
                               DT::dataTableOutput("filteredByBoth"),
                               h1(),
                               disabled(downloadButton("downloadFilteredByBoth",
                                                       paste0("Download data filtered by Sample Pairs and Features"))),
                               tags$hr(),
                               h4("Filtered by only Sample Pairs"),
                               DT::dataTableOutput("filteredBySP"),
                               h1(),
                               disabled(downloadButton("downloadFilteredBySP",
                                                       paste0("Download data filtered by only Sample Pairs"))),
                               disabled(downloadButton("downloadRemovedS",
                                                       paste0("Download removed samples"))),
                               tags$hr(),
                               h4("Filtered by only Features"),
                               DT::dataTableOutput("filteredByF"),
                               h1(),
                               disabled(downloadButton("downloadFilteredByF",
                                                       paste0("Download data filtered by only Features"))),
                               disabled(downloadButton("downloadRemovedF",
                                                       paste0("Download removed features"))),
                               tags$hr())
                  ),
                  width = 7
              )
          ),
          column(1)
    ),
    
    # Further Information Tab UI -----------------------------------------------
    tabPanel("Further Information",
          column(1),
          column(10,
                 includeMarkdown("www/furtherInfo.md")),
          column(1)
    )
)