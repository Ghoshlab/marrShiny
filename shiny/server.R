

# Define server logic
server <- function(input, output, session) {
    
    # Handle time-out on BioServer ---------------------------------------------
    observeEvent(input$timeOut, { 
        # Modified from: https://stackoverflow.com/questions/33839543/shiny-server-session-time-out-doesnt-work
        showModal(modalDialog(
            title = "Inactivity Timeout",
            paste("Session timeout due to", input$timeOut, "inactivity -", Sys.time()),
            footer = NULL
        ))
        stopApp() # I made this change so that the app closes instead of the window only
    })
    
    # Data Validation ----------------------------------------------------------
    # Disables Run Analysis button until data is uploaded (in Analysis tab)
    observe({
        file <- input$file1
        if (is.null(file) || file == "") {
            shinyjs::disable("runAnalysis")
        } else {
            shinyjs::enable("runAnalysis")
        }
    })
    
    # Disables download of filtered files until analysis is run and tables are
    #   not NULL
    observeEvent(input$runAnalysis, {
        if (!is.null(dataFilteredByBoth()$filteredData)) {
            shinyjs::enable("downloadFilteredByBoth")
        } else {
            shinyjs::disable("downloadFilteredByBoth")
        }
        if (!is.null(dataFilteredBySP()$filteredData)) {
            shinyjs::enable("downloadFilteredBySP")
        } else {
            shinyjs::disable("downloadFilteredBySP")
        }
        if (!is.null(dataFilteredByF()$filteredData)) {
            shinyjs::enable("downloadFilteredByF")
        } else {
            shinyjs::disable("downloadFilteredByF")
        }
        
        if (!is.null(dataFilteredBySP()$removedSamples)) {
            shinyjs::enable("downloadRemovedS")
        } else {
            shinyjs::disable("downloadRemovedS")
        }
        if (!is.null(dataFilteredByF()$removedFeatures)) {
            shinyjs::enable("downloadRemovedF")
        } else {
            shinyjs::disable("downloadRemovedF")
        }
    })
    
    observeEvent(input$exampleRunAnalysis, {
        
        if (!is.null(exampleDataFilteredByBoth()$filteredData)) {
            shinyjs::enable("exampleDownloadFilteredByBoth")
        } else {
            shinyjs::disable("exampleDownloadFilteredByBoth")
        }
        if (!is.null(exampleDataFilteredBySP()$filteredData)) {
            shinyjs::enable("exampleDownloadFilteredBySP")
        } else {
            shinyjs::disable("exampleDownloadFilteredBySP")
        }
        if (!is.null(exampleDataFilteredByF()$filteredData)) {
            shinyjs::enable("exampleDownloadFilteredByF")
        } else {
            shinyjs::disable("exampleDownloadFilteredByF")
        }
        
        if (!is.null(exampleDataFilteredBySP()$removedSamples)) {
            shinyjs::enable("exampleDownloadRemovedS")
        } else {
            shinyjs::disable("exampleDownloadRemovedS")
        }
        if (!is.null(exampleDataFilteredByF()$removedFeatures)) {
            shinyjs::enable("exampleDownloadRemovedF")
        } else {
            shinyjs::disable("exampleDownloadRemovedF")
        }
    })
    
    validateMarrOutput <- function() {
        validate(need(input$file1 != "", "Please add data set"))
        validate(need(input$runAnalysis, "Please click Run Analysis"))
        
        validate(need(any(!(any(input$excludedVars %in% input$featureVars)),
                          length(input$excludedVars) == 0,
                          length(input$featureVars) == 0),
                      paste0("Cannot exclude variables specified by \"Feature",
                             " Identifying Columns.\" Please remove the column",
                             "(s) from either \"Feature Identifying Columns\" ",
                             "or \"Exclude Columns.\"")))
    }
    
    exampleValidateMarrOutput <- function() {
        validate(need(input$exampleRunAnalysis, "Please click Run Analysis"))
        
        validate(need(any(!(any(input$exampleExcludedVars %in% 
                                    input$exampleFeatureVars)),
                          length(input$exampleExcludedVars) == 0,
                          length(input$exampleFeatureVars) == 0),
                      paste0("Cannot exclude variables specified by \"Feature",
                             " Identifying Columns.\" Please remove the column",
                             "(s) from either \"Feature Identifying Columns\" ",
                             "or \"Exclude Columns.\"")))
    }
    
    # Data / variable prep -----------------------------------------------------
    # Get data from user
    inData <- reactive({
        validate(need(input$file1 != "", "\nPlease add a data set"))
        data <- read.csv(input$file1$datapath, stringsAsFactors = FALSE)
        
        # if (input$rowNames == "Has Row Names") {
        #     data <- read.csv(input$file1$datapath, stringsAsFactors = FALSE,
        #                      row.names = 1)
        # } else {
        #     data <- read.csv(input$file1$datapath, stringsAsFactors = FALSE)
        # }
        
        if (input$rowNames == "Has Row Names") {
            data <- data[2:ncol(data)]
        }
        
        if(input$transpose == "Samples as Rows") {
            data <- data %>%
                t() %>%
                as.data.frame()
        }
        
        validate(need(!(any(is.na(data))),
                      "\nData cannot contain missing values"))
        
        data
    })
    
    exampleInData <- reactive({
        if (input$exampleDataSelect) {
            data <- msprepData
        } else {
            data <- msprepDataNoFeats
        }
        
        if (input$exampleRowNames == "Has Row Names") {
            data <- data[2:ncol(data)]
        }
        
        if(input$exampleTranspose == "Samples as Rows") {
            data <- data %>%
                t() %>%
                as.data.frame()
        }
        
        data
    })
    
    # Remove variables specified by "Excluded Vars", transpose if asked
    updatedInData <- reactive({
        if(length(input$excludedVars) != 0) {
            updatedData <- inData() %>% select(!(c(!!!input$excludedVars)))
        } else {
            updatedData <- inData()
        }
        
        updatedData
    })
    
    exampleUpdatedInData <- reactive({
        
        if(length(input$exampleExcludedVars) != 0) {
            exampleUpdatedInData <- 
                exampleInData() %>% select(!(c(!!!input$exampleExcludedVars)))
        } else {
            exampleUpdatedInData <- exampleInData()
        }
        
        exampleUpdatedInData
    })
    
    # These observe functions enable variable selection from data to remain
    #   up-to-date as users make changes
    observe({
        updateVarSelectInput(session, "excludedVars", data = inData())
    })
    
    observe({
        updateVarSelectInput(session, "exampleExcludedVars", 
                             data = exampleInData())
    })
    
    observe({
        updateVarSelectInput(session, "featureVars", data = inData())
    })
    
    observe({
        updateVarSelectInput(session, "exampleFeatureVars", 
                             data = exampleInData())
    })
    
    # Selection of Feature ID Variables is validated then manipulated to create
    #   a reactive variable which can be used as an argument in Marr
    featureVars <- reactive({
        validate(need(any(!(any(input$excludedVars %in% input$featureVars)),
                          length(input$excludedVars) == 0,
                          length(input$featureVars) == 0),
                      paste0("Cannot exclude variables specified by \"Feature",
                             " Identifying Columns.\" Please remove the column",
                             "(s) from either \"Feature Identifying Columns\" ",
                             "or \"Exclude Columns.\"")))
        
        if(length(input$featureVars) != 0) {
            featureVars <- unlist(strsplit(toString(input$featureVars), 
                                           split = ", "))
        } else {
            featureVars <- NULL
        }
        
    })
    
    exampleFeatureVars <- reactive({
        validate(need(any(!(any(input$exampleExcludedVars %in% 
                                    input$exampleFeatureVars)),
                          length(input$exampleExcludedVars) == 0,
                          length(input$exampleFeatureVars) == 0),
                      paste0("Cannot exclude variables specified by \"Feature",
                             " Identifying Columns.\" Please remove the column",
                             "(s) from either \"Feature Identifying Columns\" ",
                             "or \"Exclude Columns.\"")))
        
        if(length(input$exampleFeatureVars) != 0) {
            exampleFeatureVars <- 
                unlist(strsplit(toString(input$exampleFeatureVars), 
                                split = ", "))
        } else {
            exampleFeatureVars <- NULL
        }
        
    })
    
    # Run main Marr function ---------------------------------------------------
    marrOutput <- eventReactive(input$runAnalysis, {
        #validate(need(input$file1 != "", "\nPlease add a data set"))
        marrOutput <- Marr(updatedInData(), pSamplepairs = input$pSamplepairs,
                           pFeatures = input$pFeatures, alpha = input$alpha,
                           featureVars = featureVars())
    })
    
    exampleMarrOutput <- eventReactive(input$exampleRunAnalysis, {
        exampleMarrOutput <- Marr(exampleUpdatedInData(), 
                                  pSamplepairs = input$examplePSamplepairs,
                                  pFeatures = input$examplePFeatures, 
                                  alpha = input$exampleAlpha,
                                  featureVars = exampleFeatureVars())
    })
    
    # Render inData tables -----------------------------------------------------
    output$inData <- DT::renderDataTable({
        data <- updatedInData() %>%
            mutate_if(is.numeric, round, digits = 2)
        DT::datatable(data, options = list(scrollX = TRUE,
                                           pageLength = 15),
                      rownames = FALSE)
    })
    
    output$exampleInData <- DT::renderDataTable({
        data <- exampleUpdatedInData() %>%
            mutate_if(is.numeric, round, digits = 2)
        DT::datatable(data, options = list(scrollX = TRUE,
                                           pageLength = 15),
                      rownames = FALSE)
    })
    
    # Render sample pairs table ------------------------------------------------
    samplePairData <- eventReactive(input$runAnalysis, {
        samplePairData <- marrOutput() %>%
            MarrSamplepairs() %>%
            mutate_if(is.numeric, round, digits = 2)
    })
    
    output$samplePairs <- DT::renderDataTable({
        validateMarrOutput()
        DT::datatable(samplePairData(), 
                      options = list(scrollX = TRUE, pageLength = 5),
                      rownames = FALSE)
    })
    
    exampleSamplePairData <- eventReactive(input$exampleRunAnalysis, {
        exampleSamplePairData <- exampleMarrOutput() %>%
            MarrSamplepairs() %>%
            mutate_if(is.numeric, round, digits = 2)
    })
    
    output$exampleSamplePairs <- DT::renderDataTable({
        exampleValidateMarrOutput()
        DT::datatable(exampleSamplePairData(), 
                      options = list(scrollX = TRUE, pageLength = 5),
                      rownames = FALSE)
    })
    
    # Render features tables ---------------------------------------------------
    featureData <- eventReactive(input$runAnalysis, {
        featureData <- marrOutput() %>%
            MarrFeatures() %>%
            mutate_if(is.numeric, round, digits = 2)
    })
    
    output$features <- DT::renderDataTable({
        validateMarrOutput()
        DT::datatable(featureData(), 
                      options = list(scrollX = TRUE, pageLength = 5),
                      rownames = FALSE)
    })
    
    exampleFeatureData <- eventReactive(input$exampleRunAnalysis, {
        exampleFeatureData <- exampleMarrOutput() %>%
            MarrFeatures() %>%
            mutate_if(is.numeric, round, digits = 2)
    })
    
    output$exampleFeatures <- DT::renderDataTable({
        exampleValidateMarrOutput()
        DT::datatable(exampleFeatureData(), 
                      options = list(scrollX = TRUE, pageLength = 5),
                      rownames = FALSE)
    })
    
    # Render filtered data tables ----------------------------------------------
    dataFilteredByBoth <- reactive({
        filteredData <- marrOutput() %>%
            MarrFilterData(by = "both")
    })
    
    output$filteredByBoth <- DT::renderDataTable({
        validateMarrOutput()
        
        validate(need(!is.null(dataFilteredByBoth()$filteredData),
                      "All data removed"))
        
        DT::datatable(dataFilteredByBoth()$filteredData %>% 
                          mutate_if(is.numeric, round, digits = 2),
                      options = list(scrollX = TRUE,
                                     pageLength = 3,
                                     searching = FALSE),
                      rownames = FALSE)
    })
    
    exampleDataFilteredByBoth <- reactive({
        filteredData <- exampleMarrOutput() %>%
            MarrFilterData(by = "both")
    })
    
    output$exampleFilteredByBoth <- DT::renderDataTable({
        exampleValidateMarrOutput()
        
        validate(need(!is.null(exampleDataFilteredByBoth()$filteredData),
                      "All data removed"))
        
        DT::datatable(exampleDataFilteredByBoth()$filteredData %>% 
                          mutate_if(is.numeric, round, digits = 2),
                      options = list(scrollX = TRUE,
                                     pageLength = 3,
                                     searching = FALSE),
                      rownames = FALSE)
    })
    
    # Data filtered by only sample pairs
    dataFilteredBySP <- reactive({
        filteredData <- marrOutput() %>%
            MarrFilterData(by = "samplePairs")
    })
    
    output$filteredBySP <- DT::renderDataTable({
        validateMarrOutput()
        
        validate(need(!is.null(dataFilteredBySP()$filteredData),
                      "All samples removed"))
        
        DT::datatable(dataFilteredBySP()$filteredData %>% 
                          mutate_if(is.numeric, round, digits = 2), 
                      options = list(scrollX = TRUE,
                                     pageLength = 3,
                                     searching = FALSE),
                      rownames = FALSE)
    })
    
    exampleDataFilteredBySP <- reactive({
        filteredData <- exampleMarrOutput() %>%
            MarrFilterData(by = "samplePairs")
    })
    
    output$exampleFilteredBySP <- DT::renderDataTable({
        exampleValidateMarrOutput()
        
        validate(need(!is.null(exampleDataFilteredBySP()$filteredData),
                      "All samples removed"))
        
        DT::datatable(exampleDataFilteredBySP()$filteredData %>% 
                          mutate_if(is.numeric, round, digits = 2), 
                      options = list(scrollX = TRUE,
                                     pageLength = 3,
                                     searching = FALSE),
                      rownames = FALSE)
    })
    
    # Data filtered by only features
    dataFilteredByF <- reactive({
        filteredData <- marrOutput() %>%
            MarrFilterData(by = "feature")
    })
    
    output$filteredByF <- DT::renderDataTable({
        validateMarrOutput()
        
        validate(need(!is.null(dataFilteredByF()$filteredData),
                      "All features removed"))
        
        DT::datatable(dataFilteredByF()$filteredData %>% 
                          mutate_if(is.numeric, round, digits = 2), 
                      options = list(scrollX = TRUE,
                                     pageLength = 3,
                                     searching = FALSE),
                      rownames = FALSE)
    })
    
    exampleDataFilteredByF <- reactive({
        filteredData <- exampleMarrOutput() %>%
            MarrFilterData(by = "feature")
    })
    
    output$exampleFilteredByF <- DT::renderDataTable({
        exampleValidateMarrOutput()
        
        validate(need(!is.null(exampleDataFilteredByF()$filteredData),
                      "All features removed"))
        
        DT::datatable(exampleDataFilteredByF()$filteredData %>% 
                          mutate_if(is.numeric, round, digits = 2), 
                      options = list(scrollX = TRUE,
                                     pageLength = 3,
                                     searching = FALSE),
                      rownames = FALSE)
    })
    
    # Example table tooltips
    addTooltip(session, "exampleInData",
               title = paste0("Here your uploaded data will be displayed for you to examine prior to analyzing it with MaRR. Note that samples selected with Exclude Samples will disappear from this table, but those selected with Feature Identifying Columns will remain."),
               placement = "bottom")
    
    addTooltip(session, "exampleSamplePairs",
               title = paste0("Here, each sample pair's reproducibility is given. For each sample pair, its reproducibility is determined as the percentage of features per sample pair which are reproducible according to the MaRR procedure based on the chosen Alpha."),
               placement = "top")
    
    addTooltip(session, "exampleFeatures",
               title = paste0("Here, each features's reproducibility is given. For each feature, its reproducibility is determined as the percentage of sample pairs per feature which are reproducible according to the MaRR procedure based on the chosen Alpha."),
               placement = "top")
    
    addTooltip(session, "exampleFilteredByBoth",
               title = paste0("This table displays data after both its samples and features have been filtered according to the MaRR procedure. For further explanation on when a feature or sample pair is filtered, see the tooltips below. You may download this table with the button directly below it."),
               placement = "bottom")
    
    addTooltip(session, "exampleFilteredBySP",
               title = paste0("This table displays data after its samples have been filtered. A sample is removed if its reproducibility is below P Sample Pairs for ALL sample pairs in which it is a member. In other words, a sample that's reproducibility is above the threshold P Sample Pairs when paired with any other sample will not be removed. You may download this table with the button directly below it."),
               placement = "top")
    
    addTooltip(session, "exampleFilteredByF",
               title = paste0("This table displays data after its features have been filtered. A feature is removed if its reproducibility is below P Features. You may download this table with the button directly below it."),
               placement = "top")
    
    # Render text --------------------------------------------------------------
    samplePairsFiltered <- eventReactive(input$runAnalysis, {
        samplePairsFiltered <- marrOutput() %>%
            MarrSamplepairsfiltered()
    })
    
    output$summaryHTMLfPerS <- renderText({
        validateMarrOutput()
        paste0("Percent of sample pairs with ",
               "greater than ", input$pSamplepairs*100, "% ",
               " reproducible features: <b>", 
               round(samplePairsFiltered(), 2), "%</b>")
    })
    
    exampleSamplePairsFiltered <- eventReactive(input$exampleRunAnalysis,{
        exampleSamplePairsFiltered <- exampleMarrOutput() %>%
            MarrSamplepairsfiltered()
    })
    
    output$exampleSummaryHTMLfPerS <- renderText({
        exampleValidateMarrOutput()
        paste0("Percent of sample pairs with ",
               "greater than ", input$pSamplepairs*100, "% ",
               " reproducible features: <b>", 
               round(exampleSamplePairsFiltered(), 2), "%</b>")
    })
    
    featuresFiltered <- eventReactive(input$runAnalysis, {
        featuresFiltered <- marrOutput() %>%
            MarrFeaturesfiltered()
    })
    
    output$summaryHTMLsPerF <- renderText({
        validateMarrOutput()
        paste0("Percent of features with ",
               "greater than ", input$pFeatures*100, "% ",
               " reproducible sample pairs: <b>", 
               round(featuresFiltered(), 2), "%</b>")
    })
    
    exampleFeaturesFiltered <- eventReactive(input$exampleRunAnalysis, {
        exampleFeaturesFiltered <- exampleMarrOutput() %>%
            MarrFeaturesfiltered()
    })
    
    output$exampleSummaryHTMLsPerF <- renderText({
        exampleValidateMarrOutput()
        paste0("Percent of features with ",
               "greater than ", input$examplePFeatures*100, "% ",
               " reproducible sample pairs: <b>", 
               round(exampleFeaturesFiltered(), 2), "%</b>")
    })
    
    # Example text tooltips
    addTooltip(session, "exampleSummaryHTMLfPerS", 
               title = paste0("Here the percentage of reproducible sample pairs, as determined by P Samples Pairs and Alpha, is given."))
    
    addTooltip(session, "exampleSummaryHTMLsPerF", 
               title = paste0("Here the percentage of reproducible features, as determined by P Features and Alpha, is given."))
    
    # Render plots -------------------------------------------------------------
    output$fPerSP <- renderPlot({
        validateMarrOutput()
        MarrPlotSamplepairs(marrOutput(), yLab = "Density") +
            geom_vline(xintercept = input$pSamplepairs*100,
                       linetype = "dotted", color = "red",
                       size = 1) +
            xlim(c(-1, 101)) +
            annotate(geom = "text", x = input$pSamplepairs*100 + 2, y = Inf,
                     label = "P Sample Pairs", color = "red", angle = 90,
                     hjust = 1.2)
    })
    
    output$exampleFPerSP <- renderPlot({
        exampleValidateMarrOutput()
        MarrPlotSamplepairs(exampleMarrOutput(), yLab = "Density") +
            geom_vline(xintercept = input$examplePSamplepairs*100,
                       linetype = "dotted", color = "red",
                       size = 1) +
            xlim(c(-1, 101)) +
            annotate(geom = "text", x = input$examplePSamplepairs*100 + 2, 
                     y = Inf, label = "P Sample Pairs", color = "red", 
                     angle = 90, hjust = 1.2)
    })
    
    output$sPerF <- renderPlot({
        validateMarrOutput()
        MarrPlotFeatures(marrOutput(), yLab = "Density") +
            geom_vline(xintercept = input$pFeatures*100,
                       linetype = "dotted", color = "red",
                       size = 1) +
            xlim(c(-1, 101)) +
            annotate(geom = "text", x = input$pFeatures*100 + 2, y = Inf,
                     label = "P Features", color = "red", angle = 90,
                     hjust = 1.2)
    })
    
    output$exampleSPerF <- renderPlot({
        exampleValidateMarrOutput()
        MarrPlotFeatures(exampleMarrOutput(), yLab = "Density") +
            geom_vline(xintercept = input$examplePFeatures*100,
                       linetype = "dotted", color = "red",
                       size = 1) +
            xlim(c(-1, 101)) +
            annotate(geom = "text", x = input$examplePFeatures*100 + 2, 
                     y = Inf, label = "P Features", color = "red", angle = 90, 
                     hjust = 1.2)
    })
    
    # Example plot tooltips
    addTooltip(session, "exampleFPerSP",
               paste0("The above plot shows the distribution of sample pairs according to their respective features per sample pair reproducibility."))
    
    addTooltip(session, "exampleSPerF", #"Sample Pair per Feature",
               paste0("The above plot shows the distribution of features according to each their respective sample pairs per feature reproducibility."))
    
    
    # Download Button Helpers --------------------------------------------------
    # Analysis Tab
    output$downloadFilteredByBoth <- downloadHandler(
        filename = "filteredData.csv",
        content = function(file) {
            write.csv(dataFilteredByBoth()$filteredData, file, row.names = FALSE)
        }
    )
    
    output$downloadFilteredBySP <- downloadHandler(
        filename = "filteredData.csv",
        content = function(file) {
            write.csv(dataFilteredBySP()$filteredData, file, row.names = FALSE)
        }
    )
    
    output$downloadFilteredByF <- downloadHandler(
        filename = "filteredData.csv",
        content = function(file) {
            write.csv(dataFilteredByF()$filteredData, file, row.names = FALSE)
        }
    )
    
    output$downloadRemovedS <- downloadHandler(
        filename = "removedSamples.csv",
        content = function(file) {
            write.csv(dataFilteredBySP()$removedSamples, file, 
                      row.names = FALSE)
        }
    )
    
    output$downloadRemovedF <- downloadHandler(
        filename = "removedFeatures.csv",
        content = function(file) {
            write.csv(dataFilteredByF()$removedFeatures, file, 
                      row.names = FALSE)
        }
    )
    
    # Example Tab
    output$exampleDownloadFilteredByBoth <- downloadHandler(
        filename = "filteredData.csv",
        content = function(file) {
            write.csv(exampleDataFilteredByBoth()$filteredData, file, 
                      row.names = FALSE)
        }
    )
    
    output$exampleDownloadFilteredBySP <- downloadHandler(
        filename = "filteredData.csv",
        content = function(file) {
            write.csv(exampleDataFilteredBySP()$filteredData, file, 
                      row.names = FALSE)
        }
    )
    
    output$exampleDownloadFilteredByF <- downloadHandler(
        filename = "filteredData.csv",
        content = function(file) {
            write.csv(exampleDataFilteredByF()$filteredData, file, 
                      row.names = FALSE)
        }
    )
    
    output$exampleDownloadRemovedS <- downloadHandler(
        filename = "removedSamples.csv",
        content = function(file) {
            write.csv(exampleDataFilteredBySP()$removedSamples, file, 
                      row.names = FALSE)
        }
    )
    
    output$exampleDownloadRemovedF <- downloadHandler(
        filename = "removedFeatures.csv",
        content = function(file) {
            write.csv(exampleDataFilteredByF()$removedFeatures, file, 
                      row.names = FALSE)
        }
    )
    
}