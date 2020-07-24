start <- Sys.time()

reportFolder <- "./report"
source("global.R")
source("report/ReportPlotsAndTables.R")
library("magrittr")

# global -----------------------------------------------------------------------
blank <- ""
databaseIds <- database$databaseId
outcomeIds <- outcomeOfInterest$outcomeId
exposureOfInterest$shortName[exposureOfInterest$exposureId == 2] <- "HCQ"
exposureOfInterest$shortName[exposureOfInterest$exposureId == 28] <- "SSZ"
cohortMethodAnalysis$analysisShortName[cohortMethodAnalysis$analysisId == 1] <- "On-treatment"
cohortMethodAnalysis$analysisShortName[cohortMethodAnalysis$analysisId == 2] <- "30-day"

headingFormat <- officer::fp_text(font.size = 12, bold = TRUE, font.family = "Calibri")
titleFormat <- officer::fp_text(font.size = 12, bold = TRUE, font.family = "Calibri")
comparisonSummary <- comparisonSummary[comparisonSummary$targetId != 137, ]

# patient counts ---------------------------------------------------------------

section1 <- "Study population counts"
section1 <- officer::fpar(officer::ftext(section1, prop = headingFormat))
drops <- 
  (attrition$databaseId == "OptumEHR" & attrition$analysisId == 1) | # panther on-treatment
  (attrition$databaseId %in% c("AmbEMR", "CPRD", "DAGermany", "IMRD") & attrition$outcomeId %in% c(16526, 15636)) | # databases with no IP
  (attrition$targetId == 137) | # combination rows
  (attrition$outcomeId %in% c(15696, 15636)) # influenza outcomes
patientCounts <- attrition[!drops, ]
counts <- getPatientCounts(attritionLevel = "First cohort only & restrict to common period")
counts$subjects <- formatC(counts$subjects, big.mark = ",", format = "d")
header <- c("Exposure", "Database", "Patients", "Percent")
counts <- rbind(header, counts)
counts <- createCountsFlextable(counts)

# doc <- officer::read_docx() %>%
#   officer::body_add_fpar(section1, style = "heading 1") %>%
#   flextable::body_add_flextable(counts) %>%
#   print(target = file.path(reportFolder, "studyPopCounts.docx"))

# tables 1s --------------------------------------------------------------------

section2 <- "Patient baseline characteristics"
section2 <- officer::fpar(officer::ftext(section2, prop = headingFormat))

table1s <- list()
table1Titles <- list()
  
for (i in 1:nrow(comparisonSummary)) { # i=1
  targetLabel <- exposureOfInterest$shortName[exposureOfInterest$exposureId == comparisonSummary$targetId[i]]
  comparatorLabel <- exposureOfInterest$shortName[exposureOfInterest$exposureId == comparisonSummary$comparatorId[i]]
  balance <- getCovariateBalance(targetId = comparisonSummary$targetId[i],
                                 comparatorId = comparisonSummary$comparatorId[i],
                                 databaseId = comparisonSummary$databaseId[i],
                                 analysisId = 2,
                                 outcomeId = 16526) # hosp for psychosis outcomes because most rare
  table1 <- prepareTable1(balance,
                          targetLabel = targetLabel,
                          comparatorLabel = comparatorLabel)
  facs <- sapply(table1, is.factor)
  table1[facs] <- lapply(table1[facs], as.character)
  rm(facs)
  table1 <- rbind(c(blank, "Before PS stratification", blank, blank, "After PS stratification", blank, blank), table1)
  colnames(table1) <- letters[1:length(colnames(table1))]
  table1 <- createTable1FlexTable(table1)
  table1s[[length(table1s) + 1]] <- table1
  title <- comparisonSummary$databaseId[i]
  title <- officer::fpar(officer::ftext(title, prop = titleFormat))  
  table1Titles[[length(table1Titles) + 1]] <- title
}
table1Pairs <- list(table1Titles, table1s)
doc <- officer::read_docx() %>%
  officer::body_add_fpar(section2, style = "heading 1")
for(i in 1:length(table1s)) { #i=1
  doc <- doc %>%
    officer::body_add_fpar(table1Pairs[[1]][[i]], style = "heading 2") %>%
    flextable::body_add_flextable(table1Pairs[[2]][[i]]) %>%
    officer::body_add_break()
}
print(doc, target = file.path(reportFolder, "table1s.docx"))

# diagnostics ------------------------------------------------------------------

section3 <- "Study dianosticis"
section3 <- officer::fpar(officer::ftext(section3, prop = headingFormat))

diagnosticsTitles <- list()
diagnosticsFileNames <- list()
comparisonSummaries <- list(comparisonSummary[1:5, ], comparisonSummary[6:10, ])

plotNumber <- 0
for (cs in comparisonSummaries) {
  plotNumber <- plotNumber + 1
  psPlots <- list()
  balPlots <- list()
  calPlots <- list()
  
  fileName <- file.path(reportFolder, sprintf("diagnostics_%s.png", plotNumber))
  diagnosticsFileNames[[length(diagnosticsFileNames) + 1]] <- fileName
  title <- sprintf("Diagnostics plot %s", plotNumber)
  title <- officer::fpar(officer::ftext(title, prop = titleFormat))
  diagnosticsTitles[[length(diagnosticsTitles) + 1]] <- title

  for (i in 1:nrow(cs)) { # i = 1
    targetId <- cs$targetId[i]
    targetLabel <- exposureOfInterest$shortName[exposureOfInterest$exposureId == tcos$targetId[i]]
    comparatorId <- cs$comparatorId[i]
    comparatorLabel <- exposureOfInterest$shortName[exposureOfInterest$exposureId == tcos$comparatorId[i]]
    databaseId <- cs$databaseId[i]
    
    ps <- getPs(connection, targetId, comparatorId, analysisId = 2, databaseId)
    psPlot <- plotPs2(ps, "HCQ", "SSZ")
    psPlots[[length(psPlots) + 1]] <- psPlot
    bal <- getCovariateBalance(connection, targetId, comparatorId, databaseId, analysisId = 2, outcomeIds[3])
    balPlot <- plotCovariateBalanceScatterPlot2(bal)
    balPlots[[length(balPlots) + 1]] <- balPlot
    for (analysisId in cohortMethodAnalysis$analysisId) { # analysisId = 2
      controlResults <- getControlResults(connection = connection,
                                          targetId = targetId,
                                          comparatorId = comparatorId,
                                          analysisId = analysisId,
                                          databaseId = databaseId)
      controlEstimates <- controlResults[controlResults$effectSize == 1, ]
      calPlot <- plotLargeScatter2(controlEstimates, xLabel = "HR")
      calPlots[[length(calPlots) + 1]] <- calPlot
    }
  }
  col0 <- grid::textGrob("")
  col1 <- grid::textGrob("PS distribution", gp = grid::gpar(fontsize = 12))
  col2 <- grid::textGrob("Covariate balanace", gp = grid::gpar(fontsize = 12))
  col3 <- grid::textGrob("Null dist. (30d)", gp = grid::gpar(fontsize = 12))
  col4 <- grid::textGrob("Null dist. (on-treatment)", gp = grid::gpar(fontsize = 12))
  row1 <- grid::textGrob(cs$databaseId[1], rot = 90, gp = grid::gpar(fontsize = 12))
  row2 <- grid::textGrob(cs$databaseId[2], rot = 90, gp = grid::gpar(fontsize = 12))
  row3 <- grid::textGrob(cs$databaseId[3], rot = 90, gp = grid::gpar(fontsize = 12))
  row4 <- grid::textGrob(cs$databaseId[4], rot = 90, gp = grid::gpar(fontsize = 12))
  row5 <- grid::textGrob(cs$databaseId[5], rot = 90, gp = grid::gpar(fontsize = 12))
  plotGrid <- gridExtra::grid.arrange(col0, col1, col2, col3, col4,
                                      row1, psPlots[[1]], balPlots[[1]], calPlots[[1]], calPlots[[2]],
                                      row2, psPlots[[2]], balPlots[[2]], calPlots[[3]], calPlots[[4]],
                                      row3, psPlots[[3]], balPlots[[3]], calPlots[[5]], calPlots[[6]],
                                      row4, psPlots[[4]], balPlots[[4]], calPlots[[7]], calPlots[[8]],
                                      row5, psPlots[[5]], balPlots[[5]], calPlots[[9]], calPlots[[10]],
                                      nrow = 6,
                                      heights = c(0.25, 4, 4, 4, 4, 4),
                                      widths = c(0.25, 3.5, 3.5, 3.5, 3.5))
  ggplot2::ggsave(fileName, plotGrid, width = 8, height = 12, dpi = 400)
}
diagnosticsPlotPairs <- list(diagnosticsTitles, diagnosticsFileNames)
# doc <- officer::read_docx() %>%
#   officer::body_add_fpar(section3, style = "heading 1")
# for(i in 1:length(diagnosticsFileNames)) { #i=1
#   doc <- doc %>%
#     officer::body_add_fpar(diagnosticsTitles[[i]], style = "heading 2") %>%
#     officer::body_add_img(diagnosticsPlotPairs[[2]][[i]], width = 5, height = 7) %>%
#     officer::body_add_break()
# }
# print(doc, target = file.path(reportFolder, "diagnosticPlots.docx"))


# IR tables --------------------------------------------------------------------

section4 <- "Incidence rates"
section4 <- officer::fpar(officer::ftext(section4, prop = headingFormat))

eventTables <- list()
eventTableTitles <- list()

for (analysisId in cohortMethodAnalysis$analysisId) { # analysisId=1
  analysisName <- cohortMethodAnalysis$analysisShortName[cohortMethodAnalysis$analysisId == analysisId]
  mainResults <- getMainResults(targetIds = exposureOfInterest$exposureId, 
                                comparatorIds = exposureOfInterest$exposureId, 
                                outcomeIds = outcomeIds, 
                                databaseIds = databaseIds,
                                analysisIds = analysisId) 
  mainResults <- mainResults[!is.na(mainResults$seLogRr), ]
  irTable <- prepareReportIrTable(mainResults, outcomeOfInterest)
  irTable$outcomeOrder <- match(irTable$outcomeId, outcomeIds)
  irTable$dbOrder <- match(irTable$databaseId, databaseIds)
  irTable <- irTable[order(irTable$outcomeOrder, irTable$dbOrder), ]
  irTable[, c("outcomeId", "outcomeOrder", "dbOrder")] <- NULL
  if (analysisId == 1) {
    irTable <- irTable[!(irTable$outcomeName == "Hospitalization for psychosis" & irTable$databaseId == "Meta-analysis"), ]  
  }
  header1 <- c(blank, blank, "Patients", blank, "TAR", blank, "Events", blank, "IR", blank, blank)
  header2 <- c("Outcome", "Database", rep(c("T", "C"), 4), "MDRR")    
  irTable <- rbind(header1, header2, irTable)
  irTable <- createIrFlexTable(irTable)
  eventTables[[length(eventTables) + 1]] <- irTable
  title <- analysisName
  title <- officer::fpar(officer::ftext(title, prop = titleFormat))
  eventTableTitles[[length(eventTableTitles) + 1]] <- title 
}
eventTablePairs <- list(eventTableTitles, eventTables)
# doc <- officer::read_docx() %>% 
#   officer::body_add_fpar(section3, style = "heading 1")
# for(i in 1:length(eventTables)) { #i=1
#   doc <- doc %>% 
#     officer::body_add_fpar(eventTablePairs[[1]][[i]], style = "heading 2") %>%
#     flextable::body_add_flextable(eventTablePairs[[2]][[i]])
# }
# print(doc, target = file.path(reportFolder, "irTables.docx"))

# hrs and forest plots ---------------------------------------------------------

section5 <- "Calibrated hazard ratios"
section5 <- officer::fpar(officer::ftext(section5, prop = headingFormat))

hrTitles <- list()
hrFileNames <- list()

for (i in 1:nrow(outcomeOfInterest)) { # i=1
  mainResults <- getMainResults(targetIds = exposureOfInterest$exposureId,
                                comparatorIds = exposureOfInterest$exposureId,
                                outcomeIds = outcomeOfInterest$outcomeId[i],
                                databaseIds = databaseIds,
                                analysisIds = cohortMethodAnalysis$analysisId)
  mainResults <- merge(mainResults, cohortMethodAnalysis[, c("analysisId", "analysisShortName")])
  mainResults$analysisOrder <- match(mainResults$analysisId, cohortMethodAnalysis$analysisId)
  mainResults$dbOrder <- match(mainResults$databaseId, databaseIds)
  mainResults <- mainResults[order(mainResults$analysisOrder, mainResults$dbOrder), ]
  names(mainResults)[names(mainResults) == "analysisShortName"] <- "tar"
  forestPlot <- plotReportForest(mainResults, exposureOfInterest$shortName[1], exposureOfInterest$shortName[2])
  
  fileName <- file.path(reportFolder, sprintf("hr_plots %s.png", outcomeOfInterest$outcomeName[i]))
  ggplot2::ggsave(fileName, forestPlot, width = 8.5, height = 6, dpi = 400)
  hrFileNames[[length(hrFileNames) + 1]] <- fileName
  title <- outcomeOfInterest$outcomeName[i]
  title <- officer::fpar(officer::ftext(title, prop = titleFormat))
  hrTitles[[length(hrTitles) + 1]] <- title
}
# doc <- officer::read_docx() %>%
#   officer::body_add_fpar(section5, style = "heading 1") %>%
#   officer::body_add_fpar(hrTitles[[1]], style = "heading 2") %>%
#   officer::body_add_img(hrFileNames[[1]], width = 5, height = 4) %>%
#   officer::body_add_fpar(hrTitles[[2]], style = "heading 2") %>%
#   officer::body_add_img(hrFileNames[[2]], width = 5, height = 4) %>%
#   print(doc, target = file.path(reportFolder, "hrPlots.docx"))


# build report -----------------------------------------------------------------
source("report/buildReport.R")

delta <- Sys.time() - start
paste("Creating document took", signif(delta, 3), attr(delta, "units"))
