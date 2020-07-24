getPatientCounts <- function(attritionLevel) {
  patientCounts <- patientCounts[patientCounts$description == attritionLevel & patientCounts$databaseId %in% databaseIds & patientCounts$analysisId == 2, 
                                 c("databaseId", "exposureId", "subjects")]
  patientCounts <- patientCounts[!duplicated(patientCounts), ]
  exposureTotals <- aggregate(patientCounts["subjects"], by = patientCounts["exposureId"], FUN = sum)
  names(exposureTotals)[2] <- "exposureSubjects"
  databaseTotals <- aggregate(patientCounts["subjects"], by = patientCounts["databaseId"], FUN = sum)
  names(databaseTotals)[2] <- "databaseSubjects"
  patientCounts <- merge(patientCounts, exposureTotals)
  patientCounts$exposurePercent <- round(patientCounts$subjects / patientCounts$exposureSubjects * 100, 2)
  patientCounts <- merge(exposureOfInterest[, 1:2], patientCounts)
  patientCounts$exposureSubjects <- formatC(patientCounts$exposureSubjects, big.mark = ",", format = "d")
  patientCounts$exposureName <- sprintf("%s (n = %s)", patientCounts$exposureName, patientCounts$exposureSubjects)
  patientCounts[, c("exposureId", "exposureSubjects")] <- NULL
  return(patientCounts)
}

createCountsFlextable <- function(countsTable) {
  countsTable <- flextable::qflextable(countsTable)
  countsTable <- flextable::delete_part(countsTable, part = "header")
  countsTable <- flextable::fontsize(countsTable, part = "all", size = 6)
  countsTable <- flextable::padding(countsTable, padding = 0, part = "all")
  countsTable <- flextable::merge_v(countsTable, j = 1, part = "body")
  border <- officer::fp_border(color = "black", width = 1)
  countsTable <- flextable::border_inner(countsTable, border = border, part = "all")
  countsTable <- flextable::border_outer(countsTable, border = border, part = "all")
  countsTable <- flextable::align(countsTable, j = 1:2, align = "left", part = "all")
  countsTable <- flextable::align(countsTable, i = 1, align = "left", part = "all")
  countsTable <- flextable::autofit(countsTable, add_w = 0.1, add_h = 0.1)
  return(countsTable)
}

createTable1FlexTable <- function(table1) {
  table1 <- flextable::qflextable(table1)
  table1 <- flextable::delete_part(table1, part = "header")
  table1 <- flextable::fontsize(table1, part = "all", size = 6)
  table1 <- flextable::align(table1, j = 1, align = "left", part = "all")
  table1 <- flextable::merge_h(table1, i = 1, part = "body")
  table1 <- flextable::autofit(table1, add_w = 0, add_h = 0)
  table1 <- flextable::padding(table1, padding = 0, part = "all")
  border <- officer::fp_border(color = "black", width = 1)
  table1 <- flextable::border_inner(table1, border = border, part = "all")
  table1 <- flextable::border_outer(table1, border = border, part = "all")
  return(table1)
}

getIrIp <- function(studyPop) {
  targetPatients <- sum(studyPop$treatment == 1)
  targetPersonYears <- sum(studyPop$survivalTime[studyPop$treatment == 1]) / 365.25
  targetEvents <- sum(ifelse(studyPop$outcomeCount[studyPop$treatment == 1] > 0, 1, 0))
  result <- data.frame(patients = formatC(targetPatients, big.mark = ",", format = "d"),
                       events = formatC(targetEvents, big.mark = ",", format = "d"),
                       pys = formatC(targetPersonYears, big.mark = ",", format = "d"),
                       ir1k = round(1000 * targetEvents / targetPersonYears, 3),
                       ip1k = round(1000 * targetEvents / targetPatients, 3),
                       stringsAsFactors = FALSE)
  return(result)
}

prepareReportIrTable <- function(mainResults, outcomeOfInterest) {
  irTable <- merge(mainResults, outcomeOfInterest)
  alpha <- 0.05
  power <- 0.8
  z1MinAlpha <- qnorm(1 - alpha/2)
  zBeta <- -qnorm(1 - power)
  pA <- irTable$targetSubjects/(irTable$targetSubjects + irTable$comparatorSubjects)
  pB <- 1 - pA
  totalEvents <- abs(irTable$targetOutcomes) + (irTable$comparatorOutcomes)
  irTable$mdrr <- exp(sqrt((zBeta + z1MinAlpha)^2/(totalEvents * pA * pB)))
  irTable$targetYears <- irTable$targetDays/365.25
  irTable$comparatorYears <- irTable$comparatorDays/365.25
  irTable$targetIr <- 1000 * irTable$targetOutcomes/irTable$targetYears
  irTable$comparatorIr <- 1000 * irTable$comparatorOutcomes/irTable$comparatorYears
  irTable$targetSubjects <- formatC(irTable$targetSubjects, big.mark = ",", format = "d")
  irTable$comparatorSubjects <- formatC(irTable$comparatorSubjects, big.mark = ",", format = "d")
  irTable$targetYears <- formatC(irTable$targetYears, big.mark = ",", format = "d")
  irTable$comparatorYears <- formatC(irTable$comparatorYears, big.mark = ",", format = "d")
  irTable$targetOutcomes <- formatC(irTable$targetOutcomes, big.mark = ",", format = "d")
  irTable$comparatorOutcomes <- formatC(irTable$comparatorOutcomes, big.mark = ",", format = "d")
  irTable$targetIr <- sprintf("%.2f", irTable$targetIr)
  irTable$comparatorIr <- sprintf("%.2f", irTable$comparatorIr)
  irTable$mdrr <- sprintf("%.2f", irTable$mdrr)
  irTable$targetSubjects <- gsub("^-", "<", irTable$targetSubjects)
  irTable$comparatorSubjects <- gsub("^-", "<", irTable$comparatorSubjects)
  irTable$targetOutcomes <- gsub("^-", "<", irTable$targetOutcomes)
  irTable$comparatorOutcomes <- gsub("^-", "<", irTable$comparatorOutcomes)
  irTable$targetIr <- gsub("^-", "<", irTable$targetIr)
  irTable$comparatorIr <- gsub("^-", "<", irTable$comparatorIr)
  idx <- (irTable$targetOutcomes < 0 | irTable$comparatorOutcomes < 0)
  irTable$mdrr[idx] <- paste0(">", irTable$mdrr[idx])
  irTable <- irTable[, c("outcomeId",
                         "outcomeName",
                         "databaseId",
                         "targetSubjects",
                         "comparatorSubjects",
                         "targetYears",
                         "comparatorYears",
                         "targetOutcomes",
                         "comparatorOutcomes",
                         "targetIr",
                         "comparatorIr",
                         "mdrr")]
  return(irTable)
}

createIrFlexTable <- function(irTable) {
  irTable <- flextable::qflextable(irTable)
  irTable <- flextable::delete_part(irTable, part = "header")
  irTable <- flextable::fontsize(irTable, part = "all", size = 6)
  irTable <- flextable::align(irTable, j = 1:2, align = "left", part = "all")
  irTable <- flextable::merge_v(irTable, j = 1, part = "body")
  irTable <- flextable::autofit(irTable, add_w = 0, add_h = 0)
  irTable <- flextable::padding(irTable, padding = 0, part = "all")
  border <- officer::fp_border(color = "black", width = 1)
  irTable <- flextable::border_inner(irTable, border = border, part = "all")
  irTable <- flextable::border_outer(irTable, border = border, part = "all")
  return(irTable)
}

plotPs2 <- function(ps, targetName, comparatorName) {
  ps <- rbind(data.frame(x = ps$preferenceScore, y = ps$targetDensity, group = targetName),
              data.frame(x = ps$preferenceScore, y = ps$comparatorDensity, group = comparatorName))
  ps$group <- factor(ps$group, levels = c(as.character(targetName), as.character(comparatorName)))
  theme <- ggplot2::element_text(colour = "#000000", size = 8, margin = ggplot2::margin(0, 0.5, 0, 0.1, "cm"))
  plot <- ggplot2::ggplot(ps,
                          ggplot2::aes(x = x, y = y, color = group, group = group, fill = group)) +
    ggplot2::geom_density(stat = "identity") +
    ggplot2::scale_fill_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5),
                                          rgb(0, 0, 0.8, alpha = 0.5))) +
    ggplot2::scale_color_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5),
                                           rgb(0, 0, 0.8, alpha = 0.5))) +
    ggplot2::scale_x_continuous("Preference score", limits = c(0, 1)) +
    ggplot2::scale_y_continuous("Density") +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   legend.position = "none",
                   legend.text = theme,
                   axis.text = theme,
                   axis.title = theme)
  return(plot)
}

plotCovariateBalanceScatterPlot2 <- function(balance, beforeLabel = "Before stratification", afterLabel = "After stratification") {
  limits <- c(min(c(balance$absBeforeMatchingStdDiff, balance$absAfterMatchingStdDiff),
                  na.rm = TRUE),
              max(c(balance$absBeforeMatchingStdDiff, balance$absAfterMatchingStdDiff),
                  na.rm = TRUE))
  theme <- ggplot2::element_text(colour = "#000000", size = 8)
  plot <- ggplot2::ggplot(balance, ggplot2::aes(x = absBeforeMatchingStdDiff, y = absAfterMatchingStdDiff)) +
    ggplot2::geom_point(color = rgb(0, 0, 0.8, alpha = 0.3), shape = 16, size = 2) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::scale_x_continuous(beforeLabel, limits = limits) +
    ggplot2::scale_y_continuous(afterLabel, limits = limits) +
    ggplot2::theme(text = theme)
  
  return(plot)
}

plotLargeScatter2 <- function(d, xLabel) {
  d$Significant <- d$ci95Lb > 1 | d$ci95Ub < 1
  estimates <- length(d$seLogRr[!is.na(d$seLogRr)])
  # oneRow <- data.frame(nLabel = paste0(formatC(estimates, big.mark = ","), " estimates"),
  #                      meanLabel = paste0(formatC(100 *
  #                                                   mean(!d$Significant, na.rm = TRUE), digits = 1, format = "f"), "% of CIs includes 1"))
  breaks <- c(0.1, 0.25, 0.5, 1, 2, 4, 6, 8, 10)
  theme <- ggplot2::element_text(colour = "#000000", size = 8)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 8, hjust = 1)
  
  alpha <- 1 - min(0.95 * (nrow(d)/50000)^0.1, 0.95)
  plot <- ggplot2::ggplot(d, ggplot2::aes(x = logRr, y = seLogRr)) +
    ggplot2::geom_vline(xintercept = log(breaks), colour = "white", lty = 1, size = 0.5) +
    ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1/qnorm(0.025)),
                         colour = rgb(0.8, 0, 0),
                         linetype = "dashed",
                         size = 1,
                         alpha = 0.5) +
    ggplot2::geom_abline(ggplot2::aes(intercept = 0, slope = 1/qnorm(0.975)),
                         colour = rgb(0.8, 0, 0),
                         linetype = "dashed",
                         size = 1,
                         alpha = 0.5) +
    ggplot2::geom_point(size = 2, color = rgb(0, 0, 0, alpha = 0.05), alpha = alpha, shape = 16) +
    ggplot2::geom_hline(yintercept = 0) +
    # ggplot2::geom_label(x = log(0.11),
    #                     y = 1,
    #                     alpha = 1,
    #                     hjust = "left",
    #                     ggplot2::aes(label = nLabel),
    #                     size = 5,
    #                     data = oneRow) +
    # ggplot2::geom_label(x = log(0.11),
    #                     y = 0.9,
    #                     alpha = 1,
    #                     hjust = "left",
    #                     ggplot2::aes(label = meanLabel),
    #                     size = 5,
    #                     data = oneRow) +
    ggplot2::scale_x_continuous(xLabel, limits = log(c(0.1, 10)), breaks = log(breaks), labels = breaks) +
    ggplot2::scale_y_continuous("Standard Error", limits = c(0, 1)) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text.y = theme,
                   axis.text.x = theme,
                   axis.title = theme, #ggplot2::element_blank(),
                   legend.key = ggplot2::element_blank(),
                   strip.text.x = theme, #ggplot2::element_blank(),
                   strip.background = ggplot2::element_blank(),
                   legend.position = "none")
  return(plot)
}

plotReportForest <- function(mainResults, targetLabel, comparatorLabel) {
  breaks <- c(0.175, 0.25, 0.5, 1, 2, 4, 6)
  #plotLabels <- c(0.175, 0.25, 0.5, 1, 2, 4, 6)
  plotLabels <- c(0.175, 0.25, paste("0.5\nFavors", targetLabel), paste("1\ncHR"), paste("2\nFavors", comparatorLabel), 4, 6)
  
  d <- mainResults[, c("tar", "databaseId", "calibratedRr", "calibratedCi95Lb", "calibratedCi95Ub", "i2")]
  d[, c("calibratedRr", "calibratedCi95Lb", "calibratedCi95Ub")] <- log(d[, c("calibratedRr", "calibratedCi95Lb", "calibratedCi95Ub")])
  d$type[d$databaseId == "Meta-analysis"] <- "ma"
  d$type[is.na(d$type)] <- 'db'
  d$databaseId[d$databaseId == "Meta-analysis"] <- paste0("Summary (I2", ifelse(d$i2[d$databaseId == "Meta-analysis"] < 0.01, "<0.01", paste0("=", d$i2[d$databaseId == "Meta-analysis"])), ")")
  d$i2 <- NULL
  names(d) <- c("tar", "databaseId", "logRr", "logLb95Ci", "logUb95Ci", "type")
  header <- data.frame(tar = "Time-at-risk",
                       databaseId = "Database",
                       logRr = -100,
                       logLb95Ci = -100,
                       logUb95Ci = -100,
                       type = "header")
  d <- rbind(header, d)
  d$lcl <- ifelse(d$logLb95Ci < log(0.175), log(0.175), d$logLb95Ci)
  d$ucl <- ifelse(d$logUb95Ci > log(6), log(6), d$logUb95Ci)
  d$lcl[d$type == "header"] <- -100
  d$ucl[d$type == "header"] <- -100
  d$lcl[is.na(d$lcl)] <- -100
  d$ucl[is.na(d$ucl)] <- -100
  d$logRr[is.na(d$logRr)] <- -100
  d$logLb95Ci[is.na(d$logLb95Ci)] <- -100
  d$logUb95Ci[is.na(d$logUb95Ci)] <- -100
  dropRows <- (d$logRr == -100 | d$logLb95Ci == -100 | d$logUb95Ci == -100) & d$type %in% c("db", "ma")
  d <- d[!dropRows, ]
  d$row <- rev(1:nrow(d))
  
  rr <- ifelse(exp(d$logRr) > 100, ">100", formatC(exp(d$logRr), digits = 2, format = "f"))
  rr[rr == "0.00"] <- "<0.01"
  
  rrLb95Ci <- ifelse(exp(d$logLb95Ci) < 0.01, "<0.01", formatC(exp(d$logLb95Ci), digits = 2, format = "f"))
  rrUb95Ci <- ifelse(exp(d$logUb95Ci) > 100, ">100", formatC(exp(d$logUb95Ci), digits = 2, format = "f"))
  rrUb95Ci[rrUb95Ci == "0.00"] <- "<0.01"
  
  labels <- paste0(rr, " (", rrLb95Ci, ", ", rrUb95Ci, ")")
  labels <- data.frame(y = rep(d$row, 3),
                       x = rep(c(-7.5, -5.5, -3.5), each = nrow(d)),
                       label = c(as.character(d$tar), as.character(d$databaseId), labels),
                       tar = rep(d$tar, 3),
                       stringsAsFactors = FALSE)
  labels$label[labels$x == -7.5 & duplicated(labels$label)] <- ""
  labels$label[labels$label == "<0.01 (<0.01, <0.01)" & labels$comparison == "T vs C"] <- "cHR (95% CI)"
  labels$label[labels$label == "<0.01 (<0.01, <0.01)"] <- ""
  labels$label[labels$label == ">100 (<0.01, <0.01)"] <- ""
  labels$label[labels$y == max(labels$y) & labels$x == -3.5] <- "cHR (95%)"
  
  if (length(d$row[d$logLb95Ci < d$lcl]) > 0) {
    lclData <- data.frame(x = log(0.175),
                          xend = log(0.175),
                          y = d$row[d$logLb95Ci < d$lcl],
                          yend = d$row[d$logLb95Ci < d$lcl])
  } else {
    lclData <- data.frame(x = -100, xend = -100, y = -100, yend = -100)
  }
  if (length(d$row[d$logUb95Ci > d$ucl]) > 0) {
    uclData <- data.frame(x = log(6),
                          xend = log(6),
                          y = d$row[d$logUb95Ci > d$ucl],
                          yend = d$row[d$logUb95Ci > d$ucl])
  } else {
    uclData <- data.frame(x = -100, xend = -100, y = -100, yend = -100)
  }
  
  plot <- ggplot2::ggplot(d, ggplot2::aes(x = logRr, y = row)) +
    ggplot2::scale_fill_manual(values = c('#f7f7f7','#cccccc','#f7f7f7','#cccccc'), breaks = levels(d$tar)) +
    ggplot2::geom_rect(ggplot2::aes(xmin = -7.5, xmax = 10, ymin = row - 0.5, ymax = row + 0.5, fill = tar), alpha =0.5) +
    ggplot2::geom_vline(xintercept = log(breaks), colour = "light gray", lty = 1, size = 0.2) +
    ggplot2::geom_vline(xintercept = 0, size = 0.5) +
    ggplot2::geom_errorbarh(height = 0, ggplot2::aes(xmin = lcl, xmax = ucl)) +
    ggplot2::geom_segment(data = lclData,
                          ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
                          arrow = grid::arrow(angle = 30, type = "open", length = ggplot2::unit(0.05, "inches"))) +
    ggplot2::geom_segment(data = uclData,
                          ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
                          arrow = grid::arrow(angle = 210, type = "open", length = ggplot2::unit(0.05, "inches"))) +
    ggplot2::geom_point(size = 3, ggplot2::aes(shape = type), fill = "white", show.legend = FALSE) +
    ggplot2::scale_shape_manual(values = c(18, 16, 23)) +
    ggplot2::scale_x_continuous(breaks = log(breaks), labels = plotLabels) +
    ggplot2::coord_cartesian(xlim = c(-7.5, log(5)), ylim = c(min(d$row), max(d$row))) + 
    ggplot2::geom_text(size = 4.5, hjust = 0, vjust = 0.5, ggplot2::aes(x = x, y = y, label = label), data = labels) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = nrow(d) - 0.5)) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   plot.background  = ggplot2::element_blank(),
                   legend.position = "none",
                   panel.border = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   plot.margin = grid::unit(c(0,0,0.1,0), "lines"))
  return(plot)
}
