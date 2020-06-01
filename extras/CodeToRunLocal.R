library(Covid19EstimationHydroxychloroquine2)

options(fftempdir = "S:/FFTemp")
maxCores <- parallel::detectCores()
studyFolder <- "G:/StudyResults/Covid19EstimationHcqPsychInfluenza"

source("S:/MiscCode/SetEnvironmentVariables.R")
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "pdw",
                                                                server = Sys.getenv("server"),
                                                                user = NULL,
                                                                password = NULL,
                                                                port = Sys.getenv("port"))

mailSettings <- list(from = Sys.getenv("emailAddress"),
                     to = c(Sys.getenv("emailAddress")),
                     smtp = list(host.name = Sys.getenv("emailHost"), port = 25,
                                 user.name = Sys.getenv("emailAddress"),
                                 passwd = Sys.getenv("emailPassword"), ssl = FALSE),
                     authenticate = FALSE,
                     send = TRUE)

# CCAE settings ---------------------------------------------------------------- running
databaseId <- "CCAE"
databaseName <- "CCAE"
databaseDescription <- "CCAE"
cdmDatabaseSchema <- "CDM_IBM_CCAE_V1103.dbo"
outputFolder <- file.path(studyFolder, databaseId)
cohortDatabaseSchema = "scratch.dbo"
cohortTable = "covid19_hcq_psych_ccae"

# Optum DOD settings -----------------------------------------------------------  done
databaseId <- "Clinformatics"
databaseName <- "Clinformatics"
databaseDescription <- "Clinformatics"
cdmDatabaseSchema = "CDM_OPTUM_EXTENDED_DOD_V1107.dbo"
outputFolder <- file.path(studyFolder, databaseId)
cohortDatabaseSchema <- "scratch.dbo"
cohortTable <- "covid19_hcq_psych_optum"

# CPRD settings ---------------------------------------------------------------- 
databaseId <- "CPRD"
databaseName <- "CPRD"
databaseDescription <- "CPRD"
cdmDatabaseSchema = "CDM_CPRD_V1102.dbo"
outputFolder <- file.path(studyFolder, databaseId)
cohortDatabaseSchema <- "scratch.dbo"
cohortTable <- "covid19_hcq_psych_cprd"

# MDCD settings ---------------------------------------------------------------- 
databaseId <- "MDCD"
databaseName <- "MDCD"
databaseDescription <- "MDCD"
cdmDatabaseSchema = "CDM_IBM_MDCD_V1105.dbo"
outputFolder <- file.path(studyFolder, databaseId)
cohortDatabaseSchema <- "scratch.dbo"
cohortTable <- "covid19_hcq_psych_mdcd"

# MDCR settings ----------------------------------------------------------------
databaseId <- "MDCR"
databaseName <- "MDCR"
databaseDescription <- "MDCR"
cdmDatabaseSchema = "CDM_IBM_MDCR_V1104.dbo"
outputFolder <- file.path(studyFolder, databaseName)
cohortDatabaseSchema <- "scratch.dbo"
cohortTable <- "covid19_hcq_psych_mdcr"

# JMDC ------------------------------------------------------------------------- 
databaseId <- "JMDC"
databaseName <- "JMDC"
databaseDescription <- "JMDC"
cdmDatabaseSchema = "CDM_JMDC_V1106.dbo"
outputFolder <- file.path(studyFolder, databaseName)
cohortDatabaseSchema <- "scratch.dbo"
cohortTable <- "covid19_hcq_psych_jmdc"

# PanTher ----------------------------------------------------------------------
databaseId <- "OptumEHR"
databaseName <- "OptumEHR"
databaseDescription <- "OptumEHR"
cdmDatabaseSchema = "CDM_OPTUM_PANTHER_V1109.dbo"
outputFolder <- file.path(studyFolder, databaseName)
cohortDatabaseSchema <- "scratch.dbo"
cohortTable <- "covid19_hcq_psych_panther"

# IPCI -------------------------------------------------------------------------
databaseId <- "IPCI"
databaseName <- "IPCI"
outputFolder <- file.path(studyFolder, databaseName)

# DAGermany --------------------------------------------------------------------
databaseId <- "DAGermany"
databaseName <- "DAGermany"
outputFolder <- file.path(studyFolder, databaseName)

# VA ---------------------------------------------------------------------------
databaseId <- "VA"
databaseName <- "VA"
outputFolder <- file.path(studyFolder, databaseName)

# SIDIAP -----------------------------------------------------------------------
databaseId <- "sidiap17"
databaseName <- "SIDIAP"
outputFolder <- file.path(studyFolder, databaseName)

# IMRD -------------------------------------------------------------------------
databaseId <- "IMRD"
databaseName <- "IMRD"
outputFolder <- file.path(studyFolder, databaseName)

# AmbEMR -----------------------------------------------------------------------
databaseId <- "AmbEMR"
databaseName <- "AmbEMR"
outputFolder <- file.path(studyFolder, databaseName)

# OpenClaims -------------------------------------------------------------------
databaseId <- "OpenClaims"
databaseName <- "OpenClaims"
outputFolder <- file.path(studyFolder, databaseName)

# NHIS-NSC ---------------------------------------------------------------------
databaseId <- "NHIS-NSC"
databaseName <- "NHIS-NSC"
outputFolder <- file.path(studyFolder, databaseName)

# Run --------------------------------------------------------------------------
OhdsiRTools::runAndNotify(expression = {
  execute(connectionDetails = connectionDetails,
          cdmDatabaseSchema = cdmDatabaseSchema,
          cohortDatabaseSchema = cohortDatabaseSchema,
          cohortTable = cohortTable,
          oracleTempSchema = NULL,
          outputFolder = outputFolder,
          databaseId = databaseId,
          databaseName = databaseName,
          databaseDescription = databaseDescription,
          createCohorts = TRUE,
          synthesizePositiveControls = FALSE,
          runAnalyses = TRUE,
          runDiagnostics = FALSE,
          packageResults = TRUE,
          maxCores = maxCores)
}, mailSettings = mailSettings, label = paste0("Covid19EstimationHydroxychloroquine2 ", databaseId), stopOnWarning = FALSE)

resultsZipFile <- file.path(outputFolder, "export", paste0("Results", databaseId, ".zip"))
dataFolder <- file.path(outputFolder, "shinyData")
prepareForEvidenceExplorer(resultsZipFile = resultsZipFile, dataFolder = dataFolder)

fullShinyDataFolder <- file.path(studyFolder, "shinyData")
if (!file.exists(fullShinyDataFolder)) {
  dir.create(fullShinyDataFolder)
}
file.copy(from = c(list.files(file.path(studyFolder, "CCAE", "shinyData"), full.names = TRUE),
                   list.files(file.path(studyFolder, "Clinformatics", "shinyData"), full.names = TRUE)
                   # list.files(file.path(studyFolder, "CPRD", "shinyData"), full.names = TRUE),
                   # list.files(file.path(studyFolder, "MDCD", "shinyData"), full.names = TRUE),
                   # list.files(file.path(studyFolder, "MDCR", "shinyData"), full.names = TRUE),
                   # list.files(file.path(studyFolder, "JMDC", "shinyData"), full.names = TRUE),
                   # list.files(file.path(studyFolder, "OptumEHR", "shinyData"), full.names = TRUE),
                   # list.files(file.path(studyFolder, "DAGermany", "shinyData"), full.names = TRUE),
                   # list.files(file.path(studyFolder, "VA", "shinyData"), full.names = TRUE),
                   # list.files(file.path(studyFolder, "IMRD", "shinyData"), full.names = TRUE),
                   # list.files(file.path(studyFolder, "OpenClaims", "shinyData"), full.names = TRUE),
                   # list.files(file.path(studyFolder, "AmbEMR", "shinyData"), full.names = TRUE),
                   # list.files(file.path(studyFolder, "SIDIAP", "shinyData"), full.names = TRUE),
                   # list.files(file.path(studyFolder, "IPCI", "shinyData"), full.names = TRUE),
                   # list.files(file.path(studyFolder, "MetaAnalysis", "shinyData"), full.names = TRUE)
                   ),
          to = fullShinyDataFolder,
          overwrite = TRUE)

launchEvidenceExplorer(dataFolder = fullShinyDataFolder, blind = FALSE, launch.browser = FALSE)
