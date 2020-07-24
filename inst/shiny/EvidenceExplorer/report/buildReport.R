# create report document

doc <- officer::read_docx() %>%

# section 1: patient counts

  officer::body_add_fpar(section1, style = "heading 1") %>%
  flextable::body_add_flextable(counts) %>%
  officer::body_add_break()
  
# section 2: table 1s

  doc <- doc %>% 
    officer::body_add_fpar(section2, style = "heading 1")
    for(i in 1:length(table1s)) { #i=1
      doc <- doc %>%
        officer::body_add_fpar(table1Pairs[[1]][[i]], style = "heading 2") %>%
        flextable::body_add_flextable(table1Pairs[[2]][[i]]) %>%
        officer::body_add_break()
    }

# section 3: diagnostics

  doc <- doc %>% 
    officer::body_add_fpar(section3, style = "heading 1")
    for(i in 1:length(diagnosticsFileNames)) { #i=1
      doc <- doc %>%
        officer::body_add_fpar(diagnosticsTitles[[i]], style = "heading 2") %>%
        officer::body_add_img(diagnosticsPlotPairs[[2]][[i]], width = 5, height = 7) %>%
        officer::body_add_break()
    }

# section 4: IR tables

  doc <- doc %>% 
    officer::body_add_fpar(section4, style = "heading 1")
    for(i in 1:length(eventTables)) { #i=1
      doc <- doc %>%
        officer::body_add_fpar(eventTablePairs[[1]][[i]], style = "heading 2") %>%
        flextable::body_add_flextable(eventTablePairs[[2]][[i]]) %>%
        officer::body_add_break()
    }

# section 5: HR plots
  
  doc <- doc %>% 
    officer::body_add_fpar(section5, style = "heading 1") %>%
    officer::body_add_fpar(hrTitles[[1]], style = "heading 2") %>%
    officer::body_add_img(hrFileNames[[1]], width = 5, height = 4) %>%
    officer::body_add_fpar(hrTitles[[2]], style = "heading 2") %>%
    officer::body_add_img(hrFileNames[[2]], width = 5, height = 4) %>%
    
# print full report
    print(target = file.path(reportFolder, "tablesFigures.docx"))