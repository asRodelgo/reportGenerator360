#
source("global_utils.R", local = TRUE) #global functions available for the whole session
# 
function(input, output, session) {
  # download pre-generated PDF -----------------------------
  output$downloadReportStatic <- downloadHandler(
    filename = paste0("/srv/shiny-server/shinyTCMN-data/pdf/TCMN_Full_",.getCountryCode(input$inCouSel),".pdf"),
    content = function(file) file.copy(paste0("/srv/shiny-server/shinyTCMN-data/pdf/TCMN_Full_",.getCountryCode(input$inCouSel),".pdf"), file),
    contentType = 'application/pdf'
  )

  # download on the fly PDF ----------------------------
  output$downloadReport <- downloadHandler(
    filename = paste0(input$inTopic,"_",.getCountryCode(input$inCountry),".pdf"),
    content = function(file) {
      knit2pdf('PDF_LaTeX.Rnw', clean = TRUE,
               encoding = "UTF-8",
               output = paste0(input_reportID,"_",iso3,".tex"))
      # copy file to pdf directory
      file.copy(paste0(input_reportID,"_",iso3,".pdf"), paste0("templates/",input_reportID,"_final_pdf/"),overwrite=TRUE)
      file.remove(paste0(input_reportID,"_",iso3,".pdf"))
      file.remove(paste0(input_reportID,"_",iso3,".tex"))
    },
    contentType = 'application/pdf'
  )
  
}
  
  