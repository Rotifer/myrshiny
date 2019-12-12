function(input, output, session) {
  db <- reactiveValues(pg_conn = NULL)
  observeEvent(input$btn_login, {
    user_name <- input$user_name
    password <- input$password
    msg <- try({
      db$pg_conn <- get_pg_connection(user_name, password)
      output$logged_status <-
        renderText(sprintf("%s logged in!", user_name))
      # show(selector = '#tabs li a[data-value="metis_tasks_screen"]')
      shinyjs::show(selector = '#tabs li a[data-value="blood_rcpt_storage_form"]')
      shinyjs::show(selector = '#tabs li a[data-value="manuf_sample_rcpt_proc_storage_form"]')
      shinyjs::show(selector = '#tabs li a[data-value="tissue_sample_receipt_storage_form"]')
      #updateSelectInput(session, "blood_receipt_storage_patient_uins", choices = c(c(""), get_patient_uins(db$pg_conn)))
      updateSelectInput(
        session,
        "blood_rcpt_storage_study_site_names",
        choices = c(c(""), get_study_site_names(db$pg_conn))
      )
      updateSelectInput(session,
                        "blood_rcpt_storage_study_codes",
                        choices = c(c(""), get_study_codes(db$pg_conn)))
      #updateSelectInput(session, "manufacturing_or_qc_patient_uins", choices = c(c(""), get_patient_uins(db$pg_conn)))
      updateSelectInput(
        session,
        "manuf_or_qc_samp_rcpt_proc_storage_site_names",
        choices = c(c(""), get_study_site_names(db$pg_conn))
      )
      updateSelectInput(session,
                        "manuf_or_qc_samp_rcpt_proc_storage_study_codes",
                        choices = c(c(""), get_study_codes(db$pg_conn)))
      updateSelectInput(
        session,
        "tissue_sample_receipt_storage_site_names",
        choices = c(c(""), get_study_site_names(db$pg_conn))
      )
      updateSelectInput(session,
                        "tissue_sample_receipt_storage_study_codes",
                        choices = c(c(""), get_study_codes(db$pg_conn)))
      })
    if(is.null(db$pg_conn)) {
      output$logged_status <-
        renderText(sprintf("Login error for user: %s", user_name))
    }
  })
  observeEvent(input$btn_logout, {
    try({
      close_pg_connection(db$pg_conn)
      output$logged_status <- renderText("Logged out!")
    })
    close_all_tabs()
  })
################################################## Blood Sample Receipt and Storage ##########################################
  observeEvent(input$blood_rcpt_storage_get_patient_uins, {
    site_name <- input$blood_rcpt_storage_study_site_names
    study_code <- input$blood_rcpt_storage_study_codes
    patient_uins <- c("", get_patient_uins_site_study(db$pg_conn, site_name, study_code))
    updateSelectInput(session,
                      "blood_rcpt_storage_study_site_patient_uins", 
                      choices = patient_uins)
  })
############################################## Manuf/QC Sample receipt processing and storage #####################################################################
  observeEvent(input$manuf_or_qc_samp_rcpt_proc_storage_get_patient_uins, {
    site_name <- input$manuf_or_qc_samp_rcpt_proc_storage_site_names
    study_code <- input$manuf_or_qc_samp_rcpt_proc_storage_study_codes
    patient_uins <- c("", get_patient_uins_site_study(db$pg_conn, site_name, study_code))
    updateSelectInput(session,
                      "manuf_or_qc_samp_rcpt_proc_storage_study_site_patient_uins", 
                      choices = patient_uins)
  })
############################################# Tissue Sample Receipt and Storage ################################################
  observeEvent(input$tissue_sample_receipt_storage_get_patient_uins, {
    site_name <- input$tissue_sample_receipt_storage_site_names
    study_code <- input$tissue_sample_receipt_storage_study_codes
    print(c(site_name, study_code))
    patient_uins <- c("", get_patient_uins_site_study(db$pg_conn, site_name, study_code))
    updateSelectInput(session,
                      "tissue_sample_receipt_storage_study_site_patient_uins", 
                      choices = patient_uins)    
  })
}
