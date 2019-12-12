library(shiny)
library(stringr)
library(shinyjs)
library(shinyalert)
library(shinydashboard)
library(DT)


jscode <- "
shinyjs.init = function(){
$('#tabs li a[data-value=blood_rcpt_storage_form]').hide();
$('#tabs li a[data-value=manuf_sample_rcpt_proc_storage_form]').hide();
$('#tabs li a[data-value=tissue_sample_receipt_storage_form]').hide();
}"


fluidPage(
  useShinyjs(),
  useShinyalert(),
  extendShinyjs(text = jscode),
  mainPanel(
    span(tags$h1("METIS Test"), style="color:red"),
    tabsetPanel(
      id = "tabs",
      tabPanel(
        "Log In/Out",
        selectInput("user_name",
                    "User Name",
                    choices = c(c(''), get_user_list())),
        passwordInput("password", "Password"),
        actionButton("btn_login", "Log In"),
        actionButton("btn_logout", "Log Out"),
        textOutput("logged_status")
      ),
      tabPanel(
        "Blood Sample Receipt and Storage",
        value = "blood_rcpt_storage_form",
        selectInput(
          "blood_rcpt_storage_study_site_names",
          "Select Centre",
          selected = NULL,
          choices = c(),
          multiple = FALSE
        ),
        selectInput(
          "blood_rcpt_storage_study_codes",
          "Select Study Code",
          selected = NULL,
          choices = c(),
          multiple = FALSE
        ),
        actionButton(
          "blood_rcpt_storage_get_patient_uins",
          "Get Patients for Site/Study"
        ),
        selectInput(
          "blood_rcpt_storage_study_site_patient_uins",
          "Patient UINs",
          selected = NULL,
          choices = c(),
          multiple = FALSE
        ),
        textInput("blood_rcpt_storage_isbt", "ISBT Identifier"),
        tags$hr(),
        textInput("blood_rcpt_storage_visit", "Visit"),
        tags$hr(),
        tags$h3("Timepoint"),
        fluidRow(column(6, selectInput("blood_rcpt_storage_timepoint_target", "Target Timepoint", choices = c("", "Day", "Week"))), 
                 column(6, numericInput("blood_rcpt_storage_timepoint_target_value", "Target Value", value = NULL))),
        fluidRow(column(6, selectInput("blood_rcpt_storage_timepoint_actual", "Actual Timepoint", choices = c("", "Day", "Week"))),
                 column(6, numericInput("blood_rcpt_storage_timepoint_actual_value", "Actual Value", value = NULL))),
        tags$h3("Samples Received"),
        tags$hr(),
        fluidRow(
          box(
            width = 12,
            title = "Sample Collected at Site Date-Time (24 Hour Clock)",
            splitLayout(
              dateInput(
                "blood_rcpt_storage_sample_collected_date",
                "Sample Collected Date",
                value = NULL,
                min = NULL,
                max = NULL,
                format = "yyyy-mm-dd",
                startview = "month",
                weekstart = 0,
                language = "en",
                width = NULL
              ),
              numericInput(
                "blood_rcpt_storage_sample_collected_hour",
                "Time: Hour",
                12,
                min = 0,
                max = 23,
                step = NA
              ),
              numericInput(
                "blood_rcpt_storage_sample_collected_minute",
                "Time: Minute",
                30,
                min = 0,
                max = 55,
                step = 5
              )
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Sample Received at RFH Date-Time (24 Hour Clock)",
            splitLayout(
              dateInput(
                "blood_rcpt_storage_sample_received_date",
                "Sample Received Date",
                value = NULL,
                min = NULL,
                max = NULL,
                format = "yyyy-mm-dd",
                startview = "month",
                weekstart = 0,
                language = "en",
                width = NULL
              ),
              numericInput(
                "blood_rcpt_storage_sample_received_hour",
                "Time: Hour",
                12,
                min = 0,
                max = 23,
                step = NA
              ),
              numericInput(
                "blood_rcpt_storage_sample_received_minute",
                "Time: Minute",
                30,
                min = 0,
                max = 55,
                step = 5
              )
            )
          )
        ),
        selectInput("blood_rcpt_storage_sample_received_by", "Samples Received at RFH By", choices = c("", "jen_middleton", "becki_pike")),
        tags$hr(),
        tags$h1("Blood Tube Type"),
        tags$h3("Sodium Heparin"),
        fluidRow(column(6, selectInput("blood_rcpt_storage_sample_blood_tube_type_sodium_haparin", "Sample received", choices = c("", "Yes", "No", "Not due"))),
                 column(6, numericInput("blood_rcpt_storage_sample_blood_tube_type_sodium_haparin_quantity", "Quantity received", value = NULL))),
        tags$h3("Sodium Citrate"),
        fluidRow(column(6, selectInput("blood_rcpt_storage_sample_blood_tube_type_sodium_citrate", "Sample received", choices = c("", "Yes", "No", "Not due"))),
                 column(6, numericInput("blood_rcpt_storage_sample_blood_tube_type_sodium_citrate_quantity", "Quantity received", value = NULL))), 
        tags$h3("Tempus RNA"),
        fluidRow(column(6, selectInput("blood_rcpt_storage_sample_blood_tube_type_tempus_rna", "Sample received", choices = c("", "Yes", "No", "Not due"))),
                 column(6, numericInput("blood_rcpt_storage_sample_blood_tube_type_tempus_rna_quantity", "Quantity received", value = NULL))),
        tags$h3("Streck DNA"),
        fluidRow(column(6, selectInput("blood_rcpt_storage_sample_blood_tube_type_streck_dna", "Sample received", choices = c("", "Yes", "No", "Not due"))),
                 column(6, numericInput("blood_rcpt_storage_sample_blood_tube_type_streck_dna_quantity", "Quantity received", value = NULL))),
        tags$hr(),
        tags$h3("Sample Integrity and Labelling Check"),
        selectInput("blood_rcpt_storage_sample_integrity_issues", "Any sample integrity issues identified?", choices = c("", "Yes", "No")),
        tags$hr(),
        textAreaInput(
          "blood_sample_receipt_comment",
          "Blood sample receipt comments",
          width = "1000px",
          height = "100px",
          value = " "
        ),
        actionButton(
          "btn_blood_rcpt_storage_sample_add_to_db",
          "Add form data to database"
        )
      ),
      ############################################## Manuf/QC Sample receipt processing and storage #####################################################################
      tabPanel(
        "Manufacturing Sample Receipt",
        value = "manuf_sample_rcpt_proc_storage_form",
        selectInput(
          "manuf_sample_rcpt_proc_storage_site_names",
          "Select Centre",
          selected = NULL,
          choices = c(),
          multiple = FALSE
        ),
        selectInput(
          "manuf_sample_rcpt_proc_storage_study_codes",
          "Select Study Code",
          selected = NULL,
          choices = c(),
          multiple = FALSE
        ),
        actionButton(
          "manuf_sample_rcpt_proc_storage_get_patient_uins",
          "Get Patients for Site/Study"
        ),
        selectInput(
          "manuf_sample_rcpt_proc_storage_study_site_patient_uins",
          "Patient UINs",
          selected = NULL,
          choices = c(),
          multiple = FALSE
        ),
        tags$hr(),
        textInput("manuf_sample_rcpt_proc_storage_isbt", "ISBT Identifier"),
        tags$h1("Samples Received"),
        fluidRow(
          box(
            width = 12,
            title = "Received Date-Time (24 Hour Clock)",
            splitLayout(
              dateInput(
                "manuf_sample_rcpt_proc_storage_received_date",
                "Sample Received Date",
                value = NULL,
                min = NULL,
                max = NULL,
                format = "yyyy-mm-dd",
                startview = "month",
                weekstart = 0,
                language = "en",
                width = NULL
              ),
              numericInput(
                "manuf_sample_rcpt_proc_storage_received_hour",
                "Time: Hour",
                12,
                min = 0,
                max = 23,
                step = NA
              ),
              numericInput(
                "manuf_sample_rcpt_proc_storage_received_minute",
                "Time: Minute",
                30,
                min = 0,
                max = 55,
                step = 5
              )
            )
          )
        ),
        tags$hr(),
        selectInput("manuf_sample_rcpt_proc_storage_sample_received_by", "Samples Received at RFH By", choices = c("", "jen_middleton", "becki_pike")),
        tags$h3("TILs (for TCR sequencing)"),
        fluidRow(column(6, selectInput("manuf_sample_rcpt_proc_storage_tils_for_tcr_seq", "Sample received", choices = c("", "Yes", "No", "Not due"))), 
                 column(6, numericInput("manuf_sample_rcpt_proc_storage_tils_for_tcr_seq_quantity", "Quantity received", value = NULL))),
        tags$h3("cNeT (for TCR sequencing)"),
        fluidRow(column(6, selectInput("manuf_sample_rcpt_proc_storage_cnet_for_tcr_seq", "Sample received", choices = c("", "Yes", "No", "Not due"))), 
                 column(6, numericInput("manuf_sample_rcpt_proc_storage_cnet_for_tcr_seq_quantity", "Quantity received", value = NULL))), 
        tags$h3("CD14-negative fraction"),
        fluidRow(column(6, selectInput("manuf_sample_rcpt_proc_storage_cd14_neg_fraction", "Sample received", choices = c("", "Yes", "No", "Not due"))), 
                 column(6, numericInput("manuf_sample_rcpt_proc_storage_cd14_neg_fraction_quantity_10x6", "Quantity received (x10^6)", value = NULL))),         
        tags$h3("Excess TILs"),
        fluidRow(column(6, selectInput("manuf_sample_rcpt_proc_storage_excess_tils", "Sample received", choices = c("", "Yes", "No", "Not due"))), 
                 column(6, numericInput("manuf_sample_rcpt_proc_storage_excess_tils_quantity_10x6", "Quantity received (x10^6)", value = NULL))),  
        tags$h3("Excess MDCs"),
        fluidRow(column(6, selectInput("manuf_sample_rcpt_proc_storage_excess_mdcs", "Sample received", choices = c("", "Yes", "No", "Not due"))), 
                 column(6, numericInput("manuf_sample_rcpt_proc_storage_excess_mdcs_quantity_10x6", "Quantity received (x10^6)", value = NULL))), 
        tags$h3("cNeT frozen vials (for FIO assays)"),
        fluidRow(column(6, selectInput("manuf_sample_rcpt_proc_storage_cnet_frozen_vials", "Sample received", choices = c("", "Yes", "No", "Not due"))), 
                 column(6, numericInput("manuf_sample_rcpt_proc_storage_cnet_frozen_vials_quantity_10x6", "Quantity received", value = NULL))),
        tags$hr(),
        tags$h3("Pleural Fluid"),
        fluidRow(column(6, selectInput("manuf_sample_rcpt_proc_storage_pleural_fluid", "Sample received", choices = c("", "Yes", "No", "Not due"))),
                 column(6, numericInput("manuf_sample_rcpt_proc_storage_pleural_fluid_quantity", "Quantity received", value = NULL))),
        tags$h3("Streck DNA"),
        fluidRow(column(6, selectInput("manuf_sample_rcpt_proc_storage_streck_dna", "Sample received", choices = c("", "Yes", "No", "Not due"))),
                 column(6, numericInput("manuf_sample_rcpt_proc_storage_pleural_streck_dna_quantity", "Quantity received", value = NULL))),        
        tags$hr(),
        tags$h3("Sample Integrity and Labelling Check"),
        selectInput("manuf_sample_rcpt_proc_storage_sample_integrity_issues", "Any sample integrity issues identified?", choices = c("", "Yes", "No")),
        tags$hr(),
        textAreaInput(
          "manuf_sample_receipt_comment",
          "Manufacturing sample receipt comments",
          width = "1000px",
          height = "100px",
          value = " "
        ),
        actionButton(
          "btn_manuf_sample_rcpt_proc_storage_add_to_db",
          "Add form data to database"
        )
      ),
############################################################### Tissue Sample Receipt ##################################################################
      tabPanel(
        "Tissue Sample Receipt",
        value = "tissue_sample_receipt_storage_form",
        selectInput(
          "tissue_sample_receipt_storage_site_names",
          "Select Centre",
          selected = NULL,
          choices = c(),
          multiple = FALSE
        ),
        selectInput(
          "tissue_sample_receipt_storage_study_codes",
          "Select Study Code",
          selected = NULL,
          choices = c(),
          multiple = FALSE
        ),
        actionButton(
          "tissue_sample_receipt_storage_get_patient_uins",
          "Get Patients for Site/Study"
        ),
        selectInput(
          "tissue_sample_receipt_storage_study_site_patient_uins",
          "Patient UINs",
          selected = NULL,
          choices = c(),
          multiple = FALSE
        ),
        tags$hr(),
        textInput("tissue_sample_receipt_proc_storage_isbt", "ISBT Identifier"),
        tags$hr(),
        textInput("tissue_sample_receipt_storage_visit", "Visit"),
        tags$hr(),
        tags$h3("Timepoint"),
        fluidRow(column(6, selectInput("tissue_sample_receipt_storage_timepoint_target", "Target Timepoint", choices = c("", "Day", "Week"))), 
                 column(6, numericInput("tissue_sample_receipt_storage_timepoint_target_value", "Target Value", value = NULL))),
        fluidRow(column(6, selectInput("tissue_sample_receipt_storage_timepoint_actual", "Actual Timepoint", choices = c("", "Day", "Week"))),
                 column(6, numericInput("tissue_sample_receipt_storage_timepoint_actual_value", "Actual Value", value = NULL))),
        tags$hr(),
        tags$h3("Fixed Samples Received"),
        fluidRow(
          box(
            width = 12,
            title = "Sample Collected at Site Date-Time (24 Hour Clock)",
            splitLayout(
              dateInput(
                "tissue_sample_receipt_storage_sample_collected_date",
                "Sample Collected Date",
                value = NULL,
                min = NULL,
                max = NULL,
                format = "yyyy-mm-dd",
                startview = "month",
                weekstart = 0,
                language = "en",
                width = NULL
              ),
              numericInput(
                "tissue_sample_receipt_storage_sample_collected_hour",
                "Time: Hour",
                12,
                min = 0,
                max = 23,
                step = NA
              ),
              numericInput(
                "tissue_sample_receipt_storage_sample_collected_minute",
                "Time: Minute",
                30,
                min = 0,
                max = 55,
                step = 5
              )
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Sample Received at RFH Date-Time (24 Hour Clock)",
            splitLayout(
              dateInput(
                "tissue_sample_receipt_storage_fixed_sample_received_date",
                "Sample Received Date",
                value = NULL,
                min = NULL,
                max = NULL,
                format = "yyyy-mm-dd",
                startview = "month",
                weekstart = 0,
                language = "en",
                width = NULL
              ),
              numericInput(
                "tissue_sample_receipt_storage_fixed_sample_received_hour",
                "Time: Hour",
                12,
                min = 0,
                max = 23,
                step = NA
              ),
              numericInput(
                "tissue_sample_receipt_storage_fixed_sample_received_minute",
                "Time: Minute",
                30,
                min = 0,
                max = 55,
                step = 5
              )
            )
          )
        ),
        selectInput("tissue_sample_receipt_storage_fixed_sample_received_at_rfh_by", "Sample Received at RFH By", choices = c("", "jen_middleton", "becki_pike")),
        tags$h3("Archival Tissue Block"),
        fluidRow(column(6, selectInput("tissue_sample_receipt_storage_archival_tissue_block", "Sample received", choices = c("", "Yes", "No", "Not due"))), 
                 column(6, numericInput("tissue_sample_receipt_storage_archival_tissue_block_quantity", "Quantity received", value = NULL))),
        tags$h3("Archival Tissue Slides"),
        fluidRow(column(6, selectInput("tissue_sample_receipt_storage_archival_tissue_slides", "Sample received", choices = c("", "Yes", "No", "Not due"))), 
                 column(6, numericInput("tissue_sample_receipt_storage_archival_tissue_slides_quantity", "Quantity received", value = NULL))),
        tags$h3("Archival FFPE Curls"),
        fluidRow(column(6, selectInput("tissue_sample_receipt_storage_archival_ffpe_curls", "Sample received", choices = c("", "Yes", "No", "Not due"))), 
                 column(6, numericInput("tissue_sample_receipt_storage_archival_ffpe_curls_quantity", "Quantity received", value = NULL))),        
        tags$h3("Optional Tissue Biopsy"),
        fluidRow(column(6, selectInput("tissue_sample_receipt_storage_optional_tissue_biopsy", "Sample received", choices = c("", "Yes", "No", "Not due"))), 
                 column(6, numericInput("tissue_sample_receipt_storage_optional_tissue_biopsy_quantity", "Quantity received", value = NULL))),        
        
        tags$hr(),
        tags$h3("Fresh Samples Received"),
        fluidRow(
          box(
            width = 12,
            title = "Sample Received at RFH Date-Time (24 Hour Clock)",
            splitLayout(
              dateInput(
                "tissue_sample_receipt_storage_fresh_sample_received_date",
                "Sample Received Date",
                value = NULL,
                min = NULL,
                max = NULL,
                format = "yyyy-mm-dd",
                startview = "month",
                weekstart = 0,
                language = "en",
                width = NULL
              ),
              numericInput(
                "tissue_sample_receipt_storage_fresh_sample_received_hour",
                "Time: Hour",
                12,
                min = 0,
                max = 23,
                step = NA
              ),
              numericInput(
                "tissue_sample_receipt_storage_fresh_sample_received_minute",
                "Time: Minute",
                30,
                min = 0,
                max = 55,
                step = 5
              )
            )
          )
        ),
        selectInput("tissue_sample_receipt_storage_fresh_sample_received_at_rfh_by", 
                    "Sample Received at RFH By", choices = c("", "jen_middleton", "becki_pike")),
        tags$h3("Procurement Sample for Embedding"),
        fluidRow(column(6, selectInput("tissue_sample_receipt_storage_procurement_sample_embedding", "Sample received", choices = c("", "Yes", "No", "Not due"))), 
                 column(6, numericInput("tissue_sample_receipt_storage_procurement_sample_embedding_quantity", "Quantity received", value = NULL))),
        tags$h3("Procurement Sample for RNA Stabilisation"),
        fluidRow(column(6, selectInput("tissue_sample_receipt_storage_procurement_sample_rna_stabilisation", "Sample received", choices = c("", "Yes", "No", "Not due"))), 
                 column(6, numericInput("tissue_sample_receipt_storage_procurement_sample_rna_stabilisation_quantity", "Quantity received", value = NULL))),        
        tags$hr(),
        tags$h3("Sample Integrity and Labelling Check"),
        selectInput("tissue_sample_receipt_storage_sample_integrity_issues", "Any sample integrity issues identified?", choices = c("", "Yes", "No")),
        tags$hr(),
        textAreaInput(
          "tissue_sample_receipt_comment",
          "Tissue sample receipt comments",
          width = "1000px",
          height = "100px",
          value = " "
        ),
        actionButton(
          "tissue_sample_receipt_storage_add_to_db",
          "Add form data to database"
        )        
      )
    )
  )
)