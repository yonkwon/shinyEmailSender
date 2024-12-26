list.of.packages <- c("shiny", "mailR", "readr", "DT")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) {install.packages(new.packages); library(new.package)}

# Define UI ----
ui <- fluidPage(
  titlePanel("SMTP Email Sender with Shiny"),
  
  sidebarLayout(
    sidebarPanel(
      h3("SMTP Configuration"),
      textInput("smtpHost", "SMTP Host", "smtp.gmail.com"),
      numericInput("smtpPort", "SMTP Port", 587),
      textInput("smtpUsername", "SMTP Username", "your_email@gmail.com"),
      passwordInput("smtpPassword", "Google App Password", ""),
      checkboxInput("smtpTLS", "Use TLS", TRUE),
      
      tags$p(
        "Don't have a Google App Password? ",
        tags$a("Learn how to set up a Google App Password", 
               href = "https://support.google.com/accounts/answer/185833", 
               target = "_blank")
      ),
      
      h3("Email Details"),
      textInput("emailSubject", "Email Subject", "Hello {name}"),
      textAreaInput("emailBody", "Email Body", 
                    "Hello {name},

This is a personalized message just for you!

Best regards,  
Your Name"),
      
      h4("Load Recipients"),
      fileInput("csvFile", "Upload CSV File (name, email)", accept = ".csv"),
      downloadButton("downloadEmptyCSV", "Download Empty Contact CSV"),
      
      h4("Attach a File"),
      fileInput("fileAttachment", "Upload File to Attach (Optional)")
    ),
    
    mainPanel(
      h3("Email Preview"),
      verbatimTextOutput("previewOutput"),
      actionButton("sendEmails", "Send Emails", style = "margin-top: 10px; margin-bottom: 30px;"),
      
      h3("Recipient Table"),
      DTOutput("contactTable"),
      
      h3("Email Log"),
      verbatimTextOutput("outputLog"),
      downloadButton("downloadLog", "Download Email Log", style = "margin-top: 10px;")
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  
  # Reactive values for email log and status
  emailLog <- reactiveVal(data.frame(
    name = character(), 
    email = character(), 
    status = character(),
    error = character(),
    stringsAsFactors = FALSE
  ))
  
  # Default preview on startup
  observe({
    updateTextInput(session, "emailSubject", value = "Hello {name}")
    updateTextAreaInput(session, "emailBody", value = 
                          "Hello {name},

This is a personalized message just for you!

Best regards,  
Your Name")
  })
  
  # Dynamic Email Preview
  output$previewOutput <- renderPrint({
    subject <- gsub("\\{name\\}", "Receiver", input$emailSubject)
    body <- gsub("\\{name\\}", "Receiver", input$emailBody)
    
    cat("To: Receiver (receiver@example.com)\n")
    cat("Subject:", subject, "\n\n")
    cat(body)
    if (!is.null(input$fileAttachment)) {
      cat("\nAttachment: ", input$fileAttachment$name)
    }
  })
  
  # Load Contact CSV and Display Table
  contactData <- reactiveVal(data.frame())
  
  observeEvent(input$csvFile, {
    req(input$csvFile)
    data <- read_csv(input$csvFile$datapath, col_types = cols())
    
    if (!all(c("name", "email") %in% names(data))) {
      showNotification("CSV file must contain 'name' and 'email' columns.", type = "error")
      return()
    }
    
    data$status <- "Pending"
    data$error <- ""
    contactData(data)
  })
  
  output$contactTable <- renderDT({
    datatable(contactData(), editable = FALSE, rownames = FALSE)
  })
  
  # Send Emails
  observeEvent(input$sendEmails, {
    # Validate SMTP Configurations
    if (input$smtpHost == "" || input$smtpPort == 0 || input$smtpUsername == "" || input$smtpPassword == "") {
      emailLog(data.frame(
        name = "N/A", 
        email = "N/A", 
        status = "Failed",
        error = "SMTP configuration is incomplete. Please fill all required fields."
      ))
      showNotification("SMTP configuration is incomplete!", type = "error")
      return()
    }
    
    # Validate Recipients
    data <- contactData()
    if (nrow(data) == 0) {
      emailLog(data.frame(
        name = "N/A", 
        email = "N/A", 
        status = "Failed",
        error = "No recipients uploaded. Please upload a valid CSV file."
      ))
      showNotification("No recipients found. Upload a CSV file.", type = "error")
      return()
    }
    
    log <- data.frame(
      name = character(), 
      email = character(), 
      status = character(),
      error = character(),
      stringsAsFactors = FALSE
    )
    
    smtp <- list(
      host.name = input$smtpHost,
      port = input$smtpPort,
      user.name = input$smtpUsername,
      passwd = input$smtpPassword,
      tls = input$smtpTLS
    )
    
    attachment_path <- NULL
    if (!is.null(input$fileAttachment)) {
      attachment_path <- input$fileAttachment$datapath
    }
    
    for (i in 1:nrow(data)) {
      recipient_name <- data$name[i]
      recipient_email <- data$email[i]
      
      email_subject <- gsub("\\{name\\}", recipient_name, input$emailSubject)
      email_body <- gsub("\\{name\\}", recipient_name, input$emailBody)
      
      tryCatch({
        send.mail(
          from = input$smtpUsername,
          to = recipient_email,
          subject = email_subject,
          body = email_body,
          smtp = smtp,
          authenticate = TRUE,
          attach.files = attachment_path,
          send = TRUE
        )
        data$status[i] <- "Success"
        data$error[i] <- ""
        log <- rbind(log, data.frame(name = recipient_name, email = recipient_email, status = "Success", error = ""))
      }, error = function(e) {
        data$status[i] <- "Failed"
        data$error[i] <- e$message
        log <- rbind(log, data.frame(name = recipient_name, email = recipient_email, status = "Failed", error = e$message))
      })
    }
    
    contactData(data)
    emailLog(log)
    showNotification("Emails have been processed. Check the table for results.", type = "message")
  })
  
  # Download Empty Contact CSV
  output$downloadEmptyCSV <- downloadHandler(
    filename = "empty_contact_template.csv",
    content = function(file) {
      write.csv(data.frame(name = "John Doe", email = "john@example.com"), file, row.names = FALSE)
    }
  )
  
  # Display Email Log
  output$outputLog <- renderPrint({
    log <- emailLog()
    if (nrow(log) == 0) {
      cat("No email activity logged yet.")
    } else {
      print(log)
    }
  })
  
  output$downloadLog <- downloadHandler(
    filename = "email_log.csv",
    content = function(file) {
      write.csv(emailLog(), file, row.names = FALSE)
    }
  )
}

# Run the app ----
shinyApp(ui, server)
