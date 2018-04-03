library(shiny)
library(shinydashboard)
library(dplyr)
library(pdftools)
library(stringr)
library(stringi)
library(textreadr)
library(RCurl)




ui =
  
dashboardPage(skin = 'black',
                
dashboardHeader(title = "Resume Parser"),
                
dashboardSidebar(
  
sidebarMenu(
fileInput('file', 'Choose Files', multiple = TRUE, accept = c('.pdf', '.doc', '.docx')),
selectInput("columns", "Select Stream", choices = NULL, selectize = FALSE),
actionButton("submit", "Analyze")
)

),

dashboardBody(
fluidRow(
box(title = "Table", solidHeader = TRUE, width = 12,collapsible = TRUE, color="purple",div(style = 'overflow-x: scroll',DT::dataTableOutput("table1")))
)
)
)


server = function(input, output, session) { 
   
skills =  read.csv("https://www.dropbox.com/s/cqa0yjbxob8jywt/skills.csv?dl=1", stringsAsFactors = FALSE)
is.na(skills) = skills==''
vars = names(skills)
  
updateSelectInput(session = session, inputId = "columns", choices = vars)
  
file_names = eventReactive(input$submit, {
  
    
    file_name = input$file$name
    pdf_files = grepl("*.pdf", file_name)
    doc_files = grepl("*.doc$", file_name)
    docx_files = grepl("*.docx", file_name)
    pdf_list = file_name[pdf_files]
    doc_list = file_name[doc_files]
    docx_list = file_name[docx_files]
    paths = input$file$datapath
    paths = as.character(paths)
    
    pdf_filenames = grepl("*.pdf", paths)
    doc_filenames = grepl("*.doc$", paths)
    docx_filenames = grepl("*.docx", paths)
    
    pdf_paths = paths[pdf_filenames]
    doc_paths = paths[doc_filenames]
    docx_paths = paths[docx_filenames]
    
    
    
    output_df = data.frame()
    doc_df = data.frame()
    docx_df = data.frame()
    
    if (!identical(pdf_list, character(0))){
      
    for (i in 1: length(pdf_paths))
    {
      
      candidate_name = stri_extract_first(str = pdf_list[i], regex = ".*(?=\\.)")
      
      text = pdf_text(pdf_paths[i])
      text_df = data_frame(text = text)
      text_df$text = tolower(text_df$text)
      selected_skill = input$columns
      skill_mat = matrix(0, ncol = length(unique(skills[,selected_skill][!is.na(skills[,selected_skill])])))
      
      skill_df = data.frame(skill_mat)
      
      colnames(skill_df) = unique(skills[,selected_skill][!is.na(skills[,selected_skill])])
      for (n in 1:length(skills[,selected_skill][!is.na(skills[,selected_skill])])){
        for (m in 1:length(text))
        {
          
          skill_df[m,n] = str_detect(text_df$text[m], skills[,selected_skill][n])
        }
      }
      
      final_df = cbind(text_df, skill_df)
      
      final_df_length = length(final_df)
      
      final_table = matrix(0, ncol = length(unique(skills[,selected_skill][!is.na(skills[,selected_skill])]))+1)
      
      final_table = data.frame(final_table)
      
      colnames(final_table) = colnames(final_df)
      
      candidate_name2 = str_replace_all(candidate_name, "_|-", " ")
      candidate_name3 = str_replace_all(candidate_name2, "Resume|resume", " ")
      
      final_table$text = candidate_name3
      
      names(final_table)[1] = "candidate_name"
      
      for (o in 2:length(colnames(final_table)))
      {
        final_table[o] = if (sum(final_df[o]) >0) {1} else {0}
      }
      totals = length(final_table)-1
      final_table$matches = rowSums(final_table[,-1])
      final_table$totals = totals
      final_table$percentage_matches = round((final_table$matches/totals)*100, 0)
      email = as.character(na.omit(str_extract(text_df$text, "\\S*@\\S*")))
      email2 = str_replace_all(email, "[^[:alnum:]@.]", " ")
      
      email3 = sub(".*? (.+)", "\\1", email2)
      if(identical(email3, character(0)))
      {
        final_table$email = "Not Available"
      }
      else
      {
        final_table$email = email3
      }
      
      phone = na.omit(str_extract(text_df$text, "\\(?\\d{3}\\)?[.-]? *\\d{3}[.-]? *[.-]?\\d{4}"))
      phone_number = gsub(" ","",phone)
      
      phone2 = as.numeric(phone_number)
      if(identical(phone2, numeric(0)))
      {
        final_table$phone = "Not Available"
      }
      else
      {
        final_table$phone = phone2
      }
      
      final_table = final_table %>% select(candidate_name, email, phone, everything())
      
      output_df = rbind(output_df, final_table)
      
      
    } }
    
    if (!identical(docx_list, character(0)))
    {
    
    for (i in 1: length(docx_paths))
    {
      
      candidate_name = stri_extract_first(str = docx_list[i], regex = ".*(?=\\.)")
      
      text = read_docx(docx_paths[i])
      text_df = data_frame(text = text)
      text_df$text = tolower(text_df$text)
      selected_skill = input$columns
      skill_mat = matrix(0, ncol = length(unique(skills[,selected_skill][!is.na(skills[,selected_skill])])))
      
      skill_df = data.frame(skill_mat)
      
      colnames(skill_df) = unique(skills[,selected_skill][!is.na(skills[,selected_skill])])
      for (n in 1:length(skills[,selected_skill][!is.na(skills[,selected_skill])])){
        for (m in 1:length(text))
        {
          
          skill_df[m,n] = str_detect(text_df$text[m], skills[,selected_skill][n])
        }
      }
      
      final_df = cbind(text_df, skill_df)
      
      final_df_length = length(final_df)
      
      final_table = matrix(0, ncol = length(unique(skills[,selected_skill][!is.na(skills[,selected_skill])]))+1)
      
      final_table = data.frame(final_table)
      
      colnames(final_table) = colnames(final_df)
      
      candidate_name2 = str_replace_all(candidate_name, "_|-", " ")
      candidate_name3 = str_replace_all(candidate_name2, "Resume|resume", " ")
      
      final_table$text = candidate_name3
      
      names(final_table)[1] = "candidate_name"
      
      for (o in 2:length(colnames(final_table)))
      {
        final_table[o] = if (sum(final_df[o]) >0) {1} else {0}
      }
      totals = length(final_table)-1
      final_table$matches = rowSums(final_table[,-1])
      final_table$totals = totals
      final_table$percentage_matches = round((final_table$matches/totals)*100, 0)
      email = as.character(na.omit(str_extract(text_df$text, "\\S*@\\S*")))
      email2 = str_replace_all(email, "[^[:alnum:]@.]", " ")
      
      email3 = sub(".*? (.+)", "\\1", email2)
      if(identical(email3, character(0)))
      {
        final_table$email = "Not Available"
      }
      else
      {
        final_table$email = email3
      }
      
      phone = na.omit(str_extract(text_df$text, "\\(?\\d{3}\\)?[.-]? *\\d{3}[.-]? *[.-]?\\d{4}"))
      phone_number = gsub(" ","",phone)
      
      phone2 = as.numeric(phone_number)
      if(identical(phone2, numeric(0)))
      {
        final_table$phone = "Not Available"
      }
      else
      {
        final_table$phone = phone2
      }
      
      final_table = final_table %>% select(candidate_name, email, phone, everything())
      
      docx_df = rbind(docx_df, final_table)
      
      
    }}
    
    if (!identical(doc_list, character(0)))
    {
    for (i in 1: length(doc_paths))
    {
      
      candidate_name = stri_extract_first(str = doc_list[i], regex = ".*(?=\\.)")
      
      text = read_doc(doc_paths[i])
      text_df = data_frame(text = text)
      text_df$text = tolower(text_df$text)
      selected_skill = input$columns
      skill_mat = matrix(0, ncol = length(unique(skills[,selected_skill][!is.na(skills[,selected_skill])])))
      
      skill_df = data.frame(skill_mat)
      
      colnames(skill_df) = unique(skills[,selected_skill][!is.na(skills[,selected_skill])])
      for (n in 1:length(skills[,selected_skill][!is.na(skills[,selected_skill])])){
        for (m in 1:length(text))
        {
          
          skill_df[m,n] = str_detect(text_df$text[m], skills[,selected_skill][n])
        }
      }
      
      final_df = cbind(text_df, skill_df)
      
      final_df_length = length(final_df)
      
      final_table = matrix(0, ncol = length(unique(skills[,selected_skill][!is.na(skills[,selected_skill])]))+1)
      
      final_table = data.frame(final_table)
      
      colnames(final_table) = colnames(final_df)
      
      candidate_name2 = str_replace_all(candidate_name, "_|-", " ")
      candidate_name3 = str_replace_all(candidate_name2, "Resume|resume", " ")
      
      final_table$text = candidate_name3
      
      names(final_table)[1] = "candidate_name"
      
      for (o in 2:length(colnames(final_table)))
      {
        final_table[o] = if (sum(final_df[o]) >0) {1} else {0}
      }
      totals = length(final_table)-1
      final_table$matches = rowSums(final_table[,-1])
      final_table$totals = totals
      final_table$percentage_matches = round((final_table$matches/totals)*100, 0)
      
      email = as.character(na.omit(str_extract(text_df$text, "\\S*@\\S*")))
      email2 = str_replace_all(email, "[^[:alnum:]@.]", " ")
      
      email3 = sub(".*? (.+)", "\\1", email2)
      if(identical(email3, character(0)))
      {
        final_table$email = "Not Available"
      }
      else
      {
        final_table$email = email3
      }
      
      phone = na.omit(str_extract(text_df$text, "\\(?\\d{3}\\)?[.-]? *\\d{3}[.-]? *[.-]?\\d{4}"))
      phone_number = gsub(" ","",phone)
      
      phone2 = as.numeric(phone_number)
      if(identical(phone2, numeric(0)))
      {
        final_table$phone = "Not Available"
      }
      else
      {
        final_table$phone = phone2
      }
      
      
      final_table = final_table %>% select(candidate_name, email, phone, everything())
      
      doc_df = rbind(doc_df, final_table)
      
      
    }}
    
    
    
    
    main_df = rbind(output_df, docx_df, doc_df)
    
    main_df = main_df[!(is.na(main_df$candidate_name)), ]
    main_df = main_df[order(main_df$percentage_matches, decreasing = TRUE),]
    main_df
    
  })
  
  
  
  
  
  output$table1 = DT::renderDataTable(
    
    file_names(), extensions = 'Buttons', options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel'), pageLength=20), rownames=FALSE
  )
  
  
  
}

shinyApp(ui, server)