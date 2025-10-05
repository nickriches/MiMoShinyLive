

library(shiny)
library(DT) # To create a datatable
library(dplyr)
library(stringr)
library(udpipe) # Part-of-speech-tagger
library(textcat) # To detect the language of a text
library(zoo) # For the time series analysis.
library(koRpus) # To obtain lexical diversity measures
library(koRpus.lang.en) # Corpus English Language
library(colourpicker) # to select lovely colours

# if (FALSE) { # Needed to autoload these libraries
# }

colours_text = "Word_class,scheme1,scheme2,scheme3,scheme4
VERB,#FFAB94,#FFAB94,#FFFFFF,#FFFFFF
COPULA,#FFAB94,#FFAB94,#FFFFFF,#FFFFFF
AUXILIARY,#FAD4CB,#FAD4CB,#FFFFFF,#FFFFFF
PARTICLE,#FAD4CB,#FAD4CB,#FFFFFF,#FFFFFF
ADVB,#FAD4CB,#FAD4CB,#FFFFFF,#FFFFFF
NOUN,#B6B6F5,#FFFFFF,#B6B6F5,#FFFFFF
DET,#ADFFFF,#FFFFFF,#ADFFFF,#FFFFFF
ADJ,#ADFFFF,#FFFFFF,#ADFFFF,#FFFFFF
PRON,#99FF69,#FFFFFF,#99FF69,#FFFFFF
PREP,#FFFF52,#FFFFFF,#FFFFFF,#FFFF52
SUB,#FCAD46,#FFFFFF,#FFFFFF,#FCAD46
COORD,#FFCD7D,#FFFFFF,#FFFFFF,#FFCD7D
PUNCT,#eeeedd,#FFFFFF,#FFFFFF,#FFFFFF
INTERJECTION,#C29A72,#FFFFFF,#FFFFFF,#FFFFFF"

colours <- read.table(text = colours_text, sep =",", header = TRUE, comment.char = "", stringsAsFactors = FALSE)

# 
age_calc2 = function(dob, dot){
  
  dob_day = as.numeric(strftime(dob, "%d"))
  dob_month = as.numeric(strftime(dob, "%m"))
  dob_year = as.numeric(strftime(dob, "%Y"))
  
  dot_day = as.numeric(strftime(dot, "%d"))
  dot_month = as.numeric(strftime(dot, "%m"))
  dot_year = as.numeric(strftime(dot, "%Y"))
  
  years_diff = dot_year - dob_year
  months_diff = dot_month - dob_month
  days_diff = dot_day - dob_day
  
  total_months_diff = (years_diff*12) + (months_diff) + (days_diff/31)
  
  return(total_months_diff)
}


fill = function(df, varName){
  varPosn = which(names(df) == varName)
  vector = df[,varPosn]
  fill_variable = ""
  for(i in 1:length(vector)){
    if(!is.na(vector[i])){
      fill_variable = vector[i]
    }
    if(is.na(vector[i])){
      vector[i] = fill_variable
    }
  }
  return(vector)
}

shinyApp(
  
  ui <- fluidPage(#theme = "flatly.css",
    
    
    # Instructions page ----
    navbarPage("🔎 MiMo App",
               id = "navbar",
               # Let's get started navbar ----
               navbarMenu("Let's get started!",
                          #(1) Enter text tab panel ----
                          tabPanel("(1) Enter text",
                                   radioButtons("paste_or_upload",
                                                label = h3("Do you wish to paste or upload your data?"),
                                                choices = list("Paste" = 1, "Upload (.txt file only)" = 2),
                                                width = '100%', selected = 1),
                                   conditionalPanel(condition = "input.paste_or_upload == 1",
                                                    textAreaInput("text_file_TA", "Enter text here...",
                                                                  placeholder = "Enter text here...",
                                                                  width = "100%", height = "400px", resize = "both"
                                                    )
                                   ),
                                   conditionalPanel(condition = "input.paste_or_upload == 2",
                                                    fileInput("text_file", "Select file",
                                                              multiple = FALSE,
                                                              accept = c("text/plain")
                                                    )
                                   ),
                          ),
                          #(2) Check language tab panel ----
                          tabPanel("(2) Check language",
                                   htmlOutput("text_example"),
                                   radioButtons("proceed", label = h3("How do you wish to proceed?"),
                                                choices = list("Continue" = 1, "Select another language" = 2), 
                                                width = '100%', selected = 1),
                                   conditionalPanel(condition = "input.proceed == 2",
                                                    textAreaInput("manual_language", "Enter a language...",
                                                                  placeholder = "Enter language name here...",
                                                                  width = "1000px", resize = "both"),
                                                    textAreaInput("manual_url", "(OPTIONAL) Choose model...",
                                                                  placeholder = "For a list of models, see section on udpipe_download_model in the R udpipe manual at https://cran.r-project.org/web/packages/udpipe/index.html",
                                                                  width = "1000px", resize = "both")
                                   )
                          )
               ),
               # Let's explore nav bar ----
               navbarMenu("Let's explore!",
                          #(1) coloured output tab panel ----
                          tabPanel("(1) Coloured output", value = "lets_explore",
                                   
                                   
                                   tags$head(
                                     
                                     tags$style(HTML({"
                                       .mytooltip {
                                       position: relative;
                                       display: inline-block;
                                       }
                                       
                                       .mytooltip .tooltiptext {
                                       visibility: hidden;
                                       width: 120px;
                                       background-color: #ac3973;
                                       color: #fff;
                                       text-align: center;
                                       border: 2px solid #df9fbf;
                                       border-radius: 6px
                                       padding: 5px 0;

                                       
                                       /* Position the tooltip */
                                       position: absolute;
                                       z-index: 1;
                                       bottom: 100%;
                                       left: 50%;
                                       margin-left: -60px;
                                       }
                                       
                                       .mytooltip:hover .tooltiptext {
                                       visibility: visible;
                                       }

table.dataTable tbody tr.selected td,
table.dataTable tbody td.selected {
    border-top-color: white !important;
    box-shadow: inset 0 0 0 9999px #FFEBED !important;
    color: #000000
}


table.dataTable tbody tr.selected td,
table.dataTable tbody td.selected {
    border-top-color: white !important;
    box-shadow: inset 0 0 0 9999px #FFEBED !important;
    color: #000000
}

table.dataTable tbody tr:active td {
    background-color: #FFEBED !important;
}

:root {
    --dt-row-selected: transparent !important;
}

table.dataTable tbody tr:hover, table.dataTable tbody tr:hover td {
    background-color: #FFEBED !important;
}


      /* Turn OFF selection everywhere in DT body... */
      #tbl table.dataTable tbody td {
        -webkit-user-select: none !important;
        -moz-user-select: none !important;
        -ms-user-select: none !important;
        user-select: none !important;
      }
      /* ...then turn it ON only for allowed columns */
      #tbl table.dataTable tbody td.selectable-col {
        -webkit-user-select: text !important;
        -moz-user-select: text !important;
        -ms-user-select: text !important;
        user-select: text !important;
        cursor: text;
      }

                                       "}))
                                   ),
                                   tags$div(
                                     style = "display: flex; gap: 10px;",  # flexbox with spacing
                                     numericInput(label = "Number utts per page", value = 200, inputId = "nrows"),
                                     selectizeInput(label = "Speaker filter",
                                                    choices = NULL,
                                                    multiple = TRUE,
                                                    inputId = "select_speaker")
                                   ),
                                   tags$div(
                                     style = "display:flex; align-items:center; gap:6px;",
                                     actionButton("sort_btn_order", "Sort by order"),
                                     actionButton("sort_btn_length", "Sort by length"),
                                     actionButton("sort_btn_alph", "Sort alphabetically"),
                                     checkboxInput("column_filter", "Show column filters", FALSE, width = "auto"),
                                     checkboxInput("enable_copy", "Enable copy", FALSE, width = "auto")
                                   ),
                                   DT::dataTableOutput("table_coloured")
                          ),
                          
                          
                          
                          #(2) Syntactic measures tab panel-----
                          tabPanel("(2) Syntactic measures",
                                   h3("Syntactic measures"),
                                   DT::dataTableOutput("table_summaries"),
                                   br()
                          ),  # end of tab panel ----
                          
                          
                          
                          
                          
                          #(3) Lexical measures tab panel-----
                          tabPanel("(3) Lexical measures",
                                   h3("Lexical measures"),
                                   DT::dataTableOutput("table_summaries2")
                          ), # end of tab panel
                          
                          
                          #(4) Tags -----
                          tabPanel("(4) Tags",
                                   
                                   mainPanel(
                                     h4("Number of tags per utterance"),
                                     DT::dataTableOutput("tag_table_v1"),
                                     br(),
                                     h4("Percentage of utterances containing a specific tag"),
                                     DT::dataTableOutput("tag_table_v2")
                                   )
                                   
                          ) # end of tags tabpanel
                          
               ), # end of nav bar menu
               
               
               # Colour tab panel----
               tabPanel("Colours", 
                        
                        selectInput(inputId = "colour_scheme",
                                    label = h3("Select colour scheme"), 
                                    choices = list("All colours" = 2,
                                                   "Verb-related words only" = 3,
                                                   "Noun-related words only" = 4,
                                                   "Linking words (conjunctions and Prepositions)" = 5), 
                                    selected = 2),
                        
                        h3("Widgets contain hexadecimal colour codes.
                                   Colours may be conveniently copied and pasted by copying and pasting these codes."),
                        
                        br(),
                        h3("Word classes in the Verb Complex (sometimes called Verb Phrase)"),
                        htmlOutput("colour_picker_verb"),
                        htmlOutput("colour_picker_copula"),
                        htmlOutput("colour_picker_auxiliary"),
                        htmlOutput("colour_picker_particle"),
                        htmlOutput("colour_picker_advb"),
                        br(),
                        h3("Word classes in the Noun Phrase"),
                        htmlOutput("colour_picker_noun"),
                        htmlOutput("colour_picker_det"),
                        htmlOutput("colour_picker_adj"),
                        htmlOutput("colour_picker_pron"),
                        br(),
                        h3("Linking words"),
                        htmlOutput("colour_picker_prep"),
                        htmlOutput("colour_picker_sub"),
                        htmlOutput("colour_picker_coord"),
                        br(),
                        h3("Other"),
                        htmlOutput("colour_picker_punct"),
                        htmlOutput("colour_picker_interjection")
                        
                        
               ), # End of tabPanel "Colors"
               
               
               # A dummy tab that looks like a tab, but opens an external link
               tabPanel("📈 Norms", value = "norms-link"),
               tabPanel("📄 Docs", value = "docs-link"),
               
               # Make that tab open your URL instead of switching panels
               tags$head(
                 tags$style(HTML("a[data-value='norms-link'] { cursor: pointer; }")),
                 tags$script(HTML("
                     $(document).on('click', 'a[data-value=\"norms-link\"]', function(e) {
                     e.preventDefault();
                     window.open('http://norms.mimolanguageanalysis.uk', '_blank');
                     });"
                 ))
               ),
               
               tags$head(
                 tags$style(HTML("a[data-value='docs-link'] { cursor: pointer; }")),
                 tags$script(HTML("
                     $(document).on('click', 'a[data-value=\"docs-link\"]', function(e) {
                     e.preventDefault();
                     window.open('http://docs.mimolanguageanalysis.uk', '_blank');
                     });"
                 ))
               )
               
    ) # end of nav bar page
  ),
  
  # server statement----
  server <- function(input, output, session){
    
    # ***REACTIVE STATEMENTS*** ----
    # text (read in text file) ----
    
    sort_dir_order <- reactiveVal("asc")
    sort_dir_alph <- reactiveVal("asc")
    sort_dir_length <- reactiveVal("desc")
    
    text <- reactive({
      
      if(input$paste_or_upload == "1" & input$text_file_TA=="") return(NULL)
      
      if(input$paste_or_upload == "2"){
        text <- read.delim(input$text_file$datapath)$text
      }
      
      if(input$text_file_TA!=""){
        text <- input$text_file_TA
      }
      
      return(text)
      
    })
    
    
    
    
    # lang (obtaining language) ----
    
    lang <- reactive({
      
      if(input$paste_or_upload == "1" & input$text_file_TA=="") return(NULL)
      
      if(input$paste_or_upload == "2"){
        text <- read.delim(input$text_file$datapath)$text
      }
      
      if(input$text_file_TA!=""){
        text <- input$text_file_TA
      }
      
      
      lang <- textcat(text)
      
      return(lang)
      
    })
    
    
    
    # table (showing transcript)----
    
    table <- reactive({
      
      withProgress(message = "Loading / refreshing table...", {
        
        
        
        if(input$paste_or_upload == "1" & input$text_file_TA=="") return(NULL)
        
        if(input$paste_or_upload == "2"){
          text <- read.delim(input$text_file$datapath)$text
        }
        
        if(input$text_file_TA!=""){
          text <- input$text_file_TA
        }
        
        
        
        if(input$manual_language==""){
          lang <- textcat(text)} else{
            lang <- input$manual_language
          }
        
        lang <- trimws(tolower(lang))
        
        str_split_keep_delimiter <- function(string, delV){ #string and delimiter vector
          for(i in 1:length(delV)){
            search_string <- paste0("(", delV[i], ")")
            replace_string <- "\\1***"
            string <- str_replace_all(string, search_string, replace_string)
          }
          string <- str_split(string, "[\x2a][\x2a][\x2a]")
          string <- unlist(string)
          string <- string[which(string != "")] # removes any blanks which may have been created
          return(string)
        }
        
        # Convert text object to vector
        
        alphanumeric <- function(x){
          return(grepl("[a-zA-Z0-9]",x))
        }
        
        
        text <- str_split_keep_delimiter(text, c("[\x2e]+[\x22|\x27]*[\x20]*([\\[|\x28][^\\[|\x28]*[\\]|\x29][\x20]*)*",
                                                 "[\x21]+[\x22|\x27]*[\x20]*([\\[|\x28][^\\[|\x28]*[\\]|\x29][\x20]*)*",
                                                 "[\x3f]+[\x22|\x27]*[\x20]*([\\[|\x28][^\\[|\x28]*[\\]|\x29][\x20]*)*")
        )
        
        
        
        text <- as.data.frame(text)
        
        text <- text %>% filter(alphanumeric(text) == TRUE) # Gets rid of blank lines / lines with only punctuation
        text$text <- str_trim(text$text) # Trims lead/trailing spaces
        
        extract_speaker <- function(x){ 
          x <- str_trim(x) # trim leading and trailing spaces
          x <- strsplit(x, " ")[[1]][1] # split by space and identify first word
          x <- stringr::str_match(x, "[a-zA-Z]+[\x3a]$") # identify whether first word could be speaker
          return(x)
        }
        
        remove_speaker <- function(x){ 
          x <- gsub("[a-zA-Z]+[\x3a]", "", x) # replace speaker with ""
          return(x)
        }
        
        text$speaker <- sapply(text$text, extract_speaker)
        
        # browser()
        
        text$speaker = fill(text, "speaker")
        
        text$speaker[which(is.na(text$speaker))] <- "xxx:"
        
        speakers <- text$speaker
        
        text$text <- sapply(text$text, remove_speaker)
        
        #Remove standalone punctation
        
        text$text <- gsub("[\x20][:punct:][\x20]", "", text$text) # needs to be changed
        
        # Calculate Num Words - using spaces to delimit words
        
        count_words_using_spaces <- function(x){
          return(str_count(x, "[^\x20]+"))
        }
        
        remove_non_alphanumeric <- function(x){
          return(gsub("[^\x20a-zA-Z0-9]", "", x))
        }
        
        extract_comments_as_vector <- function(x){
          result <- unlist(str_extract_all(x, "[\x28][^\x28|\x29]*[\x29]"))
          result <- paste0("", result)
          result <- as.vector(result)
          return(result)
        }
        
        extract_tags_as_vector <- function(x){
          result <- unlist(str_extract_all(x, "\\[[^\\[]*\\]"))
          result <- paste0("", result)
          result <- as.vector(result)
          return(result)
        }
        
        extract_comments_as_string <- function(x){
          result <- unlist(str_extract_all(x, "[\x28][^\x28|\x29]*[\x29]"))
          result <- paste0("", result, collapse = "")
          return(result)
        }
        
        extract_tags_as_string <- function(x){
          result <- unlist(str_extract_all(x, "\\[[^\\[]*\\]"))
          result <- paste0("", result, collapse = "")
          return(result)
        }
        
        replace_comments <- function(x){
          result <- str_replace_all(x, "[\x28][^\x28|\x29]*[\x29]", "\x28")
          return(result)
        }
        
        replace_tags <- function(x){
          result <- str_replace_all(x, "\\[[^\\[]*\\]", "\\[")
          return(result)
        }
        
        insert_comments <- function(x, y){ ## Doesn't work
          y <- str_replace_all(y, "[\x28]", "OPENBRACKETHERE")
          y <- str_replace_all(y, "[\x29]", "CLOSEDBRACKETHERE")
          for(i in 1:length(y)){
            x <- str_replace(x, "[\x28]", y[i])
          }
          x <- str_replace_all(x, "OPENBRACKETHERE","\x28")
          x <- str_replace_all(x, "CLOSEDBRACKETHERE", "\x29")
          return(x)
        }
        
        insert_tags <- function(x, y){ ## x = string, y = vector
          y <- str_replace_all(y, "\\[", "OPENBRACKETHERE")
          y <- str_replace_all(y, "\\]", "CLOSEDBRACKETHERE")
          for(i in 1:length(y)){
            x <- str_replace(x, "\\[", y[i])
          }
          x <- str_replace_all(x, "OPENBRACKETHERE","\\[")
          x <- str_replace_all(x, "CLOSEDBRACKETHERE", "\\]")
          return(x)
        }
        
        
        remove_comments <- function(str){
          str <- str_replace_all(str, "[\x28][^\x28]*[\x29]", "")
          return(str)
        }
        
        remove_tags <- function(str){
          str <- str_replace_all(str, "\\[[^\\[]*\\]", "")
          return(str)
        }
        
        
        identify_words_containing_punct <- function(vec){
          
          result = rep("", length(vec))
          
          for(i in 1:length(vec)){
            result[i] = paste(unlist(str_extract_all(vec[i], "[^\x20]+[:punct:][^\x20]+")), collapse = " ")
          }
          
          return(result)
        }
        
        
        Num_Words <- count_words_using_spaces(remove_non_alphanumeric(remove_comments(remove_tags(text$text))))
        
        morph_complex_words = identify_words_containing_punct(remove_comments(remove_tags(text$text)))
        
        text$comments <- sapply(text$text, extract_comments_as_string)
        
        text$tags <- sapply(text$text, extract_tags_as_string)
        
        comments <- text$comments
        tags <- text$tags
        
        text$text <- replace_comments(text$text)
        text$text <- replace_tags(text$text)
        
        text_comments_extracted <- gsub("\\[", "", text$text)
        text_comments_extracted <- gsub("\\(", "", text_comments_extracted)
        
        has_period <- function(x){
          return(grepl("[\x2e]", x))
        }
        
        has_question_mark <- function(x){
          return(grepl("[\x3f]", x))
        }
        
        has_single_exclamation_mark <- function(x){
          return(str_count(x, "[\x21]") == 1)
        }
        
        has_multiple_exclamation_marks <- function(x){
          return(str_count(x, "[\x21]") >= 2)
        }
        
        text$mood <- ""
        
        text$mood[has_period(text$text)] <- "isdeclarative"
        text$mood[has_question_mark(text$text)] <- "isinterrogative isquestion"
        text$mood[has_single_exclamation_mark(text$text)] <- "isimperative"
        text$mood[has_multiple_exclamation_marks(text$text)] <- "isexclamative"
        
        mood <- text$mood
        
        # Download language model and parse text
        
        if(input$manual_url==""){
          model <- udpipe_download_model(lang, model_dir = tempdir()) # NB can add "model_dir = tempdir()"
        }
        
        if(input$manual_url!=""){
          model <- udpipe_download_model(input$manual_url, model_dir = tempdir())
        }
        
        
        model <- udpipe_load_model(model$file_model)
        
        # browser(); one <- 1; one <- 1; one <- 1; one <- 1; one< -1
        
        text <- udpipe_annotate(model, text$text)
        
        text <- as.data.frame(text)
        
        
        text$morpheme <- 1
        text$morpheme[which(text$upos == "PUNCT")] <- 0 # So we don't count punctuation as a morpheme
        
        
        text$upos[which(text$dep_rel == "cop")] <- "COPULA"
        
        # Identify number of clauses
        
        text$num_clause <- as.numeric(text$upos == "VERB" | text$upos == "COPULA")
        
        text$num_fin_clause <- as.numeric(grepl("VerbForm=Fin", text$feats))
        
        verb_form <- function(x){
          return(
            case_when(
              grepl("Tense=Past[\x7c]VerbForm=Fin",x) == TRUE ~ "hasPastTense",
              grepl("Tense=Pres[\x7c]VerbForm=Fin", x) == TRUE ~ "hasPresTense hasPresentTense",
              grepl("Tense=Past[\x7c]VerbForm=Part", x) == TRUE ~ "hasPastParticiple",
              grepl("Tense=Pres[\x7c]VerbForm=Part", x) == TRUE ~ "hasPresentParticiple hasPresParticiple",
              grepl("VerbForm=Inf", x) == TRUE ~ "hasInfinitive",
              TRUE ~ ""
            )
          )
        }
        
        
        text$verb_form <- sapply(text$feats, verb_form)
        
        rel_clause <- function(x){
          return(
            case_when(
              grepl("relcl", x) == TRUE ~ "hasRelativeClause",
              TRUE ~ ""
            )
          )
        }
        
        text$rel_clause <- sapply(text$dep_rel, rel_clause)
        
        
        highlight <- function(text, colour){ # highlights text in a particular colour
          result <- paste0("<span style=\"background-color:", colour, ";\">",
                           "&thinsp;", text, "&thinsp;",
                           "</span>")
          return(result)
        }
        
        text$features_coloured <- paste0(highlight(text$dep_rel, "#cc99ff"), # Violet
                                         highlight(text$token, "#ff6666"), # Orange
                                         highlight(text$upos, "#ffc299"), # Green
                                         highlight(text$xpos, "#9999ff"), # Dark blue
                                         highlight(text$feats, "#c68c53"), # Brown
                                         " "
        )
        
        text$features <- paste0(text$dep_rel, "-",
                                text$token, "-",
                                text$upos, "-",
                                text$xpos, "-",
                                text$feats, " "
        )
        
        
        add_tool_tip <- function(text, label){
          result <- paste0("<div class=\"mytooltip\">",
                           text,
                           "<span class=\"tooltiptext\">",
                           label,
                           "</span>",
                           "</div>")
          return(result)
        }
        
        add_tool_tip_reduced_text <- function(text, label){
          result <- paste0("<div class=\"mytooltip\">",
                           paste0(substr(text, 1, 15), "..."),
                           "<span class=\"tooltiptext\">",
                           label,
                           "</span>",
                           "</div>")
          return(result)
        }
        
        
        if(is.null(input$VERB_colour)) {VERB_colour <- "#FFAB94"} else {VERB_colour <- input$VERB_colour}
        if(is.null(input$COPULA_colour)) {COPULA_colour <- "#FFAB94"} else {COPULA_colour <- input$COPULA_colour}
        if(is.null(input$AUXILIARY_colour)) {AUXILIARY_colour <- "#FAD4CB"} else {AUXILIARY_colour <- input$AUXILIARY_colour}
        if(is.null(input$PARTICLE_colour)) {PARTICLE_colour <- "#FAD4CB"} else {PARTICLE_colour <- input$PARTICLE_colour}
        if(is.null(input$ADVB_colour)) {ADVB_colour <- "#FAD4CB"} else {ADVB_colour <- input$ADVB_colour}
        if(is.null(input$NOUN_colour)) {NOUN_colour <- "#B6B6F5"} else {NOUN_colour <- input$NOUN_colour}
        if(is.null(input$DET_colour)) {DET_colour <- "#ADFFFF"} else {DET_colour <- input$DET_colour}
        if(is.null(input$ADJ_colour)) {ADJ_colour <- "#ADFFFF"} else {ADJ_colour <- input$ADJ_colour}
        if(is.null(input$PRON_colour)) {PRON_colour <- "#99FF69"} else {PRON_colour <- input$PRON_colour}
        if(is.null(input$PREP_colour)) {PREP_colour <- "#FFFF52"} else {PREP_colour <- input$PREP_colour}
        if(is.null(input$SUB_colour)) {SUB_colour <- "#FCAD46"} else {SUB_colour <- input$SUB_colour}
        if(is.null(input$COORD_colour)) {COORD_colour <- "#FFCD7D"} else {COORD_colour <- input$COORD_colour}
        if(is.null(input$PUNCT_colour)) {PUNCT_colour <- "#eeeedd"} else {PUNCT_colour <- input$PUNCT_colour}
        if(is.null(input$INTERJECTION_colour)) {INTERJECTION_colour <- "#C29A72"} else {INTERJECTION_colour <- input$INTERJECTION_colour}
        
        
        highlight_wc <- function(string, wc){
          if(is.na(wc)){return(string)}
          # red (original colours - user may change)
          else if(wc == "VERB"){result <- add_tool_tip(highlight(paste0("<b>",string,"</b>"), VERB_colour), "VERB")}
          else if(wc == "COPULA"){result <- add_tool_tip(highlight(paste0("<b>", string, "</b>"), COPULA_colour), "COPULA")}
          # orange
          else if(wc == "SCONJ"){result <- add_tool_tip(highlight(string, SUB_colour), "SCONJ.")}
          # light orange
          else if(wc == "CCONJ"){result <- add_tool_tip(highlight(string, COORD_colour), "CCONJ.")}
          # green
          else if(wc == "PRON"){result <- add_tool_tip(highlight(string, PRON_colour), "PRON.")}
          # pink
          else if(wc == "AUX"){result <- add_tool_tip(highlight(string, AUXILIARY_colour), "AUX.")}
          else if(wc == "ADV"){result <- add_tool_tip(highlight(string, ADVB_colour), "ADV.")}
          else if(wc == "PART"){result <- add_tool_tip(highlight(string, PARTICLE_colour), "PARTICLE")}
          # dark blue
          else if(wc == "NOUN"){result <- add_tool_tip(highlight(string, NOUN_colour), "NOUN")}
          else if(wc == "PROPN"){result <- add_tool_tip(highlight(string, NOUN_colour), "PROPN")}
          # cyan
          else if(wc == "DET"){result <- add_tool_tip(highlight(string, DET_colour), "DET.")}
          else if(wc == "DET.poss"){result <- add_tool_tip(highlight(string, DET_colour), "DET.poss")}
          else if(wc == "ADJ"){result <- add_tool_tip(highlight(string, ADJ_colour), "ADJ.")}
          else if(wc == "NUM"){result <- add_tool_tip(highlight(string, DET_colour), "NUM.")}
          # brown
          else if(wc == "INTJ"){result <- add_tool_tip(highlight(string, INTERJECTION_colour), "INTJ")}
          # yellow
          else if(wc == "ADP"){result <- add_tool_tip(highlight(string, PREP_colour), "PREP.")}
          # grey
          else if(wc == "PUNCT"){result <- add_tool_tip(highlight(string, PUNCT_colour), "PUNCT.")}
          else if(wc == "X"){result <- add_tool_tip(highlight(string, "#b8b894"), "X")}
          else if(wc == "SYM"){result <- add_tool_tip(highlight(string, "#b8b894"), "SYM")}
          else{result <- string}
          return(result)
        }
        
        
        
        text %>% filter(!is.na(upos)) -> text
        
        text$coloured <- mapply(highlight_wc, text$token, text$upos) # Applies html formatting to tokens
        
        # Create placemarkers used for inserting comments and tags back into text
        text$coloured[which(text$token == "(")] <- "("
        text$coloured[which(text$token == "[")] <- "["
        
        # create variable showing line of text
        text$line <- NULL
        get_doc_number <- function(x){
          return(as.numeric(substr(x, 4, nchar(x))))
        }
        text$line <- sapply(text$doc, get_doc_number)
        
        # create variable which will allow user to search for word by class
        
        text$upos[which(text$upos == "ADP")] <- "PREP"
        text$hasclass <- paste0("has", tolower(text$upos))
        
        text$neg <- ""
        text$neg[which(grepl("not", text$token))] <- "hasneg"
        text$neg[which(grepl("n't", text$token))] <- "hasneg"
        
        # Reshape data
        text %>%
          group_by(line) %>%
          summarise(sentence_coloured = paste(coloured, collapse = " "),
                    sentence = paste(token, collapse = " "),
                    features_coloured = paste(features_coloured, collapse = " "),
                    features = paste(features, collapse = " "),
                    has_class = paste(hasclass, collapse = " "),
                    pos_tags = paste(upos, collapse = " "),
                    neg = paste(neg, collapse = " "),
                    `Num Morphs` = sum(morpheme),
                    num_clause = sum(num_clause),
                    num_fin_clause = sum(num_fin_clause),
                    verb_form = paste(verb_form, collapse = " "),
                    rel_clause = paste(rel_clause, collapse = " ")
          ) -> text
        
        
        text$`Morph Complex Words` = morph_complex_words
        
        if(lang == "english"){ # English labelling rules ----
          
          extract_word_from_features = function(x){
            if(length(x) == 0){
              return("")
            }
            else(return(unlist(strsplit(x, "-"))[2]))
          }
          
          regular_plural_nouns_all_features = str_extract_all(text$features, "[^\x20]*s-NOUN-NNS-Number=Plur[^\x20]*")
          regular_past_tense_verbs_all_features = str_extract_all(text$features, "[^\x20]*ed-VERB-VBD-Mood=Ind[\x7c]Tense=Past[\x7c]VerbForm=Fin[^\x20]*")
          regular_perfective_verbs_all_features = str_extract_all(text$features, "[^\x20]*ed-VERB-VBN-Tense=Past[\x7c]VerbForm=Part[^\x20]*")
          progressive_verbs_all_features = str_extract_all(text$features, "[^\x20]*ing-VERB-VBG-Tense=Pres[\x7c]VerbForm=Part[^\x20]*")
          
          
          text$`Num Morphs` = text$`Num Morphs` +
            lengths(regular_plural_nouns_all_features) +
            lengths(regular_past_tense_verbs_all_features) +
            lengths(regular_perfective_verbs_all_features) +
            lengths(progressive_verbs_all_features)
          
          morph_complex_words_to_add = rep("", length(text$`Num Morphs`))
          
          for(i in 1:length(text$`Num Morphs`)){
            morph_complex_words_to_add[i] = c(
              paste(
                paste(sapply(unlist(regular_plural_nouns_all_features[i]), extract_word_from_features), collapse = " "),
                paste(sapply(unlist(regular_past_tense_verbs_all_features[i]), extract_word_from_features), collapse = " "),
                paste(sapply(unlist(regular_perfective_verbs_all_features[i]), extract_word_from_features), collapse = " "),
                paste(sapply(unlist(progressive_verbs_all_features[i]), extract_word_from_features), collapse = " "),
                collapse = " ")
            )
          }
          
          text$`Morph Complex Words` = paste(text$`Morph Complex Words`, morph_complex_words_to_add)
          
        }
        
        
        text$NPexpansion <- ""
        
        text$NPexpansion[grepl("((DET|DET\x2eposs|ADJ|NUM)\x20)+(NOUN|PROPN)", text$pos_tags)] <- "hasNPexpansion"
        
        text$VCexpansion <- ""
        
        text$VCexpansion[grepl("((AUX|ADV|VERB|PART)\x20)+(VERB)", text$pos_tags)] <- "hasVCexpansion hasVPexpansion"
        
        text$clause2 <- ""
        text$multipleclauses <- ""
        text$clause3 <- ""
        text$clause4 <- ""
        text$clause5 <- ""
        
        text$clause2[which(text$num_fin_clause == 2)] <- "has2clauses"
        text$multipleclauses[which(text$num_fin_clause >= 2)] <- "hasmultipleclauses, iscomplex"
        text$clause3[which(text$num_fin_clause == 3)] <- "has3clauses"
        text$clause4[which(text$num_fin_clause == 4)] <- "has4clauses"
        text$clause5[which(text$num_fin_clause == 5)] <- "has5clauses"
        
        # This section has functions to colour comments and tags. The comments and tags are then
        # inserted back into the sentence_coloured variable, and coloured accordingly
        
        colour_comments <- function(x){
          result <- str_replace_all(x, "[\x28]", "<span style=\"color:#333399;\">(")
          result <- str_replace_all(result, "[\x29]", ")</span>")
          return(result)
        }
        
        colour_tags <- function(x){
          result <- str_replace_all(x, "\\[", "<span style=\"color:#992600;\">\\[")
          result <- str_replace_all(result, "\\]", "\\]</span>")
          return(result)
        }
        
        for(i in 1:nrow(text)){
          
          text$sentence_coloured[i] <- colour_comments(insert_comments(text$sentence_coloured[i],
                                                                       extract_comments_as_vector(comments[i])))
          
          text$sentence_coloured[i] <- colour_tags(insert_tags(text$sentence_coloured[i],
                                                               extract_tags_as_vector(tags[i])))
        }
        
        text$speaker <- speakers
        text$speaker_no_html <- speakers
        
        unique_speakers <- unique(speakers)
        unique_speakers <- unique_speakers[which(unique_speakers != "")]
        
        speaker_colours <- c("#f2ffe6", "#ffddcc", "#e6f7ff", "#ffe6ff",
                             "#ffffcc", "#ffe6ff", "#ccddff", "#ffcce0",
                             "#ccccff", "#ff0000", "#81a375", "	#ccffcc")
        
        speaker_colours <- c(speaker_colours, rep("#ffffff", 100)) # just in case there are lots of speakers!
        
        for(i in 1:nrow(text)){
          text$speaker[i] <- highlight(text$speaker[i],
                                       speaker_colours[which(unique_speakers == text$speaker[i])])
          
        }
        
        text$mood <- mood
        
        text$passive <- ""
        text$passive[which(grepl("Voice=Pass", text$features))] <- "haspassive"
        
        text$relativepronoun <- ""
        text$relativepronoun[which(grepl("PronType=Rel", text$features))] <- "hasrelativepronoun"
        
        text$modal <- ""
        text$modal[which(grepl("MD", text$features))] <- "hasmodalverb"
        
        text$speaker[which(text$`Num Morphs`==0)] <- ""
        text$speaker_no_html[which(text$`Num Morphs`==0)] <- ""
        
        text$`Num Words` <- Num_Words
        
        text$text_comments_extracted <- text_comments_extracted
        
        # Create variable to allow user to identify lines with comments or tags
        hascomment <- rep("", length(comments))
        hascomment[which(comments!="")] <- "hascomment"
        text$hascomment <- hascomment
        
        hastag <- rep("", length(tags))
        hastag[which(tags!="")] <- "hastag"
        text$hastag <- hastag
        
        # Create a column to allow user to search for specific tags
        tags_plus_content <- str_replace_all(tags, "\x20", "") #remove gaps
        tags_plus_content <- str_replace_all(tags_plus_content, "\\[", "hastag") #start with "hastag"
        tags_plus_content <- str_trim(str_replace_all(tags_plus_content, "\\]", "\x20")) #remove final brackets
        text$tags_plus_content <- tags_plus_content # create variable
        text$tags <- tags
        
        # Code produces position_within_turn and turn_length variables.
        
        text$new_turn <- 0
        speaker <- ""
        
        for(i in 1:nrow(text)){
          if(text$speaker[i] != "" & text$speaker[i] != speaker){text$new_turn[i] <- 1}
          if(text$speaker[i] != "" & text$speaker[i] != speaker){speaker <- text$speaker[i]}
        }
        
        text$position_within_turn <- NA
        position_within_turn <- 1
        
        for(i in 1:nrow(text)){
          if(text$new_turn[i] == 1){position_within_turn <- 0}
          if(text$speaker[i] != ""){position_within_turn <- position_within_turn + 1}
          text$position_within_turn[i] <- position_within_turn
        }
        
        text$position_within_turn[which(text$speaker == "")] <- 0
        
        text$turn_length <- NA
        top <- 0
        
        for(i in nrow(text):1){
          
          top <- max(top, text$position_within_turn[i])
          text$turn_length[i] <- top
          if(text$position_within_turn[i] == 1){top <- 0}
        }
        
        text$turn_length_string <- paste0("turn", text$turn_length)
        text$turn_length_string[which(text$turn_length == 0)] <- ""
        text$turn_length_string[which(text$turn_length == 5)] <- "turn5, turn5plus"
        text$turn_length_string[which(text$turn_length >= 6)] <- "turn5plus"
        
        text$turn_length_first_turn <- NA
        
        text$turn_length_first_turn[which(text$new_turn == 1)] <- text$turn_length[which(text$new_turn == 1)]
        
        text %>% select(line, speaker, sentence_coloured,
                        `Num Words`, `Num Morphs`,
                        num_clause, num_fin_clause, turn_length,
                        `Morph Complex Words`, # here comes the line
                        hascomment, hastag, tags_plus_content, tags,
                        mood, neg, verb_form,
                        NPexpansion, VCexpansion,
                        has_class,
                        clause2, clause3, clause4, clause4, multipleclauses,
                        passive, relativepronoun, rel_clause, modal, text_comments_extracted,
                        new_turn, position_within_turn, turn_length, turn_length_string, turn_length_first_turn,
                        features_coloured, features, speaker_no_html) -> text
        
        
        return(text)
      })
      
    })
    
    
    
    # table_lex (HDD etc) ----
    table_lex <- reactive({
      
      df <- table()
      df %>% group_by(speaker, speaker_no_html) %>% filter(speaker != "") %>%
        summarise(lex = paste(text_comments_extracted, collapse = " ")) -> df
      
      df$hdd <- as.numeric(NA)
      df$ttr <- as.numeric(NA)
      
      for(i in 1:nrow(df)){
        corpus <- koRpus::tokenize(df$lex[i], lang = "en", format = "obj")
        hdd <- HDD(corpus)
        ttr <- TTR(corpus)
        df$types[i] <- length(types(corpus))
        df$tokens[i] <- length(tokens(corpus))
        df$hdd[i] <- as.numeric(koRpus::summary(hdd)[2])
        df$ttr[i] <- as.numeric(koRpus::summary(ttr)[2])
      }
      
      return(df)
    })
    
    
    output$text_example <- renderUI({
      text <- substr(text(), 1, 1000)
      HTML(paste0("<p><h1>Text</h1><h3>(up to 1000th character)</h3>", text,"</p> <p><h2>Language detected: ",
                  "<strong><span style=\"background-color:#00ffff;\">",
                  lang(),
                  "</style></h2></p>"))
    })
    
    search_options = c("hasNoun",
                       "hasPron",
                       "hasDet",
                       "hasVerb",
                       "hasAux",
                       "hasModalVerb",
                       "hasAdjective",
                       "hasAdverb",
                       "hasPrep",
                       "hasCConj",
                       "hasSConj",
                       "hasNeg",
                       "hasNum",
                       "hasPastTense",
                       "hasPresTense",
                       "hasCopula",
                       "hasPresParticiple",
                       "hasPerfParticiple",
                       "has2clauses", "has3clauses", "has4clauses", "has5clauses", "hasMultipleClauses",
                       "hasPassive", "hasRelativePronoun",
                       "isDeclarative", "isInterrogative", "isQuestion", "isImperative", "isExclamative",
                       "hasComment", "hasTag", "hasTagContentsOfTagMinusSpaces",
                       "turn1", "turn2", "turn3", "turn4", "turn5", "turn5plus")
    
    # table_coloured ----
    output$table_coloured = DT::renderDataTable({
      
      table = table()
      
      selection_string = "multiple"
      
      
      if(length(input$select_speaker) >= 1) {
        table %>% filter(speaker_no_html %in% unique(input$select_speaker)) -> table
      }
      
      
      if(input$column_filter == TRUE){
        column_filter_string = "top"
      } else {
        column_filter_string = "none"
      }
      
      
      if(input$enable_copy == TRUE){
        table %>%
          mutate(sentence_coloured = stringr::str_replace_all(string = sentence_coloured,
                                                              pattern = "<div class=\"mytooltip\">",
                                                              replacement = "")) %>%
          mutate(sentence_coloured = stringr::str_replace_all(string = sentence_coloured,
                                                              pattern = "</div>",
                                                              replacement = "")) %>%
          mutate(sentence_coloured = stringr::str_replace_all(string = sentence_coloured,
                                                              pattern = "<span class=\"tooltiptext\">[^\x20]+</span>",
                                                              replacement = "")) ->
          table
        selection_string = "none"
      }
      
      
      datatable(table,
                plugins = "ellipsis",
                # extensions = c("Select"),
                selection = selection_string,
                filter = column_filter_string,
                rownames = FALSE,
                escape = FALSE,
                options = list(
                  dom = 'Bfti<"bottom"p>',
                  select = list(style = "os", items = "cell"),
                  initComplete = DT::JS(paste0("
          function(settings, json) {
            // Hide the default search input
            $('.dataTables_filter input').hide();
            
            // Create selectize dropdown
            var searchOptions = ", jsonlite::toJSON(search_options), ";
            
            // Create select element
            var selectElement = $('<select id=\"custom-search\" multiple></select>');
            
            // Add options to select
            searchOptions.forEach(function(option) {
              selectElement.append('<option value=\"' + option + '\">' + option + '</option>');
            });
            
            // Replace the search input with our select
            $('.dataTables_filter').append(selectElement);
            $('.dataTables_filter label').contents().first()[0].textContent = 'Search: ';
            
            // Initialize selectize
            $('#custom-search').selectize({
              plugins: ['remove_button'],
              delimiter: ',',
              persist: false,
              create: true,
              placeholder: 'Select search terms...',
              onChange: function(values) {
                // Perform the search
                var searchTerm = values.join('&');
                settings.oInstance.api().search(searchTerm, true, false).draw();
              } 
            });
          }")),
                  pageLength = input$nrows,
                  autoWidth = FALSE, searching = TRUE,
                  search = list(regex = TRUE, scrollX = TRUE),
                  columnDefs = list(
                    list( # This is supposed to truncate content, but does not work
                      targets = which(names(table())=="Morph Complex Words")-1,
                      render = JS("$.fn.dataTable.render.ellipsis( 20, false )")
                    ),
                    list(targets = c(1, 2), className = "selectable-col")
                  )
                ),
                callback = JS(
                  # store the table instance globally
                  'table.on("init", function(){window.myDT = table;});'
                )
      ) %>% formatStyle(columns = c(2), width='800px') %>% 
        formatStyle("features_coloured","white-space"="nowrap") %>%
        formatStyle("tags_plus_content","white-space"="nowrap") %>%
        formatStyle("features","white-space"="nowrap") %>%
        formatStyle("sentence_coloured","white-space"="nowrap") %>%
        formatStyle("Morph Complex Words","white-space"="nowrap") %>%
        formatStyle("verb_form", "white-space"="nowrap") %>%
        formatStyle("has_class", "white-space"="nowrap") %>%
        formatStyle("neg", "white-space"="nowrap") %>%
        formatStyle("text_comments_extracted", "white-space"="nowrap") %>%
        formatStyle("rel_clause", "white-space" = "nowrap") %>%
        formatStyle("tags", "white-space"="nowrap") %>%
        formatStyle("mood", "white-space"="nowrap") %>%
        formatStyle("VCexpansion", "white-space"="nowrap") %>%
        formatStyle("multipleclauses", "white-space"="nowrap") %>%
        formatStyle(9, `border-right` = "solid 2px")
    }) # End of renderDatatable----
    
    
    
    
    # table_summaries (MLU etc)----
    output$table_summaries = DT::renderDataTable({
      
      
      datatable(table() %>% filter(speaker!="") %>% group_by(speaker) %>%
                  summarise(`NUtts` = n(),
                            `MLU-w` = round(mean(`Num Words`), digits = 2),
                            `MLU-m` = round(mean(`Num Morphs`), digits = 2),
                            `MNumCl` = round(mean(num_clause), digits = 2),
                            `MLT` = round(mean(turn_length_first_turn, na.rm = TRUE), digits = 2)),
                rownames = FALSE,
                escape = FALSE,
                options = list(paging = FALSE, autoWidth = TRUE, searching = FALSE, dom = 'ft'
                ),
                caption = htmltools::tags$caption(
                  style = 'caption-side: bottom; text-align: left;',
                  'NUtts: Number of Utterances, 
                  MLU-w: Mean Length of Utterance in Words, 
                  MLU-m: Mean Length of Utterance in Morphemes, 
                  MNumCl: Mean Number of (finite) Clauses per utterance, 
                  MLT: Mean Length of Turn')
      )
      
    })
    
    
    
    # table_summaries2 (diversity) ----
    output$table_summaries2 = DT::renderDataTable({
      datatable(table_lex() %>% filter(speaker!="") %>% group_by(speaker) %>%
                  summarise(`HDD` = round(mean(hdd), digits = 2),
                            `TTR` = round(mean(ttr), digits = 2)),
                # filter = c("top"),
                rownames = FALSE,
                escape = FALSE,
                options = list(paging = FALSE, autoWidth = TRUE, searching = FALSE, dom = 'ft'
                ),
                caption = htmltools::tags$caption(
                  style = 'caption-side: bottom; text-align: left;',
                  'HDD: Hypergeometric Distribution density (virtually identical to VocD), 
                  TTR: Type-Token Ratio')
      ) 
    })
    
    # (4) tag_table  ----
    output$tag_table_v1 = DT::renderDataTable({
      
      tryCatch({ 
        
        df = table()
        tags = unique(df$tags)
        speakers_list = df$speaker
        
        
        for(i in 1:length(tags)){
          if(i == 1) {
            initial_vector =  stringr::str_count(df$tags, fixed(tags[i]))
            tags_df = cbind.data.frame(initial_vector)
          }
          if(i > 1) {
            new_vector = stringr::str_count(df$tags, fixed(tags[i]))
            tags_df =cbind.data.frame(tags_df, new_vector)
          }
        }
        
        names(tags_df) = tags
        tags_df$speakers = speakers_list
        
        tags_df %>% relocate(speakers) -> tags_df
        
        datatable(tags_df %>% filter(speakers!="") %>%
                    group_by(speakers) %>%
                    summarize(across(where(is.numeric), ~ round(mean(.x, na.rm = TRUE), digits = 2))),
                  rownames = FALSE,
                  escape = FALSE,
                  options = list(paging = FALSE, autoWidth = TRUE, searching = FALSE, dom = 'ft')
        )
      }, error = function(e){
        validate(need(FALSE, "No tag data to summarise"))
      })
      
    })
    
    
    output$tag_table_v2 = DT::renderDataTable({
      
      tryCatch({
        
        df = table()
        tags = unique(df$tags)
        tags =  unique(unlist(strsplit(tags, "(?<=\\])", perl = TRUE)))
        # tags = unlist(str_replace_all(tags, "[\\[\\]]", ""))
        speakers_list = df$speaker
        
        # browser()
        
        for(i in 1:length(tags)){
          if(i == 1) {
            initial_vector =  stringr::str_count(df$tags, fixed(tags[i]))
            tags_df = cbind.data.frame(initial_vector)
          }
          if(i > 1) {
            new_vector = stringr::str_count(df$tags, fixed(tags[i]))
            tags_df =cbind.data.frame(tags_df, new_vector)
          }
        }
        
        names(tags_df) = tags
        tags_df$speakers = speakers_list
        
        # browser()
        
        tags_df %>% relocate(speakers) -> tags_df
        
        # browser()
        
        tags_df %>% mutate(across(where(is.numeric), ~ifelse(.x >0, 1, 0))) -> df
        
        datatable(tags_df %>% filter(speakers!="") %>%
                    group_by(speakers) %>%
                    summarize(across(where(is.numeric), ~ round(100*mean(.x, na.rm = TRUE),  digits = 2))),
                  rownames = FALSE,
                  escape = FALSE,
                  options = list(paging = FALSE, autoWidth = TRUE, searching = FALSE, dom = 'ft')
        )
      }, error = function(e){
        validate(need(FALSE, "No tag data to summarise"))
      })
      
    })
    
    
    # colours ----
    
    verb_col <- reactive({
      colour <- colours[1, as.numeric(input$colour_scheme)]
      return(colour)
    })
    
    copula_col <- reactive({
      colour <- colours[2, as.numeric(input$colour_scheme)]
      return(colour)
    })
    
    auxiliary_col <- reactive({
      colour <- colours[3, as.numeric(input$colour_scheme)]
      return(colour)
    })
    
    particle_col <- reactive({
      colour <- colours[4, as.numeric(input$colour_scheme)]
      return(colour)
    })
    
    advb_col <- reactive({
      colour <- colours[5, as.numeric(input$colour_scheme)]
      return(colour)
    })
    
    noun_col <- reactive({
      colour <- colours[6, as.numeric(input$colour_scheme)]
      return(colour)
    })
    
    det_col <- reactive({
      colour <- colours[7, as.numeric(input$colour_scheme)]
      return(colour)
    })
    
    adj_col <- reactive({
      colour <- colours[8, as.numeric(input$colour_scheme)]
      return(colour)
    })
    
    pron_col <- reactive({
      colour <- colours[9, as.numeric(input$colour_scheme)]
      return(colour)
    })
    
    prep_col <- reactive({
      colour <- colours[10, as.numeric(input$colour_scheme)]
      return(colour)
    })
    
    sub_col <- reactive({
      colour <- colours[11, as.numeric(input$colour_scheme)]
      return(colour)
    })
    
    coord_col <- reactive({
      colour <- colours[12, as.numeric(input$colour_scheme)]
      return(colour)
    })
    
    punct_col <- reactive({
      colour <- colours[13, as.numeric(input$colour_scheme)]
      return(colour)
    })
    
    interjection_col <- reactive({
      colour <- colours[14, as.numeric(input$colour_scheme)]
      return(colour)
    })
    
    output$colour_picker_verb <- renderUI({
      colourpicker::colourInput(
        inputId = "VERB_colour",
        label = "Main Verb (label = VERB)",
        value = verb_col()
      )
    })
    
    output$colour_picker_copula <- renderUI({
      colourpicker::colourInput(
        inputId = "COPULA_colour",
        label = "Copula (label = COPULA)",
        value = copula_col()
      )
    })
    
    output$colour_picker_auxiliary <- renderUI({
      colourpicker::colourInput(
        inputId = "AUXILIARY_colour",
        label = "Auxiliary verb (label = AUXILIARY)",
        value = auxiliary_col()
      )
    })
    
    output$colour_picker_particle <- renderUI({
      colourpicker::colourInput(
        inputId = "PARTICLE_colour",
        label = "Verb particle (label = PARTICLE)",
        value = particle_col()
      )
    })
    
    output$colour_picker_advb <- renderUI({
      colourpicker::colourInput(
        inputId = "ADVB_colour",
        label = "Adverb (label = ADVB)",
        value = advb_col()
      )
    })
    
    output$colour_picker_noun <- renderUI({
      colourpicker::colourInput(
        inputId = "NOUN_colour",
        label = "Noun (label = NOUN)",
        value = noun_col()
      )
    })
    
    
    output$colour_picker_det <- renderUI({
      colourpicker::colourInput(
        inputId = "DET_colour",
        label = "Determiner (label = DET)",
        value = det_col()
      )
    })
    
    
    output$colour_picker_adj <- renderUI({
      colourpicker::colourInput(
        inputId = "ADJ_colour",
        label = "Adjective (label = ADJ)",
        value = adj_col()
      )
    })
    
    
    output$colour_picker_pron <- renderUI({
      colourpicker::colourInput(
        inputId = "PRON_colour",
        label = "Pronoun (label = PRON)",
        value = pron_col()
      )
    })
    
    output$colour_picker_prep <- renderUI({
      colourpicker::colourInput(
        inputId = "PREP_colour",
        label = "Preposition (label = PREP)",
        value = prep_col()
      )
    })
    
    output$colour_picker_sub <- renderUI({
      colourpicker::colourInput(
        inputId = "SUB_colour",
        label = "Subordinator (label = SUB)",
        value = sub_col()
      )
    })
    
    output$colour_picker_coord <- renderUI({
      colourpicker::colourInput(
        inputId = "COORD_colour",
        label = "Coordinator (label = COORD)",
        value = coord_col()
      )
    })
    
    output$colour_picker_punct <- renderUI({
      colourpicker::colourInput(
        inputId = "PUNCT_colour",
        label = "Punctuation (label = PUNCT)",
        value = punct_col()
      )
    })
    
    output$colour_picker_interjection <- renderUI({
      colourpicker::colourInput(
        inputId = "INTERJECTION_colour",
        label = "Interjection (label = INTERJECTION)",
        value = interjection_col()
      )
    })
    
    observeEvent(input$sort_btn_order, {
      # send the current direction and then toggle it
      session$sendCustomMessage("sortDT", list(col = 1, dir = sort_dir_order()))
      sort_dir_order(ifelse(sort_dir_order() == "asc", "desc", "asc"))
    })
    
    
    observeEvent(input$sort_btn_length, {
      # send the current direction and then toggle it
      session$sendCustomMessage("sortDT", list(col = 4, dir = sort_dir_length()))
      sort_dir_length(ifelse(sort_dir_length() == "desc", "asc", "desc"))
    })
    
    observeEvent(input$sort_btn_alph, {
      # send the current direction and then toggle it
      session$sendCustomMessage("sortDT", list(col = 3, dir = sort_dir_alph()))
      sort_dir_alph(ifelse(sort_dir_alph() == "asc", "desc", "asc"))
    })
    
    
    observeEvent(input$navbar, {
      req(input$navbar == "lets_explore")
      # Only re-evaluate table() when the navbar changes to this tab
      speakers <- isolate(table()$speaker_no_html)
      updateSelectizeInput(
        session,
        "select_speaker",
        choices = sort(c("", unique(speakers)))
      )
    })
    
    
    # JavaScript handler for sorting ----
    shiny::insertUI(
      selector = "head",
      where = "beforeEnd",
      ui = tags$script(HTML("
      Shiny.addCustomMessageHandler('sortDT', function(message) {
        if(window.myDT){
          window.myDT.order([message.col - 1, message.dir]).draw();
        }
      });
    "))
    )
    
    
  } # end of server statement----
  
)

shinyApp(ui = ui, server = server)



