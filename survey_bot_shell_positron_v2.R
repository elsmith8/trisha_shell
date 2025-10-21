setwd('/Users/ericasmith/not_icloud/chatbot_v2/')
logo_base64 <- base64enc::base64encode("www/trisha_logo.jpg")

library(data.table)
library(shiny)
library(httr)
library(jsonlite)
library(DT)

# Configuration
ANTHROPIC_API_KEY <- Sys.getenv("ANTHROPIC_API_KEY")
SURVEY_FILE <- "survey_questions.csv"
LOG_FILE <- "survey_interactions.csv"
TRAUMA_INFORMED_GUIDE <- "tic_kb/trauma_informed_responses.txt"  

# Load trauma-informed response guidelines
load_trauma_guidelines <- function() {
    guidelines <- readLines(TRAUMA_INFORMED_GUIDE, warn = FALSE)
    return(paste(guidelines, collapse = "\n"))
}

# Load survey questions
load_survey_questions <- function() {
  sq <- read.csv(SURVEY_FILE, stringsAsFactors = FALSE)
  return(sq)
}

survey_questions <- load_survey_questions()

# Call the Anthropic API with single attempt and immediate fallback
call_claude <- function(prompt, current_question_idx = NULL) {
  response <- POST(
    "https://api.anthropic.com/v1/messages",
    add_headers(
      "x-api-key" = ANTHROPIC_API_KEY,
      "Content-Type" = "application/json",
      "anthropic-version" = "2023-06-01"
    ),
    body = toJSON(list(
      model = "claude-sonnet-4-5-20250929",
      max_tokens = 1000,
      messages = list(list(role = "user", content = prompt))
    ), auto_unbox = TRUE),
    encode = "raw"
  )
  
  status <- status_code(response)
  
  # Success case
  if (status == 200) {
    result <- fromJSON(content(response, "text"))
    return(result$content$text[1])
  }
  
  # Any error - immediately use fallback
  cat("API call failed with status", status, "- using fallback response\n")
  return(generate_fallback_response(prompt, current_question_idx))
}

# Generate fallback trauma-informed response when API is unavailable
generate_fallback_response <- function(prompt, current_question_idx = NULL) {
  current_q <- current_question_idx
  
  if (!is.null(current_q) && current_q > 0 && current_q <= nrow(survey_questions)) {
    fallback_text <- survey_questions$fallback_response[current_q]
    
    if (!is.na(fallback_text) && nchar(trimws(fallback_text)) > 0) {
      return(fallback_text)
    }
  }
  
  return("Thank you for your response. Let's continue with the next question.")
}

# Enhanced function to generate trauma-informed supportive response
generate_supportive_response <- function(question_text, user_response, conversation_context, questions_data, question_group, current_question_idx = NULL) {
  trauma_guidelines <- load_trauma_guidelines()
  
  context_string <- ""
  if (length(conversation_context) > 0) {
    context_parts <- c()
    for (i in 1:length(conversation_context)) {
      context_parts <- c(context_parts, 
                         paste("Q:", conversation_context[[i]]$question, 
                               "A:", conversation_context[[i]]$response))
    }
    context_string <- paste("Previous conversation:\n", paste(context_parts, collapse = "\n"), "\n\n")
  }
  
  group_responses <- c()
  group_negatives <- c()
  
  if (length(conversation_context) > 0) {
    for (i in 1:length(conversation_context)) {
      ctx <- conversation_context[[i]]
      q_row <- questions_data[questions_data$question_text == ctx$question, ]
      if (nrow(q_row) > 0 && !is.na(q_row$question_group[1]) && q_row$question_group[1] == question_group) {
        group_responses <- c(group_responses, 
                             paste("- Q:", ctx$question, "-> A:", ctx$response))
        
        if (!is.na(q_row$negative_responses[1]) && q_row$negative_responses[1] != "") {
          negative_opts <- trimws(strsplit(q_row$negative_responses[1], "\\|")[[1]])
          if (ctx$response %in% negative_opts) {
            group_negatives <- c(group_negatives, 
                                 paste("- Challenge identified:", ctx$question, "->", ctx$response))
          }
        }
      }
    }
  }
  
  group_responses <- c(group_responses, paste("- Q:", question_text, "-> A:", user_response))
  
  prompt <- paste(
    "You are TRISHA, a trauma-informed healthcare screening chatbot.",
    "",
    "IMPORTANT: You MUST base your response ONLY on the trauma-informed guidelines provided below.",
    "",
    "=== TRAUMA-INFORMED RESPONSE GUIDELINES ===",
    if (trauma_guidelines != "") trauma_guidelines else "No guidelines loaded - use basic empathetic responses only.",
    "=== END GUIDELINES ===",
    "",
    if (length(conversation_context) > 0) {
      paste("Full conversation context:\n", context_string)
    } else "",
    paste("Responses in the", question_group, "section:"),
    paste(group_responses, collapse = "\n"),
    "",
    if (length(group_negatives) > 0) {
      paste("Challenges identified in this", question_group, "section:",
            paste(group_negatives, collapse = "\n"),
            "")
    } else "",
    "Instructions:",
    paste("1. You are providing supportive feedback after completing the", question_group, "section"),
    "2. STRICTLY follow the trauma-informed guidelines provided above",
    "3. Generate a response (2-3 sentences) that applies the appropriate guideline principles",
    "4. Focus specifically on challenges related to", question_group,
    "5. If you know their name from previous responses, use it naturally",
    "6. Do NOT provide advice, solutions, or clinical recommendations unless specifically mentioned in the guidelines",
    "",
    "Generate a trauma-informed response based ONLY on the provided guidelines:",
    sep = "\n"
  )
  
  call_claude(prompt, current_question_idx)
}

# UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f5f5f5;
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif;
      }
      
      .header-container {
        background-color: #f8f9fa;
        padding: 15px;
        border-bottom: 1px solid #dee2e6;
        margin-bottom: 0;
        position: sticky;
        top: 0;
        z-index: 1000;
      }
      
      .header-content {
        display: flex;
        align-items: center;
        max-width: 800px;
        margin: 0 auto;
      }
      
      .chat-container {
        max-width: 800px;
        margin: 0 auto;
        height: calc(100vh - 180px);
        display: flex;
        flex-direction: column;
        background-color: white;
        border-radius: 10px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.1);
      }
      
      .messages-area {
        flex: 1;
        overflow-y: auto;
        padding: 20px;
        background-color: #ffffff;
        border-radius: 10px 10px 0 0;
        min-height: 200px;
      }
      
      .message {
        margin-bottom: 15px;
        display: flex;
        animation: slideIn 0.3s ease-out;
      }
      
      @keyframes slideIn {
        from {
          opacity: 0;
          transform: translateY(10px);
        }
        to {
          opacity: 1;
          transform: translateY(0);
        }
      }
      
      .message.bot {
        justify-content: flex-start;
      }
      
      .message.user {
        justify-content: flex-end;
      }
      
      .message-bubble {
        max-width: 70%;
        padding: 12px 16px;
        border-radius: 18px;
        word-wrap: break-word;
        line-height: 1.4;
        font-size: 16px;
      }
      
      .message.bot .message-bubble {
        background-color: #e9ecef;
        color: #000;
        border-bottom-left-radius: 4px;
      }
      
      .message.user .message-bubble {
        background-color: #007bff;
        color: white;
        border-bottom-right-radius: 4px;
      }
      
      .message.supportive .message-bubble {
        background-color: #e9ecef;
        color: #000;
        border-bottom-left-radius: 4px;
      }
      
      .input-area {
        padding: 15px;
        background-color: #f8f9fa;
        border-top: 1px solid #dee2e6;
        border-radius: 0 0 10px 10px;
        max-height: 60vh;
        overflow-y: auto;
      }
      
      .button-group {
        display: flex;
        flex-direction: column;
        gap: 10px;
        margin-top: 10px;
      }
      
      .choice-btn {
        width: 100%;
        padding: 12px 20px;
        border: 2px solid #007bff;
        background-color: white;
        color: #007bff;
        border-radius: 20px;
        cursor: pointer;
        transition: all 0.2s;
        font-size: 16px;
        white-space: normal;
        word-wrap: break-word;
        overflow-wrap: break-word;
        text-align: left;
        line-height: 1.4;
        min-height: 44px;
        display: block;
      }
      
      .choice-btn:hover {
        background-color: #007bff;
        color: white;
      }
      
      .choice-btn.selected {
        background-color: #007bff;
        color: white;
      }
      
      .text-input-container {
        display: flex;
        gap: 10px;
        align-items: flex-end;
      }
      
      .text-input-container textarea {
        flex: 1;
        border-radius: 20px;
        border: 1px solid #ced4da;
        padding: 10px 15px;
        resize: none;
        font-size: 16px;
      }
      
      .send-btn {
        background-color: #007bff;
        color: white;
        border: none;
        border-radius: 50%;
        width: 40px;
        height: 40px;
        cursor: pointer;
        display: flex;
        align-items: center;
        justify-content: center;
        transition: background-color 0.2s;
      }
      
      .send-btn:hover {
        background-color: #0056b3;
      }
      
      .continue-btn-container {
        display: flex;
        justify-content: center;
        margin-top: 15px;
      }
      
      .continue-btn {
        padding: 10px 30px;
        background-color: #007bff;
        color: white;
        border: none;
        border-radius: 20px;
        cursor: pointer;
        font-size: 16px;
        transition: all 0.2s;
      }
      
      .continue-btn:hover {
        background-color: #0056b3;
      }
      
      .action-buttons {
        display: flex;
        gap: 10px;
        margin-top: 10px;
      }
      
      .action-btn {
        padding: 8px 16px;
        border-radius: 20px;
        border: 1px solid #6c757d;
        background-color: white;
        color: #6c757d;
        cursor: pointer;
        font-size: 13px;
        transition: all 0.2s;
      }
      
      .action-btn:hover {
        background-color: #6c757d;
        color: white;
      }
      
      .welcome-screen {
        text-align: center;
        padding: 40px 20px;
        max-width: 600px;
        margin: 50px auto;
        background-color: white;
        border-radius: 10px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.1);
      }
      
      .welcome-btn {
        padding: 12px 30px;
        margin: 10px;
        border-radius: 25px;
        border: none;
        font-size: 16px;
        cursor: pointer;
        transition: all 0.2s;
      }
      
      .btn-start {
        background-color: #007bff;
        color: white;
      }
      
      .btn-start:hover {
        background-color: #0056b3;
      }
      
      .btn-exit {
        background-color: #dc3545;
        color: white;
      }
      
      .btn-exit:hover {
        background-color: #c82333;
      }
      
      .progress-indicator {
        text-align: center;
        padding: 10px;
        color: #6c757d;
        font-size: 13px;
      }
    "))
  ),
  
  # Header
  div(class = "header-container",
    div(class = "header-content",
      tags$img(src = paste0("data:image/jpeg;base64,", logo_base64),
               height = "60px", 
               style = "margin-right: 15px; border-radius: 8px;"),
      h1("TRISHA - HRSN Screening Chatbot", 
         style = "margin: 0; color: #2c3e50; font-weight: 500; font-size: 24px;")
    )
  ),
  
  # Welcome screen
  conditionalPanel(
    condition = "!output.survey_started && !output.survey_complete",
    div(class = "welcome-screen",
      h3("Welcome!", style = "color: #2c3e50; margin-bottom: 20px;"),
      p("I'm TRISHA, an AI-enabled chatbot. Your provider has some questions about non-medical needs that you may have which could affect your health. My job is to work through those questions with you today."),
      p("If you agree to it, we may share your answers with your other healthcare providers, and with your health plan and social services organizations, so they can see if you qualify for any free non-medical services that could be helpful."),
      p("You can choose not to answer this survey, or you can choose to skip any questions that make you uncomfortable. Please know that we can only check for available assistance for the questions you do answer."),
      p(strong("Please click Start Survey if you would like to continue, or Exit if you do not want to participate.")),
      div(style = "margin-top: 30px;",
        actionButton("start_survey", "Start Survey", class = "welcome-btn btn-start"),
        actionButton("exit_survey", "Exit", class = "welcome-btn btn-exit")
      )
    )
  ),
  
  # Chat interface
  conditionalPanel(
    condition = "output.survey_started && !output.survey_complete",
    div(class = "chat-container",
      div(class = "messages-area", id = "messages_container",
        uiOutput("chat_messages")
      ),
      div(class = "input-area",
        uiOutput("input_controls"),
        div(class = "action-buttons",
          actionButton("skip_question", "Skip", class = "action-btn"),
          actionButton("exit_survey_chat", "Exit Survey", class = "action-btn")
        )
      )
    ),
    div(class = "progress-indicator",
      uiOutput("progress")
    )
  ),
  
  # Completion screen
  conditionalPanel(
    condition = "output.survey_complete",
    div(class = "welcome-screen",
      h3("Survey Complete!", style = "color: #28a745;"),
      p("Thank you for your responses.", style = "font-size: 16px; margin: 20px 0;"),
      actionButton("restart_survey", "Take Again", class = "welcome-btn btn-start")
    )
  ),
  
  # JavaScript for auto-scrolling
  tags$script(HTML("
    $(document).on('shiny:value', function(event) {
      if (event.name === 'chat_messages') {
        setTimeout(function() {
          var container = document.getElementById('messages_container');
          if (container) {
            container.scrollTop = container.scrollHeight;
          }
        }, 100);
      }
    });
  "))
)

# Server
server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    survey_started = FALSE,
    survey_complete = FALSE,
    current_idx = 1,
    session_id = as.character(as.numeric(Sys.time())),
    questions = NULL,
    chat_history = list(),
    conversation_context = list(),
    waiting_for_response = FALSE,
    selected_choice = NULL,
    waiting_for_continue = FALSE
  )
  
  # Load questions when app starts
  observe({
    if (is.null(values$questions)) {
      values$questions <- load_survey_questions()
    }
  })
  
  # Output flags for UI
  output$survey_started <- reactive({ values$survey_started })
  output$survey_complete <- reactive({ values$survey_complete })
  outputOptions(output, "survey_started", suspendWhenHidden = FALSE)
  outputOptions(output, "survey_complete", suspendWhenHidden = FALSE)
  
  # Start survey
  observeEvent(input$start_survey, {
    values$survey_started <- TRUE
    values$current_idx <- 1
    values$session_id <- as.character(as.numeric(Sys.time()))
    values$conversation_context <- list()
    values$chat_history <- list()
    
    # Add first question
    if (!is.null(values$questions)) {
      values$chat_history[[length(values$chat_history) + 1]] <- list(
        type = "bot",
        text = values$questions$question_text[1]
      )
    }
  })
  
  # Render chat messages
  output$chat_messages <- renderUI({
    messages <- lapply(values$chat_history, function(msg) {
      div(class = paste("message", msg$type),
        div(class = "message-bubble", msg$text)
      )
    })
    do.call(tagList, messages)
  })
  
  # Render input controls
  output$input_controls <- renderUI({
    if (values$current_idx > nrow(values$questions)) return(NULL)
    
    # If waiting for continue button, show it
    if (values$waiting_for_continue) {
      return(
        div(class = "continue-btn-container",
          actionButton("continue_after_support", "Continue", class = "continue-btn")
        )
      )
    }
    
    current_q <- values$questions[values$current_idx, ]
    
    if (current_q$response_options == "Open Text") {
      # Text input
      div(class = "text-input-container",
        textAreaInput("text_response", NULL, width = "100%", rows = 2, 
                     placeholder = "Type your response..."),
        actionButton("submit_text", "➤", class = "send-btn")
      )
    } else {
      # Multiple choice buttons
      options <- trimws(strsplit(current_q$response_options, "\\|")[[1]])
      div(
        div(class = "button-group",
          lapply(seq_along(options), function(i) {
            actionButton(paste0("choice_", i), options[i], 
                        class = "choice-btn",
                        onclick = paste0("Shiny.setInputValue('choice_clicked', ", i, ", {priority: 'event'});"))
          })
        )
      )
    }
  })
  
  # Handle choice button clicks
  observeEvent(input$choice_clicked, {
    if (values$current_idx <= nrow(values$questions)) {
      current_q <- values$questions[values$current_idx, ]
      options <- trimws(strsplit(current_q$response_options, "\\|")[[1]])
      selected_text <- options[input$choice_clicked]
      
      # Add user response to chat
      values$chat_history[[length(values$chat_history) + 1]] <- list(
        type = "user",
        text = selected_text
      )
      
      process_response(selected_text)
    }
  })
  
  # Handle text submission
  observeEvent(input$submit_text, {
    if (!is.null(input$text_response) && trimws(input$text_response) != "") {
      response_text <- input$text_response
      
      # Add user response to chat
      values$chat_history[[length(values$chat_history) + 1]] <- list(
        type = "user",
        text = response_text
      )
      
      process_response(response_text)
      updateTextAreaInput(session, "text_response", value = "")
    }
  })
  
  # Process response and advance
  process_response <- function(response_text) {
    current_q <- values$questions[values$current_idx, ]
    
    # Add to conversation context
    values$conversation_context[[length(values$conversation_context) + 1]] <- list(
      question = current_q$question_text,
      response = response_text,
      question_id = current_q$question_id,
      question_group = current_q$question_group
    )
    
    # Check for supportive response
    if (!is.na(current_q$last_question_in_group) && current_q$last_question_in_group == "Yes") {
      group_has_negatives <- FALSE
      current_group <- current_q$question_group
      
      for (ctx in values$conversation_context) {
        if (!is.null(ctx$question_group) && ctx$question_group == current_group) {
          q_row <- values$questions[values$questions$question_text == ctx$question, ]
          if (nrow(q_row) > 0 && !is.na(q_row$negative_responses[1]) && q_row$negative_responses[1] != "") {
            negative_opts <- trimws(strsplit(q_row$negative_responses[1], "\\|")[[1]])
            if (ctx$response %in% negative_opts) {
              group_has_negatives <- TRUE
              break
            }
          }
        }
      }
      
      if (group_has_negatives) {
        supportive_msg <- generate_supportive_response(
          current_q$question_text, 
          response_text,
          values$conversation_context,
          values$questions,
          current_group,
          values$current_idx
        )
        
        # Add supportive message to chat
        values$chat_history[[length(values$chat_history) + 1]] <- list(
          type = "supportive",
          text = supportive_msg
        )
        
        # Set flag to wait for continue button
        values$waiting_for_continue <- TRUE
        return()
      }
    }
    
    # Move to next question
    advance_to_next_question()
  }
  
  # Helper function to advance to next question
  advance_to_next_question <- function() {
    values$current_idx <- values$current_idx + 1
    
    if (values$current_idx <= nrow(values$questions)) {
      # Add next question to chat
      values$chat_history[[length(values$chat_history) + 1]] <- list(
        type = "bot",
        text = values$questions$question_text[values$current_idx]
      )
    } else {
      values$survey_complete <- TRUE
    }
  }
  
  # Handle continue button after supportive message
  observeEvent(input$continue_after_support, {
    values$waiting_for_continue <- FALSE
    advance_to_next_question()
  })
  
  # Skip question
  observeEvent(input$skip_question, {
    if (values$current_idx <= nrow(values$questions)) {
      current_q <- values$questions[values$current_idx, ]
      
      # Add skipped response to chat and context
      values$chat_history[[length(values$chat_history) + 1]] <- list(
        type = "user",
        text = "[Skipped]"
      )
      
      values$conversation_context[[length(values$conversation_context) + 1]] <- list(
        question = current_q$question_text,
        response = "SKIPPED",
        question_id = current_q$question_id,
        question_group = current_q$question_group
      )
      
      values$current_idx <- values$current_idx + 1
      
      if (values$current_idx <= nrow(values$questions)) {
        values$chat_history[[length(values$chat_history) + 1]] <- list(
          type = "bot",
          text = values$questions$question_text[values$current_idx]
        )
      } else {
        values$survey_complete <- TRUE
      }
    }
  })
  
  # Exit survey
  observeEvent(input$exit_survey, {
    values$survey_complete <- TRUE
    values$survey_started <- FALSE
  })
  
  observeEvent(input$exit_survey_chat, {
    values$survey_complete <- TRUE
    values$survey_started <- FALSE
  })
  
  # Restart survey
  observeEvent(input$restart_survey, {
    values$survey_started <- FALSE
    values$survey_complete <- FALSE
    values$current_idx <- 1
    values$chat_history <- list()
    values$conversation_context <- list()
  })
  
  # Progress info
  output$progress <- renderUI({
    if (values$survey_started && !values$survey_complete && !is.null(values$questions)) {
      p(paste("Question", values$current_idx, "of", nrow(values$questions)))
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server,
         options = list(launch.browser = TRUE, port = 8888))