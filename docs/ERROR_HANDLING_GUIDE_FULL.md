# Error Handling & Logging Guide for TexAn 2.0

> **MANDATORY PATTERN:** All risky operations MUST use `safe_run()`. All logging MUST use `log_*()` wrappers.

## Core Concept

**Three-layer system:**
1. `safe_run()` - Wraps risky code (PRIMARY)
2. Global handler - Catches unhandled errors (FALLBACK)
3. Daily logs - Records everything (PERSISTENCE)

**Result:** Errors never crash the UI. Users see friendly messages. Developers get full logs.

### Example: Loading Different File Types

```r
# CSV file loading
result <- safe_run(
  expr = {
    log_info("Reading CSV file: {name}", name = file_info$name)
    
    data <- read.csv(file_info$datapath)
    
    # Any stop() here triggers the user-friendly modal!
    if (ncol(data) < 2) {
      stop("CSV file must have at least 2 columns.")
    }
    
    log_info("CSV loaded: {rows} rows", rows = nrow(data))
    data
  },
  context = "load_data:read_csv",
  session = session,
  user_msg = "Unable to read the CSV file. Please check the file format.",
  show_modal = TRUE,
  on_error = function(e) {
    loaded_data(NULL)  # Reset state on error
  }
)

# JSON file loading
result <- safe_run(
  expr = {
    log_info("Reading JSON file: {name}", name = file_info$name)
    
    data <- jsonlite::fromJSON(file_info$datapath)
    
    # Validation errors are caught and shown to user
    if (!is.list(data)) {
      stop("Invalid JSON structure.")
    }
    
    log_info("JSON loaded successfully")
    data
  },
  context = "load_data:read_json",
  session = session,
  user_msg = "Unable to parse the JSON file.",
  show_modal = TRUE
)
```

## Key Points

### 1. **`stop()` Shows the Modal - Not the Console!**

When you call `stop()` inside `safe_run()`:
- ❌ It does **NOT** print to the console
- ✅ It **DOES** trigger the error modal with your `user_msg`
- ✅ The technical details (including your stop message) are in the expandable section
- ✅ Everything is logged to the daily log file

```r
# This stop() will show a nice modal to the user
if (nrow(data) == 0) {
  stop("The uploaded file appears to be empty or invalid.")
}
```

### 2. **Clean Logging - No More Verbose Checks**

Instead of:
```r
# ❌ Verbose and repetitive
if (requireNamespace("logger", quietly = TRUE)) {
  logger::log_info("Reading file: {name}", name = filename)
}
```

Use:
```r
# ✅ Clean and simple
log_info("Reading file: {name}", name = filename)
```

Available wrappers:
- `log_info()` - General information
- `log_warn()` - Warnings
- `log_error()` - Errors
- `log_debug()` - Debug messages

### 3. **Modal vs Notification**

```r
# Show modal (blocks interaction, good for critical errors)
safe_run(
  expr = { ... },
  show_modal = TRUE  # Default
)

# Show notification (non-blocking, good for minor issues)
safe_run(
  expr = { ... },
  show_modal = FALSE
)
```

## Pattern for New Modules

```r
server_my_module <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    
    shiny::observeEvent(input$action_button, {
      result <- safe_run(
        expr = {
          # 1. Log what you're doing
          log_info("Starting operation X")
          
          # 2. Do the risky work
          data <- some_risky_function()
          
          # 3. Validate results
          if (!is_valid(data)) {
            stop("Validation failed: reason here")
          }
          
          # 4. Log success
          log_info("Operation X completed successfully")
          
          # 5. Return the result
          data
        },
        context = "my_module:operation_x",
        session = session,
        user_msg = "Unable to complete operation X. Please try again.",
        show_modal = TRUE,
        on_error = function(e) {
          # Optional: cleanup on error
          reset_state()
        }
      )
      
      # Check if operation succeeded
      if (!is.null(result)) {
        # Process successful result
        process_data(result)
      }
    })
  })
}
```

## File Structure & Initialization

### Utility Files (Already Created)

```
R/utils/
├── logging.R              # Logging functions and setup
└── error_handling.R       # Error handling functions
```

### Key Functions Available

#### From `R/utils/logging.R`:
- `init_logging()` - Initialize logging system
- `log_app_startup()` - Log application startup info
- `setup_session_logging(session)` - Setup session tracking
- `log_info(msg, ...)` - Log info messages
- `log_warn(msg, ...)` - Log warnings
- `log_error(msg, ...)` - Log errors
- `log_debug(msg, ...)` - Log debug messages

#### From `R/utils/error_handling.R`:
- `setup_global_error_handler()` - Configure global Shiny error handler
- `safe_run(expr, context, session, user_msg, ...)` - Wrap risky code
- `handle_app_error(error, context, session, user_msg, ...)` - Handle errors

### App Initialization Pattern (app.R)

```r
# 1. Source utilities FIRST
source("R/utils/logging.R")
source("R/utils/error_handling.R")

# 2. Source modules
source("R/ui/modules/pages/ui_load_data.R")
source("R/server/modules/pages/server_load_data.R")
# ... other modules

# 3. Load packages
library(shiny)
library(openxlsx)
# ... other packages

# 4. Initialize logging
init_logging(
  log_dir = "logs",
  log_level = "INFO",
  console_log = TRUE
)

# 5. Log startup
log_app_startup()

# 6. Setup global error handler
setup_global_error_handler()

# 7. Define UI and server
app_ui <- fluidPage(...)

app_server <- function(input, output, session) {
  # Setup session logging
  setup_session_logging(session)
  
  # Register modules
  server_load_data("load_data_id")
  # ... other modules
}

shinyApp(ui = app_ui, server = app_server)
```

## Shiny Options Configuration

### Currently Configured (via `setup_global_error_handler()`)

```r
options(
  shiny.error = function() {
    # Custom handler that:
    # - Logs the error
    # - Shows user-friendly modal
    # - Keeps app running
  },
  shiny.sanitize.errors = FALSE  # We control error display
)
```

**Why `shiny.sanitize.errors = FALSE`?**
- We control ALL error display through `safe_run()` and `handle_app_error()`
- Users see our custom `user_msg` (safe and friendly)
- Technical details are in expandable section (opt-in)
- Developers get full error context in logs

### Optional: Environment-Based Configuration

For development vs production:

```r
# In app.R, after sourcing utilities
is_dev <- Sys.getenv("R_ENV") == "development" || interactive()

if (is_dev) {
  options(
    shiny.fullstacktrace = TRUE,  # Full stack traces
    shiny.reactlog = TRUE          # Enable reactive log (Ctrl+F3)
  )
  log_info("Running in DEVELOPMENT mode")
} else {
  log_info("Running in PRODUCTION mode")
}

# Optional: Increase file upload size
options(shiny.maxRequestSize = 30*1024^2)  # 30MB
```

## Admin Panel

The app includes an **Admin** tab for log inspection:
- View daily log files
- See error/warning/info counts
- Download logs for debugging
- Clean up old log files

**Access:** Navigate to the "Admin" tab in the app UI

## Installation Requirements

The system requires the `logger` package:

```r
install.packages("logger")
```

**Note:** If `logger` is not installed, the app will still work but logging will be limited to console output.

## Quick Reference for LLMs

### When Adding New Features:

1. **Wrap ALL risky operations in `safe_run()`**
   - File I/O, database queries, API calls, data transformations

2. **Use logging functions (NOT `logger::log_*` directly)**
   - `log_info()`, `log_warn()`, `log_error()`, `log_debug()`

3. **Use `stop()` for validation errors**
   - It triggers the modal, NOT console output

4. **Always provide clear `user_msg`**
   - Tell users WHAT went wrong and WHAT to do

5. **Use descriptive `context` strings**
   - Format: `"module_name:operation_name"`
   - Example: `"load_data:read_excel"`

6. **Check if result is `NULL`**
   - `safe_run()` returns `NULL` on error

### DO NOT:

❌ Use `tryCatch()` directly - use `safe_run()` instead
❌ Use `logger::log_*()` directly - use `log_*()` wrappers
❌ Put error handling code in `app.R` - use utility functions
❌ Show raw error messages to users - provide friendly `user_msg`
❌ Assume operations will succeed - always wrap in `safe_run()`

## Real-World Implementation Example

Here's the actual implementation from `server_load_data.R` showing best practices:

```r
server_load_data <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    loaded_data <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$data_file, {
      shiny::req(input$data_file)
      file_info <- input$data_file
      file_ext <- tolower(tools::file_ext(file_info$name))

      # Validate file type (simple validation, no safe_run needed)
      if (!identical(file_ext, "xlsx")) {
        loaded_data(NULL)
        shiny::showNotification(
          "Only XLSX files are currently supported.",
          type = "warning"
        )
        log_warn("User attempted to upload unsupported file type: {ext}", ext = file_ext)
        return()
      }

      # Use safe_run for risky file reading operation
      result <- safe_run(
        expr = {
          # 1. Log what we're doing
          log_info("Reading Excel file: {name}", name = file_info$name)
          
          # 2. Perform risky operation
          data <- openxlsx::read.xlsx(
            xlsxFile = file_info$datapath,
            sheet = 1
          )
          
          # 3. Validate results (stop() triggers modal!)
          if (!is.data.frame(data) || nrow(data) == 0) {
            stop("The uploaded file appears to be empty or invalid.")
          }
          
          # 4. Log success
          log_info(
            "Successfully loaded data: {rows} rows, {cols} columns",
            rows = nrow(data),
            cols = ncol(data)
          )
          
          # 5. Return result
          data
        },
        context = "load_data:read_excel",
        session = session,
        user_msg = paste(
          "Unable to read the uploaded Excel file.",
          "Please ensure it is a valid XLSX file with data in the first sheet."
        ),
        show_modal = TRUE,
        on_error = function(e) {
          # Custom cleanup on error
          loaded_data(NULL)
        }
      )
      
      # 6. Check if operation succeeded
      if (!is.null(result)) {
        loaded_data(result)
        shiny::showNotification(
          "Data loaded successfully!",
          type = "message",
          duration = 3
        )
      }
    })

    # Render with safe_run (use show_modal = FALSE for rendering)
    output$data_preview <- DT::renderDataTable({
      shiny::req(loaded_data())
      
      safe_run(
        expr = {
          DT::datatable(
            loaded_data(),
            options = list(pageLength = 10, scrollX = TRUE),
            rownames = FALSE
          )
        },
        context = "load_data:render_datatable",
        session = session,
        user_msg = "Unable to display the data table.",
        show_modal = FALSE  # Notification instead of modal
      )
    })
  })
}
```

### Key Takeaways from This Example

1. **Simple validation** (file type check) doesn't need `safe_run()`
2. **Risky operations** (file I/O) are wrapped in `safe_run()`
3. **Logging** at key points: start, success, warnings
4. **Validation with `stop()`** triggers user-friendly modal
5. **Custom cleanup** via `on_error` callback
6. **Result checking** with `!is.null(result)`
7. **Rendering errors** use `show_modal = FALSE` for less intrusive feedback

## Summary

**The mandatory pattern:**
1. ✅ Wrap risky code in `safe_run()`
2. ✅ Use `log_info()`, `log_warn()`, etc. for logging
3. ✅ Use `stop()` for validation errors - it triggers the modal!
4. ✅ Provide a clear `user_msg` for end users
5. ✅ Check if result is `NULL` to handle errors
6. ✅ Use descriptive `context` strings for debugging

---

**For LLMs:** When implementing new features, copy the pattern from the real-world example above. This is the EXACT pattern used throughout the application.
