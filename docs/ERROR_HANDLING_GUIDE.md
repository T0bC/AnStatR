# Error Handling & Logging - TexAn 2.0

> **FOR LLMs:** MANDATORY pattern for ALL new code. No exceptions.

## The Pattern (Copy This)

```r
result <- safe_run(
  expr = {
    log_info("Starting operation")
    data <- risky_operation()
    if (!is_valid(data)) stop("Validation failed")
    log_info("Success")
    data
  },
  context = "module:operation",
  session = session,
  user_msg = "User-friendly error message here"
)

if (!is.null(result)) {
  # Success - process result
}
```

## Key Rules

1. **Wrap risky code** → Use `safe_run()`
2. **Log operations** → Use `log_info()`, `log_warn()`, `log_error()`
3. **Validate with `stop()`** → Triggers user modal (NOT console)
4. **Check result** → `safe_run()` returns `NULL` on error
5. **Context format** → `"module_name:operation_name"`

## Available Functions

### Logging (`R/utils/logging.R`)
- `log_info(msg, ...)` - Info messages
- `log_warn(msg, ...)` - Warnings  
- `log_error(msg, ...)` - Errors
- `log_debug(msg, ...)` - Debug

### Error Handling (`R/utils/error_handling.R`)
- `safe_run(expr, context, session, user_msg, show_modal=TRUE, on_error=NULL)`
- `setup_global_error_handler()` - Call once in app.R
- `setup_session_logging(session)` - Call in app_server

## App Initialization (app.R)

```r
# 1. Source utilities FIRST
source("R/utils/logging.R")
source("R/utils/error_handling.R")

# 2. Source modules
source("R/ui/modules/pages/ui_*.R")
source("R/server/modules/pages/server_*.R")

# 3. Load packages
library(shiny)

# 4. Initialize
init_logging(log_dir = "logs", log_level = "INFO", console_log = TRUE)
log_app_startup()
setup_global_error_handler()

# 5. Define app
app_server <- function(input, output, session) {
  setup_session_logging(session)
  # ... register modules
}
```

## Real Example (from server_load_data.R)

```r
# Simple validation - no safe_run needed
if (!identical(file_ext, "xlsx")) {
  log_warn("Unsupported file type: {ext}", ext = file_ext)
  showNotification("Only XLSX files supported", type = "warning")
  return()
}

# Risky operation - MUST use safe_run
result <- safe_run(
  expr = {
    log_info("Reading Excel: {name}", name = file_info$name)
    data <- openxlsx::read.xlsx(file_info$datapath, sheet = 1)
    
    if (!is.data.frame(data) || nrow(data) == 0) {
      stop("File is empty or invalid")  # Shows modal!
    }
    
    log_info("Loaded {rows} rows, {cols} cols", rows = nrow(data), cols = ncol(data))
    data
  },
  context = "load_data:read_excel",
  session = session,
  user_msg = "Unable to read Excel file. Ensure it's valid XLSX with data in sheet 1.",
  on_error = function(e) { loaded_data(NULL) }
)

if (!is.null(result)) {
  loaded_data(result)
  showNotification("Data loaded!", type = "message")
}
```

## DO / DON'T

### ✅ DO
- Wrap file I/O, DB queries, API calls in `safe_run()`
- Use `log_*()` wrappers (NOT `logger::log_*()` directly)
- Use `stop()` for validation (triggers modal)
- Provide clear `user_msg`
- Check `!is.null(result)`

### ❌ DON'T
- Use `tryCatch()` directly
- Use `logger::log_*()` directly
- Put error handling in `app.R`
- Show raw errors to users
- Assume operations succeed

## Shiny Options (Configured Automatically)

```r
setup_global_error_handler()  # Sets:
# - shiny.error = custom handler
# - shiny.sanitize.errors = FALSE
```

**Why FALSE?** We control ALL error display. Users see friendly messages, devs get full logs.

## Quick Checklist

When adding new code:
- [ ] Risky operations wrapped in `safe_run()`?
- [ ] Using `log_*()` functions?
- [ ] Clear `user_msg` provided?
- [ ] Descriptive `context` string?
- [ ] Checking `!is.null(result)`?
- [ ] Using `stop()` for validation?

---

**Full documentation:** See `ERROR_HANDLING_GUIDE_FULL.md` for detailed explanations.
