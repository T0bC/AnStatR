# Shiny Error Handling Options Reference

## Currently Configured Options

### `shiny.sanitize.errors = FALSE`

**Location:** `app.R` line 82

**Purpose:** Controls whether Shiny sanitizes error messages to hide sensitive information.

**Our Setting:** `FALSE` - Show full error messages

**Rationale:** 
- We control all error display through `safe_run()` and `handle_app_error()`
- Users see our custom `user_msg` (safe and friendly)
- Technical details are in an expandable section (opt-in)
- Developers get full error context in logs
- This gives us complete control over what users see

### `shiny.error = function()`

**Location:** `app.R` lines 31-78

**Purpose:** Global fallback handler for unhandled errors

**Our Implementation:**
- Logs the error with `log_error()`
- Shows a user-friendly modal with technical details
- Keeps the app running
- Acts as a safety net for errors not wrapped in `safe_run()`

## Other Available Options (Not Currently Used)

### `shiny.fullstacktrace = TRUE`

```r
options(shiny.fullstacktrace = TRUE)
```

**What it does:** Shows full R stack traces in error messages

**Should we use it?** 
- ✅ **Recommended for development**
- ❌ **Not for production** (too verbose for users)

**How to implement:**
```r
# In app.R, make it environment-dependent
if (Sys.getenv("R_ENV") == "development") {
  options(shiny.fullstacktrace = TRUE)
}
```

### `shiny.trace = TRUE`

```r
options(shiny.trace = TRUE)
```

**What it does:** Prints detailed execution traces to console

**Should we use it?**
- ✅ **Useful for debugging reactive chains**
- ❌ **Not for production** (very verbose)

### `shiny.reactlog = TRUE`

```r
options(shiny.reactlog = TRUE)
```

**What it does:** Enables the reactive log visualizer (press Ctrl+F3 in browser)

**Should we use it?**
- ✅ **Excellent for development** - visualize reactive dependencies
- ❌ **Disable in production** (performance overhead)

**How to implement:**
```r
# In app.R
if (Sys.getenv("R_ENV") == "development") {
  options(shiny.reactlog = TRUE)
}
```

### `shiny.maxRequestSize`

```r
options(shiny.maxRequestSize = 30*1024^2)  # 30MB
```

**What it does:** Maximum file upload size (default: 5MB)

**Should we use it?**
- ✅ **Yes, if users upload large Excel files**

**Current status:** Not set (using 5MB default)

### `shiny.silent.error = TRUE`

```r
options(shiny.silent.error = TRUE)
```

**What it does:** Suppresses error messages in the R console

**Should we use it?**
- ❌ **No** - We want errors in console for development
- Our logging system already captures everything

## Recommended Configuration

### Development Environment

```r
# app.R - Add environment detection
is_dev <- Sys.getenv("R_ENV") == "development" || interactive()

if (is_dev) {
  options(
    shiny.fullstacktrace = TRUE,
    shiny.reactlog = TRUE,
    shiny.trace = FALSE  # Too verbose even for dev
  )
}
```

### Production Environment

```r
# Current configuration is good for production
options(
  shiny.error = function() { /* our custom handler */ },
  shiny.sanitize.errors = FALSE,  # We control display
  shiny.maxRequestSize = 30*1024^2  # Allow larger files
)
```

## Summary

### ✅ What We Have

1. **Custom global error handler** - Catches unhandled errors
2. **`shiny.sanitize.errors = FALSE`** - Full control over error display
3. **`safe_run()` wrapper** - Handles all risky operations
4. **Logging system** - Captures everything to daily log files

### 🎯 What We Could Add

1. **Environment-based options** - Different settings for dev/prod
2. **`shiny.maxRequestSize`** - If users need to upload large files
3. **`shiny.reactlog`** - For development debugging

### ❌ What We Don't Need

1. **`shiny.silent.error`** - We want console output
2. **`shiny.trace`** - Too verbose
3. **`shiny.fullstacktrace`** - Only useful in development

## Implementation Example

If you want environment-based configuration:

```r
# app.R - Add after sourcing utilities

# Detect environment
is_dev <- Sys.getenv("R_ENV") == "development" || interactive()

# Initialize logging with appropriate level
init_logging(
  log_dir = "logs",
  log_level = if (is_dev) "DEBUG" else "INFO",
  console_log = is_dev
)

# Set environment-specific options
if (is_dev) {
  options(
    shiny.fullstacktrace = TRUE,
    shiny.reactlog = TRUE
  )
  log_info("Running in DEVELOPMENT mode")
} else {
  log_info("Running in PRODUCTION mode")
}

# Set common options (both dev and prod)
options(
  shiny.error = function() { /* our handler */ },
  shiny.sanitize.errors = FALSE,
  shiny.maxRequestSize = 30*1024^2
)
```

## Testing the Error Handler

To test that unhandled errors are caught:

```r
# Add a test button in dev mode
if (is_dev) {
  # In UI
  actionButton("test_error", "Test Error Handler")
  
  # In server
  observeEvent(input$test_error, {
    stop("This is a test error!")
  })
}
```

This should trigger your global `shiny.error` handler and show the modal.
