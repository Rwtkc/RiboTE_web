create_session_state <- function() {
  reactiveValues(
    upload_context = NULL,
    upload_saved = FALSE,
    preprocess_context = NULL,
    preprocess_ready = FALSE,
    te_context = NULL,
    te_ready = FALSE,
    last_module = NULL
  )
}

ribote_has_upload <- function(session_state) {
  !is.null(session_state$upload_context) && isTRUE(session_state$upload_saved)
}

ribote_has_te <- function(session_state) {
  isTRUE(session_state$te_ready) && !is.null(session_state$te_context)
}

ribote_has_preprocess <- function(session_state) {
  isTRUE(session_state$preprocess_ready) && !is.null(session_state$preprocess_context)
}
