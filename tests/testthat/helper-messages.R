# Capture all message() conditions emitted while evaluating `expr` and return
# them as a character vector (with the messages muffled so they do not clutter
# the test output).
catch_messages <- function(expr) {
  msgs <- character()
  withCallingHandlers(
    expr,
    message = function(c) {
      msgs[[length(msgs) + 1]] <<- conditionMessage(c)
      invokeRestart("muffleMessage")
    }
  )
  msgs
}
