#' Query the USGS M2M API
#' @param endpoint The API access point (e.g., "login-token", "scene-search")
#' @param parameters The parameters to send in the request body
#' @param session The session token for authenticated requests (default: NULL)
#' @export
#' @return Data frame of available scenes
query_usgs <- function(endpoint, parameters, session = NULL) {
  arguments <- list(
    url = paste0(API, endpoint),
    body = parameters,
    encode = "json",
    timeout(120)
  ) # créer les arguments pour la connexion avec les paramètres prédéfinis
  if (!is.null(session)) {
    arguments <- c(arguments, list(add_headers("X-Auth-Token" = session)))
  }
  response <- do.call(POST, arguments) #communication avec l'api
  stop_for_status(response) # vérifie que le serveur répond
  result <- content(response, as = "parsed") # met en forme le résultat
  if (!is.null(result$errorCode)) stop(result$errorMessage)
  if(endpoint=="login-token"){
    session <- result$data
    if (is.null(session) || !nzchar(session)) stop("No connection to USGS.")
    cat("Connection to USGS was established.\n")
  }
  return(result$data)
}
