
aurora_models <- function() {
  # only nr 2 and 3 below seem to work as of Sept 30, 2024
  c("aurora-sdg", "aurora-sdg-multi", "elsevier-sdg-multi", "osdg")
}

aurora_classify <- function(snippet, model = aurora_models()) {

  if (missing(model)) 
    model <- aurora_models()[2]

  mod <- match.arg(model, aurora_models(), several.ok = FALSE)

  url <- "https://aurora-sdg.labs.vu.nl/classifier/classify/%s" |>
    sprintf(mod)

  payload <- jsonlite::toJSON(list(text = snippet), auto_unbox = TRUE)

  httr::POST(url, body = payload, httr::content_type_json())
}

aurora_parse_response <- function(x, fmt = c("table", "object")) {

  prediction <- NULL

  fmt <- match.arg(fmt, several.ok = FALSE)

  res <- 
    x |> httr::content()  |> rvest::html_text()  |> 
    jsonlite::fromJSON()

  if (fmt == "object") return(res)
  
  res |> getElement("predictions")  |> 
    tibble::as_tibble()  |> 
    dplyr::arrange(desc(prediction)) |> 
    tidyr::unnest(cols = any_of(c("sdg", "prediction"))) |> 
    dplyr::rename(c(pred_type = "@type"))

}

#' SDG classification for a text snippet
#' 
#' Predict a suitable SDG goal for the provided text
#' 
#' See details about the API: <https://aurora-sdg.labs.vu.nl/sdg-classifier/api>
#' @param snippet the text snippet, character
#' @param fmt the response returned, one of "table" or "object"
#' @return either table or object with the response
#' @export 
aurora <- function(snippet, fmt = c("table", "object")) {

  fmt <- match.arg(fmt, several.ok = FALSE)

  snippet |> 
    aurora_classify() |> 
    aurora_parse_response(fmt = fmt)
}
