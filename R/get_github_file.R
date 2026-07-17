#' Read the pods lookup table from a YAML file in the NHP Outputs repo
read_pods_lookup <- function(file = "golem-config.yml") {
  yaml12::parse_yaml(readr::read_lines(get_outputs_gh_url(file)))
}
possibly_read_pods_lookup <- \(...) purrr::possibly(read_pods_lookup)(...)

#' Read the TPMAs lookup table from a CSV file in the TPMAs repo
read_tpmas_lookup <- function(file = "tpma-lookup.csv") {
  readr::read_csv(get_tpmas_gh_url(file), col_types = "-ccccc---c")
}
possibly_read_tpmas_lookup <- \(...) purrr::possibly(read_tpmas_lookup)(...)


#' Get the direct URL to a file in the NHP Outputs GitHub repo
#'
#' @param ... Pass the name of the file in via `...`
#' @keywords internal
get_outputs_gh_url <- function(...) {
  purrr::partial(get_su_gh_file, repo = "nhp_outputs", folder = "inst")(...)
}

#' Get the direct URL to a file in the TPMAs GitHub repo
#'
#' @inheritParams get_outputs_gh_url
#' @keywords internal
get_tpmas_gh_url <- function(...) {
  purrr::partial(get_su_gh_file, repo = "TPMAs", folder = "reference")(...)
}


#' Read in a file from a Strategy Unit GitHub repo
#'
#' @param repo string. The name of the repository in which to find the file
#' @param folder string. The folder where the file is located. Set to `""` to
#'   use the root folder of the repo.
#' @param file string. The name of the file to read in
#' @returns The URL to the raw file contents, to be passed to a reader function
#' @keywords internal
get_su_gh_file <- function(repo, folder, file, pat = get_github_pat()) {
  ua_string <- "The-Strategy-Unit/nhp_reskit R package"
  resp <- httr2::request("https://api.github.com") |>
    httr2::req_url_path_append("repos") |>
    httr2::req_url_path_append("The-Strategy-Unit") |>
    httr2::req_url_path_append(repo) |>
    httr2::req_url_path_append("contents") |>
    httr2::req_url_path_append(folder) |>
    httr2::req_url_path_append(file) |>
    httr2::req_headers(`User-Agent` = ua_string) |>
    httr2::req_headers(`X-GitHub-Api-Version` = "2026-03-10") |>
    httr2::req_headers(`Accept` = "application/vnd.github+json") |>
    httr2::req_auth_bearer_token(pat) |>
    httr2::req_perform() |>
    httr2::resp_check_status()

  base64enc::base64decode(httr2::resp_body_json(resp)[["content"]])
}


#' Read in a file from the NHP Outputs app GitHub repo
#'
#' @param ... Pass the name of the file in via `...`
#' @keywords internal
get_outputs_gh_file <- function(...) {
  purrr::partial(get_su_gh_file, repo = "nhp_outputs", folder = "inst")(...)
}


#' Read in a file from the TPMAs GitHub repo
#'
#' @inheritParams get_outputs_gh_file
#' @keywords internal
get_tpmas_gh_file <- function(...) {
  purrr::partial(get_su_gh_file, repo = "TPMAs", folder = "reference")(...)
}

possibly_get_outputs_gh_file <- \(...) purrr::possibly(get_outputs_gh_file)(...)
possibly_get_tpmas_gh_file <- \(...) purrr::possibly(get_tpmas_gh_file)(...)


#' Return a GitHub access token
#'
#' If the envvar "RESKIT_GH_APP_PAT" is set, this will be used as the token.
#' Otherwise, the envvars "RESKIT_GH_APP_ID" and "RESKIT_GH_APP_PRIVATE_KEY"
#'  must be set. The former as a string, the latter as a path to a PEM key.
#' @returns A personal access token (PAT) with permissions granted to the app
#' @keywords internal
get_github_pat <- function() {
  pat <- Sys.getenv("RESKIT_GH_APP_PAT")
  if (nzchar(pat)) {
    pat
  } else {
    msg1 <- "Environment variable {.var RESKIT_GH_APP_ID} is not set"
    msg2 <- "Environment variable {.var RESKIT_GH_APP_PRIVATE_KEY} is not set"
    azkit::check_nzchar(Sys.getenv("RESKIT_GH_APP_ID"), msg1)
    azkit::check_nzchar(Sys.getenv("RESKIT_GH_APP_PRIVATE_KEY"), msg2)
    get_github_pat_via_api()
  }
}


#' Return a GitHub PAT via the API using the reskit authentication GitHub app
#'
#' The envvars "RESKIT_GH_APP_ID" and "RESKIT_GH_APP_PRIVATE_KEY" must
#'  be set. The former as a string, the latter as a path to a PEM key.
#' @returns An access token (PAT) as a string
#' @keywords internal
get_github_pat_via_api <- function() {
  ua_string <- "The-Strategy-Unit/nhp_reskit R package"
  jwt <- get_github_jwt()
  installation_id <- get_github_app_installation_id(jwt)
  req <- httr2::request("https://api.github.com/")
  resp <- req |>
    httr2::req_url_path_append("app") |>
    httr2::req_url_path_append("installations") |>
    httr2::req_url_path_append(installation_id) |>
    httr2::req_url_path_append("access_tokens") |>
    httr2::req_auth_bearer_token(jwt) |>
    httr2::req_headers(`User-Agent` = ua_string) |>
    httr2::req_headers(`X-GitHub-Api-Version` = "2026-03-10") |>
    httr2::req_headers(`Accept` = "application/vnd.github+json") |>
    httr2::req_method("POST") |>
    httr2::req_perform() |>
    httr2::resp_check_status()

  httr2::resp_body_json(resp)[["token"]]
}


#' Returns the installation ID for the reskit GitHub app
#'
#' The envvars "RESKIT_GH_APP_ID" and "RESKIT_GH_APP_PRIVATE_KEY" must
#'  be set. The former as a string, the latter as a path to a PEM key.
#' @param jwt JWT for the app. Defaults to the output of `get_github_jwt()`.
#' @returns An installation ID as an integer
#' @keywords internal
get_github_app_installation_id <- function(jwt = get_github_jwt()) {
  ua_string <- "The-Strategy-Unit/nhp_reskit R package"
  req <- httr2::request("https://api.github.com/")
  resp <- req |>
    httr2::req_url_path_append("app") |>
    httr2::req_url_path_append("installations") |>
    httr2::req_auth_bearer_token(jwt) |>
    httr2::req_headers(`User-Agent` = ua_string) |>
    httr2::req_headers(`X-GitHub-Api-Version` = "2026-03-10") |>
    httr2::req_headers(`Accept` = "application/vnd.github+json") |>
    httr2::req_perform() |>
    httr2::resp_check_status()
  httr2::resp_body_json(resp)[[1]][["id"]]
}


#' Returns a GitHub JWT for the reskit authentication app
#'
#' The envvars "RESKIT_GH_APP_ID" and "RESKIT_GH_APP_PRIVATE_KEY" must
#'  be set. The former as a string, the latter as a path to a PEM key.
#' @returns A JSON Web Token (JWT) as a string
#' @keywords internal
get_github_jwt <- function() {
  claim <- httr2::jwt_claim(Sys.getenv("RESKIT_GH_APP_ID"))
  key <- openssl::read_key(Sys.getenv("RESKIT_GH_APP_PRIVATE_KEY"))
  httr2::jwt_encode_sig(claim, key)
}
