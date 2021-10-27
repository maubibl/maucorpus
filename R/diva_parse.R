extract_re <- function(x, re, mode = c("regexec", "gregexpr")) {
  fn <- switch(match.arg(mode), regexec = regexec(re, x), gregexpr = gregexpr(re, x))
  res <- lapply(regmatches(x, fn), "[", -1)
  sapply(res, function(s) if (length(s) == 0) NA_character_ else paste(s, collapse = " "))
}

extract_re2 <- function(x, re) {
  res <- lapply(x, function(y) stringi::stri_match_all_regex(y, re)[[1]][, 2])
  vapply(res, function(s) ifelse(length(s) > 1, paste(s, collapse = " "), s), character(1))
}

extract_re3 <- function(x, re, capture_group = 2) {
  stringi::stri_match_all_regex(x, re)[[1]][, capture_group]
}

extract_aff_count <- function(x) {
  max(as.integer(extract_re3(x, "AFF#(\\d+)", 2)))
}

extract_fullname <- function(x) {
  trimws(extract_re(x, "([^(\\[|\\(]*)"))
}

extract_kthid <- function(x) {
  extract_re2(x, "\\[(u1.{6})\\]")
}

extract_orcid <- function(x) {

  re <- sprintf("\\[(%s)\\]",
    paste0(collapse = "-", c(rep("\\d{4}", 3), "\\d{3}[1-9Xx]")))

  extract_re(x, re)
}

extract_orgids <- function(x) {
  extract_re2(x, "\\[(\\d+)\\]")
}

extract_inner_parens <- function(s) {
  # looks behind for an open parenthesis ?<=\\(
  # looks ahead for a closing parenthesis ?=\\)
  # and grabs everything in the middle (lazily) .+?
  # in other words (?<=\\().+?(?=\\))
  res <- regmatches(s, gregexpr("(?<=\\().+?(?=\\))", s, perl = T))
  res[[1]]
}

extract_outer_parens <- function(s) {
  # looks recursively for outer parenthesis
  matched <- gregexpr("\\((?>[^()]|(?R))*\\)", s, perl = T)
  substring(s, matched[[1]], matched[[1]] + attr(matched[[1]], "match.length") - 1)
}

extract_affs <- function(s) {
  affs <- gsub("\\(\\s*(.*?)\\s*\\)$", "\\1", extract_outer_parens(s))
  affs[nzchar(affs) > 0]
}

extract_extorgs <- function(x) {
  # NB modified from
  # extract_re2(extract_re2(x, "(\\([^(\\[|\\(]*\\))$"), "\\((.*?)\\)")
  # use second parens ONLY
  extract_affs(x)[2]
}

tokenize_affs <- function(x) {
  m <- gregexpr(pattern = "[^;]*?\\s*(\\(\\{AFF#\\d+\\}\\)\\s*)+", x, perl = T)
  unlist(regmatches(x, m))
}

extract_aff_tokens <- function(x) {
  as.integer(extract_re3(x, "AFF#(\\d+)", 2))
}

has_etal <- function(x) {
  re <- ";?\\s*et al\\.?,?"
  any(stringi::stri_detect_regex(x, re))
}

replace_etal <- function(x) {
  re <- ";?\\s*et al\\.?,?"
  if (!has_etal(x)) return(x)
  stringi::stri_replace_all(x, replacement = "", regex = re)
}

#' Parse a DiVA namestring with bibliographic data
#'
#' This function parses a string and returns the components (such as
#' name, kthid and ORCID, org affiliation)
#'
#' @param diva_name the string with author names and identifiers
#' @return data frame with results
#' @export
#' @importFrom stringi stri_detect_regex stri_replace_all stri_match_all
#' @importFrom dplyr tibble
parse_diva_name <- function(diva_name) {

  # diva_name <- "Jia, Shi;Pang, Xiaodan [u1e3qilb] (KTH [177], Skolan för informations- och kommunikationsteknik (ICT) [5994]) (RISE Acreo AB, NETLAB, SE-16425 Kista, Sweden);Ozolins, Oskars;Yu, Xianbin;Hu, Hao;Yu, Jinlong;Guan, Pengyu;Da Ros, Francesco;Popov, Sergei [u1fs8je2] (KTH [177], Skolan för informations- och kommunikationsteknik (ICT) [5994]);Jacobsen, Gunnar;Galili, Michael;Morioka, Toshio;Zibar, Darko;Oxenlowe, Leif K."
  # diva_name <- "Asem, Heba [u1dc4g2x] [0000-0001-8887-9141] (KTH [177], Skolan för kemi, bioteknologi och hälsa (CBH) [879224], Fiber- och polymerteknologi [879315], Ytbehandlingsteknik [879338]) (Experimental Cancer Medicine (ECM), Department of Laboratory Medicine, NOVUM, Karolinska Institutet (KI), Stockholm, Sweden; Department of Materials Science, Institute of Graduate Studies and Research, University of Alexandria, Alexandria, Egypt);Abd El-Fattah, Ahmed;Nafee, Noha;Zhao, Ying;Khalil, Labiba;Muhammed, Mamoun [u1m5r8td] (KTH [177], Skolan för informations- och kommunikationsteknik (ICT) [5994], Material- och nanofysik [13000], Funktionella material, FNM [13302]);Hassan, Moustapha;Kandil, Sherif"

  has_etal <- any(stringi::stri_detect_regex(diva_name, ";?\\s*et al\\.?,?"))
  if (has_etal) {
    diva_name <- stringi::stri_replace_all(diva_name,
      replacement = "", regex = ";?\\s*et al\\.?,?"
    )
  }

  dns <- strsplit(diva_name, ";")
  x <- unlist(dns)

  # http://www.drregex.com/2017/11/match-nested-brackets-with-regex-new.html
  # use clipr::read_clip() with below if not R v 4, else ...
  # r"(?=\()(?:(?=(?(1).*?(?=\1)).*?\((.*))(?=(?(2).*?(?=\2)).*?\)(.*)).)+?(?>.*?(?=\1))[^(]*?(?=\2$)"
  # r"(?=\()(?:(?=.*?\((?!.*?\1)(.*\)(?!.*\2).*))(?=.*?\)(?!.*?\2)(.*)).)+?.*?(?=\1)[^(]*(?=\2$)"
  # r"\((?:[^()]+|(?R))*+\)"

  re_parens <-
    paste0(
      "(?=\\()(?:(?=.*?\\((?!.*?\\1)(.*\\)(?!.*\\2).*))",
      "(?=.*?\\)(?!.*?\\2)(.*)).)+?.*?(?=\\1)[^(]*(?=\\2$)"
    )

  re_kthid <- "\\[(u1.{6})\\]"
  re_orcid <- sprintf("\\[(%s)\\]", paste0(collapse = "-", c(rep("\\d{4}", 3), "\\d{3}[1-9Xx]")))

  parse_re <- function(x, re, mode = c("regexec", "gregexpr")) {
    fn <- switch(match.arg(mode), regexec = regexec(re, x), gregexpr = gregexpr(re, x))
    res <- lapply(regmatches(x, fn), "[", -1)
    sapply(res, function(s) if (length(s) == 0) NA_character_ else paste(s, collapse = " "))
  }

  parse_re2 <- function(x, re) {
    res <- lapply(x, function(y) stringi::stri_match_all_regex(y, re)[[1]][, 2])
    vapply(res, function(s) ifelse(length(s) > 1, paste(s, collapse = " "), s), character(1))
  }

  res <- tibble(
    Name = x,
    name = trimws(parse_re(x, "([^(\\[|\\(]*)")),
    kthid = parse_re2(x, re_kthid),
    orcid = parse_re(x, re_orcid),
    orgids = parse_re2(x, "\\[(\\d+)\\]"),
    extorg = parse_re2(parse_re2(x, "(\\([^(\\[|\\(]*\\))$"), "\\((.*?)\\)"),
    has_etal = has_etal
  )
}
#' Parse all DiVA namestrings in DiVA publications
#'
#' This function parses all DiVA namestrings in a set of DiVA publications.
#'
#' @param pubs a data frame with diva publications, by default kth_diva_pubs()
#' @return data frame with results
#' @export
#' @importFrom progress progress_bar
#' @importFrom purrr map2_df
#' @import dplyr
parse_diva_names <- function(pubs = kth_diva_pubs()) {
  items <- pubs %>%
    filter(!is.na(PID) & !is.na(Name)) %>%
    select(PID, Name)

  pb <- progress::progress_bar$new(
    format = "parsing items [:bar] :percent eta: :eta",
    total = nrow(items)
  )

  parse_names <- function(x, y) {
    pb$tick()
    bind_cols(tibble(PID = x), parse_diva_name(y))
  }

  res <- purrr::map2_df(items$PID, items$Name, parse_names)

  # remap some values due to dirty input
  res %>%
    mutate(is_external = is.na(kthid) & is.na(orcid) & is.na(orgids)) %>%
    mutate(is_unmatched_extorg = is_external & (Name != name) & is.na(extorg)) %>%
    mutate(n_commas = nchar(gsub("[^,]", "", name))) %>%
    mutate(is_remappable_extorg = n_commas > 2 & is.na(extorg)) %>%
    mutate(extorg = ifelse(is_remappable_extorg, name, extorg)) %>%
    mutate(name = ifelse(is_remappable_extorg, NA_character_, name)) %>%
    mutate(extorg = gsub("\\.\\)$", "", extorg))

  # pubs may have a kthid, orcid, name or extorg
  # we group on combinations of those and assign unique PIDs
  pids <-
    res %>%
    inner_join(pubs, by = "PID") %>%
    group_by(kthid, orcid, name, extorg) %>%
    mutate(pids = paste0(collapse = " ", unique(PID))) %>%
    mutate(n_pid = lengths(strsplit(pids, " "))) %>%
    select(kthid, orcid, name, extorg, pids, orgids, n_pid) %>%
    distinct() %>%
    arrange(desc(n_pid))

  res %>% left_join(pids)
}

#library(dplyr)

#
# Asem, Heba [u1dc4g2x] [0000-0001-8887-9141]
# (
#   KTH [177],
#   Skolan för kemi, bioteknologi och hälsa (CBH) [879224],
#   Fiber- och polymerteknologi [879315],
#   Ytbehandlingsteknik [879338]
# )
# (
#   Experimental Cancer Medicine (ECM),
#   Department of Laboratory Medicine,
#   NOVUM,
#   Karolinska Institutet (KI),
#   Stockholm, Sweden
#   ;
#   Department of Materials Science,
#   Institute of Graduate Studies and Research,
#   University of Alexandria, Alexandria, Egypt
# )
# ;
# Abd El-Fattah, Ahmed
# ;
# Nafee, Noha
# ;
# Zhao, Ying
# ;
# Khalil, Labiba
# ;
# Muhammed, Mamoun [u1m5r8td]
# (
#   KTH [177],
#   Skolan för informations- och kommunikationsteknik (ICT) [5994],
#   Material- och nanofysik [13000],
#   Funktionella material,
#   FNM [13302]
# )
# ;
# Hassan, Moustapha
# ;
# Kandil, Sherif


#fullname
#brackets with orcid and/or kthid
#first parenthesis (with semicolon-separated affils)
#second parenthesis (with semicolon-separated affils)



extract_affiliations <- function(s) {

  #s <- "Markus Skyttner (blaha;ihi) (öåö;ööö); Linus (oj;oj2)(blähä;boho;aha)(boho)"
  s2 <- sandr(s)
  res <- tokenize_affs(s2)
  n_aff_total <- extract_aff_count(s2)

  if (is.na(n_aff_total))
    return(NULL)

  re <- paste0("AFF#", 1:n_aff_total)

  data <- Map(function(x) grep(x, res, value = T), re)
  namez <- gsub("\\s*[[].*[]]\\s*", "", data)
  namez <- gsub("\\s*\\(.*\\)\\s*", "", namez)

  affs <- extract_affs(s)

  extorg_idx <- sapply(unlist(lapply(unlist(strsplit(s2, ";")), extract_extorgs)), extract_aff_tokens)
  is_extorg <- rep(FALSE, length(affs))
  is_extorg[extorg_idx] <- TRUE

  tibble(
    aff_id = sprintf("{%s}", names(data)),
    fullname = trimws(namez),
    aff = affs,
    n_aff = lengths(strsplit(affs, split = ";")),
    divaorgs = extract_orgids(affs),
    is_extorg
  )
}

sandr <- function(x) {

  rs <- extract_affs(x)

  mysub <- function(p, t, i) {
    if (nchar(p) < 1) return (t)
    sub(p, sprintf("{AFF#%s}", i), t, fixed = TRUE)
  }

  res <- x
  i <- 0

  for (r in rs) {
    i <- i + 1
    res <- mysub(r, res, i)
  }

  res
}

parse_re3 <- function(x, re, sep = " ") {
  res <- lapply(x, function(y) stringi::stri_match_all_regex(y, re)[[1]][, 2])
  vapply(res, function(s) ifelse(length(s) > 1, paste(s, collapse = sep), s), character(1))
}

parse_dns <- function(x) {

  uses_etal <- has_etal(x)
  dns <- strsplit(sandr(replace_etal(x)), ";")
  x <- trimws(unlist(dns))

  tibble(
    fullname = extract_fullname(x),
    kthid = extract_kthid(x),
    orcid = extract_orcid(x),
    orgids = extract_orgids(x),
    extorg = extract_extorgs(x),
    uses_etal
  )

}

parse_diva_namestring <- function(s) {

  aff <- aff_one <- aff_two <- divaorgs <- is_extorg <- NULL

  uses_etal <- has_etal(s)
  dns <- strsplit(sandr(replace_etal(s)), ";")
  x <- trimws(unlist(dns))

  dns <- tibble(
    fullname = extract_fullname(x),
    kthid = extract_kthid(x),
    orcid = extract_orcid(x),
    orgids = extract_orgids(x),
    extorg = extract_extorgs(x),
    uses_etal
  )

  affs <- extract_affiliations(replace_etal(s))

  res <-
    dns %>%
    left_join(affs %>% filter(is_extorg == FALSE), by = "fullname") %>%
    mutate(aff_one = aff) %>%
    mutate(orgids = divaorgs) %>%
    select(one_of(names(dns)), aff_one)

  res %>%
    left_join(affs %>% filter(is_extorg == TRUE), by = "fullname") %>%
    mutate(aff_two = aff) %>%
    mutate(extorg = aff_two) %>%
    select(one_of(names(dns)))

}

