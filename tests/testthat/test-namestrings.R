test_that("parsing a namestring with two external affiliations works", {

  diva_name <- paste0(
    "Asem, Heba [u1dc4g2x] [0000-0001-8887-9141] ",
    "(KTH [177], Skolan för kemi, bioteknologi och hälsa (CBH) [879224], Fiber- och polymerteknologi [879315], Ytbehandlingsteknik [879338]) ",
    "(Experimental Cancer Medicine (ECM), Department of Laboratory Medicine, NOVUM, Karolinska Institutet (KI), Stockholm, Sweden; ",
    "Department of Materials Science, Institute of Graduate Studies and Research, University of Alexandria, Alexandria, Egypt)",
    ";Abd El-Fattah, Ahmed;Nafee, Noha;Zhao, Ying;Khalil, Labiba;",
    "Muhammed, Mamoun [u1m5r8td] ",
    "(KTH [177], Skolan för informations- och kommunikationsteknik (ICT) [5994], Material- och nanofysik [13000], Funktionella material, FNM [13302])",
    ";Hassan, Moustapha;Kandil, Sherif"
  )

  data <- parse_diva_namestring(diva_name)

  data$orgids
  is_ok <- with(subset(data, name == "Asem, Heba"),
       kthid == "u1dc4g2x" &&
       orgids == "177 879224 879315 879338 5994 13000 13302" &&
       orcid == "0000-0001-8887-9141"
  )

  is_valid <- nrow(data) == 8 && is_ok
  expect_true(is_valid)
})

test_that("parsing a namestring with some quirks works", {

  a <- "Osqulda af der KTH [u1234567][0000-0001-8887-9141] (KTH [177])(blaha (CBH);boho); Oscar von KTH [u1234567] (KTH [177] well)(woho; baha) et al"

  # replace_etal(a)
  # extract_kthid(a)
  # extract_orcid(a)
  # extract_orgids(a)
  # extract_extorgs(a)
  # extract_fullname(a)
  #
  # extract_aff_tokens(sandr(a))
  # extract_aff_count(sandr(a))
  # extract_affiliations(a)
  #
  # has_etal(a)
  # parse_dns(a)
  data <- parse_diva_namestring(a)

  is_ok <- with(subset(data, name == "Osqulda af der KTH"),
    kthid == "u1234567" &&
    orgids == "177" &&
    orcid == "0000-0001-8887-9141"
  )

  is_valid <- nrow(data) == 2 && is_ok
  expect_true(is_valid)

})

test_that("parsing another real namestring works", {

  x <- paste0(
    "Jia, Shi;Pang, Xiaodan [u1e3qilb] (KTH [177], ",
    "Skolan för informations- och kommunikationsteknik (ICT) [5994]) ",
    "(RISE Acreo AB, NETLAB, SE-16425 Kista, Sweden)",
    ";Ozolins, Oskars;Yu, Xianbin;Hu, Hao;Yu, Jinlong;Guan, Pengyu;",
    "Da Ros, Francesco;",
    "Popov, Sergei [u1fs8je2] ",
    "(KTH [177], Skolan för informations- och kommunikationsteknik (ICT) [5994])",
    ";Jacobsen, Gunnar;Galili, Michael;Morioka, Toshio;Zibar, Darko;Oxenlowe, Leif K.")

  data <- parse_diva_namestring(x)

  is_ok <- with(subset(data, name == "Popov, Sergei"),
    kthid == "u1fs8je2" &&
    orgids == "177 5994"
  )

  is_valid <- nrow(data) == 14 && is_ok
  expect_true(is_valid)

})

# TODO: add tests where there can be at least THREE parens
# and where the first TWO can be KTH affiliations for example

#  kth_diva_pubs() %>%
#  filter(PID==1351863) %>% # mixed affs
#  filter(PID==1513081) %>% # three parens, kth affs only
#  filter(PID==1121547) %>% # three parens, mixed affs
#  pull(Name) %>%
#  extract_fullname()
#  extract_affiliations()
#  parse_diva_namestring()

# TODO add tests for LONG namestrings

# b <- kth_diva_pubs() %>% mutate(n = nchar(Name)) %>% arrange(desc(n)) %>% head(100) %>% pull(Name)
# parse_diva_names(b[1]) %>% arrange(desc(kthid))

# TODO: add timings

# tictoc::tic()
# parse_diva_name(x)
# tictoc::toc()
#
# tictoc::tic()
# parse_diva_namestring(x)
# tictoc::toc()
