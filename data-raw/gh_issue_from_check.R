library(purrr)
library(glue)

mkthid <-
  check_missing_kthid() %>%
  filter(name == "Yu, Ze")
  arrange(desc(n_pid)) %>%
  group_by(name) %>%
  summarise(pidz = paste0(collapse=" ", PID)) %>%
  arrange(desc(nchar(pidz)))

suggest_kthid <- function(author, pids) {
  kth_diva_authors() %>% filter(name == author) %>%
    count(kthid, orcid, orgids) %>%
    arrange(kthid, orcid, desc(nchar(orgids)), desc(n)) %>%
  bind_cols(name = author, pids = pids, .) %>%
  head(1)
}

link_md <- function(pid) {
  glue("[{pid}](https://kth.diva-portal.org/smash/record.jsf?pid=diva2:{pid})")
}

format_pids <- function(pids) {
  links <- unlist(strsplit(pids, " "))
  linkz <- link_md(links)
  paste0(collapse = "\n", sprintf("- [ ] %s", linkz))
}

# needs a progress bar with eta
ghissues <-
  mkthid %>%
  select(author = name, pids = pidz) %>%
  pmap_dfr(suggest_kthid) %>%
  filter(!is.na(kthid) | !is.na(orcid)) %>%
  rowwise() %>%
  mutate(issue = glue("gh issue create --label 'enhancement' \\
--title 'Author `{name}` has no kthid' \\
--body '`{name}` - should it be kthid `{kthid}` and/or orcid `{orcid}` affiliated with {orgids}? - appears on PIDs
{format_pids(pids)}
'"))

# after 150 issues, a "GraphQL error: was submitted too quickly" may be rate limiting upload of issues
# see potential rate limit issue: https://github.com/cli/cli/issues/4801
# advise is to refactor to make 1h pause after 150 issues

script <- glue("#!/bin/bash
{paste0(collapse='\nsleep 3\n', ghissues$issue)}
")

write_lines(script, file = "~/repos/curation/create_issues.sh")
