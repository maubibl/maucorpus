#' Assemble a header for loading node data into neo4j
#'
#' @param data a data frame to be exported
#' @param id_field the name of the primary key field
#' @param array_fields a vector of field names for fields that contain arrays
#' @param ignore_fields a vector of field names to exclude, Default: NULL
#' @param id_namespace an identifier namespace in neo4j parlance, Default: NULL
#' @param label_field the name of the field containing labels
#' @return a string with header info that can be used by neo4j imports
#' @examples
#' \dontrun{
#' if(interactive()){
#'  neo4j_header_nodes(kth_diva_pubs(), id_field = "PID", label_field = "Title")
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{select}},\code{\link[dplyr]{filter_all}},\code{\link[dplyr]{slice}}
#'  \code{\link[tidyselect]{all_of}}
#' @rdname neo4j_header_nodes
#' @export
#' @importFrom dplyr select filter_all slice
#' @importFrom tidyselect all_of
neo4j_header_nodes <- function(data, id_field, array_fields, ignore_fields = NULL,
                               id_namespace = NULL, label_field) {

  colnames <- names(data)

  allfields <- id_field

  if (!missing(array_fields))
    allfields <- c(allfields, array_fields)

  if (!missing(label_field))
    allfields <- c(allfields, label_field)

  stopifnot(allfields %in% colnames, length(id_field) == 1)

  if (!missing(array_fields)) {
    stopifnot(array_fields %in% colnames)
    colnames[which(colnames %in% array_fields)] <- paste0(array_fields, ":STRING[]")
    message("Verify that (multi)valued array fields have their values separated by ';':")
    data %>%
      dplyr::select(tidyselect::all_of(array_fields)) %>%
      dplyr::filter_all(function(x) !is.na(x)) %>%
      dplyr::slice(1:10) %>%
      print()
  }

  id_header <- function(field, ns) {
    if (!is.null(ns))
      return(sprintf("%s:ID(%s)", field, ns))
    sprintf("%s:ID", field)
  }

  colnames[which(colnames == id_field)] <- id_header(id_field, id_namespace)

  if (!is.null(ignore_fields) && ignore_fields %in% allfields) {
    warning("Mandatory fields should not be ignored...")
    ignore_fields <- setdiff(ignore_fields, allfields)
  }

  colnames[which(colnames %in% ignore_fields)] <- paste0(ignore_fields, ":IGNORE")

  if (!is.null(label_field))
    colnames[which(colnames %in% label_field)] <- paste0(label_field, ":LABEL")

  paste(collapse = ",", colnames)
}

#' Assemble a header for loading edge data into neo4j
#'
#' @param data a data frame to be exported
#' @param relation_type_field field name with values describing the relationship type
#' @param relation_src_field field name with id values for the node where the relation starts
#' @param relation_dst_field field name with id values for the node where the relation ends
#' @param ignore_fields logical to indicate which fields should be ignored (if any), Default: NULL
#' @param array_fields a vector of field names for fields that contain arrays
#' @return a string with header info that can be used by neo4j imports
#' @examples
#' \dontrun{
#' if(interactive()){
#'  neo4j_header_edges(data)
#'  }
#' }
#' @export
#' @importFrom dplyr select filter_all slice
#' @importFrom tidyselect all_of
neo4j_header_relations <- function(data, relation_type_field,
  relation_src_field, relation_dst_field,
  ignore_fields = NULL, array_fields) {

  colnames <- names(data)
  allfields <- c(relation_src_field, relation_dst_field, relation_type_field)

  if (!missing(array_fields)) {
    stopifnot(array_fields %in% colnames)
    colnames[which(colnames %in% array_fields)] <- paste0(array_fields, ":STRING[]")
    message("Verify that (multi)valued array fields have their values separated by ';':")
    data %>%
      dplyr::select(tidyselect::all_of(array_fields)) %>%
      dplyr::filter_all(function(x) !is.na(x)) %>%
      dplyr::slice(1:10) %>%
      print()
  }

  stopifnot(allfields %in% colnames, length(unique(allfields)) == 3)

  if (!is.null(ignore_fields) && ignore_fields %in% allfields) {
    warning("Mandatory fields should not be ignored...")
    ignore_fields <- setdiff(ignore_fields, allfields)
  }

  colnames[which(colnames %in% relation_type_field)] <- paste0(relation_type_field, ":TYPE")
  colnames[which(colnames %in% relation_src_field)] <- paste0(relation_src_field, ":START_ID")
  colnames[which(colnames %in% relation_dst_field)] <- paste0(relation_dst_field, ":END_ID")
  colnames[which(colnames %in% ignore_fields)] <- paste0(ignore_fields, ":IGNORE")

  paste(collapse = ",", colnames)
}



# There are three mandatory fields for relationship data:
# :START_ID — ID refering to a node.
# :END_ID — ID refering to a node.
# :TYPE — The relationship type.
# In order to create a relationship between two nodes, the IDs defined in actors.csv and movies.csv are used for the :START_ID and :END_ID fields. You also need to provide a relationship type (in this case ACTED_IN) for the :TYPE field.


# TODO: load relationships



# TODO later


# coltypes <-
#   data %>% slice(1) %>% mutate_all(typeof) %>%
#   t() %>% as.data.frame() %>% .$V1 %>% unname() %>% as.character()
#
# unique(coltypes)



#https://neo4j.com/docs/operations-manual/current/tutorial/neo4j-admin-import/
#https://neo4j.com/developer/guide-import-csv/
#https://neo4j.com/docs/operations-manual/current/tools/neo4j-admin-import/#import-tool-header-format-nodes
# In neo4j imports, use one of
# int, long, float, double, boolean, byte, short, char, string,
# point, date, localtime, time, localdatetime, datetime, and duration

# If no data type is given, this defaults to string.
# To define an array type, append [] to the type.

# if character, do nothing
# if boolean, convert to "true" and mark as :boolean
# if double, mark as :double
# if "array", mark as :[]

# These are some things you will need to keep in mind when creating your input files:
#
#   Fields are comma-separated by default but a different delimiter can be specified.
#
# All files must use the same delimiter.
#
# Multiple data sources can be used for both nodes and relationships.
#
# A data source can optionally be provided using multiple files.
#
# A separate file with a header that provides information on the data fields, must be the first specified file of each data source.
#
# Fields without corresponding information in the header will not be read.
#
# UTF-8 encoding is used.
#
# By default, the importer will trim extra whitespace at the beginning and end of strings. Quote your data to preserve leading and trailing whitespaces.

neo4j_bulk_extract <- function(outdir) {

  if (missing(outdir))
    outdir <- "~/repos/neo4kth/import"

  stopifnot(dir.exists(outdir))

  pubs <-
    kth_diva_pubs() %>%
#    select("Contributor") %>%
#    filter(!is.na(Contributor))
    select(-c("Abstract")) %>%
    mutate(label = "Publication")

  header <- neo4j_header_nodes(pubs,
    id_field = "PID",
    label_field = "label",
    #id_namespace = "Publication",
    array_fields = c("Keywords", "Name", "Contributor")
    )

  message("neo4j header: ", header)

  outheader <- file.path(outdir, "pubs-header.csv")
  write_lines(header, outheader)

  outfile <- file.path(outdir, "pubs.csv")
  write_csv(pubs, outfile, col_names = FALSE, na = "")

  #outfiles <- paste(sep = ",", outheader, outfile)
  #relpath <- gsub(paste0(outdir, "/"), "", outfiles)
  #neo4j_load_command <- sprintf("neo4j-admin import --nodes=%s", relpath)
  #message(neo4j_load_command)

  authors <-
    kth_diva_authors() %>%
    #filter(!is.na(kthid)) %>%
    mutate(mid = paste0("m", 1:nrow(.))) %>%
    select("mid", PID, kthid, name, orcid, orgids, extorg, pids, n_pid) %>%
    #mutate(aid = paste0("a", 1:nrow(kth_diva_authors()))) %>%
    mutate(label = "Author") %>%
    mutate(pids = gsub("\\s+", ";", pids))

  header <-
    neo4j_header_nodes(authors,
       id_field = "mid",
       label_field = "label",
       array_fields = "pids"
    )

  outheader <- file.path(outdir, "authors-header.csv")
  write_lines(header, outheader)

  outfile <- file.path(outdir, "authors.csv")
  write_csv(authors, outfile, col_names = FALSE, na = "")

  # relations

  rels <-
    authors %>%
    select(PID, mid, name, kthid, orcid, orgids, extorg, pids, n_pid) %>%
    mutate(reltype = "AUTHOR") %>%
    mutate(pids = gsub(" ", ";", pids))

  header <-
    neo4j_header_relations(
      rels, relation_dst_field = "mid", relation_src_field = "PID", relation_type_field = "reltype",
      ignore_fields = "extorg", array_fields = "pids"
    )

  outheader <- file.path(outdir, "rels-header.csv")
  write_lines(header, outheader)

  outfile <- file.path(outdir, "rels.csv")
  write_csv(rels, outfile, col_names = FALSE, na = "")

  message("Done. Files are in ", file.path(outdir))
}

# MATCH (p:pubs)-[r]->(a:authors {PID: "1500455"}) RETURN r,p,a limit 10
# MATCH (p:pubs)-[r]->(a:authors {kthid: "u18prnvn"}) RETURN p, r, a limit 20
# https://neo4j.com/docs/operations-manual/current/tutorial/neo4j-admin-import/
# https://neo4j.com/download-center/#desktop


# glimpse(pubs %>% slice(1))
#
# # TODO use --delimiter='\t' which does double-escape quotes with "
# # fix html codes etc preventing import in neo4j
# cp pubs.csv import/
# grep -n "PURR~" pubs.csv | cut -d : -f 1
# sed -i '30024d' pubs.csv
# grep -n "Old Liding" pubs.csv | cut -d : -f 1
# sed -i '4356d;33659d;33660d;38231d' pubs.csv
# cp pubs.csv import
#
# # cypher
# LOAD CSV WITH HEADERS FROM 'file:///pubs.csv' as row
# MERGE (p:Publication {ID: row.PID:ID})
#
# LOAD CSV WITH HEADERS FROM 'file:///pubs.csv' as row
# RETURN row LIMIT 1
#
# // clear data
# MATCH (n)
# DETACH DELETE n;
# // load Employee nodes
# LOAD CSV WITH HEADERS FROM 'file:///people.csv' AS row
# MERGE (e:Employee {employeeId: row.employeeId, name: row.Name})
# RETURN count(e);
# // load Company nodes
# LOAD CSV WITH HEADERS FROM 'file:///companies.csv' AS row
# WITH row WHERE row.Id IS NOT NULL
# WITH row,
# (CASE row.BusinessType
#  WHEN 'P' THEN 'Public'
#  WHEN 'R' THEN 'Private'
#  WHEN 'G' THEN 'Government'
#  ELSE 'Other' END) AS type
# MERGE (c:Company {companyId: row.Id, hqLocation: coalesce(row.Location, "Unknown")})
# SET c.emailAddress = CASE trim(row.Email) WHEN "" THEN null ELSE row.Email END
# SET c.businessType = type
# RETURN count(c);
# // create relationships
# LOAD CSV WITH HEADERS FROM 'file:///people.csv' AS row
# MATCH (e:Employee {employeeId: row.employeeId})
# MATCH (c:Company {companyId: row.Company})
# MERGE (e)-[:WORKS_FOR]->(c)
# RETURN *;
