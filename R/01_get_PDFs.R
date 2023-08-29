library(europepmc)
library(rentrez)
library(tidyverse)
library(pdfRetrieve)
library(openalexR)
#heatmap[Figure/Table Caption] 

#(((((fret[Figure/Table Caption]) OR barcoding[Figure/Table Caption])) OR ((EMS[Figure/Table Caption]) OR electromicrograph[Figure/Table Caption])) OR heatmap[Figure/Table Caption]) OR flow cytometry[Figure/Table Caption] 
my.data <- epmc_search(query = 'heatmap[capt]', limit = 250)

compound_query <- "((((((flow cytometry[CAPT]) OR fret[CAPT]) OR barcoding[CAPT]) OR ems[CAPT]) OR electromicrograph[CAPT]) OR heatmap[CAPT]) OR lasagna[CAPT]"
compound_query <- "(flow cytometry[CAPT]) OR fret[CAPT]"

terms <- c("fMRI",
           "doppler",
           "PET",
           "PET/CT",
           "CT/PET",
           "echocardiography",
           "echocardiogram",
           "false color",
           "color mammogram",
           "micro-CT",
           "cage monitoring system")

make_capt_query <- function(output, input) {
  make_simple_query <- function(term) {
    if (!stringr::str_detect(term, "\\[")) {
      return(paste0(term, "[CAPT]")) 
    } else {
        return(term)
      }
  }
  paste0("(", make_simple_query(output), ") OR ", make_simple_query(input))
  
  
    
}

compound_query <- purrr::reduce(terms, make_capt_query)

compound_query <- "((((((fMRI[CAPT]) OR doppler[CAPT]) OR PET[CAPT]) OR CT/PET[CAPT]) OR false color[CAPT]) OR echocardiography[CAPT]) OR echocardiogram[CAPT]"


my.data <- epmc_search(query = compound_query, limit = 300)

res <- entrez_search(db = "pmc", term = "heatmap[CAPT]", retmax = 500, use_history = TRUE)

# res <- entrez_search(db = "pmc", term = compound_query, retmax = 350, use_history = TRUE)
res <- entrez_search(db = "pmc", term = "false color[CAPT]", retmax = 50, use_history = TRUE)

results <- entrez_summary(db = "pmc", id = res$ids, api_key = Sys.getenv("ENTREZ_KEY"))


results[[170]]$articleids
record <- results[[170]]
get_id <- function(record, id_type = c("pmcid", "doi")) {
  # id_type <- "doi"
    
  id <- record |>
    purrr::pluck("articleids") |> 
    dplyr::filter(idtype == id_type) |> 
    dplyr::pull(value)
  
  if (length(id) == 0) return(NA)
    id
  }

pmcids <- results |>
  map_chr(get_id, "pmcid") |> 
  unname()


dois <- results |> 
  map_chr(get_id, "doi") |> 
  unname()
  
dois <- dois[!is.na(dois)]




filter_downloaded <- function(dois, download_folder) {
  downloaded_dois <- list.files(download_folder) |> 
    tolower() |> 
    stringr::str_remove(".pdf") |> 
    stringr::str_replace_all("\\+", "/")
  
  dois[!tolower(dois) %in% downloaded_dois]
  
}

pdf_folder <- "C:/Users/Vladi/Documents/imagePDFs2/"

dois <- dois |> 
  filter_downloaded(pdf_folder)

email <- Sys.getenv("EMAIL")
pdf_retrieve(dois, email = email, save_folder = pdf_folder, overwrite_files = FALSE, sleep = 1,
             # only_PMC = TRUE,
                         repository_pdf = TRUE, check_website = TRUE,
                         check_crossref = TRUE,
                         use_fulltext = TRUE)


compound_query <- '((OPEN_ACCESS:y) AND (PUB_TYPE:"Preprint" OR PUB_TYPE:"Journal Article" OR PUB_TYPE:"article-commentary" OR PUB_TYPE:"research-article" OR PUB_TYPE:"protocol" OR PUB_TYPE:"rapid-communication" OR PUB_TYPE:"product-review") AND (LANG:"eng" OR LANG:"en" OR LANG:"us") AND (FIRST_PDATE:[2020-01-01 TO 2026-12-31])) AND (SRC:PPR OR ((SRC:MED OR SRC:PMC OR SRC:AGR OR SRC:CBA) NOT (PUB_TYPE:"Review")))'
my.data <- epmc_search(query = compound_query, limit = 100000)


epmc_search
my.data_filtered <- my.data |> 
  filter(!is.na(doi), hasPDF == "Y", !str_detect(pubType, "comment|preprint")) |> 
  mutate(doi_prefix = str_extract(doi, ".*(?=/)")) |> 
  group_by(doi_prefix) |>
  slice_sample(n = 100) |> 
  group_by(journalTitle) |> 
  slice_sample(n = 25) |> 
  ungroup()

my.data_filtered |> 
  count(journalTitle, sort = TRUE)

my.data_filtered |> 
  count(doi_prefix, sort = TRUE)



# ca 100 preprints

my.data_preprints <- my.data |> 
  filter(!is.na(doi), str_detect(pubType, "preprint")) |> 
  mutate(doi_prefix = str_remove(doi, "/.*")) |> 
  group_by(doi_prefix) |>
  slice_sample(n = 25) |>  
  ungroup()


my.data_subset <- my.data_filtered |> 
  bind_rows(my.data_preprints)

getwd()
write_excel_csv2(my.data_subset, "C:/Users/Vladi/Documents/dois_credit.csv")


pdf_folder <- "C:/Users/Vladi/Documents/PDFs_credit/"
email <- Sys.getenv("EMAIL")

dois <- my.data_subset |> 
  pull(doi) |> 
  filter_downloaded(pdf_folder)

missing_papers <- my.data_subset |> 
  filter(doi %in% dois)

pdf_retrieve(dois, email = email, save_folder = pdf_folder, overwrite_files = FALSE, sleep = 1,
             # only_PMC = TRUE,
             repository_pdf = TRUE, check_website = TRUE,
             check_crossref = TRUE,
             use_fulltext = TRUE)


concepts <- c("https://openalex.org/C41008148", #computer science
              "https://openalex.org/C121332964", #physics
              "https://openalex.org/C17744445", #pol sci
              "https://openalex.org/C15744967", #psychol
              "https://openalex.org/C33923547", #math
              "https://openalex.org/C144024400", #sociol
              "https://openalex.org/C162324750", #econ
              "https://openalex.org/C127313418", #geol
              "https://openalex.org/C95457728")  #history


query_credit <- oa_query(
  
  entity = "works",
  concept.id = concepts,
  is_oa = "true",
  type = "article",
  type_crossref = "journal-article",
  primary_location.source.type = "journal",
  primary_location.version = "!submittedVersion",
  from_publication_date = "2023-08-01"
  
)

res <- oa_request(query_url = query_credit, mailto = Sys.getenv("EMAIL"), verbose = TRUE)

df_credit <- oa2df(res, entity = "works")

# 
# df_credit <- df_credit |> 
#   filter(is_oa == TRUE,
#          version == "publishedVersion")


df_credit |> 
  count(version)

set.seed(42)


concept_df <- df_credit$concepts[[1]]

concept_df |> 
  filter(level == 0) |> 
  slice_max(score, n = 1)


df_oa_fields <- df_credit |> 
  filter(!is.na(doi),
         is_oa == TRUE,
         oa_status != "closed",
         language == "en",
         version == "publishedVersion",
         !is.na(pdf_url),
         !is.na(referenced_works),
         !is.na(author),
         !is.na(ab),
         !str_detect(doi, "abs"),
         ab != "") |>
  mutate(authors = map_chr(author, \(au) pull(au, au_display_name) |> paste0(collapse = "; "))) |>
  filter(str_detect(authors, ";")) |> 
  rename(oa_id = id, title = display_name) |> 
  unnest(concepts) |>
  filter(level == 0) |> 
  group_by(doi) |> 
  slice_max(score, n = 1) |> 
  mutate(doi_prefix = str_extract(doi, ".*(?=/)")) |> 
  ungroup()


df_oa_fields |> 
  count(display_name, sort = TRUE)



df_oa_subfields <- df_oa_fields |>
  filter(!display_name %in% c("Medicine", "Chemistry", "Biology"))
  
df_oa_subfields |> 
  count(display_name, sort = TRUE)

set.seed(42)

df_oa_samples <- df_oa_subfields |> 
  group_by(display_name) |>
  slice_sample(n = 250) |>
  group_by(host_organization) |>
  slice_sample(n = 50) |> 
  group_by(so) |> 
  slice_sample(n = 25) |> 
  ungroup()

df_oa_samples |> 
  count(so, sort = TRUE)



pdf_folder <- "C:/Users/Vladi/Documents/PDFs_credit/"
email <- Sys.getenv("EMAIL")

dois <- df_oa_samples |> 
  pull(doi) |> 
  filter_downloaded(pdf_folder)

missing_papers <- df_oa_samples |> 
  filter(doi %in% dois)

set.seed(42)
pdf_retrieve(dois, email = email, save_folder = pdf_folder, overwrite_files = FALSE, sleep = 1,
             # only_PMC = TRUE,
             repository_pdf = TRUE, check_website = TRUE,
             check_crossref = TRUE,
             use_fulltext = TRUE)

df_oa_fields
