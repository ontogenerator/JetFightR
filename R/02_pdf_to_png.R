library(tidyverse)
library(magick)
library(furrr)

pdf_folder <- "C:/Users/Vladi/Documents/imagePDFs2/"
img_folder <- "C:/Users/Vladi/Documents/pages2/"


extract_page_as_png <- function(pdf_file, page, density = 300, dest_folder) {
  file_name <- stringr::str_remove(pdf_file, ".*/") |> 
    stringr::str_remove(".pdf")
  magick::image_read_pdf(pdf_file, pages = page, density = density) |> 
    magick::image_write(path = paste0(dest_folder, "/", file_name, "_p",
                                      page, ".png"))
}

# pdf_file <- paste0(pdf_folder, pdf_file[1])
# dest_folder <- img_folder
extract_pages_as_png <- function(pdf_file, pages = NULL, density = 300, dest_folder) {
  
  extracted <- list.files(dest_folder) |> tolower()
  
  if (any(stringr::str_detect(extracted, stringr::str_extract(pdf_file, ("(?<=\\+).*(?=\\.pdf)"))))) {
    print("PDF file already extracted.")
    return()
  }
  
  if (is.null(pages)) {
    pages <- magick::image_read_pdf(pdf_file, pages = NULL, density = 2) |> 
      magick::image_info() |>
      nrow()
    pages <- 1:pages
  }
  
  
  
  purrr::walk(pages, \(page) extract_page_as_png(pdf_file,
                                                 page = page,
                                                 density = density,
                                                 dest_folder = dest_folder))
}

pdf_files <- list.files(pdf_folder, full.names = TRUE)

plan(multisession)

pdf_files |>  
  future_walk(\(pdf_file) extract_pages_as_png(pdf_file = pdf_file, dest_folder = img_folder),
       .progress = TRUE)



pdf_files <- list.files(img_folder, full.names = TRUE)

length(pdf_files) %% 200

ceiling(5001/files_per_folder)
25 * 200

folder <- img_folder
start_at <- 34
# almost, but also moved dirlast to dir2 for some reason?
split_into_subfolders <- function(folder, files_per_folder = 200, start_at = 1) {
  files_to_move <- list.files(folder, full.names = TRUE)
  old_names <- list.files(folder)
  #create dirs
  number_of_folders <- ceiling(length(files_to_move)/files_per_folder)
  vec_folders <- paste0(folder, "dir", start_at:(number_of_folders + start_at - 1))
  
  #create new folders
  vec_folders |> 
    purrr::walk(dir.create)
  
  old_path <- stringr::str_remove(files_to_move[1], stringr::fixed(old_names[1]))
  # new filenames
  new_names <- paste0(rep(vec_folders, each = files_per_folder), "/", old_names)
  new_names <- new_names[1:length(files_to_move)]
  file.rename(from = files_to_move, new_names)
  
}

split_into_subfolders(img_folder, start_at = 34)


tt <- tibble(new_names)
