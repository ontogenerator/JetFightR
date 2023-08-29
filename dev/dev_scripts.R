library(devtools)
library(usethis)
library(tidyverse)
library(vroom)
library(magick)
library(colordistance)
library(furrr)


install()
load_all()

# usethis::use_description()
# usethis::use_package("colordistance")
# usethis::use_package("magick")

images <- list.files("C:/Users/vladi/Documents/imagetests/", full.names = TRUE)
images <- list.files("C:/Users/vladi/Documents/pagetests/", full.names = TRUE)

image <- images[16]

ref_cmap_folder <- "C:/Users/vladi/Documents/ref_cmaps/"
images <- list.files(ref_cmap_folder, full.names = TRUE)
threshold <- 0.8

plan(multisession)


res <- images |>
  future_map(\(x) has_cmap(x, ref_cmap_folder = ref_cmap_folder,
                    threshold = 1.2),
      .progress = TRUE)


results <- tibble(file = str_remove(images, ".*/"),
                   cmaps = res) |> 
  unnest(cmaps) |> 
  mutate(has_cmap = distance < 1.2)


results_summary <- results |>
  group_by(file) |> 
  summarise(has_rainbow = any(has_cmap),
            min_distance = min(distance))

image <- images[2]
image_read(image) |> class()


mg <- image_read(image)


tricky_page <- "C:/Users/vladi/Documents/pagetests/10.1101+2022.04.06.487394_p39.png"
tricky_page <- "C:/Users/vladi/Documents/pagetests/10.1101+2022.05.13.491770_p28.png"
tricky_page <- "C:/Users/vladi/Documents/pagetests/10.1101+2022.04.06.487394_p39.png"

# image

tricky_page |> 
  magick_preprocess()

tricky_page |> 
  magick_preprocess() |> 
  magick_to_col_2d(tricky_page) |> 
  getImageHist()

# image |> 
#   magick_preprocess() |> 
#   magick_to_col_2d(tricky_page) |> 
#   getImageHist()


ref_image <- 

others <- c(8, 8.2, 9) 


image |> loadImage(hsv = TRUE)
"#ffffffff"
"#162122ff"

