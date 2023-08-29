#' Convert a magick image object to a 2d rgb matrix.
#' 
#' The function allows for the image processing functions in
#' the magick package to be applied and the results passed
#' on to the colordistance package functions for color
#' histogram extraction and comparison.
#' 
#' @param magick_img Image object from the magick package.
#' @param path Path to the original image file. Necessary for
#' downstream colordistance functions.
#' @param threshold Threshold value for white removal. Color
#' pixels with values higher than the threshold on all three
#' channels (r, g, b) will be removed.
#' @param original_image Currently unused, but saved for
#' compatibility with the colordistance formatting.
#' 
#' @return A matrix with r, g, b columns, values between 0 and 1
#' for each color channel. Transparent and white pixels
#' (see threhsold above) are removed.
#' 
#' @example
#' \dontrun{
#' has_cmap(image, path = "image.png", threshold = 0.8)
#' } 
#' 
#' @export
has_cmap <- function(image, ref_cmap_folder, lower = rep(0.8, 3), upper = rep(1, 3),
                     method = c("chisq", "weighted.pairs", "emd"),
                     threshold = 0.5,
                     ordering = FALSE, size.weight = 0.7, color.weight = 0.3) {
  refs <- dir(ref_cmap_folder, full.names = TRUE)
  
  method <- match.arg(method)
  
  processed_image <- magick_preprocess(image) |> 
    magick_to_col_2d(image)
    
  # save as object to speed up?
  ref_clusters <- colordistance::getHistList(refs, 
                                             hsv = TRUE) 
  
  image_cluster <- colordistance::getImageHist(processed_image,
                                               hsv = TRUE,
                                               plotting =  FALSE)
  
  distances <- get_wp_distance_from_refs(image_cluster, ref_clusters,
                                         size.weight = size.weight,
                                         color.weight = color.weight) |> 
    tibble::enframe(name = "ref", value = "distance") |> 
    dplyr::mutate(distance = unlist(distance),
                  has_cmap = distance < threshold)
  # CDM <- colordistance::getColorDistanceMatrix(all_clusters, method = method,
  #                                              ordering = ordering, size.weight = size.weight,
  #                                              color.weight = color.weight)[1,]
  # colordistance::getColorDistanceMatrix(clusters, method = method)[1,]
  # dplyr::tibble(distance = CDM[-1], ref = names(CDM[-1])) |> 
  #   dplyr::mutate(has_cmap = distance < threshold)
  # results
  
}


#' Get weighted pairs distances for a target color cluster
#'  compared to color clusters of reference images.
#' 
#' Color clusters obtained with the `getImageHist` function
#' are used as input to the `weightedPairsDistance` function,
#' both from the `colordistance` package.  
#' 
#' @param image_cluster Color cluster for the target image.
#' @param ref_clusters Color clusters for the reference images.
#' @param size.weight Relative weight of the size of the clusters.
#' @param color.weight Relative weight of the color of the clusters.
#' 
#' @return A named list with the distance values for each
#' comparison between the target and one of the reference
#' images. The names are the names of the reference images.
#' 
#' @example
#' \dontrun{
#' get_wp_distance_from_refs(image_cluster, ref_clusters,
#' size.weight = 0.7, color.weight = 0.3)
#' } 
#' 
#' @export
get_wp_distance_from_refs <- function(image_cluster, ref_clusters,
                                      # method = c("weighted.pairs", "emd"),
                                      size.weight = 0.5, color.weight = 0.5) {
  
  # method <- match.arg(method)
  
  ref_clusters |> 
    purrr::map(\(ref_cluster) 
               colordistance:::weightedPairsDistance(image_cluster,
                                                     ref_cluster,
                                                     ordering = FALSE,
                                                     size.weight = size.weight,
                                                     color.weight = color.weight))
  
}


#' Convert a `magick` image object to a 2d rgb array.
#' 
#' Allows for the image processing functions in
#' the `magick` package to be applied and the results passed
#' on to the `colordistance` package functions for color
#' histogram extraction and comparison.
#' 
#' @param magick_img Image object from the `magick` package.
#' @param path Path to the original image file. Necessary for
#' downstream `colordistance` functions.
#' @param threshold Threshold value for white removal. Color
#' pixels with values higher than the threshold on all three
#' channels (r, g, b) will be removed.
#' @param original_image Currently unused, but saved for
#' compatibility with the `colordistance` formatting.
#' 
#' @return A matrix with r, g, b columns, values between 0 and 1
#' for each color channel. Transparent and white pixels
#' (see threhsold above) are removed.
#' 
#' @example
#' \dontrun{
#' magick_to_col_2d(image, path = "image.png", threshold = 0.8)
#' } 
#' 
#' @export
magick_to_col_2d <- function(magick_img, path, threshold = 0.7, original_image = NULL, hsv = TRUE) {
 # magick_img <- image_read(image) 
  col_names <- c("x", "y", "col", "r", "g", "b")
  transparence_removed <- magick::image_raster(magick_img) |>
    # rgb_m <- pixels |> 
    dplyr::filter(col != "transparent")
  
  cm <- transparence_removed |> 
    dplyr::mutate(rgb = purrr::map(col, \(x) grDevices::col2rgb(x)),
                  hsv = purrr::map(col, \(x) DescTools::ColToHsv(x))) |> 
    tidyr::unnest_wider(rgb) |> 
    tidyr::unnest_wider(hsv, names_repair = \(x) stringr::str_sub(x, 1, 1)) |> 
    dplyr::mutate(dplyr::across(c("r", "g", "b"), .fn = \(x) as.vector(x) / 255),
                  dplyr::across(c("h", "s", "v"), as.vector)) |> 
    dplyr::mutate(is_desaturated = s < 0.5,
                  is_dark = v < 0.2, 
      # is_white = r >= threshold & g >= threshold & b >= threshold
      ) |> 
    dplyr::filter(is_desaturated == FALSE,
                  is_dark == FALSE) 
  
  rgb_m <- cm |> 
    dplyr::select(r, g, b) |> 
    as.matrix()
  hsv_m <- cm |> 
    dplyr::select(h, s, v) |> 
    as.matrix()
  
  list(path = path, original.rgb = original_image, filtered.rgb.2d = rgb_m,
       filtered.hsv.2d = hsv_m)
}

#' Process an image file with `magick` functions then convert to a 2d rgb array.
#' 
#' Preprocessing involves rescaling to 400 pixels in width,
#' making white, black, and gray pixels transparent,
#' despeckling and finally converting to a 2d rgb array in the format
#' of `colordistance` package for color histogram extraction and comparison.
#' 
#' @param image_file Path to the original image file in png or jpg format.
#' 
#' @return A `magick` image object. Transparent and white pixels
#' (see threhsold above) are removed.
#' 
#' @example
#' \dontrun{
#' magick_preprocess(image)
#' } 
#' 
#' @export
magick_preprocess <- function(image_file) {
  image_file |> 
    magick::image_read() |> 
    magick::image_scale("400x") |> # rescale to 400 pix width
    magick::image_transparent("white", fuzz = 25) |> # make white transparent
    magick::image_transparent("black", fuzz = 15) |> # make black transparent
    magick::image_transparent("gray", fuzz = 10) # make gray transparent
    # magick::image_despeckle(times = 5) # smooth stray pixels
}