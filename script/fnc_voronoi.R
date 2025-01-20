# Feeder main
# Rodrigo Dal Ben
# 2025-01-13
# https://roddalben.github.io/

# this piece of generative art is built from a "Lateral X-ray of the neck with cervical collar in a patient with Cervical Spine Trauma"
# by © Nevit Dilmen
# attribution: "This file is licensed under the Creative Commons Attribution-Share Alike 3.0 Unported license. Attribution: © Nevit Dilmen"
# source: https://commons.wikimedia.org/wiki/File:Medical_X-Ray_imaging_RAH06_nevit.jpg

# preprocessing done outside of R
# 1. cut the original image into a square (see "input/01_step1.jpg")
# 2. remove the background from the square image using the "www.remove.bg" website (see "input/02_step2.png")
# 3. transform from .png to .jpg with Preview to replace transparency layer with solid black background (see "input/03_step3.jpg")

fnc_voronoi <- function(title_no_spaces = NA,
                        sample = 500,
                        input_file = "03_step3.jpg",
                        input_path = here("input"),
                        output_path = here("output"),
                        qr_code_signature = NA){
  
  # check required info 
  if(is.na(title_no_spaces)){stop("Please provide title with no spaces")}
  if(is.na(qr_code_signature)){stop("Please provide information for qr_code_info")}
  
  # if necessary, install packages
  if(!require(tidyverse)){install.packages("tidyverse")}
  if(!require(here)){install.packages("here")}
  if(!require(qrcode)){install.packages("qrcode")}
  if(!require(imager)){install.packages("imager")}
  if(!require(magick)){install.packages("magick")}
  if(!require(ggvoronoi)){remotes::install_github("garretrc/ggvoronoi", dependencies = TRUE, build_opts = c("--no-resave-data"))}
  if(!require(tidyverse)){devtools::install_github("doehm/cropcircles")}
  
  # if necessary, load packages
  if(!"tidyverse" %in% .packages()){library(tidyverse)}
  if(!"here" %in% .packages()){library(here)}
  if(!"qrcode" %in% .packages()){library(qrcode)}
  if(!"imager" %in% .packages()){library(imager)}
  if(!"ggvoronoi" %in% .packages()){library(ggvoronoi)}
  if(!"cropcircles" %in% .packages()){library(cropcircles)}
  if(!"magick" %in% .packages()){library(magick)}
  
  # generate qrcode for signature   
  create_qr_code <- qrcode::generate_svg(qrcode::qr_code(qr_code_signature),
                                         filename = file.path(input_path, "qr_code.svg"),
                                         size = 150,
                                         show = F)
  
  img_qr_code <- magick::image_read(file.path(input_path, "qr_code.svg"))
  
  # read input image
  img <- imager::load.image(file.path(input_path, input_file))
  
  # transform image as data frame and each row as a point
  img_wide <- 
    as.data.frame(img) %>% 
    mutate(channel = case_when(cc == 1 ~ "Red",
                               cc == 2 ~ "Green", 
                               cc == 3 ~ "Blue")) %>% 
    select(-cc) %>%
    pivot_wider(names_from = channel, values_from = value) %>% 
    mutate(color = rgb(Red, Green, Blue))
  
  # sample data frame
  sample_size <- sample
  img_sample <- img_wide[sample(nrow(img_wide), sample_size), ]
  
  # add random weights for point size
  img_sample$size <- runif(sample_size)
  
  # plot a voronoi diagram
  p <- 
    ggplot(img_sample) +
    geom_voronoi(mapping = aes(x = x, y = y, fill = color)) +
    scale_fill_identity() +
    scale_y_reverse() +
    theme_void() +
    theme(plot.margin = unit(c(-3, -3, -3, -3), "cm"))
  
  # save vonoroi diagram
  ggsave(plot = p,
         filename = "01_step1.jpeg", path = output_path,
         height = 1500, width = 1500, units = "px")
  
  # load image, crop circle, add border
  img_circle <- 
    magick::image_read(cropcircles::crop_circle(here("output", "01_step1.jpeg"), 
                                                bg_fill = "black", 
                                                border_size = 5)) %>% 
    magick::image_border(., color = "black", geometry = "200x200")
  
  # save circle with border  
  magick::image_write(img_circle, 
                      path = file.path(output_path, "02_step2.jpeg"), 
                      format = "jpeg",
                      quality = 100,
                      density = 300)
  
  # read flatten image circle
  img_circle <- magick::image_read(file.path(output_path, "02_step2.jpeg"))
  
  img_final <- magick::image_composite(img_circle,
                                       img_qr_code,
                                       offset = paste0("+", (1910-250), "+", (1910-250))) 
  
  # save final work
  magick::image_write(img_final, 
                      path = file.path(output_path, paste0(title_no_spaces, ".jpeg")), 
                      format = "jpeg",
                      quality = 100,
                      density = 300)
  
  # status message
  message(paste0("All done! See ", title_no_spaces, " at: ", output_path))
}

# create feeder main
fnc_voronoi(title_no_spaces = "feeder_main",
            sample = 25000,
            qr_code_signature = "https://github.com/RodDalBen/feeder_main_gen_art")
