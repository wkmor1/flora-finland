for (page in setdiff(list.dirs("src"), "src/favicon")) {

  for (i in list.files(page, pattern = ".jpeg", full.names = TRUE)) {

    scaled_image <- sub("_orig", "", i)

    if (!file.exists(scaled_image)) {

      imgi <- magick::image_read(i)

      imgi <- magick::image_scale(imgi, "700")

      magick::image_write(imgi, path = scaled_image, format = "jpg")

    }

  }

}
