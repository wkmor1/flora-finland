for (page in setdiff(list.dirs("src"), c("src/favicon"))) {

  for (i in list.files(page, pattern = ".jpeg", full.names = TRUE)) {

    imgi <- magick::image_read(i)

    imgi <- magick::image_scale(imgi, "700")

    magick::image_write(imgi, path = sub("_orig", "", i), format = "jpg")

  }

}
