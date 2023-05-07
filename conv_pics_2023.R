
library(rvest)
library(dplyr)
library(stringr)

url.conv <- "https://www.isical.ac.in/~conv_pics/"
webpage.conv <- session(url.conv)

link.titles <- webpage.conv %>% html_nodes("img")
img.url <- link.titles %>% html_attr("src")
link.images <- paste0("https://www.isical.ac.in/~conv_pics/",img.url)
path.images <- paste0('conv_pics_2023/', img.url %>% str_remove('images/thumbnail/'))


for(k in 1:length(path.images)){
   download.file(link.images[k], path.images[k], mode = 'wb')
   
   print(k)
}


# download.file(link.images,
#               path.images,
#               mode = 'wb')

# download.file("https://www.isical.ac.in/~conv_pics/images/thumbnail/DSC_3396.jpg",
#               "trial.jpg", mode = 'wb')

