

# It was tested for Ubuntu 16.04

library(extrafont)

download.file('http://simonsoftware.se/other/xkcd.ttf', 
              dest='xkcd.ttf', mode='wb')
system('mkdir ~/.fonts')
system('cp xkcd.ttf  ~/.fonts')
font_import(paths = '~/.fonts', pattern='[X/x]kcd')
system('fc-cache -f -v')
fonts()
loadfonts()