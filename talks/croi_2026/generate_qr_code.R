install.packages("qrcode")                                                               
library(qrcode)                                                                          
qr <- qr_code("https://jheem.org")                                                       
generate_svg(qr, "jheem-qr.svg")       
