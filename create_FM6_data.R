#
# Convert data to binary
#
# time long lat alt ch1 ch2 ch3 ch4
# 24550	-43.2	-77.2	3900.0	10	112	0	5
#
#  152720    -123.8  47.5  11500 20 0 0 0 
#

create_fm6_data <- function(time_start,long,lat,alt,ch1,ch2,ch3,ch4) {
  library(bit)
  #d <- c(24550,-43.2,-77.2,3900.0,10,112,0,5)
  #d <- c(152720,-123.8,47.5,11500.0,20,0,0,0)
  d <- c(time_start,long,lat,alt,ch1,ch2,ch3,ch4)
  
  # TIME
  b1 <- as.hexmode(bitwAnd(as.integer(d[1])/10,0xff))
  b1 <- format(as.hexmode(b1),width = 2, upper.case = TRUE)
  b2 <- as.hexmode(bitwAnd(bitwShiftR(as.integer(d[1])/10,8),0x7f))

  # LONGITUDE 
  tmp <- bit(16)
  tmp <- (d[2]  * 10.0) + 1800.0

  tmp1 <- format(as.hexmode(tmp),width = 4, upper.case = TRUE)
  b2 <- as.hexmode(bitwOr(b2,bitwShiftL(bitwAnd(as.hexmode(tmp1),0x0001),7)))
  b2 <- format(as.hexmode(b2),width = 2, upper.case = TRUE)

  b3 <- as.hexmode(bitwAnd(bitwShiftR(as.hexmode(tmp1),1),0xff))
  b3 <- format(as.hexmode(b3),width = 2, upper.case = TRUE)
  b4 <- as.hexmode(bitwAnd(bitwShiftR(as.hexmode(tmp1),9),0x7f))

  # LATITUDE - TBD???
  tmp <- (d[3] * 10) + 900.0 
  tmp1 <- bitwAnd(bitwShiftL(as.hexmode(tmp),3),0xf8)
  b4 <- as.hexmode(bitwOr(b4,tmp1))
  b4 <- format(as.hexmode(b4),width = 2, upper.case = TRUE)
  b5 <- as.hexmode(bitwAnd(bitwShiftR(as.hexmode(tmp),5),0x3f))

  # ALTITUDE  - 
  tmp <- (d[4] + 1000) / 100
  tmp1 <- bitwAnd(bitwShiftL(as.hexmode(tmp),6),0xc0)
  b5 <- as.hexmode(bitwOr(b5,tmp1))
  b6 <- as.hexmode(bitwAnd(bitwShiftR(as.hexmode(tmp),2),0x7f))
  b6 <- format(as.hexmode(b6),width = 2, upper.case = TRUE)

  b7 <- format(as.hexmode(d[5]),width = 2, upper.case = TRUE)
  b8 <- format(as.hexmode(d[6]),width = 2, upper.case = TRUE)
  b9 <- format(as.hexmode(d[7]),width = 2, upper.case = TRUE)
  b10 <- format(as.hexmode(d[8]),width = 2, upper.case = TRUE)

  return(paste(b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,sep=''))
}


time_sequence <- c(24550,24600,24610,24620)

create_fm6_data(time_start = 24550,long = -43.2,lat = -77.2,alt = 3900.0,ch1 = 10,
                ch2 = 112,ch3 = 0,ch4 = 5)

for (i in time_sequence) {
  x <- create_fm6_data(time_start = i,long = -43.2,lat = -77.2,alt = 3900.0,ch1 = 10,
                  ch2 = 112,ch3 = 0,ch4 = 5)
  print(x)
}

