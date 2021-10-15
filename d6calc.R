#s3calc.R
neutral = "gray90"
makeS3data <- function(neutral) {
  N <- 12
  DF <- data.frame(button=character(N),
                   perm =character(N),color=character(N),stringsAsFactors= FALSE)
  DF[1,] <- c("btnI","I",neutral)
  DF[2,] <- c("btn123456","(123456)",neutral)
  DF[3,] <- c("btn135246","(135)(246)",neutral)
  DF[4,] <- c("btn142536","(14)(25)(36)",neutral)
  DF[5,] <- c("btn153264","(153)(264)",neutral)
  DF[6,] <- c("btn165432","(165432)",neutral)
  DF[7,] <- c("btn2635","(26)(35)",neutral)
  DF[8,] <- c("btn123645","(12)(36)(45)",neutral)
  DF[9,] <- c("btn1346","(13)(46)",neutral)
  DF[10,] <- c("btn142356","(14)(23)(56)",neutral)
  DF[11,] <- c("btn1524","(15)(24)",neutral)
  DF[12,] <- c("btn162534","(16)(25)(34)",neutral)
  return(DF)
}

DF <- makeS3data(neutral)
DF
