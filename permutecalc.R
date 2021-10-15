#This should work for any set of characters
library(stringr)
Perm.apply <- function(x,perm){
  #If x is not in the permutation, nothing happens
  if (!str_detect(perm,x))
    return(x)
  #Otherwise locate it and find the next symbol
  pos <- str_locate(perm,x)
  #To extract a character, we get a one-character substring
  nextch <- str_sub(perm,pos+1,pos+1)
  #If it's numeric, we are done
  if (nextch != ")")
    return(nextch)
  #Otherwise back up to find the number at the start of the cycle
  while(str_sub(perm,pos-1,pos-1) != "(")
    pos <- pos-1
  return (str_sub(perm,pos,pos))
}
#Perm.apply("5",("(13)(245)"))
#undebug(Perm.apply)

#Converts a vector of function values to cycle notation
#This is specific to permutations of the digits 1 through 9
#It will be reused in other apps
Perm.cycle.convert <- function(fval){
#We build the answer as a vector of characters
  cycles <- character(0)  #empty vector
  for (nextstart in c("1","2","3","4","5","6","7","8")) {
    m <- as.numeric(nextstart)  #convert to integer index
    if (m > length(fval)) break
    #If the symbol is unchanged or is already in a cycle, there is nothing to do
    if ((fval[m] == nextstart) || (nextstart %in% cycles)) next
    #Otherwise keep tracing through the cycle until we find the end
    nextch <- fval[m]
    cycles <- c(cycles,"(",nextstart,nextch) #start a new cycle
    nextch <- fval[as.numeric(nextch)]
    while (nextch != nextstart){
      cycles <- c(cycles,nextch)
      nextch <- fval[as.numeric(nextch)]
    }
    cycles <- c(cycles, ")")   #close the cycle
  }
  #Collapse the vector of cycles into a string    
  return(paste(cycles, sep = "", collapse=""))

}
#undebug(Perm.cycle.convert)

#Perm.cycle.convert(c("2","4","3","1"))
#Compute the product ab of two permutations of the symbols "1" through "9"
Perm.multiply <- function(a,b){
  if (a == "I") return(b)
  if (b == "I") return(a)
  #Make a vector of the function values
  fval <- character(0)
  for (i in c("1","2","3","4","5","6","7","8","9")) {
    fval <- c(fval, Perm.apply(Perm.apply(i,b),a) )
  }
  #If input and output are equal we have the identity
  if (sum(fval != "1":"9") == 0) return("I")
  #Otherwise generate cycle notation
  return(Perm.cycle.convert(fval))
}
#Perm.multiply("(123)","(12)(34)")
#undebug(Perm.multiply)
#Makes a list of powers separated by HTML line breaks
Perm.powerString <- function(perm) {
  power <- perm
  result <- perm
  while (power != "I") {
    power <- Perm.multiply(power,perm)
    result <- paste(result,power,sep = "<br/>")
  }
  return(result)
}

#Finds the inverse by stopping when the next power is the identity
Perm.inverse <- function(perm) {
  power <- perm
  while (TRUE) {
    if (Perm.multiply(power,perm) == "I")
      return(power)
    power <- Perm.multiply(power,perm)
  }
}
Perm.inverse("(123)(4689)")

#Forms the conjugate aba^(-1)
Perm.conjugate <- function(a,b) {
  Perm.multiply(a,Perm.multiply(b, Perm.inverse(a)))
}
Perm.conjugate("(24)(567)","(123)(4689)")


