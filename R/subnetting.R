library(plyr)

int2ip <- function (int) {
  
  # inverse of ip2int
  # takes an integer, returns a dotted decimal IP address
  
  return (sapply(int, FUN=function(n){
# FIX ME
#    if (n < 0 | n >= 32^2) stop ("that integer is not an IP address")
# FIX ME
    o1 <- floor(n / 256^3)
    o2 <- floor((n - (o1 * 256^3)) / 256^2)
    o3 <- floor((n - (o1 * 256^3) - (o2 * 256^2)) / 256)
    o4 <- (n - (o1 * 256^3) - (o2 * 256^2) - (o3 *256))
    paste(o1, o2, o3, o4, sep=".")
  }))
}

ip2int <- function(ip) {
  
  # inverse of int2ip
  # takes a list of IP addresses as input and returns their
  # integer equivalents
  
  octets <- strsplit(ip, "\\.")
  # if strsplit returns more or less than 4, stop("that is not an IP address")
  return (laply(octets, .fun=function(n){
# FIX ME
#    if (length(n != 4)) stop ("every IP needs exactly 4 octets")
# FIX ME
    (as.numeric(n[1]) * 256^3) + (as.numeric(n[2]) * 256^2) + (as.numeric(n[3]) * 256) + (as.numeric(n[4]))
    }))
}

findBegin <- function(network, CIDR) {
#  if (CIDR > 32 | CIDR < 0) stop("illegal subnet size")
  networkInt = ip2int(network) - (ip2int(network) %% (2^32 / 2^CIDR))
  return (int2ip(networkInt))
}

findEnd <- function(network, CIDR) {
#  if (CIDR >32 | CIDR < 0) stop("illegal subnet size")
  networkInt = ip2int(network) - (ip2int(network) %% (2^32 / 2^CIDR))
  return (int2ip(networkInt + (2^32 / 2^CIDR) - 1))
}

isWithin <- function(ip, network, CIDR) {
  if (length(CIDR) > 1 & length(network) != length(CIDR)) stop("a single CIDR is applied to all networks, or each network needs a CIDR");
  beginInt <- ip2int(findBegin(network, CIDR))
  endInt <- ip2int(findEnd(network, CIDR))
  ipInt <- ip2int(ip)
  for (eachIP in ipInt){
    for (eachInt in 1:(length(beginInt))) {
      if (beginInt[eachInt] < eachIP & eachIP < endInt[eachInt]){
        print(paste(int2ip(eachIP), "is within", int2ip(beginInt[eachInt]), int2ip(endInt[eachInt])))
      }
    }
  }
}

isBogon <- function(ip, refresh=FALSE) {
# Thanks for being awesome, TC!
  
  bogon.dir <- file.path(path.expand("~"),".ipcache")
  bogon.file <- file.path(bogon.dir,"bogons.txt")
  dir.create(bogon.dir, showWarnings=FALSE)
  
  if (refresh || file.access(bogon.file)) {
    bogon.url <- "http://www.team-cymru.org/Services/Bogons/fullbogons-ipv4.txt";
    download.file(bogon.url, bogon.file)
  }
  
  b <- read.table(bogon.file, comment.char="#", colClasses=c("character"))
  b <- laply(b, .fun=function(n) {strsplit(n,"/", fixed=TRUE)})
  b <- adply(b, c(1))
  b <- b[-1]
  names(b) <- c("network","CIDR")
  isWithin(ip, b$network, as.numeric(b$CIDR))
}
