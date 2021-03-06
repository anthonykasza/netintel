#
# bulkorigin.R - perform bulk IP to ASN mapping via Team Cymru whois service
#
# Author: @hrbrmstr
# Version: 0.1
# Date: 2013-02-07
#
# Copyright 2013 Bob Rudis
# 
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
#   
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
# LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
# OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
# WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.#

library(plyr)

# short function to trim leading/trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

BulkOrigin <- function(ip.list,host="v4.whois.cymru.com",port=43) {
  
  # Retrieves BGP Origin ASN info for a list of IP addresses
  #
  # NOTE: IPv4 version
  #
  # NOTE: The Team Cymru's service is NOT a GeoIP service!
  # Do not use this function for that as your results will not
  # be accurate.
  #
  # Args:
  #   ip.list : character vector of IP addresses
  #   host: which server to hit for lookup (defaults to Team Cymru's server)
  #   post: TCP port to use (defaults to 43)
  #
  # Returns:
  #   data frame of BGP Origin ASN lookup results
  
  
  # setup query
  cmd = "begin\nverbose\n" 
  ips = paste(unlist(ip.list), collapse="\n")
  cmd = sprintf("%s%s\nend\n",cmd,ips)
  
  # setup connection and post query
  con = socketConnection(host=host,port=port,blocking=TRUE,open="r+")  
  cat(cmd,file=con)
  response = readLines(con)
  close(con)

  # trim header, split fields and convert results
  response = response[2:length(response)]
  response = laply(response,function(n) {
    sapply(strsplit(n,"|",fixed=TRUE),trim)
  })  
  response = adply(response,c(1))
  response = response[,2:length(response)]
  names(response) = c("AS","IP","BGP.Prefix","CC","Registry","Allocated","AS.Name")
  
  return(response)
  
}

BulkASN <- function(asn.list,host="v4.whois.cymru.com",port=43) {
  
  # Retrieves basic AS info for a list of ASN
  #
  # NOTE: IPv4 version
  #
  # NOTE: The Team Cymru's service is NOT a GeoIP service!
  # Do not use this function for that as your results will not
  # be accurate.
  #
  # Args:
  #   asn.list : character vector of AS numbers 
  #   host: which server to hit for lookup (defaults to Team Cymru's server)
  #   post: TCP port to use (defaults to 43)
  #
  # Returns:
  #   data frame of BGP Origin ASN lookup results
  
  
  # setup query
  cmd = "begin\nverbose\n" 
  asns = paste(unlist(asn.list), collapse="\n")
  cmd = sprintf("%s%s\nend\n",cmd,asns)
  
  # setup connection and post query
  con = socketConnection(host=host,port=port,blocking=TRUE,open="r+")  
  cat(cmd,file=con)
  response = readLines(con)
  close(con)

  # trim header, split fields and convert results
  response = response[2:length(response)]
  response = laply(response,function(n) {
    sapply(strsplit(n,"|",fixed=TRUE),trim)
  })  
  response = adply(response,c(1))
  response = response[,2:length(response)]
  names(response) = c("AS","CC","Registry","Allocated","AS Name")
  
  return(response)
  
}

BulkPeer <- function(ip.list,host="v4-peer.whois.cymru.com",port=43) {
  
  # Retrieves BGP Peer ASN info for a list of IP addresses
  #
  # NOTE: IPv4 version
  #
  # NOTE: The Team Cymru's service is NOT a GeoIP service!
  # Do not use this function for that as your results will not
  # be accurate.
  #
  # Args:
  #   ip.list : character vector of IP addresses
  #   host: which server to hit for lookup (defaults to Team Cymru's server)
  #   post: TCP port to use (defaults to 43)
  #
  # Returns:
  #   data frame of BGP Peer ASN lookup results
  
  
  # setup query
  cmd = "begin\nverbose\n" 
  ips = paste(unlist(ip.list), collapse="\n")
  cmd = sprintf("%s%s\nend\n",cmd,ips)
  
  # setup connection and post query
  con = socketConnection(host=host,port=port,blocking=TRUE,open="r+")  
  cat(cmd,file=con)
  response = readLines(con)
  close(con)
  
  # trim header, split fields and convert results
  response = response[2:length(response)]
  response = laply(response,function(n) {
    sapply(strsplit(n,"|",fixed=TRUE),trim)
  })  
  response = adply(response,c(1))
  response = response[,2:length(response)]
  names(response) = c("Peer.AS","IP","BGP.Prefix","CC","Registry","Allocated","Peer.AS.Name")
  return(response)
  
}



CIRCL.BGP.Rank <- function(asn, circl.base.url="http://bgpranking.circl.lu/csv/") {
  
  # Retrieves CIRCL aggregated, historical/current BGP rank data
  #
  # Args:
  #   asn : ASN list to lookup
  #   circl.base.url : lookup URL (optional parameter in the event it changes)
  #
  # Returns:
  #   data frame of CIRCL rank data (day|rank)
  #
  # Background on CIRCL project (+ source): https://github.com/CIRCL/bgp-ranking
  # CIRCL BGP site: http://bgpranking.circl.lu/
  # Sample CIRCL output: http://bgpranking.circl.lu/csv/57954
  # day,rank
  # 2012-09-19,1.00020833333
  # 2012-09-18,1.00020833333
  
  ranks = lapply(asn,function(x){
    read.csv(sprintf("%s%s",circl.base.url,x))
  })

  return(ranks)
}


SANS.ASN.Detail <- function(asn, sans.base.url="http://isc.sans.edu/asdetailsascii.html?as=") {

  # Retrieves SANS ASN currently tracked IP detail 
  #
  # Args:
  #   asn : ASN to lookup
  #   sans.base.url : lookup URL (optional parameter in the event it changes)
  #
  # NOTE: If you use the "https" URL access, you'll need to change the code below 
  #       on some systems that can't read https directly
  #
  # Returns:
  #   data frame of SANS ASN IP data (see example below)
  #
  # https://isc.sans.edu/asdetailsascii.html?as=21844
  # asdetailsascii.html
  # created: Fri, 08 Feb 2013 14:17:01 +0000#
  # Source IP is 0 padded so each byte is three digits long
  # Reports: number of packets received
  # Targets: number of target IPs that reported packets from this source.
  # First Seen: First time we saw a packet from this source
  # Last Seen: Last time we saw a packet from this source
  # Updated: Last time the record was updated.
  #
  # IPs are removed if they have not been seen in 30 days.
  #
  # source IP <tab> Reports <tab> Targets <tab> First Seen <tab> Last Seen <tab> Updated <CR>
  # 118.102.187.188  4495  3909	2010-05-12	2013-02-02	2013-02-02 10:42:01
    
  src = readLines(sprintf("%s%s",sans.base.url,asn))
  asn.df = read.table(textConnection(src[grep("^#", src, invert=TRUE)]))
  names(asn.df) = c("Source.IP","Reports.Count","Targets.Count","First.Seen","Last.Seen","Updated.Date","Updated.Time")
  
  return(asn.df)
  
}


Alien.Vault.Reputation <- function(refresh=FALSE) {
  
  # Retrieves Alien Vault's IP reputation database
  #
  # Args:
  #   refresh: should the function refresh the database; AlienValut refreshes every hour, but the onus
  #            is on the caller to force a refresh. First-time call will setup a cache directory & file
  #            in the user's home directory, download & generate the data frame then write the data frame
  #            out as an R object. Future calls will just re-read this data frame unless refresh == TRUE
  #            Please be kind to the Alien Valut servers & only refresh if you really need to.
  #
  # Returns:
  #   data frame with IP & Reputation factor
  #
  # TODO: Need to split out the ";" separated factors
  #
  # Background on Alien Valut's IP rep db: http://labs.alienvault.com/labs/index.php/projects/open-source-ip-reputation-portal/download-ip-reputation-database/
  #
  
  alien.valut.reputation.gzurl = "http://reputation.alienvault.com/reputation.snort.gz"
  
  av.dir = file.path(path.expand("~"), ".ipcache")
  av.file =  file.path(av.dir,"alienvaultrepdf")
  av.gzfile =  file.path(av.dir,"reputation.snort.gz")
  
  dir.create(av.dir, showWarnings=FALSE)
  if (refresh || file.access(av.file)) {
    download.file(alien.valut.reputation.gzurl,av.gzfile)  
    src = gzfile(av.gzfile)
    raw = readLines(src)
    close(src)
    av.matrix = t(sapply(strsplit(raw[9:length(raw)],"\ +#\ +"),c))  
    av.df = data.frame(IP=av.matrix[,1],Reputation=factor(av.matrix[,2]),stringsAsFactors=FALSE)
    dput(av.df,av.file)
  } else {
    av.df = dget(av.file)
  }
  
  return(av.df)
  
}
