# This is written to strip the multi-hyphenated city names from the census

chomp = function (x) {gsub("^\\s+|\\s+$", "", x)} # desperately needed

nicename = function(name) {
    v = strsplit(name, split = ",")
    
    state = substr(chomp(v[[1]][2]), 1, 2)
    restofname = chomp(v[[1]][1])
    firstname = strsplit(restofname, split = "-")[[1]][1]
    return(paste(firstname, state, sep = ", "))
}
nicenames = sapply(msaname.short, nicename)
cbind(nicenames)

# manually fix up some that got screwed this way
nicenames[grep("Louisville", names(nicenames))] = "Louisville, KY"
nicenames[grep("Winston", names(nicenames))] = "Winston-Salem, NC"
nicenames[grep("Dallas", names(nicenames))] = "Dallas-Ft. Worth, TX"
nicenames[grep("Midland", names(nicenames))] = "Midland-Odessa, TX"
nicenames[grep("Minneapolis", names(nicenames))] = "Minneapolis-St. Paul, MN"
nicenames[grep("Daytona", names(nicenames))] = "Daytona Beach, FL"
nicenames[grep("Seattle", names(nicenames))] = "Seattle-Tacoma, WA"
nicenames[grep("Tampa", names(nicenames))] = "Tampa-St. Petersburg, FL"

names(nicenames) = NULL
cbind(nicenames)

