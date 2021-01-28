# testing function


## testing
sheetref<-'https://docs.google.com/spreadsheets/d/1pv041th4inXsSRTy8afM0gE14dgeR9TvwmJWEnnC6LQ/edit'

min_abund<-60
# bring in data from googlesheet
#  survey<-gsheet2tbl('https://docs.google.com/spreadsheets/d/1pv041th4inXsSRTy8afM0gE14dgeR9TvwmJWEnnC6LQ/edit#gid=0')

googlekites(sheetref, min_abund)



# profile

profile<-read.csv(file.choose())

profile

plot(profile$Distance, profile$Height, xlab="Distance above low water (m)", ylab="Height above c.d.")
lines(profile$Distance, profile$Height)
