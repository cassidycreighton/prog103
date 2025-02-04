library(marinecs100b)


# Review: write a function ------------------------------------------------

# P1 Describe succinctly what the following code does. This should be a
# high-level, one-sentence description, not a line-by-line breakdown.
#It describes the hours per day of extreme cold events due to air exposure in
#Nuka pass in the late winter
site <- "Nuka_Pass"
season <- "Late winter"
n_cold <- sum(kefj_site == site &
                kefj_season == season &
                kefj_temperature <= -4 &
                kefj_exposure == "air")
n_total <- sum(kefj_site == site &
                 kefj_season == season)
hours_cold <- n_cold * 30 / 60
days_total <- n_total * 30 / 60 / 24
hours_cold_per_day <- hours_cold / days_total
hours_cold_per_day

# P2 Let's turn that code chunk into a function. What would you call that
# function? How many parameters should it take and what would you call them?
#hrs_cold_per_day
#variables: site, season

# P3 Write a function to encapsulate the code chunk above. Check that it
# contains all five parts of a function.
hrs_cold_per_day <- function(site, season){
  n_cold <- sum(kefj_site == site &
                  kefj_season == season &
                  kefj_temperature <= -4 &
                  kefj_exposure == "air")
  n_total <- sum(kefj_site == site &
                   kefj_season == season)
  hours_cold <- n_cold * 30 / 60
  days_total <- n_total * 30 / 60 / 24
  return (hours_cold / days_total)
}
hrs_cold_per_day("Nuka_Pass", "Late winter")
# Make an extreme choice --------------------------------------------------

# P4 Fill in the code below to create a logical vector indicating extreme
# temperatures.

extreme_typ <- "cold"
if (extreme_typ == "cold") {
  is_extreme <- kefj_temperature <= -4
} else if (extreme_typ == "hot"){
  is_extreme <- kefj_temperature >= 25
}
is_extreme
extreme_type <- function(type){
  if (type == "cold") {
    is_extreme <- kefj_temperature <= -4
  } else if (type == "hot"){
    is_extreme <- kefj_temperature >= 25
  }
  return (is_extreme)
}
# P5 Copy-paste the code from P1 and edit it to incorporate the is_extreme
# vector into the extreme temperature exposure procedure.
site <- "Nuka_Pass"
season <- "Early winter"
ext_type <- "cold"
n_ext <- sum(kefj_site == site &
                kefj_season == season &
                extreme_type(ext_type) &
                kefj_exposure == "air")
n_total <- sum(kefj_site == site &
                 kefj_season == season)
hours_ext <- n_cold * 30 / 60
days_total <- n_total * 30 / 60 / 24
hours_ext_per_day <- hours_cold / days_total
hours_ext_per_day

# P6 Copy-paste the function you wrote in P3 and edit it to add a parameter that
# lets you switch between extreme heat and cold exposure.
extreme_type <- function(type){
  if (type == "cold") {
    is_extreme <- kefj_temperature <= -4
  } else if (type == "hot"){
    is_extreme <- kefj_temperature >= 25
  }
  return (is_extreme)
}
hrs_extreme_per_day <- function(site, season, type){
  n_extreme <- sum(kefj_site == site &
                     kefj_season == season &
                     extreme_type(type) &
                     kefj_exposure == "air")
  n_total <- sum(kefj_site == site &
                   kefj_season == season)
  hours_extreme <- n_extreme * 30 / 60
  days_total <- n_total * 30 / 60 / 24
  return (hours_extreme / days_total)
}
hrs_extreme_per_day("Nuka_Pass", "Late winter", "hot")

# Season to taste ---------------------------------------------------------

# P7 What seasons are in the kefj dataset? What function would you use to
# identify them?
#late winter, spring, summer, fall, early winter
table(kefj_season)
# P8 Fill in the blanks below to make a for loop that prints the extreme hot and
# cold exposure across seasons at site Aialik.

seasons <- c("Late winter", "Spring", "Summer", "Fall", "Early winter")
for (i in 1:length(seasons)) {
    heat_exposure <- hrs_extreme_per_day("Aialik", seasons[i], "hot")
    cold_exposure <- hrs_extreme_per_day("Aialik", seasons[i], "cold")
    print(paste("Aialik", seasons[i], heat_exposure, cold_exposure))
}

# P9 Copy-paste your answer to P8 and add a nested for loop to iterate across
# sites as well as seasons.
table(kefj_site)
sites <- c("Aialik", "Harris", "McCarty", "Nuka_Bay", "Nuka_Pass")
for (h in 1:length(sites)){
  for (i in 1:length(seasons)) {
    heat_exposure <- hrs_extreme_per_day(sites[h], seasons[i], "hot")
    cold_exposure <- hrs_extreme_per_day(sites[h], seasons[i], "cold")
    print(paste(sites[h], seasons[i], heat_exposure, cold_exposure))
  }
}

# P10 Examine your results from P9. You should find two outputs where both
# extreme heat and cold exposure were 0. What season were they in?
#Fall
