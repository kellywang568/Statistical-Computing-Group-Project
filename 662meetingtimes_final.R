# Names of group members: Kelly Wang, Dohyun Lee, Emily Goldfarb

class_name <- '662'      
department_name <- "S&DS"
meeting_length <- "45 min"
day <- "Thursday"

# For our project, we will be utilizing a portion of Jay's code to help with 
# the setup of the code. 
# 
# Generating a grid of time availability given data set from multiple people
################################################################################
# Pull in and process the time availability from the survey
# For this case, we assume times will be on "Thursday" and "Friday

x <- read.csv("662_survey.csv", as.is = TRUE)

names(x)[c(10,12)] <- c("MeetThursday", "MeetFriday")
x <- x[, substring(names(x), 1, 1) != "X"]
x <- x[order(x$name, x$submitted, decreasing = TRUE), ]
x <- x[!duplicated(x$name), ]

x <- x[, c("name", "MeetThursday", "MeetFriday")]

################################################################################
# Create a csv file named, "AssignedTimesMain.csv"
# If file already exists, read the file 
# Assign everyone the time of '9' and order the names 

if (file.exists("AssignedTimesMain.csv")) {
  a <- read.csv("AssignedTimesMain.csv", as.is=TRUE)
} else {
  # Initialize the first time this is run:
  x$day <- "Thursday"
  x$time <- 9
  
  write.csv(a <- x[, c("name", "section", "day", "time")],
            "AssignedTimesMain.csv", row.names = FALSE)
}

x <- merge(x[, names(x) != "section"], a, all.x = TRUE, all.y = FALSE)
# Special code used later
x$time[is.na(x$time)] <- -99                    

# We put [NONE] if a student's assigned time does not match with one of their
# available times 

for (i in 1:nrow(x)) {
  possible <- ifelse(x$day[i] == "Thursday", x$MeetThursday[i], x$MeetFriday[i])
  if (!grepl(paste0(x$time[i], ":00"), possible)) {
    x$name[i] <- paste(x$name[i], "[NONE]") 
  }
}

y <- x[, c("name", "MeetThursday")]
y$name <- gsub(" [NONE]", "", y$name, fixed = TRUE)
names(y)[2] <- "available"

z <- x[, c("name", "MeetFriday")]
z$name <- gsub(" [NONE]", "", z$name, fixed = TRUE)
names(z)[2] <- "available"

# Create empty list 'thurs':
thurs <- vector(mode = "list", length = nrow(x))
names(thurs) <- y$name

# Create empty list 'fri':
fri <- vector(mode = "list", length = nrow(x))
names(fri) <- z$name

#'
#' Converts string to numeric times
#'
#' @param x, list of times
#'
#' @return a, list of times converted to numeric values
#' 
#' @details, this function converts given list of times elements in strings
#' into numeric value. The string is converted to facilitate sequential 
#' organization of the time slots. time_to_numeric primarily
#' acts as a conversion tool that can create an intermediate internal system 
#' organization before converting back to time format.  
#'
time_to_numeric <- function(x) {
  a <- matrix(as.numeric(unlist(strsplit(x, ":"))),
              ncol = 2, byrow = TRUE)
  a <- round(a[, 1] + a[, 2] / 60, 2)
  a[a < 8] <- a[a < 8] + 12     # Make it a 24-hour clock
  return(a)
}

# Process list of available times for each day:
for (i in 1:length(thurs)) {
  temp <- unlist(strsplit(y$available[i], ","))
  temp <- strsplit(temp, split = "-")
  temp <- lapply(temp, time_to_numeric)
  temp <- lapply(temp, function(a) {
    names(a) <- c("start", "end")
    return(a)
  })
  thurs[[i]] <- temp
}

for (i in 1:length(fri)) {
  temp <- unlist(strsplit(z$available[i], ","))
  temp <- strsplit(temp, split = "-")
  temp <- lapply(temp, time_to_numeric)
  temp <- lapply(temp, function(a) {
    names(a) <- c("start", "end")
    return(a)
  })
  fri[[i]] <- temp
}

# Make list of Thursday available times
thurs_data <- lapply(thurs, function(a) do.call(rbind, a))

# Make list of Friday available times
fri_data <- lapply(fri, function(a) do.call(rbind, a))

# Get vector of all time intervals
# Append a time into 'longest' if it wasn't in the previous student's
# available times then sort in ascending order

thurs_longest <- thurs_data[[1]][, 1]
for (i in 2:length(thurs_data)) {
  if (length(thurs_data[[i]]) != 0) {
    for (j in 1:length(thurs_data[[i]][, 1])) {
      if (!(thurs_data[[i]][, 1][j] %in% thurs_longest)) {
        thurs_longest <- append(thurs_longest, thurs_data[[i]][, 1][j])
      }
    }
  }
}

fri_longest <- fri_data[[1]][, 1]
for (i in 2:length(fri_data)) {
  if (length(fri_data[[i]]) != 0) {
    for (j in 1:length(fri_data[[i]][, 1])) {
      if (!(fri_data[[i]][, 1][j] %in% fri_longest)) {
        fri_longest <- append(fri_longest, fri_data[[i]][, 1][j])
      }
    }
  }
}

# Sort to have time slots in chronological order 
thurs_longest <- sort(thurs_longest)
fri_longest <- sort(fri_longest)

# Create list of assigned times student by student
# Initializes thurs_assigned
thurs_assigned <- vector(mode = "list", length = nrow(x))
names(thurs_assigned) <- y$name
for (i in 1:length(thurs_assigned)) {
  thurs_assigned[[i]] <- x[, "time"][i]
}

# Initializes fri_assigned
fri_assigned <- vector(mode = "list", length = nrow(x))
names(fri_assigned) <- z$name
for (i in 1:length(fri_assigned)) {
  fri_assigned[[i]] <- x[, "time"][i]
}

# Assigns Thurs availability times to first available time for each student
for (i in 1:length(thurs_assigned)) {
  if (length(thurs_data[[i]]) != 0) {
    thurs_assigned[[i]] <- thurs_data[[i]][1]
  } else {
    thurs_assigned <- 0
  }
}

# Assigns Fri availability times to first available time for each student
for (i in 1:length(fri_assigned)) {
  if (length(fri_data[[i]]) != 0) {
    fri_assigned[[i]] <- fri_data[[i]][1]
  } else {
    fri_assigned[[i]] <- 0
  }
}

#
#' Convert numeric times to 12 hour time format
#'
#' @param longest, numeric vector
#'
#' @return times string vector 
#' @export
#'
#' @details the function below takes the numeric vector we had originally 
#' converted our times into and re-converts them back into a 12 hour clock
#' format; each of the times are then individually labeled as an element 
#' within a string to then be visually presented 
#' 
dec_to_time <- function(longest) {
  times <- c()
  for (i in (1:length(longest))) {
    if (longest[i] > 12){
      longest[i] = longest[i] - 12
    }
    if (longest[i] %% 1 == 0) {
      times[i+1] <- paste0(longest[i] , ":00 ")
    } else {
      times[i+1] <- paste0(longest[i] - longest[i] %% 1, ":" , 
                           (longest[i] %% 1) * 60, " " )
    }
  }
  times <- times[-1]
  return(times)
}

library(grid)

# 
#' Make interface with names and times
#' 
#' @param x  A list of length equal to the number of options
#' that will appear on the y-axis.  Each component of x contains a 
#' numeric list of start times that the options are available
#' @param times a numeric vector containing all possible start times wherein 
#' at least one option was available. 
#' @param assigned a list of length equal to x.  
#' Each component of assigned contains the times each option will have a 
#' meeting.
#'
#' @return None
#'
#' @details this function primarily serves to map the available times and then 
#' the assigned times for each student;
#' 
#' we first create a viewport of the student names and place them in the left 
#' column; the column names are then spaced and positioned with a fixed font
#' that would be reasonably viewed from different devices;
#' 
#' the second viewport creates the rectangles for which we can then assign times
#' and shade in for picking student availability
#' 
#' the third viewport maps the time availability as rectangles to each row
#' of students. These all start out as blank rectangles; in the dataframe, for 
#' times that are not labeled as available to students, we leave those areas 
#' as blank 
#' 
#' the fourth viewport fills in the first available time for the students with 
#' an orange color; this visual strike allows the viewer to see where the list
#' of time availabilities begin at each row
#' 
#' the fifth viewport creates the header of times and spaces it out in 
#' proportion to the number of students listed; this accounts for when we have
#' a case where there is a plethora of students or participants and when we 
#' have a dearth; additionally, after it creates the times, it proceeds to 
#' draw in the rectangles underneath respective times for each student. 
#' 
#' after popping out the viewports, the user can retrieve a pdf copy of all
#' visual layout and designs printed cleanly in said sheet with adjusted title 
#' and labels that correspond to the variables of what the user intends to use
#' said pdf for;

map_avail_times <- function(x, times, assigned) {
  grid.newpage()
  
  pushViewport(viewport(x = 0.5, y = 0.5,
                        width = 0.98, height = 0.85))
  grid.rect()
  
  maxlen <- unit(1, "strwidth",
                 data = names(x)[which.max(nchar(names(x)))])
  
  # (1) Names, left side
  pushViewport(viewport(x = 0, y = 0,
                        width = maxlen + unit(1, "char"), 
                        height = 1,
                        just = c("left", "bottom"),
                        yscale = c(0.5, length(x) + 1.5)))
  grid.text(names(x), 
            x = unit(0.5, "char"), 
            y = unit(1:length(x), "native"),
            just = c("left", "center"))
  popViewport(1) #pops Names viewport
  
  # (2) Viewport for mapping time availability
  pushViewport(viewport(x = 1, y = 0, 
                        width = unit(1, "npc") - (maxlen + unit(1, "char")), 
                        height = unit(1, "npc"),
                        just = c("right", "bottom"),
                        xscale = c(times[1] - 2, times[length(times)] + 2),
                        yscale = c(0.5, length(x) + 1.5)))
  grid.rect()
  
  # (3) Map the available times for each student
  for (n in 1:length(x)) {
    if (length(x[[n]]) != 0) {
      for (t in 1:length(x[[n]][, 1])) {
        grid.rect(x = unit(x[[n]][, 1][t], "native"), 
                  y = unit(n, "native"),
                  width = 0.06 ,
                  height = 0.04)
      }
    }
  }
  
  # (4) Map assigned time to each student
  for (i in 1:length(assigned)) {
    if (assigned[[i]] != 0){
      grid.rect(x = unit(assigned[[i]], "native"), 
                y = unit(i, "native"),
                width = 0.06,
                height = 0.04,
                gp = gpar(col = "black", fill = "orange"))
    }
  }
  popViewport(1) # pops time mapping viewport
  
  # (5) Create header for the times
  pushViewport(viewport(x = 1, y = 1, 
                        width = unit(1, "npc") - (maxlen + unit(1, "char")), 
                        height = unit(0.05, "npc"),
                        just = c("right", "top"),
                        xscale = c(times[1] - 2, times[length(times)] + 2),
                        yscale = c(0.5, length(x) + 1.5)))
  grid.rect() # Draw rectangles in respective times 
  
  for (i in 1:length(times)) {
    grid.text(dec_to_time(times[i]),  #write in respective times
              x = unit(times[i], "native"),
              y = unit(0.5, "npc"))
  }
  popViewport(1) # pops header viewport
  
  popViewport(1) # pops outer black rectangle
  
  graph_title <- paste0(department_name, " ", class_name, 
                       " ", day, " Meeting Times (", 
                       meeting_length, " sessions)")
  grid.text(x = 0.5, y = 0.95, graph_title) # writes in title of grid
}
map_avail_times(thurs_data, thurs_longest, thurs_assigned)

# Create pdf file 
pdf("662test.pdf") 
map_avail_times(thurs_data, thurs_longest, thurs_assigned)

# Set variables for Friday's calendar; can also be different class, department 
# or time 

class_name <- '662'      
department_name <- "S&DS"
meeting_length <- "45 min"
day <- "Friday"

map_avail_times(fri_data, fri_longest, fri_assigned)

dev.off()    