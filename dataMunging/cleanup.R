
#***********************
# Use any needed part
#***********************

# Cleanup feature names
dat.names <- names(dat)
dat.names <- make.names(dat.names, unique = TRUE)
names(dat) <- dat.names

# Update feature names in dat
data.table::setnames(
  dat,
  as.character(features.name$feature),
  as.character(features.name$Description)
)

# Convert to Numerics
dat$name <- as.numeric(as.character(dat$name))


# Class of each column
dat.type <- sapply(1:ncol(dat), function(x)
  class(dat[[x]]))

# Separate Char and Num col
dat.char.names <- names(dat[dat.type == 'character'])
dat.num.names  <- names(dat[dat.type == 'numeric'])

# Table of each column
sapply(dat[dat.char.names], function(x)
  table(x, exclude = NULL))

# Columns which has NA
nacols <- function(df) {
  colnames(df)[unlist(lapply(df, function(x)
    anyNA(x)))]
}

dat.na.names <- nacols(dat)


#** change data all columns 'N/A' -> NA
dat[dat == 'N/A'] <- NA

#** dplyr remove a column(s)
dat <- select(dat,-col1)
dat <-
  select(dat,-one_of(c(
    'col2', 'col3', 'col4', 'col5'
  )))


#** Replace NAs with other values
dat$name[is.na(dat$name)] <- 0

# String to Numeric Factor
Str2NumFac <- function(x) {
  x <- as.factor(x)
  levels(x) <- 1:length(levels(x))
  x <- as.numeric(x)
  return(x)
}

#** Hard coding name to yes = 1 and others to 0
dat$name <-
  ifelse(tolower(dat$name) %in% c('yes', 'y'), 1, 0)


#** Get updated column names
dat.type <- sapply(1:ncol(dat), function(x)
  class(dat[[x]]))

#** Separate Char and Num col
dat.char.names <- names(dat[dat.type == 'character'])
dat.num.names  <- names(dat[dat.type == 'numeric'])

#** apply Str2NumFac funcion on data
dat[dat.char.names] <-
  lapply(dat[dat.char.names], function(x)
    Str2NumFac(x))


#** Sort by name col
dat <- dat %>%
  arrange(name)

#** filter by value
dat <-
  dat %>%
  filter(dat$name %in% c(0, 'soroosh', 'sorooshsohangir'))


saveRDS(dat, file = "ch_dat.rds")
