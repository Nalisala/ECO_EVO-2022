
# how to write functions in R:

mean(c(4, 5, 6))
mean(x = c(4, 5, 6), na.rm = TRUE)

# write a function to convert fahrenheit to celsius
(100 - 32)*5/9

# input temperature in fahrenheight

# setting up a function
fahrenheight_to_celsius <- function(temp_F) { 
  
  temp_C <- (temp_F - 32)*5/9
  return(temp_C)
  
}

# test the function
fahrenheight_to_celsius(temp_F = -40)

# celsius to kelvin -> + 273.15
celsius_to_kelvin <- function(temp_C) {
  temp_K <- temp_C + 273.15
  return(temp_K)
}

# test the function
celsius_to_kelvin(temp_C = 10)
celsius_to_kelvin(10)

mean(x, na.rm)
mean(c(32, 45, 67), 0, TRUE)
mean(na.rm = TRUE, x = c(32, 45, 67))

# convert 77 F to Kelvin
fahrenheight_to_celsius(77) %>%
  celsius_to_kelvin()

tempc <- fahrenheight_to_celsius(77) 
celsius_to_kelvin(tempc)

# fahrenheight to kelvin
fahrenheight_to_kelvin <- function(temp_F) {
  temp_C <- fahrenheight_to_celsius(temp_F = temp_F)
  temp_K <- celsius_to_kelvin(temp_C = temp_C)
  return(temp_K)
}

# test the function
fahrenheight_to_kelvin(temp_F = 77)

### END
