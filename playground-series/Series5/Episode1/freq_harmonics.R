##
create_fourier_columns <- function(data, time_col){
  k_frequencies = seq(2,10,2)#2:48
  m_seasonal_period_harmonics = c(7,30.4, 365.25)
  #
  # print(data[time_col])
  #
  time_data = data[time_col]
  nrow_data = nrow(time_data)
  # print(nrow_data)
  #
  for (index_row in 1:nrow_data){
    time_rad <- data[index_row, time_col]
    #
    # print(time_rad)
    #
    for (k in k_frequencies) {
      for (m in m_seasonal_period_harmonics) {
        sin_colname <- paste0("sin_", k, "_", gsub(pattern = "\\.", replacement = "p", m))
        cos_colname <- paste0("cos_", k, "_", gsub(pattern = "\\.", replacement = "p", m))
        
        data[index_row, sin_colname] <- sin(k * pi * time_rad/m)
        data[index_row, cos_colname] <- cos(k * pi * time_rad/m)
      }
    }
  }
  return(data)
  #
}
#
# Example usage with sample data
df <- data.frame(time = 1:10) 
df <- create_fourier_columns(df, "time")
# warnings()
# View the resulting data frame
head(df)
dim(df)#
#
names_cols = c()
k_frequencies = seq(2,10,2)#2:48
m_seasonal_period_harmonics = c(7,30.4, 365.25)
for (k in k_frequencies) {
  for (m in m_seasonal_period_harmonics) {
    sin_colname <- paste0("sin_", k, "_", gsub(pattern = "\\.", replacement = "p", m))
    cos_colname <- paste0("cos_", k, "_", gsub(pattern = "\\.", replacement = "p", m))
    names_cols = c(names_cols, sin_colname, cos_colname)
  }
}
names_cols
#
paste0(names_cols, collapse = "+")
#
#
sin_2_7+cos_2_7+sin_2_30p4+cos_2_30p4+sin_2_365p25+cos_2_365p25+sin_4_7+cos_4_7+sin_4_30p4+cos_4_30p4+sin_4_365p25+cos_4_365p25+sin_6_7+cos_6_7+sin_6_30p4+cos_6_30p4+sin_6_365p25+cos_6_365p25+sin_8_7+cos_8_7+sin_8_30p4+cos_8_30p4+sin_8_365p25+cos_8_365p25+sin_10_7+cos_10_7+sin_10_30p4+cos_10_30p4+sin_10_365p25+cos_10_365p25+sin_12_7+cos_12_7+sin_12_30p4+cos_12_30p4+sin_12_365p25+cos_12_365p25+sin_14_7+cos_14_7+sin_14_30p4+cos_14_30p4+sin_14_365p25+cos_14_365p25+sin_16_7+cos_16_7+sin_16_30p4+cos_16_30p4+sin_16_365p25+cos_16_365p25+sin_18_7+cos_18_7+sin_18_30p4+cos_18_30p4+sin_18_365p25+cos_18_365p25+sin_20_7+cos_20_7+sin_20_30p4+cos_20_30p4+sin_20_365p25+cos_20_365p25+sin_22_7+cos_22_7+sin_22_30p4+cos_22_30p4+sin_22_365p25+cos_22_365p25+sin_24_7+cos_24_7+sin_24_30p4+cos_24_30p4+sin_24_365p25+cos_24_365p25+sin_26_7+cos_26_7+sin_26_30p4+cos_26_30p4+sin_26_365p25+cos_26_365p25+sin_28_7+cos_28_7+sin_28_30p4+cos_28_30p4+sin_28_365p25+cos_28_365p25+sin_30_7+cos_30_7+sin_30_30p4+cos_30_30p4+sin_30_365p25+cos_30_365p25+sin_32_7+cos_32_7+sin_32_30p4+cos_32_30p4+sin_32_365p25+cos_32_365p25+sin_34_7+cos_34_7+sin_34_30p4+cos_34_30p4+sin_34_365p25+cos_34_365p25+sin_36_7+cos_36_7+sin_36_30p4+cos_36_30p4+sin_36_365p25+cos_36_365p25+sin_38_7+cos_38_7+sin_38_30p4+cos_38_30p4+sin_38_365p25+cos_38_365p25+sin_40_7+cos_40_7+sin_40_30p4+cos_40_30p4+sin_40_365p25+cos_40_365p25




#
# # TIDY VERSION
# #
# library(dplyr)
# library(purrr)
# library(tidyr)
# 
# # Function to create Fourier series columns
# create_fourier_columns <- function(data, time_col) {
#   
#   # Define the frequency and harmonic values
#   k_frequencies <- seq(2, 6, 2)  # k values: 2, 4, 6, 8, 10
#   m_seasonal_period_harmonics <- c(7, 30.4, 365.25)  # m values for seasonal periods
#   
#   # Apply Fourier transform for each value in `time_col`
#   data %>%
#     mutate(
#       # Apply Fourier transformation row by row
#       time_fourier = map(.data[[time_col]], function(time_val) {
#         # Generate a tibble for sin and cos for each combination of k and m
#         map_dfc(k_frequencies, function(k) {
#           map_dfc(m_seasonal_period_harmonics, function(m) {
#             tibble(
#               !!paste0("sin_", k, "_", gsub(pattern = "\\.", replacement = "p", m)) := sin(k * pi * time_val / m),
#               !!paste0("cos_", k, "_", gsub(pattern = "\\.", replacement = "p", m)) := cos(k * pi * time_val / m)
#             )
#           })
#         })
#       })
#     ) %>%
#     unnest_wider(time_fourier)  # Unnest the list columns into separate columns
# }
# 
# # Example usage with sample data
# df <- data.frame(time = 1:10)
# 
# # Apply the function to the data
# df <- create_fourier_columns(df, "time")
# #

# View the resulting data frame
# 
paste0(colnames(df)[-1], collapse = "+")
#

#
#
library(dplyr)
library(purrr)
library(tidyr)

# Function to create Fourier series columns
create_fourier_columns <- function(data, time_col) {
  
  # Define the frequency and harmonic values
  k_frequencies <- seq(2, 10, 2)  # k values: 2, 4, 6, 8, 10
  m_seasonal_period_harmonics <- c(7, 30.4, 365.25)  # m values for seasonal periods
  
  # Apply Fourier transform for each value in `time_col`
  data %>%
    mutate(
      # Apply Fourier transformation row by row
      time_fourier = map(.data[[time_col]], function(time_val) {
        # Generate a data frame for sin and cos for each combination of k and m
        map_dfc(k_frequencies, function(k) {
          map_dfc(m_seasonal_period_harmonics, function(m) {
            # Create clean column names for sin and cos
            sin_colname <- paste0("sin_", k, "_", gsub(pattern = "\\.", replacement = "p", m))
            cos_colname <- paste0("cos_", k, "_", gsub(pattern = "\\.", replacement = "p", m))
            
            tibble(
              #!!sin_colname := 
                sin(k * pi * time_val / m),
              #!!cos_colname := 
              cos(k * pi * time_val / m)
            ) |> 
              setNames(c(sin_colname, cos_colname))
          })
        })
      })
    ) %>%
    unnest_wider(time_fourier)  # Unnest the list columns into separate columns
}

# Example usage with sample data
df <- data.frame(time = 1:10)

# Apply the function to the data
df <- create_fourier_columns(df, "time")

# View the resulting data frame
head(df)
##
#
# mutate(
#   # Apply Fourier transformation row by row
#   time_fourier = map(.data[["day_year"]], function(time_val) {
#     # Generate a data frame for sin and cos for each combination of k and m
#     map_dfc(seq(1, 20, 1), function(k) { #frequencies k
#       map_dfc(c(7, 30.4, 365.25/4, 365.25/2, 365.25, 365.25*2, 365.25*4), function(m) { #m_seasonal_period_harmonics 
#         # Create clean column names for sin and cos
#         sin_colname <- paste0("sin_", k, "_", gsub(pattern = "\\.", replacement = "p", m))
#         cos_colname <- paste0("cos_", k, "_", gsub(pattern = "\\.", replacement = "p", m))
#         
#         tibble(
#           #!!sin_colname := 
#           sin(k * pi * time_val / m),
#           #!!cos_colname := 
#           cos(k * pi * time_val / m)
#         ) |> 
#           setNames(c(sin_colname, cos_colname))
#       })
#     })
#   })
# ) |>
# unnest_wider(time_fourier) 
# mutate(
#   si_n = sin(2*pi*day_year/7),
#   co_s = cos(2*pi*day_year/7),
#   si_n4 = sin(2*pi*day_year/30.4),
#   co_s4 = cos(2*pi*day_year/30.4),
#   si_n6 = sin(2*pi*day_year/365.25/4),
#   co_s6 = cos(2*pi*day_year/365.25/4),
#   si_n8 = sin(2*pi*day_year/365.25/2),
#   co_s8 = cos(2*pi*day_year/365.25/2),
#   si_n12 = sin(2*pi*day_year/365.25),
#   co_s12 = cos(2*pi*day_year/365.25),
#   si_n24 = sin(2*pi*day_year/365.25*2),
#   co_s24 = cos(2*pi*day_year/365.25*2),
#   si_n1 = sin(4*pi*day_year/7),
#   co_s1 = cos(4*pi*day_year/7),
#   si_n41 = sin(4*pi*day_year/30.4),
#   co_s41 = cos(4*pi*day_year/30.4),
#   si_n61 = sin(4*pi*day_year/365.25/4),
#   co_s61 = cos(4*pi*day_year/365.25/4),
#   si_n81 = sin(4*pi*day_year/365.25/2),
#   co_s81 = cos(4*pi*day_year/365.25/2),
#   si_n121 = sin(4*pi*day_year/365.25),
#   co_s121 = cos(4*pi*day_year/365.25),
#   si_n241 = sin(4*pi*day_year/365.25*2),
#   co_s241 = cos(4*pi*day_year/365.25*2),
#   si_n11 = sin(6*pi*day_year/7),
#   co_s11 = cos(6*pi*day_year/7),
#   si_n411 = sin(6*pi*day_year/30.4),
#   co_s411 = cos(6*pi*day_year/30.4),
#   si_n611 = sin(6*pi*day_year/365.25/4),
#   co_s611 = cos(6*pi*day_year/365.25/4),
#   si_n811 = sin(6*pi*day_year/365.25/2),
#   co_s811 = cos(6*pi*day_year/365.25/2),
#   si_n1211 = sin(6*pi*day_year/365.25),
#   co_s1211 = cos(6*pi*day_year/365.25),
#   si_n2411 = sin(6*pi*day_year/365.25*2),
#   co_s2411 = cos(6*pi*day_year/365.25*2),
#   si_n111 = sin(8*pi*day_year/7),
#   co_s111 = cos(8*pi*day_year/7),
#   si_n4111 = sin(8*pi*day_year/30.4),
#   co_s4111 = cos(8*pi*day_year/30.4),
#   si_n6111 = sin(8*pi*day_year/365.25/4),
#   co_s6111 = cos(8*pi*day_year/365.25/4),
#   si_n8111 = sin(8*pi*day_year/365.25/2),
#   co_s8111 = cos(8*pi*day_year/365.25/2),
#   si_n12111 = sin(8*pi*day_year/365.25),
#   co_s12111 = cos(8*pi*day_year/365.25),
#   si_n24111 = sin(8*pi*day_year/365.25*2),
#   co_s24111 = cos(8*pi*day_year/365.25*2),
#   si_n1111 = sin(10*pi*day_year/7),
#   co_s1111 = cos(10*pi*day_year/7),
#   si_n41111 = sin(10*pi*day_year/30.4),
#   co_s41111 = cos(10*pi*day_year/30.4),
#   si_n61111 = sin(10*pi*day_year/365.25/4),
#   co_s61111 = cos(10*pi*day_year/365.25/4),
#   si_n81111 = sin(10*pi*day_year/365.25/2),
#   co_s81111 = cos(10*pi*day_year/365.25/2),
#   si_n121111 = sin(10*pi*day_year/365.25),
#   co_s121111 = cos(10*pi*day_year/365.25),
#   si_n241111 = sin(10*pi*day_year/365.25*2),
#   co_s241111 = cos(10*pi*day_year/365.25*2),
#   si_n11111 = sin(12*pi*day_year/7),
#   co_s11111 = cos(12*pi*day_year/7),
#   si_n411111 = sin(12*pi*day_year/30.4),
#   co_s411111 = cos(12*pi*day_year/30.4),
#   si_n611111 = sin(12*pi*day_year/365.25/4),
#   co_s611111 = cos(12*pi*day_year/365.25/4),
#   si_n811111 = sin(12*pi*day_year/365.25/2),
#   co_s811111 = cos(12*pi*day_year/365.25/2),
#   si_n1211111 = sin(12*pi*day_year/365.25),
#   co_s1211111 = cos(12*pi*day_year/365.25),
#   si_n2411111 = sin(12*pi*day_year/365.25*2),
#   co_s2411111 = cos(12*pi*day_year/365.25*2),
#   si_n111111 = sin(14*pi*day_year/7),
#   co_s111111 = cos(14*pi*day_year/7),
#   si_n4111111 = sin(14*pi*day_year/30.4),
#   co_s4111111 = cos(14*pi*day_year/30.4),
#   si_n6111111 = sin(14*pi*day_year/365.25/4),
#   co_s6111111 = cos(14*pi*day_year/365.25/4),
#   si_n8111111 = sin(14*pi*day_year/365.25/2),
#   co_s8111111 = cos(14*pi*day_year/365.25/2),
#   si_n12111111 = sin(14*pi*day_year/365.25),
#   co_s12111111 = cos(14*pi*day_year/365.25),
#   si_n24111111 = sin(14*pi*day_year/365.25*2),
#   co_s24111111 = cos(14*pi*day_year/365.25*2),
#   si_n1111111 = sin(16*pi*day_year/7),
#   co_s1111111 = cos(16*pi*day_year/7),
#   si_n41111111 = sin(16*pi*day_year/30.4),
#   co_s41111111 = cos(16*pi*day_year/30.4),
#   si_n61111111 = sin(16*pi*day_year/365.25/4),
#   co_s61111111 = cos(16*pi*day_year/365.25/4),
#   si_n81111111 = sin(16*pi*day_year/365.25/2),
#   co_s81111111 = cos(16*pi*day_year/365.25/2),
#   si_n121111111 = sin(16*pi*day_year/365.25),
#   co_s121111111 = cos(16*pi*day_year/365.25),
#   si_n241111111 = sin(16*pi*day_year/365.25*2),
#   co_s241111111 = cos(16*pi*day_year/365.25*2),
#   si_n11111111 = sin(18*pi*day_year/7),
#   co_s11111111 = cos(18*pi*day_year/7),
#   si_n411111111 = sin(18*pi*day_year/30.4),
#   co_s411111111 = cos(18*pi*day_year/30.4),
#   si_n611111111 = sin(18*pi*day_year/365.25/4),
#   co_s611111111 = cos(18*pi*day_year/365.25/4),
#   si_n811111111 = sin(18*pi*day_year/365.25/2),
#   co_s811111111 = cos(18*pi*day_year/365.25/2),
#   si_n1211111111 = sin(18*pi*day_year/365.25),
#   co_s1211111111 = cos(18*pi*day_year/365.25),
#   si_n2411111111 = sin(18*pi*day_year/365.25*2),
#   co_s2411111111 = cos(18*pi*day_year/365.25*2),
#   si_n111111111 = sin(20*pi*day_year/7),
#   co_s111111111 = cos(20*pi*day_year/7),
#   si_n4111111111 = sin(20*pi*day_year/30.4),
#   co_s4111111111 = cos(20*pi*day_year/30.4),
#   si_n6111111111 = sin(20*pi*day_year/365.25/4),
#   co_s6111111111 = cos(20*pi*day_year/365.25/4),
#   si_n8111111111 = sin(20*pi*day_year/365.25/2),
#   co_s8111111111 = cos(20*pi*day_year/365.25/2),
#   si_n12111111111 = sin(20*pi*day_year/365.25),
#   co_s12111111111 = cos(20*pi*day_year/365.25),
#   si_n24111111111 = sin(20*pi*day_year/365.25*2),
#   co_s24111111111 = cos(20*pi*day_year/365.25*2),
#   si_n1111111111 = sin(21*pi*day_year/7),
#   co_s1111111111 = cos(21*pi*day_year/7),
#   si_n41111111111 = sin(21*pi*day_year/30.4),
#   co_s41111111111 = cos(21*pi*day_year/30.4),
#   si_n61111111111 = sin(21*pi*day_year/365.25/4),
#   co_s61111111111 = cos(21*pi*day_year/365.25/4),
#   si_n81111111111 = sin(21*pi*day_year/365.25/2),
#   co_s81111111111 = cos(21*pi*day_year/365.25/2),
#   si_n121111111111 = sin(21*pi*day_year/365.25),
#   co_s121111111111 = cos(21*pi*day_year/365.25),
#   si_n241111111111 = sin(21*pi*day_year/365.25*2),
#   co_s241111111111 = cos(21*pi*day_year/365.25*2)
# )
