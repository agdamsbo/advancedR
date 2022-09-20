library(tidyverse)

#
# fertilityplots
# 

fertilityData <- structure(list(Country = structure(c(2L, 5L, 6L, 
                                                      9L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L), .Label = c("Abkhazia", 
                                                                                                              "Afghanistan", "Akrotiri and Dhekelia", "Åland", "Albania", 
                                                                                                              "Algeria", "American Samoa", "Andorra", "Angola", "Anguilla", 
                                                                                                              "Antigua and Barbuda", "Argentina", "Armenia", "Aruba", "Australia", 
                                                                                                              "Austria", "Azerbaijan", "Bahamas", "Bahrain", "Bangladesh", 
                                                                                                              "Barbados", "Belarus", "Belgium", "Belize", "Benin", "Bermuda", 
                                                                                                              "Bhutan", "Bolivia", "Bosnia and Herzegovina", "Botswana", "Brazil", 
                                                                                                              "British Virgin Islands", "Brunei", "Bulgaria", "Burkina Faso", 
                                                                                                              "Burundi", "Cambodia", "Cameroon", "Canada", "Cape Verde", "Cayman Islands", 
                                                                                                              "Central African Republic", "Chad", "Channel Islands", "Chile", 
                                                                                                              "China", "Christmas Island", "Cocos Island", "Colombia", "Comoros", 
                                                                                                              "Congo, Dem. Rep.", "Congo, Rep.", "Cook Is", "Costa Rica", "Cote d'Ivoire", 
                                                                                                              "Croatia", "Cuba", "Cyprus", "Czech Republic", "Czechoslovakia", 
                                                                                                              "Denmark", "Djibouti", "Dominica", "Dominican Republic", "East Germany", 
                                                                                                              "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", "Eritrea", 
                                                                                                              "Eritrea and Ethiopia", "Estonia", "Ethiopia", "Faeroe Islands", 
                                                                                                              "Falkland Is (Malvinas)", "Fiji", "Finland", "France", "French Guiana", 
                                                                                                              "French Polynesia", "Gabon", "Gambia", "Georgia", "Germany", 
                                                                                                              "Ghana", "Gibraltar", "Greece", "Greenland", "Grenada", "Guadeloupe", 
                                                                                                              "Guam", "Guatemala", "Guernsey", "Guinea", "Guinea-Bissau", "Guyana", 
                                                                                                              "Haiti", "Holy See", "Honduras", "Hong Kong, China", "Hungary", 
                                                                                                              "Iceland", "India", "Indonesia", "Iran", "Iraq", "Ireland", "Isle of Man", 
                                                                                                              "Israel", "Italy", "Jamaica", "Japan", "Jersey", "Jordan", "Kazakhstan", 
                                                                                                              "Kenya", "Kiribati", "Kosovo", "Kuwait", "Kyrgyz Republic", "Lao", 
                                                                                                              "Latvia", "Lebanon", "Lesotho", "Liberia", "Libya", "Liechtenstein", 
                                                                                                              "Lithuania", "Luxembourg", "Macao, China", "Macedonia, FYR", 
                                                                                                              "Madagascar", "Malawi", "Malaysia", "Maldives", "Mali", "Malta", 
                                                                                                              "Marshall Islands", "Martinique", "Mauritania", "Mauritius", 
                                                                                                              "Mayotte", "Mexico", "Micronesia, Fed. Sts.", "Moldova", "Monaco", 
                                                                                                              "Mongolia", "Montenegro", "Montserrat", "Morocco", "Mozambique", 
                                                                                                              "Myanmar", "Namibia", "Nauru", "Nepal", "Netherlands", "Netherlands Antilles", 
                                                                                                              "New Caledonia", "New Zealand", "Ngorno-Karabakh", "Nicaragua", 
                                                                                                              "Niger", "Nigeria", "Niue", "Norfolk Island", "North Korea", 
                                                                                                              "North Yemen (former)", "Northern Cyprus", "Northern Mariana Islands", 
                                                                                                              "Norway", "Oman", "Pakistan", "Palau", "Panama", "Papua New Guinea", 
                                                                                                              "Paraguay", "Peru", "Philippines", "Pitcairn", "Poland", "Portugal", 
                                                                                                              "Puerto Rico", "Qatar", "Reunion", "Romania", "Russia", "Rwanda", 
                                                                                                              "Samoa", "San Marino", "Sao Tome and Principe", "Saudi Arabia", 
                                                                                                              "Senegal", "Serbia", "Serbia and Montenegro", "Serbia excluding Kosovo", 
                                                                                                              "Seychelles", "Sierra Leone", "Singapore", "Slovak Republic", 
                                                                                                              "Slovenia", "Solomon Islands", "Somalia", "Somaliland", "South Africa", 
                                                                                                              "South Korea", "South Ossetia", "South Yemen (former)", "Spain", 
                                                                                                              "Sri Lanka", "St. Barthélemy", "St. Helena", "St. Kitts and Nevis", 
                                                                                                              "St. Lucia", "St. Martin", "St. Vincent and the Grenadines", 
                                                                                                              "St.-Pierre-et-Miquelon", "Sudan", "Suriname", "Svalbard", "Swaziland", 
                                                                                                              "Sweden", "Switzerland", "Syria", "Taiwan", "Tajikistan", "Tanzania", 
                                                                                                              "Thailand", "Timor-Leste", "Togo", "Tokelau", "Tonga", "Transnistria", 
                                                                                                              "Trinidad and Tobago", "Tunisia", "Turkey", "Turkmenistan", "Turks and Caicos Islands", 
                                                                                                              "Tuvalu", "Uganda", "Ukraine", "United Arab Emirates", "United Kingdom", 
                                                                                                              "United Korea (former)\n", "United States", "Uruguay", "USSR", 
                                                                                                              "Uzbekistan", "Vanuatu", "Venezuela", "Vietnam", "Virgin Islands (U.S.)", 
                                                                                                              "Wallis et Futuna", "West Bank and Gaza", "West Germany", "Western Sahara", 
                                                                                                              "Yemen", "Yugoslavia", "Zambia", "Zimbabwe"), class = "factor"), 
                                `1800` = c(7, 4.6, 6.99, 6.93, 5, 6.8, 7.8, 5.64, 6.5, 5.1, 
                                           8.1, 5.9), `1801` = c(7, 4.6, 6.99, 6.93, 5, 6.8, 7.8, 5.64, 
                                                                 6.48, 5.1, 8.1, 5.9), `1802` = c(7, 4.6, 6.99, 6.93, 4.99, 
                                                                                                  6.8, 7.81, 5.64, 6.46, 5.1, 8.1, 5.9), `1803` = c(7, 4.6, 
                                                                                                                                                    6.99, 6.93, 4.99, 6.8, 7.81, 5.64, 6.44, 5.1, 8.1, 5.9), 
                                `1804` = c(7, 4.6, 6.99, 6.93, 4.99, 6.8, 7.81, 5.64, 6.42, 
                                           5.1, 8.1, 5.9), `1805` = c(7, 4.6, 6.99, 6.93, 4.98, 6.8, 
                                                                      7.82, 5.64, 6.4, 5.1, 8.1, 5.9)), .Names = c("Country", 
                                                                                                                   "1800", "1801", "1802", "1803", "1804", "1805"), row.names = c(NA, 
                                                                                                                                                                                  -12L), class = c("tbl_df", "tbl", "data.frame"))

(fertilityTidy <- fertilityData %>%
    tidyr::pivot_longer(cols = -Country, names_to = "Year",
                        values_to = "Fertility") %>%
    mutate(Year = as.integer(Year)))

ggplot(fertilityTidy, aes(Year, Fertility, color = Country)) + 
  geom_point() +
  geom_line()

#
#

head(iris)
library(ggplot2)
library(viridis)
theme_set(theme_bw(18))

iris |> # Data set
  pivot_longer(cols = -Species) |> # Pivot to long
  ggplot() + # base plot
  geom_density(aes(value, fill = Species), alpha = 0.6, linetype=3)+ # Density base plot
  facet_wrap( ~name, nrow=2, scales="free")+ # Facet wrapping with free scales
  theme(aspect.ratio = 0.8, legend.key.width = unit(3, "line"))+ # Setting plot ratio
  scale_fill_viridis(discrete = T) # Auto scales colors for color blind friendliness




