#===============================================================================
# Description: Actract all file names of RPG data on the URL adress
# author: Loic Henry and El Ghali Debbagh
# Contact loic.henry@dauphine.psl.eu
#===============================================================================


# Idea: scrape all url links, and associate region name and year
# Specify the URL of the webpage
url <- "https://geoservices.ign.fr/rpg"

# Read the webpage
webpage <- read_html(url)

# Define the xpath of the block with all downloading links
block_xpath <- '//*[@id="block-ignpro-content"]/div/article/div[2]/div[1]/div[1]/div/div[2]/div[4]/div/div'

# Extract the specific block using the XPath
block <- webpage %>% 
  html_node(xpath = block_xpath)

# Extract the ul elements within the block
ul_elements <- block %>% 
  html_nodes("ul")

# Extract all the URLs from the ul elements
ul_urls <- ul_elements %>% 
  html_nodes("a") %>% 
  html_attr("href")



# Now, create a database of region code and name
region_code_dta <- data.frame(
  region_name = c("Auvergne-Rhône-Alpes",
                  "Bourgogne-Franche-Comté",
                  "Bretagne",
                  "Centre-Val de Loire",
                  "Corse",
                  "Grand Est",
                  "Hauts-de-France",
                  "Île-de-France",
                  "Normandie",
                  "Nouvelle-Aquitaine",
                  "Occitanie",
                  "Pays de la Loire",
                  "Provence-Alpes-Côte d’Azur"),
  region_code = c("R84",
                  "R27",
                  "R53",
                  "R24",
                  "R94",
                  "R44",
                  "R32",
                  "R11",
                  "R28",
                  "R75",
                  "R76",
                  "R52",
                  "R93")
)

# Create dataframe with region and year of all existing files
years <- as.character(c(2007:2022)) # Data exists from 2007 to 2022
rpg_dta_structure <- expand.grid(year = years, region_name = region_code_dta$region_name) %>% # I make all the cartesian combinations of years and region code
  left_join(region_code_dta, by = "region_name") %>% # I add the region name
  dplyr::select(region_name, region_code, year)

# To finally add a variable containing the URL link:
region_codes <- paste(unique(rpg_dta_structure$region_code), 
                      collapse = "|") # Vector to identify region code in the url link
all_years <- paste(unique(rpg_dta_structure$year), 
                   collapse = "|") # Vector to identify year of observation in the url link
url_df <- data.frame(
  url = ul_urls) %>% 
  mutate(region_code = str_extract(url, region_codes), # Associate to each url its region code
         year = str_extract(url, all_years) # Associate to each  url its year
  ) %>% 
  filter(!is.na(region_code))
table(url_df$year)

# Final data: merge to add the url links
all_rpg_links <- url_df %>% 
  left_join(rpg_dta_structure, by = c("region_code","year")) %>% 
  dplyr::select(region_name, region_code, year, url) %>% 
  arrange(year, region_name)

