library(WaveletComp)
library(tidyverse)
library(patchwork)

# Import ------------------------------------------------------------------
# Attention! The data is not original, noise has been added to it!
# The actual publication uses different dataset, but the general dynamics are the same.

# set species names
shrew_name <- c("S._araneus", "S._caecutiens", 
                "S._isodon",	"S._minutus",		"S._roboratus",	"S._tundrensis") # Species of shrews
rodent_name <- c("M._oeconomus","C._rufocanus",
                 "C._rutilus",	"M._agrestis",	
                 "M._schisticolor",	"S._betulina") # Species of rodent

# read the path to the file
paths <- list.files("initial_data/", full.names = T)

# read data into data.frame (not tibble!!!), select column in right order, add column with 
# info about bank of river

read_files <- function(path){
  read.csv2(path) |>
    select(any_of(c("Year", shrew_name, rodent_name, "Community"))) |> 
    mutate(Bank = substr(path[1],14,15))
}

# combine all files into one data frame
data <- map_dfr(paths, read_files) 

# XX century
XX <- data |> 
  pivot_wider(id_cols = Year, names_from = 'Bank', values_from = c(2:14)) |> 
  filter(Year < 2000) |> 
  mutate(Year = as.Date(as.character(Year), format = "%Y"))

# XXI century
XXI <- data |> 
  pivot_wider(id_cols = Year, names_from = 'Bank', values_from = c(2:14)) |> 
  filter(Year > 2000) |> 
  mutate(Year = as.Date(as.character(Year), format = "%Y"))

rm(paths, read_files)


# Functions ---------------------------------------------------------------

wavelet_coherency_and_average <- function(df, species){
  
  # Input:
  # df - DataFrame (XX or XXI above)
  # species - species name (from rodent_name or shrew_name variables)
  # Output:
  # wavelet coherency spectrogram (to images/XX or images/XXI depending on the year)
  # data frame with average spectrum
  
  # preparing dataset: select only thee columns (Year and with info about selected species)
  t <- df |> 
    select(Year, starts_with(species))
  
  # start analyze coherency
  all_data <- analyze.coherency(t, c(2,3),
                         loess.span=0, 
                         upperPeriod = 5,
                         lowerPeriod=2, 
                         make.pval=T, n.sim=10)
  
  
  # set directory for saving spectrograms
  if (all(year(t$Year)<2000)){
    directory_name <- paste('images/spectrograms/XX/', species, '.png', sep = '')
  }
  
  if (all(year(t$Year)>2000)){
    directory_name <- paste('images/spectrograms/XXI/', species, '.png', sep = '')
  }
  

  # saving coherency spectrogram
  png(filename = directory_name, width = 1430, height = 870, pointsize = 15, res = 100)
  wc.image(all_data,
           which.image = "wp",
           plot.coi = T,
           plot.arrow = F,
           main = "",
           siglvl.contour = 0.05,
           show.date = F,
           periodlab = '',
           timelab = '',
           plot.legend = F,
           label.time.axis = F,
           clear.area = F,
           siglvl.area = 0.05,
           lwd = 20)
  dev.off()
  
  # get average data
  average_wavelet_power <- data.frame(Power = all_data$Power.xy.avg,
                   Period = all_data$Period,
                   Spec = species)
  
  return(average_wavelet_power)
}
  
average_wavelet_power_graph <- function(df){
  # Plotting average cross-wavelet from output of wavelet_coherency_and_average function 
  # Input:
  # df:   dataframe from output of wavelet_coherency_and_average function
  
  ggplot(df, aes(Period, Power))+
  geom_col()+
  facet_grid(Spec~., scales = "free_x", switch = "y")+
  xlab('Period')+
  ylab('Power')+
  coord_flip()+
  theme(text = element_text(size = 14, colour = 'black', family = 'sans'),
        axis.text.x = element_text(angle = 90, vjust = .5),
        strip.placement = "outside")+
  theme_minimal()
  
}

graph_of_numbers <- function(df, species_name){
  
  # Graph of numbers
  # Input
  # df - data variable above
  # Output
  # Graph of numbers grouped by species and century. Lines show different banks
  
  
  df |> 
    select(Year, Bank, any_of(species_name)) |> 
    pivot_longer(cols = !c(Year, Bank), names_to = "Species", values_to = "Numbers") |> 
    mutate(Period = case_when(Year < 2000 ~ "XX",
                              Year > 2000 ~ "XXI")) |> 
    ggplot(aes(Year, Numbers, linetype = Bank))+
    geom_line()+
    facet_grid(Species~Period, scales = "free", switch = "y")+
    scale_x_continuous(breaks = c(1976,1980,1985,1990,1994, 2008,2013,2017, 2023))+
    theme_minimal(base_size = 14, base_family = 'sans')+
    theme(plot.background = element_rect(fill = "transparent", color = NA),
          legend.position = 'bottom',
          axis.text.x = element_text(angle = 90))
} 


# Results -----------------------------------------------------------------

shrew_XX_average <- map_dfr(shrew_name,
                            \(x)wavelet_coherency_and_average(XX, x))

shrew_XXI_average <- map_dfr(shrew_name,
                            \(x)wavelet_coherency_and_average(XXI, x))


rodent_XX_average <- map_dfr(rodent_name,
                            \(x)wavelet_coherency_and_average(XX, x))

rodent_XXI_average <- map_dfr(rodent_name,
                             \(x)wavelet_coherency_and_average(XXI, x))


# Graphs ------------------------------------------------------------------

# Graphs for shrews
numbers_shrew <- graph_of_numbers(data, shrew_name)
average_wavelet_shrew_XX <- average_wavelet_power_graph(shrew_XX_average)
average_wavelet_shrew_XXI <- average_wavelet_power_graph(shrew_XXI_average)+
  theme(strip.text = element_blank())

shrew_average_wavelet_and_numbers <- average_wavelet_shrew_XX + numbers_shrew + average_wavelet_shrew_XXI +
  plot_layout(widths = c(1,3.5,1))+
  plot_annotation(tag_levels = 'A') &
  theme(plot.background = element_rect(fill = "transparent", color = NA))

ggsave(device = png, filename = 'images/numbers/shrew_average_wavelet_and_numbers.png',
       plot = shrew_average_wavelet_and_numbers, bg ='transparent', width = 2481, height = 3000, units = "px" )



# Graphs for rodent
numbers_rodent <- graph_of_numbers(data, rodent_name)
average_wavelet_rodent_XX <- average_wavelet_power_graph(rodent_XX_average)
average_wavelet_rodent_XXI <- average_wavelet_power_graph(rodent_XXI_average)+
  theme(strip.text = element_blank())

rodent_average_wavelet_and_numbers <- average_wavelet_rodent_XX + numbers_rodent + average_wavelet_rodent_XXI +
  plot_layout(widths = c(1,3.5,1))+
  plot_annotation(tag_levels = 'A') &
  theme(plot.background = element_rect(fill = "transparent", color = NA))

ggsave(device = png, filename = 'images/numbers/rodent_average_wavelet_and_numbers.png',
       plot = rodent_average_wavelet_and_numbers, bg ='transparent', width = 2481, height = 3000, units = "px" )



# calculation of the proportion of species in the community and plotting


proportion_graph <- data |> 
  select(!Community) |> 
  pivot_longer(!c(Year, Bank), names_to = "Species", values_to = "Numbers") |> 
  group_by(Year, Bank) |> 
  mutate(Total = sum(Numbers)) |> 
  ungroup() |> 
  mutate(Proportion = Numbers/Total) |> 
  mutate(Period = case_when(Year < 2000 ~ 'XX',
                            Year > 2000 ~ 'XXI')) |> 
  mutate(Species = str_replace(Species, "_", " ")) |> 
  mutate(Species = factor(Species, levels=c("M. agrestis",  "M. schisticolor",
                                      "S. betulina", "M. oeconomus",
                                      "C. rufocanus",  "C. rutilus",
                                      "S. roboratus","S. isodon","S. minutus",
                                      "S. tundrensis","S. caecutiens",
                                      "S. araneus"))) |>
  ggplot(aes(Year,Species, fill = Proportion ))+
  geom_tile()+
  facet_grid(Bank ~ Period, scales = "free_x")+
  scale_fill_gradientn(colours = c("LightCyan", "Pink","Red"),
                       breaks = c(0.2,0.4,0.6),
                       na.value = "white")+
  ylab('')+
  xlab('')+
  labs(fill = "Propotion")+
  scale_x_continuous(breaks = c(1976,1980,1985,1990,1994, 2008,2013,2017, 2023))+
  theme_minimal()

ggsave(device = png, filename = 'images/numbers/proportion_graph.png',
       plot = proportion_graph, bg ='transparent', width = 2481, height = 1900, units = "px" )