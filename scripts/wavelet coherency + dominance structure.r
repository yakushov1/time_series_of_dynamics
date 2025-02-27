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

rm(paths, read_files)

data |> 
  filter(Year < 2000, Bank == "RB") |> 
  select(-Bank) |> 
  mutate(Year = as.Date(as.character(Year), format = "%Y"))

w <- analyze.coherency(total, c(paste("LB_",name, sep = ''), paste("RB_",name, sep = '')),
                       loess.span=0, 
                       upperPeriod = 5,
                       lowerPeriod=2, 
                       make.pval=T, n.sim=10)
w2 <- data.frame(Power = w$Power.xy.avg,
                 Period = w$Period,
                 Spec = name)


# Functions ---------------------------------------------------------------

# Cross-wavelet
  # Input: 
    # L - table with data from left bank
    # R - table with data from right bank
  # Spec - vector with species name
  # Output
    # Cross-wavelet graph will be saved in the working directory
    # Average cross-wavelet will be saved as dataframe
wavelet_coherency <- function(L,R,Spec){
# data frame preparation
# set directory for saving
  if (L[1,1] > 1999 & R[1,1] > 1999){
    cat <- "../images/spectrograms/XXI" 
  } 
  else {
    if (L[1,1] < 1999 & R[1,1] < 1999){
      cat <- "../images/spectrograms/XX"
    }
    else {warning("Error! Tables are from different period!")}
  }
  
  
  
  # add prefix to each column
  L <- L |> 
    rename_with(~paste0("LB_", .x), .cols=-1)
  R <- R |> 
    rename_with(~paste0("RB_", .x), .cols=-1)
  total <- full_join(L,R) |> 
    mutate(Year = as.Date(as.character(Year), format = "%Y"))
	
	
	# cross-wavelet calculation and plotting 
  # plot will be saved in the images directory with the name corresponding to the column
	# Average wavelet cross-spectrum  will be saved in df_average
  df_average <- data.frame()
  
  pl <- function(name){
    w <- analyze.coherency(total, c(paste("LB_",name, sep = ''), paste("RB_",name, sep = '')),
                          loess.span=0, 
                          upperPeriod = 5,
                          lowerPeriod=2, 
                          make.pval=T, n.sim=10)
    w2 <- data.frame(Power = w$Power.xy.avg,
                     Period = w$Period,
                     Spec = name)
    
  png(filename = paste(cat, '/', name,  '.png', sep = ''), width = 1430, height = 870, pointsize = 15, res = 100)
  wc.image(w, which.image = "wp", plot.coi = T, plot.arrow = F, main = "", siglvl.contour = 0.05, show.date = F,
                  periodlab = '', timelab = '', plot.legend = F, label.time.axis = F, clear.area = F, siglvl.area = 0.05, lwd = 20)
  dev.off()
  return(w2) 
  }
  for (i in Spec){
    new <-  pl(i)
    df_average <- rbind(df_average, new)
  }
return(df_average)
} 

# Plotting average cross-wavelet from output of wavelet_coherency function 
  # Input:
      # Setting:     element_text(size = 14, colour = "black") or other 
      # df:   dataframe from output of wavelet_coherency function
plot_average_wavelet_coherence <- function(df, setting){
  df$Spec <- gsub('_', ' ', df$Spec)
  
  ggplot(df, aes(Period, Power))+
    geom_col()+
    facet_grid(Spec~., scales = "free_x", switch = "y")+
    xlab('Period')+
    ylab('Power')+
    coord_flip()+
    theme(text=element_text(family="sans"),
          axis.title.x = element_text(size = 14, colour = "black"),
          axis.title.y = element_text(size = 14, colour = "black"),
          axis.text.x = element_text(size = 14,angle = 90, vjust = .5, colour = "black"),
          axis.text.y = element_text(size = 14, colour = "black"),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.border = element_rect(colour = "black", fill=NA),
          strip.background = element_rect(fill = "transparent"),
          strip.text = setting,
          strip.placement = "outside"
    ) 
}

# Graph of numbers
  # Input
    # L_XX - numbers of small mammals on the  left bank in 20th century
    # L_XXI - numbers of small mammals on the  left bank in 21st century
    # R_XX - numbers of small mammals on the  right bank in 20th century
    # R_XXI - numbers of small mammals on the  right bank in 21st century
    # Spec_for_plot - vector with species name
  # Output
    # Graph of numbers grouped by species and century. Lines show different banks
num_both_bank <- function(L_XX, R_XX,L_XXI, R_XXI, Spec_for_plot){
  df <- L_XX%>%
    mutate(Bank = "L", Period = "XX")
  df2 <- R_XX%>%
    mutate(Bank = "R", Period = "XX")
  
  df3 <- L_XXI%>%
    mutate(Bank = "L", Period = "XXI")
  df4 <- R_XXI%>%
    mutate(Bank = "R", Period = "XXI")
  
  data <- bind_rows(df,df2, df3, df4)%>%
    pivot_longer(cols = c(-date, -Bank, - Period), names_to = "Spec", values_to = "Num")%>%
    filter(Spec %in% Spec_for_plot)

  data$Spec <- gsub('_', ' ', data$Spec)
  
  p <- ggplot(data, aes(date, Num, linetype = Bank))+
    geom_line()+
    geom_point()+
    facet_grid(Spec~Period, scales = "free_x", switch = "y")+
    xlab('Year')+
    ylab('Numbers')+
    labs(linetype = "Bank")+
    scale_x_continuous(breaks = c(1976,1980,1985,1990,1994, 2008,2013,2017, 2023))+
    #theme_bw()+
    theme(text=element_text(family="sans"),
          axis.title.x = element_text(size = 14, colour = "black"),
          axis.title.y = element_text(size = 13, colour = "black"),
          axis.text.x = element_text(size = 14,angle = 90, vjust = .5, colour = "black"),
          axis.text.y = element_text(size = 14, colour = "black"),
          plot.title = element_text(vjust = -6, size = 18),
          legend.text = element_text(size = 14, colour = "black"),
          legend.title = element_text(size = 14), 
          legend.background = element_blank(),
          legend.box.background = element_rect(fill = "transparent"),
          legend.position = c("bottom"),
          legend.key = element_rect(fill = "transparent", colour = "transparent"),
          panel.background = element_rect(fill = "transparent"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.border = element_rect(colour = "black", fill=NA),
          strip.background = element_rect(fill = "transparent"),
          strip.text = element_blank(),
          strip.placement = "outside")
          
  return(p)
  
}

# Dominance Structure Calculation (proportion of each species)
  # Input
    # L_XX - numbers of small mammals on the  left bank in 20th century
    # L_XXI - numbers of small mammals on the  left bank in 21st century
    # R_XX - numbers of small mammals on the  right bank in 20th century
    # R_XXI - numbers of small mammals on the  right bank in 21st century
  # Output
    # Dataframe, the proportion of each species in the community
dominant <- function(L_XX, R_XX,L_XXI, R_XXI){
  name <- c("S._araneus", "S._caecutiens", 
            "S._isodon",	"S._minutus",		"S._roboratus",	"S._tundrensis",
            "M._oeconomus","C._rufocanus", "C._rutilus",	"M._agrestis",	
            "M._schisticolor",	"S._betulina")
  com_L_XX <- LB_XX%>%
    select(date,Community)%>%
    mutate(Bank = "L", Period = "XX")
  com_R_XX <- R_XX%>%
    select(date,Community)%>%
    mutate(Bank = "R", Period = "XX")
  
  com_L_XXI <- L_XXI%>%
    select(date,Community)%>%
    mutate(Bank = "L", Period = "XXI")
  com_R_XXI <- R_XXI%>%
    select(date,Community)%>%
    mutate(Bank = "R", Period = "XXI")
  community <- bind_rows(com_L_XX, com_R_XX, com_L_XXI, com_R_XXI)
  
  df <- L_XX%>%
    mutate(Bank = "L", Period = "XX")%>%
    select( -Community, -Com_without_ara, -Without_ara_iso_cae, -"Без доминантов")
  df2 <- R_XX%>%
    mutate(Bank = "R", Period = "XX")%>%
    select( -Community, -Com_without_ara, -Without_ara_iso_cae, -"Без доминантов")
  
  df3 <- L_XXI%>%
    mutate(Bank = "L", Period = "XXI")%>%
    select( -Community, -"Без доминантов")
  df4 <- R_XXI%>%
    mutate(Bank = "R", Period = "XXI")%>%
    select( -Community, -"Без доминантов")
  
  data <- bind_rows(df,df2, df3, df4)%>%
    pivot_longer(cols = c(-date, -Bank, - Period), names_to = "Spec", values_to = "Num")%>%
    left_join(community, by = c("date", "Bank", "Period"))%>%
    mutate(Dom = Num/Community)%>%
    mutate_if(is.numeric, round, digits=2)%>%
    arrange(date, Bank, desc(Dom))%>%
    filter(Spec %in% name)
  
  
  data$Spec <- gsub('_', ' ', data$Spec)
  data <- data %>%
    mutate(Spec = factor(Spec, levels=c("M. agrestis",  "M. schisticolor",
                                        "S. betulina", "M. oeconomus",
                                        "C. rufocanus",  "C. rutilus",
                                        "S. roboratus","S. isodon","S. minutus",
                                        "S. tundrensis","S. caecutiens",
                                        "S. araneus"))) %>%
    filter(is.na(Spec) == F)
  
  data$Dom[data$Dom == 0] <- NA
  return(data)
  
}


#

# Results -----------------------------------------------------------------

# Calculating of cross-wavelet for shrews
shrew_XX <- wavelet_coherency(LB_XX, RB_XX,Shrew_name)
shrew_XXI <- wavelet_coherency(LB_XXI, RB_XXI,Shrew_name)

# Graphs for shrews
plot_shrew <- num_both_bank(LB_XX, RB_XX,LB_XXI, RB_XXI, Shrew_name)
pl1 <- plot_average_wavelet_coherence(shrew_XX, element_text(size = 14, colour = "black"))
pl2 <- plot_average_wavelet_coherence(shrew_XXI, element_blank())

p <- pl1 + plot_shrew + pl2 +
  plot_layout(widths = c(1,3.5,1))+
  plot_annotation(tag_levels = 'A') &
  theme(plot.background = element_rect(fill = "transparent", color = NA))

ggsave(device = png, filename = 'Shrew.png',
       plot = p, bg ='transparent', width = 2481, height = 3000, units = "px" )


# Calculating of cross-wavelet  for rodents
rodent_XX <- wavelet_coherency(LB_XX, RB_XX,Rodent_name)
rodent_XXI <- wavelet_coherency(LB_XXI, RB_XXI,Rodent_name)
# Graphs for rodents
plot_rodent <- num_both_bank(LB_XX, RB_XX,LB_XXI, RB_XXI, Rodent_name)
pl3 <- plot_average_wavelet_coherence(rodent_XX, element_text(size = 14, colour = "black"))
pl4 <- plot_average_wavelet_coherence(rodent_XXI, element_blank())

p_rodent <- pl3 + plot_rodent + pl4 +
  plot_layout(widths = c(1,5,1))+
  plot_annotation(tag_levels = 'A') &
  theme(plot.background = element_rect(fill = "transparent", color = NA))
ggsave(device = png, filename = 'Rodent.png',
       plot = p_rodent, bg ='transparent', width = 2481, height = 3000, units = "px" )

# For community
community_XX <- wavelet_coherency(LB_XX, RB_XX, Com)
community_XXI <- wavelet_coherency(LB_XXI, RB_XXI, Com)

plot_community <- num_both_bank(LB_XX, RB_XX,LB_XXI, RB_XXI, Com)
pl5 <- plot_average_wavelet_coherence(community_XX, element_blank())
pl6 <- plot_average_wavelet_coherence(community_XXI, element_blank())

p_comm <- pl5 + plot_community + pl6 +
  plot_layout(widths = c(1,4,1))+
  plot_annotation(tag_levels = 'A') &
  theme(plot.background = element_rect(fill = "transparent", color = NA))

ggsave(device = png, filename = 'Community.png',
       plot = p_comm, bg ='transparent', width = 2481, height = 1150, units = "px" )



# The spectrograms saved as images were added on the background in Photoshop

# calculation of the proportion of species in the community and plotting

b <- dominant(LB_XX, RB_XX, LB_XXI, RB_XXI)
pl <- ggplot(b, aes(date,Spec, fill = Dom ))+
  geom_tile()+
  facet_grid(Bank ~ Period, scales = "free_x")+
  scale_fill_gradientn(colours = c("LightCyan", "Pink","Red"),
                       breaks = c(0.2,0.4,0.6),
                       na.value = "white")+
  ylab('')+
  xlab('')+
  labs(fill = "Propotion")+
  scale_x_continuous(breaks = c(1976,1980,1985,1990,1994, 2008,2013,2017, 2023))+
  theme(text=element_text(family="sans"),
        axis.title.x = element_text(size = 14, colour = "black"),
        axis.title.y = element_text(size = 14, colour = "black"),
        axis.text.x = element_text(size = 14,angle = 90, vjust = .5, colour = "black"),
        axis.text.y = element_text(size = 14, colour = "black"),
        legend.text = element_text(size = 14, colour = "black"),
        legend.title = element_text(size = 14),
        strip.text = element_text(size = 14),
        legend.background = element_blank(),
        legend.box.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA),
        strip.background = element_rect(fill = "transparent"),
        panel.border = element_rect(colour = "black", fill=NA))
        
ggsave(device = png, filename = 'dominant.png',
       plot = pl, bg ='transparent', width = 2481, height = 1900, units = "px" )