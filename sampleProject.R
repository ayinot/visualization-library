# Load Libraries
library(ggplot2)
library(hexbin)
library(lattice)
library(univariate)

# Read the input File
movies <- read.csv("Movies.csv",header = T)

# Set the theme
theme_function <- function(plot)
{
  theme <- plot+ggtitle("Count of Movie Titles by Rating") # Change the Title
  theme <- theme + xlab("") # Change the X label
  theme <- theme + ylab("")  # Change the Y label
  # theme <- theme +ylim(0,250) # Change the Y label
  # theme <- theme +xlim(0,250) # Change the x label+
  return(theme)
}


#### Univariate Categorical ########
plot1 <- univariateCategorical(movies,movies$Rating,"a") # Initial plot
plot <- theme_function(plot1) # Applying theme
ggsave(filename="uni_categorical.jpg", plot=plot)


#### Univariate Numerical ########
plot1 <- univariateNumerical(movies,movies$Runtime,"b") # Initial plot
plot <- theme_function(plot1) # Applying theme
plot
ggsave(filename="uni_numerical.jpg", plot=plot)
