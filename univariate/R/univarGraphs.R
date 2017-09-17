#' @title A univariate graph function
#'
#' @description  A univariate function to generate graphs for categorical variables
#'
#' @param ip.data is your dataframe with data
#' @param cat_var is the column name of your categorical variable
#' @param plot.name the name of the plot u want to see where a = bar chart
#' b = horizontal bar , c = pie chart , d = point
#'
#' @keywords A basic plot will be returned in this function
#'
#' @export univariateCategorical
#'
#' @examples univariateCategorical(iris,iris$Species,"a")
#'

univariateCategorical<- function(ip.data, cat_var, plot.name)
{

  value<- cat_var
  plot.name <- as.character(plot.name)

  if (plot.name == "a")
  {
    # Creating a bar Graph
    uni_plot <-  ggplot(data = ip.data,
                        aes(x = value,fill= value)) +
      geom_bar()

  }else if (plot.name == "b")
  {
    # Creating a Horizontal Bar graph
    uni_plot <- ggplot(
      data = ip.data,
      aes(x = value,fill=value)) +
      geom_bar() +
      coord_flip()
  }else if (plot.name == "c")
  {

    # Pie chart
    uni_plot <- ggplot(
      data = ip.data,
      aes(x = "", fill = value)) +
      geom_bar() +
      coord_polar(theta = "y") +
      xlab("")+
      ylab("")
  }else if (plot.name == "d")
  {
    # Create cleveland dot plot
    uni_plot <- ggplot(
      data = ip.data,
      aes(x = value)) +
      geom_point(stat = "count")
  }

  return(uni_plot)

}

#' @title A univariate graph function
#'
#' @description  A univariate function to generate graphs for Numerical variables
#'
#' @param ip.data is your dataframe with data
#' @param cat_var is the column name of your numerical variable
#' @param plot.name the name of the plot u want to see where a = density plot
#' b = box plot , c = histogram , d = jitter
#'
#' @keywords A basic plot will be returned in this function
#'
#' @export univariateNumerical
#'
#' @examples univariateNumerical(mtcars,mtcars$mpg,"a")
#'

univariateNumerical <- function(ip.data, num_var, plot.name)
{
  value<- num_var
  plot.name <- as.character(plot.name)

  if (plot.name == "a")
  {
    # Create density plot with ggplot
    uni_num_plot <- ggplot(
      data = ip.data,
      aes(x = value)) +
      geom_density()
  }else if(plot.name == "b")
  {
    # Create box plot of runtime
    uni_num_plot <- ggplot(
      data = ip.data,
      aes(x = "", y = value)) +
      geom_boxplot()

  }else if(plot.name == "c")
  {
    # Create histogram with ggplot
    uni_num_plot <- ggplot(
      data = ip.data,
      aes(x = value)) +
      geom_histogram(binwidth = 10)

  }else if(plot.name == "d")
  {

    # Create dot plot with jitter
    uni_num_plot <- stripplot(
      x = ~value,
      data = ip.data,
      jitter = TRUE,
      amount = 0.5)

  }


}
