# Beta Distribution Shiny App

These three documents — Proj1_Rmd_template-copy.pdf, Proj1_Rmd_template-copy.Rmd, and app-copy_beta together form a project centered on understanding and visualizing the Beta distribution using R and Shiny. The R Markdown file and PDF provide background information and documentation of the project, while the R script serves as the main Shiny application.You can move the sliders for alpha and beta and watch the shape of the distribution change in real time, switch between the PDF and CDF, calculate probabilities, and generate random samples.

## Live app 
https://sandraosei.shinyapps.io/Statistical-Modelling-2/

## What it does 
The Shiny app is an interactive tool designed to help users explore how the α (alpha) and β (beta) parameters shape the Beta distribution. Through an intuitive interface built with Shiny, users can adjust these parameters using sliders and immediately see the resulting changes in a dynamically generated plot. The app also allows switching between the Probability Density Function (PDF) and the Cumulative Distribution Function (CDF), giving users a deeper understanding of both perspectives.

Additionally, the app includes a probability slider that enables users to compute or highlight specific probability values on the plot. By combining the power of ggplot2 for visualization and dplyr for data handling, this project effectively bridges theory and interactivity. It serves as a valuable educational resource for anyone seeking to grasp how the Beta distribution behaves under different conditions, making statistical learning  both visual and engaging.
Sliders to adjust alpha and beta (0.1 to 10)

## Features
Toggle between the PDF and the CDF
Mean, variance, standard deviation, and mode update automatically as you change the parameters
Pick a value of x and get P(X ≤ x), P(X > x), and the density at that point, marked on the plot
Generate random samples from the current distribution and see their sample mean and SD

## Why the Beta distribution
The Beta distribution is bounded between 0 and 1, which makes it useful for modeling probabilities or proportions — things like election odds, conversion rates in A/B testing, or the chance a treatment works in a medical trial. It's controlled by two shape parameters, alpha and beta, and depending on their values you get pretty different shapes: uniform, U-shaped, bell-shaped, or skewed one way or the other.

## Running it yourself
You'll need R with a few packages installed:
```{r}
install.packages(c("shiny", "ggplot2", "dplyr"))
shiny::runApp("app.R")
```{r}
Author
Sandra

