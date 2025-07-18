# Base R Shiny image
FROM rocker/shiny-verse

# Make a directory in the container
RUN mkdir /home/shiny-app

# Install R dependencies
RUN R -e "install.packages(c('ggplot2',  'dplyr', 'shinyjs', 'bslib', 'shinyWidgets', 'dygraphs', 'htmltools', 'DT'))"

# Copy the Shiny app code
COPY . /home/shiny-app/

# Expose the application port
EXPOSE 8188

# Run the R Shiny app
CMD ["R", "-e", "shiny::runApp('/home/shiny-app', host='0.0.0.0', port=8188)"]