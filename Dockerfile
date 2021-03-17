# This can change with the whatever version of R the app was built
# sets the base image and OS on which the entire computer will be built
# builds on debian 
FROM rocker/shiny-verse:latest

# Need to install Microsoft SQL ODBC drivers on Linux 
# for connection to Azure Sql Database
# The gnupg is required to executer the curl apt-keyadd commonad
RUN apt-get update -y
RUN apt install curl -y
RUN apt-get install -y gnupg1 -y

RUN curl  https://packages.microsoft.com/keys/microsoft.asc | apt-key add -

#Ubuntu 20.04
RUN curl https://packages.microsoft.com/config/ubuntu/20.04/prod.list > /etc/apt/sources.list.d/mssql-release.list

RUN exit
RUN apt-get update 
RUN ACCEPT_EULA=Y apt-get install msodbcsql17 -y
# optional: for bcp and sqlcmd
RUN ACCEPT_EULA=Y apt-get install mssql-tools
RUN echo 'export PATH="$PATH:/opt/mssql-tools/bin"' >> ~/.bash_profile
RUN echo 'export PATH="$PATH:/opt/mssql-tools/bin"' >> ~/.bashrc
RUN . ~/.bashrc


# Special install to allow B2C functionality for R inside a container. 
# Should be fixed in later Microsoft updates. 
RUN R -e 'install.packages("devtools");'
RUN R -e 'library("devtools"); install_github("LHaferkamp/httpuv")'

# Download the desired package(s)
# R packages needed for Shiny app 
# (This will hopefully be the only change to the docker file, assuming the folder structure is set up right)
RUN install2.r tidyverse shiny DT shinythemes DBI odbc pool reactable glue shinyjs shinyWidgets visNetwork digest\
    ## clean up
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

# The . and /srv/shiny-server/ are two different arguments that relate to your local file and the 
# file path in the container; not to be read as one string. 
# Copy files from the root directory . into the docker image file location /srv/shiny-server/ 
#COPY . /srv/shiny-server/ 

# Copy files from the app folder /app into the docker image file location /srv/shiny-server/ 
COPY /app /srv/shiny-server/

# Exposing port to listen on. This was needed when running on an indivdual VM as opposed to an app service plan
# so when the docker starts port 3838 is exposed. 
EXPOSE 3838
# Required for shiny server 
#USER shiny
