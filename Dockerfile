# Βασική εικόνα από το Rocker project
FROM rocker/r-ver:latest

# Εγκατάσταση πακέτων συστήματος που χρειάζονται για το R και το RStudio
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libnlopt-dev

# Ρύθμιση του working directory
WORKDIR /app

# Αντιγραφή της εφαρμογής και του script εγκατάστασης πακέτων στο container
COPY . /app
COPY install_packages.R /app/install_packages.R

# Εκτέλεση του script εγκατάστασης πακέτων
RUN Rscript /app/install_packages.R

# Έκθεση του port που χρησιμοποιεί η εφαρμογή
EXPOSE 3838

# Εκκίνηση της εφαρμογής
CMD ["R", "-e", "shiny::runApp('/app')"]
