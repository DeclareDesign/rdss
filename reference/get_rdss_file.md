# Download a replication file from the dataverse archive for Research Design in the Social Sciences: Declaration, Diagnosis, and Redesign

See
https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/HYVPO5
for further details and the code used to create these files.

## Usage

``` r
get_rdss_file(name, verbose = TRUE)
```

## Arguments

- name:

  quoted name of the file on the dataverse archive

- verbose:

  print declaration code if requesting a declaration

## Value

an r object

## Details

The available names include:

Design declaration objects:

declaration_9.5  
declaration_2.1  
declaration_2.2  
declaration_4.1  
declaration_5.1  
declaration_7.1  
declaration_9.1  
declaration_9.2  
declaration_9.3  
declaration_9.4  
declaration_9.6  
declaration_9.7  
declaration_10.1  
declaration_10.2  
declaration_10.3  
declaration_10.4  
declaration_10a  
declaration_11.1  
declaration_11.2  
declaration_11.3  
declaration_11.4  
declaration_11.5  
declaration_12.1a  
declaration_12.1b  
declaration_12.1c  
declaration_12.1d  
declaration_13.1  
declaration_13.2  
declaration_15.1  
declaration_15.2  
declaration_15.3a  
declaration_15.3b  
declaration_15.3c  
declaration_15.4  
declaration_15.5  
declaration_15.6  
declaration_16.1a  
declaration_16.1b  
declaration_16.2  
declaration_16.3  
declaration_16.4  
declaration_16.5  
declaration_16.6  
declaration_17.1  
declaration_17.2  
declaration_17.3  
declaration_17.4  
declaration_17.5  
declaration_17.6_a  
declaration_17.6_b  
declaration_18.1  
declaration_18.2  
declaration_18.3  
declaration_18.4  
declaration_18.5  
declaration_18.6  
declaration_18.7  
declaration_18.8  
declaration_18.9a  
declaration_18.9b  
declaration_18.9c  
declaration_18.10  
declaration_18.11  
declaration_18.12  
declaration_18.13  
declaration_19.1  
declaration_19.2  
declaration_19.3  
declaration_19.4  
declaration_23.1a  
declaration_23.1b  
declaration_23.1c  
declaration_23.1d  

Diagnosis objects:

diagnosis_2.1  
diagnosis_4.1  
diagnosis_9.1  
diagnosis_9.2  
diagnosis_9.3  
diagnosis_9.4  
diagnosis_9.5  
diagnosis_9.6  
diagnosis_9.7  
simulation_10.1  
diagnosis_10.1  
diagnosis_10.2  
diagnosis_10.3  
diagnosis_10.4  
diagnosis_10.5  
diagnosis_10a  
diagnosis_11.1  
diagnosis_11.2  
diagnosis_11.3  
diagnosis_11.4  
diagnosis_11.5  
diagnosis_12.1  
diagnosis_12.2  
diagnosis_13.1  
diagnosis_15.1  
diagnosis_15.2  
diagnosis_15.3  
diagnosis_15.4  
diagnosis_15.5  
diagnosis_16.1  
diagnosis_16.2  
diagnosis_16.3  
diagnosis_16.4  
diagnosis_16.5  
diagnosis_17.1  
diagnosis_17.2  
diagnosis_17.3  
diagnosis_17.4  
diagnosis_17.5  
diagnosis_18.1  
diagnosis_18.10_encouragment  
diagnosis_18.10_placebo  
diagnosis_18.11  
diagnosis_18.12  
diagnosis_18.13  
diagnosis_18.2  
diagnosis_18.3  
diagnosis_18.4  
diagnosis_18.5  
diagnosis_18.6  
diagnosis_18.7  
diagnosis_18.8  
diagnosis_18.9  
diagnosis_19.1  
diagnosis_19.2  
diagnosis_19.3  
diagnosis_19.4  
diagnosis_19a  
diagnosis_21a  
diagnosis_21b  
diagnosis_23.1  
diagnosis_23a  

## Examples

``` r
if (FALSE) { # \dontrun{
# Requires internet access
if(curl::has_internet()) {
  diagnosis_2.1 <- get_rdss_file("diagnosis_2.1")
  diagnosis_2.1
}
} # }
```
