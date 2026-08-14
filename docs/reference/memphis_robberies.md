# Personal robberies in Memphis in 2019

A dataset containing records of personal robberies recorded by police in
Memphis, Tennessee, in 2019.

## Usage

``` r
memphis_robberies
```

## Format

A simple-features tibble with 2,245 rows and four variables:

- uid:

  a unique identifier for each robbery

- offense_type:

  the type of crime (always 'personal robbery')

- date:

  the date and time at which the crime occurred

- geometry:

  the co-ordinates at which the crime occurred, stored in
  simple-features point format

## Source

Crime Open Database, <https://osf.io/zyaqn/>
