# Personal robberies in Memphis in January 2019

A dataset containing records of personal robberies recorded by police in
Memphis, Tennessee, in January 2019. This dataset is too small for some
types of analysis but is included for testing purposes.

## Usage

``` r
memphis_robberies_jan
```

## Format

A simple-features tibble with 206 rows and four variables:

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
