# Historical data repository using the IAMC format

*Structure for collaboratively collecting and processing up-to-date historical reference data on energy supply and demand and emissions into the IAMC format  , for assessing the plausibility of near-term scenario trajectories*

## Folder structure:

- **data**: original data sources, links are available in src/process_hist_data.R script, but we also collect all needed files in a shared drive location

- **figures**: folder for created comparison figures, and near-term forecast analysis

- **mappings**: folder for mappings for regions/ countries, as well as energy system and emissions variables to IAMC template

- **output**: folder for processed data files: both R objects for detailed data (structure close to original, but cleaned up), and csv files for harmonized IAMC format

- **runs**: folder for scenario data in IAMC format to compare against historic data

- **src**: R scripts for primary creation of data, for comparison plots, as well as tools for near-term projections and targets compared to historic data.



## Workflows:

After cloning this repository to your local computer:

1. Double-click rhistiamc.Rproj to load the project in Rstudio 

### Option A - Use already processed results: to make comparison plots with IAMs, etc

2. Download the processed historical data in IAMC format from: [PUBLIC_output_rhistiamc](https://drive.google.com/open?id=117cTkVRekeu3vHYrFkH93zstpCqGxkM8&usp=drive_fs) 
into your local output/ folder

3. Open and run the src/combine_and_plot.R script, following the comments inside 

### Option B - Start from raw data: to add / update a dataset | to aggregate to new regions that match IAM / analysis 

2. Open src/process_hist_data.R

3. Download the open access historical data files from the original sources shown in the comments throughout the script, 
or for a quick start from this [data_rhistiamc](https://drive.google.com/open?id=1mGYipWX2EEYgQzcLunJbPD8vCxFknQfK&usp=drive_fs) shared folder. 

4. Run the src/process_hist_data.R script

5. If desired to make figures or perform analysis on the historical and IAM data (optional), then open and run the src/combine_and_plot.R script.
Make any additional src/fig_...R scripts based on the ones provided or with a different setup.  


## Notes: 
- Only the files in mappings and src are git managed, others are ignored to reduce repository size (see .gitignore).

- This repository is on purpose similar, in some aspects, to model-internal code such as gcamdata, in that it provides a harmonized platform for processing data, and directly targets comparison with GCAM data. However, this repository enables comparisons at the IAMC format output level (allowing for checks on historic calibration, and correctness of our reporting), and allows for faster updates, given its lower complexity, and focus on fewer key variables.

- This repository also produces historical data at an aggregate region level of the user's choice, in addition to the the country level, so that it can also be used for analysis of IAM results. However, be aware that not all datasets are comprehensive by country, which means that the aggregate mapped results, for example into five global regions (R5), may not be the fully complete values. 

## How to update or add historical data:
The script src/process_hist_data.R reads in a lot of data from publicly available sources, and converts them into a common format, in line with the IAMC variable template as used in the IPCC AR6 database (https://data.ene.iiasa.ac.at/ar6).

See comment lines on top of this script for short explanation on structure.


## Contributing:

We invite collaboration on and improvement of this community repository. Please use the Issues tab for any questions / comments and use or improvement. To contribute to the repository, first fork this one (umd-cgs/rhistiamc) to your local organization's GitHub, then push commits to that one and start a pull request to this repository. We will consider these pull requests and discuss any questions that we have with your team. 
