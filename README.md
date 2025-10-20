# Historical data repository using the IAMC format

*Structure for collaboratively collecting and processing up-to-date historical reference data on energy supply and demand and emissions into the IAMC format, for assessing the plausibility of near-term scenario trajectories*

This repository uses data from publicly available sources, and converts them into a common format, in line with the IAMC variable template being developed in the Common Definitions work (https://files.ece.iiasa.ac.at/common-definitions/common-definitions-template.xlsx). 

[Overview presentation](https://docs.google.com/presentation/d/11v2VsqhGYUi_FiGnaTh6nX_JylEhmXHu?rtpof=true&usp=drive_fs) given at the IAMC Conference in November 2024

## Folder structure:

- **data/raw_historical**: location to put original data sources, links are available in the [Dataset Tracker](https://docs.google.com/spreadsheets/d/1ZcuLWFAPcPumLa3p5A3jvaRGcuUG5gHVOVDiupuJ36U/edit?usp=sharing)

- **data/processed_historical**: location of processed data files: both R objects for detailed data (structure close to original, but cleaned up), and csv files for harmonized IAMC format

- **data/scenarios**: location to put scenario data in IAMC format to compare against historic data

- **figures**: location where comparison and near-term forecast analysis figures are created

- **mappings**: location of mappings for regions / countries, as well as energy system and emissions variables to IAMC template

- **src**: location of R scripts for primary creation of data and to make comparison plots



## Workflows:

After cloning this repository to your local computer:

1. Double-click rhistiamc.Rproj to load the project in Rstudio 

### Option A - Use already processed results:
- to make **comparison plots** with IAMs, etc

2. Download the processed historical data in IAMC format from: [processed_historical](https://drive.google.com/open?id=1dvOgPsSDInEo4KnLajaYP3c-7I6f5ZTd&usp=drive_fs) 
into your local ```data/processed_historical``` folder

3. Open and run the ```src/combine_and_plot.R``` script, following the comments inside 

### Option B - Start from raw data: 
- to **add / update a dataset**
- to **aggregate to new regions** that match IAM / analysis 

2. Open ```src/process_hist_data.R```

3. Download the open access raw historical data files from the original sources shown in the [Dataset Tracker](https://docs.google.com/spreadsheets/d/1ZcuLWFAPcPumLa3p5A3jvaRGcuUG5gHVOVDiupuJ36U/edit?usp=sharing) and some additional notes in the comments of ```src/process_hist_data.R script```, 
or for a quick start download them from this [raw_historical](https://drive.google.com/drive/folders/1mGYipWX2EEYgQzcLunJbPD8vCxFknQfK?usp=sharing) folder (shared internally at CGS) into your local ```data/raw_historical``` folder

4. Run the ```src/process_hist_data.R``` script

5. If desired to make figures or perform analysis on the historical and IAM data (optional), then open and run the ```src/combine_and_plot.R``` script.
Make any additional ```src/fig_...R``` scripts based on the ones provided or with a different setup.  

## Contributing:

We invite collaboration on and improvement of this community repository. Please use the Issues tab for any questions / comments regarding useage or improvement. To contribute to the repository, first fork this one (umd-cgs/rhistiamc) to your local organization's GitHub, then push commits to that one and start a pull request to this repository. We will consider these pull requests and discuss any questions that we have with your team. 

### Updating / Adding Data

When updating or adding a dataset:

1. Download updated dataset, add timestamp to the file name if it lacks timestamp / version number
   
2. Edit `src/process_hist_data.R` to read in, process the new / updated dataset

3. If updating data, compare with previous version to confirm that there is additional data, and that regions / variables aren't removed

4. Update the mapping files as necessary to incorporate additional variables / data structure changes

5. Run the script to regenerate outputs and check results

6. Update the [Dataset Tracker](https://docs.google.com/spreadsheets/d/1ZcuLWFAPcPumLa3p5A3jvaRGcuUG5gHVOVDiupuJ36U/edit?usp=sharing):
*View-only Google Sheet — request edit access if needed for updates.*
   - Change the “Last Updated” date
   - Add any notes on variable or format changes
   - Add a new row if it’s a new dataset

7. Commit your changes and open a pull request with a short summary

---

## Notes: 
- Only the files in mappings and src are git managed, others are ignored to reduce repository size (see .gitignore).

- This repository somewhat similar to model-internal code such as gcamdata, which provide a harmonized platform for processing data for use with that model. However, this repository enables comparisons at the IAMC format output level (allowing for checks on historic calibration, and correctness of our reporting), and allows for faster updates, given its lower complexity, and focus on fewer key variables.

- This repository also produces historical data at an aggregate region level of the user's choice, in addition to the the country level, so that it can also be used for analysis of IAM results. However, be aware that not all datasets are comprehensive by country, which means that the aggregate mapped results, for example into five global regions (R5), may not be the fully complete values. 


