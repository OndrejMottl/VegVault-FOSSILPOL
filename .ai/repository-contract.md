# VegVault-FOSSILPOL Repository Contract

## Role and outputs

This repository owns Neotoma acquisition, chronology-control preparation, Bchron age-depth modelling, age prediction and uncertainty, taxonomic harmonisation, filtering, and final fossil-pollen assembly.

The downstream contract includes `Outputs/Data/data_assembly_light_*.qs`, partitioned `Outputs/Data/data_age_uncertainty_*.qs`, and the metadata/reference products under `Outputs/Meta_and_references/`. Treat filenames, nested structures, identifiers, ages, uncertainty iterations, taxonomy, citations, and units as interfaces consumed by the main VegVault repository.

## Safety

- Author names, contact details and ORCIDs come from the public Neotoma API. They may be tracked under `Data/Input/Author_info/` and published in the author table under `Outputs/Meta_and_references/`, so that data contributors can be credited and contacted. Do not add contact details from any other source.
- Do not expose or redistribute private records, local personal-database storage, or inputs whose licences do not permit redistribution.
- Do not edit chronology selections, depositional-environment filters, harmonisation tables, regional age limits, or manual classification decisions as incidental cleanup.
- Full Neotoma downloads, age-depth model fitting, chronology prediction, harmonisation, and assembly are expensive and require explicit user authorization.
- Never enable broad overwrite/rerun behavior without identifying the exact datasets, model products, and output paths affected.
- Preserve `Data/Temp/` as disposable only; important manual decisions belong in tracked input tables or durable documentation.

## Change and validation contract

For changes to final output shape or meaning, trace consumers in `../VegVault/R/02_Main_analyses/03_Import_fossilpol_data.R`, coordinate a producer release/tag, and update the pinned integration reference. Validate small fixtures and affected stages first; run the full workflow only when requested. Preserve age-uncertainty dimensions, sample/dataset keys, reference provenance, and deterministic seeds where applicable.
