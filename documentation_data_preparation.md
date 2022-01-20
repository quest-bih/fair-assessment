# Documentation of data preparation

A number of data preparation steps must occur prior to the FAIR assessment.

## 1. Searching for Charité publications in bibliographic databases
Charite publications are identified as follows ...

## 2. Text mining of article full texts with ODDPub 
[ODDPub](https://github.com/quest-bih/oddpub) (Open Data Detection in Publications) is an Open Source algorithm in R. It screens the publications for the data sharing statements through the keywords defined in the script. For this the publications have to be prepared in the PDF format and stored in one local folder. Only publications with full text access will be screened, since downloaded PDF files are needed for the ODDPub input. The maximum number of publications is not limited, so the algorithm works well with large numbers of publications. 

The workflow of ODDPub in *three steps*:
- **First**, ODDPub generates PDF files into text files. 
- **Second**, the algorithm searches for the keywords, defined in the script, such as repository name, data availability statement or reference to a supplementary material. The words could be found both within and across the sentence borders (spread through the larger paragraph).
- **Finally**, the match of found keywords and regular expressions (from the script) occurs. If any keyword group matches, the publication is categorized as Open Data.

After the screening an *output* in .csv format will occur in the same folder as the ODDPub script.

Beside the detection of Open Data and its statements, the algorithm detects the type of repository (general-purpose, field-specific, supplement), where the Open Data was shared, and the Open Code statements.

## 3. Screening of data statements with Numbat
Once all articles are screened with ODDPub, the entries with TRUE-Statements for Open Data are **validated manually** in [Numbat](https://github.com/bgcarlisle/Numbat). Numbat is used to manage large databases by extracting them for the purpose of systematic review performance. 

For this, the filtered output from ODDPub (only TRUEs for Open Data) is imported in appropriate format (.tsv).

To validate the Open Data performance an **Extraction Form** *'Openness'* was created in Numbat. It follows the [Open Data Criteria](https://www.bihealth.org/en/translation/innovation-enabler/quest-center/projects/incentives/open-data-funds) for LOM. Going one by one each publication from ODDPub output the extractor verifies the openness of the dataset(s) referred to this publication. As a helpul source of information about, how to find the data statement in the text, the Open Data Statement from ODDPub is used, where both repository name and accession code of the dataset could appear. One article can be referred from 0 to more than 2 datasets. 

The extractor validates sequentially every dataset, until Open Data will be found. If the extractor has a previous knowledge about the repositories that are more likely to store Open Data, she or he can start the extraction with that repository.

The *output* in form of a .csv file can be downloaded any time from the Numbat server, where all data about criteria decisions, as well as the final decision are stored.

## 4. Manually verifying results and research data ids
The further validation requires a large database for a significant result.

Manual verification ensures that at least one dataset identifier per repository is documented. Each Open Data categorized publication from the Numbat output is manually screened in order to find all in the study used datasets. During both the Numbat extraction and the manual verification the best identifier is tracked, that leads directly to the data files.

## 5. Querying servers of FAIR assessment tools like F-UJI or FAIR Enough

## 6. Extracting FAIR scores and other test results from JSON files

## 7. Enriching results with data from Unpaywall, FoR, re3data ...
