# Overview
Submission to ACDC follows three separate steps
1) Researchers must provide details on the data using the submission website (LINK). For more details - see "Submitting Data" below
2) The resulting submission.json file must be added to acdc.db via an R script (see "Adding submissions to ACDC").
3) The updated acdc.db file must be added as a new release to GitHub. (see "Releasing a new version of ACDC")

In case of issues or questions, feel free to reach out to the maintainers of ACDC. Currently: sven.lesche@psychologie.uni-heidelberg.de and julia.haaf@uni-potsdam.de

# Submitting Data

This guide provides detailed instructions on how to:

- Submit new data to ACDC
- Validate submissions
- Convert submissions into final ACDC-compatible format

---

## 1. Setup
Before starting, ensure you have:

- **Raw, trial-level data** (e.g., accessible via OSF or another repository)
- **Detailed experimental information**, including:
    - Experimental setup
    - Conditions
    - Associated publication (if available)

---

## 2. Using the Submission Website

To streamline data entry, ACDC provides a dedicated submission tool:

🔗 [https://slesche.github.io/acdc-entry/](https://slesche.github.io/acdc-entry/)

### Key Features

- Guides you through the required fields step by step
- Ensures correct formatting
- Checks for inconsistencies
- Outputs a structured `.json` file compatible with ACDC

### Workflow

Follow the pages in this order:

1. **Overview**
2. **Publication**
3. **Task**
4. **Study**

Carefully follow the instructions on each page and let the website guide you through the rest of the process.

### Saving Your Progress

- Use **“Save Progress”** (on the Overview page) to download a backup file
- Use **“Upload Progress”** to resume your work later

⚠️ Important: Reloading the page will erase all entered data. Save frequently.

### Submitting Your Data

Once all required information is complete:

- Click **“Submit”** on the Overview page
- A `.json` file will be downloaded

This file is called a **`submission_check.json`** and serves as the basis for further processing.

---

## 3. Converting Submission Files

Although labeled as a submission, the downloaded file is still a **progress file**.

### Why this extra step?

1. Only maintainers can create final submission files
2. It allows validation before inclusion in ACDC

---

## 4. Validating Submitted Data

Before converting a submission, verify its integrity:

### Steps

1. Upload the file using **“Upload Progress”**
2. Review all entered responses

### What to Check

- Are all fields complete and meaningful?
- Is there a valid publication or open data link?
- Does the linked dataset match the submission?
- Are responses coherent (not spam or placeholder text)?
- Are there comments indicating confusion or issues?

This step is especially important for third-party submissions.

---

## 5. Generating the Final Submission File

To convert a `submission_check.json` into a final submission file:

### Step 1: Modify the Source Code

Go to the repository:

https://github.com/SLesche/acdc-entry

Clone the repository, edit the following file locally:

docs/functions/submissionFunctions.js

Inside the `submitData()` function:

- Locate the commented section in the latter half
- **Uncomment the indicated lines**

---

### Step 2: Run the Tool Locally

- Open `docs/index.html` in your browser
- Upload the `submission_check.json` file
- Click **“Submit”**

### Output

Two files will be generated:

1. A new progress file
2. A final submission file:
    
    submission_author_year.json
    

This second file is the **official ACDC submission object**.

---

### Step 3: Restore Default Behavior

After generating the final file:

- Re-comment the previously uncommented lines
- This ensures the tool continues producing `submission_check.json` files by default

---

## 6. Troubleshooting

If you are unable to:

- Modify the code
- Run the tool locally

→ Contact the ACDC maintainers for assistance.

---

## Summary

- Use the website to create a `submission_check.json`
- Validate the submission carefully
- Convert it into a final submission via source code modification
- Restore the tool afterward
# Adding Submissions to ACDC

Before making any modifications to the ACDC database or related scripts, it is **strongly recommended** to use version control (e.g., Git). This ensures that changes can be tracked and easily rolled back if needed.

---

## 1. Preparing the Repository

To begin:

- Clone the ACDC repository:  
    [https://github.com/jstbcs/acdc-database](https://github.com/jstbcs/acdc-database)
- Navigate to the folder:
    
    add_data_from_website
    
- Place your final submission file (`submission_author_year.json`) into:
    
    add_data_from_website/data
    

---

## 2. Running the Data Integration Script

The main script for adding new data is:

add_data_from_website/add_data_from_website.R

Open this file and follow the embedded comments carefully.

---

## 3. What the R Script Does

The script performs several steps to integrate your submission into ACDC:

### Data Transformation

- Reads the `.json` submission file
- Converts it into an R list structure

The JSON file already mirrors the hierarchical structure of ACDC:

- Publications
    - Studies
        - Datasets (and further nested levels)

The script extracts relevant components and transforms them into the format required by the database.

### Helper Functions

Transformation is handled by functions located in:

add_data_from_website/functions

Key functions include:

- `extract_from_submission_json()`
- `prep_submission_data()`

If issues arise during transformation, possible causes include:

- Using a **progress file** instead of a final submission file
- Outdated preprocessing functions

In such cases, updates may be required in the `functions` directory.

---

## 4. Inspecting the Prepared Data

After transformation, you can manually inspect the processed data at different levels using:

inspect_LEVEL()

These functions help verify correctness at each stage (e.g., publication, study, dataset).

---

## 5. Structure Validation

Before adding data to the database, run:

check_overall_structure()

### What this function checks:

- Required fields are present
- Variable names match expected formats
- Relationships between entities are valid
- Data is consistent with ACDC schema

⚠️ This function must complete **without errors** before proceeding.

---

## 6. Adding Data to the Database

Once validation passes:

1. Provide the path to the target `acdc.db` file
2. Use:
    
    add_object()
    
    on the prepared and validated data object
    

### What `add_object()` does:

- Iterates through all levels of the submission:
    - Publication → Task → Study → etc.
- Inserts entries into the corresponding database tables
- Automatically assigns new IDs:
    - Ensures IDs follow the existing sequential structure
    - Uses the next available identifier (e.g., `publication_id`)

During execution, you will see messages indicating progress.  
A successful run typically ends with a confirmation message related to the **observation table**.

---

## 7. Error Handling

If the process fails:

- **Immediately roll back** changes to the database
- Review error messages carefully
- Debug the issue (common causes include formatting or structural mismatches)

If the issue cannot be resolved, contact the maintainers.

---

## 8. Verifying the Updated Database

After a successful run, inspect the updated database to confirm that the new data was added correctly.

### Steps:

- Use the `acdcquery` package
- Query the database using:
    
    query_db()
    
- Filter results:
    - Newly added entries typically have the **highest IDs**

Inspect multiple table levels to ensure:

- Data is present
- Relationships are correct
- Values match the original submission

---

## 9. Final Check

If everything looks correct:  
**Your submission has been successfully added to ACDC.** 
You can now move on to publishing the updated ACDC file on GitHub.

# Releasing a New Version of ACDC

This section describes the steps required to publish a new version of the ACDC database.

---

## 1. Generate a Hash File

Before releasing a new version, you must generate a hash file for the updated database. This allows users to:

- Verify the integrity of their local database
- Check whether they are using the most up-to-date version

You can generate the hash using the following function from the `acdcquery` package:

```
create_sqlite_hash_file <- function(sqlite_path,  
                                    output_file = NULL,  
                                    algo = "sha256") {  
  if (!file.exists(sqlite_path)) {  
    stop("SQLite file does not exist.", call. = FALSE)  
  }  
  
  hash_value <- digest::digest(file = sqlite_path, algo = algo)  
  
  if (is.null(output_file)) {  
    output_file <- paste0(sqlite_path, ".", algo)  
  }  
  
  writeLines(hash_value, output_file)  
  
  invisible(output_file)  
}
```

### Notes:

- The default hashing algorithm is **SHA-256**
- The output file will typically be:
    
    acdc.db.sha256
    

---

## 2. Finalize Repository State

Before creating a release:

- Ensure all changes are committed
- Push all updates to the remote repository
- If applicable, merge the `dev` branch into the `main` branch

This ensures the release reflects the latest stable state of the database.

---

## 3. Create a New Release

Create a new GitHub release here:  
[https://github.com/jstbcs/acdc-database/releases/new](https://github.com/jstbcs/acdc-database/releases/new)

---

## 4. Configure the Release

Each release must include the following:

### 4.1 Version Tag

- Assign a new version tag
- Follow semantic versioning (increment appropriately based on changes)

Example:

- If the previous version was `v1.0.0`, the next could be:
    
    v1.1.0
    

---

### 4.2 Attach Required Files

Before publishing the release, upload the following files:

- `acdc.db` (the updated database)
- `acdc.db.sha256` (the corresponding hash file)

Use the **“Attach binaries”** option on the release page to upload these files.

---

## 5. Publish and Verify

After publishing the release:

- Confirm that the release is publicly accessible
- Verify that both files are correctly attached

Then, test accessibility using the `acdcquery` package to ensure users can:

- Download the new version
- Validate it against the hash file

---

## Summary

To release a new version of ACDC:

1. Generate a hash file for the database
2. Finalize and synchronize repository changes
3. Create a new GitHub release
4. Add a version tag
5. Upload `acdc.db` and its hash file
6. Verify that the release is accessible and functional
