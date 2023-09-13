# Code review notes

### Reproducibility

- The original codebase needs some supporting files to run locally. The review PR includes a fix (instructions file + toy data files to mimic the private supporting files).

### Functionality

- Everything works great! 

My session info:

```
R version 4.2.1 (2022-06-23)
Platform: aarch64-apple-darwin20 (64-bit)
Running under: macOS Monterey 12.5.1
```

### User-facing documentation

Breaking:

- `waterbody_name` is a required column for user-uploaded data in the "Compare Your Data" tab, but not listed in the list of required columns. This PR adds it to the list of required columns in the "Instructions" tab.

Optional: 

- The in-app user documentation doesn't include a citation or license. Consider adding one, especially for data downloaded using the app?
- Not having a fish background, it took me a while to figure out that the x-axes for the size plots correspond to Gabelhouse lengths. This PR adds "Gabelhouse length" to the axes to help clarify.

### Developer-facing documentation

- In reading through `app.R`, I added comments and moved some code chunks around so that code relating to each section of the app is grouped together. These changes don't affect functionality, but make the code more readable for an external user. These changes are in a separate branch to make it easier to review the changes to the instructions and user-facing documentation, above. 


### Code health/stability/longevity

- Per the README, there are several files that aren't used in the final iteration of the app. These could be deleted to tidy up (but aren't otherwise hurting anything, and could reasonably stay for archival purposes):
    - The `analysis_scripts` directory
    - The `input_examples` directory
    - `app/process_user_data.Rmd`
