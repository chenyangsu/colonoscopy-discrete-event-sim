# Colonoscopy Discrete Event Simulation 

Evaluating colonoscopy wait times and waitlist size in Quebec for individuals 50-74 years of age.

## Scripts

- `des.qmd` contains code for running discrete event simulation for base cases and generates figure 3 of the manuscript.
- `des.R` contains code for running scenario analysis (different scenarios) for the discrete event simulation model.
  - Use `lanuch_des.sh` to launch `des.R` on a high performance compute cluster if available. Otherwise, you can just run `des.R` regularly which will take under 15 min to run for all scenarios.
- `plot_results.R` generates figures 4a and 4b of the manuscript.