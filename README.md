# MFRM Simulation Data Generator

This Shiny app builds synthetic datasets for Many-Facet Rasch Model (MFRM) studies. It is written for test development researchers who are comfortable with the underlying measurement ideas but prefer a guided, point-and-click interface for configuring simulations. The app emphasises dataset design, metadata capture, and export—not model estimation.

---

## Before You Launch
- **R environment**: R ≥ 4.1 with internet access for installing CRAN packages (one-time). Tested on current macOS and Windows builds.
- **Packages**: `shiny`, `tidyverse`, `mvtnorm`, `DT`, `zip`, plus the `pacman` helper used inside the app. If a package is missing the first run, pacman will install it.
- **Project setup**:
  1. Open R or RStudio and set the working directory to this project folder.
  2. Install dependencies if needed:
     ```r
     install.packages("pacman")
     pacman::p_load(shiny, tidyverse, mvtnorm, DT, zip)
     ```
  3. Start the app:
     ```r
     shiny::runApp("app.R")
     ```
- **First-time tip**: Leave the default values in place for your inaugural run so you can verify the interface before moving to bespoke designs.

---

## Workflow Overview
The UI is organised into a left control column (inputs) and a right results area (generated content). Work from top to bottom in the left column, then review outputs on the right.

1. **Frame the design** (`Basic Settings` tab)
   - *Ask yourself*: Do I need a complete cross, or do workload and logistics require a partial/balanced design? How much missingness should I mimic?
   - Steps:
     1. Choose the cross design. The radio buttons map to `complete`, `balanced`, or `random` assignment logic.
     2. If you pick `balanced` or `random`, adjust the three assignment rate sliders to control rater/task/criteria coverage per participant. The UI automatically enforces at least one selection.
     3. Set a missing-data rate if you intend to drop rows after sampling.
     4. Record the seed that should anchor reproducibility for the batch.
     5. Specify counts: participants, raters, tasks, criteria.
     6. Configure the rating scale minimum/maximum. Keeping **Auto-generate thresholds** checked lets the app fill evenly spaced thresholds for you; uncheck it only when you intend to supply manual cut scores.
     7. Select the residual structure type and tune the `Residual SD` and `Within-facet correlation` sliders to control unexplained variance.

   Cross design options at a glance:

   | Option | Use when | Sampling impact |
   | --- | --- | --- |
   | `complete` | You need every participant scored by every rater on every task/criterion to match a textbook MFRM study. | Generates the full Cartesian product—ideal for calibration checks but produces the largest datasets. |
   | `balanced` | Operational constraints require smaller rater teams, yet you still want systematic coverage. | Splits participants into blocks, rotates raters/tasks/criteria (optionally in equal blocks) so each facet appears evenly across the sample. |
   | `random` | Field conditions are irregular and you want to mimic opportunistic assignments. | Draws raters/tasks/criteria per participant at random according to the assignment rates, yielding organic but potentially uneven coverage. |

   When `balanced` is selected, the **Create equal blocks** checkbox ensures each block receives the same number of participants, then cycles raters/tasks/criteria through those blocks for a clean, repeated structure.

   Residual structure options:

   | Type | When to use | Effect on data |
   | --- | --- | --- |
   | `Random residuals` | Baseline simulations where you assume independent residual noise. | Adds iid error to each observation; lower SD drives tighter fit around expected scores. |
   | `Rater-related residuals` | You want to model dependence introduced by rater-specific behaviours (halo, leniency bursts). | Generates correlated residuals within each rater across all tasks/criteria; higher correlation clusters misfit by rater. |
   | `Task-related residuals` | Tasks may trigger common shocks (prompt difficulty swings). | Correlates residuals within each task across raters/criteria, creating task-level drift. |
   | `Criteria-related residuals` | Certain rubric traits share variance (e.g., language and content intertwined). | Correlates residuals within criteria across raters/tasks, highlighting construct overlap at the criterion level. |

   `Residual SD` sets the magnitude of these deviations, while `Within-facet correlation` determines how strongly scores co-move inside the chosen facet. Combining a high SD with a high correlation produces pronounced multidimensional signals, whereas a low SD with zero correlation approximates the Rasch ideal.

2. **Control ability, severity, difficulty** (`Ability/Difficulty` tab)
   - *Ask yourself*: Which facet variances drive the behaviour I want to examine? Do I need to emphasise rater severity differences or participant spread?
   - Steps:
     1. Pick the participant distribution (`normal` or `uniform`).
     2. Dial in the means and standard deviations for each facet. Sliders expose a −6 to +6 logit window so you can emulate moderate or extreme effects.
     3. Use the facet effect-size sliders if you need to inflate or dampen rater/task/criteria spreads beyond the specified SDs.

   Participant distribution options:

   | Choice | When to use | Effect |
   | --- | --- | --- |
   | `normal` (default) | Most measurement scenarios where ability is bell-shaped. | Draws θ from a Gaussian distribution centred on the mean. SD controls spread; symmetric tails make it easy to model outliers. |
   | `uniform` | Stress tests where every ability level should appear equally, or when you want to avoid clustering around the mean. | Samples θ across the specified range with equal probability. Expect flatter score distributions and stronger impact from thresholds at the extremes. |

   Remember that the **Mean** sliders shift the centre of each facet, while the **SD** sliders widen/narrow the spread. The **Effect Size** sliders multiply the SDs after sampling, so doubling the rater effect size, for example, doubles the realised severity variation without altering the mean.

3. **Shape thresholds** (`Threshold Settings` tab)
   - *Ask yourself*: Is my rating scale fully defined by automatic, evenly spaced thresholds, or do I require manual cut scores?
   - Steps:
     1. Decide in the `Rating Scale Settings` box (back on the `Basic Settings` tab) whether thresholds are auto-generated.
     2. If auto mode is on, the tab simply previews the current values.
     3. If you turned auto mode off, follow this exact sequence:
      - After unchecking **Auto-generate thresholds**, switch to the **Threshold Settings** tab.
      - Adjust each threshold slider to the desired cut score. Sliders appear only when manual mode is active.
      - Click **Update Thresholds** to commit the new values. The notification confirms success, and the app re-sorts the thresholds to keep them ascending.
      - If you later change the rating scale min/max, repeat the same process so the counts match; otherwise the app regenerates them automatically.

4. **Manage reusable configurations** (`Save Parameters` tab)
   - *Ask yourself*: Will I revisit this scenario later or share it with a colleague?
   - Steps:
     1. Click **Save Current Parameters** to download an `.rds` snapshot of every visible input (including the number of datasets and manual thresholds).
     2. Use **Load Parameters** to restore an earlier session. All inputs update instantly, and any threshold mismatches trigger regeneration.

5. **How many datasets?** (`Simulation Settings` box)
   - Set the number of datasets to generate. A batch increments the seed for each dataset (`seed + dataset_index - 1`) so you can perform bootstrap-style replications.

6. **Generate**
   - Press **Generate Datasets**. A modal progress bar summarizes the run. If threshold counts and the rating scale conflict, the app automatically repairs them and sends a notification.

7. **Inspect results** (`Generated Data` tab in the main panel)
   - *Ask yourself*: Which dataset do I need right now? What metadata should I note for downstream scripts?
   - Steps:
     1. Use the dataset selector (appears after a run) to switch between generated datasets.
     2. Review the metadata table for seed, facet counts, rating scale, and threshold summary.
     3. Check the dataset summary for counts by facet to confirm the design executed as expected.
     4. Scroll through the `Data Preview` (DT table) to spot-check scores, logits, and residuals.
     5. Choose a download option (see "Take Your Results With You").

8. **Audit parameter history** (`Parameters` tab in the main panel)
   - Displays a formatted table of the last successful run and a raw printout of all parameters saved in the result object. Handy for copying settings into reports or scripts.

---

## Take Your Results With You
All download buttons live inside the **Generated Data** tab.

- **Selected dataset (CSV)**: Core variables for one dataset (`participant_id`, `rater_id`, `task`, `criteria`, `item_id`, ability/difficulty logits, expected score, variance, residuals, observed score).
- **Selected dataset (RDS)**: A list storing `data`, `metadata`, and the dataset-specific seed.
- **All datasets (combined CSV)**: Every dataset stacked with a `dataset_label` column—convenient for piping directly into analysis code that expects long-form data.
- **All datasets (ZIP of CSVs)**: Each dataset exported separately as `dataset_label.csv` inside an archive.
- **All datasets (RDS)**: The full run object (`datasets`, `dataset_names`, `params`) for programmatic use in R.

Remember to label any downstream analyses with the seed and dataset label shown in the metadata table so collaborators can recreate the exact draw.

---

## Example Scenario: Calibrating Rater Drift Training
**Goal**: Compare how often an anchored rater needs recalibration when task difficulty increases while participant ability remains stable.

1. **Design**: Choose a balanced cross design with `rater_assignment_rate = 0.5` to mimic partial overlap between rater teams. Set `missing_data_rate = 0.1` to reflect unscored performances.
2. **Ability/Difficulty**: Keep `participant_mean = 0`, `participant_sd = 1`, but raise `task_sd` to 0.8 and `task_effect_size` to 2.0 to accentuate task shifts. Leave rater means at 0 but raise `rater_sd` to 0.6 to allow for drift.
3. **Residuals**: Select **Rater-related residuals** with `within_facet_corr = 0.6` so inflation manifests within rater-specific clusters.
4. **Thresholds**: Maintain automatic thresholds for a 1–5 scale.
5. **Simulation**: Request `n_datasets = 100` replicates with seed 830.
6. **Review**: After generation, scan the dataset summary to verify rater coverage per participant, then download the combined CSV and all-dataset RDS. The RDS gives you the entire parameter log for documentation.

This workflow produces 100 labelled datasets, each capturing rater/task combinations under drift pressure while maintaining participant ability structure.

---

## Troubleshooting Checklist
- **No datasets appear after clicking Generate**: Ensure `n_datasets` is at least 1 and that no required input is blank. The progress bar should advance; if it does not, check the R console for error messages.
- **Threshold warning pops up repeatedly**: Confirm the rating scale minimum/maximum align with the number of manual thresholds. Switching back to auto-generation resets them instantly.
- **Downloads blocked by your browser**: Most browsers flag multiple downloads (ZIP + CSV). Allow popups for `127.0.0.1` (or the host shown in the address bar) during the session.
- **Facet counts look off in the summary**: For `balanced` or `random` designs, inspect the assignment rate sliders; rates below 1 can reduce the number of raters/tasks/criteria per participant by design.
- **Re-running with different seeds still yields identical data**: Remember that each dataset reuses the seed shown in metadata. Change the base `Random seed value` before regenerating a new batch.

---

## Glossary (UI ↔︎ Rasch Terminology)
- **Cross Design Type**: Specifies the sampling design for the facets (fully crossed vs. partially crossed) used in MFRM study planning.
- **Residual Structure Type**: Chooses the facet whose residuals share correlation, modelling multidimensional departures.
- **Residual Standard Deviation**: Controls unexplained variance (θ estimates vs. observed scores) to tune unidimensionality.
- **Within-Facet Correlation**: Sets the magnitude of residual clustering within the chosen facet.
- **Effect Size sliders**: Scale the standard deviation for rater/task/criteria to exaggerate or compress severity/difficulty spreads.
- **Thresholds**: Rating category step calibrations; manual mode lets you impose bespoke cut scores.
- **Dataset Label**: Internal name (e.g., `Dataset 001`) attached to each simulated dataset and reused in exports.

---

## Contributing
Feel free to adapt the code for your institution’s workflow. If you introduce new UI panels or export formats, document them in this README so fellow researchers can follow the updated prompts.
