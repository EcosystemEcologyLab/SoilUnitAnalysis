# New Project Initialisation Prompt

## How to use this prompt

Copy the entire text under "PROMPT" below and paste it as your first message
to Claude Code in a new Codespace. Do this after the Codespace has opened and
Claude Code is running, but before writing any project code.

Claude Code will read CLAUDE.md and the principles files, set up the repository
structure, install dependencies, and run a configuration check. Review its
output before approving each step.

Replace [REPO-NAME] with your actual repository name before pasting.

---

## PROMPT

Please read CLAUDE.md and any SCIENCE_PRINCIPLES*.md files in this repository.
Then set up the SoilUnitAnalysis repository by completing the following steps in order.
Pause and report after each numbered step before proceeding to the next.

**Step 1 — Verify principles files**
Check that SCIENCE_PRINCIPLES.md is present in the repository root.
Report which principles files are present and confirm you have read them.
If any file referenced in CLAUDE.md under "Lab Principles Source" is missing,
stop and report — do not proceed until the missing files are added.

**Step 2 — Create directory structure**
Create the standard directory structure for this project as described in CLAUDE.md.
Add a .gitkeep file only to directories that should be git-tracked but are
currently empty. Do not create files in gitignored directories.

**Step 3 — Create core project files**
Create the following files, reading CLAUDE.md carefully for the specific
requirements of each:
- .env.example (all required environment variables documented)
- DESCRIPTION (minimal R package metadata if this is an R project)
- All files in R/ described in CLAUDE.md
- All numbered scripts in scripts/ described in CLAUDE.md

**Step 4 — Install dependencies**
Install all required packages using pak or renv as appropriate for this project.
Then initialise renv and create a lockfile. Report the R version and the number
of packages locked.

**Step 5 — Run configuration check**
Run check_pipeline_config() (or the equivalent for this project) and report
the full output. Flag any warnings — do not suppress them.

**Step 6 — Commit everything**
Stage and commit all created files with a clear commit message:
"Initial project setup: directory structure, R functions, scripts, renv lockfile"
Then push to origin/main.

**Step 7 — Report gaps**
Read CLAUDE.md and all SCIENCE_PRINCIPLES*.md files one more time.
Report any topics covered in the principles files that are not addressed
in CLAUDE.md — particularly around output metadata, exclusion logging,
and confidence vocabulary. Do not fix these gaps now — just report them
so the PI can decide what to add to CLAUDE.md.

---

## After initialisation

Once Claude Code has completed all seven steps, review its report and then:

1. Commit any updates to CLAUDE.md that address the gaps identified in Step 7
2. Run the first data acquisition step (e.g. flux_listall()) and commit the
   resulting snapshot
3. Test a small end-to-end run before downloading the full dataset
