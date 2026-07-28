# Workshop Demo Script

A scripted walkthrough of the Bioconductor Proteomics Agent for live
workshop demonstrations. Total time: ~10-15 minutes.

> Research demonstration only. Not intended for clinical diagnosis or
> clinical decision-making.

## 1. Configure your Claude API key

```bash
cp .env.example .env
```

Open `.env` and paste your key into `ANTHROPIC_API_KEY`. Leave
`LLM_MODE=claude` (the default in `.env.example`) to use real Claude
reasoning during the demo. `ANTHROPIC_MODEL` should be set to a currently
supported Claude model id.

## 2. Launch the app

```bash
docker compose up --build
```

First build installs the full Bioconductor stack and can take a while;
subsequent builds are cached and much faster.

## 3. Open the app

```
http://localhost:3838
```

## 4. Confirm Claude is connected

On the **Home** tab, click **Check Claude Connection**. You should see
"Claude connected" with your configured model name. The API key itself is
never displayed.

## 5. Load the example mzML file

Still on **Home**, click **Load Demo Data**. This loads:

- `demo_lcmsms.mzML` -- a small synthetic LC-MS/MS run
- `demo_psm_table.csv` -- a small PSM identification table
- `demo_abundance_table.csv` -- a small peptide abundance table
- `demo_sample_metadata.csv` -- sample group metadata

## 6. Inspect the Spectra and MsExperiment summaries

Go to **Upload & Experiment**. Point out:

- File metadata (SHA-256 checksum, detected format)
- The `Spectra` / `MsExperiment` summary (spectrum counts by MS level, RT
  range)

## 7. View one MS1 and one MS2 spectrum

Go to **Spectra Explorer**. Set the MS-level filter to `1`, pick a
spectrum index, view the stick plot. Repeat with MS level `2`. Show the
TIC and BPC tabs.

## 8. Run the QC Agent

Go to **Quality Control**, click **Run QC Agent**. Walk through:

- MS1/MS2 counts and ratio
- Peaks-per-spectrum, TIC/BPI summaries
- Precursor m/z and charge distributions
- Any severity-tagged warnings (HIGH/MEDIUM/LOW)

## 9. Submit the objective

Go to **Agent Workspace**. The objective box is pre-filled with:

```
Inspect this LC-MS/MS experiment, identify major data-quality concerns,
apply a conservative MS2 peak filter, compare the results before and
after processing, and generate a reproducible report.
```

## 10. Show the Supervisor Agent's plan

Click **Create Plan**. Read the numbered plan aloud -- note that Claude
names specialist agents and tools but has not executed anything yet.

## 11. Execute the approved plan

Click **Run Plan**. The loop is bounded by `MAX_AGENT_STEPS` (default 12)
and every step goes through the Guardrail Agent before it runs.

## 12. Show the specialist agents in action

As the run completes, point out entries in the **Agent Trace** table
attributed to `data_intake`, `qc`, `spectrum_processing`, and `reporting`.

## 13. Show Claude's tool calls and the deterministic R outputs

Highlight that each trace row shows: the tool Claude selected, why, the
validated arguments, the R function that actually executed, and the
resulting object id -- Claude never touched the data directly.

## 14. Show the agent trace

Scroll the full **Agent Trace** table; download it as CSV if useful.

## 15. Generate the HTML report

Go to **Report**, click **Generate Report**, then **Download HTML
Report**. Open it and point out the clearly labeled **"AI-assisted
interpretation generated using Claude"** section, separated from the
deterministic metrics/plots/tables above it.

## 16. Download provenance JSON

Still on **Report**, click **Download Provenance (JSON)**. Open it to show
the full, structured, replayable record of every tool call.

## 17. Show the Docker and GitHub Actions configuration

- `Dockerfile` -- non-root user, pinned Bioconductor base image, health
  check, no secrets baked in.
- `docker-compose.yml` -- `127.0.0.1:3838` binding only, `env_file: .env`.
- `.github/workflows/ci.yml` -- unit tests, lint, mock-mode app health
  check, Docker build + boot + health check, all with `LLM_MODE=mock` and
  no `ANTHROPIC_API_KEY` required.
