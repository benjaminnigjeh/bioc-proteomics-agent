# Bioconductor Proteomics Agent

A multi-agent, Claude-powered R Shiny application for opening, inspecting,
processing, visualizing, and reporting protein mass-spectrometry data using
R and Bioconductor. Runs entirely on your local computer through Docker.

> **Research demonstration only. Not intended for clinical diagnosis or
> clinical decision-making.**

## What this is

- An R Shiny + `bslib` web app for LC-MS/MS proteomics data exploration,
  QC, processing, identification review, and quantification.
- A restricted, auditable **multi-agent architecture**: a Supervisor Agent
  (Claude or a deterministic mock backend) plans and selects from a fixed
  registry of **tools**, and every tool is a deterministic R/Bioconductor
  function. Claude never writes or executes code.
- A **bounded tool-use loop** with a full, downloadable **agent trace** and
  **provenance** for every computed result.
- Fully **local**: Docker Compose binds the app to `127.0.0.1:3838` only.

## Quick start

```bash
cp .env.example .env
# edit .env and paste your Claude API key into ANTHROPIC_API_KEY
docker compose up --build
```

Open **http://localhost:3838**.

Stop the app:

```bash
docker compose down
```

Tail logs (never contains the API key):

```bash
docker compose logs -f app
```

Without a Claude key, set `LLM_MODE=mock` in `.env` (already the CI/test
default) to use the deterministic mock backend -- every tool still runs for
real, only the planning/narration is templated instead of Claude-generated.

## Architecture

```
Browser (http://localhost:3838)
        |
   R Shiny + bslib UI  (modules/*.R)
        |
   Supervisor Agent  (R/agent_supervisor.R, R/agent_execution.R)
        |            \
   Claude API         Mock LLM backend
 (R/claude_client.R)  (R/mock_llm.R)      <-- LLM_MODE switches between these
        |
   Guardrail Agent (R/agent_guardrails.R)
        |  validates tool name, JSON schema, parameter ranges
        v
   Tool Registry (R/agent_registry.R) -- 22 allowlisted tools
        |
        v
   Deterministic R/Bioconductor functions
   (R/ms_import.R, ms_qc.R, ms_processing.R, ms_identification.R,
    ms_quantification.R, ms_reporting.R)
        |
        v
   Provenance store (R/provenance.R) -> Agent Trace UI + HTML report
```

Claude (or the mock backend) can only ever request one of the 22 registered
tools with JSON arguments; it never sees or executes R/shell code, never
reads environment variables, and never gets direct file-system access. See
[SECURITY.md](SECURITY.md) for the full guardrail list.

### Agents

| Agent | Responsibility |
|---|---|
| Supervisor | Plans, selects tools, reviews results, writes the final grounded summary. Never computes results itself. |
| Data Intake | Validates uploads, imports mzML/mzXML/MGF into `Spectra`/`MsExperiment`, checksums. |
| QC | File/spectrum QC metrics, severity-tagged warnings, QC plots. |
| Spectrum Processing | Filtering, normalization, top-N peaks as new named stages; before/after comparison. |
| Identification | PSM table import/validation/filtering, peptide/protein summaries. |
| Quantification | `QFeatures` construction, normalization, aggregation, PCA, two-group comparison. |
| Reporting | Combines all computed results + trace + provenance into an HTML report. |
| Guardrail | Tool allowlisting, schema/range validation, secret redaction, narrative grounding checks. |

### Bioconductor packages

Core: `Spectra`, `MsExperiment`, `MsCoreUtils`, `mzR`, `ProtGenerics`,
`QFeatures`, `SummarizedExperiment`, `MultiAssayExperiment`, `BiocParallel`.
Also used: `MsBackendMgf`, `PSMatch`, `limma` (mzML/mzXML import uses the
`MsBackendMzR` backend class that ships inside `Spectra` itself).

`Spectra` is the canonical spectrum abstraction and `MsExperiment` the
canonical experiment container throughout the app; `QFeatures` is the
canonical quantitative container. Data frames are only used for
display/export, never as a replacement for these objects.

## Configuration

All Claude configuration is read from environment variables only (never
hard-coded, never logged, never sent to the browser):

| Variable | Purpose | Default |
|---|---|---|
| `ANTHROPIC_API_KEY` | Claude API key | *(none)* |
| `ANTHROPIC_MODEL` | Claude model id | *(required in claude mode)* |
| `LLM_MODE` | `claude` or `mock` | `mock` |
| `MAX_AGENT_STEPS` | Bounded tool-use loop cap | `12` |
| `MAX_TOOL_RETRIES` | Retries for *transient* Claude API failures only | `3` |
| `CLAUDE_REQUEST_TIMEOUT` | Seconds | `120` |
| `SHINY_PORT` | Container/app port | `3838` |
| `MAX_UPLOAD_MB` | Upload size limit | `500` |

```bash
cp .env.example .env
```

`.env` is git-ignored and is never baked into the Docker image -- Compose
injects it at container start via `env_file:`.

The app fails safely (clear UI message, no crash) when the key is missing,
the model is unavailable, the API rate-limits, auth fails, the request
times out, Claude returns malformed tool arguments, or the step budget is
reached.

## Running without Docker

```bash
Rscript scripts/install_packages.R
Rscript scripts/generate_demo_data.R
Rscript scripts/run_app.R
```

Also serves at **http://localhost:3838**. Loads `.env` for local dev
convenience (Docker does not need or use this loader).

## Using the app

1. **Home** -- check Claude connection status, LLM mode, Bioconductor/R
   versions, or click **Load Demo Data**.
2. **Upload & Experiment** -- upload mzML/mzXML/MGF, or use demo data.
   View file metadata, `Spectra`/`MsExperiment` summaries.
3. **Spectra Explorer** -- filter by MS level/RT/precursor m/z, browse
   spectra, view TIC/BPC/stick plots.
4. **Quality Control** -- run the QC Agent, review severity-tagged
   warnings, view TIC/BPC.
5. **Identifications** -- upload a PSM CSV/TSV, review auto column
   mapping, filter by score/FDR, see peptide/protein summaries.
6. **Quantification** -- upload an abundance table, build a `QFeatures`
   object, normalize, aggregate to protein, run PCA / two-group
   comparison.
7. **Agent Workspace** -- enter an objective (a sensible default is
   pre-filled), **Create Plan**, review it, **Run Plan**, watch the trace,
   read the Claude-assisted interpretation.
8. **Report** -- **Generate Report**, download the HTML report, QC CSV,
   processed results CSV, provenance JSON, and agent trace CSV.

See [WORKSHOP_DEMO.md](WORKSHOP_DEMO.md) for a scripted walkthrough.

## Testing

```bash
LLM_MODE=mock Rscript -e "testthat::test_dir('tests/testthat')"
# or
make test
```

Tests never call the real Claude API (`LLM_MODE=mock` is enforced). A
`shinytest2` end-to-end test drives a full demo-data -> QC -> plan ->
execute -> report flow in a headless browser (skips gracefully if no
headless Chrome is available in the environment).

## CI/CD

`.github/workflows/ci.yml` runs unit tests, lints `R/`, boots the app in
mock mode with a health check, renders a test report, then builds and
boots the Docker image and verifies `http://localhost:3838`. CI never
requires or uses `ANTHROPIC_API_KEY`.

## Repository layout

```
app.R                   Shiny UI + server assembly
R/                       Deterministic science + agent infrastructure
modules/                 One Shiny module per UI section
reports/                 R Markdown report template
tests/testthat/          Unit + end-to-end tests
inst/extdata/            Bundled demo data (mzML, PSM/abundance/sample CSVs)
scripts/                 install_packages.R, generate_demo_data.R, run_app.R
www/                     Static assets (CSS)
local-data/, outputs/    Runtime volumes (mounted into the container)
Dockerfile, docker-compose.yml
.github/workflows/ci.yml
```

## Limitations

- Proprietary vendor formats (Thermo RAW, Bruker, SCIEX, Waters) are not
  read directly. Convert to mzML first, e.g. with ProteoWizard `msconvert`.
- `renv.lock` is a best-effort dependency snapshot for documentation
  purposes; `Dockerfile` / `scripts/install_packages.R` are the source of
  truth for what actually gets installed. Regenerate it locally with
  `renv::snapshot()` if you need an exact, reproducible lock for your
  environment.
- The **Stop** button in the Agent Workspace is best-effort: it is checked
  between tool-use steps, not mid-computation, since a single-process
  local Shiny app cannot pre-emptively interrupt a running R call.
- Statistical comparisons (two-group, PCA) only run when minimum replicate
  requirements are met, and are exploratory, not clinical-grade.

## License

Workshop/demonstration code. See your organization's policy before
production use.
