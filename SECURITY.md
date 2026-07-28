# Security

This project is a **localhost-only research/workshop demonstration**. It
is not hardened for multi-tenant or public deployment. That said, several
concrete guardrails are implemented and worth understanding before you
extend the app.

> Research demonstration only. Not intended for clinical diagnosis or
> clinical decision-making.

## Secret handling

- `ANTHROPIC_API_KEY` (and all other config) is read **only** from
  environment variables (`R/config.R`). It is never hard-coded, never
  written to a file the app controls, and never returned to the browser.
- `.env` is listed in `.gitignore` and `.dockerignore`. `.env.example`
  ships with placeholder values only.
- The Docker image does **not** bake in the key: `docker-compose.yml`
  injects it at container *start* via `env_file:`, not at build time.
- `R/config.R::redact_secrets()` strips API-key-shaped strings and
  `Authorization`/`x-api-key` header values from any text before it can
  reach a log, the agent trace, or the HTML report.
  `R/agent_guardrails.R::guardrail_scrub_log()` is the single choke point
  used before writing to logs.
- The Home tab shows only *whether* a key is configured and whether a live
  connection check succeeded -- never the key itself.

## Tool execution boundary

Claude (and the mock backend) can only request tools from the fixed
registry in `R/agent_registry.R` (22 tools). The Guardrail Agent
(`R/agent_guardrails.R`) is the single choke point between "the LLM asked
for tool X with arguments Y" and "R function X actually runs":

1. `guardrail_validate_call()` rejects any tool name not present in the
   registry.
2. `schema_check()` rejects missing required arguments, unknown arguments,
   wrong types, and out-of-range values (`minimum`/`maximum`/`enum`) before
   the tool's own domain validator runs.
3. Each tool's `validate()` function applies additional domain-specific
   checks (e.g. `rt_min <= rt_max`).
4. Only after both pass does `guardrail_execute_call()` run the tool's
   deterministic R function, under a soft timeout (`tool$timeout_s`, via
   `R.utils::withTimeout` when available).

**No tool ever wraps** `eval(parse(...))`, `system()`/`shell()`,
unrestricted file reads, `Sys.getenv()` passthrough, or runtime package
installation. `R/agent_guardrails.R::assert_registry_is_safe()` runs at
app startup and fails loudly if a tool name matching a forbidden pattern
(`eval`, `system`, `shell`, `exec`, `source`, `install`, ...) is ever
registered.

## Upload safety (`R/file_validation.R`)

- Extension allowlist: mzML/mzXML/MGF for spectra, CSV/TSV/TXT for PSM and
  abundance tables.
- `MAX_UPLOAD_MB` enforced before any parsing.
- Filenames are sanitized (`safe_filename()`) and re-checked with
  `assert_within_dir()` to prevent path traversal, even if a crafted name
  slipped past sanitization.
- Each Shiny session gets its own private temp directory
  (`create_session_dir()`), cleaned up on session end
  (`cleanup_session_dir()`).
- SHA-256 checksums are recorded for every imported file
  (`file_checksum()`).

## Agent execution bounds

- `MAX_AGENT_STEPS` bounds the tool-use loop (`R/agent_execution.R`) --
  the loop cannot run forever or recurse without limit.
- `MAX_TOOL_RETRIES` applies **only** to transient Claude API failures
  (timeouts, 5xx, 429) in `R/claude_client.R`; invalid scientific
  parameters are never silently retried.
- `verify_narrative_grounding()` heuristically checks that numeric claims
  in Claude's final summary are backed by numbers that actually appeared
  in tool results, surfacing a low-confidence warning rather than trusting
  the narrative blindly.

## Network exposure

- `docker-compose.yml` publishes the app as `127.0.0.1:3838:3838` --
  bound to loopback only, not `0.0.0.0`, so it is not reachable from other
  machines on your network even though Shiny listens on `0.0.0.0:3838`
  *inside* the container.
- The only outbound network call the app ever makes is to the Anthropic
  Messages API, and only when `LLM_MODE=claude` and a key is configured.
  No other outbound calls, no telemetry, no package installation at
  runtime.

## Reporting a vulnerability

This is workshop/demonstration code without a dedicated security contact.
If you find an issue, open an issue in your fork/internal tracker and
avoid including real API keys, patient data, or other sensitive material
in the report.
