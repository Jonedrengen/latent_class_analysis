# Repository Guidance

## Project layout

- Put reusable analysis and plotting functions in `src/`; keep plot-specific
  functions in `src/plots/` and shared helpers in `src/utils/`.
- Keep executable analysis/model scripts in `src/` as thin orchestration
  layers. They may parse arguments, read inputs, call reusable functions, and
  save outputs, but should not contain substantial business or transformation
  logic.
- Keep raw inputs under `data/` and tests under `tests/testthat/`. Reuse or add
  small fixtures in `tests/testthat/helper-fixtures.R` rather than relying on
  local files or the current working directory.

## Design rules

- Prefer small, cohesive functions with explicit inputs and return values.
  Validation, transformation, classification, and plot construction should be
  side-effect-free whenever practical.
- Isolate side effects--file I/O, command-line parsing, model execution, and
  plot saving--at the script boundary. Pass data, paths, options, and other
  dependencies explicitly instead of reading globals or changing the working
  directory.
- Keep each module responsible for one domain concern. Do not create a generic
  utility dump or source unrelated scripts to reuse a function; extract a
  focused helper in the appropriate `src/` area instead.
- Preserve public function arguments, return types, and error messages unless
  an intentional behavior change is part of the task. Validate required input
  columns, scalar options, and data ranges at public function boundaries, with
  clear and stable errors.
- Avoid adding dependencies unless they materially simplify the implementation.
  When one is needed, add it to `environment.yml` and keep its use local to the
  code that needs it.

## Tests and verification

- Add or update focused `testthat` tests for every changed behavior. Cover the
  expected result, invalid input, and meaningful boundary cases; inspect the
  returned data or plot object rather than image pixels where possible.
- Place tests beside the feature they cover using `test-<feature>.R` naming.
  Keep fixtures deterministic, small, and independent of external data or
  network access.
- Run `Rscript tests/run_tests.R` after changes in an R environment that
  satisfies `environment.yml`. Do not treat a missing local dependency as a
  passing test run; report it clearly if the configured environment is not
  available.
