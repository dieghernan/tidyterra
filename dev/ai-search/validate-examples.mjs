import { readdir, readFile, mkdir, writeFile } from 'node:fs/promises';
import { spawnSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import { join, resolve } from 'node:path';

// Run only the manually reviewed R blocks from this evaluation's fixed cases.
const directory = fileURLToPath(new URL('.', import.meta.url));
const root = resolve(directory, '../..');
const preview = join(directory, 'preview');
await mkdir(preview, { recursive: true });
const selected = ['01-facets', '04-mutate', '05-vector-filter', '06-rgb', '08-scales', '09-followup-base', '10-followup'];
const records = [];
const runs = (await readdir(join(directory, 'evaluations'), { withFileTypes: true }))
  .filter(entry => entry.isDirectory()).map(entry => entry.name).sort();
for (const run of runs) {
  for (const file of (await readdir(join(directory, 'evaluations', run))).filter(name => name.endsWith('.json'))) {
    const record = JSON.parse(await readFile(join(directory, 'evaluations', run, file), 'utf8'));
    if (selected.includes(record.id) && record.answer) records.push({ ...record, evidenceFile: `${run}/${file}` });
  }
}
const results = [];
for (const record of records) {
  const blocks = [...record.answer.matchAll(/```(?:r|R)?\s*\n([\s\S]*?)```/g)].map(match => match[1]);
  if (!blocks.length) throw new Error(`No R example in ${record.id}.`);
  const path = join(preview, `${record.id}.R`);
  await writeFile(path, blocks.join('\n'));
  const fixture = record.id.includes('followup')
    ? 'temp <- terra::rast(nrows = 2, ncols = 2, nlyr = 3, vals = 1:12);' : '';
  const assertions = record.id === '04-mutate'
    ? 'stopifnot(inherits(r_new, "SpatRaster"), terra::nlyr(r_new) == 1, as.numeric(terra::values(r_new)) == 10);'
    : record.id === '05-vector-filter'
      ? 'stopifnot(inherits(v_filtered, "SpatVector"), nrow(v_filtered) == 1, v_filtered$value == 15);'
      : ['01-facets', '09-followup-base', '10-followup'].includes(record.id)
        ? 'stopifnot(nrow(built$layout$layout) == 3);' : '';
  const code = `devtools::load_all(); ${fixture}
    example_result <- source(${JSON.stringify(path.replaceAll('\\', '/'))}, local = globalenv())$value;
    if (inherits(example_result, "ggplot")) built <- ggplot2::ggplot_build(example_result);
    ${assertions}
    cat("EXAMPLE PASSED\\n");
    cat("R:", as.character(getRversion()), "tidyterra:", as.character(packageVersion("tidyterra")),
      "terra:", as.character(packageVersion("terra")), "ggplot2:", as.character(packageVersion("ggplot2")), "\\n");`;
  const driver = join(preview, `${record.id}-validation.R`);
  await writeFile(driver, code);
  const started = Date.now();
  const process = spawnSync('Rscript', [driver], { cwd: root, encoding: 'utf8', timeout: 120000 });
  const result = { id: record.id, evidenceFile: record.evidenceFile, fixture: fixture || null,
    exitCode: process.status, elapsedMs: Date.now() - started, stdout: process.stdout, stderr: process.stderr,
    error: process.error?.message ?? null,
    passed: process.status === 0 && (process.stdout ?? '').includes('EXAMPLE PASSED') };
  results.push(result);
  await writeFile(join(directory, 'evaluations', 'r-validation.json'), JSON.stringify(results, null, 2));
  console.log(JSON.stringify({ id: result.id, exitCode: result.exitCode, elapsedMs: result.elapsedMs }));
}
if (results.some(result => !result.passed)) process.exitCode = 1;
