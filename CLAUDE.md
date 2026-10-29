# Theoricalizer

Static re-frame/shadow-cljs SPA for basic theory (intervals, scales, triads), no backend. Sibling of `../harmoneer` and the other music-trainer apps (same stack; `audio`, `guard` and the samples were copied from harmoneer). See README.

## Commands

- `npx shadow-cljs watch app`: dev server on :8284
- `npx shadow-cljs compile test`: runs `test/` on node
- `npm run dist`: release build into `dist/`

## Conventions

- `dist/` is committed on purpose. Run `npm run dist` before committing changes to `src/` or `resources/public/`.
- Notes are `{:s step :a alt}` (C4 = 28), spellings `{:l letter :a alt}`, keys `{:tonic {:l :a} :mode :major|:minor}` with the natural-minor signature. Scale types are `:major :natural :harmonic :melodic` (`t/patterns`); minor keys count the forms in `exercise/forms` (harmonic only by default).
- `theory.cljs` and `exercise.cljs` are pure. `exercise/generate` is a multimethod on `:kind` taking a rand fn; `exercise/check` grades an answer. The test `every-exercise-generates-and-its-answer-checks` runs every kind over the option combinations: run it after touching the generators.
- Intervals are always shown melodically, lower note first. Writing exercises keep the user's notes in `:answer` as columns (vectors of notes); `:given` columns can't be edited, `:at` is the column an interval is written in.
- `staff.cljs` is one staff of whole notes; display-only staves are cropped to their notes, writable ones keep the full range.
- Don't pass lazy seqs that call `tr` as component arguments: Reagent compares props outside a reactive context. Use `vec`, or call the helper as a function (lessons' `example`).
- Persistence: IndexedDB in its own store `theoricalizer` (idb-keyval), keys `settings` and `stats`, as EDN strings. New setting keys need a default in `events/default-settings`.
- `resources/public/sw.js` lists the app shell and samples; bump `VERSION` when the cached set changes.

## Roadmap

`TODO.md`.
