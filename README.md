# Theoricalizer

Basic music theory in the browser, in English and Greek: intervals, scales and triads, with a theory tab for each and exercises on the staff. No backend; it installs as a PWA and works offline.

Sibling of [Chord Trainer](https://github.com/nvlass/chordtrainer), [Counterpointer](https://github.com/nvlass/counterpointer), [Dicteeineer](https://github.com/nvlass/dicteeineer) and [Harmoneer](https://github.com/nvlass/harmoneer), at [music-trainer.app](https://music-trainer.app).

## Exercises

- **Intervals**: name an interval (two notes in sequence); write one above a given lower note or below a given upper note; find the keys it belongs to.
- **Scales**: major and the three minor forms (natural, harmonic, melodic) in keys up to 7 sharps or flats: name a written scale; write one, with accidentals or with its key signature.
- **Triads**: major, minor, diminished, augmented, in root position or inversion: name one, write one, find the keys it belongs to (with its numeral).

Options: clef, how many sharps/flats the keys may have, augmented/diminished and compound intervals, which minor forms count when looking for keys (harmonic only, as in the exam question, or all three), and note names (C D E or Do Re Mi).

The theory tabs explain each topic with examples that are computed by the same code the exercises use.

## Development

```sh
npm install
npx shadow-cljs watch app      # http://localhost:8284
npx shadow-cljs compile test   # tests on node
npm run dist                   # release build into dist/
```

Piano samples: FluidR3_GM acoustic grand (via [gleitz/midi-js-soundfonts](https://github.com/gleitz/midi-js-soundfonts), CC BY 3.0).

## License

MIT, see [LICENSE](LICENSE). The piano samples are CC BY 3.0 (see above).
