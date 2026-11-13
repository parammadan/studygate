# StudyGate R: demo narration

`demo/StudyGate_Demo.mp4` (59.9 s) has an **AI-generated voice**: Kokoro TTS, voice `am_onyx`,
generated locally with no paid service. It also has burned-in captions. The closing card
discloses the AI voice.

| Time | Scene | Narration |
|---|---|---|
| 0:00.0 | Title | StudyGate is an R package that checks clinical analysis datasets before analysis. |
| 0:06.1 | 1 · Reuse | Two synthetic studies. Adult: age in years, a thirty-day window. Pediatric: age in months, a fourteen-day window. Same package, different specifications. Study B passes. |
| 0:16.8 | 2 · Failure | In Study A, one adverse event belongs to a subject missing from the subject-level dataset. The gate blocks release, and explains why, what to check, and how to see the row in R. |
| 0:28.0 | 3 · Resolution | Once the data query is answered, one documented correction goes onto a copy of the source data. The same twenty-one rules run again, and the gate passes. |
| 0:38.0 | 4 · Evidence | The rules are versioned with a change log. Eighty tests pass, and each run records hashes of the spec and the data. |
| 0:47.1 | Report | The same result is exported as an HTML report, ready for code review. |
| 0:53.1 | Close | One rule set drives validation, the release gate, and programmer guidance. All data are synthetic. |

## Rebuild

```sh
~/brutalist.art/.venv/bin/python demo/voice/make_voice.py   # script.json -> audio/*.mp3, Whisper readback
python3 demo/build_video.py                                 # records real terminal scenes sized to the voice
python3 demo/build_video.py --mix-only                      # re-mix voice only (after editing a line that still fits)
```

Each scene is at least as long as its line plus 0.9 s. Terminal scenes get there by recording a
longer real pause; frames are never frozen. The voice is normalized to about −16 LUFS.

## Live version (if asked to run it in the interview)

```sh
./scripts/demo.sh      # pauses after each step; Enter to continue
```

## Honesty notes

- Everything on screen is real output from real commands. The test summary comes from the suite
  the build runs just before recording, stamped with the commit and time.
- Say "synthetic" for the data and "independent prototype" for the project. Never imply CBAR
  uses or endorsed it.
- The SAS lines in rule cards are orientation hints and have never been run (no SAS licence).
