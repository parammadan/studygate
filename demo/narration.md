# StudyGate R: demo narration (about 55 seconds)

The video has burned-in captions and no voice. This script is for an optional voice-over or
for narrating live. About 140 words, timed to the scenes in `demo/build_video.py`. Times are
approximate; check `demo/timeline.json` after a build.

| Time | Scene | Say |
|---|---|---|
| 0:00 | Title | "StudyGate is an R package that turns a study's analysis-data requirements into versioned, executable rules." |
| 0:05 | 1 · Reuse | "Here are two synthetic studies: an adult study with age in years and a 30-day window, and a pediatric study with age in months, toxicity grades and a 14-day window. Same package, different specifications. Study B passes." |
| 0:16 | 2 · Failure | "In Study A, one adverse event belongs to SYN-A-011, a subject who isn't in ADSL. StudyGate blocks release and explains why that matters, what to check, which spec section applies, and the R code that shows the row." |
| 0:28 | 3 · Resolution | "After the data query is answered, a documented correction goes onto a copy of the source data. The same 21 rules rerun, and the gate passes." |
| 0:39 | 4 · Evidence | "The rules are versioned with a change log, the test suite passes, and every run writes evidence: hashes of the spec and the data." |
| 0:49 | Report + close | "The same rule set drives validation, the release gate and the guidance a programmer reads. All data here are synthetic." |

## Live version (if asked to run it in the interview)

```sh
DEMO_NO_PAUSE= ./scripts/demo.sh      # pauses after each step; Enter to continue
```

## Honesty notes

- Everything on screen is real output from real commands. The test summary is from the suite
  the build runs just before recording, stamped with commit and time.
- Say "synthetic" for the data and "independent prototype" for the project. Never imply CBAR uses
  or endorsed it.
- The SAS lines in rule cards are orientation hints and have never been run (no SAS licence).
