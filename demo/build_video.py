#!/usr/bin/env python3
"""Build demo/StudyGate_Demo.mp4 (about 55 s) from REAL terminal recordings.

Every terminal scene is a VHS recording (https://github.com/charmbracelet/vhs):
VHS starts a real bash shell, types the command and records what the shell
prints. Nothing is simulated or sped up. The only hidden step in each tape is
the shell setup (prompt, lower CPU priority, clear). The report scene is a
headless-Chrome screenshot of the report.html the failing run just wrote.
Title and closing cards and the caption bar are rendered from HTML.

Run from the repository root:   python3 demo/build_video.py
Needs: vhs, ffmpeg/ffprobe, Google Chrome, R with studygate installed + haven.
"""
import hashlib, html, json, os, shutil, subprocess, sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
DEMO = ROOT / "demo"
WORK = DEMO / "work"
TAPES = DEMO / "tapes"
OUT = DEMO / "StudyGate_Demo.mp4"
CHROME = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
REPO_URL = "github.com/parammadan/studygate"
W, H, TERM_H = 1920, 1080, 950
FADE = 0.3
BG = "#1e1e2e"

SG = "./bin/studygate"
SA = "inst/extdata/specs/study_a.yml"
SB = "inst/extdata/specs/study_b.yml"
DA = "inst/extdata/studies/study_a"
DB = "inst/extdata/studies/study_b"

# steps: (command, pause after typing Enter, expected command runtime in seconds)
SCENES = [
    {"id": "s01", "kind": "card", "card": "title", "secs": 4.5,
     "caption": "StudyGate R: one versioned rule set drives validation, the release gate and programmer guidance."},
    {"id": "s02", "kind": "term", "sleep": 3.5,
     "steps": [("scripts/spec_summary.R", 1.6, 0.9),
               (f"{SG} validate --spec {SB} --data {DB} | tail -n 1", 0, 0.9)],
     "caption": "1 · Reuse: two synthetic studies, two versioned specs (years vs months, severity vs grade, "
                "30- vs 14-day window). Same package, no code changes. Study B passes."},
    {"id": "s03", "kind": "term", "sleep": 7.5,
     "steps": [(f"{SG} validate --spec {SA} --data {DA} --out artifacts/before | sed -n '/^SGA-ADAE-004  /,/^GATE/p'", 0, 1.0)],
     "caption": "2 · Failure: an ADAE record for SYN-A-011, who is not in ADSL. The finding names the record, "
                "why it matters, what to check, the spec section, and R code to see the row."},
    {"id": "s04", "kind": "term", "sleep": 3.5,
     "steps": [(f"scripts/apply_correction.R {DA} artifacts/study_a_fixed", 1.6, 0.8),
               (f"{SG} validate --spec {SA} --data artifacts/study_a_fixed | tail -n 1", 0, 0.9)],
     "caption": "3 · Resolution: after the query is answered, one documented correction is applied to a copy "
                "of the source data. The same 21 rules rerun: gate PASS."},
    {"id": "s05", "kind": "term", "sleep": 4.0,
     "steps": [(f"sed -n '/^spec:/,/^codelists:/p' {SA} | head -n 12", 1.8, 0.1),
               ("cat artifacts/test-summary.txt", 1.4, 0.1),
               ("grep -E '\"(gate|spec_version|spec_md5|evidence_id)\"' artifacts/before/evidence.json", 0, 0.1)],
     "caption": "4 · Evidence: a versioned spec with a change log; the test suite (run by this build just before "
                "recording); and evidence JSON hashing spec + data + package version."},
    {"id": "s06", "kind": "image", "secs": 5.0,
     "caption": "The exported HTML report from the failing run: finding, guidance, reference and evidence, "
                "ready for code review. (Synthetic data.)"},
    {"id": "s07", "kind": "card", "card": "closing", "secs": 5.0,
     "caption": "StudyGate R: an independent research prototype. Synthetic data only; not a validated or regulatory tool."},
]


def run(cmd, **kw):
    print("+", " ".join(map(str, cmd)) if isinstance(cmd, list) else cmd, flush=True)
    return subprocess.run(cmd, check=True, **kw)


def probe(p):
    return float(subprocess.check_output(["ffprobe", "-v", "error", "-show_entries", "format=duration",
                                          "-of", "csv=p=0", str(p)]).decode())


def vhs_quote(s):
    for q in ('"', "`", "'"):
        if q not in s:
            return f"{q}{s}{q}"
    raise ValueError(f"cannot quote for VHS: {s}")


def write_tape(scene):
    raw = WORK / f"{scene['id']}_raw.mp4"
    lines = [f"Output {raw.relative_to(ROOT)}", 'Set Shell "bash"', "Set FontSize 21", 'Set FontFamily "Menlo"',
             f"Set Width {W}", f"Set Height {TERM_H}", "Set Padding 32",
             'Set Theme "Catppuccin Mocha"', "Set TypingSpeed 15ms", "Set Framerate 20",
             "Hide", "Type \"export PS1='$ ' PS2='> ' && renice -n 15 -p $$ >/dev/null && clear\"", "Enter",
             "Sleep 300ms", "Show"]
    for cmd, pause, _ in scene["steps"]:
        lines += [f"Type {vhs_quote(cmd)}", "Enter"]
        if pause:
            lines.append(f"Sleep {pause}s")
    lines.append(f"Sleep {scene['sleep']}s")
    tape = TAPES / f"{scene['id']}.tape"
    tape.write_text("\n".join(lines) + "\n")
    return tape, raw


CARD_CSS = f"""
* {{ margin:0; padding:0; box-sizing:border-box; }}
html,body {{ width:{W}px; height:{H}px; background:{BG}; color:#cdd6f4;
  font-family:-apple-system,"Helvetica Neue",Helvetica,Arial,sans-serif; }}
.wrap {{ position:absolute; inset:0; padding:110px 150px; display:flex; flex-direction:column; justify-content:center; }}
h1 {{ font-size:110px; font-weight:700; letter-spacing:-1px; color:#ffffff; }}
h2 {{ font-size:42px; font-weight:400; color:#89b4fa; margin-top:18px; line-height:1.3; }}
.mono {{ font-family:Menlo,monospace; font-size:28px; color:#a6e3a1; background:#181825;
  border-left:4px solid #89b4fa; padding:14px 22px; margin-top:12px; white-space:pre; }}
.flow {{ display:flex; gap:22px; margin-top:44px; font-size:30px; align-items:center; }}
.box {{ border:2px solid #45475a; border-radius:10px; padding:14px 22px; color:#ffffff; }}
.arrow {{ color:#89b4fa; font-size:36px; }}
.small {{ font-size:24px; color:#7f849c; margin-top:44px; line-height:1.5; }}
.url {{ font-family:Menlo,monospace; font-size:38px; color:#f9e2af; margin-top:36px; }}
.stat {{ font-family:Menlo,monospace; font-size:28px; color:#a6e3a1; margin-top:26px; }}
"""


def card_html(kind, test_line):
    if kind == "title":
        body = """<h1>StudyGate R</h1>
<h2>Study-specific validation rules and programmer guidance<br>for clinical analysis datasets</h2>
<div style="margin-top:40px"><div class="mono">result &lt;- validate_study(list(ADSL = adsl, ADAE = adae), "study_a.yml")</div></div>
<div class="small">Param Madan &middot; independent research prototype &middot; synthetic data only<br>
Not affiliated with or endorsed by Harvard, CBAR or CDISC &middot; not a validated or regulatory tool</div>"""
    else:
        body = f"""<h2 style="margin-top:0;color:#ffffff;font-size:50px">One versioned rule set, three jobs</h2>
<div class="flow"><div class="box">study spec (YAML)</div><div class="arrow">&rarr;</div>
<div class="box">validation</div><div class="box">release gate</div><div class="box">programmer guidance</div></div>
<div class="stat">{html.escape(test_line)} &middot; R CMD check: OK</div>
<div class="url">{REPO_URL}</div>
<div class="small">Synthetic data only. Not a CDISC conformance checker, not validated or regulatory-certified.<br>
Not affiliated with or endorsed by Harvard, CBAR, ACTG, IMPAACT or CDISC.</div>"""
    return f"<!doctype html><html><head><meta charset='utf-8'><style>{CARD_CSS}</style></head><body><div class='wrap'>{body}</div></body></html>"


def caption_html(text, tag):
    return f"""<!doctype html><html><head><meta charset="utf-8"><style>
* {{ margin:0; padding:0; box-sizing:border-box; }}
html,body {{ width:{W}px; height:{H - TERM_H}px; background:{BG};
  font-family:-apple-system,"Helvetica Neue",Helvetica,Arial,sans-serif; }}
.bar {{ position:absolute; inset:0; border-top:2px solid #45475a; padding:14px 48px;
  display:flex; align-items:center; gap:40px; }}
.t {{ flex:1; font-size:27px; line-height:1.3; color:#ffffff; }}
.tag {{ font-size:18px; color:#a6e3a1; text-align:right; line-height:1.4; white-space:nowrap;
  border:1px solid #45475a; border-radius:6px; padding:8px 14px; }}
</style></head><body><div class="bar"><div class="t">{html.escape(text)}</div>
<div class="tag">{tag}</div></div></body></html>"""


def shot(html_text, png, w, h, url=None, scale=1):
    if url is None:
        src = WORK / (png.stem + ".html")
        src.write_text(html_text)
        url = src.as_uri()
    run([CHROME, "--headless=new", "--disable-gpu", "--hide-scrollbars", f"--force-device-scale-factor={scale}",
         f"--window-size={w},{h}", f"--screenshot={png}", url],
        stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    if not png.exists():
        sys.exit(f"Chrome did not produce {png}")


def enc(extra_in, filt, out, dur=None):
    cmd = ["nice", "-n", "19", "ffmpeg", "-loglevel", "error", "-y", *extra_in, "-filter_complex", filt,
           "-map", "[v]", "-an", "-c:v", "libx264", "-preset", "medium", "-crf", "20",
           "-pix_fmt", "yuv420p", "-r", "30"]
    if dur:
        cmd += ["-t", f"{dur:.3f}"]
    run(cmd + [str(out)])


def srt_time(t):
    ms = int(round(t * 1000)); h, ms = divmod(ms, 3600000); m, ms = divmod(ms, 60000); s, ms = divmod(ms, 1000)
    return f"{h:02}:{m:02}:{s:02},{ms:03}"


def main():
    os.chdir(ROOT)
    for tool in ("vhs", "ffmpeg", "ffprobe", "Rscript"):
        if not shutil.which(tool):
            sys.exit(f"missing tool: {tool}")
    WORK.mkdir(parents=True, exist_ok=True); TAPES.mkdir(exist_ok=True)
    shutil.rmtree(ROOT / "artifacts", ignore_errors=True)   # nothing on screen may be stale
    (ROOT / "artifacts").mkdir()
    run(["./scripts/run_tests.sh"])                          # fails the build if any test fails
    test_line = (ROOT / "artifacts" / "test-summary.txt").read_text().splitlines()[1].replace("testthat: ", "")
    segs = []
    for sc in SCENES:
        seg = WORK / f"{sc['id']}.mp4"
        if sc["kind"] == "term":
            tag = "LIVE TERMINAL RECORDING<br>real commands &middot; real output &middot; not sped up"
            cap = WORK / f"{sc['id']}_cap.png"; shot(caption_html(sc["caption"], tag), cap, W, H - TERM_H)
            tape, raw = write_tape(sc)
            run(["vhs", str(tape)], stdout=subprocess.DEVNULL)
            d = probe(raw)
            # Real-time check: VHS drops frames under CPU load, which would make the clip play faster
            # than the session ran. Expected = typing (15 ms/char + Enter) + pauses + runtime + final sleep.
            expected = sum(len(c) * 0.015 + 0.1 + p + rt for c, p, rt in sc["steps"]) + sc["sleep"]
            sc["realtime_ratio"] = round(d / expected, 3)
            print(f"{sc['id']}: recorded {d:.2f}s, expected ~{expected:.2f}s (ratio {d / expected:.2f})")
            if not 0.9 <= d / expected <= 1.35:
                sys.exit(f"{sc['id']}: recording is not real-time (ratio {d / expected:.2f}); re-run on a quieter machine")
            enc(["-i", str(raw), "-loop", "1", "-i", str(cap)],
                f"[0:v]fps=30,pad={W}:{H}:0:0:color={BG}[b];[b][1:v]overlay=0:{TERM_H}:shortest=1,"
                f"fade=t=in:st=0:d={FADE},fade=t=out:st={d - FADE:.3f}:d={FADE},format=yuv420p[v]", seg, d)
        elif sc["kind"] == "image":
            report = ROOT / "artifacts" / "before" / "report.html"
            if not report.exists():
                sys.exit("artifacts/before/report.html missing: scene s03 must run first")
            png = WORK / f"{sc['id']}_report.png"
            shot(None, png, 1280, 633, url=report.as_uri() + "#findings", scale=1.5)   # 1920x950 px
            cap = WORK / f"{sc['id']}_cap.png"
            shot(caption_html(sc["caption"], "SCREENSHOT OF THE<br>GENERATED report.html"), cap, W, H - TERM_H)
            d = sc["secs"]
            enc(["-loop", "1", "-framerate", "30", "-i", str(png), "-loop", "1", "-framerate", "30", "-i", str(cap)],
                f"[0:v]scale={W}:{TERM_H},pad={W}:{H}:0:0:color={BG}[b];[b][1:v]overlay=0:{TERM_H},"
                f"fade=t=in:st=0:d={FADE},fade=t=out:st={d - FADE:.3f}:d={FADE},format=yuv420p[v]", seg, d)
        else:
            png = WORK / f"{sc['id']}_card.png"; shot(card_html(sc["card"], test_line), png, W, H)
            d = sc["secs"]
            enc(["-loop", "1", "-framerate", "30", "-i", str(png)],
                f"[0:v]fade=t=in:st=0:d=0.5,fade=t=out:st={d - 0.5:.3f}:d=0.5,format=yuv420p[v]", seg, d)
        segs.append((sc, seg, probe(seg)))

    lst = WORK / "list.txt"
    lst.write_text("".join(f"file '{p}'\n" for _, p, _ in segs))
    run(["ffmpeg", "-loglevel", "error", "-y", "-f", "concat", "-safe", "0", "-i", str(lst),
         "-c", "copy", "-movflags", "+faststart", str(OUT)])

    t, cues = 0.0, []
    for i, (sc, _, d) in enumerate(segs, 1):
        cues.append(f"{i}\n{srt_time(t + 0.2)} --> {srt_time(t + d - 0.2)}\n{sc['caption']}\n")
        t += d
    (DEMO / "captions.srt").write_text("\n".join(cues))
    timeline = [{"id": sc["id"], "kind": sc["kind"], "start": round(sum(x[2] for x in segs[:i]), 2),
                 "duration": round(d, 2), "realtime_ratio": sc.get("realtime_ratio"),
                 "commands": [c for c, _, _ in sc.get("steps", [])]} for i, (sc, _, d) in enumerate(segs)]
    (DEMO / "timeline.json").write_text(json.dumps(timeline, indent=1))
    sha = hashlib.sha256(OUT.read_bytes()).hexdigest()
    print(f"\n{OUT}  {probe(OUT):.2f}s  {OUT.stat().st_size} bytes  sha256 {sha}")


if __name__ == "__main__":
    main()
