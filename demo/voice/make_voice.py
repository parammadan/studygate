"""Narration for the demo video: script.json -> audio/<id>.mp3 with Kokoro (free, local,
voice am_onyx), then Whisper reads each file back so mispronunciations are caught.

    ~/brutalist.art/.venv/bin/python demo/voice/make_voice.py [segment-id ...]
Uses the Kokoro helpers from ~/brutalist.art (see ~/walkthrough-kit/tools/tts.py).
"""
import importlib.util, json, sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
SPEED = 1.08
only = set(sys.argv[1:])
spec = importlib.util.spec_from_file_location("g", Path.home() / "brutalist.art/runtime/scripts/generate_audio_kokoro.py")
g = importlib.util.module_from_spec(spec); spec.loader.exec_module(g)
from faster_whisper import WhisperModel
k, m = g.load_engine(), WhisperModel("small.en", device="cpu", compute_type="int8")
(HERE / "audio").mkdir(exist_ok=True); total = 0
for seg in json.loads((HERE / "script.json").read_text()):
    if only and seg["id"] not in only:
        continue
    out = HERE / "audio" / f"{seg['id']}.mp3"
    s, sr = k.create(seg["text"], voice="am_onyx", speed=SPEED, lang="en-us"); g.write_mp3(s, sr, out)
    d = g.measure(out); total += d
    heard = " ".join(x.text.strip() for x in m.transcribe(str(out), vad_filter=True)[0])
    print(f"{seg['id']:5} {d:5.1f}s | {heard}")
print(f"total {total:.1f}s")
