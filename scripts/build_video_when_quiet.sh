#!/usr/bin/env bash
# Waits until the 1-minute load average is low enough for VHS to record in
# real time, then builds the demo video. Tries up to 4 times.
cd "$(dirname "$0")/.."
for attempt in 1 2 3 4; do
  until [ "$(sysctl -n vm.loadavg | awk '{print int($2)}')" -lt 8 ]; do sleep 30; done
  echo "attempt $attempt at $(date '+%H:%M:%S'), load $(sysctl -n vm.loadavg)"
  if python3 demo/build_video.py > demo/build.log 2>&1; then tail -n 1 demo/build.log; exit 0; fi
  grep -E "ratio|error|Error" demo/build.log | tail -n 3
  sleep 60
done
exit 1
