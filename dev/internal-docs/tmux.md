### Where *tmux* fits in

`tmux` itself is “just” a terminal multiplexer, but in CI it becomes a **persistent, script‑controllable shell that survives across commands**.
That unlocks two distinct use–cases:

| Use‑case                   | What tmux gives you                                                                               | Typical add‑ons                                                                                                           |
| -------------------------- | ------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------- |
| **Interactive debugging**  | A live shell you (or an AI) can attach to while the job is still running                          | GitHub‑Actions wrappers around *tmate* or *upterm* start tmux and print an SSH/Web URL so you can attach from your laptop |
| **Headless agent sandbox** | A detached session the LLM can drive with `tmux send-keys …` and read with `tmux capture-pane -p` | A tiny wrapper script or function‑calling layer that turns “run this command” into the appropriate send/capture calls     |

Below are the concrete patterns that work today (June 2025).

---

## 1 · Interactive “open a shell” steps

### a.  **`mxschmitt/action‑tmate`**

```yaml
steps:
  - uses: actions/checkout@v4
  - name: Open live shell
    uses: mxschmitt/action-tmate@v3          # waits until you detach
```

The step launches an SSH‑ready `tmate` session **running inside tmux**; the connection string is printed in the job log. You (or anyone you share the URL with) can attach, inspect the workspace, rerun tests, even edit files, then detach and let the workflow continue.

### b.  **`lhotari/action‑upterm`** (uses *upterm* + tmux)

Same idea—SSH in, but based on an open‑source upterm server that you can self‑host and lock down to specific GitHub accounts:

```yaml
- name: Upterm debug session (only on failure)
  if: failure()
  uses: lhotari/action-upterm@v1
  with:
    limit-access-to-actor: true
    wait-timeout-minutes: 10
```

**When does this help an AI agent?**
If your Claude/Cursor agent lives *outside* the runner (for example in a Slack bot), you can programmatically read the SSH URI from the job output and have the bot connect over SSH to run fixes, then push a commit/PR.

---

## 2 · Embedding tmux as a “local REPL” for the LLM

If you prefer the agent to run *inside* the GitHub runner (no outbound SSH), treat tmux as a private command bus:

1. **Install & start a detached session early in the job**

```bash
sudo apt-get update && sudo apt-get install -y tmux
tmux new-session  -d  -s agent  -n shell
```

2. **Expose two helper scripts** (very small):

```bash
# run.sh
tmux send-keys  -t agent:0  "$*" C-m

# grab.sh
tmux capture-pane -t agent:0 -p -S -32768
```

3. **Call those helpers from your agent code** (Python example):

```python
subprocess.run(["./run.sh", "pytest -q"])
out = subprocess.check_output(["./grab.sh"]).decode()
```

The model receives the command output as context, decides on the next action, and loops until tests pass.

The approach is reliable because **tmux keeps state** (current directory, environment variables, background processes) between calls, and because it is trivial to script with `send-keys`/`capture-pane` as shown in community examples.

---

## 3 · Putting it together in a workflow

```yaml
name: self-healing-ci
on: [push]

jobs:
  repair:
    runs-on: ubuntu-latest
    timeout-minutes: 30

    steps:
    - uses: actions/checkout@v4

    - name: Install tmux
      run: sudo apt-get update && sudo apt-get install -y tmux

    - name: Bootstrap tmux sandbox
      run: |
        tmux new-session -d -s agent -n shell
        echo '#!/usr/bin/env bash' > run.sh
        echo 'tmux send-keys -t agent:0 "$*" C-m' >> run.sh
        chmod +x run.sh
        echo '#!/usr/bin/env bash' > grab.sh
        echo 'tmux capture-pane -t agent:0 -p -S -32768' >> grab.sh
        chmod +x grab.sh

    - name: Launch AI repair loop
      env:
        ANTHROPIC_API_KEY: ${{ secrets.ANTHROPIC_API_KEY }}
      run: python scripts/ci_agent.py   # your Claude/Cursor driver

    # optional: open interactive shell if the loop failed
    - name: Manual rescue (tmate)
      if: failure()
      uses: mxschmitt/action-tmate@v3
```

### Why this is safe(‑ish)

* The LLM can **only** issue shell commands through `run.sh`.
  You can whitelist/blacklist dangerous commands in that wrapper if needed.
* The job’s network is already sandboxed by GitHub Actions; malicious commands cannot reach internal resources unless you inject secrets.
* You still get a normal PR diff or a separate “repair” branch—merge only after review.

---

## 4 · Pros & cons of the tmux path

| ✔️  Strengths                                                                      | ❌  Trade‑offs                                                                                                  |
| ---------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------- |
| Zero extra cloud resources—everything lives in the standard runner.                | Session vanishes when the job ends; not ideal for very long‑running fixes unless you use a self‑hosted runner. |
| Scriptable API (`send-keys`, `capture-pane`) is UNIX‑simple and language‑agnostic. | Output parsing is plain text—you must teach your agent to locate error messages.                               |
| Human operators can still attach with tmate/upterm if the bot gets stuck.          | Each step that executes the agent costs runner minutes; infinite loops need a timeout guard.                   |

---

### When to choose tmux vs. higher‑level agents

*Pick tmux* **if** you need a lightweight, transparent sandbox that you can trouble‑shoot yourself and you are comfortable writing a little glue code.

*Pick an autonomous framework (OpenHands, Dagger self‑healing, etc.)* when you’d rather install a turnkey Action and let it manage the loop logic for you.

Either way, tmux remains a handy fallback: every mature “self‑healing” Action still drops to a tmate/upterm shell on failure so you—or the AI—can dive in live.
