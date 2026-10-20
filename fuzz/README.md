# Differential fuzzing

`powershell_differential` compares this crate's parser against the official PowerShell `7.6.6` parser (`Parser.ParseInput`), reached through the persistent oracle in `oracle.ps1`. The oracle only parses, it never executes. Every disagreement panics with both projections, which `ClusterFuzzLite` reports as a crash.

## Running locally

```sh
POWERSHELL_FUZZ_PWSH=/path/to/pwsh cargo +nightly fuzz run --fuzz-dir . powershell_differential seeds/powershell_differential
```

Needs a local [PowerShell `7.6.6`](https://learn.microsoft.com/en-us/powershell/scripting/install/installing-powershell), on `PATH` as `pwsh` or pointed at through `POWERSHELL_FUZZ_PWSH`. `ClusterFuzzLite` bundles its own pinned runtime instead.

## Seed corpus

`seeds/powershell_differential/` holds sources where the frozen `main` grammar and the oracle currently agree. It excludes every source already known to disagree, so a clean `ClusterFuzzLite` run reports only genuinely new divergences. The empty input is separately rejected in `src/lib.rs`, since `libFuzzer` always replays it and the grammar rejects it while the oracle accepts it.
