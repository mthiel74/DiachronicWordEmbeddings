# Repo notes for Claude

## Purpose

Produce a Wolfram Community post on **diachronic word embeddings** —
200 years of semantic drift. Follow individual English words
(*gay, broadcast, awful, queer, literally, computer*) as trajectories
through a shared, time-aligned word-embedding space, one point per
decade (1800s–1990s). Pre-trained HistWords / Google-N-grams "eng-all"
SGNS embeddings.

End-product: `community/diachronic_embeddings.nb` (+ `.pdf`), in the
style of the `ENSO-emergence` repo's community notebook.

## Division of labour

- **Wolfram Language does the science**: orthogonal Procrustes
  alignment, PCA / dimensionality reduction, cosine nearest-neighbour
  search, trajectory plots, the notebook build.
- **Python does ONE thing only**: read the NumPy `.npy` matrices and
  pickled vocab files (WL has no native `.npy`/pickle import) and
  re-emit them as WL-friendly float32 binaries + plain-text vocab.
  `src/convert_histwords.py`. Nothing scientific lives in Python.

## Key technical points

- **Decades are NOT comparable as-shipped.** Each decade's SGNS space
  is trained independently → axes are an arbitrary rotation/reflection.
  Must align every decade to a reference decade (default: 1990s) via
  orthogonal Procrustes on the shared high-frequency vocabulary, using
  the SVD solution: for matrices A (this decade) and B (reference) over
  the common vocab, R = U Vᵀ where U S Vᵀ = SVD(Aᵀ B); then A·R is in
  B's frame. R is orthogonal so it preserves cosine distances *within*
  each decade while making them comparable *across* decades.
- HistWords SGNS vocab is frequency-ordered (most frequent first); the
  matrix rows correspond 1:1 to the vocab list. Verify after download.
- Embeddings are 300-dim, ~100k vocab/decade. Do NOT commit the full
  matrices. Only commit small derived products under `data/*.json`.

## Conventions (mirroring ENSO-emergence)

- Plain-text `.wls`/`.wl` is the source of truth; the `.nb` + `.pdf`
  in `community/` are committed *outputs*.
- Figures live in `docs/images/` only — referenced from README + notebook.
- `data/raw/` is git-ignored (zip, `.npy`, `.pkl`, intermediate `.f32`,
  vocab `.txt`). Tidy small derived JSON/CSV used by the pipeline +
  notebook are committed under `data/`.
- HTTP fetches: use `URLRead[HTTPRequest[...]]` and check the status
  code; avoid `URLDownload` (it silently writes error pages on 4xx/5xx).

## Commit cadence

Commit + push after each meaningful step (skeleton, fetch+convert,
loader, alignment, trajectories, neighbours, notebook). Short, factual
messages. Repo is **private** for now.
