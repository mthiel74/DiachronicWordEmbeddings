# Diachronic Word Embeddings — 200 Years of Semantic Drift

Tracking how the *meanings* of English words have drifted over two
centuries, by following individual words as **trajectories through a
shared word-embedding space**, one point per decade from the 1800s to
the 1990s.

The headline words — **gay, broadcast, awful, queer, literally,
computer** — each tell a compact cognitive-historical story:

- **gay** — *cheerful, lively* → *homosexual*
- **broadcast** — *to scatter seed by hand* → *to transmit a signal*
- **awful** — *awe-inspiring, solemn* → *terrible, very bad*
- **queer** — *strange, peculiar* → *(reclaimed) LGBTQ identity*
- **literally** — *word-for-word, exactly* → *intensifier ("figuratively")*
- **computer** — *a person who computes* → *an electronic machine*

The visuals — the path of a word drifting across a 2-D projection of
the embedding space, with the nearest neighbours that anchor each end
of the journey — are the point of the project.

## Data

Pre-trained diachronic embeddings from **HistWords** (Hamilton, Leskovec
& Jurafsky, ACL 2016), SGNS / word2vec vectors trained per decade on the
**Google N-grams "eng-all"** corpus (300-dimensional, 20 decades,
1800s–1990s). This is the same corpus and embedding family behind the
canonical figures in *"Diachronic Word Embeddings Reveal Statistical
Laws of Semantic Change"*.

> The raw HistWords download (`eng-all_sgns.zip`, ~1.6 GB) is **not**
> committed — it lives under the git-ignored `data/raw/`. Only small,
> tidy derived products (aligned target vectors, nearest-neighbour
> tables, 2-D trajectory coordinates) are committed under `data/`.

Each decade's embedding is trained independently, so the coordinate
axes are arbitrary and **not comparable across decades**. Before any
word can be tracked through time, the decades must be brought into a
common frame via **orthogonal Procrustes alignment** (rotate each
decade onto a reference decade using the shared high-frequency
vocabulary). That alignment is the mathematical heart of the project.

## Pipeline

The science — alignment, projection, nearest-neighbour search,
plotting — is **pure Wolfram Language**. A thin Python shim handles only
the one thing WL cannot do natively: read NumPy `.npy` matrices and
pickled vocab files.

```
data/raw/eng-all_sgns.zip                (git-ignored, ~1.6 GB)
        │  unzip
        ▼
data/raw/sgns/{decade}-w.npy + {decade}-vocab.pkl
        │  src/convert_histwords.py   (Python: .npy/.pkl → WL-friendly binary + vocab)
        ▼
data/raw/wl/{decade}.f32 + {decade}-vocab.txt   (git-ignored intermediate)
        │  wolfram/load_embeddings.wl   (shared loader package)
        ▼
wolfram/align.wls       ── orthogonal Procrustes: rotate every decade onto a reference
wolfram/trajectories.wls── extract target-word paths, project to 2-D, render
wolfram/neighbors.wls   ── nearest neighbours of each target word, per decade
wolfram/run_all.wls     ── one entry point → writes docs/images/*.png + data/*.json

community/build_notebook.wls   ── assembles the Wolfram Community notebook (.nb + .pdf)
```

## Reproducing

```sh
# 1. Download + unzip the HistWords eng-all SGNS embeddings into data/raw/
wolframscript -file wolfram/fetch_data.wls          # (or see data/raw/ below)

# 2. Convert .npy/.pkl to WL-friendly binaries (thin Python shim)
python3 src/convert_histwords.py

# 3. Align decades, compute trajectories + neighbours, render figures
wolframscript -file wolfram/run_all.wls

# 4. Build the community notebook
wolframscript -file community/build_notebook.wls
```

## Status

🚧 **Exploration phase.** Setting up the data pipeline and validating
the alignment + trajectory approach in Wolfram Language. Target
end-product: a self-contained Wolfram Community post.

## References

- W. L. Hamilton, J. Leskovec, D. Jurafsky (2016). *Diachronic Word
  Embeddings Reveal Statistical Laws of Semantic Change.* ACL 2016.
  [arXiv:1605.09096](https://arxiv.org/abs/1605.09096)
- HistWords project & data: https://nlp.stanford.edu/projects/histwords/
