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
- **literally** — *word-for-word, exactly*; the modern intensifier sense is
  largely spoken and barely surfaces in the book corpus — a deliberate
  cautionary case (see §5 of the notebook)
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
wolfram/trajectories.wls   ── Procrustes alignment + 2-D projection → trajectories.json, neighbors.json
wolfram/laws_and_drift.wls ── drift leaderboard + the two laws → laws.json, laws_points.json, drift_ranking.json
wolfram/alignment.wls      ── alignment diagnostic → alignment.json
        ▼
wolfram/DiachronicEmbeddings.wl   ── rendering/analysis package (reads data/*.json)
community/bundle_package.wls      ── folds package + data/ into the self-contained community/DiachronicEmbeddings.wl
community/build_notebook.wls      ── assembles the Wolfram Community notebook (.nb + .pdf)
```

## Reproducing

```sh
# 1. Download + unzip the HistWords eng-all SGNS embeddings into data/raw/
wolframscript -file wolfram/fetch_data.wls

# 2. Convert .npy/.pkl to WL-friendly binaries (thin Python shim)
python3 src/convert_histwords.py

# 3. Align decades; compute trajectories, neighbours, laws, drift → data/*.json
wolframscript -file wolfram/trajectories.wls
wolframscript -file wolfram/laws_and_drift.wls
wolframscript -file wolfram/alignment.wls

# 4. Bundle the self-contained package, then build the community notebook
wolframscript -file community/bundle_package.wls
wolframscript -file community/build_notebook.wls
```

## Posting to Wolfram Community

The notebook is self-contained from **two files only** — upload both,
in the same folder:

- `community/diachronic_embeddings.nb`
- `community/DiachronicEmbeddings.wl`  *(carries the code **and** the embedded data)*

The notebook's Setup cell loads the `.wl` from its own directory; no
repository checkout and no raw embeddings are needed.

## License & data

Code is released into the **public domain** (The Unlicense — see
[`LICENSE`](LICENSE)); reuse with no conditions. The HistWords embeddings are
public-domain (Open Data Commons **PDDL v1.0**), so the derived products
committed under `data/` are unrestricted; the underlying Google Books
Ngram corpus is **CC BY 3.0** and WordNet is © Princeton (free licence).
Full attribution is in the notebook's *Data, licensing, and
acknowledgements* section.

## Status

✅ **Complete.** Self-contained Wolfram Community notebook built and
verified to run from the two upload files alone.

## References

- W. L. Hamilton, J. Leskovec, D. Jurafsky (2016). *Diachronic Word
  Embeddings Reveal Statistical Laws of Semantic Change.* ACL 2016.
  [arXiv:1605.09096](https://arxiv.org/abs/1605.09096)
- HistWords project & data: https://nlp.stanford.edu/projects/histwords/
