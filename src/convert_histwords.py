#!/usr/bin/env python3
"""Thin Python shim: HistWords .npy/.pkl  ->  WL-friendly float32 binaries.

This is the ONLY Python in the project, and it does NO science. Wolfram
Language has no native NumPy / pickle import, so this script just reads
each decade's

    data/raw/sgns/{decade}-w.npy     (100000 x 300 float64, L2-normalized,
                                       zero rows for words absent that decade)
    data/raw/sgns/{decade}-vocab.pkl (frequency-ordered list of 100000 words)

and re-emits three tiers of plain little-endian float32 binaries plus
plain-text vocab files under data/raw/wl/, which wolfram/load_embeddings.wl
reads back with BinaryReadList.

Tiers
-----
1. anchors_{decade}.bin / anchors_vocab.txt
   The shared alignment anchors: words present (non-zero) in EVERY decade,
   top ANCHOR_K by frequency, in one fixed word order across all decades.
   Used by the orthogonal-Procrustes alignment in WL.

2. targets_{decade}.bin / targets_vocab.txt
   The handful of words we track (gay, broadcast, ...) in a fixed order.
   Rows are zero where the word is absent in that decade (WL skips those).

3. {decade}.bin / {decade}-vocab.txt
   Per-decade neighbour-search pool: the top NEIGHBOR_N most frequent
   PRESENT words that decade (native, un-aligned space — orthogonal
   alignment preserves within-decade cosine, so nearest neighbours are
   computed per decade in native coordinates).

Each .bin is row-major little-endian float32 ('<f4'); the matching
*-vocab.txt lists one word per line in the SAME row order. data/raw/wl/
is git-ignored (regenerable). A manifest.json records the shapes.
"""
import json
import os
import pickle
import sys

import numpy as np

HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.dirname(HERE)
SGNS = os.path.join(REPO, "data", "raw", "sgns")
OUT = os.path.join(REPO, "data", "raw", "wl")

DECADES = [str(y) for y in range(1800, 2000, 10)]
DIM = 300
ANCHOR_K = 5000        # shared high-frequency alignment anchors
NEIGHBOR_N = 50000     # per-decade neighbour-search pool size (capped by coverage)
TARGETS = [
    # headline six
    "gay", "broadcast", "awful", "queer", "literally", "computer",
    # curated classics (kept for clean, striking trajectories)
    "terrific", "guy", "media", "fun", "cell", "nice", "mouse", "web",
]
PRESENT_NORM = 0.5     # a row counts as "present" if ||v|| > this (real rows are ~1.0)


def load_decade(dec):
    W = np.load(os.path.join(SGNS, f"{dec}-w.npy")).astype(np.float64)
    with open(os.path.join(SGNS, f"{dec}-vocab.pkl"), "rb") as fh:
        vocab = pickle.load(fh)
    return W, list(vocab)


def write_bin(path, mat):
    """Row-major little-endian float32."""
    np.ascontiguousarray(mat, dtype="<f4").tofile(path)


def write_vocab(path, words):
    with open(path, "w", encoding="utf-8") as fh:
        fh.write("\n".join(words) + "\n")


def main():
    os.makedirs(OUT, exist_ok=True)

    # ---- Pass 1: vocab + presence only (cheap; matrices loaded one at a time) ----
    print("Pass 1: reading vocab + presence per decade ...")
    vocabs, index_of, present = {}, {}, {}
    for dec in DECADES:
        W, vocab = load_decade(dec)
        vocabs[dec] = vocab
        index_of[dec] = {w: i for i, w in enumerate(vocab)}
        norms = np.linalg.norm(W, axis=1)
        present[dec] = norms > PRESENT_NORM
        print(f"  {dec}: {int(present[dec].sum()):6d} present / {len(vocab)}")
        del W

    # ---- Anchor vocabulary: present in EVERY decade, ranked by frequency ----
    # A word's "rank cost" = its worst (largest) frequency index over the
    # decades in which it appears; smaller = more reliably frequent. We keep
    # only words present in all 20 decades, then take the ANCHOR_K best.
    common = None
    for dec in DECADES:
        s = {vocabs[dec][i] for i in np.nonzero(present[dec])[0]}
        common = s if common is None else (common & s)
    print(f"Words present in ALL decades: {len(common)}")

    def worst_rank(w):
        return max(index_of[dec][w] for dec in DECADES)

    # Deterministic: break worst_rank ties alphabetically. `common` is a set
    # whose iteration order varies between Python runs (hash randomization),
    # so without the secondary key the boundary anchors (and hence every
    # downstream Procrustes rotation) would differ slightly run-to-run.
    anchors = sorted(common, key=lambda w: (worst_rank(w), w))[:ANCHOR_K]
    write_vocab(os.path.join(OUT, "anchors_vocab.txt"), anchors)
    write_vocab(os.path.join(OUT, "targets_vocab.txt"), TARGETS)
    print(f"Anchors: {len(anchors)} (max worst-rank {worst_rank(anchors[-1])})")

    # ---- Pass 2: load each matrix once, emit the three tiers ----
    print("Pass 2: emitting WL binaries ...")
    manifest = {
        "decades": DECADES, "dim": DIM,
        "anchor_k": len(anchors), "neighbor_n": NEIGHBOR_N,
        "targets": TARGETS, "byte_order": "little", "dtype": "float32",
        "neighbor_counts": {},
    }
    for dec in DECADES:
        W, vocab = load_decade(dec)
        idx = index_of[dec]

        # tier 1: anchors (fixed order)
        write_bin(os.path.join(OUT, f"anchors_{dec}.bin"),
                  W[[idx[w] for w in anchors]])

        # tier 2: targets (zero row if absent)
        trows = np.zeros((len(TARGETS), DIM), dtype=np.float64)
        for k, w in enumerate(TARGETS):
            if w in idx and present[dec][idx[w]]:
                trows[k] = W[idx[w]]
        write_bin(os.path.join(OUT, f"targets_{dec}.bin"), trows)

        # tier 3: per-decade neighbour pool (top-N present, frequency order)
        present_rows = [i for i in range(len(vocab)) if present[dec][i]]
        present_rows = present_rows[:NEIGHBOR_N]   # vocab is frequency-ordered
        pool_words = [vocab[i] for i in present_rows]
        write_bin(os.path.join(OUT, f"{dec}.bin"), W[present_rows])
        write_vocab(os.path.join(OUT, f"{dec}-vocab.txt"), pool_words)
        manifest["neighbor_counts"][dec] = len(pool_words)
        print(f"  {dec}: anchors {len(anchors)}, neighbour-pool {len(pool_words)}")
        del W

    with open(os.path.join(OUT, "manifest.json"), "w") as fh:
        json.dump(manifest, fh, indent=2)
    print("Done. Wrote", OUT)


if __name__ == "__main__":
    sys.exit(main())
