# Artificial Intelligence Governance and Non-Proliferation

This public repository is the sanitized reproducibility layer for applied work on AI governance, non-proliferation, and institutional process terrain. It is intentionally narrower than the private working repository: only releasable analytical code, embedded reproducibility inputs, and public-facing documentation are included here.

## Public analytical scope

The initial release focuses on one bounded reproducibility target: representing similarity among selected multilateral AI-governance processes from a structured process-evidence profile, then visualizing that terrain without treating geometric proximity as political alignment or influence.

The public workflow contains:

1. a 7 × 19 structured process-evidence matrix embedded directly in code;
2. five equal-weight feature blocks;
3. a mixed-data structural Gower distance;
4. a Lingoes correction for non-Euclidean distance structure;
5. principal coordinates analysis (PCoA);
6. deterministic axis orientation and numerical regression checks; and
7. a publication-style strategic-terrain figure using the reproduced coordinates.

## Research boundary

This repository does **not** contain the broader private evidence corpus or the full working project. It excludes private source archives, evidence packets, working process panels, negotiation notes, internal intermediate objects, and decision-support materials.

The public release therefore supports reproduction of the process-terrain geometry and figure logic. It should not be interpreted as publication of every source, model, or analytical branch used in the broader research program.

## Repository structure

```text
.research/project.yml                    machine-readable research metadata
analysis/01_pcoa_process_terrain.R       exact public PCoA reproduction
analysis/02_strategic_terrain_figure.R   sanitized publication-style figure
data/README.md                           public/non-public data boundary
figures/                                 generated locally; not committed by default
pcoa_outputs/                            generated locally; not committed by default
REPRODUCIBILITY.md                       execution and validation contract
RIGHTS.md                                rights and reuse boundary
CITATION.cff                             citation metadata
```

## Reproduce the public baseline

From the repository root:

```bash
Rscript analysis/01_pcoa_process_terrain.R
Rscript analysis/02_strategic_terrain_figure.R
```

The coordinate workflow is base R only and contains numerical assertions. The figure workflow uses `tidyverse`, `ggplot2`, `ggforce`, `ggrepel`, and `scales`.

The expected PCoA baseline is:

```text
PCoA 1                         39.804568%
PCoA 2                         27.093710%
PCoA 1 + PCoA 2               66.898278%
Lingoes constant               0.0107432366
maximum coordinate error       < 1e-5
```

See [REPRODUCIBILITY.md](REPRODUCIBILITY.md) for the full execution contract.

## Interpretation

The process embedding measures similarity in the coded process-evidence profiles used by this release. Distance in the resulting map is **not** a direct measure of political influence, ideological alignment, policy preference, causal effect, institutional effectiveness, or national power.

The visualization also contains descriptive institutional groupings and contextual nodes. Those are presentation layers, not additional PCoA-derived dimensions.

## Methodological context

- [Dimension Reduction](https://github.com/LystadJS/method-dimension-reduction)
- [Statistical Computing and Visualization](https://github.com/LystadJS/method-statistical-computing)

## Research domains

- [Emerging Technology](https://github.com/LystadJS/domain-emerging-technology)
- [Human Security](https://github.com/LystadJS/domain-human-security)

## Portfolio

[Research portfolio entry](https://lystadjs.github.io/research.html#project-ai-nonproliferation)

## Disclosure

This is a sanitized public reproducibility repository. Public visibility does not imply that the underlying private working repository, source archive, or every evidence item has been released.

See [data/README.md](data/README.md) and [RIGHTS.md](RIGHTS.md) before redistributing materials.
