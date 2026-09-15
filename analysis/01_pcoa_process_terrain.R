# ==============================================================================
# AI GOVERNANCE STRATEGIC TERRAIN
# Exact R reproduction of the PCoA coordinate-generation pipeline
#
# This script reproduces the process-terrain venue coordinates used in the
# publication figure.
#
# Pipeline:
#   1. Define the 19-feature process-evidence matrix.
#   2. Divide features into five conceptual blocks.
#   3. Give each block equal total weight.
#   4. Compute a mixed-data structural Gower distance:
#        - both values NA        -> distance contribution 0
#        - one NA, one observed  -> distance contribution 1
#        - both observed         -> absolute difference
#   5. Apply a Lingoes correction if the raw distance matrix yields negative
#      PCoA eigenvalues.
#   6. Perform principal coordinates analysis (PCoA) by eigendecomposition.
#   7. Orient the axes deterministically so the coordinates match the published
#      strategic-terrain figure.
#   8. Export coordinates, eigenvalues, variance shares, and distance matrices.
#
# No external R packages are required.
#
# Expected result:
#   PCoA 1 = 39.8046% of corrected positive inertia
#   PCoA 2 = 27.0937%
#   Combined = 66.8983%
#   Lingoes constant = 0.0107432366
#
# NOTE ON AXIS SIGNS:
# Eigenvector signs are mathematically arbitrary. A PCoA solution can be
# reflected left/right or up/down without changing any distances. This script
# fixes the signs to reproduce the orientation used in the publication figure:
#   - CSTD 29 is positive on PCoA 1
#   - ITU C24/85 is positive on PCoA 2
# ==============================================================================

OUT_DIR <- "pcoa_outputs"
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

base_features <- c(
  "Initial proposal/concept",
  "Successive drafts",
  "Final adopted text",
  "U.S. actor evidence",
  "China actor evidence",
  "EU actor evidence",
  "Other actor evidence",
  "Sponsorship/co-sponsorship",
  "Joint statement/coalition",
  "Meeting records/transcripts",
  "Vote/EOV",
  "Formal roles",
  "Procedural decisions",
  "Unsuccessful/withdrawn",
  "Funding/TA",
  "Paragraph-level citations"
)

inference_features <- c(
  "Comparable public-issue denominator",
  "Proposal-survival usable",
  "Full source-body access"
)

all_features <- c(base_features, inference_features)

X <- rbind(
  "GDC" = c(
    1, 1, 1,
    0.5, 0.5, 0.5, 0.5,
    NA, 1,
    0.5, 1, 1, 0.5, 0.5, 0.5, 0.5,
    1, 1, 1
  ),
  "AI Panel / Dialogue" = c(
    1, 1, 1,
    0.5, 0.5, 0.5, 0.5,
    0.5, 1,
    0.5, 1, 1, 0.5, 0.5, 0.5, 0.5,
    1, 1, 1
  ),
  "ITU C24/85" = c(
    1, 1, 1,
    0, 1, 0, 1,
    1, 1,
    1, 0, 0.5, 1, 1, NA, 0.5,
    0.5, 1, 1
  ),
  "WSIS+20" = c(
    1, 1, 1,
    1, 0.5, 1, 1,
    NA, 1,
    0.5, 1, 0.5, 0.5, 1, 0.5, 1,
    1, 1, 1
  ),
  "Global Dialogue 2026" = c(
    0.5, NA, NA,
    0.5, 0.5, 1, 0.5,
    NA, 1,
    0.5, NA, 0.5, 0.5, 0, 0.5, 0.5,
    1, 0, 1
  ),
  "CSTD 29" = c(
    0, 0, 1,
    0.5, 0.5, 0.5, 0.5,
    0, 0,
    1, 1, 0.5, 1, 0.5, 0.5, 1,
    0, 0, 1
  ),
  "UNESCO AI Ethics" = c(
    1, 0.5, 1,
    NA, 0.5, 0.5, 0.5,
    NA, 0.5,
    0.5, 0, 1, 1, 0, 0.5, 0.5,
    0, 0, 0.5
  )
)

colnames(X) <- all_features
stopifnot(ncol(X) == 19)
stopifnot(nrow(X) == 7)

blocks <- list(
  "Document lineage" = c(
    "Initial proposal/concept",
    "Successive drafts",
    "Final adopted text"
  ),
  "Actor & coalition evidence" = c(
    "U.S. actor evidence",
    "China actor evidence",
    "EU actor evidence",
    "Other actor evidence",
    "Sponsorship/co-sponsorship",
    "Joint statement/coalition"
  ),
  "Process mechanics & disposition" = c(
    "Meeting records/transcripts",
    "Vote/EOV",
    "Formal roles",
    "Procedural decisions",
    "Unsuccessful/withdrawn",
    "Funding/TA"
  ),
  "Citation granularity" = c("Paragraph-level citations"),
  "Inference readiness" = c(
    "Comparable public-issue denominator",
    "Proposal-survival usable",
    "Full source-body access"
  )
)

block_membership <- unlist(blocks, use.names = FALSE)
stopifnot(
  length(block_membership) == length(all_features),
  setequal(block_membership, all_features)
)

feature_weights <- numeric(length(all_features))
names(feature_weights) <- all_features
for (block_name in names(blocks)) {
  block_features <- blocks[[block_name]]
  feature_weights[block_features] <- 1 / length(block_features)
}
block_weight_check <- sapply(
  blocks,
  function(features) sum(feature_weights[features])
)
stopifnot(all(abs(block_weight_check - 1) < 1e-12))

structural_gower <- function(data_matrix, weights) {
  data_matrix <- as.matrix(data_matrix)
  n <- nrow(data_matrix)
  D <- matrix(
    0,
    nrow = n,
    ncol = n,
    dimnames = list(rownames(data_matrix), rownames(data_matrix))
  )

  for (i in seq_len(n - 1)) {
    for (j in (i + 1):n) {
      diffs <- numeric(ncol(data_matrix))
      for (k in seq_len(ncol(data_matrix))) {
        a <- data_matrix[i, k]
        b <- data_matrix[j, k]
        if (is.na(a) && is.na(b)) {
          d <- 0
        } else if (xor(is.na(a), is.na(b))) {
          d <- 1
        } else {
          d <- abs(a - b)
        }
        diffs[k] <- d
      }
      dij <- weighted.mean(diffs, weights)
      D[i, j] <- dij
      D[j, i] <- dij
    }
  }
  D
}

D_raw <- structural_gower(X, feature_weights)

pcoa_eigendecomp <- function(D) {
  D <- as.matrix(D)
  n <- nrow(D)
  J <- diag(n) - matrix(1 / n, nrow = n, ncol = n)
  B <- -0.5 * J %*% (D ^ 2) %*% J
  eig <- eigen(B, symmetric = TRUE)
  list(
    eigenvalues = eig$values,
    eigenvectors = eig$vectors,
    B = B
  )
}

pcoa_lingoes <- function(D, tolerance = 1e-12) {
  D <- as.matrix(D)
  raw <- pcoa_eigendecomp(D)
  lambda_min <- min(raw$eigenvalues)
  lingoes_constant <- 0
  D_corrected <- D

  if (lambda_min < -tolerance) {
    lingoes_constant <- -lambda_min
    n <- nrow(D)
    off_diagonal <- matrix(1, nrow = n, ncol = n) - diag(n)
    D2_corrected <- (D ^ 2) + (2 * lingoes_constant * off_diagonal)
    D_corrected <- sqrt(pmax(D2_corrected, 0))
    diag(D_corrected) <- 0
    dimnames(D_corrected) <- dimnames(D)
  }

  corrected <- pcoa_eigendecomp(D_corrected)
  positive <- corrected$eigenvalues > tolerance
  positive_eigenvalues <- corrected$eigenvalues[positive]
  positive_eigenvectors <- corrected$eigenvectors[, positive, drop = FALSE]
  coordinates <- sweep(
    positive_eigenvectors,
    MARGIN = 2,
    STATS = sqrt(positive_eigenvalues),
    FUN = "*"
  )
  rownames(coordinates) <- rownames(D)
  variance_share <- positive_eigenvalues / sum(positive_eigenvalues)

  list(
    raw_eigenvalues = raw$eigenvalues,
    corrected_eigenvalues = corrected$eigenvalues,
    positive_eigenvalues = positive_eigenvalues,
    variance_share = variance_share,
    coordinates = coordinates,
    lingoes_constant = lingoes_constant,
    corrected_distance = D_corrected
  )
}

pcoa_fit <- pcoa_lingoes(D_raw)
coords <- as.data.frame(pcoa_fit$coordinates[, 1:2, drop = FALSE])
names(coords) <- c("PCoA1", "PCoA2")
coords$venue <- rownames(coords)
coords <- coords[, c("venue", "PCoA1", "PCoA2")]

if (coords$PCoA1[coords$venue == "CSTD 29"] < 0) {
  coords$PCoA1 <- -coords$PCoA1
}
if (coords$PCoA2[coords$venue == "ITU C24/85"] < 0) {
  coords$PCoA2 <- -coords$PCoA2
}

variance_summary <- data.frame(
  dimension = paste0("PCoA", seq_along(pcoa_fit$variance_share)),
  eigenvalue = pcoa_fit$positive_eigenvalues,
  share = pcoa_fit$variance_share,
  percent = 100 * pcoa_fit$variance_share
)

dim1_pct <- 100 * pcoa_fit$variance_share[1]
dim2_pct <- 100 * pcoa_fit$variance_share[2]
dim12_pct <- 100 * sum(pcoa_fit$variance_share[1:2])

write.csv(coords, file.path(OUT_DIR, "pcoa_coordinates.csv"), row.names = FALSE)
write.csv(D_raw, file.path(OUT_DIR, "gower_distance_matrix.csv"), row.names = TRUE)
write.csv(
  pcoa_fit$corrected_distance,
  file.path(OUT_DIR, "gower_distance_matrix_lingoes_corrected.csv"),
  row.names = TRUE
)
write.csv(
  variance_summary,
  file.path(OUT_DIR, "pcoa_eigenvalue_summary.csv"),
  row.names = FALSE
)
write.csv(
  data.frame(feature = names(feature_weights), weight = as.numeric(feature_weights)),
  file.path(OUT_DIR, "pcoa_feature_weights.csv"),
  row.names = FALSE
)
write.csv(
  X,
  file.path(OUT_DIR, "pcoa_input_feature_matrix.csv"),
  row.names = TRUE,
  na = "NA"
)
write.csv(
  data.frame(
    metric = c(
      "Lingoes constant",
      "PCoA 1 percent",
      "PCoA 2 percent",
      "PCoA 1-2 combined percent"
    ),
    value = c(
      pcoa_fit$lingoes_constant,
      dim1_pct,
      dim2_pct,
      dim12_pct
    )
  ),
  file.path(OUT_DIR, "pcoa_diagnostics.csv"),
  row.names = FALSE
)

expected_coords <- data.frame(
  venue = c(
    "GDC",
    "AI Panel / Dialogue",
    "ITU C24/85",
    "WSIS+20",
    "Global Dialogue 2026",
    "CSTD 29",
    "UNESCO AI Ethics"
  ),
  PCoA1 = c(
    -0.115746,
    -0.115620,
    -0.201763,
    -0.098116,
     0.077909,
     0.322772,
     0.130564
  ),
  PCoA2 = c(
    -0.046991,
    -0.008983,
     0.113429,
     0.105182,
    -0.284644,
     0.171558,
    -0.049551
  )
)

check <- merge(
  coords,
  expected_coords,
  by = "venue",
  suffixes = c("_observed", "_expected")
)

max_coordinate_error <- max(
  abs(check$PCoA1_observed - check$PCoA1_expected),
  abs(check$PCoA2_observed - check$PCoA2_expected)
)

stopifnot(
  abs(dim1_pct - 39.804568) < 0.001,
  abs(dim2_pct - 27.093710) < 0.001,
  abs(dim12_pct - 66.898278) < 0.001,
  abs(pcoa_fit$lingoes_constant - 0.0107432366) < 1e-8,
  max_coordinate_error < 1e-5
)

cat("\n")
cat("============================================================\n")
cat("AI GOVERNANCE PCoA REPRODUCTION COMPLETE\n")
cat("============================================================\n\n")
cat(sprintf("Lingoes constant: %.10f\n", pcoa_fit$lingoes_constant))
cat(sprintf("PCoA 1: %.4f%%\n", dim1_pct))
cat(sprintf("PCoA 2: %.4f%%\n", dim2_pct))
cat(sprintf("PCoA 1 + 2: %.4f%%\n", dim12_pct))
cat(sprintf("Maximum coordinate validation error: %.10f\n", max_coordinate_error))
cat("\nCoordinates:\n")
print(coords, row.names = FALSE, digits = 6)
cat("\nFiles written to:\n")
cat(normalizePath(OUT_DIR, mustWork = FALSE), "\n\n")
