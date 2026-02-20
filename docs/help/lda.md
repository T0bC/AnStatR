# LDA / QDA — Discriminant Analysis

## What is LDA?

**Linear Discriminant Analysis (LDA)** is a supervised method that finds linear combinations of your measurement variables which maximize the separation between predefined groups (e.g., species). Unlike PCA, which maximizes total variance regardless of group labels, LDA specifically targets the directions along which groups differ most.

## What is QDA?

**Quadratic Discriminant Analysis (QDA)** relaxes LDA's assumption that all groups share the same covariance matrix. Each group gets its own covariance estimate, allowing curved (quadratic) decision boundaries. QDA is more flexible but requires more observations per group.

## When to use which?

- **LDA**: Good default when groups have roughly similar spread and you have limited observations per group.
- **QDA**: Use when groups clearly have different variances or shapes, and you have enough observations per group (at least more than the number of variables).

## Workflow

1. **Select metadata columns** — columns that describe your specimens (species, site, tooth type, etc.)
2. **Select a grouping column** — the categorical variable defining your groups (e.g., species). Must have at least 2 levels.
3. **Select measurement columns** — the numeric variables to include in the analysis.
4. **Choose data scaling** — recommended when variables have different units or magnitudes.
5. **Configure analysis settings** — choose LDA vs QDA, estimation method, priors, and cross-validation.
6. Click **Compute LDA / QDA**.

## Key settings

- **Estimation method**: "moment" (standard) is the default. "mve" and "t" provide robust alternatives when outliers are present.
- **Prior probabilities**: "Proportional" uses group sizes from your data. "Equal" treats all groups as equally likely.
- **Cross-validation**: Leave-one-out CV assesses how well the model classifies specimens without a separate test set.
- **Tolerance**: Controls singularity detection. Increase if you get singular matrix errors.

## Tips

- If you have more variables than observations per group, consider running PCA first and using the PCA scores as input (reduces dimensionality).
- The degree of overlap between groups in LDA space directly indicates how similar their measurement profiles are.
- Compare LDA and QDA results — if they differ substantially, the equal-covariance assumption of LDA may not hold.
