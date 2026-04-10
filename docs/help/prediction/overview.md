#### Prediction

Apply a previously trained PCA, LDA, QDA, or MDA model to new (unknown) specimens. The model bundle encodes all training decisions — the unknown data is preprocessed identically to the training data before prediction.

##### Required Inputs

Two files are required before running a prediction:

- **Model bundle (.rds)** — exported from the PCA, LDA, QDA, or MDA tab. Upload via the **Upload** sidebar tab. The bundle summary card confirms the analysis type, number of training observations, and creation date after loading
- **Unknown data (CSV or XLSX)** — the specimens you want to classify or project. Must contain **the same measurement columns** used during model training. Upload formats: CSV or Excel (`.xlsx`, first sheet)

**Critical requirement — reference population match**: the unknown data must come from the same reference population as the training data. This means specimens were measured using the same protocol, instrument, and measurement definitions. A model trained on, e.g., modern reference tooth enamel cannot be applied reliably to specimens measured with a different instrument or under different conditions, even if column names match. The bundle was built from a specific comparative reference collection; only apply it to unknowns that are reasonably assumed to be drawn from the same underlying population.

##### Workflow

1. Upload the **Model bundle (.rds)** — the bundle summary confirms the analysis type, variable count, and training size
2. Upload the **Unknown data** — a validation badge (**Ready** / warnings / errors) appears immediately
3. Configure the **Label column** in the **Plot Settings** tab to identify unknown samples by name in results and plots
4. Click **Run Prediction** — results appear in the main panel

##### Outputs After Prediction

| Analysis type | Results table | Overlay plot |
|--------------|---------------|--------------|
| **PCA** | PC scores (`Dim.1`, `Dim.2`, …) per unknown sample | Unknown triangles overlaid on training biplot |
| **LDA** | Predicted class + posterior probabilities + LD scores | Unknown triangles overlaid on LD Scores plot |
| **MDA** | Predicted class + posterior probabilities + discriminant variates | Unknown triangles overlaid on LD Scores plot |
| **QDA** | Predicted class + posterior probabilities + LD scores (companion LDA) | Unknown triangles overlaid on QDA decision boundary plot |

Unknown samples are rendered as **filled triangles** (opaque); training samples remain as **circles** (semi-transparent). Hover over any unknown triangle to see its predicted class and axis coordinates.

##### Downloads

- **Excel** — full results table (labels, predicted class, posterior probabilities, scores)
- **SVG / PNG** — overlay plot at configurable dimensions (cm)
