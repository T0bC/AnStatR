flowchart TB
    subgraph TexAn["TexAn2.0 Application"]
        direction TB

        %% ==================== LOAD DATA MODULE ====================
        subgraph LoadDataModule["📥 Load Data Module"]
            direction TB

            LD_Source{"Input source"}

            LD_Upload["User uploads a file
            ---
            CSV · XLSX"]

            LD_Example["Built-in example dataset
            ---
            📦 openxlsx"]

            LD_CSVOpts["CSV Parsing Options
            ---
            Header row: yes / no
            Delimiter: comma · semicolon · tab
            Quote character: none · double · single"]

            LD_Read["Parse file into data frame
            ---
            📦 utils · openxlsx"]

            LD_Validate["Validate structure · fix column names
            Flag ambiguous / renamed columns
            ---
            📦 tools"]

            LD_ColModal["Notify user of column renames"]
            LD_ErrDisplay["Display structured error on failure"]
            LD_Log["Log load event
            ---
            📦 rhino"]

            LD_Preview["Interactive data table
            ---
            📦 DT"]

            LD_Missing["Missing values bar chart
            ---
            📦 DataExplorer · ggplot2"]

            LD_Summary["Descriptive data summary
            ---
            📦 summarytools"]

            LD_Source -->|file| LD_Upload
            LD_Source -->|example| LD_Example
            LD_Upload -->|CSV| LD_CSVOpts --> LD_Read
            LD_Upload -->|XLSX| LD_Read
            LD_Example --> LD_Read
            LD_Read -->|success| LD_Validate
            LD_Read -->|failure| LD_ErrDisplay
            LD_Read --> LD_Log
            LD_Validate --> LD_ColModal
            LD_Validate --> LD_Preview
            LD_Validate --> LD_Missing
            LD_Validate --> LD_Summary
        end

        %% ==================== MEDIAN MODULE ====================
        subgraph MedianModule["🔧 Median & Quality Filter Module"]
            direction TB

            subgraph MED_Config["Configuration"]
                direction LR
                MED_OptGrouping["Grouping columns
                ---
                None (filter whole dataset) · 1+ descriptive columns"]
                MED_OptQuality["Quality column & threshold
                ---
                None · categorical bad-value list · numeric minimum threshold"]
            end

            MED_QualityFilter["Apply quality filter to raw data
            ---
            Without grouping: remove bad rows globally
            With grouping: remove bad rows only from groups
            that contain at least one good value;
            groups where ALL values are bad are kept intact"]

            MED_GroupBranch{"Grouping selected?"}

            MED_NoGroup["Pass quality-filtered rows as-is
            ---
            No median aggregation; quality column dropped"]

            MED_Medians["Compute per-group medians
            ---
            Aggregate numeric measurement columns by grouping keys;
            descriptive columns that vary within groups are dropped
            📦 stats"]

            MED_TableNoGroup["Interactive result table — raw rows
            ---
            Each row is one quality-filtered measurement;
            Excel-style column filters remove individual rows
            📦 DT"]

            MED_TableGrouped["Interactive result table — median rows
            ---
            Each row is one aggregated group median;
            Excel-style column filters remove whole group-median rows
            📦 DT"]

            MED_ExcelFilter["Apply active DT column filters
            ---
            Downstream modules and download receive only
            the rows visible after the user's table filters"]

            MED_Download["Export visible rows as XLSX
            ---
            📦 openxlsx"]

            MED_Log["Log processing event
            ---
            📦 rhino"]

            MED_Config --> MED_QualityFilter
            MED_QualityFilter --> MED_GroupBranch
            MED_GroupBranch -->|no grouping| MED_NoGroup
            MED_GroupBranch -->|grouping| MED_Medians
            MED_NoGroup --> MED_TableNoGroup
            MED_Medians --> MED_TableGrouped
            MED_TableNoGroup --> MED_ExcelFilter
            MED_TableGrouped --> MED_ExcelFilter
            MED_ExcelFilter --> MED_Download
            MED_ExcelFilter --> MED_Log
        end

        %% ==================== PLOTTING MODULE ====================
        subgraph PlottingModule["📈 Plotting Module"]
            direction TB

            PL_Input["Receive analysis data from Median module
            ---
            median-aggregated groups OR raw quality-filtered rows"]

            PL_XAxis{"Select X-axis nesting structure"}

            PL_XConfig["Configure grouping hierarchy
            ---
            Primary: single descriptive column
            Secondary: optional nested subgroup column
            Creates interaction term for plotting"]

            subgraph PL_Options["Processing Options"]
                direction LR
                PL_OptFilter["Filter data
                ---
    Row: numeric thresholds per measure
    Group: exclude entire groups by name"]
                PL_OptTrim["Trim data
                ---
    Trim percent: 0% · 5% · 10% · 15% · 20%
    Trimming method: symmetric · lower only · upper only"]
                PL_OptNormalize["Normalize data
                ---
    Per-group normalization by primary group
    Reference group: user-selected from levels"]
                PL_OptOutlier["Outlier detection
                ---
    Method: IQR · Z-Score · Adjusted Boxplot · KDE · Isolation Forest · LOF
    Action: flag · remove"]
            end

            PL_Plot["Interactive scatter plot with error bars
            ---
    📦 ggplot2 · ggiraph"]

            PL_Custom["Plot customization
            ---
    Factor reordering: drag-drop level ordering
    Shapes: custom per X-axis interaction term
    Colors: custom per X-axis interaction term
    Themes · error bar display · download options"]

            PL_Assumptions["Check statistical assumptions
            ---
    Shapiro-Wilk normality test
    Levene's test for homogeneity
    📦 stats · car"]

            PL_Input --> PL_XAxis
            PL_XAxis --> PL_XConfig
            PL_XConfig --> PL_Options
            PL_OptFilter --> PL_Plot
            PL_OptTrim --> PL_Plot
            PL_OptNormalize --> PL_Plot
            PL_OptOutlier --> PL_Plot
            PL_Plot --> PL_Custom
            PL_Plot --> PL_Assumptions
        end

        %% ==================== SUMMARY MODULE ====================
        subgraph SummaryModule["📋 Summary Module"]
            direction TB

            SUM_Input["Receive processed data from Plotting module
            ---
            Includes outlier · trimmed · normalized flag columns"]

            subgraph SUM_Config["Configuration"]
                direction LR
                SUM_OptGroup["Group by
                ---
                1+ descriptive columns (defaults to Plotting X-axis)"]
                SUM_OptShapiro["Normality test
                ---
                Shapiro-Wilk: off · on"]
                SUM_OptTransform["Show transformed summary
                ---
                Raw values · Normalized values
                (visible only when normalization is active in Plotting)"]
            end

            SUM_Compute["Compute grouped descriptive statistics
            ---
            n · mean · median · var · sd · sem · cv
            Respects outlier & trim flags per measurement column
            📦 stats"]

            SUM_Shapiro["Shapiro-Wilk normality test per group
            ---
            W · p-value · normal: yes / no
            📦 stats"]

            SUM_Tables["Interactive summary table per measurement
            ---
            One card per measurement column
            📦 DT"]

            SUM_DownloadSingle["Export single table as XLSX
            ---
            📦 openxlsx"]

            SUM_DownloadAll["Export all tables as multi-sheet XLSX
            ---
            📦 openxlsx"]

            SUM_Log["Log computation and download events
            ---
            📦 rhino"]

            SUM_ErrDisplay["Display structured error on failure"]

            SUM_Input --> SUM_Config
            SUM_Config --> SUM_Compute
            SUM_Compute --> SUM_Shapiro
            SUM_Compute --> SUM_Tables
            SUM_Shapiro --> SUM_Tables
            SUM_Tables --> SUM_DownloadSingle
            SUM_Tables --> SUM_DownloadAll
            SUM_Tables --> SUM_Log
            SUM_Compute -->|failure| SUM_ErrDisplay
        end

        %% ==================== STATISTICS MODULE ====================
        subgraph StatisticsModule["📊 Statistics Module"]
            direction TB

            STAT_Input["Receive plot data from Plotting module
            ---
            Includes outlier · trim · normalization flags"]

            subgraph STAT_Config["Configuration"]
                direction LR
                STAT_OptApproach["Test approach
                ---
                Parametric · Nonparametric · Robust"]
                STAT_OptOmnibus["Omnibus test family
                ---
                1-way · 2-way · 3-way design"]
                STAT_OptAdjust["P-value adjustment
                ---
                Holm · Hochberg · Hommel · Bonferroni · FDR"]
                STAT_OptBootstrap["Bootstrap options
                ---
                Enable · Samples · Sample size"]
            end

            STAT_Omnibus{"Omnibus test family"}
            STAT_OmniParam["Classical ANOVA
            ---
            F-test for 1/2/3-way designs
            📦 stats"]
            STAT_OmniNP["Kruskal-Wallis / ART
            ---
            Rank-based for 1/2/3-way designs
            📦 stats · ARTool"]
            STAT_OmniRob["Robust ANOVA
            ---
            Trimmed means t1way/t2way/t3way
            📦 WRS2"]

            STAT_PostHoc{"Post-hoc test family"}
            STAT_PostParam["Parametric pairwise
            ---
            Tukey HSD · emmeans
            📦 stats · emmeans"]
            STAT_PostNP["Nonparametric pairwise
            ---
            Dunn test · Pairwise Wilcoxon · ART contrasts
            📦 dunn.test · stats · ARTool"]
            STAT_PostRob["Robust pairwise
            ---
            Lincon trimmed means comparisons
            📦 WRS2"]

            STAT_Effect["Effect size estimation
            ---
            Cohen's d · Cliff's Delta
            📦 effsize"]

            STAT_Report["Generate HTML results report
            ---
            APA-style tables with embedded plot
            📦 htmltools · ggplot2 · base64enc"]

            STAT_Download["Download results as HTML file
            ---
            One report per measurement column"]

            STAT_Log["Log computation events
            ---
            📦 rhino"]

            STAT_Input --> STAT_Config
            STAT_Config --> STAT_Omnibus
            STAT_Omnibus -->|parametric| STAT_OmniParam
            STAT_Omnibus -->|nonparametric| STAT_OmniNP
            STAT_Omnibus -->|robust| STAT_OmniRob
            STAT_OmniParam --> STAT_PostHoc
            STAT_OmniNP --> STAT_PostHoc
            STAT_OmniRob --> STAT_PostHoc
            STAT_PostHoc -->|parametric| STAT_PostParam
            STAT_PostHoc -->|nonparametric| STAT_PostNP
            STAT_PostHoc -->|robust| STAT_PostRob
            STAT_PostParam --> STAT_Effect
            STAT_PostNP --> STAT_Effect
            STAT_PostRob --> STAT_Effect
            STAT_Effect --> STAT_Report --> STAT_Download
            STAT_Report --> STAT_Log
        end

        %% ==================== PCA MODULE ====================
        subgraph PCAModule["🔵 PCA Module"]
            direction TB

            PCA_Input["Receive analysis data
            ---
            Median-aggregated from Median Module OR raw from Load Data"]

            subgraph PCA_Config["Configuration"]
                direction LR
                PCA_CfgData["Data Selection
                ---
                Measurement columns (numeric)
                Metadata columns (descriptive)
                Scale: scale_center · center_only · none
                Skewness correction: on / off"]
                PCA_CfgPlot["Plotting Controls
                ---
                Biplot layer: individuals · variables · combined
                Group biplot: by metadata column(s)
                    Convex hull / 95% ellipse
                Point alpha: contribution · 0.25-1.0
                Point size: contribution · 1-10
                Dimensions: Dim.X · Dim.Y · Dim.Z
                Export size: width · height (cm)"]
            end

            PCA_Clean["Remove rows with missing values in measurement columns<br/>📦 stats"]
            PCA_SkewDetect["Detect highly skewed variables (|skewness| > 2)<br/>📦 moments"]
            PCA_SkewApply["Transform skewed variables via bestNormalize<br/>---<br/>📦 bestNormalize"]
            PCA_SkewBranch{"Skewness correction enabled?"}

            PCA_Scale["Scale and center selected measurement columns<br/>---<br/>Method: z-score standardization (scale_center) or center-only<br/>📦 stats"]

            PCA_Out_Corr["Correlation Matrix<br/>---<br/>Variable correlation heatmap displayed in UI<br/>📦 stats · ggcorrplot"]
            PCA_Out_KMO["KMO Sampling Adequacy<br/>---<br/>Overall measure with interpretation badge + per-variable adequacy table<br/>📦 psych"]
            PCA_Out_Opt["Optimal Components Estimation<br/>---<br/>Parallel analysis · Kaiser · Elbow · MAP<br/>Scree plot with thresholds + methods comparison table<br/>📦 psych · nFactors"]

            PCA_PCA["Execute PCA Computation<br/>---<br/>Eigenvalues · Variable coordinates/contrib/cos2 · Individual scores<br/>📦 FactoMineR · factoextra"]

            PCA_Out_Results["PCA Results Tables<br/>---<br/>Eigenvalues & variance · Variable results (coord/contrib/cos2) · Individual results"]
            PCA_Out_Biplot2D["2D Biplot<br/>---<br/>Interactive ggiraph plot with individuals/variables/both layers<br/>Group coloring · Convex hull or 95% ellipse"]
            PCA_Out_Biplot3D["3D Biplot<br/>---<br/>Interactive plotly visualization (shown when ≥3 components)"]
            PCA_Out_VarContrib["Variable Contributions<br/>---<br/>Jitter/strip plot per dimension with cos2 filtering"]
            PCA_Out_IndContrib["Individual Contributions<br/>---<br/>Jitter/strip plot per dimension"]
            PCA_Out_EigenCor["Dimension–Metadata Correlation<br/>---<br/>Heatmap of correlations between PC dimensions and metadata columns<br/>📦 ggplot2 · ggiraph"]

            PCA_Export["Export Results<br/>---<br/>Excel: eigenvalues · variables · individuals · correlations<br/>RDS: full PCA object with raw data, transform params, settings<br/>📦 openxlsx"]
            PCA_DownExcel["Download Excel file"]
            PCA_DownRDS["Download RDS bundle"]

            PCA_PassLDA["Pass PCA result downstream to LDA Module<br/>|pca_result|"]

            PCA_Input --> PCA_Clean
            PCA_Clean --> PCA_SkewDetect --> PCA_SkewBranch
            PCA_SkewBranch -->|yes| PCA_SkewApply --> PCA_Scale
            PCA_SkewBranch -->|no| PCA_Scale
            PCA_Config --> PCA_Scale
            PCA_Scale --> PCA_Out_Corr
            PCA_Scale --> PCA_Out_KMO
            PCA_Scale --> PCA_Out_Opt
            PCA_Scale --> PCA_PCA
            PCA_PCA --> PCA_Out_Results
            PCA_PCA --> PCA_Out_Biplot2D
            PCA_PCA --> PCA_Out_Biplot3D
            PCA_PCA --> PCA_Out_VarContrib
            PCA_PCA --> PCA_Out_IndContrib
            PCA_PCA --> PCA_Out_EigenCor
            PCA_PCA --> PCA_Export --> PCA_DownExcel
            PCA_Export --> PCA_DownRDS
            PCA_PCA --> PCA_PassLDA
        end

        %% ==================== LDA MODULE ====================
        subgraph LDAModule["📊 LDA Module"]
            direction TB

            LDA_Input["Receive analysis data
            ---
            Raw from Median Module OR PCA scores from PCA Module"]

            subgraph LDA_Config["Configuration"]
                direction LR
                LDA_CfgData["Data Selection
                ---
                Measurement columns (numeric)
                Metadata columns (descriptive)
                Grouping column (categorical)
                Data source: raw · PCA scores"]
                LDA_CfgModel["Analysis Settings
                ---
                Method: LDA · QDA · MDA
                Prior: proportional · equal
                Skewness correction: on / off
                Scale: z-score · none
                Evaluation: full model · LOO-CV · train/test split
                MDA subclasses · train fraction"]
                LDA_CfgPlot["Plotting Controls
                ---
                Axes: LD1 · LD2 · …
                Overlay: diagnostics · decision boundaries
                Export size: width · height (cm)"]
            end

            LDA_Clean["Remove rows with missing values in measurement columns"]
            LDA_SkewDetect["Detect highly skewed variables (|skewness| > 2)
            ---
            📦 moments"]
            LDA_SkewBranch{"Skewness correction enabled?"}
            LDA_SkewApply["Transform skewed variables via bestNormalize
            ---
            📦 bestNormalize"]
            LDA_Scale["Scale and center selected measurement columns
            ---
            Method: z-score standardization or none
            📦 stats"]
            LDA_Split["Stratified train / test split
            ---
            Proportional per-group sampling · configurable fraction"]
            LDA_Validate["Validate inputs for selected method
            ---
            Column presence · group count · per-group sample size"]
            LDA_Branch{"Method"}
            LDA_LDA["Fit Linear Discriminant Analysis
            ---
            LD axes · scaling coefficients · proportion of trace
            📦 MASS"]
            LDA_QDA["Fit Quadratic Discriminant Analysis
            ---
            Separate per-group covariance · companion LDA for LD projection
            📦 MASS"]
            LDA_MDA["Fit Mixture Discriminant Analysis
            ---
            EM subclass fitting · discriminant coordinates
            📦 mda"]
            LDA_Predict["Predict classes on held-out / test data
            ---
            Posterior probabilities · confusion matrix"]
            LDA_Diag["Assumption diagnostics
            ---
            Covariance ellipses · pooled ellipses · decision boundaries
            📦 heplots · colorspace"]

            LDA_Out_ScorePlot["LD Scores scatter plot
            ---
            Interactive 2D / 1D strip plot · group colouring
            Optional: diagnostics overlay · boundary regions
            📦 ggplot2 · ggiraph"]
            LDA_Out_VarContrib["Variable contributions per LD axis
            ---
            Jitter plot reusing PCA contrib structure"]
            LDA_Out_DimEval["Discriminant dimension evaluation
            ---
            One-way ANOVA per LD axis · F · p-value · R²"]
            LDA_Out_Results["Classification results table
            ---
            Predicted class · posterior probabilities · confusion matrix
            Per-class precision · recall · F1"]

            LDA_Export["Export results
            ---
            Excel: LD scores · group means · coefficients · confusion matrix
            RDS bundle: model · transform params · scale params · settings
            📦 openxlsx"]
            LDA_DownExcel["Download Excel file"]
            LDA_DownRDS["Download RDS bundle"]

            LDA_PassCluster["|lda_result|"]
            LDA_PassPrediction["|lda_bundle|"]

            LDA_Input --> LDA_Clean
            LDA_Clean --> LDA_SkewDetect --> LDA_SkewBranch
            LDA_SkewBranch -->|yes| LDA_SkewApply --> LDA_Scale
            LDA_SkewBranch -->|no| LDA_Scale
            LDA_Config --> LDA_Scale
            LDA_Scale --> LDA_Split --> LDA_Validate --> LDA_Branch
            LDA_Branch -->|LDA| LDA_LDA
            LDA_Branch -->|QDA| LDA_QDA
            LDA_Branch -->|MDA| LDA_MDA
            LDA_LDA --> LDA_Predict
            LDA_QDA --> LDA_Predict
            LDA_MDA --> LDA_Predict
            LDA_Predict --> LDA_Diag
            LDA_Predict --> LDA_Out_Results
            LDA_Diag --> LDA_Out_ScorePlot
            LDA_Diag --> LDA_Out_VarContrib
            LDA_Diag --> LDA_Out_DimEval
            LDA_Diag --> LDA_Export
            LDA_Export --> LDA_DownExcel
            LDA_Export --> LDA_DownRDS
            LDA_Export --> LDA_PassPrediction
            LDA_Predict --> LDA_PassCluster
        end

        %% ==================== CLUSTER MODULE ====================
        subgraph ClusterModule["🎯 Cluster Module"]
            direction TB

            CL_Source{"Data source?"}
            CL_Raw["Preprocess raw data
            ---
            na_handling · skewness_transform · scale_data<br/>📦 stats · moments · bestNormalize"]
            CL_PCA["Use PCA scores<br/>---<br/>Skip preprocessing · Dim.1, Dim.2, ..."]
            CL_LDA["Use LDA scores<br/>---<br/>Skip preprocessing · LD1, LD2, ..."]
            CL_Hopkins["Assess cluster tendency
            ---
            Hopkins statistic · 📦 hopkins"]
            CL_Optimal["Determine optimal k
            ---
            Elbow · Silhouette · Gap statistic<br/>📦 cluster · NbClust"]
            CL_Algo{"Algorithm?"}
            CL_KMeans["K-Means / PAM<br/>---<br/>📦 stats · cluster"]
            CL_HClust["Hierarchical clustering<br/>---<br/>📦 stats"]
            CL_DBSCAN["DBSCAN<br/>---<br/>Auto eps estimation · 📦 dbscan"]
            CL_Valid["Validate clustering<br/>---<br/>Silhouette · BSS/TSS<br/>📦 cluster"]
            CL_Heatmap["Cluster heatmap<br/>---<br/>📦 heatmaply · plotly"]
            CL_Biplot["Cluster biplot<br/>---<br/>Convex hulls · 📦 ggplot2 · ggiraph"]

            CL_Source -->|raw| CL_Raw
            CL_Source -->|pca_scores| CL_PCA
            CL_Source -->|lda_scores| CL_LDA
            CL_Raw --> CL_Hopkins
            CL_PCA --> CL_Hopkins
            CL_LDA --> CL_Hopkins
            CL_Hopkins --> CL_Optimal --> CL_Algo
            CL_Algo -->|kmeans| CL_KMeans --> CL_Valid
            CL_Algo -->|pam| CL_KMeans
            CL_Algo -->|hclust| CL_HClust --> CL_Valid
            CL_Algo -->|dbscan| CL_DBSCAN --> CL_Valid
            CL_Valid --> CL_Heatmap
            CL_Valid --> CL_Biplot
        end

        %% ==================== PREDICTION MODULE ====================
        subgraph PredictionModule["🔮 Prediction Module"]
            direction TB

            PR_BundleInput["Receive LDA/QDA/MDA/PCA bundle
            ---
            Upload saved RDS bundle file"]
            PR_LoadBundle["Load and validate bundle structure
            ---
            Required fields · analysis type · model presence
            📦 base"]
            PR_DataInput["Receive unknown observations
            ---
            Upload new data file to classify"]
            PR_ValidData["Validate unknown data against bundle schema
            ---
            Required measurement columns · numeric types
            Range plausibility vs training data · metadata warnings"]

            subgraph PR_Config["Plot &amp; prediction options"]
                direction LR
                PR_OptAxes["Plot axes
                ---
                Dim / LD axis: x · y"]
                PR_OptMeta["Label column
                ---
                Metadata column for point labels"]
                PR_OptOverlay["Overlay options
                ---
                Decision boundaries · diagnostics (LDA/MDA)"]
            end

            PR_Preprocess["Apply stored preprocessing to unknowns
            ---
            Skewness transforms · center/scale (LDA/MDA/QDA)
            PCA scaling handled by predict.prcomp"]
            PR_Branch{"Analysis type?"}
            PR_PCA["Project unknowns into PCA space
            ---
            PC scores via predict.prcomp · rename to Dim.1…n"]
            PR_LDA["Classify via LDA
            ---
            Predicted class · posterior probabilities · LD scores
            📦 MASS"]
            PR_QDA["Classify via QDA
            ---
            Predicted class · posterior probabilities
            Companion LDA projection for LD scores
            📦 MASS"]
            PR_MDA["Classify via MDA
            ---
            Predicted class · posterior probabilities · variate scores
            📦 mda"]

            PR_OverlayPlot["Prediction overlay plot
            ---
            Training data base plot + unknown triangles
            Axis labels · tooltips · group colouring
            📦 ggplot2 · ggiraph"]
            PR_PostTable["Posterior probabilities table
            ---
            Per-observation predicted class + class probabilities
            📦 DT"]
            PR_RangeWarn["Range warnings display
            ---
            Columns where unknowns exceed training range"]
            PR_LogEvent["Log bundle load and prediction events"]
            PR_ErrorDisplay["Display validation or prediction errors"]

            PR_BundleInput --> PR_LoadBundle
            PR_DataInput --> PR_ValidData
            PR_LoadBundle --> PR_ValidData
            PR_ValidData --> PR_Preprocess
            PR_Config --> PR_Preprocess
            PR_Preprocess --> PR_Branch
            PR_Branch -->|PCA| PR_PCA
            PR_Branch -->|LDA| PR_LDA
            PR_Branch -->|QDA| PR_QDA
            PR_Branch -->|MDA| PR_MDA
            PR_PCA --> PR_OverlayPlot
            PR_LDA --> PR_OverlayPlot
            PR_QDA --> PR_OverlayPlot
            PR_MDA --> PR_OverlayPlot
            PR_LDA --> PR_PostTable
            PR_QDA --> PR_PostTable
            PR_MDA --> PR_PostTable
            PR_ValidData --> PR_RangeWarn
            PR_LoadBundle --> PR_LogEvent
            PR_Branch --> PR_LogEvent
            PR_ValidData --> PR_ErrorDisplay
            PR_LoadBundle --> PR_ErrorDisplay
        end

        %% ==================== POWER ANALYSIS MODULE ====================
        subgraph PowerModule["⚡ Power Analysis Module"]
            direction TB

            PWR_Input["Receive design parameters
            ---
            Effect size · alpha · target power · groups"]
            PWR_Valid["Validate inputs
            ---
            Range checks · distribution compatibility"]

            subgraph PWR_Config["Configuration options"]
                direction LR
                PWR_SolveMode["Solve for:
                ---
                sample_size · power · mde"]
                PWR_EffectInput["Effect input:
                ---
                standardized · raw"]
                PWR_InputMode{"Input mode?"}
                PWR_MeanSD["Mean + SD"]
                PWR_MedIQR["Median + IQR"]
                PWR_Dist["Distribution:
                ---
                normal · lognormal · exponential"]
                PWR_Approach["Approach:
                ---
                parametric · robust · nonparametric"]
            end

            PWR_SimData["Simulate illustrative data
            ---
            Distribution-aware sampling<br/>📦 stats"]
            PWR_Calc{"Calculation path?"}
            PWR_Parametric["Parametric power analysis
            ---
            📦 pwr · stats"]
            PWR_Simulation["Monte Carlo simulation
            ---
            Binary search for N or effect<br/>📦 stats"]

            PWR_Out_Curve["Power curve data"]
            PWR_Out_Design["Design table
            ---
            N per cell · total N"]
            PWR_Out_Result["Power analysis result
            ---
                    Description · value · type"]
            PWR_Out_Plot["Power curve plot
            ---
            📦 ggplot2"]
            PWR_Out_Viz["Illustrative data plot"]

            PWR_Error["Display validation errors"]

            PWR_Input --> PWR_Valid
            PWR_Valid -->|invalid| PWR_Error
            PWR_Valid -->|valid| PWR_Config
            PWR_Config --> PWR_SimData
            PWR_InputMode -->|mean_sd| PWR_MeanSD
            PWR_InputMode -->|median_iqr| PWR_MedIQR
            PWR_Config --> PWR_Calc
            PWR_Calc -->|normal + parametric| PWR_Parametric
            PWR_Calc -->|simulation| PWR_Simulation
            PWR_Parametric --> PWR_Out_Curve
            PWR_Parametric --> PWR_Out_Design
            PWR_Parametric --> PWR_Out_Result
            PWR_Simulation --> PWR_Out_Curve
            PWR_Simulation --> PWR_Out_Design
            PWR_Simulation --> PWR_Out_Result
            PWR_Out_Curve --> PWR_Out_Plot
            PWR_SimData --> PWR_Out_Viz
        end

        %% ==================== SHARED INFRASTRUCTURE ====================
        subgraph SharedInfra["🔧 Shared Infrastructure"]
            direction TB
            SH_Error["Safe execution wrapper
---
📦 rlang"]
            SH_Columns["Column classification
---
Identify measurement vs descriptive columns"]
            SH_DataUtils["Data transformation helpers
---
filter · interactions · color palettes"]
            SH_Logging["Session event logging"]
            SH_Settings["App configuration
---
theme · version · 📦 config"]
            SH_Preproc["Data preprocessing
---
missing values · normalization · skewness
📦 moments · bestNormalize · stats"]
        end
    end

    %% ==================== EXTERNAL PACKAGES SUMMARY ====================
    subgraph ExternalPackages["📚 Key External R Packages"]
        direction LR
        PKG_UI["UI / Reactivity<br/>shiny · bslib · bsicons"]
        PKG_Tables["Tables<br/>DT · summarytools · DataExplorer"]
        PKG_Viz["Visualisation<br/>ggplot2 · ggiraph · plotly · pheatmap · ggcorrplot"]
        PKG_IO["Data I/O<br/>openxlsx · utils (read.csv)"]
        PKG_Multivar["Multivariate Analysis<br/>FactoMineR · factoextra · MASS · mda"]
        PKG_Cluster["Clustering<br/>cluster · dbscan · NbClust · clustertend"]
        PKG_Stats["Inference<br/>stats · WRS2 · emmeans · multcomp · dunn.test · effsize · ARTool · car"]
        PKG_Power["Power Analysis<br/>pwr · WebPower"]
        PKG_Preproc["Preprocessing<br/>moments · bestNormalize · psych · caret"]
    end

    %% ==================== DATA FLOW ====================
    LoadDataModule -.->|data + version| MedianModule
    LoadDataModule -.->|raw data| PowerModule

    MedianModule -.->|analysis_data median OR raw| PlottingModule
    MedianModule -.->|analysis_data| PCAModule
    MedianModule -.->|analysis_data| LDAModule
    MedianModule -.->|analysis_data| ClusterModule

    PlottingModule -.->|plot_data| SummaryModule
    PlottingModule -.->|plot_data| StatisticsModule

    PCAModule -.->|pca_result| LDAModule
    PCAModule -.->|pca_result| ClusterModule
    LDAModule -.->|lda_result| ClusterModule
    LDAModule -.->|lda_bundle RDS| PredictionModule

    MedianModule -.-> SharedInfra
    PlottingModule -.-> SharedInfra
    StatisticsModule -.-> SharedInfra
    PCAModule -.-> SharedInfra
    LDAModule -.-> SharedInfra
    ClusterModule -.-> SharedInfra
    PredictionModule -.-> SharedInfra
    PowerModule -.-> SharedInfra

    %% Styling
    %% Module base colors (from your palette)
    style LoadDataModule fill:#8dd3c7,stroke:#5a968d,stroke-width:4px
    style MedianModule fill:#ffffb3,stroke:#cccc8f,stroke-width:4px
    style PlottingModule fill:#bebada,stroke:#8f8ba3,stroke-width:4px
    style SummaryModule fill:#fb8072,stroke:#c9665b,stroke-width:4px
    style StatisticsModule fill:#80b1d3,stroke:#668da9,stroke-width:4px
    style PCAModule fill:#fdb462,stroke:#ca8f4e,stroke-width:4px
    style LDAModule fill:#b3de69,stroke:#8fb154,stroke-width:4px
    style ClusterModule fill:#fccde5,stroke:#c9a4b7,stroke-width:4px
    style PredictionModule fill:#d9d9d9,stroke:#aeaeae,stroke-width:4px
    style PowerModule fill:#bc80bd,stroke:#966697,stroke-width:4px
    style SharedInfra fill:#f0f0f0,stroke:#999999,stroke-width:4px
    style ExternalPackages fill:#f5f5f5,stroke:#cccccc,stroke-width:2px

    %% Internal nodes - lighter shades of parent module colors
    style LD_Source fill:#c5ebe3,stroke:#5a968d
    style MED_GroupBranch fill:#ffffe6,stroke:#cccc8f
    style PL_XAxis fill:#ddd9eb,stroke:#8f8ba3
    style CL_Source fill:#fde4f0,stroke:#c9a4b7
    style CL_Algo fill:#fde4f0,stroke:#c9a4b7
    style LDA_Branch fill:#d4eb9d,stroke:#8fb154
    style LDA_SkewBranch fill:#d4eb9d,stroke:#8fb154
    style STAT_Omnibus fill:#b0d0e6,stroke:#668da9
    style STAT_PostHoc fill:#b0d0e6,stroke:#668da9
    style PCA_SkewBranch fill:#fee09f,stroke:#ca8f4e
    style PR_Branch fill:#d4eb9d,stroke:#8fb154
    style PWR_InputMode fill:#e6c8e5,stroke:#966697
    style PWR_Calc fill:#e6c8e5,stroke:#966697

    %% Internal processing nodes - LoadDataModule (teal family)
    style LD_Upload fill:#c5ebe3,stroke:#5a968d
    style LD_Example fill:#c5ebe3,stroke:#5a968d
    style LD_CSVOpts fill:#c5ebe3,stroke:#5a968d
    style LD_Read fill:#c5ebe3,stroke:#5a968d
    style LD_Validate fill:#c5ebe3,stroke:#5a968d
    style LD_ColModal fill:#c5ebe3,stroke:#5a968d
    style LD_ErrDisplay fill:#c5ebe3,stroke:#5a968d
    style LD_Log fill:#c5ebe3,stroke:#5a968d
    style LD_Preview fill:#c5ebe3,stroke:#5a968d
    style LD_Missing fill:#c5ebe3,stroke:#5a968d
    style LD_Summary fill:#c5ebe3,stroke:#5a968d

    %% Internal processing nodes - MedianModule (yellow family)
    style MED_Config fill:#ffffe6,stroke:#cccc8f
    style MED_OptGrouping fill:#ffffe6,stroke:#cccc8f
    style MED_OptQuality fill:#ffffe6,stroke:#cccc8f
    style MED_QualityFilter fill:#ffffe6,stroke:#cccc8f
    style MED_NoGroup fill:#ffffe6,stroke:#cccc8f
    style MED_Medians fill:#ffffe6,stroke:#cccc8f
    style MED_TableNoGroup fill:#ffffe6,stroke:#cccc8f
    style MED_TableGrouped fill:#ffffe6,stroke:#cccc8f
    style MED_ExcelFilter fill:#ffffe6,stroke:#cccc8f
    style MED_Download fill:#ffffe6,stroke:#cccc8f
    style MED_Log fill:#ffffe6,stroke:#cccc8f

    %% Internal processing nodes - PlottingModule (lavender family)
    style PL_Input fill:#ddd9eb,stroke:#8f8ba3
    style PL_XConfig fill:#ddd9eb,stroke:#8f8ba3
    style PL_Options fill:#ddd9eb,stroke:#8f8ba3
    style PL_OptFilter fill:#ddd9eb,stroke:#8f8ba3
    style PL_OptTrim fill:#ddd9eb,stroke:#8f8ba3
    style PL_OptNormalize fill:#ddd9eb,stroke:#8f8ba3
    style PL_OptOutlier fill:#ddd9eb,stroke:#8f8ba3
    style PL_Plot fill:#ddd9eb,stroke:#8f8ba3
    style PL_Custom fill:#ddd9eb,stroke:#8f8ba3
    style PL_Assumptions fill:#ddd9eb,stroke:#8f8ba3

    %% Internal processing nodes - SummaryModule (coral family)
    style SUM_Input fill:#fdb4ac,stroke:#c9665b
    style SUM_Config fill:#fdb4ac,stroke:#c9665b
    style SUM_OptGroup fill:#fdb4ac,stroke:#c9665b
    style SUM_OptShapiro fill:#fdb4ac,stroke:#c9665b
    style SUM_OptTransform fill:#fdb4ac,stroke:#c9665b
    style SUM_Compute fill:#fdb4ac,stroke:#c9665b
    style SUM_Shapiro fill:#fdb4ac,stroke:#c9665b
    style SUM_Tables fill:#fdb4ac,stroke:#c9665b
    style SUM_DownloadSingle fill:#fdb4ac,stroke:#c9665b
    style SUM_DownloadAll fill:#fdb4ac,stroke:#c9665b
    style SUM_Log fill:#fdb4ac,stroke:#c9665b
    style SUM_ErrDisplay fill:#fdb4ac,stroke:#c9665b

    %% Internal processing nodes - StatisticsModule (blue family)
    style STAT_Input fill:#b0d0e6,stroke:#668da9
    style STAT_Config fill:#b0d0e6,stroke:#668da9
    style STAT_OptApproach fill:#b0d0e6,stroke:#668da9
    style STAT_OptOmnibus fill:#b0d0e6,stroke:#668da9
    style STAT_OptAdjust fill:#b0d0e6,stroke:#668da9
    style STAT_OptBootstrap fill:#b0d0e6,stroke:#668da9
    style STAT_OmniParam fill:#b0d0e6,stroke:#668da9
    style STAT_OmniNP fill:#b0d0e6,stroke:#668da9
    style STAT_OmniRob fill:#b0d0e6,stroke:#668da9
    style STAT_PostParam fill:#b0d0e6,stroke:#668da9
    style STAT_PostNP fill:#b0d0e6,stroke:#668da9
    style STAT_PostRob fill:#b0d0e6,stroke:#668da9
    style STAT_Effect fill:#b0d0e6,stroke:#668da9
    style STAT_Report fill:#b0d0e6,stroke:#668da9
    style STAT_Download fill:#b0d0e6,stroke:#668da9
    style STAT_Log fill:#b0d0e6,stroke:#668da9

    %% Internal processing nodes - PCAModule (orange family)
    style PCA_Input fill:#fee09f,stroke:#ca8f4e
    style PCA_Config fill:#fee09f,stroke:#ca8f4e
    style PCA_CfgData fill:#fee09f,stroke:#ca8f4e
    style PCA_CfgPlot fill:#fee09f,stroke:#ca8f4e
    style PCA_Clean fill:#fee09f,stroke:#ca8f4e
    style PCA_SkewDetect fill:#fee09f,stroke:#ca8f4e
    style PCA_SkewApply fill:#fee09f,stroke:#ca8f4e
    style PCA_Scale fill:#fee09f,stroke:#ca8f4e
    style PCA_Out_Corr fill:#fee09f,stroke:#ca8f4e
    style PCA_Out_KMO fill:#fee09f,stroke:#ca8f4e
    style PCA_Out_Opt fill:#fee09f,stroke:#ca8f4e
    style PCA_PCA fill:#fee09f,stroke:#ca8f4e
    style PCA_Out_Results fill:#fee09f,stroke:#ca8f4e
    style PCA_Out_Biplot2D fill:#fee09f,stroke:#ca8f4e
    style PCA_Out_Biplot3D fill:#fee09f,stroke:#ca8f4e
    style PCA_Out_VarContrib fill:#fee09f,stroke:#ca8f4e
    style PCA_Out_IndContrib fill:#fee09f,stroke:#ca8f4e
    style PCA_Out_EigenCor fill:#fee09f,stroke:#ca8f4e
    style PCA_Export fill:#fee09f,stroke:#ca8f4e
    style PCA_DownExcel fill:#fee09f,stroke:#ca8f4e
    style PCA_DownRDS fill:#fee09f,stroke:#ca8f4e
    style PCA_PassLDA fill:#fee09f,stroke:#ca8f4e

    %% Internal processing nodes - LDAModule (green family)
    style LDA_Input fill:#d4eb9d,stroke:#8fb154
    style LDA_Config fill:#d4eb9d,stroke:#8fb154
    style LDA_CfgData fill:#d4eb9d,stroke:#8fb154
    style LDA_CfgModel fill:#d4eb9d,stroke:#8fb154
    style LDA_CfgPlot fill:#d4eb9d,stroke:#8fb154
    style LDA_Clean fill:#d4eb9d,stroke:#8fb154
    style LDA_SkewDetect fill:#d4eb9d,stroke:#8fb154
    style LDA_SkewApply fill:#d4eb9d,stroke:#8fb154
    style LDA_Scale fill:#d4eb9d,stroke:#8fb154
    style LDA_Split fill:#d4eb9d,stroke:#8fb154
    style LDA_Validate fill:#d4eb9d,stroke:#8fb154
    style LDA_LDA fill:#d4eb9d,stroke:#8fb154
    style LDA_QDA fill:#d4eb9d,stroke:#8fb154
    style LDA_MDA fill:#d4eb9d,stroke:#8fb154
    style LDA_Predict fill:#d4eb9d,stroke:#8fb154
    style LDA_Diag fill:#d4eb9d,stroke:#8fb154
    style LDA_Out_ScorePlot fill:#d4eb9d,stroke:#8fb154
    style LDA_Out_VarContrib fill:#d4eb9d,stroke:#8fb154
    style LDA_Out_DimEval fill:#d4eb9d,stroke:#8fb154
    style LDA_Out_Results fill:#d4eb9d,stroke:#8fb154
    style LDA_Export fill:#d4eb9d,stroke:#8fb154
    style LDA_DownExcel fill:#d4eb9d,stroke:#8fb154
    style LDA_DownRDS fill:#d4eb9d,stroke:#8fb154
    style LDA_PassCluster fill:#d4eb9d,stroke:#8fb154
    style LDA_PassPrediction fill:#d4eb9d,stroke:#8fb154

    %% Internal processing nodes - ClusterModule (pink family)
    style CL_Raw fill:#fde4f0,stroke:#c9a4b7
    style CL_PCA fill:#fde4f0,stroke:#c9a4b7
    style CL_LDA fill:#fde4f0,stroke:#c9a4b7
    style CL_Hopkins fill:#fde4f0,stroke:#c9a4b7
    style CL_Optimal fill:#fde4f0,stroke:#c9a4b7
    style CL_KMeans fill:#fde4f0,stroke:#c9a4b7
    style CL_HClust fill:#fde4f0,stroke:#c9a4b7
    style CL_DBSCAN fill:#fde4f0,stroke:#c9a4b7
    style CL_Valid fill:#fde4f0,stroke:#c9a4b7
    style CL_Heatmap fill:#fde4f0,stroke:#c9a4b7
    style CL_Biplot fill:#fde4f0,stroke:#c9a4b7

    %% Internal processing nodes - PredictionModule (gray family)
    style PR_BundleInput fill:#f0f0f0,stroke:#aeaeae
    style PR_LoadBundle fill:#f0f0f0,stroke:#aeaeae
    style PR_DataInput fill:#f0f0f0,stroke:#aeaeae
    style PR_ValidData fill:#f0f0f0,stroke:#aeaeae
    style PR_Config fill:#f0f0f0,stroke:#aeaeae
    style PR_OptAxes fill:#f0f0f0,stroke:#aeaeae
    style PR_OptMeta fill:#f0f0f0,stroke:#aeaeae
    style PR_OptOverlay fill:#f0f0f0,stroke:#aeaeae
    style PR_Preprocess fill:#f0f0f0,stroke:#aeaeae
    style PR_PCA fill:#f0f0f0,stroke:#aeaeae
    style PR_LDA fill:#f0f0f0,stroke:#aeaeae
    style PR_QDA fill:#f0f0f0,stroke:#aeaeae
    style PR_MDA fill:#f0f0f0,stroke:#aeaeae
    style PR_OverlayPlot fill:#f0f0f0,stroke:#aeaeae
    style PR_PostTable fill:#f0f0f0,stroke:#aeaeae
    style PR_RangeWarn fill:#f0f0f0,stroke:#aeaeae
    style PR_LogEvent fill:#f0f0f0,stroke:#aeaeae
    style PR_ErrorDisplay fill:#f0f0f0,stroke:#aeaeae

    %% Internal processing nodes - PowerModule (purple family)
    style PWR_Input fill:#e6c8e5,stroke:#966697
    style PWR_Valid fill:#e6c8e5,stroke:#966697
    style PWR_Config fill:#e6c8e5,stroke:#966697
    style PWR_SolveMode fill:#e6c8e5,stroke:#966697
    style PWR_EffectInput fill:#e6c8e5,stroke:#966697
    style PWR_InputMode fill:#e6c8e5,stroke:#966697
    style PWR_MeanSD fill:#e6c8e5,stroke:#966697
    style PWR_MedIQR fill:#e6c8e5,stroke:#966697
    style PWR_Dist fill:#e6c8e5,stroke:#966697
    style PWR_Approach fill:#e6c8e5,stroke:#966697
    style PWR_SimData fill:#e6c8e5,stroke:#966697
    style PWR_Calc fill:#e6c8e5,stroke:#966697
    style PWR_Parametric fill:#e6c8e5,stroke:#966697
    style PWR_Simulation fill:#e6c8e5,stroke:#966697
    style PWR_Out_Curve fill:#e6c8e5,stroke:#966697
    style PWR_Out_Design fill:#e6c8e5,stroke:#966697
    style PWR_Out_Result fill:#e6c8e5,stroke:#966697
    style PWR_Out_Plot fill:#e6c8e5,stroke:#966697
    style PWR_Out_Viz fill:#e6c8e5,stroke:#966697
    style PWR_Error fill:#e6c8e5,stroke:#966697

    %% Edges colored by origin module - 12px thick
    %% LoadDataModule (teal, indices 0-12)
    linkStyle 0,1,2,3,4,5,6,7,8,9,10,11,12 stroke:#5a968d,stroke-width:12px
    %% MedianModule (yellow, indices 13-22)
    linkStyle 13,14,15,16,17,18,19,20,21,22 stroke:#cccc8f,stroke-width:12px
    %% PlottingModule (lavender, indices 23-31)
    linkStyle 23,24,25,26,27,28,29,30,31 stroke:#8f8ba3,stroke-width:12px
    %% SummaryModule (coral, indices 32-40)
    linkStyle 32,33,34,35,36,37,38,39,40 stroke:#c9665b,stroke-width:12px
    %% StatisticsModule (blue, indices 41-57)
    linkStyle 41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57 stroke:#668da9,stroke-width:12px
    %% PCAModule (orange, indices 58-78)
    linkStyle 58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78 stroke:#ca8f4e,stroke-width:12px
    %% LDAModule (green, indices 79-104)
    linkStyle 79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104 stroke:#8fb154,stroke-width:12px
    %% ClusterModule (pink, indices 105-121)
    linkStyle 105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121 stroke:#c9a4b7,stroke-width:12px
    %% PredictionModule (gray, indices 122-143)
    linkStyle 122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143 stroke:#aeaeae,stroke-width:12px
    %% PowerModule (purple, indices 144-160)
    linkStyle 144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160 stroke:#966697,stroke-width:12px
    %% Inter-module data flow connectors - colored by origin subgraph
    %% LoadDataModule origin (teal): → MedianModule, → PowerModule
    linkStyle 161,162 stroke:#8dd3c7,stroke-width:18px
    %% MedianModule origin (yellow): → PlottingModule, → PCAModule, → LDAModule, → ClusterModule, → SharedInfra
    linkStyle 163,164,165,166,173 stroke:#ffffb3,stroke-width:18px
    %% PlottingModule origin (lavender): → SummaryModule, → StatisticsModule, → SharedInfra
    linkStyle 167,168,174 stroke:#bebada,stroke-width:18px
    %% PCAModule origin (orange): → LDAModule, → ClusterModule, → SharedInfra
    linkStyle 169,170,176 stroke:#fdb462,stroke-width:18px
    %% LDAModule origin (green): → ClusterModule, → PredictionModule, → SharedInfra
    linkStyle 171,172,177 stroke:#b3de69,stroke-width:18px
    %% StatisticsModule origin (blue): → SharedInfra
    linkStyle 175 stroke:#80b1d3,stroke-width:18px
    %% ClusterModule origin (pink): → SharedInfra
    linkStyle 178 stroke:#fccde5,stroke-width:18px
    %% PredictionModule origin (gray): → SharedInfra
    linkStyle 179 stroke:#d9d9d9,stroke-width:18px
    %% PowerModule origin (purple): → SharedInfra
    linkStyle 180 stroke:#bc80bd,stroke-width:18px
    