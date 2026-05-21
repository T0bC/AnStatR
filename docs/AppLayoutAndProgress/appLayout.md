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

            LDA_UI["UI: Data Selection · Analysis Settings · Plotting Controls<br/>📦 bslib · ggiraph"]
            LDA_Clean["na_handling::clean_na_rows() · 📦 stats"]
            LDA_Skew["skewness_transform.R<br/>📦 moments · bestNormalize"]
            LDA_Scale["pca/scaling::scale_data() · 📦 stats (scale)"]
            LDA_Split["data_splitting::create_stratified_split()<br/>Train / Test split · 📦 caret"]
            LDA_Branch{"Method"}
            LDA_LDA["run_lda() - Linear DA<br/>📦 MASS (lda)"]
            LDA_QDA["run_qda() - Quadratic DA<br/>📦 MASS (qda)"]
            LDA_MDA["run_mda() - Mixture DA<br/>📦 mda"]
            LDA_Predict["run_predict() - posterior probs<br/>📦 MASS (predict.lda/qda)"]
            LDA_Diag["lda_diagnostics.R<br/>Box-M · Mahalanobis · CV accuracy<br/>📦 heplots · stats"]
            LDA_Plot["ld_plot.R · lda_var_contrib.R<br/>📦 ggplot2 · ggiraph"]
            LDA_Export["lda_export.R<br/>create_lda_excel() · create_lda_bundle()<br/>📦 openxlsx"]

            LDA_Packages["📦 MASS (lda, qda, predict) · mda (MDA)<br/>📦 caret · heplots (Box-M) · openxlsx"]

            LDA_UI --> LDA_Clean --> LDA_Skew --> LDA_Scale --> LDA_Split
            LDA_Split --> LDA_Branch
            LDA_Branch -->|LDA| LDA_LDA
            LDA_Branch -->|QDA| LDA_QDA
            LDA_Branch -->|MDA| LDA_MDA
            LDA_LDA --> LDA_Predict
            LDA_QDA --> LDA_Predict
            LDA_MDA --> LDA_Predict
            LDA_Predict --> LDA_Diag
            LDA_Diag --> LDA_Plot
            LDA_Diag --> LDA_Export
            LDA_Export --> LDA_Packages
        end

        %% ==================== CLUSTER MODULE ====================
        subgraph ClusterModule["🎯 Cluster Module"]
            direction TB

            CL_UI["UI: Data Selection · Clustering Settings · Display Options<br/>📦 bslib · ggiraph · plotly"]
            CL_Source{"Data Source?"}
            CL_Raw["Raw: na_handling + skewness_transform + scale_data<br/>📦 stats · moments · bestNormalize"]
            CL_PCA["PCA Scores (skip preprocessing)<br/>Dim.1, Dim.2, ... from pca_result"]
            CL_LDA["LDA Scores (skip preprocessing)<br/>LD1, LD2, ... from lda_result"]
            CL_Hopkins["hopkins::compute_hopkins()<br/>Cluster tendency · 📦 clustertend"]
            CL_Optimal["optimal_clusters.R<br/>Gap statistic · Elbow · NbClust<br/>📦 NbClust · cluster · stats"]
            CL_Run["cluster::run_clustering()"]
            CL_Algo{"Algorithm"}
            CL_KMeans["K-Means / PAM<br/>📦 stats (kmeans) · cluster (pam)"]
            CL_HClust["Hierarchical<br/>📦 stats (hclust, cutree, dist)"]
            CL_DBSCAN["DBSCAN (auto eps)<br/>📦 dbscan"]
            CL_Stats["Silhouette · BSS/TSS<br/>📦 cluster (silhouette)"]
            CL_Heatmap["heatmap.R<br/>📦 pheatmap · ggplot2"]
            CL_Biplot["cluster_biplot.R<br/>📦 factoextra · ggplot2 · ggiraph"]

            CL_Packages["📦 cluster (PAM, silhouette) · dbscan · NbClust<br/>📦 stats (kmeans, hclust) · pheatmap · factoextra"]

            CL_UI --> CL_Source
            CL_Source -->|raw| CL_Raw
            CL_Source -->|pca_scores| CL_PCA
            CL_Source -->|lda_scores| CL_LDA
            CL_Raw --> CL_Hopkins
            CL_PCA --> CL_Hopkins
            CL_LDA --> CL_Hopkins
            CL_Hopkins --> CL_Optimal --> CL_Run --> CL_Algo
            CL_Algo -->|kmeans/pam| CL_KMeans --> CL_Stats
            CL_Algo -->|hierarchical| CL_HClust --> CL_Stats
            CL_Algo -->|dbscan| CL_DBSCAN --> CL_Stats
            CL_Stats --> CL_Heatmap
            CL_Stats --> CL_Biplot
            CL_Stats --> CL_Packages
        end

        %% ==================== PREDICTION MODULE ====================
        subgraph PredictionModule["🔮 Prediction Module"]
            direction TB

            PR_UI["UI: Upload new data · Plotting Controls · Results Display<br/>📦 bslib · DT"]
            PR_Load["bundle_io::load_bundle() · validate_bundle()<br/>Load saved LDA RDS bundle · 📦 base (readRDS)"]
            PR_Valid["validation.R<br/>Check new data vs bundle schema"]
            PR_Classify["predict.R::run_prediction()<br/>📦 MASS (predict.lda/qda)"]
            PR_Plots["prediction_plots.R<br/>Posterior probability plots<br/>📦 ggplot2 · DT"]

            PR_Packages["📦 MASS (predict.lda/qda) · ggplot2 · DT"]

            PR_UI --> PR_Load --> PR_Valid --> PR_Classify --> PR_Plots --> PR_Packages
        end

        %% ==================== POWER ANALYSIS MODULE ====================
        subgraph PowerModule["⚡ Power Analysis Module"]
            direction TB

            PWR_UI["UI: Design · Effect Input · Options<br/>📦 bslib · ggplot2"]
            PWR_Valid["validate.R<br/>Validate effect size / alpha / n inputs"]
            PWR_Dummy["dummy_data.R<br/>Generate illustrative dummy dataset<br/>📦 stats (rnorm, sample)"]
            PWR_Calc["power_calc.R<br/>One-sample / Two-sample / ANOVA<br/>📦 pwr (pwr.t.test, pwr.anova.test) · WebPower"]
            PWR_Plot["Power curve plot · 📦 ggplot2"]

            PWR_Packages["📦 pwr · WebPower · stats (power.t.test) · ggplot2"]

            PWR_UI --> PWR_Valid --> PWR_Dummy
            PWR_Valid --> PWR_Calc --> PWR_Plot --> PWR_Packages
        end

        %% ==================== SHARED INFRASTRUCTURE ====================
        subgraph SharedInfra["🔧 Shared Infrastructure"]
            direction TB
            SH_Error["error_handling.R<br/>safe_execute() · create_app_error()<br/>📦 rlang"]
            SH_Columns["column_utils.R<br/>get_measurement_cols() · get_descriptive_cols()"]
            SH_DataUtils["data_utils.R<br/>create_interaction() · filter_data() · default_palette()"]
            SH_Logging["logging.R<br/>configure_session_logging() · 📦 rhino (log)"]
            SH_Settings["settings.R<br/>get_default_theme() · app_version()<br/>📦 bslib · config"]
            SH_Preproc["preprocessing/<br/>na_handling · normalize · skewness_transform<br/>📦 moments · bestNormalize · stats"]
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
    style LoadDataModule fill:#e3f2fd,stroke:#1976d2,stroke-width:2px
    style MedianModule fill:#fff3e0,stroke:#f57c00,stroke-width:2px
    style PlottingModule fill:#e8eaf6,stroke:#3949ab,stroke-width:2px
    style SummaryModule fill:#f1f8e9,stroke:#558b2f,stroke-width:2px
    style StatisticsModule fill:#fbe9e7,stroke:#bf360c,stroke-width:2px
    style PCAModule fill:#ede7f6,stroke:#4527a0,stroke-width:2px
    style LDAModule fill:#f3e5f5,stroke:#7b1fa2,stroke-width:2px
    style ClusterModule fill:#e8f5e9,stroke:#388e3c,stroke-width:2px
    style PredictionModule fill:#fce4ec,stroke:#c2185b,stroke-width:2px
    style PowerModule fill:#fff8e1,stroke:#fbc02d,stroke-width:2px
    style SharedInfra fill:#f5f5f5,stroke:#616161,stroke-width:2px
    style ExternalPackages fill:#e0f7fa,stroke:#00838f,stroke-width:2px

    style LD_Source fill:#ffcc80,stroke:#e65100
    style MED_GroupBranch fill:#ffcc80,stroke:#e65100
    style PL_XAxis fill:#ffcc80,stroke:#e65100
    style CL_Source fill:#ffcc80,stroke:#e65100
    style CL_Algo fill:#ffcc80,stroke:#e65100
    style LDA_Branch fill:#ffcc80,stroke:#e65100
    style STAT_Omnibus fill:#ffcc80,stroke:#e65100
    style STAT_PostHoc fill:#ffcc80,stroke:#e65100
    style PCA_SkewBranch fill:#ffcc80,stroke:#e65100
    