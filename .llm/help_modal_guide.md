# Help Modal Content Structure Guide

Guidelines for creating and maintaining help documentation for the application's help modal system. Follow these standards to ensure consistent, professional documentation across all modules.

---

## File Organization

Help content lives in `docs/help/{module_name}/` with exactly three possible files:

```
docs/help/{module_name}/
├── overview.md   # Brief introduction and quick reference
├── details.md    # Comprehensive reference documentation
└── faq.md        # Common issues and troubleshooting
```

The help modal automatically detects which files exist and displays them as tabs. If only one file exists, it displays without tabs.

---

## Content Architecture by File

### overview.md

**Purpose**: First point of contact. Brief, scannable introduction that orients users to the module's core functionality.

**Structure**:
1. **H4 heading with module name** — Short description of what the module does
2. **Feature highlights** — 2-4 subsections (H5) covering:
   - Primary actions available (upload, configure, analyze)
   - Key UI elements and their purpose
   - Immediate next steps after loading/activation
3. **Data/output preview** — What users see after completing the primary action

**Tone**: Direct, instructional, minimal explanation. Focus on "what" rather than "why" or "how deep".

**Length**: 30-50 lines maximum. Scannable in under 30 seconds.

---

### details.md

**Purpose**: Comprehensive reference for users who need to understand requirements, conventions, and interpretation guidance.

**Structure**:
1. **H4 heading: Requirements** — What the module needs to function (file formats, data structures, naming conventions)
2. **H4 heading: Technical Specifications** — Concrete rules, patterns, validation logic
3. **H4 heading: Data Interpretation** — How to read outputs, what statistics mean, how to validate results
4. **H4 heading: Quality Assurance** — How to verify correctness, what warning signs to watch for
5. **H4 heading: Best Practices** — Actionable recommendations (bulleted list)

**Tone**: Technical but accessible. Explain "why" behind conventions. Use tables for structured comparisons.

**Length**: 80-150 lines. Comprehensive but not exhaustive.

**Key Elements**:
- **Tables** for comparing options, patterns, or statistics
- **Code/inline formatting** for column names, file extensions, specific values
- **Example structures** (minimal working tables with realistic data)
- **Cross-references** to FAQ for troubleshooting

---

### faq.md

**Purpose**: Problem-solution pairs for common issues, conceptual explanations, and edge cases.

**Structure**:
1. **H4 heading: FAQ title**
2. **`<details>` blocks** — Each containing:
   - `<summary>`: Question phrased as user query (e.g., "Why are visualizations important?", "My columns are not detected correctly")
   - Content: Answer with 1-3 paragraphs, bullet points, or tables as needed

**Tone**: Conversational but professional. Anticipate user frustration. Provide actionable solutions, not just explanations.

**Length**: Variable. Each answer should fully resolve the issue without excess.

**Question Categories** (include as applicable):

| Category | Examples |
|----------|----------|
| **Conceptual** | "Why is X important?", "What does Y mean?", "How do I interpret Z?" |
| **Troubleshooting** | "X is not working", "Unexpected Y behavior", "Z error message" |
| **Validation** | "How much missing data is acceptable?", "What thresholds should I use?" |
| **Technical constraints** | "Can I use X format?", "Why only first sheet?", "No header row?" |
| **Data quality** | "Unexpected patterns in charts", "Wrong variable types", "Outliers" |

---

## Writing Standards

### Heading Levels

| Level | Usage |
|-------|-------|
| `####` (H4) | Primary sections within files. Start every file with H4. |
| `#####` (H5) | Subsections for grouping related points |
| `######` (H6) | Rare — only for deep nesting within complex subsections |

**Never use H1-H3** in help files — they are too prominent for sidebar content and may conflict with application UI.

### Formatting Conventions

| Element | Format | Example |
|---------|--------|---------|
| UI labels | Bold | **Load** button, **Example Datasets** dropdown |
| File formats | All caps | CSV, XLSX, TSV |
| Column names | Code backticks | `SAMPLE_ID`, `Asfc` |
| Code patterns | Code backticks | `^[A-Z_]+$` |
| User actions | Bold imperative | Click **Load**, Select from dropdown |
| Key statistics | Bold numbers | **>30% missing**, **<20 unique values** |

### Content Principles

1. **overview.md**: Tell users what they can do and what they'll see
2. **details.md**: Tell users how it works and what the rules are
3. **faq.md**: Tell users why something happens and how to fix it

### Cross-Reference Strategy

- Reference FAQ from details.md when mentioning common pitfalls: "See the FAQ for troubleshooting import errors"
- Reference details.md from FAQ for deep explanations: "See Details tab for naming convention specifications"
- Never reference overview.md — it should stand alone as the entry point

---

## Review Checklist

Before committing help content:

- [ ] All files start with H4 (`####`)
- [ ] overview.md is under 50 lines and scannable
- [ ] details.md includes at least one table for structured information
- [ ] faq.md uses `<details>` blocks for every entry
- [ ] No H1-H3 headings anywhere
- [ ] UI elements are bolded (**Button Name**)
- [ ] Technical terms (column names, patterns) use code formatting
- [ ] All three files maintain consistent terminology
- [ ] FAQ questions are phrased as user queries, not statements
- [ ] Each FAQ answer provides actionable guidance, not just explanation

---
