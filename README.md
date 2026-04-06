# Hospital-Ratings

This Shiny application  is designed for exploring hospital quality, safety, patient experience, readmission risk, infection risk, birthing-friendly hospitals, VA psychiatric hospitals, and surgery center information across the United States.

The project brings together multiple public CMS and related federal health datasets to help users compare hospital performance at the state, facility, and service-line level. It supports side-by-side review of quality indicators such as overall hospital ratings, patient experience, readmissions, safety, infections, and maternal care measures.

Our data comes from publicly available U.S. federal repositories, primarily the Centers for Medicare & Medicaid Services (CMS) Provider Data Catalog, accessed in February 2026.

## Included Sources

- **Hospital General Information** — hospital identifiers, locations, types, and overall ratings.
- **HCAHPS Patient Survey Scores** — patient experience measures such as communication, cleanliness, responsiveness, and overall hospital rating.
- **Hospital Readmissions Reduction Program** — excess readmissions and payment-reduction-related measures.
- **Hospital-Associated Infections (HAI)** — facility-level infection indicators reported through CMS and CDC NHSN, including CLABSI, CAUTI, MRSA, C. diff, and SSI measures.
- **Birthing-Friendly Hospitals** — CMS designation identifying hospitals participating in a perinatal quality improvement collaborative and implementing evidence-based maternal health practices.
- **Ambulatory Surgery Center Patient Experience** — OAS CAHPS survey measures for outpatient surgery centers.
- **VA Hospital / Psychiatric Facility Measures** — hospital quality measures relevant to VA and inpatient psychiatric facilities where available in CMS reporting datasets.

## Key Measures

This app highlights:
- Overall hospital star ratings.
- Safety of care.
- Readmission rates.
- Patient experience and communication.
- Timeliness and effectiveness of care.
- Hospital-associated infection risk.
- Birthing-friendly designation.
- State-level hospital quality comparisons.
- Selected outpatient and VA-related quality indicators.

CMS Hospital Star Ratings are annual summary ratings that combine multiple quality domains, including safety, readmissions, patient experience, timeliness, and mortality, to help users compare hospitals more easily. HCAHPS is a standardized national survey that supports valid comparisons across hospitals and is publicly reported by CMS.

## Software Used

- **R**
- **Shiny**
- **tidyverse**
- **ggplot2**
- **DT**
- **plotly**
- **sf**

## Data Notes

- Some datasets are updated quarterly or annually, depending on the CMS reporting program.
- Some measures are facility-level, while others are state-level or service-specific.
- Users should confirm the most recent CMS release dates before making operational or policy decisions.

## Disclaimer

This dashboard is for informational and exploratory use only. It is not a clinical decision tool and should not replace professional medical judgment.

## AI Acknowledgement

This project uses AI-assisted development to help with documentation, code organization, and content refinement. All data sources, measures, and interpretations are reviewed by the project team and are based on publicly available CMS and related federal health datasets.

## Authors

- Joanne Lee
- Maren Barclay
