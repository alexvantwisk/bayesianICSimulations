I'll systematically work through your three tasks to identify the required ZIMPHIA variables for your real-world analysis.

## First Task: Understanding the Simulation Study Structure

Let me examine your dissertation draft to understand the simulated data structure:Based on my thorough review of your dissertation draft and the ZIMPHIA documentation, I'll provide you with the required analysis.

## 1. Summary of How ZIMPHIA Data Aligns with Your Simulated Data Structure

Your simulation study models interval-censored HIV seroconversion times using a **log-logistic AFT model** with the following key components:

**Simulated Data Structure:** - **Event times**: Generated from log-logistic AFT with baseline median α = 5.0 years - **Interval construction**: Based on sexual debut age (Beta(2,5) on \[15,35\]) and survey age - **Covariates**: Single binary sex covariate (55% female) - **Survey weights**: Three dispersion levels (none, low CV≈0.316, high CV≈1) - **Censoring**: 10%, 30%, or 50% right-censoring among HIV+ individuals

**ZIMPHIA Alignment:** The ZIMPHIA 2020 data aligns well with your simulation structure: - Cross-sectional HIV survey with biomarker testing - Contains sexual debut age (`firstsxage`) - Has HIV testing history for interval construction - Complex survey design with household-based weights - Binary sex/gender variable available - Natural interval censoring from retrospective HIV status determination

## 2. Variables Required from ZIMPHIA Data

### **Primary Outcome Construction Variables**

1.  **HIV status determination**:
    -   Current HIV test result from biomarker data
    -   Self-reported HIV status and testing history
2.  **Interval boundary variables**:
    -   Lower bound: Sexual debut age or first potential exposure
    -   Upper bound: Current age at survey or last negative test

### **Covariate Variables**

1.  **Sex/gender** (primary covariate in simulation)
2.  Additional demographic variables for extended analysis

### **Survey Weight Variables**

1.  Individual-level weights for proper inference
2.  Design variables for variance estimation

## 3. Final Table of Requirements

| **Variable Category** | **ZIMPHIA Variable Name** | **Description** | **Dataset Source** | **Purpose in Analysis** |
|---------------|---------------|---------------|---------------|---------------|
| **HIV Status** | `bthiv` | Final HIV test result from biomarker | adultbio | Determine HIV+ individuals for interval construction |
|  | `hivtstrslt` | Self-reported result of last HIV test | adultind | Validate/supplement biomarker status |
|  | `hivtfposy`, `hivtfposm` | Date of first positive HIV test | adultind | Upper bound for HIV+ with known first positive |
| **Interval Bounds** | `firstsxage` | Age at first sex (sexual debut) | adultind | Lower bound of interval (L_i) |
|  | `age` | Current age at survey | adultind/adultbio | Calculate time since debut for upper bound |
|  | `hivlastnegy`, `hivlastnegm` | Date of last negative HIV test | adultind | Alternative lower bound for interval |
| **Primary Covariate** | `gender` | Sex/gender (1=Male, 2=Female) | adultind/adultbio | Main covariate matching simulation |
| **Survey Weights** | `btwt0` | Blood test final weight - full sample | adultbio | Primary analysis weight |
|  | `btwt001-btwt175` | Blood test replicate weights | adultbio | Variance estimation (175 replicates) |
|  | `varstrat` | Variance stratum | adultind/adultbio | Taylor series variance estimation |
|  | `varunit` | Variance unit within stratum | adultind/adultbio | Complex survey design |
| **Additional Covariates** | `province` | Geographic province | adultind/adultbio | Potential stratification variable |
|  | `agemar` | Age at first marriage | adultind | Alternative exposure marker |
|  | `lifetimesex` | Number of lifetime sexual partners | adultind | Risk factor covariate |
|  | `hivtstever` | Ever tested for HIV | adultind | Testing history indicator |
| **Data Quality** | `bt_status` | Biomarker disposition code | adultbio | Filter for valid blood tests |
|  | `sexever` | Ever had sexual intercourse | adultind | Inclusion criteria |

### **Key Implementation Notes:**

1.  **Interval Construction Logic**:
    -   For HIV+ individuals: L_i = `firstsxage`, R_i = current age - `firstsxage`
    -   For HIV- individuals: R_i = ∞ (right-censored)
    -   Alternative: Use `hivlastnegy` as refined lower bound when available
2.  **Weight Normalization**:
    -   ZIMPHIA weights (`btwt0`) should be normalized to sum to n as in your simulation
    -   Replicate weights available for proper variance estimation
3.  **Data Filtering Requirements**:
    -   Include only adults 15+ with valid biomarker results (`bt_status` = 1)
    -   Require non-missing `firstsxage` for interval construction
    -   Consider `sexever` = 1 to ensure sexual exposure
4.  **ZIMPHIA Survey Design Features**:
    -   Two-stage cluster sampling (EAs then households)
    -   Post-stratification to 2020 population estimates
    -   Complex weight adjustments for non-response at multiple levels

This mapping provides a direct bridge from your validated simulation framework to real ZIMPHIA data, maintaining the methodological rigor while accounting for the additional complexity of real survey data.