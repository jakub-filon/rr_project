# Forecasting S&P 500 Price Movements with Random Forest

A Python-based translation and extension of the original R methodology for forecasting daily S&P 500 index movements using a Random Forest classifier and resampling techniques to mitigate class imbalance.

---

## 📖 Table of Contents

- [Overview](#overview)    
- [Data](#data)  
- [Methodology](#methodology)  
  - [1. Target Construction](#1-target-construction)  
  - [2. Feature Engineering](#2-feature-engineering)  
  - [3. Modeling Pipeline](#3-modeling-pipeline)  
  - [4. Resampling Techniques](#4-resampling-techniques)  
- [Results Summary](#results-summary)  
- [Project Structure](#project-structure)  
- [Installation & Dependencies](#installation--dependencies)  
- [Software Versions Used](#software-versions-used)
---

## Overview

This project formulates **intra-day forecasting** of the S&P 500 index as a **binary classification**:

- **Target**: Will the S&P 500 close above (1) or below (0) its average price computed from market open through 30 minutes before close?
- **Data**: Minute-level S&P 500 index candles.
- **Model**: Random Forest classifier.
- **Challenge**: Class imbalance—addressed via SMOTE, random oversampling, and undersampling.

The Python implementation closely follows the original R approach (see [LuZhang907/RandomForest](https://github.com/LuZhang907/RandomForest)).

---

## Data

- **Raw minute-level files** stored in:
  - `SPX/` (original time range)
  - `SPX_new/` (extended time range)

---

## Methodology

### 1. Target Construction

1. Load minute-level candles (09:30–16:00 US/Eastern).
2. For each trading day:
   - Compute the mean of all “Close” prices up to 30 minutes before market close.  
   - Compare to the final close price:
     - **Above average →** `Y = 1`
     - **Below average →** `Y = 0`

### 2. Feature Engineering

Compute daily averages of a rich set of technical indicators over the early/mid session:

- **Moving Averages**: SMA, EMA, DEMA, GMMA  
- **Momentum & Oscillators**: RSI, MACD, CCI, ROC, Momentum, Ultimate Oscillator, TRIX, KST, SMI  
- **Volatility & Range**: ATR, True Range, Chaikin Volatility, DVI, Volatility  
- **Trend & Directional**: ADX, Aroon, Parabolic SAR, AD Lines, Centre of Gravity  
- **Channels & Bands**: Bollinger, Donchian, Price Bands (percent), CMO Channels  
- **Others**: CLV, ZigZag, VHF, Williams %R/AD, etc.

All features are averaged from market open through 30 minutes before close for each day.

### 3. Modeling Pipeline

1. **Train/Test Split**: First 2/3 of days → training; remaining → testing.  
2. **Scaling**: StandardScaler on training features; apply to test set.  
3. **Classifier**: RandomForestClassifier (100 trees, `random_state=1`, parallel jobs).  
4. **Validation**: 10-fold cross-validation repeated 10 times (accuracy scoring).  
5. **Evaluation**: Accuracy, precision, recall, F1, and confusion matrix on held-out test set.

### 4. Resampling Techniques

To address class imbalance in the training data:

- **Random Under-Sampling** (majority class reduction)  
- **Random Over-Sampling** (minority class duplication)  
- **SMOTE** (synthetic minority interpolation)  
- **Baseline**: no resampling

Each resampling variation is evaluated using the same modeling pipeline.

---

## Project Structure

```yaml
├── SPX/ # Raw minute-level S&P 500 data (original)
├── SPX_new/ # Raw data with updated timeframe
├── csvfiles/ # Legacy output CSVs from R pipeline
├── csvfiles_new/ # New output CSVs from Python pipeline with updated data
├── csvfiles_python/ # New output CSVs from Python pipeline
├── code/ # Original R scripts (e.g., sp500raw.R, sp500_denoised.R)
├── notebooks/ # Jupyter notebooks (translation & data preprocessing)
├── plots/ # Legacy charts and figures from R pipeline
├── requirements.txt # Python dependencies
├── README.md # Project overview and instructions
└── SPXdescription # Description of the legacy data
```

---

## Installation & Dependencies

1. **Clone the repository**  
   ```bash
   git clone https://github.com/jakub-filon/rr_project.git
   cd rr_project
   ```
2. **Create a virtual environment (optional but recommended)**
   ```bash
   python3 -m venv venv
   source venv/bin/activate
   ```
3. **Install dependencies**
   ```bash
   pip install -r requirements.txt
   ```

## Software versions used

- Python 3.12.9
- imbalanced-learn 0.13.0
- matplotlib 3.10.3
- numpy 2.2.6
- pandas 2.2.3
- PyWavelets 1.8.0
- pytz 2025.2
- scikit-learn 1.6.1
- seaborn 0.13.2
