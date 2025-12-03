# Thesis
📌 Overview

This repository contains the complete implementation for my undergraduate thesis at the Human Factors & Applied Statistics Lab (HFAST), University of Toronto.
The project investigates whether driver drowsiness and degraded cognitive states can be detected reliably using multi-modal high-frequency driver monitoring data.

The goal is to design a fully reproducible driver state inference pipeline integrating:
	•	Multi-modal sensor streams (face, gaze, pose, driving dynamics, physiological signals)
	•	Sliding-window time-series feature extraction (30s overlap windows)
	•	Catch22 and statistical time-domain features
	•	Label harmonization and unified annotation structure
	•	Classical ML models for interpretable classification
	•	Full evaluation pipeline (accuracy, F1, confusion matrices, SHAP, ablations)

This system is designed to support real-time or near-real-time deployment in intelligent vehicles, with an emphasis on interpretability, robustness, and ease of integration into larger driver monitoring systems.


Drowsy driving is a major cause of vehicle accidents, yet existing in-vehicle monitoring solutions often rely on:
	•	Single-modal signals (eyelid closure only)
	•	Low-frequency features
	•	Poor generalization across drivers, lighting conditions, or environments

This thesis explores whether multi-modality + time-series features can significantly improve accuracy, stability, and early-warning detection.


🔧 Feature Engineering

Sliding Windows

All signals are aggregated into 30-second sliding windows with configurable overlap.
Window features include:
	•	Means & standard deviations for 20 time-series channels
	•	Catch22 features for each variable (440 total features per window)
	•	Lane-based SD & steering reversal rate
	•	ECG-derived HRV metrics
	•	GSR peak features

Label Unification

Different experimental labels are collapsed into:
	•	Label_unify: human-readable state classes
	•	Label_num: integer-coded classes for modeling

  🤖 Models Implemented

The pipeline supports:
	•	Random Forest
	•	XGBoost
	•	Support Vector Machines
	•	Logistic Regression
	•	LightGBM (optional)
	•	Baseline majority classifier

All models include hyperparameter search via Optuna.

🔍 Key Findings (Short Summary)
	•	Multi-modal fusion substantially outperforms single-modality baselines
	•	Catch22 features provide strong discriminative power for physiological signals
	•	Steering reversal rate + lane SD improve early detection
	•	Face-only models underperform in low-light or off-angle conditions
	•	Ablation studies show physiology + behavior fusion yields highest robustness
  
🧭 Future Work
	•	Integrating deep learning models (TCN, Transformer-based temporal encoders)
	•	Real-time inference optimization
	•	On-device EdgeML deployment
	•	Cross-driver generalization using domain adaptation
	•	End-to-end multi-signal sensor fusion networks
