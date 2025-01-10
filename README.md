# INF6027 Project: Exploring Audio Features and Clusters in High-Popularity Songs

## Description
This repository contains the coursework project for the INF6027 Introduction to Data Science module. The project involves analyzing the Spotify Tracks Dataset to explore data characteristics, conduct exploratory data analysis (EDA), and perform statistical modeling to uncover insights into audio features and track popularity.

## Dataset
The dataset used for this project is sourced from [Spotify Tracks Dataset](https://hf-proxy-cf.effarig.site/datasets/maharshipandya/spotify-tracks-dataset). The dataset contains a variety of audio features and metadata for Spotify tracks, enabling in-depth analysis of track characteristics and popularity.

### Key Features
- **track_id**: The unique Spotify ID for the track.
- **artists**: Artists who performed the track (separated by `;` for multiple artists).
- **album_name**: The album name where the track appears.
- **track_name**: Name of the track.
- **popularity**: A value between 0 and 100 representing the track's popularity.
- **duration_ms**: Length of the track in milliseconds.
- **explicit**: Whether the track contains explicit content (`true` or `false`).
- **danceability**: A measure from 0.0 to 1.0 describing how suitable the track is for dancing.
- **energy**: A measure from 0.0 to 1.0 representing intensity and activity of the track.
- **key**: The musical key of the track (integer values map to standard pitch notation).
- **loudness**: Overall loudness in decibels (dB).
- **mode**: Indicates modality (1 for major, 0 for minor).
- **speechiness**: Measures the presence of spoken words in the track (ranges from 0.0 to 1.0).
- **acousticness**: A confidence measure from 0.0 to 1.0 of whether the track is acoustic.
- **instrumentalness**: Likelihood of the track containing no vocals (ranges from 0.0 to 1.0).
- **liveness**: Detects the presence of an audience in the recording.
- **valence**: A measure from 0.0 to 1.0 of the musical positiveness conveyed by the track.
- **tempo**: Estimated tempo of the track in beats per minute (BPM).
- **time_signature**: The estimated meter (ranges from 3/4 to 7/4).
- **track_genre**: Genre classification of the track.

## Features
- **Data Cleaning**: Handling missing values, removing duplicates, and standardizing variables.
- **Exploratory Data Analysis (EDA)**:
  - Statistical summaries of numeric variables.
  - Visualizations for distributions and correlations.
- **Feature Analysis**:
  - Relationship between track popularity and audio features.
  - Principal Component Analysis (PCA) for dimensionality reduction.
  - Clustering high-popularity tracks based on audio features.
- **Statistical Modeling**:
  - Hypothesis testing using ANOVA.
  - Regression modeling to explore factors influencing popularity.

## Key Findings

1. **Relationship Between Audio Features and Popularity**:
   - Energy, danceability, and loudness are the most influential audio features for determining Spotify track popularity.
   - High-energy tracks with strong rhythm and engaging dynamics are predominantly found in the top-performing clusters.

2. **Impact of Emotional Characteristics**:
   - Tracks with higher valence (positivity) are strongly associated with higher popularity scores.
   - Tracks in a major mode are more frequently found in the high-popularity category, reflecting their ability to evoke uplifting emotions.

3. **Clustering Analysis of High-Popularity Tracks**:
   - High-popularity tracks tend to combine high energy, danceability, and valence, forming a winning combination that engages listeners.
   - Cluster analysis identified three distinct patterns among highly popular tracks, highlighting different styles or combinations of audio features.


## File Structure
- `INF6027.R`: Main R script for data cleaning, analysis, and visualization.
- `dataset.csv`: Dataset used for analysis.
- `README.md`: Documentation for the project.

## Installation
Clone the repository to your local machine:
```bash
git clone https://github.com/Ccheh/INF6027_Project.git
