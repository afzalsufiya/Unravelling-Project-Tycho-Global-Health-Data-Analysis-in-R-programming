# Unravelling Project Tycho: Global Health Data Analysis in R Programming

This project explores historical epidemiological data in the United States from **1888 to 2014** using **Project Tycho** datasets. Using **R programming**, the analysis identifies patterns, trends, and regional impacts of various infectious diseases, with a focus on **Measles**, **Scarlet Fever**, and **Influenza**.

---

## 📊 Key Objectives

- Analyze and visualize disease prevalence over time.
- Identify the top 3 most widespread diseases in the U.S.
- Understand regional variations in disease impact.
- Demonstrate the power of R for public health research.

---

## 🛠️ Tools & Technologies

- **Language**: R
- **Libraries**: 
  - `dplyr`, `ggplot2`, `gganimate`, `readr`, `plotly`, `shiny`, `tidyr`, `scales`, `RColorBrewer`
- **Data Sources**:
  - [Project Tycho](https://www.tycho.pitt.edu/) ( Note: Download dataset from this link: [ProjectTycho_Level2_v1.1.0.csv](https://drive.google.com/file/d/1Ur6xlvX_fd-UMscyghyi3pFiNPAjUYzx/view?usp=sharing) )
  - [US Census Regions](https://github.com/cphalpert/census-regions/blob/master/us%20census%20bureau%20regions%20and%20divisions.csv)

---

## 🚀 Usage

### 1. Clone the Repository
```bash
git clone https://github.com/yourusername/project-tycho-analysis.git
cd project-tycho-analysis
```
### 2. Open in RStudio
Launch the .Rmd or .R file in RStudio.
### 3. Install Required Packages
```bash
install.packages(c("dplyr", "ggplot2", "gganimate", "readr", "tidyr", "scales", "RColorBrewer"))
```
### 4. Load & Run the Script
Make sure the dataset files (usregion.csv, cleanedata.csv, etc.) are in your working directory. Then, execute the script section by section.

## 📈 Visualizations & Analysis

### 1. Most Affected Diseases by State
Animated bar charts illustrate how different diseases dominated specific U.S. states across decades.

### 2. Top 3 Diseases Overall
Using frequency analysis and bar plots, we identified:
- **Measles**
- **Scarlet Fever**
- **Influenza**
as the most recurring infectious diseases over the 126-year span.

### 3. National Trends Over Time
A line graph visualizes the year-wise total case numbers for each of the top 3 diseases, showing peaks and trends.

### 4. Regional Disease Distribution
A boxplot (log-transformed) displays how deaths and cases vary across U.S. regions, offering insights into public health disparities.

---

## 💡 Results Summary

- **Measles**, **Scarlet Fever**, and **Influenza** were consistently the top 3 reported diseases.
- Certain states and regions bore the brunt of outbreaks more frequently.
- Data visualization made it easier to communicate large-scale findings effectively.

---

## 🧠 Key Takeaways

- Data storytelling is powerful when paired with strong visuals.
- Historical health data can inform modern policy and prevention.
- R provides a full pipeline: from wrangling to visualization.

---

## 📚 References

- Project Tycho: https://www.tycho.pitt.edu/
- CDC National Notifiable Diseases: https://www.cdc.gov/nndss/about/index.html
- US Census Region Data: https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf
- R Markdown: https://rmarkdown.rstudio.com/

---

## 🤝 Let's Connect

If you're interested in data science, or public health analytics or have any question regarding this repo, feel free to connect or reach out!

🔗 LinkedIn: [www.linkedin.com/in/asufiya](https://www.linkedin.com/in/asufiya)

✉️ Email: [afzal.sufiya@outlook.com](afzal.sufiya@outlook.com)

