# Quality-Assurance-PurpleAir

PurepleAir (PA) Plantower sensors have high precision, but accuracy can vary due to a multitude of factors including electrical malfunctions, sensor interference from relative humidity, or physical obstruction from insects or debris. Therefore, it is recommended that PM2.5 data should go through a screening of quality assurance before data are analyzed. 

To assess how the quality assurance screening and corrections affect the PM2.5 data, I selected the MathScience Innovation Center PA monitor as there is a DEQ monitor which reports PM2.5 data at the same site, allowing for a comparison. The DEQ PM2.5 values are reported as daily averages. For this investigation, I only used PM2.5 values from 2020 for both DEQ and PA datasets. PA data were retrieved from the API as hourly averages. First, I performed a quality assurance screening step recommended by Barkjohn et al. 2021, and references within, on all three estimations of PM2.5 (atm, cf_1, and alt). Data were excluded from the set if two criteria were met concurrently: the absolute difference between channels A and B was greater than 5 ug, and the calculated absolute percent error was greater than 2 standard deviations ( 2SD: atm = 47, cf_1 = 46, and alt = 36). This QA step removed  < 2% of the total hourly values for the year 2020. In addition to the QA screening, I considered a fourth estimation of PM2.5 utilizing the relative humidity correction recommended by Barkjohn et al. 2021. For this estimate, cf_1 values that passed the QA step were corrected using the following equation:

PM2.5 = 0.524 x PAcf_1 – 0.0862 x RH + 5.75

where PAcf_1 represents the average of channels A and B after QA and RH represents relative humidity obtained from PA sensor. This correction did results in negative concentrations of PM2.5 in some cases (< 3 % of hourly values over the course of 2020). After calculating daily averages, 1 out of the 351 daily averages resulted in a negative concentration for the RH corrected PM2.5 concentrations. This value (-1.3 ug/m3) was replaced by a zero based on the Barkjohn et al. 2022. All other estimates of PM2.5 were converted to daily averages and plotted against the DEQ daily averages (Figure 1). 

![MathSci DEQ and PA](https://user-images.githubusercontent.com/121312601/226421992-9999d437-ed02-4868-adbb-47b418987b67.png)
Figure 1. Daily PM2.5 (ug/m3) concentration values from the DEQ sensor plotted against the PA estimates for (A) atm, (B) cf_1, (C) alt, and (D) relative humidity corrected cf_1. The red line represents a 1:1 ratio, and the blue line represents a linear regression line with formulas printed in the top left quadrant.

The results demonstrate that the atm (Fig. 1A) and cf_1 (Fig. 1B) values consistently overestimate PM2.5 relative to the DEQ values, while the alt (Fig. 1C) and the RH corrected cf_1 estimates are closer to a 1:1 relationship with the DEQ values. The ALT estimates do agree quite well with the DEQ data without any humidity correct, but ultimately the RH corrected cf_1 estimate showed better agreement with the DEQ values. While there is an extra step involved in the RH correction versus the alt values, the advantage to using the RH correction value is that this is a US-wide proposed correction for relatively humidity now adapted by the EPA for PA data. Therefore, for this data set I recommend 1) utilizing the proposed quality assurance protocol, and 2) performing the RH correction to the cf_1 estimate of PM2.5.
	
Barkjohn et al. 2021 - Development and application of a United States-wide correction for PM2.5data collected with the PurpleAir sensor

Barkjohn et al. 2022 - Correction and Accuracy of PurpleAir PM2.5Measurements for Extreme Wildfire Smoke
