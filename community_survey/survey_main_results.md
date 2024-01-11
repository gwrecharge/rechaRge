# 1. Introduction

The project team conducted a simple survey to assess the community’s expectations regarding the upcoming package and to determine which groundwater recharge estimation methods could be integrated into the rechaRge package. The survey was taken by 54 potential future users and the main results are summarized below.

The scientific background of potential future users was mostly in hydrogeology and hydrology. <br />
![01-plot-sci_background](https://github.com/gwrecharge/rechaRge/assets/118429693/8f359224-1641-4b13-a7ea-22854f967b5f) <br />
*Scientific background.*

Both R and Python were the favorite coding languages of the potential future users. <br />
![02-plot-code_lang](https://github.com/gwrecharge/rechaRge/assets/118429693/0621dc2f-587d-40cc-ae74-ac153d561df5) <br />
*Preferred coding languages.*

The majority of potential future users of the package are affiliated with academic institutions or work as professionals. <br />
![03-plot-affiliation](https://github.com/gwrecharge/rechaRge/assets/118429693/a3402873-33ea-42ea-86cb-e3c0c6043fae) <br />
*Type of affiliations.*

The affiliations of the potential future users were located around the world with a majority in Western Europe. <br />
![03-plot-affiliation_2_country](https://github.com/gwrecharge/rechaRge/assets/118429693/9ccaeff5-4264-4c3b-bd68-dfd9e8544c42) <br />
*Location of the affiliations.*

Potential future users had a wide range of study areas, covering most climate zones, from cold to hot and arid regions, spread around the globe.

# 2. Intended functions for the package

The demand for integrating an automatic calibration method into the package was clear. However, the choice of the method seemed less important. Therefore, we will proceed with the caRamel algorithm. <br />
![04-plot-calib](https://github.com/gwrecharge/rechaRge/assets/118429693/a29d7875-cb2b-4107-a780-8ee41d3c8d0f) <br />
*Interest in integrating an automatic calibration method into the package.*

Similarly, the demand for integrating a sensitivity analysis into the package was clear. Although the choice of the method was not determinant here either, moving from a “One-at-a-time” method to a “global sensitivity” method will be assessed. <br />
![06-plot-sensi_ana](https://github.com/gwrecharge/rechaRge/assets/118429693/9640d7f4-2390-41f5-bc4b-60881c801852) <br />
*Interest in integrating a sensitivity analysis into the package.*

Likewise, the demand for integrating a computation of the model uncertainty into the package was clear. <br />
![08-plot-uncertainty](https://github.com/gwrecharge/rechaRge/assets/118429693/60863bbf-e364-457a-893b-e9ade7d7a777) <br />
*Interest in integrating a computation of model uncertainty into the package.*

# 3. Data formatting and utility functions

The netcdf format seems to be of interest to modelers, but not all of them. The netcdf format was of interest for being used directly as input data, extracting of data from netcdf files, and exporting simulation results as netcdf files. <br />
![09-plot-netcdf](https://github.com/gwrecharge/rechaRge/assets/118429693/bf96d221-a3a8-4954-b2c4-05d359b95d5f) <br />
*Interest in using input data in netcdf.*

![10-plot-extract_netcdf](https://github.com/gwrecharge/rechaRge/assets/118429693/500c94a8-e335-4782-9d3f-e08210fea5af) <br />
*Interest in extracting data from netcdf files.*

![11-plot-conv_netcdf](https://github.com/gwrecharge/rechaRge/assets/118429693/965306aa-94bc-4c69-9df6-563ca7fbae67) <br />
*Interest in converting tabular data into netcdf files and vice versa.*

![12-plot-export_netcdf](https://github.com/gwrecharge/rechaRge/assets/118429693/792d34f9-5aa2-4c62-98f9-6f55e5dde35e) <br />
*Interest in exporting simulation results in netcdf files.*

Additionally, functions performing spatial associations (between grid and catchments, for example) directly integrated into the package interested potential future users. <br />
![13-plot-spatial_an](https://github.com/gwrecharge/rechaRge/assets/118429693/4d4709e7-4489-41c0-b43f-67f66236e883) <br />
*Interest in spatial association functions.*

The interest in integrating a function estimating baseflow using the Lyne and Holick filter with a statistical calibration was relatively low; therefore, this will not be integrated into the future package. It was commented that a package dedicated to baseflow estimation with various methods would be pertinent. <br />
![14-plot-lh_qbase](https://github.com/gwrecharge/rechaRge/assets/118429693/df8db2ce-3f98-4fd7-900d-f2ca8061a5e1) <br />
*Interest in a function for baseflow estimation using the Lyne and Hollick filter.*

Finally, supplementary functions were suggested: 
* Conversion of recharge estimates into a Modflow recharge input format;
* Plotting function for scatterplot to compare simulated and observed variables.

&Rightarrow; If anyone developed or is developing a code to convert spatial data to Modflow input files, it would be very interesting to publish it in the package. Please get in touch with the project team!

&Rightarrow; Adding a function for scatterplot in the package is doable.

# 4. Notifications of interest for other groundwater recharge models and groundwater recharge estimation methods to be included in the package

These models and methods were listed as of interest to be included in the package:
* Hydrus 1D
* Wetspass
* Water table fluctuation
* Linkage to OpenFOAM
* Chloride mass balance
* Pastas
* Bagluva method

&Rightarrow; The project team will get in touch with the developers of the suggested models.

&Rightarrow; If anyone has developed (or is interested to develop) codes for the water table fluctuation method, the chloride mass balance, and the Bagluva method, and would like to have them published in the package, please get in touch with the project team.

# Link to other coding languages

Finally, there was an interest in having access to the package from another coding language, mostly Python. The project team will evaluate the technical feasibility to create such links. In the meantime, if anyone is interested in working on this specific question, please get in touch with the project team. <br />
![18-plot-other_lang](https://github.com/gwrecharge/rechaRge/assets/118429693/9eb65143-acba-4391-ad56-30aa48d5e043) <br />
*Interest in accessing the package from another coding language.*
