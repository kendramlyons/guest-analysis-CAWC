# guest-analysis-CAWC
**Guest stats report for Casa Alitas Program**
  Updated: 5/6/2022

**Curation Rationale**

The rationale of this project were:

  - to design a reproducible report for the Casa Alitas program that can be updated as new data are collected
  - to better understand the needs and characteristics of the population being served at the shelter in order to improve services 
  - to understand temporal trends in the numbers of arrivals and connect them to events such as U.S. policy shifts (and broader world events)
  - to make data of interest available to the public while also protecting the privacy of a vulnerable population

Trends analyzed include country of origin, destination state, language spoken, group size, arrival date (at the shelter), duration of stay and daily, weekly or monthly arrivals. 

**Participant Demographic**

The subjects of this data are asylum-seeking migrants from numerous countries who have typically been detained, interviewed, processed and then released by U.S. immigration authorities in Southern Arizona. They are allowed to stay while they await a hearing and eventual decision on their asylum cases. From Arizona, they travel to a locations all over the United States where a sponsor (typically a relative or sometimes a friend) awaits them. 

Casa Alitas receives family groups as well as adult individuals. Group sizes range from two to eight individuals who have traveled to the U.S. together. Their stays at the shelter are typically brief (one or two nights) but several factors necessitate longer stays (weeks or months longer, in rare cases). These factors include medical needs, issues with sponsors or paperwork, and separation from family members who remained behind in detention. These issues are documented, but not necessarily systematically, and so not explicitely represented in the data.

Many languages are spoken by guests, but Spanish is the most common, followed by Portuguese. Indigenous languages are specified in the data rather than grouped together. A large majority of guests are Latin American, but some arrive from overseas. They are of all ages (new mothers have even given birth during their stays) and many backgrounds, but they are all seeking a refuge from violence and injustice.

**Curator Demographic**

The data for this project was curated by Kendra Lyons, a final semester master's student in Information at the University of Arizona's iSchool, as a final Capstone project. Kendra is a 31 year old white woman (she/her/they) and leftist from Wyoming who is interested in undertaking critical data science in the public sector and in support of grassroots organizing, humanitarian work and policy initiatives.

**Data Collection**

Data were collected by trained intake volunteers and staff in order to facilitate the provision of services. iPads are used by staff to quickly add information to a private spreadsheet in the cloud. Information is then filtered according to service-type and displayed for reference. Assistance with travel arrangements and transportation to bus stations or the airport is offered, in addition to clothing, hygeine supplies, meals, snacks and shelter as needed. Volunteers and staff do their best to meet many additional needs as they arise. 

Data collection practices have been adapted, necessitated by shifting circumstances and growing need, to give staff and lead volunteers access to information about the hundreds of guests they assist. Much of this data is personal or irrelevant to this analysis. Variables examined in this project include arrival date, group size, country of origin, destination city and state, language spoken, method of travel, travel company, stay length at a shelter location in days and number of overall daily, weekly and monthly arrivals. 

**Data Pre-Processing and Limitations**

Data have been pre-cleaned but may still contain errors. Collection is by group, so the data have been "individualized" for this analysis by duplicating each row according the the group size it represents. This introduces some noise into the data, especially for family groups who are multinational or multilingual. In some cases data have been aggregated by date or category. Variables extrapolated from the data include stay length (from arrival and departure dates), week number, arrival year and arrival month, but extrapolation was not pristine and there are some errors. 

**Privacy Statement**

Because much of the data collected about people who pass through the shelter are of a personal and sensitive nature, raw data is not stored in this repository. Data have been de-identified by removing names and other private, non-relevant information. 

**Further Information**

Casa Alitas Program Website: https://www.casaalitas.org/ 
