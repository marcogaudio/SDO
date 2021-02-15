# SDO
Hospitalization data in Italy

resources:  

- http://www.salute.gov.it/portale/temi/p2_6.jsp?lingua=italiano&id=4362&area=ricoveriOspedalieri&menu=acc#:~:text=Gli%20ACC%20(aggregati%20clinici%20di,ed%20analisti%20di%20politica%20sanitaria.
- https://www.istat.it/it/files//2011/03/glossario1.pdf
- http://www.quotidianosanita.it/allegati/allegato9532784.pdf
- http://www.quadernidellasalute.it/portale/temi/p2_6.jsp?lingua=italiano&id=1349&area=ricoveriOspedalieri&menu=vuoto
- https://www.trovanorme.salute.gov.it/norme/renderPdf.spring?seriegu=SG&datagu=28/01/2013&redaz=13A00528&artp=1&art=1&subart=1&subart1=10&vers=1&prog=001


objective n1: create a tibble from excel files

  file: C_17_tavole_34_2_0_file  
  sheets: from Tav_2.2.6 to Tav_2.2.6 (22)  
  to do:  
  
    1- read sheets from row 4  
    2- discard empty rows in column B  
    3- rename columns A and B  
    4- transform MDC class as new column group  
    5- apply and repeat algorithm to all files  
