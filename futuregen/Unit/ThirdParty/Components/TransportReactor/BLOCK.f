      BLOCK DATA

      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'

C*    Gas species
      DATA SPECY/'CO  ','CO2 ','H2O ','H2  ','N2  ',
     &           'SO2 ','NO  ','NO2 ','CH4 ','O2  ',
     &           'C2H6','NH3 ','N2O ','H2S '/

      DATA MWG /
C*    Molecular weights (kg/mol)
C*    CO          CO2         H2O        H2        N2     
     &0.028,      0.044,      0.018,     0.02,    0.028,
C*    SO2         NO          NO2        CH4       O2
     &0.064,      0.030,      0.046,     0.016,    0.032,
C*    C2H6        NH3         N2O        H2S  
     &0.030,      0.017,      0.044,     0.034/
      

      DATA HEATFM /
C*    Heats of formation (J/mol) at 298 K
C*    CO          CO2         H2O        H2         N2
     &-110530.,  -393520.,    -241830.,  0.,        0.,
C*    SO2         NO          NO2        CH4        O2
     &-296840.,   90290.,     33100.,    -74870.,   0.,
C*    C2H6        NH3         N2O        H2S     
     &-83800.,    -45900.,    82050.,    -20500./
     
      DATA XFEED /
C*    composition of air      
     & 0.,         0.,         0.0,   0.,           0.79,
     & 4 * 0.,     0.21,       4 * 0. /   
c     & 0.,         0.,         0.2117,   0.,       0.6228,
c     &  4 * 0.,    0.1655,     4 * 0. /   
!     3 kg air + 0.5 kg h2o

      DATA MWS / 
C*    Molecular weights  (kg/mol)
C*         MC,      MH,      MO,     MN,       MS,       
     &     0.012,   0.001,   0.016, 0.014,    0.032, 
C*         MCA      MCAO,    MCACO3,  MCASO4,   MVOL
     &     0.040,   0.056,   0.100, 0.136,     0.025,
 
     &     10 * 0. /


      END    ! (BLOCK)

