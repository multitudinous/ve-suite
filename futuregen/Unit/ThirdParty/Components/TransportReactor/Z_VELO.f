
      REAL*8 FUNCTION Z_VELO (DP,M)

************************************************************************
* LAST CHANGE AT :                          BY:                        *
************************************************************************
* CALLED BY : PREPAR, SOLVEL, ELUTRI, CYCLON, Z_UMF                    *
* CALLS     : Z_RHOG, Z_VIS                                            *
************************************************************************
* COMMON in : G, PRESS(1), RHOS(M), TBED                               *
* out       :                                                          *
* in & out  :                                                          *
************************************************************************
* passed    : DP     -  particle diameter (m)                          *
*             M      -  material indicator                             *
* returned  : Z_VELO -  terminal velocity (m/s)                        *
************************************************************************
* PURPOSE   : Calculation of the free fall terminal velocity of a      *
*             particle according to Kunii and Levenspiel (1969),       *
*             Fluidization Engineering,p.76                            *
************************************************************************

      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'

      REAL*8      Z_RHOG, RHOG, ! gas density (kg/m3)
     &          Z_VIS,  VISG  ! gas viscosity (kg/ms)
      REAL*8      AR,           ! archimedes number
     &          DP,           ! particle diameter (m)
     &          REP           ! particle reynolds number
      INTEGER   M             ! material indicator

      RHOG = Z_RHOG(TBED,PRESS(1))
      VISG = Z_VIS(TBED)

C*     archimedes number
      AR = G * DP**3. * (RHOS(M) - RHOG) * RHOG / VISG**2.

C*     particle reynolds number REP as a function of AR
      IF (AR .LE. 103.)  THEN
         REP = AR / 18.
      ELSE IF (AR .LE. 94260.) THEN
         REP = ( (4./225.) * AR**2. ) ** (1./3.)
      ELSE
         REP = (3.1 * AR) ** 0.5
      ENDIF
      
C*     terminal velocity TVELO  (m/s)
      Z_VELO =  (REP * VISG) / (DP * RHOG)  

      RETURN
      END             ! (Z_VELO)


