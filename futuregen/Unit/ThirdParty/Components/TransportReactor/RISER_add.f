
      SUBROUTINE RISER_add

************************************************************************
* LAST CHANGE AT : 19.09.94                 BY:   J. Hannes            *
************************************************************************
* CALLED BY : CFBC                                                     *
* CALLS     : Z_RHOG                                                   *
************************************************************************
* COMMON in : AEXPIN(M), ADDAIR(K), ADDFLU(K), AREA(K),                *
*             CMP(), COALM(), FEED(M), MWG(J), NCELL(K), NCOMP,        *
*             PRESS(1-2), RHOS(M), RG, TAP, TBED, WRTOUT, WRTSCR, XGAS *
* out       : AEXPO(M), BEDMAS, EPSSHM(M), NCELLS, U(K), UANNU(K),     *
*             UBUBBL(K), TEMP(K), KCO, KAN, KBU                        *
* in & out  : HEIGHT(K), WIDTH(K), LENGTH(K), WALL(K), TUBE(K)         *
************************************************************************
* passed    : -                                                        *
* rturned   : -                                                        *
************************************************************************
* PURPOSE   : calculation of the riser's superficial gas velocity,     *
*             the volumetric gas flow, the cross sectional area per    *
*             compartment, the bed mass and the riser volume           *
*             the riser is divided into the number of cells per        *
*             compartment, as given in the input file                  *
*             the indicators CMP(K) are adapted                        *
************************************************************************

       INCLUDE 'PARAMTR.FTN'
       INCLUDE 'COMMONBL.FTN'

	 REAL*8 AIRN,FLUN,GASFL,FLG,YH2ON,VOLA	!zc
	 INTEGER K,J,P					!zc

       IF (WRTSCR) write(*,*) '  RISER GEOMETRY '
         
           GASFL    = 0.
         DO K = 1, NCELLS
		AIRN = ADDAIR(K)*1.01325E5/PRESS(1)*TEMP(K,CORE)/273.15
		FLUN = ADDFLU(K)*1.01325E5/PRESS(1)*TEMP(K,CORE)/273.15
		VOLA = (YVOLA(K)-YO2(K))*RG*TEMP(K,CORE)/PRESS(1)
		YH2ON = YH2O(K)*RG*TEMP(K,CORE)/PRESS(1)
		  FLG = 0.0		!gasif./comb. products, m3/s
		  DO J = 1,NSPECY
		  DO P = 1, 2
		  FLG = FLG + RRCOMB(J,K,P)*RG*TEMP(K,P)/PRESS(1)
		  ENDDO
		  ENDDO

           GASFL = GASFL + AIRN + FLUN + VOLA + YH2ON + FLG
           U(K)  = GASFL/AREA(K)
           UANNU(K) = U(K) / 3.
C*           provisorical bubble velocity, exact calculation in GASMIX
           UBUBBL(K)= U(K)
	   ENDDO
C*           set first and last cells
         U(0)             = U(1)
         UANNU(0)         = UANNU(1)
         UBUBBL(0)        = UBUBBL(1)
         U(NCELLS+1)      = U(NCELLS)
         UANNU(NCELLS+1)  = UANNU(NCELLS)
         UBUBBL(NCELLS+1) = UBUBBL(NCELLS)

      RETURN
      END       ! (RISER)
