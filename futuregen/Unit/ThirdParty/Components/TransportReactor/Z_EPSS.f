
      REAL*8 FUNCTION Z_EPSS(I,K,M)

************************************************************************
* LAST CHANGE AT : 05.03.95                 BY:   J. Hannes            *
************************************************************************
* CALLED BY : CORFAC, SOLVEL, FLUIDI, SOLCON, Z_CORE, Z_EPSA           *
* CALLS     : -                                                        *
************************************************************************
* COMMON in : AEXPO(I)       exp. decay fector                (1/m)    *
*             CMP(1)         indicator for reference cell              *
*             EPSELU(I,M)    solid volume fraction at infinity (-)     *
*             EPSSD(I,M)     solid volume fraction in dense bed   (-)  *
*             FCOMP(I,M)     compensation factor for voidage changes(-)*
*             HEIGHT(K)      level of cell                       (m)   *
*             NBED           indicator of dense bed surface cell       *
*             U(K)           superficial gas velocity            (m/s) *
*       out :                                                          *
* in & out  :                                                          *
************************************************************************
* passed    : I   size class indicator                                 *
*             K   cell indicator                                       *
*             M   material indicator                                   *
* returned  : Z_EPSS average solid volume fraction in cell             *
************************************************************************
* PURPOSE   : calculates average solids volume fraction in             *
*             cell K for the size class I of material M                *
************************************************************************

      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'

      INTEGER  I, K, M            ! size-,cell-,material-identifier
      REAL*8     A, HB, HT, EI, ED  ! auxiliaries

      IF (K .LE. NBED) THEN
          Z_EPSS = EPSSD(I,M) *U(CMP(1))/U(K) * FCOMP(I,M)  
      ELSE
          HT = HEIGHT(K)-HEIGHT(NBED)
          HB = HEIGHT(K-1)-HEIGHT(NBED)
          A  = AEXPO(M)
          ED = EPSSD(I,M)
          EI = EPSELU(I,M)

        Z_EPSS = ( EI*(HT-HB) + (ED-EI)/A*(EXP(-A*HB)-EXP(-A*HT)) )
     &             / (HT - HB)
     &                 * U(CMP(1))/U(K) * FCOMP(I,M)

      ENDIF


      RETURN
      END              ! (Z_EPSS)



