
      SUBROUTINE PRESOL

************************************************************************
* LAST CHANGE AT : 24.05.1995               BY:   J. Hannes            *
************************************************************************
* CALLED BY : CFBC                                                     *
* CALLS     : Z_BUBB, Z_CORE, Z_EMUL, Z_EPSS, Z_EPSA, NETFLO           *
************************************************************************
* COMMON in : HEIGHT(K), NCELLS, NCLASS, NBED, PART(I,M), WRTSCR       *
* out       : CORAVG(K), EMUAVG(K)                                     *
* in & out  :                                                          *
************************************************************************
* passed    : -                                                        *
* returned  : -                                                        *
************************************************************************
* PURPOSE   : calculates the average cross-sectional fractions of      *
*             core, bubble and annulus for use in the gas flow split   *
************************************************************************

      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'

      REAL*8
     &  Z_BUBB, ! bubble fraction in cell
     &  Z_CORE, ! core fraction in cell
     &  Z_EPSS, ! average solid volume fraction in cell
     &  Z_EPSA  ! solid volume fraction in annulus

      REAL*8
     &  EPS,    ! value of Z_EPSS
     &  ZCORE,  ! value of Z_CORE
     &  EMUL    ! value of 1 - Z_BUBB

      REAL*8 
     &  EPCOAV(0:NK),  ! average solids volume fraction of core
     &  EPANAV(0:NK)   ! average solids volume fraction of annulus

      REAL*8  SUM

      INTEGER  I, ! particle class indicator
     &         K, ! riser cell indicator
     &         M  ! particle material indicator

      IF (WRTSCR) write(*,*) ' PRESOL '

       M = BED

       CALL NETFLO(M)

       DO 300 K = 1, NCELLS

C*                   sum up for normalization
                    SUM = 0.
           DO I = 1, NCLASS  
             SUM = SUM + PART(I,M) * Z_EPSS(I,K,M)
           ENDDO


C*           initialize 
           CORAVG(K) = 0.
           EMUAVG(K) = 0.
           EPANAV(K) = 0.
           EPCOAV(K) = 0.

         DO 200 I = 1, NCLASS                                           

C*                   solid volume fraction in cell
           EPS = Z_EPSS(I,K,M)

C*                   core area fraction per class, mat and cell
           ZCORE = Z_CORE(I,K,M)

C*                   emulsion area fraction per class, material and cell
           IF (K .GT. NBED) THEN
                  EMUL = 1.
           ELSE
                  EMUL = 1.- Z_BUBB(MEAND(M),M)
           ENDIF

C*                   average core and emulsion fractions
           CORAVG(K)   = CORAVG(K) + ZCORE*PART(I,M)*EPS/SUM
           EMUAVG(K)   = EMUAVG(K) + EMUL * PART(I,M) * EPS / SUM
                   IF (CORAVG(K) .GE. 1.) THEN
                         CORAVG(K) = 0.99
                   ENDIF
                   IF (1.-EMUAVG(K) .LT. 0.001) THEN
                         EMUAVG(K) = 1.
                   ENDIF

C*                   sum up weighted solid volume fraction in core and annulus of cell
          EPANAV(K) = EPANAV(K) + Z_EPSA(I,K,M)*PART(I,M)
          EPCOAV(K) = EPCOAV(K) + ( EPS-Z_EPSA(I,K,M)*(1.-ZCORE))/ZCORE
     &                                                    *PART(I,M)

 200    CONTINUE


 300  CONTINUE

        IF (WRTSCR)  write(*,*) ' END PRESOL '
  
                                                      
************************ TESTWRITER **************************************

      If (.NOT. WRTOUT(14)) RETURN

      OPEN (UNIT=77,FILE='PREP.DAT',STATUS='UNKNOWN')

      write(77,*) ' CORE and emulsion fraction,',
     &             ' core and annulus concentration'
      write(77,*) 'CELL HEIGHT(m) CORE          EMULSION      EPS CORE',
     &            '      EPS ANNULUS'

         DO K = 0, NCELLS
           WRITE(77,1001) K,HEIGHT(K),CORAVG(K),EMUAVG(K),
     &                                 EPCOAV(K),EPANAV(K)
         ENDDO
 1001 FORMAT(I3,1X,F10.4,4(4X,F10.7))

      CLOSE(77) 

      RETURN
      END          ! (PRESOL)




