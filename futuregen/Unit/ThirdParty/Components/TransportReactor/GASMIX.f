
      SUBROUTINE GASMIX

************************************************************************
* WRITTEN        : 1993                     BY:   J. Hannes            *
* LAST CHANGE AT : 05.03.1995               BY:   J. Hannes            *
************************************************************************
* CALLED BY : GASFLO                                                   *
* CALLS     : Z_BUBB, Z_UMF                                            *
************************************************************************
* COMMON in : AREA(K)      - cross sectional area in comp. K   (m2)    *
*             CORAVG(K)    - cross sec. fraction of core               *
*             EMUAVG(K)    - cross sec. fract. of emulsion in dense bed*
*             G            - gravity acceleration                      *
*             HEIGHT(K)    - elevation of cell #K                      *
*             MEAND        - Sauter mean diameter of particle          *
*             NBED         - cell number of dense bed surface          *
*             NCELLS       - total number of cells in riser            *
*             NNOZZ        - number of Nozzles at distributor plate (-)*
*             U(K)         - sup. gas velocity (m/s)                   *
*             UBUBBL       - bubble rising velocity                    *
*             WRTSCR       - write to screen command                   *
* out       : COANMX(K)    - mixing rate core-annulus (mol/ms)         *
*             COBUMX(K)    - mixing rate emulsion-bubble (mol/ms)      *
* in & out  :                                                          *
************************************************************************
* passed    : -                                                        *
* returned  : -                                                        *
************************************************************************
* PURPOSE   : calculates the lateral mixing rates of the phases        *
************************************************************************

      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'

      REAL*8    Z_BUBB,DELBUB, ! bubble fraction (-)
     &          Z_UMF, UMF     ! min. fluid. velocity (m/s)
      REAL*8    CBUB,          ! bubble circumference       (m)
     &          DBUB(NK),      ! bubble diameter  (m)
     &          NBUB,          ! number of bubbles (m)
     &          UTR            ! bubble throughflow velocity (m/s)
      REAL*8    DDIF,          ! lateral gas dispersion (m2/s)
     &          PE             ! Peclet number (-)
      INTEGER   K              ! Riser cell indicator


       IF (WRTSCR) write(*,*) ' GASMIX '


*************** Exchange gradient bubble - emulsion *****************

      NNOZZ = 1600

             UMF    = Z_UMF(MEAND(BED),BED)
             DELBUB = Z_BUBB(MEAND(BED),BED)        

      DO K = 1, NCELLS

           IF(U(K) .GT. UMF .AND. NBED .GT. K) THEN

C*                 bubble diameter (m) (Darton) 
              DBUB(K) = 0.54 * (U(K)-UMF)**0.4 / G**0.2
     &          * (HEIGHT(K)+4.0*SQRT(AREA(1)/NNOZZ))**0.8

C*                 bubble rising velocity (m/s)
              UBUBBL(K) = U(K) - UMF + 0.711 * SQRT(G*DBUB(K))

C*                 number of bubbles in cross section (-)
              NBUB = DELBUB*AREA(K) / (PI*DBUB(K)**2./4.)

C*                 circumference times number of bubbles
              CBUB = PI * DBUB(K) * NBUB

C*                 gas throughflow velocity of bubbles (m/s)
              UTR  =(U(K)-(1.-DELBUB)*UMF)/DELBUB-UBUBBL(K)

C*            model a) gas exchange coefficient (m3/m3s)
            COBUMX(K) = UTR * CBUB / AREA(K) * (1.-EMUAVG(K))

C*            model b) Bellgadt & Werther (1986) (m3/m3s)

C              DDB = 0.67E-3 + 0.023 / HEIGHT(NBED) * DELBUB
C    &               /(1.-DELBUB)*SQRT(G*DBUB(K)**3.)* HEIGHT(K)

C              COBUMX(K) = DDB/AREA(K)/CORAVG(K)/EMUAVG(K)

            ELSE
             COBUMX(K) = 0.
            ENDIF

        ENDDO


*************** exchange gradient core-annulus ***********************
C*           Kruse, Werther, Chem.Ing.Tech.64(1992) 372-373
             PE = 465.
        DO K = 1, NCELLS
C*           initialization
          IF (K .LE. NBED) THEN
             COANMX(K) = U(K) / (HEIGHT(K)-HEIGHT(K-1))
          ELSE
             DDIF = 2. * U(K) * SQRT(CORAVG(K)*AREA(K)/PI) / PE
C*                     (m3/m3s)
             COANMX(K) = DDIF / AREA(K) 
          ENDIF
        ENDDO

        IF (WRTSCR) write(*,*) ' END GASMIX '

********************** testwriter *********************************

         IF (.NOT. WRTOUT(9)) RETURN


         OPEN (UNIT=77,FILE='GASMIX.DAT',STATUS='UNKNOWN')

         WRITE(77,*) 'Lateral gas mixing'

         WRITE(77,*)'Height(m)  CO-BU(mol/m3s) CO-AN(mol/m3s) ',
     &              'Bub ris Velo(m/s), Bub diam (m)'
         DO K = 1, NCELLS
          WRITE(77,1001) HEIGHT(K),COBUMX(K),COANMX(K),UBUBBL(K),DBUB(K)
         ENDDO
                                                     
 1001    FORMAT(F10.4,2(2X,E12.4),2X,F10.4,2X,F10.4) 

         CLOSE(77)

       RETURN
       END         ! (GASMIX)

