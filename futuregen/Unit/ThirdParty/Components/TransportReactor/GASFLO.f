
      SUBROUTINE GASFLO

************************************************************************
* LAST CHANGE AT : 17.06.1994               BY:   J. Hannes            *
************************************************************************
* CALLED BY : CFBC                                                     *
* CALLS     : GASMIX                                                   *
************************************************************************
* COMMON in : ADDAIR(K), ADDFLU(K), AREA(K), CMP(6),COANMX(K),COBUMX(K)*
*             HEIGHT(K), UBUBBL(K), UANNU(K), RG, TBED, PRESS(1),      *
*             CORAVG(K), EMUAVG(K), NBED, NCELLS, WRTSCR               *
* out       : YCORE(K), YANNU(K), YBUBB(K), YTOT(K), YFEEDA(K),        *
*             YFEEDF(K),YH2O(K), YCOAN(K), YANCO(K), YCOBU(K), YBUCO(K)*
*             YVOLA(K), YO2(K), YOUT(K)                                *
* in & out  :                                                          *
************************************************************************
* passed    : -                                                        *
* returned  : -                                                        *
************************************************************************
* PURPOSE   : calculates and initializes the gas flow rates in         *
*             core, annulus and bubble                                 *
************************************************************************

      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'

      REAL*8   MOLES ! molar gas flow (mol/s)
      INTEGER  K     ! cell indicator
	REAL*8 FLG		!zc
	INTEGER J,P		!zc

      IF (WRTSCR) WRITE(*,*) ' GASFLO '

C*        gas mixing parameters
      CALL GASMIX

C*           initializytion
         YCORE(0) = 0.
         YANNU(0) = 0.
         YBUBB(0) = 0. 
         YTOT(0)  = 0.

******************* COMPARTMENT LOOP ***************************

C*          feed flow as defined in input
       DO K=1,NCELLS
           YFEEDA(K)  = ADDAIR(K)*1.01325E5/RG/273.15
           YFEEDF(K)  = ADDFLU(K)*1.01325E5/RG/273.15
       ENDDO

C*          calculate exit flow and the total flows in (mol/s)
      DO  K=1,NCELLS
!zc
			FLG = 0.0
			DO J = 1,NSPECY
			DO P = 1, 2
			FLG = FLG + RRCOMB(J,K,P)
			ENDDO
			ENDDO
!zc
         IF (K .EQ. CMP(6)) THEN
           YOUT(K) = YTOT(K-1)+YFEEDA(K)+YFEEDF(K)
     &              +YVOLA(K)+YH2O(K)-YO2(K)+FLG
         ELSE
           YOUT(K) = 0.
         ENDIF
           YTOT(K) = YTOT(K-1)+YFEEDA(K)+YFEEDF(K)
     &              +YVOLA(K)+YH2O(K)-YO2(K)-YOUT(K-1)+FLG     !zc, -YOUT(K)
         IF(K.GT.CMP(6)) YTOT(K) = 0.0     !zc
      ENDDO

      DO 100 K = 1, NCELLS

C*              gas flow through bubbles
          IF (K .LT. NBED) THEN
           YBUBB(K) =  UBUBBL(K)*AREA(K)*(1.-EMUAVG(K))*PRESS(1)/RG/TBED
          ELSE
           YBUBB(K) = 0.
          ENDIF
          IF (YBUBB(K) .GE. YTOT(K)*0.8) THEN
           YBUBB(K) = 0.8 * YTOT(K)
           WRITE(LOG,*) 'bubble gas flow in cell ',K,
     &                  ' reduced artificially'
          ENDIF

C*               gas flow through annulus
          IF (K .LE. CMP(6)) THEN         !zc, LT
           YANNU(K) = UANNU(K)*AREA(K)*(1.-CORAVG(K))*PRESS(1)/RG/TBED
          ELSE
           YANNU(K) = 0.
          ENDIF    
          IF (YANNU(K) .GE. (YTOT(K)-YBUBB(K))*0.5) THEN
           YANNU(K) = (YTOT(K)-YBUBB(K))*0.5
           WRITE(LOG,*) 'annular gas flow in cell ',K,
     &                  ' reduced artificially'
          ENDIF
         
C*               rest goes through core
           YCORE(K) =  YTOT(K) - YANNU(K) - YBUBB(K)

C*                 cross flows between core-annulus and core-bubbles
           IF (YANNU(K) .LT. YANNU(K-1)) THEN
               YANCO(K) = YANNU(K-1)-YANNU(K)
               YCOAN(K) = 0.
           ELSE
               YCOAN(K) = YANNU(K)-YANNU(K-1)
               YANCO(K) = 0.
           ENDIF    

           IF (YBUBB(K) .LT. YBUBB(K-1)) THEN
               YBUCO(K) = YBUBB(K-1)-YBUBB(K)
               YCOBU(K) = 0.
           ELSE
               YCOBU(K) = YBUBB(K)-YBUBB(K-1)
               YBUCO(K) = 0.
           ENDIF

 100   CONTINUE

      IF (WRTSCR) write(*,*) ' END GASFLO '

C ************************ testwriter **********************************

      IF (.NOT. WRTOUT(8)) RETURN

      OPEN(UNIT=44,FILE='FLOWS.DAT',STATUS='UNKNOWN')
        write(44,*) 'K core    annu    bubb   feedA   feedF    h2o',
     &             '     vol     out     tot'
         DO K = 1, NCELLS       
           write(44,111)K,YCORE(K),YANNU(K),YBUBB(K),
     &    YFEEDA(K),YFEEDF(K),YH2O(K),YVOLA(K)-YO2(K),YOUT(K),YTOT(K)
         ENDDO
  111    FORMAT(I2,9(1X,F7.1))
           write(44,*)
           write(44,*) ' molar flows between phases (mol/s)'
           write(44,*) 
           write(44,*) ' K   CO->BU      CO->AN      CO-BU X   CO-AN X'    

         DO K=1,NCELLS
             MOLES = AREA(K)*(HEIGHT(K)-HEIGHT(K-1))*PRESS(1)/RG/TBED
          write(44,112)K,YCOBU(K)-YBUCO(K),YCOAN(K)-YANCO(K),
     &                   COBUMX(K)*MOLES,COANMX(K)*MOLES
         ENDDO
  112  FORMAT(I3,4(2X,F10.3))

         CLOSE(44)

      RETURN

      END         ! (GASFLO)

