
      SUBROUTINE COMBUS 

************************************************************************
* LAST CHANGE AT :  29.06.1994              BY:   J. Hannes            *
************************************************************************
* CALLED BY : CFBC                                                     *
* CALLS     : CHARTR,PROPER                                            *
************************************************************************
* COMMON in : ADDAIR(K), CALVAL, CMP(6), COALM(), DIAM(I), FEED(M),    *
*             HEATFM(J), KCOMMON, MWG(J), MWS(), NCELLS, NCLASS, RG,   *
*             TBED, RRTOT(I), VAPH2O, WRTOUT, WRTSCR, XGAS(J,K,P)      *
* out       :                                                          *
* in & out  :                                                          *
************************************************************************
* PURPOSE   : Pre-calculation of combustion rates and steciometry      *
************************************************************************


      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'

      REAL*8            RT(NI), ! combustion rate (mol/kgs)
     &                  DUMMY(NI) 
      REAL*8            XXO2,   ! O2 molar fraction (-)
     &                  AIRSTC, ! AIR steciometric flow (kg/s) 
     &                  AIRIN,  ! AIR flow in feed gas (kg/s)
     &                  LAMBDA  ! over-steciometric ratio
      INTEGER           I, K 

      IF (WRTSCR) WRITE (*,*) ' COMBUS '

C*         minimum need of air (kgAIR/kgFuel)
      AIRSTC = FEED(COAL)*(1.-COALM(MOIS))
     &  * ( COALM(1)*2.67 + COALM(2)*8. - COALM(3) + COALM(5) )/ 0.232

        AIRIN = 0.
      DO K=1,NCELLS
        AIRIN = AIRIN + ADDAIR(K)*1.286
      ENDDO

      IF (AIRSTC .GT. 0.) THEN
        LAMBDA = AIRIN / AIRSTC
      ELSE
        LAMBDA = 0.
      ENDIF

      CALVAL = (1.-COALM(MOIS))*
     &       (  COALM(COALC)/MWS(COALC)*HEATFM(CO2)
     &        + COALM(COALH)/MWS(COALH)*HEATFM(H2O)
     &        + COALM(COALS)/MWS(COALS)*HEATFM(SO2))
     &      - COALM(MOIS)*VAPH2O


C*                   set properties for actual temperature
             CALL PROPER(TBED)

C*             for combustion: guess average oxygen concentration as 7%
       XXO2    = 0.01

              KCOMMON = 1
C*        initial char combustion rate RT(I) (mol/kgs)
         CALL CHARTR(TBED,XXO2,RT,DUMMY)
C*         char combustion rate RT returned by gas phase calculation
C*         delivered to common - block variable RRTOT       
        DO I = 1, NCLASS
          RRTOT(I) = RT(I)
        ENDDO

        DO K=1,CMP(6)
          XGAS(O2,K,CORE) = XXO2
          XGAS(O2,K,ANNU) = XXO2
        ENDDO

C********************** testwriter ************************************

         IF (.NOT. WRTOUT(4)) RETURN

         OPEN(UNIT=44,FILE='COMBUS.DAT',STATUS='UNKNOWN')
           WRITE(44,*) ' LAMBDA :                ',LAMBDA
           WRITE(44,*)
           WRITE(44,*) ' Calorific value (MJ/kg) ',CALVAL*1.E-6
           WRITE(44,*)
           WRITE(44,*) 'CLASS               DIAMETER(mym)          '
     &                ,'        RATE (molC/kgCOALs) '
          DO I=1,NCLASS
           WRITE(44,*) I, DIAM(I)*1.E6, RT(I)
          ENDDO
         CLOSE(44)

      RETURN                     
      
      END  ! (COMBUS)


