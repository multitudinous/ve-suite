
      SUBROUTINE CHARRT
************************************************************************
* LAST CHANGE AT : 09.02.1995                BY:   J. Hannes            *
************************************************************************
* CALLED BY : CFBC                                                     *
* CALLS     :                                                          *
************************************************************************
* COMMON in : NSPECY, NCELLS, COALM(M), MWS(M),XGAS(J,K,P)             *
* out       : RRCOMB(J,K,P),RRTOT(I)                                   *
* in & out  :                                                          *
************************************************************************
* passed    : -                                                        *
* returned  : -                                                        *
************************************************************************
* PURPOSE   : Links the solids char combustion rates to the gas source *
*             terms                                                    *
************************************************************************

      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'
      INCLUDE 'KINETICS.FTN'

      REAL*8   DDM,        ! auxiliary
     &         COMCOR,     !    "
     &         COMANN      !    "

      REAL*8   RTSUM,      !    "
     &         DVSUM,      !    "
     &         SUM,        !    "
     &         SUMSUM,     !    "
     &         GSUM        !    "
      INTEGER  I,          ! size indicator
     &         J,          ! specy indicator
     &         K,          ! cell indicator
     &         P           ! phase indicator

      REAL*8 XH2OS,XCO2S,XH2S,COMW,COMC,COMH,Cc
      REAL*8 KS,KR,FACTOR,RTP,KD,SMO,Z_SPLT
      COMMON /KINE/FACTOR,KD,SMO

      IF (WRTSCR) WRITE(*,*) ' CHARRT '

C******************** COMPARTMENT LOOP ***************************
      Cc = COALM(FIXED)/0.012       !zc

      DO 200 K=1,CMP(6)
       DO P=1,2
        DO J = 1,NSPECY
           RRCOMB(J,K,P) = 0.
        ENDDO
       ENDDO

C*            core
        DO I = 1, NCLASS
!zc
           KS = ACHAR * EXP(ECHAR / TEMP(K,CORE)) 
           KR = 4. * PI * PRESS(1) * FACTOR * KS / MWS(1)
           RTP = KD * KR * (DIAM(I) / 2.)**2 * XGAS(O2,K,CORE) /
     &                 (KD + KR * DIAM(I) / 2.) 
           RRTCO(I,K) = KD * KR * (DIAM(I) / 2.)**2 /
     &                 (KD + KR * DIAM(I) / 2.) /SMO
           RRTAN(I,K) = RRTCO(I,K)
           PHICO(I,K) = Z_SPLT(I,TEMP(K,CORE),XGAS(O2,K,CORE))
           PHIAN(I,K) = PHICO(I,K)
      RTWCO(I) = KH2OG*exp(-22645./TEMP(K,CORE))*Cc      !mol C/(kg s Pa)
      RTWAN(I) = RTWCO(I)
      RTCCO(I) = KCO2G*exp(-22645./TEMP(K,CORE))*Cc
      RTCAN(I) = RTCCO(I)
      RTHCO(I) = KH2G*exp(-7.087-8078./TEMP(K,CORE))*Cc
      RTHAN(I) = RTHCO(I)
!zc
         COMCOR =  RRTCO(I,K)*MCOR(I,K,COKE)*COKCO(I,K)*XGAS(O2,K,CORE)

         XH2OS = XGAS(H2,K,CORE)*XGAS(CO,K,CORE)*PRESS(1)**2./
     &            (1.01325E5*exp(17.29-16326/TEMP(K,CORE)))
         XCO2S = (XGAS(CO,K,CORE)*PRESS(1))**2/(1.01325E5
     &            *exp(20.92-20282./TEMP(K,CORE)))
         XH2S = sqrt(1.01325E5*XGAS(CH4,K,CORE)*PRESS(1)/
     &            exp(-13.43+10999/TEMP(K,CORE)))

      COMW = max(0.0,RTWCO(I)*(XGAS(H2O,K,CORE)*PRESS(1)-XH2OS)
     &   *MCOR(I,K,COKE)*COKCO(I,K))
      COMC = max(0.0,RTCCO(I)*(XGAS(CO2,K,CORE)*PRESS(1)-XCO2S)
     &   *MCOR(I,K,COKE)*COKCO(I,K))
      COMH = max(0.0,RTHCO(I)*(XGAS(H2,K,CORE)*PRESS(1)-XH2S)
     &   *MCOR(I,K,COKE)*COKCO(I,K))

         RRCOMB(CO,K,CORE) = RRCOMB(CO,K,CORE)+COMCOR*(PHICO(I,K)-1.)
     &   +COMW+2.*COMC

         RRCOMB(CO2,K,CORE) = RRCOMB(CO2,K,CORE)+COMCOR*(2.-PHICO(I,K)) 
     &   -COMC

         RRCOMB(NO,K,CORE)  = RRCOMB(NO,K,CORE) + COMCOR  
     &    * SPLTN * COALM(4)/COALM(FIXED)*(1.-XNVOL)*MWS(1)/MWS(4)

         RRCOMB(N2O,K,CORE)  = RRCOMB(N2O,K,CORE) + COMCOR * 0.5  
     &    *(1.-SPLTN)* COALM(4)/COALM(FIXED)*(1.-XNVOL)*MWS(1)/MWS(4)

         RRCOMB(O2 ,K,CORE) = RRCOMB(O2 ,K,CORE) - COMCOR *
     &   (    1.5-PHICO(I,K)/2.
     &         + 0.25*(1.+SPLTN)*COALM(4)/COALM(FIXED)
     &         * (1.-XNVOL)*MWS(1)/MWS(4)               )

         RRCOMB(H2O,K,CORE) = RRCOMB(H2O,K,CORE)-COMW
         RRCOMB(H2,K,CORE) = RRCOMB(H2,K,CORE)+COMW-2*COMH
         RRCOMB(CH4,K,CORE) = RRCOMB(CH4,K,CORE)+COMH
        ENDDO
C*          annulus
        DO I=1,NCLASS
!zc
           KS = ACHAR * EXP(ECHAR / TEMP(K,ANNU)) 
           KR = 4. * PI * PRESS(1) * FACTOR * KS / MWS(1)
           RTP = KD * KR * (DIAM(I) / 2.)**2 * XGAS(O2,K,ANNU) /
     &                 (KD + KR * DIAM(I) / 2.) 
           RRTCO(I,K) = KD * KR * (DIAM(I) / 2.)**2 /
     &                 (KD + KR * DIAM(I) / 2.) /SMO
           RRTAN(I,K) = RRTCO(I,K)
           PHICO(I,K) = Z_SPLT(I,TEMP(K,ANNU),XGAS(O2,K,ANNU))
           PHIAN(I,K) = PHICO(I,K)
      RTWCO(I) = KH2OG*exp(-22645./TEMP(K,ANNU))*Cc      !mol C/(kg s Pa)
      RTWAN(I) = RTWCO(I)
      RTCCO(I) = KCO2G*exp(-22645./TEMP(K,ANNU))*Cc
      RTCAN(I) = RTCCO(I)
      RTHCO(I) = KH2G*exp(-7.087-8078./TEMP(K,ANNU))*Cc
      RTHAN(I) = RTHCO(I)
!zc
         COMANN =  RRTAN(I,K)*MANN(I,K,COKE)*COKAN(I,K)*XGAS(O2,K,ANNU)

         XH2OS = XGAS(H2,K,ANNU)*XGAS(CO,K,ANNU)*PRESS(1)**2./
     &            (1.01325E5*exp(17.29-16326/TEMP(K,ANNU)))
         XCO2S = (XGAS(CO,K,ANNU)*PRESS(1))**2/(1.01325E5
     &            *exp(20.92-20282./TEMP(K,ANNU)))
         XH2S = sqrt(1.01325E5*XGAS(CH4,K,ANNU)*PRESS(1)/
     &            exp(-13.43+10999/TEMP(K,ANNU)))

      COMW = max(0.0,RTWCO(I)*(XGAS(H2O,K,ANNU)*PRESS(1)-XH2OS)
     &   *MCOR(I,K,COKE)*COKCO(I,K))
      COMC = max(0.0,RTCCO(I)*(XGAS(CO2,K,ANNU)*PRESS(1)-XCO2S)
     &   *MCOR(I,K,COKE)*COKCO(I,K))
      COMH = max(0.0,RTHCO(I)*(XGAS(H2,K,ANNU)*PRESS(1)-XH2S)
     &   *MCOR(I,K,COKE)*COKCO(I,K))

         RRCOMB(CO,K,ANNU)  = RRCOMB(CO,K,ANNU)  +COMANN*(PHIAN(I,K)-1.)
     &   +COMW+2.*COMC

         RRCOMB(CO2,K,ANNU) = RRCOMB(CO2,K,ANNU) +COMANN*(2.-PHIAN(I,K))
     &   -COMC

         RRCOMB(NO,K,ANNU)  = RRCOMB(NO,K,ANNU) + COMANN  
     &    * SPLTN * COALM(4)/COALM(FIXED)*(1.-XNVOL)*MWS(1)/MWS(4)
         RRCOMB(N2O,K,ANNU)  = RRCOMB(N2O,K,ANNU) + COMANN * 0.5
     &    *(1.-SPLTN)* COALM(4)/COALM(FIXED)*(1.-XNVOL)*MWS(1)/MWS(4)

         RRCOMB(O2,K,ANNU) = RRCOMB(O2,K,ANNU) - COMANN *
     &   (    1.5-PHIAN(I,K)/2.
     &         + 0.25*(1.+SPLTN)*COALM(4)/COALM(FIXED)
     &         * (1.-XNVOL)*MWS(1)/MWS(4)               )

         RRCOMB(H2O,K,ANNU) = RRCOMB(H2O,K,ANNU)-COMW
         RRCOMB(H2,K,ANNU) = RRCOMB(H2,K,ANNU)+COMW-2*COMH
         RRCOMB(CH4,K,ANNU) = RRCOMB(CH4,K,ANNU)+COMH
        ENDDO 

 200  CONTINUE

C******************** END COMPARTMENT LOOP *******************

C*       estimate total char combustion rate
          DDM = 0.
       DO I=1,NCLASS
           RTSUM  = 0.
           DVSUM  = 0.

         DO K=1, NCELLS

C         write(*,*) 'XCELL',K,XGAS(O2,K,CORE),XGAS(O2,K,ANNU), AREA(K)
C         write(*,*) 'EPSS ',K,EPSSCO(I,K,COKE),EPSSAN(I,K,COKE)
C         write(*,*)'RRT  ',K,RRTCO(I,K),RRTAN(I,K)
C         write(*,*)
!zc
           KS = ACHAR * EXP(ECHAR / TEMP(K,CORE)) 
           KR = 4. * PI * PRESS(1) * FACTOR * KS / MWS(1)
           RTP = KD * KR * (DIAM(I) / 2.)**2 * XGAS(O2,K,CORE) /
     &                 (KD + KR * DIAM(I) / 2.) 
           RRTCO(I,K) = KD * KR * (DIAM(I) / 2.)**2 /
     &                 (KD + KR * DIAM(I) / 2.) /SMO
           RRTAN(I,K) = RRTCO(I,K)
      RTWCO(I) = KH2OG*exp(-22645./TEMP(K,CORE))*Cc      !mol C/(kg s Pa)
      RTWAN(I) = RTWCO(I)
      RTCCO(I) = KCO2G*exp(-22645./TEMP(K,CORE))*Cc
      RTCAN(I) = RTCCO(I)
      RTHCO(I) = KH2G*exp(-7.087-8078./TEMP(K,CORE))*Cc
      RTHAN(I) = RTHCO(I)
!zc
         XH2OS = XGAS(H2,K,CORE)*XGAS(CO,K,CORE)*PRESS(1)**2./
     &            (1.01325E5*exp(17.29-16326/TEMP(K,CORE)))
         XCO2S = (XGAS(CO,K,CORE)*PRESS(1))**2/(1.01325E5
     &            *exp(20.92-20282./TEMP(K,CORE)))
         XH2S = sqrt(1.01325E5*XGAS(CH4,K,CORE)*PRESS(1)/
     &            exp(-13.43+10999/TEMP(K,CORE)))

      COMW = max(0.0,RTWCO(I)*(XGAS(H2O,K,CORE)*PRESS(1)-XH2OS)
     &   *MCOR(I,K,COKE)*COKCO(I,K))
      COMC = max(0.0,RTCCO(I)*(XGAS(CO2,K,CORE)*PRESS(1)-XCO2S)
     &   *MCOR(I,K,COKE)*COKCO(I,K))
      COMH = max(0.0,RTHCO(I)*(XGAS(H2,K,CORE)*PRESS(1)-XH2S)
     &   *MCOR(I,K,COKE)*COKCO(I,K))

           RTSUM = RTSUM 
     &      + RRTCO(I,K) * XGAS(O2,K,CORE) * MCOR(I,K,4) * COKCO(I,K)
     &      + RRTAN(I,K) * XGAS(O2,K,ANNU) * MANN(I,K,4) * COKAN(I,K)
     &      + COMW + COMC + COMH
c
!zc
           KS = ACHAR * EXP(ECHAR / TEMP(K,ANNU)) 
           KR = 4. * PI * PRESS(1) * FACTOR * KS / MWS(1)
           RTP = KD * KR * (DIAM(I) / 2.)**2 * XGAS(O2,K,ANNU) /
     &                 (KD + KR * DIAM(I) / 2.) 
           RRTCO(I,K) = KD * KR * (DIAM(I) / 2.)**2 /
     &                 (KD + KR * DIAM(I) / 2.) /SMO
           RRTAN(I,K) = RRTCO(I,K)
      RTWCO(I) = KH2OG*exp(-22645./TEMP(K,ANNU))*Cc      !mol C/(kg s Pa)
      RTWAN(I) = RTWCO(I)
      RTCCO(I) = KCO2G*exp(-22645./TEMP(K,ANNU))*Cc
      RTCAN(I) = RTCCO(I)
      RTHCO(I) = KH2G*exp(-7.087-8078./TEMP(K,ANNU))*Cc
      RTHAN(I) = RTHCO(I)
!zc
         XH2OS = XGAS(H2,K,ANNU)*XGAS(CO,K,ANNU)*PRESS(1)**2./
     &            (1.01325E5*exp(17.29-16326/TEMP(K,ANNU)))
         XCO2S = (XGAS(CO,K,ANNU)*PRESS(1))**2/(1.01325E5
     &            *exp(20.92-20282./TEMP(K,ANNU)))
         XH2S = sqrt(1.01325E5*XGAS(CH4,K,ANNU)*PRESS(1)/
     &            exp(-13.43+10999/TEMP(K,ANNU)))

      COMW = max(0.0,RTWCO(I)*(XGAS(H2O,K,ANNU)*PRESS(1)-XH2OS)
     &   *MCOR(I,K,COKE)*COKCO(I,K))
      COMC = max(0.0,RTCCO(I)*(XGAS(CO2,K,ANNU)*PRESS(1)-XCO2S)
     &   *MCOR(I,K,COKE)*COKCO(I,K))
      COMH = max(0.0,RTHCO(I)*(XGAS(H2,K,ANNU)*PRESS(1)-XH2S)
     &   *MCOR(I,K,COKE)*COKCO(I,K))

           RTSUM = RTSUM + COMW + COMC + COMH

           DVSUM = DVSUM + MCOR(I,K,4) + MANN(I,K,4)

         ENDDO

         IF (DVSUM .GT. 0.) THEN
            RRTOT(I) = RTSUM / DVSUM     
         ELSE
C             write(*,*) ' WARNING  !!  RRTOT(',I,') = 0 artificially'
           RRTOT(I) = 0.
        ENDIF    

         DDM = DDM + DVSUM
       ENDDO  ! (I)

       IF (WRTSCR) write(*,*) ' END CHARRT '

          IF (.NOT. WRTOUT(18)) RETURN

C*********************** TESTWRITER *****************************

         OPEN(UNIT=33,FILE='CHARRT.DAT',STATUS='UNKNOWN')
             WRITE(33,*) 'COMB. of char  mol/s)'
            SUMSUM = 0.
         DO K = 1, NCELLS
            SUM = 0.
           DO I=1, NCLASS
!zc
           KS = ACHAR * EXP(ECHAR / TEMP(K,CORE)) 
           KR = 4. * PI * PRESS(1) * FACTOR * KS / MWS(1)
           RTP = KD * KR * (DIAM(I) / 2.)**2 * XGAS(O2,K,CORE) /
     &                 (KD + KR * DIAM(I) / 2.) 
           RRTCO(I,K) = KD * KR * (DIAM(I) / 2.)**2 /
     &                 (KD + KR * DIAM(I) / 2.) /SMO
           RRTAN(I,K) = RRTCO(I,K)
      RTWCO(I) = KH2OG*exp(-22645./TEMP(K,CORE))*Cc      !mol C/(kg s Pa)
      RTWAN(I) = RTWCO(I)
      RTCCO(I) = KCO2G*exp(-22645./TEMP(K,CORE))*Cc
      RTCAN(I) = RTCCO(I)
      RTHCO(I) = KH2G*exp(-7.087-8078./TEMP(K,CORE))*Cc
      RTHAN(I) = RTHCO(I)
!zc
         XH2OS = XGAS(H2,K,CORE)*XGAS(CO,K,CORE)*PRESS(1)**2./
     &            (1.01325E5*exp(17.29-16326/TEMP(K,CORE)))
         XCO2S = (XGAS(CO,K,CORE)*PRESS(1))**2/(1.01325E5
     &            *exp(20.92-20282./TEMP(K,CORE)))
         XH2S = sqrt(1.01325E5*XGAS(CH4,K,CORE)*PRESS(1)/
     &            exp(-13.43+10999/TEMP(K,CORE)))

      COMW = max(0.0,RTWCO(I)*(XGAS(H2O,K,CORE)*PRESS(1)-XH2OS)
     &   *MCOR(I,K,COKE)*COKCO(I,K))
      COMC = max(0.0,RTCCO(I)*(XGAS(CO2,K,CORE)*PRESS(1)-XCO2S)
     &   *MCOR(I,K,COKE)*COKCO(I,K))
      COMH = max(0.0,RTHCO(I)*(XGAS(H2,K,CORE)*PRESS(1)-XH2S)
     &   *MCOR(I,K,COKE)*COKCO(I,K))

            SUM = SUM 
     &       + RRTCO(I,K) * XGAS(O2,K,CORE) * MCOR(I,K,4)*COKCO(I,K)
     &       + RRTAN(I,K) * XGAS(O2,K,ANNU) * MANN(I,K,4)*COKAN(I,K)
     &       + COMW + COMC + COMH
c
!zc
           KS = ACHAR * EXP(ECHAR / TEMP(K,ANNU)) 
           KR = 4. * PI * PRESS(1) * FACTOR * KS / MWS(1)
           RTP = KD * KR * (DIAM(I) / 2.)**2 * XGAS(O2,K,ANNU) /
     &                 (KD + KR * DIAM(I) / 2.) 
           RRTCO(I,K) = KD * KR * (DIAM(I) / 2.)**2 /
     &                 (KD + KR * DIAM(I) / 2.) /SMO
           RRTAN(I,K) = RRTCO(I,K)
      RTWCO(I) = KH2OG*exp(-22645./TEMP(K,ANNU))*Cc      !mol C/(kg s Pa)
      RTWAN(I) = RTWCO(I)
      RTCCO(I) = KCO2G*exp(-22645./TEMP(K,ANNU))*Cc
      RTCAN(I) = RTCCO(I)
      RTHCO(I) = KH2G*exp(-7.087-8078./TEMP(K,ANNU))*Cc
      RTHAN(I) = RTHCO(I)
!zc
         XH2OS = XGAS(H2,K,ANNU)*XGAS(CO,K,ANNU)*PRESS(1)**2./
     &            (1.01325E5*exp(17.29-16326/TEMP(K,ANNU)))
         XCO2S = (XGAS(CO,K,ANNU)*PRESS(1))**2/(1.01325E5
     &            *exp(20.92-20282./TEMP(K,ANNU)))
         XH2S = sqrt(1.01325E5*XGAS(CH4,K,ANNU)*PRESS(1)/
     &            exp(-13.43+10999/TEMP(K,ANNU)))

      COMW = max(0.0,RTWCO(I)*(XGAS(H2O,K,ANNU)*PRESS(1)-XH2OS)
     &   *MCOR(I,K,COKE)*COKCO(I,K))
      COMC = max(0.0,RTCCO(I)*(XGAS(CO2,K,ANNU)*PRESS(1)-XCO2S)
     &   *MCOR(I,K,COKE)*COKCO(I,K))
      COMH = max(0.0,RTHCO(I)*(XGAS(H2,K,ANNU)*PRESS(1)-XH2S)
     &   *MCOR(I,K,COKE)*COKCO(I,K))

           SUM = SUM + COMW + COMC + COMH

           ENDDO
             SUMSUM = SUMSUM + SUM
            WRITE(33,*) K,HEIGHT(K),SUM,SUM/(HEIGHT(K)-HEIGHT(K-1))
         ENDDO

            GSUM = 0.
        DO K=1,NCELLS
            GSUM = GSUM
     &       +  RRCOMB(CO,K,CORE)   !* XGAS(O2,K,CORE) 
     &       +  RRCOMB(CO2,K,CORE)  !* XGAS(O2,K,CORE) 
     &       +  RRCOMB(CO,K,ANNU)   !* XGAS(O2,K,ANNU) 
     &       +  RRCOMB(CO2,K,ANNU)  !* XGAS(O2,K,ANNU) 
        ENDDO

            WRITE(33,*) 'molC solid ', SUMSUM
            WRITE(33,*) 'molC gas   ', GSUM

            CLOSE(33)

      RETURN
      END      ! (CHARRT)

