
      SUBROUTINE CHARBL

************************************************************************
* LAST CHANGE AT :  22.09.94                BY:   J. Hannes            *
************************************************************************
* CALLED BY : CFBC                                                     *
* CALLS     : LINEQS                                                   *
************************************************************************
* COMMON in : RRTCO(I,K), RRTAN(I,K)                                   *
*             MUP(I,K,M), MDWN(I,K,M), MX(I,K,M), MCA(I,K,M)           *
*             MAC(I,K,M), MCAX(I,K,M), MINL(I,K,M), MOUT(I,K,M),       *
*             MCYC(I,K,M), MEHE(I,K,M), MCOR(I,K,M), MANN(I,K,M),      *
*             XGAS(J,K,P), MWS(M), WRTSCR, HEIGHT(K),                  *
*             COALM(), CMP(6), NCLASS, NCELLS, PART(I,M)               *
*       out : COKCO(I,K), COKAN(I,K)                                   *
*  in & out :                                                          *
************************************************************************
* passed    : -                                                        *
* returned  : -                                                        *
************************************************************************
* PURPOSE   : Performs the char balance and calculates the char        *
*             concentration in each cell of the riser                  *
************************************************************************


      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'
      INCLUDE 'KINETICS.FTN'

      INTEGER I, K, KK
      REAL*8 A(NK,NK),   ! matrix of lin. eq. system
     &     B(NK)       ! array of lin. eq. system

      REAL*8 CON(NI,NK) ! carbon concentration in coke 
      REAL*8 XH2OS,XCO2S,XH2S,Cc
      REAL*8 KS,KR,FACTOR,RTP,KD,SMO,Z_SPLT
      COMMON /KINE/FACTOR,KD,SMO

      IF (WRTSCR) WRITE(*,*) ' CHARBL '

       KCO = 0
       KAN = NCELLS
       Cc = COALM(FIXED)/0.012      !zc

       DO 1000 I=1, NCLASS

             DO K=1,2*NCELLS
                B(K) = 0.
             ENDDO

C*               skip iteration if no coal in size class
             IF (PART(I,4) .LE. 1.E-7) THEN
               GOTO 30
             ENDIF

                DO K=1,NCELLS
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
     & (1.01325E5*exp(17.29-16326/TEMP(K,CORE)))
      XCO2S = (XGAS(CO,K,CORE)*PRESS(1))**2/(1.01325E5
     & *exp(20.92-20282./TEMP(K,CORE)))
      XH2S = sqrt(1.01325E5*XGAS(CH4,K,CORE)*PRESS(1)/
     & exp(-13.43+10999/TEMP(K,CORE)))

                    CON(I,K+KCO) = RRTCO(I,K)*XGAS(O2,K,CORE)*MWS(1)
     &   +max(0.0,RTWCO(I)*(XGAS(H2O,K,CORE)*PRESS(1)-XH2OS))*MWS(1)
     &   +max(0.0,RTCCO(I)*(XGAS(CO2,K,CORE)*PRESS(1)-XCO2S))*MWS(1)
     &   +max(0.0,RTHCO(I)*(XGAS(H2,K,CORE)*PRESS(1)-XH2S))*MWS(1)

      XH2OS = XGAS(H2,K,ANNU)*XGAS(CO,K,ANNU)*PRESS(1)**2./
     & (1.01325E5*exp(17.29-16326/TEMP(K,ANNU)))
      XCO2S = (XGAS(CO,K,ANNU)*PRESS(1))**2/(1.01325E5
     & *exp(20.92-20282./TEMP(K,ANNU)))
      XH2S = sqrt(1.01325E5*XGAS(CH4,K,ANNU)*PRESS(1)/
     & exp(-13.43+10999/TEMP(K,ANNU)))

                    CON(I,K+KAN) = RRTAN(I,K)*XGAS(O2,K,ANNU)*MWS(1)
     &   +max(0.0,RTWAN(I)*(XGAS(H2O,K,ANNU)*PRESS(1)-XH2OS))*MWS(1)
     &   +max(0.0,RTCAN(I)*(XGAS(CO2,K,ANNU)*PRESS(1)-XCO2S))*MWS(1)
     &   +max(0.0,RTHAN(I)*(XGAS(H2,K,ANNU)*PRESS(1)-XH2S))*MWS(1)

                    B(K) = MINL(I,K,4)*COALM(FIXED)/COALM(ASH)
                ENDDO

C*              reset cells
       DO K=1,2*NCELLS
         DO KK=1,2*NCELLS
          A(K,KK) = 0.
         ENDDO
       ENDDO

       K = 1
          A(K+KCO,K+KCO)   =   CON(I,K+KCO)*MCOR(I,K,4) + MUP(I,K,4)
     &                                     + MX(I,K,4)   
     &                       + MCA(I,K,4)  + MCAX(I,K,4) 
     &                       + MOUT(I,K,4)
          A(K+KCO,K+KCO+1) = - MX(I,K,4)
          A(K+KCO,K+KAN)   = - MAC(I,K,4) - MCAX(I,K,4)
          A(K+KCO,CMP(6))  =   A(K,CMP(6)) - MCYC(I,K,4) - MEHE(I,K,4)

          A(K+KAN,K+KAN)   =   CON(I,K+KAN)*MANN(I,K,4)  
     &                       + MAC(I,K,4) + MCAX(I,K,4)    
          A(K+KAN,K+KAN+1) = - MDWN(I,K+1,4)
          A(K+KAN,K+KCO)   = - MCA(I,K,4) - MCAX(I,K,4)

       DO K=2,NCELLS-1
          A(K+KCO,K+KCO-1) = - MUP(I,K-1,4)   - MX(I,K-1,4)
          A(K+KCO,K+KCO)   =   CON(I,K+KCO)*MCOR(I,K,4) + MUP(I,K,4)
     &                       + MX(I,K-1,4) + MX(I,K,4) 
     &                       + MCA(I,K,4)  + MCAX(I,K,4) 
     &                       + MOUT(I,K,4)
          A(K+KCO,K+KCO+1) = - MX(I,K,4)
          A(K+KCO,K+KAN)   = - MAC(I,K,4) - MCAX(I,K,4)
          A(K+KCO,CMP(6))  =   A(K,CMP(6)) - MCYC(I,K,4) - MEHE(I,K,4)

          A(K+KAN,K+KAN)   =   CON(I,K+KAN)*MANN(I,K,4)
     &                       + MDWN(I,K,4) 
     &                       + MAC(I,K,4) + MCAX(I,K,4)    
          A(K+KAN,K+KAN+1) = - MDWN(I,K+1,4)
          A(K+KAN,K+KCO)   = - MCA(I,K,4) - MCAX(I,K,4)
       ENDDO

       K = NCELLS 
          A(K+KCO,K+KCO-1) = - MUP(I,K-1,4)       - MX(I,K-1,4)
          A(K+KCO,K+KCO)   =   CON(I,K+KCO)*MCOR(I,K,4)
     &                       + MX(I,K-1,4) + MCA(I,K,4)
     &                       + MCAX(I,K,4) + MOUT(I,K,4)
          A(K+KCO,K+KAN)   = - MAC(I,K,4)  - MCAX(I,K,4)
          A(K+KCO,CMP(6))  =   A(K,CMP(6)) - MCYC(I,K,4) - MEHE(I,K,4)

          A(K+KAN,K+KAN)   =   CON(I,K+KAN)*MANN(I,K,4) + MDWN(I,K,4) 
     &                       + MAC(I,K,4)  + MCAX(I,K,4)    
          A(K+KAN,K+KCO)   = - MCA(I,K,4) - MCAX(I,K,4)


        CALL LINEQS(A,2*NCELLS,NK,B)

 30     CONTINUE

C*                write results to concentration arrays
           DO K=1,NCELLS
             COKCO(I,K) = min(1.0,B(K+KCO))
             COKAN(I,K) = min(1.0,B(K+KAN))
           ENDDO

 1000   CONTINUE
             
        IF (WRTSCR)   write(*,*) ' END CHARBL'

       RETURN
      END       ! (CHARBL)



