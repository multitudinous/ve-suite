
      SUBROUTINE WRITE4

************************************************************************
* LAST CHANGE AT : 17.06.1994               BY:   J. Hannes            *
************************************************************************
* CALLED BY : CFBC                                                     *
* CALLS     :                                                          *
************************************************************************
* COMMON in : HEIGHT(K), MUP, MDWN, MCOR, MANN, NCELLS, NCLASS, WRTSCR,*
*             XGAS(J,K,P), YCORE, YANNU, YBUBB, YTOT                   *
* out       :                                                          *
* in & out  :                                                          *
************************************************************************
* passed    : -                                                        *
* returned  : -                                                        *
************************************************************************
* PURPOSE   : writes out gas concentrations                            *
*             calculate the mole flows in the gas phases               *
************************************************************************

      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'

      REAL*8    SUMC, SUMA, SUMB, UP, DWN ! auxiliaries
      REAL*8    TTCOK, TMCOK(NK), TMALL(NK)            ! auxiliaries
	REAL*8    SUM, SUMALL, VVSUM
      REAL*8    PPP, VVV, ZZ, SUMHT

      INTEGER      I, J, K , M, NN


      IF (WRTSCR) WRITE(*,*) ' WRITE4'

      OPEN(UNIT=12,FILE='GASFLOW.DAT',STATUS='UNKNOWN')
      OPEN(UNIT=13,FILE='GASCN1.DAT',STATUS='UNKNOWN')
      OPEN(UNIT=14,FILE='GASCN2.DAT',STATUS='UNKNOWN')
	
      WRITE(12,*) '    Molar gas flows in phases according to hight'
      WRITE(12,*) '    HEIGHT    CORE     ANNULUS    BUBBLE   ',
     &               'GASFLOW  '
	WRITE(12,*)

	WRITE(13,*)'Concentrations are on a wet basis'
	WRITE(13,200)
	WRITE(13,201)
	WRITE(13,*)
200	FORMAT('HEIGHT(m)   O2(%)     O2(%)     CO(%)    CO(%)      
     &CO2(%)    CO2(%)    H2O(%)    H2O(%)    H2(%)    H2(%)')
201	FORMAT('            core     annulus    core     annulus    core      
     &  annulus   core      annulus   core     annulus')
202	FORMAT('HEIGHT(m)   NOx(ppm) NOx(ppm) N2O(ppm) N2O(ppm)  NH3(ppm)  
     &  NH3(ppm)  SO2(ppm)  SO2(ppm)  H2S(ppm) H2S(ppm)')
	WRITE(14,*)'Concentrations are on a wet basis'
	WRITE(14,202)
	WRITE(14,201)
	WRITE(14,*)
                               
      DO 100 K=1,NCELLS

            SUMC = 0.
            SUMA = 0.
            SUMB = 0.
         DO J=1,NSPECY
            SUMC = SUMC + XGAS(J,K,CORE)
            SUMA = SUMA + XGAS(J,K,ANNU)
            SUMB = SUMB + XGAS(J,K,BUBB)             
         ENDDO
            IF (SUMC .LE. 0.) SUMC = 1.
            IF (SUMA .LE. 0.) SUMA = 1.
            IF (SUMB .LE. 0.) SUMB = 1.

         WRITE(13,1111)HEIGHT(K),             
     &                 XGAS(O2 ,K,CORE)/SUMC  *100.,
     &                 XGAS(O2 ,K,ANNU)/SUMA  *100.,
     &                 XGAS(CO ,K,CORE)/SUMC  *100.,
     &                 XGAS(CO ,K,ANNU)/SUMA  *100.,
     &                 XGAS(CO2,K,CORE)/SUMC  *100.,
     &                 XGAS(CO2,K,ANNU)/SUMA  *100., 
     &                 XGAS(H2O,K,CORE)/SUMC  *100., 
     &                 XGAS(H2O,K,ANNU)/SUMA  *100.,
     &                 XGAS(H2 ,K,CORE)/SUMC  *100.,
     &                 XGAS(H2 ,K,ANNU)/SUMA  *100.
         WRITE(14,1111)HEIGHT(K),
     &                 (XGAS(NO ,K,CORE)+XGAS(NO2,K,CORE))/SUMC  *1.E6,
     &                 (XGAS(NO ,K,ANNU)+XGAS(NO2,K,ANNU))/SUMA  *1.E6,
     &                 XGAS(N2O,K,CORE)/SUMC  *1.E6,
     &                 XGAS(N2O,K,ANNU)/SUMA  *1.E6,
     &                 XGAS(NH3,K,CORE)/SUMC  *1.E6, 
     &                 XGAS(NH3,K,ANNU)/SUMA  *1.E6,
     &                 XGAS(SO2,K,CORE)/SUMC  *1.E6,
     &                 XGAS(SO2,K,ANNU)/SUMA  *1.E6,
     &                 XGAS(H2S,K,CORE)/SUMC  *1.E6,
     &                 XGAS(H2S,K,ANNU)/SUMA  *1.E6
          WRITE(12,1112)HEIGHT(K),YCORE(K),YANNU(K),YBUBB(K),YTOT(K)

 100  CONTINUE
 
      CLOSE(12)
      CLOSE(13)
      CLOSE(14)

 1111   FORMAT(F7.3,10F10.3)
 1112   FORMAT(F10.3,4(2X,F8.2))

!        OPEN(UNIT = 77,FILE='MFLOW.DAT',STATUS = 'UNKNOWN')
!           write(77,*) ' Solid flows through cells  '
!           write(77,*) '  CELL      HEIGHT         UP            dwn '
!      DO K = 1,NCELLS
!            UP  = 0.
!            DWN = 0.
!        DO M = 2,4  
!          DO I = 1, NCLASS 
!            UP  = UP  + MUP(I,K,M)
!            DWN = DWN + MDWN(I,K,M)
!          ENDDO
!        ENDDO
!          write(77,1113) K,UP,DWN
!      ENDDO
!        CLOSE(77)
! 1113  FORMAT(I4,F10.3,2(2X,F15.5))

      OPEN(UNIT=17,FILE='TIME.DAT',STATUS='UNKNOWN')
       WRITE(17,*)' Drying and devolatilization times '
       WRITE(17,*)'     No      Diameter     Drying           Devola',
     &'tilization' 
       WRITE(17,*)'               [µm]         [s]               [s]'
       DO I=1,NCLASS
        WRITE(17,1001) I, DIAM(I)*1.E6, TDRY(I),TDEVOL(I)
       ENDDO
      CLOSE(17)     
   
 1001 FORMAT(5X,I3,5X,F8.0,2(5X,F12.8))
C*    *************** PRESSURE ********************************

C*    write pressure and average voidage profiles
      OPEN(UNIT=57,FILE='PRESS.DAT',STATUS='UNKNOWN')
       WRITE(57,4000) 

             TTCOK = 0.
      DO K = 1, NCELLS
             TMCOK(K) = 0.
             TMALL(K) = 0.
        DO I = 1, NCLASS
         TMCOK(K) = TMCOK(K) 
     &       + COKCO(I,K)*MCOR(I,K,4)
     &       + COKAN(I,K)*MANN(I,K,4)
         TMALL(K) = TMALL(K) + MCOR(I,K,4) + MANN(I,K,4)
     &                       + MCOR(I,K,3) + MANN(I,K,3)
     &                       + MCOR(I,K,2) + MANN(I,K,2)  
        ENDDO
         TTCOK = TTCOK + TMCOK(K)/(HEIGHT(K)-HEIGHT(K-1))
      ENDDO
         IF (TTCOK .LE. 0.) THEN
               TTCOK = 1.
         ENDIF

       PPP = PRESS(4)
       VVSUM = 0.
       SUM = 0.
       SUMALL = 0.
      DO 460 K=1,NCELLS
            ZZ = HEIGHT(K)-HEIGHT(K-1)
            SUMHT =0.
            VVV = 0.
         DO 445 M=2,4  
          DO 440 I = 1,NCLASS
             VVV = VVV + (MCOR(I,K,M) + MANN(I,K,M))/RHOS(M)/AREA(K)/ZZ
             SUMHT = SUMHT+(MCOR(I,K,M)+MANN(I,K,M))*G/AREA(K)
 440       CONTINUE
 445     CONTINUE
      IF(VVV.gt.0.58) VVV = 0.58       !zc

        PPP   = PPP   - SUMHT
        VVSUM = VVSUM + VVV*ZZ
        SUM    = SUM + TMCOK(K)
        SUMALL = SUMALL + TMALL(K)
      WRITE(57,4001)K,HEIGHT(K),PPP/1000.0,VVV,TMCOK(K)/TMALL(K),
     &TEMP(K,CORE),TEMP(K,ANNU),U(K)
460   CONTINUE
4000	FORMAT('Cell  Height(m)  Pressure(kPa)  Solid fract     Char fract
     &       T core(K)    T annulus(K)    Ug (m/s)')
4001	FORMAT(I3,F10.3,F14.6,2E16.6,3(2X,F12.3))
	CLOSE(57)
      RETURN
      END            ! (WRITE4)




