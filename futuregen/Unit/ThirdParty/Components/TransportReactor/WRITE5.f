*
      SUBROUTINE WRITE5

************************************************************************
* LAST CHANGE AT : 22.10.1993               BY:   J. Hannes            *
************************************************************************
* CALLED BY : CFBC                                                     *
* CALLS     :                                                          *
************************************************************************
* COMMON in : NCELLS, NSPECY,                                          *
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

      REAL*8    GIN(5), GOUT(15), SIN(5), SOUT(5), CAS, Z_UMF
      REAL*8    BAGC, BAG, OVERC, OVER, CONTC, CONT, FLUE, SUM, SUMD
      INTEGER   C, H, O, N, S       ! Solid species
      INTEGER   I, J, K             ! size, species and cell counter
      REAL*8    XGASAVG(NJ,NK)      ! average gas mole fraction

      IF (WRTSCR) WRITE(*,*) ' WRITE5'

      C = 1
      H = 2
      O = 3
      N = 4
      S = 5

      OPEN(UNIT=43,FILE='BALANCE.DAT',STATUS='UNKNOWN')


C******************************************************************
!zc
       DO K=1,CMP(6)
         SUM = 0.0
        DO J=1,NSPECY
        XGASAVG(J,K)=(XGAS(J,K,CORE)*YCORE(K)+XGAS(J,K,ANNU)*YANNU(K))/
     &   (YCORE(K)+YANNU(K))
         SUM = SUM + XGASAVG(J,K)
        ENDDO

        DO J=1,NSPECY
        XGASAVG(J,K) = XGASAVG(J,K)/SUM
        ENDDO
       ENDDO
!zc
         SUM = 0.
       DO J=1,NSPECY
         SUM = SUM + XGASAVG(J,CMP(6))
       ENDDO
         SUMD = SUM - XGASAVG(H2O,CMP(6))

       WRITE(43,*)'Species concentrations at riser exit'
       WRITE(43,*)'      on wet basis            on dry basis'
      DO J=1,5
       WRITE(43,1111) SPECY(J),XGASAVG(J,CMP(6))/SUM *100.,
     &                         XGASAVG(J,CMP(6))/SUMD*100.                 
      ENDDO
      DO J=6,NSPECY
       WRITE(43,1112) SPECY(J),XGASAVG(J,CMP(6))/SUM *1.E6,
     &                         XGASAVG(J,CMP(6))/SUMD*1.E6
      ENDDO
 1111       FORMAT(A7,2X,'[%]   ',F10.4,4X,F10.4)
 1112       FORMAT(A7,2X,'[ppm] ',F10.4,4X,F10.4)
       WRITE(43,*)
       WRITE(43,*)'For concentration profiles see files:'
       WRITE(43,*)'GASCN1.DAT,GASCN2.DAT...'
       WRITE(43,*)
       WRITE(43,*)

C******************************************************************

       WRITE(43,*)'Sauter-Mean diameters'
       WRITE(43,*)
       WRITE(43,1113)'Average     [µm] ',MEAND(BED)  * 1.E6
       WRITE(43,1113)'Limestone   [µm] ',MEAND(LIME) * 1.E6
       WRITE(43,1113)'Inert       [µm] ',MEAND(INERT)* 1.E6
       WRITE(43,1113)'Char        [µm] ',MEAND(COKE) * 1.E6
       WRITE(43,1113)'Min. Vel. (mm/s) ',Z_UMF(MEAND(BED),BED)*1000.0
       WRITE(43,*)
 1113  FORMAT(A17,F12.3)

       
          FLUE = 0.
       DO K= 1,NCELLS
        FLUE = FLUE + YFEEDF(K)  
        GIN(C)  = YFEEDA(K)*MWS(C)*
     &            *(    XFEED(CO )
     &                + XFEED(CO2)
     &                + XFEED(CH4))
          GIN(O)  = YFEEDA(K)*MWS(O)*
     &            *(        XFEED(CO)
     &                + 2.* XFEED(CO2)
     &                + 2.* XFEED(O2) + XFEED(H2O) + 2.* XFEED(SO2)
     &                +     XFEED(NO) + XFEED(N2O) + 2.* XFEED(NO2)) 
        ENDDO

          GOUT(C) = (YOUT(CMP(6))-FLUE) * MWS(C)
     &            *(    XGASAVG(CO ,CMP(6))
     &                + XGASAVG(CO2,CMP(6))
     &                + XGASAVG(CH4,CMP(6)))
          GOUT(O) = (YOUT(CMP(6))-FLUE) * MWS(O)
     &            *(        XGASAVG(CO ,CMP(6))
     &                + 2.* XGASAVG(CO2,CMP(6))
     &                + 2.* XGASAVG(CH4,CMP(6)))
          GOUT(S) = (YOUT(CMP(6))-FLUE) * MWS(S)
     &            *( XGASAVG(SO2,CMP(6)) + XGASAVG(H2S,CMP(6)))
          GOUT(NO) = (YOUT(CMP(6))-FLUE)*MWG(NO)*XGASAVG(NO,CMP(6))
          GOUT(N2O)= (YOUT(CMP(6))-FLUE)*MWG(N2O)*XGASAVG(N2O,CMP(6))
        
          GOUT(S) = (YOUT(CMP(6))-FLUE) * MWS(S)
     &            *(XGASAVG(SO2,CMP(6)) + XGASAVG(H2S,CMP(6)))


          SIN(C)  = FEED(COAL)*(1.-COALM(MOIS))*COALM(COALC)
          SIN(O)  = FEED(COAL)*(1.-COALM(MOIS))*COALM(COALO)
          SIN(N)  = FEED(COAL)*(1.-COALM(MOIS))*COALM(COALN)
          SIN(S)  = FEED(COAL)*(1.-COALM(MOIS))*COALM(COALS)

         if(GOUT(S).GT.SIN(S)) GOUT(S) = SIN(S)       !zc

           SOUT(C) = 0.
           BAGC  = 0.
           OVERC = 0.
           CONTC = 0.
           BAG   = 0.
           OVER  = 0.
           CONT  = 0.
            
         DO I = 1,NCLASS

           BAGC = BAGC + (MOUT(I,CMP(6),4)-MCYC(I,CMP(4),4)
     &                        -MEHE(I,CMP(5),4)) *COKCO(I,CMP(6))
           BAG = BAG 
     &        +(MOUT(I,CMP(6),2)-MCYC(I,CMP(4),2)-MEHE(I,CMP(5),2))
     &        +(MOUT(I,CMP(6),3)-MCYC(I,CMP(4),3)-MEHE(I,CMP(5),3))
     &        +(MOUT(I,CMP(6),4)-MCYC(I,CMP(4),4)-MEHE(I,CMP(5),4))  

           OVERC = OVERC + MOUT(I,1,4)*COKCO(I,1)
           OVER  = OVER  + MOUT(I,1,2) + MOUT(I,1,3) + MOUT(I,1,4)

           CONTC = CONTC + (MCYC(I,CMP(4),4)
     &                      +MEHE(I,CMP(5),4)) *COKCO(I,CMP(6))
           CONT = CONT 
     &        +(MCYC(I,CMP(4),2)+MEHE(I,CMP(5),2))
     &        +(MCYC(I,CMP(4),3)+MEHE(I,CMP(5),3))
     &        +(MCYC(I,CMP(4),4)+MEHE(I,CMP(5),4))  
         ENDDO

            SOUT(C) = BAGC + OVERC

       CAS = FEED(LIME)*XCACO3/0.1/SIN(S)*0.032

       WRITE(43,*)'Solid flows'
       WRITE(43,1114)'Total feed             [kg/s] ',
     &                      FEED(COAL)+FEED(LIME)+FEED(INERT)
       WRITE(43,*) 
       WRITE(43,1114)'Feed coal              [kg/s] ', FEED(COAL)
       WRITE(43,1114)'Feed limestone         [kg/s] ', FEED(LIME)
       WRITE(43,1114)'Feed inert             [kg/s] ', FEED(INERT)
       WRITE(43,1114)'Filter ash             [kg/s] ', BAG
       WRITE(43,1114)'Discharge ash          [kg/s] ', OVER
       WRITE(43,1114)'Recirculation          [kg/s] ', CONT
       WRITE(43,*)   'Recirculation ratio'
       WRITE(43,1114)'Recirculated/feed      [-] ', CONT/(BAG+OVER)  
       WRITE(43,*)
       WRITE(43,1114)'Calorific value coal   [MJ/kg] ',CALVAL*1.E-6
       WRITE(43,1114)'Molar Ca/S ratio       [-]     ',CAS
       
       WRITE(43,*)
       WRITE(43,*) 'Carbon fraction in solids'
       WRITE(43,1114)'C in filter ash         [%] ',BAGC/BAG*100.
       WRITE(43,1114)'C in discharge ash      [%] ',OVERC/OVER*100.
       WRITE(43,1114)'C in return loop ash    [%] ',CONTC/CONT*100.
       WRITE(43,*)
       WRITE(43,*)
       WRITE(43,1114)'Cyclone efficiency                [%] ',
     &                                 CONT/(BAG+CONT)*100.
       WRITE(43,1114)'Carbon combustion efficiency      [%] ',
     &                                 (SIN(C)-SOUT(C))/SIN(C)*100.
       WRITE(43,1114)'Sulphur capture efficiency        [%] ',
     &                                 (SIN(S)-GOUT(S))/SIN(S)*100.
       WRITE(43,1114)'NO in exit  /coal-N               [%] ',
     &                                  GOUT(NO)/SIN(N)*100.
       WRITE(43,1114)'N2O in exit /coal-N               [%] ',
     &                                        GOUT(N2O)/SIN(N)*100.
 1114  FORMAT(A30,2F10.2)
       WRITE(43,*)
       WRITE(43,*)

C*********************************************************************1

       WRITE(43,*) 'Temperatures and heat transfer'
       WRITE(43,*)
       WRITE(43,1115)'Temperature in bed  [K] ',TEMP(1,CORE) 
       WRITE(43,1115)'Temperature at exit [K] ',TEMP(CMP(6),CORE)

       write(*,*)
       write(*,*)'CMP(6),NCELLS: ',CMP(6),NCELLS      !zc
       write(*,*)

       WRITE(43,*)
       WRITE(43,1115)'Coal energy in         [kW] ',
     &                 FEED(COAL)*CALVAL*0.001
       WRITE(43,*)
       WRITE(43,1115)'Heat to water walls    [kW] ',HTWALL*0.001
       WRITE(43,1115)'Heat to freebd tubes   [kW] ',HTTUBE*0.001
       WRITE(43,1115)'Heat to ext. cooler    [kW] ',HTRECY*0.001
       WRITE(43,1115)'Heat to secnd duct     [kW] ',HTGAS*0.001
       WRITE(43,*)'---------------------------------------'
       WRITE(43,1115)'Thermal output         [kW] ',(HTWALL+HTTUBE
     &                                             +HTRECY+HTGAS)*0.001
 1115     FORMAT(A30,F15.2) 
      WRITE(43,*)
      WRITE(43,*)'For heat transfer coefficients activate option:'
      WRITE(43,*)'"Write out submodule results" and see file ALFA.DAT"'
      WRITE(43,*)
!      WRITE(43,*)
!      WRITE(43,*) ' Temperature profile '
!      WRITE(43,*) 'Height(m)    Temp core(C)      Temp annulus(C)'       
!       DO K = 1,NCELLS 
!      WRITE(43,1001) HEIGHT(K),TEMP(K,CORE)-273.15,TEMP(K,ANNU)-273.15
!       ENDDO
! 1001  FORMAT(F10.5,2(4X,F15.3))                    


      WRITE(43,*)
      WRITE(43,*) ' Carbon balance  (kg/s)'
      WRITE(43,*) ' GAS   IN            OUT '
      WRITE(43,1116) GIN(C), GOUT(C) 
      WRITE(43,*)
      WRITE(43,*) ' SOLID IN            OUT '
      WRITE(43,1116) SIN(C), SOUT(C) 
      WRITE(43,*)
      WRITE(43,*) ' Combustion efficiency '
      WRITE(43,1116) (SIN(C)-SOUT(C))/SIN(C)

      WRITE(43,*)
      WRITE(43,*) '  rel. ERR'
      WRITE(43,1116)ABS((GIN(C)+SIN(C)-GOUT(C)-SOUT(C))/(GIN(C)+SIN(C)))
1116	FORMAT(2F12.6)
      CLOSE(43)
      RETURN
      END

