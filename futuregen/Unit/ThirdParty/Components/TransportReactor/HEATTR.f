
      SUBROUTINE HEATTR(ALFAT,ALFAW)
      
************************************************************************
* LAST CHANGE AT : 20.11.1996               BY:   J. Hannes            *
************************************************************************
* CALLED BY : ENERGY                                                   *
* CALLS     :                                                          *
************************************************************************
* COMMON in : ALFATB, ALFAWL, DIAM(I), HEIGHT(K), PART(I,M), SBOLTZ,   *
*             USOLA, WALL(K), NCELLS, TWALL                            *
* out       :                                                          *
* in & out  :                                                          *
************************************************************************
* passed    : -                                                        *
* returned  : ALFAT - heat transfer coeff to tubes (W/m2K)             *
*             ALFAW - heat transfer coeff to wall  (W/m2K)
************************************************************************
* PURPOSE   : deliveres the heat transfer coefficients                 *
*             for walls and tubes                                      *
************************************************************************

      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'

      REAL*8           ALFAT(0:NK) , ALFAW(0:NK),ALFACON(NK),ALFARAD(NK)
      REAL*8           Z_THC, Z_EPSS, Z_ESMF, Z_RHOG
      INTEGER          I,K,M   

C*         heat transfer to tubes and walls


      DO K=1, NCELLS
C           ALFAT(K)  = ALFATB
C           ALFAW(K)  = ALFAWL * WALL(K)
 
          ALFACON(K) = 0.
         DO M=2,4
          DO I=1,NCLASS
           IF (OPTION(4) .EQ. 0) THEN
              ALFACON(K) = ALFACON(K) + PART(I,M) *
     &         (  Z_THC(TEMP(K,ANNU))/DIAM(I)
     &           *2.85*(Z_EPSS(I,K,M)/Z_ESMF(MEAND(5),5))**0.5
     &      + 3.28E-3*USOLA(I,K,M)*1100.*Z_RHOG(TEMP(K,ANNU),PRESS(1)))
          ELSE
               ALFACON(K) = ALFACON(K) + PART(I,M) *
     &            Z_THC(TEMP(K,ANNU))/DIAM(I)
     &           *2.85*(Z_EPSS(I,K,M)/Z_ESMF(DIAM(I),M))**0.5
          ENDIF
          ENDDO
         ENDDO

C*           0.83 is the combination of emissivities
           ALFARAD(K) = SBOLTZ * 0.83 * (TEMP(K,ANNU)**4.-TWALL**4.)
     &                             /(TEMP(K,ANNU) - TWALL)

          ALFAT(K) = 0.0   !ALFATB
          ALFAW(K) = 0.0   !(ALFACON(K)+ALFARAD(K))*WALL(K)

      ENDDO

        OPEN(UNIT=44,FILE='HEATTR.DAT ',STATUS='UNKNOWN')
	WRITE(44,*)'ALFACON - Convective heat tran coeff to wall (W/m2K)'
	WRITE(44,*)'ALFARAD - Radiative heat tran coeff to wall  (W/m2K)'
	WRITE(44,*)
        WRITE(44,*) ' CELL  HEIGHT    ALFACON     ALFARAD   '
        DO K=1,NCELLS
        WRITE(44,10) K, HEIGHT(K), ALFACON(K),ALFARAD(K)
        ENDDO
10	FORMAT(I3,3F12.5)
        CLOSE(44)

      RETURN

      END        ! (HEATTR)

