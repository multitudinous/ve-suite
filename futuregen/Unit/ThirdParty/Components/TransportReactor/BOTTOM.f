
      SUBROUTINE BOTTOM (M,SEGREG,SIFTER)

************************************************************************
* LAST CHANGE AT :  8.02.1994               BY:   J. Hannes            *
************************************************************************
* CALLED BY : POPUL, PREPOP                                            *
* CALLS     : Z_BUBB, Z_ESMF                                           * 
************************************************************************
* COMMON in : DIAM(I), NCLASS, OPTION(3), WRTOUT(), WRTSCR             *
* out       :                                                          *
* in & out  :                                                          *
************************************************************************
* passed    : M           material indicator                           *
* returned  : SEGREG(I,M) bottom bed segregation correction            *
*             SIFTER(I,M) wind sifter separation correlation           *
************************************************************************
* PURPOSE   : Calculation of the bottom bed effects of each class, i.e.*
*             segregation of particles and classification due to a     *
*             sifter.                                                  *
*             Only the relation between the classes is needed, the     *
*             normalization happens in the calling routine             *
************************************************************************

      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'

      REAL*8  SEGREG(NI,NM), ! bottom bed segregation
     &        SIFTER(NI,NM), ! wind sifter characteristic
     &        Z_BUBB,        ! funct.: bubble volume fraction
     &        Z_ESMF         ! funct.: solids volume fraction at min. fl.
      INTEGER I,       ! size class indicator
     &        M,       ! material indicator
     &        NSIFTER  ! class of split

      IF (WRTSCR) WRITE (*,*) 'BOTTOM '

       NSIFTER = 18
         
          DO I=1,NCLASS
               SEGREG(I,M) = 1.
               SIFTER(I,M)  = 1.
          ENDDO     
                  
C*            bottom ash correction due to segregation

        IF (OPTION(3) .EQ. 1 .OR. OPTION(3) .EQ. 3) THEN
             DO I = 1,NCLASS
                 SEGREG(I,M) = (1.-Z_BUBB(DIAM(I),M))*Z_ESMF(DIAM(I),M)
             ENDDO
        ENDIF
        
C*                        external ash separator with split at class 18
        IF (OPTION(3) .EQ. 2 .OR. OPTION(3) .EQ. 3) THEN
              DO I = 1, NCLASS
                 IF (I .GE. NSIFTER) THEN
                    SIFTER(I,M) = 0.
                 ELSE
                    SIFTER(I,M) = 1.
                 ENDIF
              ENDDO
        ENDIF

       IF (.NOT. WRTOUT(2)) RETURN
         
C******************** TESTWRITER *********************************


      OPEN(UNIT=77,FILE='BOTTOM'//CHAR(M+48)//'.DAT',STATUS='UNKNOWN')
      WRITE(77,*)'NO.   DIAMETER   SEGREGATION     SIFTER   epsmf epsd' 
       DO I = 1, NCLASS
        WRITE(77,1001)I,DIAM(I)*1.E6,SEGREG(I,M),SIFTER(I,M),
     &  Z_ESMF(DIAM(I),M), EPSSD(I,M)
       ENDDO

      CLOSE(77)

 1001 FORMAT(I2,2X,F7.0,4(2X,F10.8))

      RETURN
      END     ! (BOTTOM)




