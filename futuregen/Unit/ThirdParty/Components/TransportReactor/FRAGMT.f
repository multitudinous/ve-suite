      SUBROUTINE FRAGMT(M)

************************************************************************
* LAST CHANGE AT : 22.10.93                 BY:   J. Hannes            *
************************************************************************
* CALLED BY : CFBC                                                     *
* CALLS     :                                                          *
************************************************************************
* COMMON in : DIAM(I), FRGFAC(M), MESH(I), NCLASS,                     *
* out       : SIVIN0(I,M)                                              *
* in & out  : SIVFED(I,M)                                              *
************************************************************************
* passed    : M  material indicator                                    *
* returned  : -                                                        *
************************************************************************
* PURPOSE   : calculation of coal feed size distribution after initial *
*             fragmentation due to thermal shock and devolatilisation  *
*             Bellgardt et. al. Proc. 9th Int.Conf.FBC Boston USA,     *
*             Vol.2 (1987) 713                                         *
************************************************************************

      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'

      REAL*8    DELTQ,   DN(NI),  GRADF, GRADP         ! auxiliaries
      REAL*8    QDI(NI), QDN(NI), SUM, SUMOLD, SUMNEW  !   "
      INTEGER   I,       M,       LWR,     UPR         !   "
                                
      IF (WRTSCR) WRITE(*,*) 'FRAGMT'

      DO 20  I = NCLASS, 2, -1
C*             new size class
         DN(I) = MESH(I) * FRGFAC(M) ** (1. / 3.)

C*             trap new size and interpolate for existing weight fraction
           LWR = I

 10      CONTINUE

           UPR = LWR - 1

         IF ((DN(I) .GE. MESH(LWR)) .AND. (DN(I) .LE. MESH(UPR))) THEN
            GRADP  = (MESH(UPR) - MESH(LWR))
            GRADF  = (SIVFED(UPR,M) - SIVFED(LWR,M))
            DELTQ  = (GRADF / GRADP) * (MESH(UPR) - DN(I))

            QDI(I) = SIVFED(UPR,M) - DELTQ

C*             calculate new weight fraction
            QDN(I) = QDI(I) * FRGFAC(M) ** (1.0 / 3.0)

C*             go round again if we didn't trap it this time (DN(I) 
C*             can be more than one size down)
         ELSE IF (LWR .GT. 2)  THEN
            LWR = LWR - 1
            GOTO 10
         ENDIF
   20 CONTINUE

C*             round new distributions to 1.0
           SUM = 0.
        DO I = 1, NCLASS
           SUM = SUM + QDN(I)
        ENDDO

        DO I = 1, NCLASS
           QDN(I) = QDN(I) / SUM
        ENDDO

      DO I = 1, NCLASS
         SIVIN0(I,M)  = SIVFED(I,M)
         SIVFED(I,M) = QDN(I)
      ENDDO



C ***************************** testwriter ****************************

        IF (.NOT. WRTOUT(7)) RETURN

      OPEN(UNIT=20,FILE='FRAG'//CHAR(M+48)//'.DAT',STATUS='UNKNOWN')

           WRITE(20,*)' FRAGMENTATION'
           WRITE(20,*)'  DIAM        OLD         NEW         SUMOLD',
     &                '      SUMNEW'

           SUMOLD = 1.
           SUMNEW = 1.
           SUM = 0.
        DO I = 1, NCLASS
           WRITE(20,1001) MESH(I)*1.E6, SIVIN0(I,M)*100.,
     &            SIVFED(I,M)*100., SUMOLD*100., SUMNEW*100.
           SUMOLD = SUMOLD - SIVIN0(I,M)
           SUMNEW = SUMNEW - SIVFED(I,M)
           SUM = SUM + SIVFED(I,M)
        ENDDO
           write(20,*)
           write(20,*) 'Sum = ',SUM*100.

      CLOSE(20)

 1001 FORMAT(F10.0,4(2X,F10.3))

 100  CONTINUE

 

      RETURN
      END       ! (FRAGMT)

