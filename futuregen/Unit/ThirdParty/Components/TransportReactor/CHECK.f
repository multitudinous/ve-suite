
      SUBROUTINE CHECK

************************************************************************
* LAST CHANGE AT : 19.09.94                 BY:   J. Hannes            *
************************************************************************
* CALLED BY : CFBC                                                     *
* CALLS     :                                                          *
************************************************************************
* COMMON in : AEXPIN, ATTCON(M), COMB(K,1-4),             CMP(1-5),    *
*             COALM(1-9), CYCDAT(1-9), FRGFAC, HEATCP(M), NCYC,        *
*             PRESS(1-4), OPTION(1-9), RHOS(M), SPHR(M),               *
*             SIVFED(I,M), FEED(M), TBED, TIN, TAMB, TUB(K)            *
* out       :                                                          *
* in & out  :                                                          *
************************************************************************
* passed    :                                                          *
* returned  : -                                                        *
************************************************************************
* PURPOSE   : Checks set of input data                                 * 
************************************************************************

        INCLUDE 'PARAMTR.FTN'
        INCLUDE 'COMMONBL.FTN'

        CHARACTER          Ch
        INTEGER            I, M
        REAL*8             TW, COAL1, COAL2

C*                     check of total weight fractions, 
C*                     and transform classes from (%) to (-)
            DO 60  M = 1, 3
                  TW = 0.
               DO I = 1, NCLASS
                     TW = TW + SIVFED(I,M)
               ENDDO
              IF ((TW.GT.0.) .AND. (TW.LT.0.95) .OR. (TW.GT.1.05)) THEN
                WRITE (IOUT,11) TW, M
   11               FORMAT(' Sum of weight fractions is ',F4.2, 
     &                     ' for input sieve array: ',I1)
                READ(*,1001) CH
                STOP
              ENDIF

C*              round out fractions if sum is near but not equal unity
              IF (TW .NE. 1.)  THEN
                 DO I = 1, NCLASS
                   SIVFED(I,M) = SIVFED(I,M) / TW
                 ENDDO
              ENDIF
  60        CONTINUE

              IF (TBED .LT. 873. .OR. TBED .GT. 1273.) THEN
               write(*,*) ' BED TEMPERATURE OUT OF RANGE ! '
              ENDIF

              IF (TIN .LT. 273. .OR. TIN .GT. 573.) THEN
               write(*,*) ' FEED TEMPERATURE OUT OF RANGE ! '
              ENDIF

              IF (TAMB .LT. 253. .OR. TAMB .GT. 323.) THEN
               write(*,*) ' AMBIENT TEMPERATURE OUT OF RANGE ! '
              ENDIF

                COAL1 = COALM(ASH) + COALM(VOLAT) + COALM(FIXED) - 1.
              IF (ABS(COAL1) .GT. 1.E-4)  THEN
                 WRITE (IOUT,*) ' Sum of ash, volatiles and char',
     &                          ' not equal unity.'
                 READ(*,*) Ch
                 STOP
              ENDIF

                COAL2 = COALM(1)+COALM(2)+COALM(3)+COALM(4)+COALM(5)
     &                 +COALM(ASH) - 1.
              IF (ABS(COAL2) .GT. 1.E-4)  THEN
                 WRITE (IOUT,*)' Sum of the elementary coal analysis',
     &                         ' not equal unity.'
                 READ(*,1001) Ch
                 STOP
              ENDIF

 1001   FORMAT(A1)

      RETURN
      END      ! (CHECK)


