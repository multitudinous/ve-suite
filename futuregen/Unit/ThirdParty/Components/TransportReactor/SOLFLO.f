
      SUBROUTINE SOLFLO(M)

************************************************************************
* LAST CHANGE AT : 19.09.1994               BY:   J. Hannes            *
************************************************************************
* CALLED BY : CFBC                                                     *
* CALLS     : SOLIDS                                                   *
************************************************************************
* COMMON in : CMP(6), NCELLS, NCLASS, NBED, RHOS(M), PART(I,M),        *
*             HEIGHT(K), USOLID(I,K,M), MCOR(I,K,M), MNETTO(I,K,M)     *
*             MCYC, MEHE, WRTOUT, WRTSCR                               *
* out       : MUP(I,K,M), MDWN(I,K,M), MX(I,K,M), MCA(I,K,M),          *
*             MAC(I,K,M), MCAX(I,K,M), MINL(I,K,M), MOUT(I,K,M),       *
* in & out  :                                                          *
************************************************************************
* passed    : M material indicator                                     *
* returned  : -                                                        *
************************************************************************
* PURPOSE   : calculates the solids flow rates into and out of the     *
*             cells                                                    *
************************************************************************

      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'

      INTEGER  I, K, M  ! counters for I=particle size, K=cell, M=material

      IF (WRTSCR) write(*,*) '  SOLFLO '

C*           flows into and out of the bed at actual cells          
      DO K = 0, NCELLS + 1
        DO I = 1, NCLASS                                           
             MX(I,K,M)     = 0.
             MCA(I,K,M)    = 0.
             MAC(I,K,M)    = 0.
             MCAX(I,K,M)   = 0.
         ENDDO    
      ENDDO

C*           individual mass flows up and down
C*           total mass flows up and down

C***************************** upflow core *****************************
      DO K=1,NCELLS
         DO I = 1, NCLASS                                                

          IF (K .EQ. NCELLS) THEN
                MUP(I,K,M) = 0.
          ELSE IF (K .LT. CMP(6)) THEN 
            MUP(I,K,M) = USOLID(I,K,M)/(HEIGHT(K)-HEIGHT(K-1))
     &         *MCOR(I,K,M)
          ELSE IF (K .GE. CMP(6)) THEN
            MUP(I,K,M) = USOLID(I,K,M)/(HEIGHT(K)-HEIGHT(K-1))
     &         *MCOR(I,K,M)  - MOUT(I,CMP(6),M)
            ENDIF
          IF (MUP(I,K,M) .LT. MNETTO(I,K,M)) THEN
            MUP(I,K,M) = ABS(MNETTO(I,K,M)) * 1.01
          ENDIF
         ENDDO
       ENDDO

C*************************** downflow annulus ***************************
      DO K=1,NCELLS
         DO I = 1, NCLASS                                                
            IF (K .EQ. 1.) THEN               !  ( or K LE NBED)
             MDWN(I,K,M) = 0. 
            ELSE
             MDWN(I,K,M)  = MUP(I,K-1,M) - MNETTO(I,K-1,M) 
            ENDIF
          ENDDO
       ENDDO

C**************************** crossflows ********************************
      DO K=1,NCELLS
         DO I = 1, NCLASS                                                
           IF (K .LE. NBED .AND. K .GT. 1) THEN
             MX(I,K,M)  = MDWN(I,K,M)
           ENDIF

            IF (MDWN(I,K,M)-MDWN(I,K+1,M) .GE. 0.) THEN
                MCA(I,K,M) = MDWN(I,K,M) - MDWN(I,K+1,M) 
                MAC(I,K,M) = 0.
            ELSE
                MCA(I,K,M) = 0.
                MAC(I,K,M) = MDWN(I,K+1,M) - MDWN(I,K,M)
            ENDIF

         IF (K .LE. NBED) THEN
               MCAX(I,K,M) = MDWN(I,K,M) 
     &               *(HEIGHT(K)-HEIGHT(K-1))
         ELSE
               MCAX(I,K,M) = MDWN(I,K,M) * 0.1
         ENDIF   
          ENDDO
      ENDDO

      IF (WRTSCR)  write(*,*) 'END SOLFLO'
           
************************ TESTWRITER **************************************

       IF (.NOT. WRTOUT(15)) RETURN

      OPEN (UNIT=55,FILE='MTEST'//CHAR(M+48)//'.DAT',STATUS='UNKNOWN')

      write(55,*) ' Test sums for core and annu flow '

         write(55,*)
         write(55,*) ' *********** Material ',M,'******'
      DO I=1, NCLASS
         write(55,*)
         write(55,*) ' Size class ',I
         write(55,*) ' K  Netto        UP          DWN        CA      ',
     &               '   INOUT'
       DO K=1,NCELLS
         write(55,1011) K,MNETTO(I,K,M),MUP(I,K,M),MDWN(I,K,M),
     &             MCA(I,K,M)-MAC(I,K,M),
     &             MINL(I,K,M)+MCYC(I,K,M)+MEHE(I,K,M)-MOUT(I,K,M)
       ENDDO
      ENDDO
 1011 FORMAT(I3,5(2X,F11.5))
      CLOSE(55)


      RETURN
      END       ! (SOLFLO)



