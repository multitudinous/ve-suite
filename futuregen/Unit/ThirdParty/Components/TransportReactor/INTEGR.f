
      SUBROUTINE INTEGR

************************************************************************
* LAST CHANGE AT : 12.11.1995               BY:   J. Hannes            *
************************************************************************
* CALLED BY : CFBC                                                     *
* CALLS     : NWTSVD                                                   *
************************************************************************
* COMMON in : NSPECY,  XFEED(K), CMP(6), KCOMMON, YBUBB(K), YANNU(K)   *
* out       :                                                          *
* in & out  : XGAS(J,K,CORE)                                           *
************************************************************************
* passed    : -                                                        *
* returned  : -                                                        *
************************************************************************
* PURPOSE   : initiates the calculation of the gas species concen-     *
*             trations in the cells                                    *
************************************************************************

      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'

      REAL*8           XIN(50)
      INTEGER          J, K, NNN, N, NCO, NAN, NBU
      LOGICAL          DUMMY

       IF (WRTSCR) WRITE(*,*) ' INTEGR'

       IF (POSTPR) THEN
         N = NPOST
       ELSE
         N = NPRE
       ENDIF
       
         NCO = 0        ! reduced pointers for preprocessing
         NAN = N
         NBU = 2*N       

       IF ((LENERG .EQ. 1) .AND. (LMASS .EQ. 1) ) THEN 
C*             initialize air flow composition
             DO J=1, NSPECY
               XGAS(J,0,CORE) = XFEED(J)
               XGAS(J,0,ANNU) = XFEED(J)
               XGAS(J,0,BUBB) = XFEED(J)
             ENDDO
               XGAS(O2 ,CMP(6),CORE) = 0.05
               XGAS(N2 ,CMP(6),CORE) = 0.79
               XGAS(CO2,CMP(6),CORE) = 0.16
        ENDIF
C******************* COMPARTMENT LOOP ***************************

      DO 200 K=1,CMP(6)
           
C            write(*,*) ' GAS CALC IN CELL ',K
           
C*              make compartment counter global
          KCOMMON = K

C********************* CHAR COMBUSTION ******************************

              NNN = 3*N

C*     for very first loop initialize array and set iteration starting
C*     value to value of lower cell
C           IF ((LENERG .EQ. 1) .AND. (LMASS .EQ. 1) .OR. POSTPR) THEN 
           IF ((LENERG .EQ. 1) .AND. (LMASS .EQ. 1)) THEN 
                  DO J = 1, NSPECY
                    XGAS(J,K,CORE)=0.
                    XGAS(J,K,ANNU)=0.
                    XGAS(J,K,BUBB)=0.
                  ENDDO
               DO J = 1, N
                 XIN(J+NCO) = XGAS(J,K-1,CORE)
                 XIN(J+NAN) = XGAS(J,K-1,ANNU)
                 XIN(J+NBU) = XGAS(J,K-1,BUBB)
               ENDDO
           ELSE
               DO J = 1, N
                 XIN(J+NCO) = XGAS(J,K,CORE)
                 XIN(J+NAN) = XGAS(J,K,ANNU)
                 XIN(J+NBU) = XGAS(J,K,BUBB)
               ENDDO
           ENDIF

C*                   look if bubbles are present
             IF (YBUBB(K) .LT. 1.E-5 .AND. YBUBB(K-1) .LT. 1.E-5) THEN
                   NNN = 2*N
                 DO J=1,N
                   XIN(J+NBU) = 0.
                 ENDDO
             ENDIF

C*                  look if annulus exist
             IF (YANNU(K) .LT. 1.E-5 .AND. YANNU(K-1) .LT. 1.E-5) THEN
                   NNN = N
                 DO J=1,N
                   XIN(J+NAN) = 0.
                 ENDDO
             ENDIF


***********************************************************************

C*       Newton algorithm with internal Single Value Decomposition
        CALL NWTSVD(XIN,NNN,DUMMY)

***********************************************************************
                                      
!zc             write(*,*) 'K  XO2 TEMP ', K, XIN(O2), TEMP(K,CORE)

                 DO J=1,N
                   IF (XIN(J+NCO) .LT. 1.E-10) THEN
                     XGAS(J,K,CORE) = 0.
                   ELSE
                     XGAS(J,K,CORE) = XIN(J+NCO)
                   ENDIF
                   IF (XIN(J+NAN) .LT. 1.E-10) THEN
                     XGAS(J,K,ANNU) = 0.
                   ELSE
                     XGAS(J,K,ANNU) = XIN(J+NAN)
                   ENDIF
                   IF (XIN(J+NBU) .LT. 1.E-10) THEN
                     XGAS(J,K,BUBB) = 0.
                   ELSE
                     XGAS(J,K,BUBB) = XIN(J+NBU)
                   ENDIF
                 ENDDO

 200  CONTINUE

******************** END COMPARTMENT LOOP *******************

      RETURN
      END        ! (INTEGR)

