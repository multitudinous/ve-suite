
       SUBROUTINE DENSBD(M)

************************************************************************
* WRITTEN        :                          BY:   J. Hannes            *
* LAST CHANGE AT : 22.09.94                 BY:   J. Hannes            *
************************************************************************
* CALLED BY : PREPAR                                                   *
* CALLS     :                                                          *
************************************************************************
* COMMON in : AEXPO(M),EPSHM(M),EPSELU(I,M),HEIGHT(K),NCLASS,WRTSCR    *
* out       : EPSSD(I,M)                                               *
* in & out  :                                                          *
************************************************************************
* passed    : M = material indicator                                   *
* returned  : -                                                        *
************************************************************************
* PURPOSE   : calculates solids volume fractions in dense bed          *
************************************************************************

      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'

      INTEGER  I,               ! particle size indicator
     &         M                ! particle material indicator
      REAL*8   A, H, HB, FACTOR ! auxiliary variables

      IF (WRTSCR) write(*,*) ' BEDHGT   ',M

      DO I=1,NCLASS
             IF (EPSSHM(M) .LT. EPSELU(I,M)) THEN
        WRITE(*,*)'ELU HOM PROBLEM bei ',I,M
             ENDIF

           A  = AEXPO(M)
           H  = HEIGHT(NCELLS) - HEIGHT(0)
           HB = HEIGHT(NBED)   - HEIGHT(0)

         FACTOR = HB  +  ( 1.-EXP(-A*(H-HB) ) ) / A

         EPSSD(I,M) = EPSELU(I,M) + H*(EPSSHM(M)-EPSELU(I,M))/FACTOR

       ENDDO

      RETURN
      END        ! (DENSBD)
