
      SUBROUTINE REXIT(M) 

************************************************************************
* LAST CHANGE AT : 22.10.1993               BY:   J. Hannes            *
************************************************************************
* CALLED BY : POPUL, PREPOP                                            *
* CALLS     :                                                          *
************************************************************************
* COMMON in : NCLASS, WRTSCR                                           *
* out       : EFFTOP(I,M)                                              *
* in & out  :                                                          *
************************************************************************
* passed    : -                                                        *
* returned  : -                                                        *
************************************************************************
* PURPOSE   : calculates the separation efficiency of the riser exit   *
************************************************************************

       INCLUDE 'PARAMTR.FTN'
       INCLUDE 'COMMONBL.FTN'

       INTEGER I, ! size indicator
     &         M  ! material indicator


       IF (WRTSCR) write (*,*) 'REXIT '

C*            separation efficiency of riser exit  (-)
        DO I=1,NCLASS,1
           EFFTOP(I,M) = 0.3
        ENDDO

      RETURN
      END        ! (REXIT)


