
      SUBROUTINE CORFAC(M)

************************************************************************
* LAST CHANGE AT : 04.10.93                 BY:   J. Hannes            *
************************************************************************
* CALLED BY : FLUIDI                                                   *
* CALLS     : Z_EPSS                                                   *
************************************************************************
* COMMON in : AREA(K), BEDMAS, CMP(1), G, HEIGHT(K), NCELLS            *
*             NCLASS, PRESS(2), RHOS(M), U(K), WRTSCR                  *
*       out : FCOMP(M)                                                 *
* in & out  :                                                          *
************************************************************************
* passed    : M = material indicator                                   *
* returned  : -                                                        *
************************************************************************
* PURPOSE   : calculates correction factor for voidage, due to gas     *
*             velocity changes in the different compartments by        *
*             additional air or changes in the cross section area      *
************************************************************************

      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'

      REAL*8    Z_EPSS,   ! cross section averaged solids volume fraction 
     &          SUM       ! auxiliary
      INTEGER   I,        ! size indicator
     &          K,        ! cell indicator
     &          M         ! material indicator

       IF (WRTSCR) write(*,*) '    CORFAC',M

       DO I=1,NCLASS
            FCOMP(I,M) = 1.
       ENDDO

C*             calculate correction factor Fcomp for existence of 
C*             different gas velocities in the compartments  

        DO I=1,NCLASS
             SUM = 0.
          DO K=1,NCELLS
           SUM = SUM + Z_EPSS(I,K,M)*RHOS(M)*G*(HEIGHT(K)-HEIGHT(K-1))
          ENDDO
             FCOMP(I,M) = PRESS(2) / SUM
        ENDDO     

      RETURN
      END      ! (CORFAC)

