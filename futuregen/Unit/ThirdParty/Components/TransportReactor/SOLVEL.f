
      SUBROUTINE SOLVEL(M)

************************************************************************
* LAST CHANGE AT : 15.04.95                 BY:   J. Hannes            *
************************************************************************
* CALLED BY : CFBC                                                     *
* CALLS     : Z_VELO, Z_EPSS                                           *
************************************************************************
* COMMON in : EPSELU(I,M), ELFLUX(I,M), RHOS(M), PART(I,M), NBED       *
*             MEAND(M), NCLASS, NCELLS, U(K), WRTSCR                   *
*       out : USOLID(I,K,M),USOLA(I,K,M)                               *
* in & out  :                                                          *
************************************************************************
* passed    : M = material indicator                                   *
* returned  : -                                                        *
************************************************************************
* PURPOSE   : calculates individual particle velocity                  *
************************************************************************

      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'
                                  
      REAL*8   UTVIRT(NI) ! virtual terminal velocity due to clusters
      REAL*8   Z_EPSS,    ! individual solids volume fraction of class
     &         EPSS       ! average solids volume fraction
      INTEGER  I     ,    ! size class counter
     &         K     ,    ! cell counter
     &         M          ! material counter

       IF (WRTSCR) write(*,*) '      SOLVEL',M
                                   
       DO I = 1, NCLASS
C*             virtual slip velocity gas-particle
           UTVIRT(I) = U(CMP(1)) - ELFLUX(I,M)/RHOS(M)/EPSELU(I,M) 
       ENDDO

         DO K=1, NCELLS

            EPSS = 0.
          DO I=1,NCLASS
            EPSS = EPSS + Z_EPSS(I,K,BED)*PART(I,BED)
          ENDDO

          DO I = 1, NCLASS
C*               the factor 1.5 considers a parabolic profile
            USOLID(I,K,M) = 1.5*U(K)/(1.-EPSS) - UTVIRT(I)
C*             guarantee any movement to later prevent solution matrix from
C*                singularity
            IF (USOLID(I,K,M) .LT. 0.001 ) THEN
             USOLID(I,K,M) = 0.001
            ENDIF

C              USOLA(I,K,M) = 1. !  Z_VELO(MEAND(BED),BED) ! *4.
              USOLA(I,K,M) = USOLID(I,K,M)
          ENDDO

         ENDDO


!      OPEN(UNIT=77,FILE='SOLVEL'//CHAR(M+48)//'.DAT',STATUS='UNKNOWN')
!
!         DO I=1,NCLASS
!           write(77,*)
!           write(77,*) ' CLASS = ', I
!           write(77,*)
!           write(77,*) ' CELL   U           USOLID      USOLA '
!         DO K = 1,NCELLS
!          write(77,1001) K,U(K),USOLID(I,K,M),USOLA(I,K,M)
!         ENDDO
!         ENDDO

!       CLOSE(77)

! 1001    FORMAT(I3,2X,3(2X,F10.4))

      RETURN
      END      ! (SOLVEL)
