
      SUBROUTINE SOLCON(M)

************************************************************************
* LAST CHANGE AT : 19.09.1994               BY:   J. Hannes            *
************************************************************************
* CALLED BY : CFBC                                                     *
* CALLS     : Z_CORE, Z_EPSS, Z_EPSA                                   *
************************************************************************
* COMMON in : AREA(K), HEIGHT(K), NCELLS, NCLASS,                      *
*             PART(I,M), BEDM(M), RHOS(M), WRTSCR                      *
* out       : MCOR(I,K,M), MANN(I,K,M)                                 *
* in & out  :                                                          *
************************************************************************
* passed    : -                                                        *
* returned  : -                                                        *
************************************************************************
* PURPOSE   : calculates the solid mass holdup in each cell per class  *
*             and material and the split between core and annulus      *
*             further the cross section fraction covered by the core   *
*             and the annulus for the gas                              *
************************************************************************

      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'

      REAL*8
     &  Z_CORE, ! core fraction in cell
     &  Z_EPSS, ! average solid volume fraction in cell
     &  Z_EPSA  ! solid volume fraction in annulus

      REAL*8
     &  ZCORE,  ! value of Z_CORE
     &  EPSSAN, ! indiv. solids volume fraction in annulus
     &  EPSSCO  ! indiv. solids volume fraction in core


      INTEGER  I, ! particle class indicator
     &         K, ! riser cell indicator
     &         M  ! particle material indicator

      IF (WRTSCR) write(*,*) ' SOLCON '

              BEDM(M) = 0.

      DO 300 K = 1, NCELLS 

         DO 200 I = 1, NCLASS                                           

C*                   core area fraction per class, mat and cell
                  ZCORE = Z_CORE(I,K,M)

C*                   solid volume fractions and core - annulus - bubble ratio
               EPSSAN = Z_EPSA(I,K,M)   
               EPSSCO = (Z_EPSS(I,K,M) - EPSSAN * (1.-ZCORE) ) /ZCORE 
C*                   set mass in cells of riser
               MCOR(I,K,M)  = AREA(K)*(HEIGHT(K)-HEIGHT(K-1))
     &                        *ZCORE*EPSSCO
     &                        *RHOS(M)*PART(I,M)
               MANN(I,K,M)  = AREA(K)*(HEIGHT(K)-HEIGHT(K-1))
     &                        *(1.- ZCORE)*EPSSAN
     &                        *RHOS(M)*PART(I,M)

               BEDM(M) = BEDM(M) + MCOR(I,K,M) + MANN(I,K,M)

 200    CONTINUE
 300  CONTINUE      
          IF (WRTSCR) write(*,*) ' END SOLCON '

      RETURN
      END          ! (SOLCON)

