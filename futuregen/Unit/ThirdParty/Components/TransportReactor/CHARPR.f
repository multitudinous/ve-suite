
      SUBROUTINE CHARPR
************************************************************************
* LAST CHANGE AT : 09.02.1995               BY:   J. Hannes            *
************************************************************************
* CALLED BY : CFBC                                                     *
* CALLS     : PROPER, CHARTR                                           *
************************************************************************
* COMMON in : TEMP(K,P),                                               *
*             NCLASS, WRTSCR, CMP(6)                                   *
* out       : RRTCO(I,K),RRTAN(I,K),PHICO(I,K), PHIAN(I,K)             *
* in & out  : -                                                        *
************************************************************************
* passed    : -                                                        *
* returned  : -                                                        *
************************************************************************
* PURPOSE   : Precalculates the char combustion rate and the CO/CO2    *
*             split and simplifies this to a funct. of the oxygen      *
*             concentrations                                           *
************************************************************************

      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'

      REAL*8   XXO2     ! molar oxygen fraction in gas (-)
      REAL*8   RT(NI),  ! specific char combustion rate (mol/kg s) 
     &         PHID(NI) ! C/O2 ratio in combustion product
      INTEGER  I,       ! size class indicator
     &         K        ! riser cell indicator

      IF (WRTSCR) WRITE(*,*) ' CHARPR '
           
C*            guessed average O2 fraction for linearization of combustion rate
           XXO2 =  0.01

C*            combustion rate in compartment per oxygen fraction RRTCO, RRTAN
C*                 (mol/kgCarbon/s/molfracO2)

             CALL PROPER(TBED)
             CALL CHARTR (TBED,XXO2,RT,PHID)
 
      DO 200 K=1,CMP(6)
           DO I = 1, NCLASS
                   RRTCO(I,K) = RT(I) / XXO2
                   PHICO(I,K) = PHID(I)
                   RRTAN(I,K) = RT(I) / XXO2
                   PHIAN(I,K) = PHID(I)
                 
           ENDDO
 200  CONTINUE

      IF (WRTSCR) write(*,*) ' END CHARPR ' 
  
      RETURN
      END        ! (CHARPR)

