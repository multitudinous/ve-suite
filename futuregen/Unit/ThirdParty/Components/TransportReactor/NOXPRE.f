
      SUBROUTINE NOXPRE
      
************************************************************************
* LAST CHANGE AT :  16.07.1996              BY:   J. Hannes            *
************************************************************************
* CALLED BY : CFBC                                                     *
* CALLS     :                                                          * 
************************************************************************
* COMMON in : AREA(K), COKCO, DIAM(I), HEIGHT(K),                      *
*             MANN(I,K,M), MCOR(I,K,M), NCELLS, NCLASS                 *
* out       :  MCOK, MCAO, MINR, MCOKD                                 *
* in & out  :                                                          *
************************************************************************
* PURPOSE   : NOx reduction precalculations for kinetics               *
************************************************************************

      INCLUDE 'PARAMTR.FTN'
      INCLUDE 'COMMONBL.FTN'

      INTEGER I,K,P

      DO 300 K=1,NCELLS

        DO P=1,2
         MCOK(K,P) = 0.
         MCAO(K,P) = 0.
         MINR(K,P) = 0.
         MCOKD(K,P) = 0.
        ENDDO

      DO 200 I = 1, NCLASS

       MCOK(K,CORE)=MCOK(K,CORE) + MCOR(I,K,4)*COKCO(I,K)/AREA(K)
     &                /(HEIGHT(K)-HEIGHT(K-1))/CORAVG(K)
       MCOK(K,ANNU)=MCOK(K,ANNU) + MANN(I,K,4)*COKAN(I,K)/AREA(K)
     &                /(HEIGHT(K)-HEIGHT(K-1))/(1.-CORAVG(K))
       MCAO(K,CORE)=MCAO(K,CORE) + MCOR(I,K,2)/AREA(K)
     &                /(HEIGHT(K)-HEIGHT(K-1))/CORAVG(K)
       MCAO(K,ANNU)=MCAO(K,ANNU) + MANN(I,K,2)/AREA(K)
     &                /(HEIGHT(K)-HEIGHT(K-1))/(1.-CORAVG(K))
       MINR(K,CORE)=MINR(K,CORE) + MCOR(I,K,3)*COKCO(I,K)/AREA(K)
     &                /(HEIGHT(K)-HEIGHT(K-1))/CORAVG(K)
       MINR(K,ANNU)=MINR(K,ANNU) + MANN(I,K,3)*COKAN(I,K)/AREA(K)
     &                /(HEIGHT(K)-HEIGHT(K-1))/(1.-CORAVG(K))
       MCOKD(K,CORE) = MCOKD(K,CORE) 
     &            + MCOR(I,K,4)*COKCO(I,K)/DIAM(I)**.74
     &            /AREA(K)/(HEIGHT(K)-HEIGHT(K-1))/CORAVG(K)
       MCOKD(K,ANNU) = MCOKD(K,ANNU)
     &            + MANN(I,K,4)*COKAN(I,K)/DIAM(I)**.74 
     &            /AREA(K)/(HEIGHT(K)-HEIGHT(K-1))/(1.-CORAVG(K))

 200   CONTINUE
 300   CONTINUE

!       OPEN(UNIT=34,FILE='NOXPRE.DAT',STATUS='UNKNOWN')
!       write(34,*) ' core '
!       write(34,*) 'CELL     MCOK     MCaO    MINRT   MCOKD'   
!        DO K=1,NCELLS
!         WRITE(34,1111) K,MCOK(K,1),MCAO(K,1),MINR(K,1),MCOKD(K,1)
!        ENDDO
!           WRITE(34,*)
!           WRITE(34,*) ' annulus '
!        write(34,*) 'CELL     MCOK     MCaO    MINRT   MCOKD'   
!        DO K=1,NCELLS
!         WRITE(34,1111) K,MCOK(K,2),MCAO(K,2),MINR(K,2),MCOKD(K,2)
!        ENDDO
! 1111  FORMAT(I3,2(2X,E12.4))
      RETURN
      END         ! (NOXPRE)




