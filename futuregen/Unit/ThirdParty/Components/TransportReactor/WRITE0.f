
      SUBROUTINE WRITE0

************************************************************************
* LAST CHANGE AT : 29.10.93                 BY:   J. Hannes            *
************************************************************************
* CALLED BY : CFBC                                                     *
* CALLS     :                                                          *
************************************************************************
* COMMON in :                                                          *
* out       : AEXPO(M), ATTCON(M), NCOMP, HEIGHT(K), LENGTH(K),WIDTH(K)*
*             COALM(1-9), CYCDAT(1-9), FRGFAC, HEATCP(M), NCYC,AREA(K),*
*             PRESS(1-4), OPTION(1-9), SPHR(M),                        *
*             FEED(M), TBED, TIN, TAMB                                 *
* in & out  :                                                          *
************************************************************************
* passed    : FLNAME = name of input file                              *
* returned  : -                                                        *
************************************************************************
* PURPOSE   : writes input data for check                              * 
************************************************************************


        INCLUDE 'PARAMTR.FTN'
        INCLUDE 'COMMONBL.FTN'

 
        INTEGER  M        ! material indicator


      OPEN (UNIT=40,FILE='DATAIN.DAT',STATUS='UNKNOWN')
           
       write(40,*) ' RISER DATA'    
       write(40,*) 
      DO M = 1, NCOMP
       write(40,*) M,HEIGHT(M),WIDTH(M),LENGTH(M),ADDAIR(M)
      ENDDO 
       write(40,*)

       write(40,*) ' CYCLONE DATA'    
       write(40,*) NCYC
      DO M = 1, 9
       write(40,*) CYCDAT(M)
      ENDDO 
       write(40,*)
       
       write(40,*) 'FEED MASS FLOWS '
        DO M = 1, 3
         write(40,*) FEED(M)
        ENDDO 
           write(40,*)


       write(40,*) 'TEMPERATURES'
           write(40,*) TBED,TIN,TAMB
               write(40,*)

       write(40,*) ' PRESSURES '
        DO M = 1, 4
         write(40,*) PRESS(M)
        ENDDO 
           write(40,*)


           write(40,*) 'CONSTANTS RHOS and SPHR'
        DO M = 1, 4
         write(40,*) RHOS(M),SPHR(M)
        ENDDO 
           write(40,*)

         write(40,*) 'CP AND ATTR '
        DO M = 1, 4
         write(40,*) HEATCP(M),ATTCON(M)
        ENDDO 
           write(40,*)

       write(40,*) ' FRAG AND EXP '
        DO M = 1, 4
         write(40,*) FRGFAC(M),AEXPO(M)
        ENDDO 
           write(40,*)

       write(40,*) ' COAL '
        DO M = 1, 9
         write(40,*) COALM(M)
        ENDDO 
           write(40,*)

       write(40,*) ' SPLTN '
         write(40,*) SPLTN
           write(40,*)

       write(40,*) 'OPTIONS  '
        DO M = 1, 10
         write(40,*) OPTION(M)
        ENDDO 
           write(40,*)

         CLOSE(40)

      RETURN
      END         ! (WRITE(0)


