
c---------------------------------------------------------------------------------------
c-----
c      This program will optimize the curve fit of a lognormal pdf to a frequency
c      distribution and then discretize either the lognormal curve, or the
c      histogram directly into any number of equal intervals defined by the user.
c      Direct discretization utilizes least square curve fits of quadratic equations
c      over each interval width.  The expected values are computed from the quadratic
c      equations.
c
c      The data being fit need not be equal intervals.
c
c      Input- all data is input through a previously written data file
c            named:  logfit.dat
c            the data should include in this order:
c
c            variable     specification
c
c               int       number of intervals in data to be fit (integer)
c      b (lower), b (upper), h (i)
c                          where
c                b(i)      interval endpoints (e.g. 8., 10.2, 20.6, etc.)
c                h(i)      population in each interval
c
c      Output- data is output to the screen and written to a file named:
c             logfit.out
c             The output data includes the distribution statistics, discretized
c             interval endpoints and the expected values for those intervals.
c
c      Subroutines: two are used to perform integration:  simp1.for, simp2.for.
c                   For direct discretization of the histogram, three subroutines
c                   are used:  trans.for, mult.for, and invmat.for.
c
c
c-----
c----------------------------------------------------------------------------------------

      subroutine histfit(ps,pmf,passed_areas,size,
     &                   percent_through_100,percent_through_200)

c     arguments passed (DAS)
      double precision ps(20),pmf(20),passed_areas(19)
      double precision percent_through_100,percent_through_200
     
c     original stuff
      dimension x(100), d(100), b(100), h(100), gmp(100), ans1(100),
     &ans2(100), amp(100), aint(100), ahint(100), gmpt(100), freq(100),
     &gmh(100), ama(100), cum(2,0:100), xc(100), jarray(100),
     &xmat(100,100), ymat(100,100), bb(100,100), cc(100,100),
     &dd(100,100), ee(100,100), ff(100,100), dh(100)

      double precision const, x, d, ans, diff, aint, sg, b, h, gmp, ans1
     &, ans2, m1, m2, m3, m4, mean, sig, sk, ku, gmh, ama, cum, xmat,
     &ymat, bb, cc, dd, ee, ff, xc, dh, slope, sinter, top, bot, ahint

      integer r1, r2, r3, r4, c1, c2, c3, c4
      integer size

cdas      open (unit=10, file='logfit.out', status='unknown')
cdas      open (unit=13, file='logfit.dat', status='old')

cdas      read (unit=13, fmt=*) int
cdas      read (unit=13, fmt=*) (b(i), ajunk, h(i), i=1, int)
cdas      b (int+1) = ajunk

          int = 3     ! HARDWIRE - always two mesh sizes (yielding three regions)
	    b(1) = 0.1
	    b(2) = 74.0
	    b(3) = 287.0  ! HARDWIRES - for 200 and 50 mesh - (#100 is 0.147)
	    b(4) = 400.0

	    h(1) = percent_through_200
	    h(2) = percent_through_100-percent_through_200  ! FROM C (DAS)
	    h(3) = 100.0-percent_through_100


c-----the program is divided into three sections
c     determination of interval midpoints, determination of lognormal
c     pdf parameters, and finally discretization of either the
c     lognormal pdf or the histogram directly.

c-----the value n sets the number of integration intervals used 
c     in the subroutine simp, tota utilizes arithmetic averaging
c     totg utilizes geometric averaging.

      n = 100
      tota = 0.
      totg = 0.
      pi = 3.14159265359

c-----compute geometric and arithmetic midpoints

      do 10 i = 1, int
         gmp(i) = sqrt(b(i) * b(i+1))
         gmh(i) = h(i)
         totg = totg + gmh(i)
         gmpt(i) = gmp(i)
         amp(i) = (b(i) + b(i + 1)) / 2.
         ama(i) = h(i)
         tota = tota + ama(i)
         cum(1,i) = b(i + 1)
         cum(2,i) = tota
 10   continue

      ad1 = 0.
      ad2 = 0.
      ad3 = 0.
      ad4 = 0.
      am1 = 0.
      am2 = 0.
      ap2 = 0.

c-----freq(i) is the normalized frequency for each interval
c     cum(2,i) contains the cummulative distribution
c     NOTE:  This program was rewritten many times consequently there is 
c     some redundancy in variables.

      do 11 i = 1, int
         freq(i) = (gmh(i) / totg) / (b(i + 1) - b(i))
         ad1 = ad1 + ama(i)*amp(i)
         ad2 = ad2 + ama(i)*(amp(i)**2)
         ad3 = ad3 + ama(i)*(amp(i)**3)
         ad4 = ad4 + ama(i)*(amp(i)**4)
         am1 = am1 + ((amp(i) * ama(i)) / tota)
         cum(2,i) = cum(2,i)/tota
 11   continue

      do 12 i = 1, int
         am2 = am2 + (ama(i) * (amp(i) **2.))
         ap2 = ap2 + (ama(i) * amp(i))
 12   continue

      kount = 0
      resid = 10.
      epsi = .0001
      oldtest = 1000000.

      do while (resid .ge. epsi)
         d1 = 0.
         d2 = 0.
         d3 = 0.
         d4 = 0.
         m1 = 0.
         m2 = 0.
         p2 = 0.
         test = 0.
         gf = 0.

c-----this section determines the average of the distribution

         do 20 i = 1, int
            d1 = d1 + gmh(i)*gmp(i)
            d2 = d2 + gmh(i)*(gmp(i)**2)
            d3 = d3 + gmh(i)*(gmp(i)**3)
            d4 = d4 + gmh(i)*(gmp(i)**4)
            m1 = m1 + ((gmp(i) * gmh(i)) / totg)
 20      continue

c-----this section determines the standard deviation

         do 21 i = 1, int
            m2 = m2 + (gmh(i) * (gmp(i)**2.))
            p2 = p2 + (gmh(i) * gmp(i))
 21      continue

         sg = sqrt((m2 - ((p2 ** 2.) / totg)) / (totg * 0.99))

c-----This section transforms the distribution mean and standard
c     deviation into values consistent with the log normal pdf
c     which assumes  y=log(x) and optimizes the curve fit.

         sig = sqrt (dlog (((sg **2.) / (m1 **2.)) +1))
         mean = dlog(m1) - (0.5 * (sig **2.))

c       print*,'M1 ', m1, ' SG ', sg, ' resid ',resid
         do 30 i = 1, int
            call simp2 (b(i), b(i + 1), mean, sig, n, ans1(i))
            call simp1 (b(i), b(i + 1), mean, sig, n, ans2(i))
            test = test + ((((gmp(i) - (ans1(i) / ans2(i))) ** 2.)
     &             / ans1(i)) * ans2(i))
            gf = gf + ((((gmpt(i) - (ans1(i) / ans2(i))) ** 2.)
     &           / ans1(i)) * ans2(i))
 30      continue

         resid = abs(test - oldtest)
         if ( test .ge. oldtest) then
c            write(unit=*, fmt=*) 'test is > oldtest'
            resid = .000001
         end if

         if (kount .gt. 100) resid = .000001
         oldtest = test
         kount = kount + 1

         totg = 0
         do 40 i = 1, int
            gmp(i) = ans1(i) / ans2(i)
            gmh(i) = h(i)
            totg = totg + gmh(i)
 40      continue

      end do

c-----This section determines the standard deviation 
c-----coef. of skewness and coef. of kurtosis.

      m3 = 0
      m4 = 0

      do 41 i = 1, int
         m3 = m3 + (((gmh(i) * ((gmp(i) - m1) ** 2.)) *
     &        (gmp(i) - m1)) / totg)
         m4 = m4 + ((gmh(i) * (((gmp(i) - m1) ** 2.)
     &        ** 2.)) / totg)
 41   continue

      am2 = sqrt((am2 - ((ap2 ** 2.) / tota)) / (tota * 0.99))
      sk = m3 / (sg ** 3.)
      ku = (m4 / (sg ** 4.)) - 3.

c-----This section determines different diameters of the fitted distribution
c-----and histogram and degrees of freedom for goodness of fit test

      d10 = d1/totg
      d20 = (d2**0.5) / (totg**0.5)
      d30 = (d3**(1./3.)) / (totg**(1./3.))
      d32 = d3/d2
      d43 = d4/d3
      ad10 = ad1/tota
      ad20 = (ad2**0.5) / (tota**0.5)
      ad30 = (ad3**(1./3.)) / (tota**(1./3.))
      ad32 = ad3/ad2
      ad43 = ad4/ad3
      df = int - 3.

c-----OUTPUT OF INTITIAL RESULTS

cdas      write (unit=*, fmt=101)
cdas      read (unit=*, fmt=*) basis  

         basis = 1  ! HARDWIRE - mass - (DAS)

cdas      if (basis .eq. 2) then
cdas
cdas         write (unit=*, fmt=166)
cdas         write (unit=10, fmt=166)
cdas         write (unit=*, fmt=165) d10, ad10, d20, ad20, d30, ad30, 
cdas     &     d32, ad32, d43, ad43, sg, am2, sk, ku, mean, sig, gf, df
cdas         write (unit=10, fmt=165) d10, ad10, d20, ad20, d30, ad30, 
cdas     &     d32, ad32, d43, ad43, sg, am2, sk, ku, mean, sig, gf, df
cdas
cdas      elseif (basis .eq. 1) then
cdas
cdas         write (unit=*, fmt=166)
cdas         write (unit=10, fmt=166)
cdas         write (unit=*, fmt=265) d10, ad10,
cdas     &     sg, am2, sk, ku, mean, sig, gf, df
cdas         write (unit=10, fmt=265) d10, ad10,
cdas     &     sg, am2, sk, ku, mean, sig, gf, df
cdas
cdas      endif
cdas
cdas      write (unit=*, fmt=921)

cdas      read (unit=*, fmt=*) gof
cdas      if (gof .ne. 1) goto 33

        gof = 1   ! HARDWIRE - must be 1 anyway so just hardwire (DAS)

c-----This part which is divided into two main sections 
c     discretizes the histogram or the lognormal pdf.
c     into user defined intervals.

 888  if (gof .eq. 1) then
cdas         write (unit=*, fmt=777)
cdas         read (unit=*, fmt=*) choice

        choice = 1   !HARDWIRE - use computed values for mean/sigma (DAS)

cdas         if (choice .eq. 0) then
cdas            write (unit=*, fmt=778)
cdas            read(unit=*, fmt=*) m1, sg
cdas            sig = sqrt (dlog (((sg ** 2.) / (m1 ** 2.)) + 1))
cdas            mean = dlog (m1) - (0.5 * (sig ** 2.))
cdas            write (unit=*, fmt=65) mean, sig
cdas            write (unit=10, fmt=65) mean, sig
cdas         end if
      end if

      if (gof .eq. 1) then

cdas         write (unit=*, fmt=922)
cdas         read (unit=*, fmt=*) m
cdas         write (unit=*, fmt=45) m
cdas         write (unit=10, fmt=45) m
cdas         write (unit=*, fmt=912)
cdas         read (unit=*, fmt=*) dch

         m   = size             ! HARDWIRE - # of intervals
         dch = 0                ! HARDWIRE - discretize by specifying area

         scale = 0.5
         const = 1. / sqrt(2. * pi)
         x(1) = b(1) * 0.00001
         
         if (dch .eq. 0) then

cdas            write (unit=*, fmt=911)
cdas            read (unit=*, fmt=*) eint

            eint = 0   ! HARDWIRE - intervals NOT equal areas

            am = m
            sum00 = 0

c-----This section determines the proper endpoints for each of the intervals.

            aleft = 1
            xc(1) = b(1)
            do 50 i = 1, m-1
               
               if (eint .ne. 1) then
cdas                  write (unit=*, fmt=913) i, aleft
cdas                  read (unit=*, fmt=*) area

                      area = passed_areas(i)   ! THIS COMES FROM C - (DAS) - array range 1->9
               else
                  area = 1 / am
               end if

               sum00 = sum00 + area
               aleft = 1 - sum00

c**discretize the histogram

               ahint(i) = area
               resid = 0.
               epsi = sum00
               j = 0
               do while (resid .lt. epsi)
                  j = j + 1
                  resid = cum(2,j)
               end do
               slope = (cum(1,j)-cum(1,j-1))/(cum(2,j)-cum(2,j-1))
               sinter = cum(1,j) - slope*cum(2,j)
               xc (i+1) = slope*sum00 + sinter
               jarray (i+1) = j

               if (i .eq. 1) then
                  resid = 0
                  epsi = 0.00001
                  j = 0
                  do while (resid .lt. epsi)
                     j = j+1
                     resid = h(j)
                  end do
                  xc(1) = b(j)
                  jarray(1) = j
               elseif (i .eq. m-1) then
                  resid =0
                  epsi = 0.00001
                  j = int

                  do while (resid .lt. epsi)
                     j = j-1
                     resid = h(j)
                  end do
                  
                  xc(m+1) = b(j+1)
                  jarray(m+1) = j
               endif

c**discretize the lognormal pdf

               x(i + 1) = b(int + 1)
               resid = 10.
               epsi = .000001
               do while (resid .ge. epsi)
                  call simp1 (x(i), x(i + 1), mean, sig, n, ans)
                  aint(i) = const * ans
                  diff = area - aint(i)
                  resid = abs(diff)
                  x(i + 1) = x(i + 1) + ((diff * x(i + 1)) * scale)
               end do
               freq(i) = area / (x(i + 1) - x(i))

 50      continue
      
         aint(m) = aleft
         ahint(m) = aleft
         freq(m) = 0.

      else

cdas         sum01 = 0
cdas
cdas         resid = 0
cdas         epsi = 0.00001
cdas         j = 0
cdas         do while (resid .lt. epsi)
cdas            j = j + 1
cdas            resid = h(j)
cdas         end do
cdas         xsm = b(j)
cdas
cdas         ahold = cum(2,j-1)
cdas
cdas         resid = 0
cdas         epsi = 0.00001
cdas         j = int
cdas         do while (resid .lt. epsi)
cdas            j = j-1
cdas            resid = h(j)
cdas         end do
cdas         xlg = b(j+1)
cdas
cdas         do 51 i = 1, m-1
cdas            write (unit=*, fmt=914) i, xsm, xlg
cdas            read (unit=*, fmt=*) x(i + 1)
cdas
cdasc**discretize the histogram
cdas
cdas            resid = 0.
cdas            epsi = x(i + 1)
cdas            j = 0
cdas            do while (resid .lt. epsi)
cdas               j = j+1
cdas               resid = b(j)
cdas            end do
cdas            xc(i+1) = x(i+1)
cdas            jarray(i+1) = j
cdas
cdas            slope = (cum(2,j) - cum(2,j-1)) / (cum(1,j)-cum(1,j-1))
cdas            sinter = cum(2,j) - slope*cum(1,j)
cdas            ahint(i) = slope*xc(i+1) + sinter
cdas            ahint(i) = ahint(i) - ahold
cdas            if(i .eq. 1) then
cdas               resid = 0
cdas               epsi = 1
cdas               j = 0
cdas               do while (resid .lt. epsi)
cdas                  j = j+1
cdas                  resid = h(j)
cdas               end do
cdas               xc(1) = b(j)
cdas               jarray(1) = j
cdas            elseif (i .eq. m-1) then
cdas               resid = 0
cdas               epsi = 1
cdas               j=int
cdas               do while (resid .lt. epsi)
cdas                  j = j-1
cdas                  resid = h(j)
cdas               end do
cdas               xc(m+1) = b(j+1)
cdas               jarray(m+1) = j
cdas            endif
cdas
cdas            ahold = ahold + ahint(i)
cdas
cdasc**discretize the lognormal
cdas
cdas            call simp1(x(i), x(i + 1), mean, sig, n, ans)
cdas            aint(i) = const * ans
cdas            freq(i) = aint(i) / (x(i + 1) - x(i))
cdas            sum01 = sum01 + aint(i)
cdas 51      continue
cdas
cdas         ahint(m) = 1 - ahold
cdas         aint(m) = 1 - sum01
cdas         freq(m) = 0.

      end if

c-----This section determines the expected values of 
c     each of the intervals found above

c**for histogram


cdas   WITH ONLY TWO mesh size points, there are not enough bins to discretize the histogram
cdas   comment this out to keep code from exceeded array bounds
cdas
cdas
cdas      bad = 0
cdas
cdas      do 61 i = 1, m
cdas
cdas         if (i .eq. 1) then
cdas            k = jarray(i+1) - jarray(i) + 1
cdas            jst = jarray(i)
cdas         elseif (i .eq. m) then
cdas            k = jarray(i+1) - jarray(i) + 1
cdas            jst = jarray(i) - 1
cdas         else
cdas            k = jarray(i+1) - jarray(i) + 2
cdas            jst = jarray(i) - 1
cdas         endif
cdas
cdas         if (k .ge. 3) then
cdas
cdas            l = 0
cdas            do 62 j = jst, jst+k
cdas               l = l+1
cdas               xmat(l,1) = 1.
cdas               xmat(l,2) = b(j+1)
cdas               xmat(l,3) = b(j+1)*b(j+1)
cdas               ymat(l,1) = (ama(j) / tota) / (b(j + 1) - b(j))
cdas 62         continue
cdas
cdas            call trans(k, 3, xmat, r1, c1, bb)
cdas            call mult(r1, c1, bb, k, 3, xmat, r2, c2, cc)
cdas            call mult(r1, c1, bb, k, 1, ymat, r3, c3, dd)
cdas            call invmat(3, cc, ff)
cdas            call mult(3, 3, ff, r3, c3, dd, r4, c4, ee)
cdas            
cdas            top = ee(1,1)*(xc(i+1)**2 - xc(i)**2)/2. +
cdas     &            ee(2,1)*(xc(i+1)**3 - xc(i)**3)/3. +
cdas     &            ee(3,1)*(xc(i+1)**4 - xc(i)**4)/4.
cdas            bot = ee(1,1)*(xc(i+1) - xc(i)) +
cdas     &            ee(2,1)*(xc(i+1)**2 - xc(i)**2)/2. +
cdas     &            ee(3,1)*(xc(i+1)**3 - xc(i)**3)/3.
cdas
cdas            dh(i) = top/bot
cdas
cdas         else
cdas
cdas            dh(i) = 0
cdas            write (unit=*, fmt=8503)
cdas            write (unit=10, fmt=8503)
cdas            bad = 1
cdas
cdas         endif
cdas
cdas 61   continue


c**For lognormal curve

      do 60 i = 1, m
         if (i .ne. m) then
            call simp2(x(i), x(i+1), mean, sig, n, ans)
            d(i) = (ans * const) * (1 / aint(i))
         else
            call simp2(b(1), x(i), mean, sig, n, ans)
            atot = exp(mean + ((sig **2.) / 2.))
            d(i) = (atot - (const * ans)) * (1 / aint(m))
         end if
 60   continue

c   **********************  final output  *************************

c**for histogram

cdas   SEE expanation above !!!!!!!!!!!!!
cdas 
cdas      if (bad .ne. 1) then
cdas
cdas         write (unit=*, fmt=8501)
cdas         write (unit=10, fmt=8501)
cdas         write (unit=*, fmt=8500)
cdas         write (unit=10, fmt=8500)
cdas
cdas         do 70 i = 1, m
cdas            write (*, 200) i, xc(i), xc(i + 1), dh(i), ahint(i)
cdas            write (10, 200) i, xc(i), xc(i + 1), dh(i), ahint(i)
cdas 70      continue
cdas      
cdas      endif


c      For lognormal curve fit

cdas      write (unit=*, fmt=8502)
cdas      write (unit=10, fmt=8502)
cdas      write (unit=*, fmt=8500)
cdas      write (unit=10, fmt=8500)

      i=1
cdas      write (unit=*, fmt=201) i, x(i), x(i + 1), d(i), aint(i)
cdas      write (unit=10, fmt=201) i, x(i), x(i + 1), d(i), aint(i)
cdas
cdas      do 71 i = 2, m-1
cdas         write (unit=*, fmt=200) i, x(i), x(i+1), d(i), aint(i)
cdas         write (unit=10, fmt=200) i, x(i), x(i+1), d(i), aint(i)
cdas 71   continue
cdas
cdas      write (unit=*, fmt=205) m, x(m), d(m), aint(m)
cdas      write (unit=10, fmt=205) m, x(m), d(m), aint(m)

c     load arrays from C  - (DAS)
      do kk=1,m
	   ps(kk)  = d(kk)
	   pmf(kk) = aint(kk)
	enddo

      end if  ! WHERE DOES THIS COME FROM? CAN'T FIND IT!


c-----modified to handle corrected and uncorrected counts.


c-----FORMAT STATEMENTS below this line

 101  format(/,' Input basis of histogram',//,
     &' 1 = based on mass distribution',/,
     &' 2 = based on number distribution',/)
 166  format(10x,'Properties of Lognormal Fit and Histogram',/)
 165  format(22x,'lognormal', 11x, 'histogram',//,
     &1x, 'd10', 19x, g11.5, 9x,
     &g11.5,/, 1x, 'd20', 19x, g11.5, 9x, g11.5, /, 1x, 'd30', 19x,
     &g11.5,
     &9x, g11.5,/,1x,'d32',19x,g11.5,9x,g11.5,
     &/,1x,'d43',19x,g11.5,9x,g11.5,
     &/,1x,'standard deviation',
     &4x,g11.5,9x,g11.5,/,1x,'coef. of skewness',
     &5x,g11.5,/,1x,'coef. of kurtosis',
     &5x,g11.5,/,1x,'log normal (mean)',
     &5x,g11.5,/,1x,'log normal (st dev)',
     &3x,g11.5,/,1x,'goodness of fit',
     &7x,g11.5,/,1x,'degrees of freedom',
     &4x,g11.5)
 265  format(22x,'lognormal',11x,'histogram',//,
     &1x,'mass mean',12x,g11.5,9x,
     &g11.5,/,1x,'standard deviation',
     &4x,g11.5,9x,g11.5,/,1x,'coef. of skewness',
     &5x,g11.5,/,1x,'coef. of kurtosis',
     &5x,g11.5,/,1x,'log normal (mean)',
     &5x,g11.5,/,1x,'log normal (st dev)',
     &3x,g11.5,/,1x,'goodness of fit',
     &7x,g11.5,/,1x,'degrees of freedom',
     &4x,g11.5)
 921  format(/,' do you want to discretize?',//,
     &' input 0 if no',/,' input 1 if yes',/)
 777  format(/,41h in order to discretize the lognormal pdf,/,
     &44h the transformed mean and sigma are required,//,
     &43h input 0 to enter values for mean and sigma,/,
     &54h input 1 to use the computed values for mean and sigma,/)
 778  format(//,21h input mean and sigma,/)
 922  format(/,51h input number of desired intervals for discretizing,/)
 912  format(/,41h input 0 to discretize by specifying area,/,
     &45h input 1 to discretize by specifying endpoint,/)
 911  format(/,37h do you want intervals of equal area?,//,
     &14h input 0 if no,/,15h input 1 if yes,/)
 913  format(/,25h input area for interval ,i3,/,
     &28h the area must be less than ,g10.3/)
 914  format(/,30h input upperbound of interval ,i3,12h  in microns,/
     &/,' initial upperbound must exceed ', 2x,f5.0,3x,'microns',
     &/,'final upperbound can not exceed ', 2x,f5.0,3x,'microns',/)
 8500 format('interval',3x,'lower bound',3x,'upper bound',3x,
     &'expected value',3x,' area of',/,12x,' (microns) ',3x,
     &' (microns) ',3x,
     &'  (microns)  ',3x,'integration',/)
 8501 format(/,'------- discretization of the histogram ------',/)
 8502 format(/,'------- discretization of the lognormal curve -----',/)
 8503 format(/,
     &'ERROR:  too many intervals for histogram discretization',/)
 722  format(/,49h do you want to rediscretize a new lognormal pdf?,//,
     &14h input 0 if no,/,15h input 1 if yes,/)
 8002 format(/,'------ curve fit based on corrected counts ------',/)
 8003 format(/,'------ curve fit based on uncorrected counts -----',/)
 45   format(/,'The histogram and lognormal pdf are divided into',i3,3x
     &,'parts.',/,'The diameters for each interval are expected ',
     &'values.')
 65   format(20h transformed mean = ,g11.5,20htransformed sigma = 
     &,g11.5)
 200  format(4x,i2,6x,g11.5,3x,g11.5,5x,g11.5,6x,f6.4)
 201  format(4x,i2,2x,f11.5,7x,g11.5,5x,g11.5,6x,f6.4)
 205  format(4x,i2,6x,g11.5,3x,'infinity',8x,g11.5,6x,f6.4)

 33   return
      end
c-----------------------------------------------------------------------
c      include "trans.f"
c      include "mult.f"
c      include "invmat.f"
c      include "simp1.f"
c      include "simp2.f"
c-----------------------------------------------------------------------
      subroutine trans(r1, c1, aa, r2, c2, bb)
      dimension aa(100,100), bb(100,100)
      double precision aa, bb
      integer r1, c1, r2, c2

      do 100 i = 1, c1
         do 200 j = 1, r1
            bb(i,j) = aa(j,i)
 200     continue
 100  continue
      r2 = c1
      c2 = r1
      return
      end
c-----------------------------------------------------------------------
c----------------------------------------------------------------------- 
      subroutine mult(r1, c1, aa, r2, c2, bb, r3, c3, cc)
      dimension aa(100,100), bb(100, 100), cc(100, 100)
      double precision aa, bb, cc
      integer r1, c1, r2, c2, r3, c3

      do 100 i = 1, r1
         do 200 j = 1, c2
            sum = 0
            do 300 k = 1, c1
               sum = sum + (aa(i,k) * bb(k,j))
 300        continue
            cc(i,j) = sum
 200     continue
 100  continue
      r3 = r1
      c3 = c2
      return
      end
c-----------------------------------------------------------------------
c-----------------------------------------------------------------------
c..Subroutine for matrix inversion and solution
c..variables
c..        ndim=row dimension of coefficient matrix
c..        nord = order of coefficient matrix
c..        a = coefficient matrix
c..        b = rhs vector
c..        aniv = inverse of matrix
c..        sv = solution vector
c..        dtnrm = normalized determinant
c..        detm = determinant

      subroutine invmat(nord, a, ainv)
      double precision a(100, 100), ainv(100, 100), j(100),
     &dtnrm, detm
      integer nord

      do 5 i = 1, nord
         do 10 k = 1, nord
            ainv(i,k) = a(i,k)
 10      continue
 5    continue

      pd = 1.
      n = nord
      do 124 l = 1, n
         dd = 0.
         do 123 k = 1, n
            dd = dd + (ainv(l,k) * ainv(l,k))
 123     continue
         dd = sqrt(dd)
         pd = pd * dd
 124  continue

      detm = 1.
      do 125 l = 1, n
         j(l + 20) = l
 125  continue

      do 144 l = 1, n
         cc =0.
         m = l
         do 135 k = l, n
            if ((abs(cc) - abs(ainv(l,k))) .ge. 0.) goto 135
 126        m = k
            cc = ainv(l,k)
 135     continue
 127     if (l .eq. m) goto 138
 128     k = j(m + 20) 
         j(m + 20) = j(l + 20)
         j(l + 20) = k
         do 137 k = 1, n
            s = ainv(k,l)
            ainv(k,l) = ainv(k,m)
            ainv(k,m) = s
 137     continue
 138     ainv(l,l) = 1.
         detm = detm * cc
         do 139 m = 1, n
            ainv(l,m) = ainv(l,m) / cc
 139     continue
         do 142 m = 1, n
            if (l .eq. m) goto 142
 129        cc = ainv(m,l)
            if (cc .eq. 0.) goto 142
 130        ainv(m,l) = 0.
            do 141 k = 1, n
               ainv(m,k) = ainv(m,k) - (cc * ainv(l,k))
 141        continue
 142     continue
 144  continue

      do 143 l = 1, n
         if (j(l + 20) .eq. l) goto 143
 131     m = l
 132     m = m + 1
         if (j(m + 20) .eq. l) goto 133
 136     if (n .gt. m) goto 132
 133     j(m + 20) = j(l + 20)
         do 163 k = 1, n
            cc = ainv(l,k)
            ainv(l,k) = ainv(m,k)
            ainv(m,k) = cc
 163     continue
         j(l + 20) = l
 143  continue

      detm = abs(detm)
c       do 20 i = 1, n
c          sv(i) = 0.
c          do 30 k = 1, n
c             sv(i) = sv(i) + ainv(i,k)*b(k)
c30        continue
c20     conitnue

      dtnrm = detm / pd

      return
      end
c-----------------------------------------------------------------------
c-----------------------------------------------------------------------
c        Simpson's composite algorithm 4.1
c
c        to approximate i = integral (f(x)dx) from a to b.
c
c        input:  endpoints a, b, and positive integer m.
c
c        output:   approximation xi to i.
c
c******************** Calculate approximation of f *********************
      subroutine simp1(a, b, xm1, xm2, m, ans)
c        step 1
      double precision a, b, xm1, xm2, ans, h, xi0, xi1, xi2, x, xi
c        step 2

      h = (b-a) / (2*m)
c        summation of f(x(2*i-1))
      xi0 = ((dexp(- (0.5 * (((dlog(a) - xm1) / xm2) **2.))) / xm2) / 
     &a) + ((dexp(- (0.5 * (((dlog(b) - xm1) / xm2)**2.))) / xm2) / b)
c        summation of f(x(2*i))
      xi1 = 0.0
c        step 3
      xi2 = 0.0

      mm = (2 * m) - 1
c        step 4

      do 10 i = 1, mm
c        step 5
         x = a + (i * h)

         if (i .eq. (2 * (i / 2))) then
          xi2 = xi2 + ((dexp(- (0.5 * (((dlog(x) - xm1) / xm2)
     &      ** 2.))) / xm2) / x)
     
         else
          xi1 = xi1 + ((dexp(- (0.5 * (((dlog(x) - xm1) / xm2) 
     &      ** 2.))) / xm2) / x)
     
         end if
c          step 6

 10   continue

      xi = (xi0 + (2 * xi2)) + (4 * xi1)
c          step 7
c          output

      xi = (xi * h) / 3

      ans = xi
      return
      end
c-----------------------------------------------------------------------
c-----------------------------------------------------------------------
c          Simpson's composite algorithm 4.1
c
c          to approximate i = integral (f(x)dx) from a to b.
c
c          input: endpoints a, b, and positive integer m.
c
c          output:   approximation xi to i.
c
c******************** Calculate approximation of f *********************
      subroutine simp2(a, b, xm1, xm2, m, ans)
c         step 1
      double precision a, b, xm1, xm2, ans, h, xi0, xi1, xi2, x, xi
c         step 2

      h = (b - a) / (2 * m)
c         summation of f(x(2 * i - 1))
      xi0 = (dexp(- (0.5 * (((dlog(a) - xm1) / xm2) ** 2.))) / xm2) 
     &+ (dexp(- (0.5 * (((dlog(b) - xm1) / xm2) ** 2.))) / xm2)
c         summation of f(x(2*i))
      xi1 = 0.0
c         step 3
      xi2 = 0.0

      mm = (2 * m) - 1
c         step 4

      do 10 i = 1, mm
c         step 5
         x = a + (i * h)

         if (i .eq. (2 * (i / 2))) then
            xi2 = xi2 + (dexp(- (0.5 * (((dlog(x) - xm1) / xm2)
     & ** 2.))) / xm2)
     
         else
            xi1 = xi1 + (dexp(- (0.5 * (((dlog(x) - xm1) / xm2) 
     & ** 2.))) / xm2)

     
         end if
c         step 6

 10   continue

      xi = (xi0 + (2 * xi2)) + (4 * xi1)
c         step 7
c         output

      xi = (xi * h) / 3

      ans = xi
      return
      end
c-----------------------------------------------------------------------
c this subroutine bins the size distribution given the passed_areas and
c the mass mean m1 and standard deviation sg. it returns the binned sizes
c ps and the associated mass fractions pmf
c-----------------------------------------------------------------------
      subroutine createbins(ps,pmf,passed_areas,size,
     &                       m1, sg)

c     arguments passed (DAS)
      double precision ps(20),pmf(20),passed_areas(19),m1,sg
c      double precision percent_through_100,percent_through_200
     
c     original stuff
      dimension x(100), d(100), b(100), h(100), gmp(100), ans1(100),
     &ans2(100), amp(100), aint(100), ahint(100), gmpt(100), freq(100),
     &gmh(100), ama(100), cum(2,0:100), xc(100), jarray(100),
     &xmat(100,100), ymat(100,100), bb(100,100), cc(100,100),
     &dd(100,100), ee(100,100), ff(100,100), dh(100)

      double precision const, x, d, ans, diff, aint, b, h, gmp, ans1
     &, ans2, m2, m3, m4, mean, sig, sk, ku, gmh, ama, cum, xmat,
     &ymat, bb, cc, dd, ee, ff, xc, dh, slope, sinter, top, bot, ahint

      integer r1, r2, r3, r4, c1, c2, c3, c4
      integer size

cdas      open (unit=10, file='logfit.out', status='unknown')
cdas      open (unit=13, file='logfit.dat', status='old')

cdas      read (unit=13, fmt=*) int
cdas      read (unit=13, fmt=*) (b(i), ajunk, h(i), i=1, int)
cdas      b (int+1) = ajunk

!mkd
      pi = 3.14159265359
          int = 3     ! HARDWIRE - always two mesh sizes (yielding three regions)
	    b(1) = 0.1
	    b(2) = 74.0
	    b(3) = 287.0  ! HARDWIRES - for 200 and 50 mesh - (#100 is 0.147)
	    b(4) = 400.0

	    h(1) = 70.0 ! HARDWIRE since used for initial guesses to make binned dist
	    h(2) = 25.0 ! 
	    h(3) = 5.0! 


c-----the program is divided into three sections
c     determination of interval midpoints, determination of lognormal
c     pdf parameters, and finally discretization of either the
c     lognormal pdf or the histogram directly.

c-----the value n sets the number of integration intervals used 
c     in the subroutine simp, tota utilizes arithmetic averaging
c     totg utilizes geometric averaging.

      n = 100
      tota = 0.
      totg = 0.

c-----compute geometric and arithmetic midpoints

      do 10 i = 1, int
         gmp(i) = sqrt(b(i) * b(i+1))
         gmh(i) = h(i)
         totg = totg + gmh(i)
         gmpt(i) = gmp(i)
         amp(i) = (b(i) + b(i + 1)) / 2.
         ama(i) = h(i)
         tota = tota + ama(i)
         cum(1,i) = b(i + 1)
         cum(2,i) = tota
 10   continue

      ad1 = 0.
      ad2 = 0.
      ad3 = 0.
      ad4 = 0.
      am1 = 0.
      am2 = 0.
      ap2 = 0.

c-----freq(i) is the normalized frequency for each interval
c     cum(2,i) contains the cummulative distribution
c     NOTE:  This program was rewritten many times consequently there is 
c     some redundancy in variables.

      do 11 i = 1, int
         freq(i) = (gmh(i) / totg) / (b(i + 1) - b(i))
         ad1 = ad1 + ama(i)*amp(i)
         ad2 = ad2 + ama(i)*(amp(i)**2)
         ad3 = ad3 + ama(i)*(amp(i)**3)
         ad4 = ad4 + ama(i)*(amp(i)**4)
         am1 = am1 + ((amp(i) * ama(i)) / tota)
         cum(2,i) = cum(2,i)/tota
 11   continue

      do 12 i = 1, int
         am2 = am2 + (ama(i) * (amp(i) **2.))
         ap2 = ap2 + (ama(i) * amp(i))
 12   continue

      kount = 0
      resid = 10.
      epsi = .0001
      oldtest = 1000000.

!mkd      do while (resid .ge. epsi)
!mkd         d1 = 0.
!mkd         d2 = 0.
!mkd         d3 = 0.
!mkd         d4 = 0.
!mkd         m1 = 0.
!mkd         m2 = 0.
!mkd         p2 = 0.
!mkd         test = 0.
!mkd         gf = 0.

c-----this section determines the average of the distribution

!mkd         do 20 i = 1, int
!mkd            d1 = d1 + gmh(i)*gmp(i)
!mkd            d2 = d2 + gmh(i)*(gmp(i)**2)
!mkd            d3 = d3 + gmh(i)*(gmp(i)**3)
!mkd            d4 = d4 + gmh(i)*(gmp(i)**4)
!mkd            m1 = m1 + ((gmp(i) * gmh(i)) / totg)
!mkd 20      continue

c-----this section determines the standard deviation

!mkd         do 21 i = 1, int
!mkd            m2 = m2 + (gmh(i) * (gmp(i)**2.))
!mkd            p2 = p2 + (gmh(i) * gmp(i))
!mkd 21      continue

!mkd         sg = sqrt((m2 - ((p2 ** 2.) / totg)) / (totg * 0.99))

c-----This section transforms the distribution mean and standard
c     deviation into values consistent with the log normal pdf
c     which assumes  y=log(x) and optimizes the curve fit.

!mkd         sig = sqrt (dlog (((sg **2.) / (m1 **2.)) +1))
!mkd         mean = dlog(m1) - (0.5 * (sig **2.))

!mkd         do 30 i = 1, int
!mkd            call simp2 (b(i), b(i + 1), mean, sig, n, ans1(i))
!mkd            call simp1 (b(i), b(i + 1), mean, sig, n, ans2(i))
!mkd            test = test + ((((gmp(i) - (ans1(i) / ans2(i))) ** 2.)
!mkd     &             / ans1(i)) * ans2(i))
!mkd            gf = gf + ((((gmpt(i) - (ans1(i) / ans2(i))) ** 2.)
!mkd     &           / ans1(i)) * ans2(i))
!mkd 30      continue

!mkd         resid = abs(test - oldtest)
!mkd         if ( test .ge. oldtest) then
c            write(unit=*, fmt=*) 'test is > oldtest'
!mkd            resid = .000001
!mkd         end if

!mkd         if (kount .gt. 100) resid = .000001
!mkd         oldtest = test
!mkd         kount = kount + 1

!mkd         totg = 0
!mkd         do 40 i = 1, int
!mkd            gmp(i) = ans1(i) / ans2(i)
!mkd            gmh(i) = h(i)
!mkd            totg = totg + gmh(i)
!mkd 40      continue

!mkd      end do

c-----This section determines the standard deviation 
c-----coef. of skewness and coef. of kurtosis.

      m3 = 0
      m4 = 0

      do 41 i = 1, int
         m3 = m3 + (((gmh(i) * ((gmp(i) - m1) ** 2.)) *
     &        (gmp(i) - m1)) / totg)
         m4 = m4 + ((gmh(i) * (((gmp(i) - m1) ** 2.)
     &        ** 2.)) / totg)
 41   continue

      am2 = sqrt((am2 - ((ap2 ** 2.) / tota)) / (tota * 0.99))
      sk = m3 / (sg ** 3.)
      ku = (m4 / (sg ** 4.)) - 3.

c-----This section determines different diameters of the fitted distribution
c-----and histogram and degrees of freedom for goodness of fit test

      d10 = d1/totg
      d20 = (d2**0.5) / (totg**0.5)
      d30 = (d3**(1./3.)) / (totg**(1./3.))
      d32 = d3/d2
      d43 = d4/d3
      ad10 = ad1/tota
      ad20 = (ad2**0.5) / (tota**0.5)
      ad30 = (ad3**(1./3.)) / (tota**(1./3.))
      ad32 = ad3/ad2
      ad43 = ad4/ad3
      df = int - 3.

c-----OUTPUT OF INTITIAL RESULTS

cdas      write (unit=*, fmt=101)
cdas      read (unit=*, fmt=*) basis  

         basis = 1  ! HARDWIRE - mass - (DAS)

cdas      if (basis .eq. 2) then
cdas
cdas         write (unit=*, fmt=166)
cdas         write (unit=10, fmt=166)
cdas         write (unit=*, fmt=165) d10, ad10, d20, ad20, d30, ad30, 
cdas     &     d32, ad32, d43, ad43, sg, am2, sk, ku, mean, sig, gf, df
cdas         write (unit=10, fmt=165) d10, ad10, d20, ad20, d30, ad30, 
cdas     &     d32, ad32, d43, ad43, sg, am2, sk, ku, mean, sig, gf, df
cdas
cdas      elseif (basis .eq. 1) then
cdas
cdas         write (unit=*, fmt=166)
cdas         write (unit=10, fmt=166)
cdas         write (unit=*, fmt=265) d10, ad10,
cdas     &     sg, am2, sk, ku, mean, sig, gf, df
cdas         write (unit=10, fmt=265) d10, ad10,
cdas     &     sg, am2, sk, ku, mean, sig, gf, df
cdas
cdas      endif
cdas
cdas      write (unit=*, fmt=921)

cdas      read (unit=*, fmt=*) gof
cdas      if (gof .ne. 1) goto 33
c      endif ! if(.false.) then ! mkd

        gof = 1   ! HARDWIRE - must be 1 anyway so just hardwire (DAS)

c-----This part which is divided into two main sections 
c     discretizes the histogram or the lognormal pdf.
c     into user defined intervals.

 888  if (gof .eq. 1) then
cdas         write (unit=*, fmt=777)
cdas         read (unit=*, fmt=*) choice

        choice = 1   !HARDWIRE - use computed values for mean/sigma (DAS)

cdas         if (choice .eq. 0) then
cdas            write (unit=*, fmt=778)
cdas            read(unit=*, fmt=*) m1, sg
                sig = sqrt (dlog (((sg ** 2.) / (m1 ** 2.)) + 1))
                mean = dlog (m1) - (0.5 * (sig ** 2.))
cdas            write (unit=*, fmt=65) mean, sig
cdas            write (unit=10, fmt=65) mean, sig
cdas         end if
      end if

      if (gof .eq. 1) then

cdas         write (unit=*, fmt=922)
cdas         read (unit=*, fmt=*) m
cdas         write (unit=*, fmt=45) m
cdas         write (unit=10, fmt=45) m
cdas         write (unit=*, fmt=912)
cdas         read (unit=*, fmt=*) dch

         m   = size             ! HARDWIRE - # of intervals
         dch = 0                ! HARDWIRE - discretize by specifying area

         scale = 0.1
         const = 1. / sqrt(2. * pi)
         x(1) = b(1) * 0.00001
         
         if (dch .eq. 0) then

cdas            write (unit=*, fmt=911)
cdas            read (unit=*, fmt=*) eint

            eint = 0   ! HARDWIRE - intervals NOT equal areas

            am = m
            sum00 = 0

c-----This section determines the proper endpoints for each of the intervals.

            aleft = 1
            xc(1) = b(1)
            do 50 i = 1, m-1
               
               if (eint .ne. 1) then
cdas                  write (unit=*, fmt=913) i, aleft
cdas                  read (unit=*, fmt=*) area

                      area = passed_areas(i)   ! THIS COMES FROM C - (DAS) - array range 1->9
               else
                  area = 1 / am
               end if

               sum00 = sum00 + area
               aleft = 1 - sum00

c**discretize the histogram

               ahint(i) = area
               resid = 0.
               epsi = sum00
               j = 0
               do while (resid .lt. epsi)
                  j = j + 1
                  resid = cum(2,j)
               end do
               slope = (cum(1,j)-cum(1,j-1))/(cum(2,j)-cum(2,j-1))
               sinter = cum(1,j) - slope*cum(2,j)
               xc (i+1) = slope*sum00 + sinter
               jarray (i+1) = j

               if (i .eq. 1) then
                  resid = 0
                  epsi = 0.00001
                  j = 0
                  do while (resid .lt. epsi)
                     j = j+1
                     resid = h(j)
                  end do
                  xc(1) = b(j)
                  jarray(1) = j
               elseif (i .eq. m-1) then
                  resid =0
                  epsi = 0.00001
                  j = int

                  do while (resid .lt. epsi)
                     j = j-1
                     resid = h(j)
                  end do
                  
                  xc(m+1) = b(j+1)
                  jarray(m+1) = j
               endif

c**discretize the lognormal pdf

               x(i + 1) = b(int + 1)
               resid = 10.
               epsi = .000001
               do while (resid .ge. epsi)
                  call simp1 (x(i), x(i + 1), mean, sig, n, ans)
                  aint(i) = const * ans
                  diff = area - aint(i)
                  resid = abs(diff)
                  x(i + 1) = x(i + 1) + ((diff * x(i + 1)) * scale)
               end do
               freq(i) = area / (x(i + 1) - x(i))

 50      continue
      
         aint(m) = aleft
         ahint(m) = aleft
         freq(m) = 0.

      else

cdas         sum01 = 0
cdas
cdas         resid = 0
cdas         epsi = 0.00001
cdas         j = 0
cdas         do while (resid .lt. epsi)
cdas            j = j + 1
cdas            resid = h(j)
cdas         end do
cdas         xsm = b(j)
cdas
cdas         ahold = cum(2,j-1)
cdas
cdas         resid = 0
cdas         epsi = 0.00001
cdas         j = int
cdas         do while (resid .lt. epsi)
cdas            j = j-1
cdas            resid = h(j)
cdas         end do
cdas         xlg = b(j+1)
cdas
cdas         do 51 i = 1, m-1
cdas            write (unit=*, fmt=914) i, xsm, xlg
cdas            read (unit=*, fmt=*) x(i + 1)
cdas
cdasc**discretize the histogram
cdas
cdas            resid = 0.
cdas            epsi = x(i + 1)
cdas            j = 0
cdas            do while (resid .lt. epsi)
cdas               j = j+1
cdas               resid = b(j)
cdas            end do
cdas            xc(i+1) = x(i+1)
cdas            jarray(i+1) = j
cdas
cdas            slope = (cum(2,j) - cum(2,j-1)) / (cum(1,j)-cum(1,j-1))
cdas            sinter = cum(2,j) - slope*cum(1,j)
cdas            ahint(i) = slope*xc(i+1) + sinter
cdas            ahint(i) = ahint(i) - ahold
cdas            if(i .eq. 1) then
cdas               resid = 0
cdas               epsi = 1
cdas               j = 0
cdas               do while (resid .lt. epsi)
cdas                  j = j+1
cdas                  resid = h(j)
cdas               end do
cdas               xc(1) = b(j)
cdas               jarray(1) = j
cdas            elseif (i .eq. m-1) then
cdas               resid = 0
cdas               epsi = 1
cdas               j=int
cdas               do while (resid .lt. epsi)
cdas                  j = j-1
cdas                  resid = h(j)
cdas               end do
cdas               xc(m+1) = b(j+1)
cdas               jarray(m+1) = j
cdas            endif
cdas
cdas            ahold = ahold + ahint(i)
cdas
cdasc**discretize the lognormal
cdas
cdas            call simp1(x(i), x(i + 1), mean, sig, n, ans)
cdas            aint(i) = const * ans
cdas            freq(i) = aint(i) / (x(i + 1) - x(i))
cdas            sum01 = sum01 + aint(i)
cdas 51      continue
cdas
cdas         ahint(m) = 1 - ahold
cdas         aint(m) = 1 - sum01
cdas         freq(m) = 0.

      end if

c-----This section determines the expected values of 
c     each of the intervals found above

c**for histogram


cdas   WITH ONLY TWO mesh size points, there are not enough bins to discretize the histogram
cdas   comment this out to keep code from exceeded array bounds
cdas
cdas
cdas      bad = 0
cdas
cdas      do 61 i = 1, m
cdas
cdas         if (i .eq. 1) then
cdas            k = jarray(i+1) - jarray(i) + 1
cdas            jst = jarray(i)
cdas         elseif (i .eq. m) then
cdas            k = jarray(i+1) - jarray(i) + 1
cdas            jst = jarray(i) - 1
cdas         else
cdas            k = jarray(i+1) - jarray(i) + 2
cdas            jst = jarray(i) - 1
cdas         endif
cdas
cdas         if (k .ge. 3) then
cdas
cdas            l = 0
cdas            do 62 j = jst, jst+k
cdas               l = l+1
cdas               xmat(l,1) = 1.
cdas               xmat(l,2) = b(j+1)
cdas               xmat(l,3) = b(j+1)*b(j+1)
cdas               ymat(l,1) = (ama(j) / tota) / (b(j + 1) - b(j))
cdas 62         continue
cdas
cdas            call trans(k, 3, xmat, r1, c1, bb)
cdas            call mult(r1, c1, bb, k, 3, xmat, r2, c2, cc)
cdas            call mult(r1, c1, bb, k, 1, ymat, r3, c3, dd)
cdas            call invmat(3, cc, ff)
cdas            call mult(3, 3, ff, r3, c3, dd, r4, c4, ee)
cdas            
cdas            top = ee(1,1)*(xc(i+1)**2 - xc(i)**2)/2. +
cdas     &            ee(2,1)*(xc(i+1)**3 - xc(i)**3)/3. +
cdas     &            ee(3,1)*(xc(i+1)**4 - xc(i)**4)/4.
cdas            bot = ee(1,1)*(xc(i+1) - xc(i)) +
cdas     &            ee(2,1)*(xc(i+1)**2 - xc(i)**2)/2. +
cdas     &            ee(3,1)*(xc(i+1)**3 - xc(i)**3)/3.
cdas
cdas            dh(i) = top/bot
cdas
cdas         else
cdas
cdas            dh(i) = 0
cdas            write (unit=*, fmt=8503)
cdas            write (unit=10, fmt=8503)
cdas            bad = 1
cdas
cdas         endif
cdas
cdas 61   continue


c**For lognormal curve

      do 60 i = 1, m
         if (i .ne. m) then
            call simp2(x(i), x(i+1), mean, sig, n, ans)
            d(i) = (ans * const) * (1 / aint(i))
         else
            call simp2(b(1), x(i), mean, sig, n, ans)
            atot = exp(mean + ((sig **2.) / 2.))
            d(i) = (atot - (const * ans)) * (1 / aint(m))
         end if
 60   continue

c   **********************  final output  *************************

c**for histogram

cdas   SEE expanation above !!!!!!!!!!!!!
cdas 
cdas      if (bad .ne. 1) then
cdas
cdas         write (unit=*, fmt=8501)
cdas         write (unit=10, fmt=8501)
cdas         write (unit=*, fmt=8500)
cdas         write (unit=10, fmt=8500)
cdas
cdas         do 70 i = 1, m
cdas            write (*, 200) i, xc(i), xc(i + 1), dh(i), ahint(i)
cdas            write (10, 200) i, xc(i), xc(i + 1), dh(i), ahint(i)
cdas 70      continue
cdas      
cdas      endif


c      For lognormal curve fit

cdas      write (unit=*, fmt=8502)
cdas      write (unit=10, fmt=8502)
cdas      write (unit=*, fmt=8500)
cdas      write (unit=10, fmt=8500)

      i=1
cdas      write (unit=*, fmt=201) i, x(i), x(i + 1), d(i), aint(i)
cdas      write (unit=10, fmt=201) i, x(i), x(i + 1), d(i), aint(i)
cdas
cdas      do 71 i = 2, m-1
cdas         write (unit=*, fmt=200) i, x(i), x(i+1), d(i), aint(i)
cdas         write (unit=10, fmt=200) i, x(i), x(i+1), d(i), aint(i)
cdas 71   continue
cdas
cdas      write (unit=*, fmt=205) m, x(m), d(m), aint(m)
cdas      write (unit=10, fmt=205) m, x(m), d(m), aint(m)

c     load arrays from C  - (DAS)
      do kk=1,m
	   ps(kk)  = d(kk)
	   pmf(kk) = aint(kk)
	enddo

      end if  ! WHERE DOES THIS COME FROM? CAN'T FIND IT!


c-----modified to handle corrected and uncorrected counts.


c-----FORMAT STATEMENTS below this line

 101  format(/,' Input basis of histogram',//,
     &' 1 = based on mass distribution',/,
     &' 2 = based on number distribution',/)
 166  format(10x,'Properties of Lognormal Fit and Histogram',/)
 165  format(22x,'lognormal', 11x, 'histogram',//,
     &1x, 'd10', 19x, g11.5, 9x,
     &g11.5,/, 1x, 'd20', 19x, g11.5, 9x, g11.5, /, 1x, 'd30', 19x,
     &g11.5,
     &9x, g11.5,/,1x,'d32',19x,g11.5,9x,g11.5,
     &/,1x,'d43',19x,g11.5,9x,g11.5,
     &/,1x,'standard deviation',
     &4x,g11.5,9x,g11.5,/,1x,'coef. of skewness',
     &5x,g11.5,/,1x,'coef. of kurtosis',
     &5x,g11.5,/,1x,'log normal (mean)',
     &5x,g11.5,/,1x,'log normal (st dev)',
     &3x,g11.5,/,1x,'goodness of fit',
     &7x,g11.5,/,1x,'degrees of freedom',
     &4x,g11.5)
 265  format(22x,'lognormal',11x,'histogram',//,
     &1x,'mass mean',12x,g11.5,9x,
     &g11.5,/,1x,'standard deviation',
     &4x,g11.5,9x,g11.5,/,1x,'coef. of skewness',
     &5x,g11.5,/,1x,'coef. of kurtosis',
     &5x,g11.5,/,1x,'log normal (mean)',
     &5x,g11.5,/,1x,'log normal (st dev)',
     &3x,g11.5,/,1x,'goodness of fit',
     &7x,g11.5,/,1x,'degrees of freedom',
     &4x,g11.5)
 921  format(/,' do you want to discretize?',//,
     &' input 0 if no',/,' input 1 if yes',/)
 777  format(/,41h in order to discretize the lognormal pdf,/,
     &44h the transformed mean and sigma are required,//,
     &43h input 0 to enter values for mean and sigma,/,
     &54h input 1 to use the computed values for mean and sigma,/)
 778  format(//,21h input mean and sigma,/)
 922  format(/,51h input number of desired intervals for discretizing,/)
 912  format(/,41h input 0 to discretize by specifying area,/,
     &45h input 1 to discretize by specifying endpoint,/)
 911  format(/,37h do you want intervals of equal area?,//,
     &14h input 0 if no,/,15h input 1 if yes,/)
 913  format(/,25h input area for interval ,i3,/,
     &28h the area must be less than ,g10.3/)
 914  format(/,30h input upperbound of interval ,i3,12h  in microns,/
     &/,' initial upperbound must exceed ', 2x,f5.0,3x,'microns',
     &/,'final upperbound can not exceed ', 2x,f5.0,3x,'microns',/)
 8500 format('interval',3x,'lower bound',3x,'upper bound',3x,
     &'expected value',3x,' area of',/,12x,' (microns) ',3x,
     &' (microns) ',3x,
     &'  (microns)  ',3x,'integration',/)
 8501 format(/,'------- discretization of the histogram ------',/)
 8502 format(/,'------- discretization of the lognormal curve -----',/)
 8503 format(/,
     &'ERROR:  too many intervals for histogram discretization',/)
 722  format(/,49h do you want to rediscretize a new lognormal pdf?,//,
     &14h input 0 if no,/,15h input 1 if yes,/)
 8002 format(/,'------ curve fit based on corrected counts ------',/)
 8003 format(/,'------ curve fit based on uncorrected counts -----',/)
 45   format(/,'The histogram and lognormal pdf are divided into',i3,3x
     &,'parts.',/,'The diameters for each interval are expected ',
     &'values.')
 65   format(20h transformed mean = ,g11.5,20htransformed sigma = 
     &,g11.5)
 200  format(4x,i2,6x,g11.5,3x,g11.5,5x,g11.5,6x,f6.4)
 201  format(4x,i2,2x,f11.5,7x,g11.5,5x,g11.5,6x,f6.4)
 205  format(4x,i2,6x,g11.5,3x,'infinity',8x,g11.5,6x,f6.4)

 33   return
      end
