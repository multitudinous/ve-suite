#ifndef GRID_H
#define GRID_H

#define MAXCELLS  100*100
#define MAXPOINTS 100*100
#define MAXTRIANGLES 100*100
#define USEINSIDE 0
#define TURBULENCE 1

#define NSEG1 50
#define NSEG2 4

#define XVAR 0
#define YVAR 1

extern void quicksort2( double *a, int *id, int number );

class Cell {

	public:
	
		int Npts, CellType, *Vid;
		double *X, *Y, Xc, Yc;
		
		/**********************************************************************************/
		// CONSTRUCTOR 1
		Cell( int npts, double *x, double *y, int celltype ){
			
			Npts = npts;
			CellType = celltype;
			Xc = Yc = 0.;
			X = new double[Npts];
			Y = new double[Npts];
			Vid = new int[Npts];
			for ( int i = 0; i < Npts; i ++ ) {
				X[i] = x[i];
				Y[i] = y[i];
				Xc += X[i];
				Yc += Y[i];
				}
			Xc /= (double)Npts;
			Yc /= (double)Npts;
			}
			
		/**********************************************************************************/
		// CONSTRUCTOR 2 (rectangular)
		Cell( Design *design, int i, int j, double dx, double dy, int celltype ) {
		
			double x0, x1, y0, y1;
			x0 = design->aa + (double)i * dx;
			x1 = x0 + (double)celltype * dx;
			y0 = -design->cc + (double)j * dy;
			y1 = y0 + (double)celltype * dy;
				
			Npts = 4;
			CellType = celltype;
			Xc = 0.5 * ( x0 + x1 );
			Yc = 0.5 * ( y0 + y1 );
			X = new double[Npts];
			Y = new double[Npts];
			Vid = new int[Npts];
			X[0] = x0;
			Y[0] = y0;
			X[1] = x1;
			Y[1] = y0;
			X[2] = x1;
			Y[2] = y1;
			X[3] = x0;
			Y[3] = y1;
			}
						
		/**********************************************************************************/
		// DESTRUCTOR
		~Cell(){
		
			delete X;
			delete Y;
			delete Vid;
			}
	};
	
/*******************************************************************************************/
/*******************************************************************************************/
	
class Mesh {

	public:
	
		/**********************************************************************************/
		// CONSTRUCTOR
		Mesh(){}
				
		/**********************************************************************************/
		// DESTRUCTOR
		~Mesh(){}
	};

/*******************************************************************************************/
/*******************************************************************************************/

class Pts {

	public:
	
		int *unique, *id, Npts;
		double *X, *Y;
		
		/**********************************************************************************/	
		// CONSTRUCTOR
		Pts( int npts ) {

			Npts = npts;
			id = new int[Npts];
			unique = new int[Npts];
			X = new double[Npts];
			Y = new double[Npts];
			// Assume every point is unique
			for ( int i = 0; i < Npts; i ++ )
				unique[i] = 1;
			}
					
		/**********************************************************************************/	
		// DESTRUCTOR
		~Pts() {

			delete unique;
			delete X;
			delete Y;
			delete id;
			}
	};

/*******************************************************************************************/
/*******************************************************************************************/

class idTriangle {

	public:
	
		int A, B, C;
		
		idTriangle( int A0, int B0, int C0 ) {
			A = A0;
			B = B0;
			C = C0;
			}
		~idTriangle(){}
	};
	
/*******************************************************************************************/
/*******************************************************************************************/

class Grid {

	public:
	
		Cell *cell1[MAXCELLS], *cell2[MAXCELLS], *cell3[MAXCELLS], *cell[3*MAXCELLS];
		int NRpts, NgridX, NgridY, Ncell[4], **iGrid, Ndiv, Ntri;
		idTriangle *Tri[MAXTRIANGLES];
		Pts *TriPts, *In, *Out;
		
		int *id_grid, npts_grid, ncells;
		double *x_grid, *y_grid;
		
		/**********************************************************************************/
		// CONSTRUCTOR
		Grid( Design *design, AirfoilBoundary *airfoilB, int NgridX0, int NgridY0 ) {
		 
			double x, y, x0, y0, x1, y1, dx, dy, distmin;
			int i, j, k1, k2, ijstep, cond[10];

			NgridX = NgridX0;
			NgridY = NgridY0;
						
			iGrid = new int *[NgridX];
			for ( i = 0; i < NgridX; i ++ )
				iGrid[i] = new int[NgridY];
			for ( i = 0; i < NgridX; i ++ )
				for ( j = 0; j < NgridY; j ++ )
					iGrid[i][j] = 0;
					
						
			/********************************************/
			/* Take care of rectangular regions of grid */
			/********************************************/
			dx = (design->bb - design->aa) / (double)NgridX;
			dy = 2. * design->cc / (double)NgridY;
			for ( ijstep = 2; ijstep >= 1; ijstep -- ) {
				for ( i = 0; i < NgridX; i += ijstep )
					for ( j = 0; j < NgridY; j += ijstep ) {
						x0 = design->aa + (double)i * dx;
						x1 = x0 + (double)ijstep * dx;
						y0 = -design->cc + (double)j * dy;
						y1 = y0 + (double)ijstep * dy;

						if ( airfoilB->IsOutside( x0, y0 ) &&
							 airfoilB->IsOutside( x0, y1 ) &&
							 airfoilB->IsOutside( x1, y0 ) &&
							 airfoilB->IsOutside( x1, y1 ) ) {

							x = 0.5 * (double)(x0+x1);
							y = 0.5 * (double)(y0+y1);
							distmin = airfoilB->FindMinDistanceFrom( x, y );

							// cond[1] = distmin < design->c2 && distmin>design->c2 * 0.5;
							// cond[2] = distmin > design->c2;
							
							cond[1] = distmin>design->c2 * 0.5;
							cond[2] = 0;
							if ( cond[ijstep] ) {
								// Add this rectangular cell to grid
								for ( k1 = 0; k1 < ijstep; k1 ++ )
									for ( k2 = 0; k2 < ijstep; k2 ++ )
										iGrid[i+k1][j+k2] = ijstep;
								}
							}
						}
					}
					
			int c1, c2, cflag;
			// Make grid functional between ijstep=1 and ijstep=2
			for ( i = 0; i < NgridX; i += 2 )
				for ( j = 0; j < NgridY; j += 2 ) {
					// if any of the 4 cells are ijstep=1, then they all are
					c1 = c2 = 0;
					for ( k1 = 0; k1 < 2; k1 ++ )
						for ( k2 = 0; k2 < 2; k2 ++ ) {
							if ( iGrid[i+k1][j+k2] == 1 )
								c1 ++;
							if ( iGrid[i+k1][j+k2] == 2 )
								c2 ++;
							}
					cflag = 0;
					// if c1 && c2 both exist, replace all 4 with c1
					if ( c1 > 0 && c2 > 0 )
						cflag = 1;	
					// if c1 > 0 && c2 == 0, AND the empty cells are farther away
					// from the airfoil than the c1 cells, set all to 1
					if ( c1 > 0 && c2 == 0 ) {
						// if surrounding grid shows ijstep=2, then set cflag to 1
						if ( i == 0 || j == 0 || i == NgridX-2 || j == NgridY-2 )
							cflag = 1;
						if ( i > 0 && j > 0 && iGrid[i-2][j-2] == 2 )
							cflag = 1;
						if ( i < NgridX-2 && j > 0 && iGrid[i+2][j-2] == 2 )
							cflag = 1;
						if ( i < NgridX-2 && j < NgridY-2 && iGrid[i+2][j+2] == 2 )
							cflag = 1;
						if ( i > 0 && j < NgridY-2 && iGrid[i-2][j+2] == 2 )
							cflag = 1;
						}
					
					if ( cflag ) {
						for ( k1 = 0; k1 < 2; k1 ++ )
							for ( k2 = 0; k2 < 2; k2 ++ )
								iGrid[i+k1][j+k2] = 1;
						}
					}
					
			Ncell[1] = Ncell[2] = Ncell[3] = 0;
			//
			// Add ijstep=1 rectangular cells
			//
			for ( i = 0; i < NgridX; i ++ )
				for ( j = 0; j < NgridY; j ++ )
					if ( iGrid[i][j] == 1 )
						cell1[Ncell[1]++] = new Cell( design, i, j, dx, dy, 1 );

			//		
			// Add ijstep=2 rectangular cells
			//
			for ( i = 0; i < NgridX; i += 2 )
				for ( j = 0; j < NgridY; j += 2 )
					if ( iGrid[i][j] == 2 )
						cell2[Ncell[2]++] = new Cell( design, i, j, dx, dy, 2 );
						
			AssignInsidePoints( airfoilB->boundary );
			AssignOutsidePoints( design );

			// Generate points between the inside and ouside boundaries
			GenerateTriangularMeshPoints();
			// ConnectTriangularMeshPoints();
			SimpleConnectTriangularMeshPoints();
			
			// system( "rm STAR/*" );
			OutputGrid();
			// printf( "Total cells: %i\n", Ncell[1] + Ncell[2] + Ncell[3] );
			OutputStarFile( design );
			// RunStar();
			GetForces();
			}
			
		/**********************************************************************************/		
		// DESTRUCTOR
		~Grid() {
		
			int i;
			delete TriPts;
			delete In;
			delete Out;
			for ( i = 0; i < NgridX; i ++ )
				delete iGrid[i];
			delete iGrid;
			for ( i = 0; i < Ncell[1]; i ++ )
				delete cell1[i];
			for ( i = 0; i < Ncell[2]; i ++ )
				delete cell2[i];
			for ( i = 0; i < Ntri; i ++ )
				delete Tri[i];
				
			// Grid points
			delete id_grid;
			delete x_grid;
			delete y_grid;
			}
			
		/**********************************************************************************/		
		// UTILITY
		void AssignInsidePoints( Boundary *b ) {
		
			double x, y, x1, y1, x2, y2, dx, dy;
			int HowMany=0, i, j, nseg, ipts=0;
			
			// First, have to count how many
			for ( i = 0; i < b->npanels; i ++ ) {
				if ( i <= 4 )
					nseg = NSEG1;
				else
					nseg = NSEG2;
				HowMany += nseg;
				}
			
			// Allocate
			In = new Pts( HowMany );
			
			// Assign
			for ( i = 0; i < b->npanels; i ++ ) {
				x1 = b->panel[i]->X1;
				y1 = b->panel[i]->Y1;
				x2 = b->panel[i]->X2;
				y2 = b->panel[i]->Y2;
				if ( i <= 4 )
					nseg = NSEG1;
				else
					nseg = NSEG2;
				dx = ( x2 - x1 ) / (double)nseg;
				dy = ( y2 - y1 ) / (double)nseg;
				for ( j = 0; j < nseg; j ++ ) {
					x = x1 + (double)j * dx;
					y = y1 + (double)j * dy;
					In->X[ipts] = x;
					In->Y[ipts] = y;
					ipts ++;
					}
				}
			}
		
		/**********************************************************************************/		
		void AssignOutsidePoints( Design *d ) {
		
			double x1, y1, x2, y2, dx, dy;
			int HowMany=0, i, j, ipts=0;
			
			// First, have to count how many
			// Refer exclusively to iGrid
			for ( i = 0; i < NgridX; i ++ )
				for ( j = 0; j < NgridY; j ++ )
					if ( iGrid[i][j] == 0 ) {
						// May contain points.  Must be adjacent to 
						// a cell with iGrid[][] = 1
						if ( i == 0 || iGrid[i-1][j] == 1 ) // Left
							HowMany += 2;
						if ( i == NgridX-1 || iGrid[i+1][j] == 1 ) // Right
							HowMany += 2;
						if ( j == 0 || iGrid[i][j-1] == 1 ) // Bottom
							HowMany += 2;
						if ( j == NgridY-1 || iGrid[i][j+1] == 1 ) // Top
							HowMany += 2;
						}
			// Allocate
			Out = new Pts( HowMany );
			
			// Assign
			dx = (d->bb - d->aa) / (double)NgridX;
			dy = 2. * d->cc / (double)NgridY;
			for ( i = 0; i < NgridX; i ++ )
				for ( j = 0; j < NgridY; j ++ )
					if ( iGrid[i][j] == 0 ) {
						// May contain points.  Must be adjacent to 
						// a cell with iGrid[][] = 1
						if ( i == 0 || iGrid[i-1][j] == 1 ) { // Left
							x1 = d->aa + (double)i * dx;
							y1 = -d->cc + (double)j * dy;
							y2 = -d->cc + (double)(j+1) * dy;
							Out->X[ipts] = x1;
							Out->Y[ipts] = y1;
							ipts ++;
							Out->X[ipts] = x1;
							Out->Y[ipts] = y2;
							ipts ++;
							}
						if ( i == NgridX-1 || iGrid[i+1][j] == 1 ) { // Right
							x1 = d->aa + (double)(i+1) * dx;
							y1 = -d->cc + (double)j * dy;
							y2 = -d->cc + (double)(j+1) * dy;
							Out->X[ipts] = x1;
							Out->Y[ipts] = y1;
							ipts ++;
							Out->X[ipts] = x1;
							Out->Y[ipts] = y2;
							ipts ++;
							}
						if ( j == 0 || iGrid[i][j-1] == 1 ) { // Bottom
							x1 = d->aa + (double)i * dx;
							x2 = d->aa + (double)(i+1) * dx;
							y1 = -d->cc + (double)j * dy;
							Out->X[ipts] = x1;
							Out->Y[ipts] = y1;
							ipts ++;
							Out->X[ipts] = x2;
							Out->Y[ipts] = y1;
							ipts ++;
							}
						if ( j == NgridY-1 || iGrid[i][j+1] == 1 ) { // Top
							HowMany += 2;
							x1 = d->aa + (double)i * dx;
							x2 = d->aa + (double)(i+1) * dx;
							y1 = -d->cc + (double)(j+1) * dy;
							Out->X[ipts] = x1;
							Out->Y[ipts] = y1;
							ipts ++;
							Out->X[ipts] = x2;
							Out->Y[ipts] = y1;
							ipts ++;
							}
						}

			//
			// "Eliminate" redundant points
			//
			for ( j = 1; j < Out->Npts; j ++ )
				for ( i = 0; i < j; i ++ ) {
					if ( Out->X[i] == Out->X[j] && Out->Y[i] == Out->Y[j] )
						Out->unique[j] = 0;
					}
			}

		/**********************************************************************************/		
		void GenerateTriangularMeshPoints( void ) {
		
			double x1, y1, x2, y2, xx, yy, t; // , dx, dy;
			int i, j, k, npts;
			
			Ndiv = 8;
			TriPts = new Pts( CountPotentialTriangularMeshPoints() );
			// Generate points, which will later be connected

			npts = 0;
			// Create points from outside to inside
			for ( i = 0; i < Out->Npts; i ++ ) {
				if ( !Out->unique[i] )
					continue;
				x1 = Out->X[i];
				y1 = Out->Y[i];
				j = FindNearestBoundaryPoint( In, x1, y1 );
				x2 = In->X[j];
				y2 = In->Y[j];
				// dx = (x2 - x1) / (double)Ndiv;
				// dy = (y2 - y1) / (double)Ndiv;
				for ( k = 0; k <= Ndiv; k ++ ) {
					// xx = x1 + (double)k * dx;
					// yy = y1 + (double)k * dy;
					t = (double)k / (double)Ndiv;
					t = (1. - exp( -t * 2. )) / (1. - exp( -2.) );
					// double f = 0.999;
					double f = 1.0;
					xx = x1 + t * f * ( x2 - x1 );
					yy = y1 + t * f * ( y2 - y1 );
					TriPts->X[npts] = xx;
					TriPts->Y[npts] = yy;
					TriPts->id[npts] = j;
					npts ++;
					}
				}
			
			if ( USEINSIDE ) {
				// Create points from inside to outside
				for ( i = 0; i < In->Npts; i ++ ) {
					x1 = In->X[i];
					y1 = In->Y[i];
					j = FindNearestBoundaryPoint( Out, x1, y1 );
					x2 = Out->X[j];
					y2 = Out->Y[j];
					// dx = (x2 - x1) / (double)Ndiv;
					// dy = (y2 - y1) / (double)Ndiv;
					for ( j = 0; j <= Ndiv; j ++ ) {
						// xx = x1 + (double)j * dx;
						// yy = y1 + (double)j * dy;
						t = (double)k / (double)Ndiv;
						t = (1. - exp( -t * 2. )) / (1. - exp( -2.) );
						xx = x2 + t * ( x1 - x2 );
						yy = y2 + t * ( y1 - y2 );
						TriPts->X[npts] = xx;
						TriPts->Y[npts] = yy;
						npts ++;
						}
					}
				}
			}

		/**********************************************************************************/
		int FindNearestBoundaryPoint( Pts *p, double x, double y ) {
		
			double dx, dy, dist2, dist2min;
			int j, jmin;
			for ( j = 0; j < p->Npts; j ++ ) {
				if ( !p->unique[j] )
					continue;
				dx = p->X[j] - x;
				dy = p->Y[j] - y;
				dist2 = dx * dx + dy * dy;
				if ( j == 0 || dist2 < dist2min ) {
					dist2min = dist2;
					jmin = j;
					}
				}
			return jmin;
			}

		/**********************************************************************************/
		int CountPotentialTriangularMeshPoints( void ) {
			
			int i, npts=0;
			for ( i = 0; i < Out->Npts; i ++ ) {
				if ( !Out->unique[i] )
					continue;
				npts += (Ndiv+1);
				}
			if ( USEINSIDE ) {
				for ( i = 0; i < In->Npts; i ++ )
					npts += (Ndiv+1);
				}
			printf( "# triangular pts = %i\n", npts );
			return npts;
			}
			
		/**********************************************************************************/
		void SimpleConnectTriangularMeshPoints( void ) {
		
			// If Ndiv=5...
			// 0 1 2 3 4 5    <-  Line1
			// 6 7 8 9 10 11  <-  Line2
			// Triangle 1:  0 6 1
			// Triangle 2:  1 6 7
			// Triangle 3:  1 7 2
			// Triangle 4:  2 7 8

			double x[3], y[3], theta, *dvec, dy, dx;
			int i, ii, j, A, B, C, M, N, *id;

			N = Ndiv + 1;
			M = TriPts->Npts / N;
			// printf( "M, N, Npts: %i %i %i\n", M, N, TriPts->Npts );
			
			dvec = new double[M];
			id = new int[M];

			// Order Outside boundary based on Theta
			for ( i = 0; i < M; i ++ ) {
				A = i * N;
				// GetRThetaFromXY( TriPts->X[A], TriPts->Y[A], r, theta );
				// dvec[i] = theta;
				B = A + Ndiv;
				dx = TriPts->X[A] - TriPts->X[B];
				dy = TriPts->Y[A] - TriPts->Y[B];
				theta = atan2( dy, dx );
				dvec[i] = (double)TriPts->id[A] - 0.01 * theta;
				}
			quicksort2( dvec, id, M );
			
			for ( ii = 0; ii < M; ii ++ ) {
				i = id[ii];
				A = i * N;
				for ( j = 0; j < N-1; j ++ ) {
				
					// Triangle 1
					A = i * N + j;
					B = id[((ii+1)%M)] * N + j;
					C = A + 1;

					x[0] = TriPts->X[A];
					y[0] = TriPts->Y[A];
					x[1] = TriPts->X[B];
					y[1] = TriPts->Y[B];
					x[2] = TriPts->X[C];
					y[2] = TriPts->Y[C];
					cell3[Ncell[3]++] = new Cell( 3, x, y, 3 );
					
					if ( ! ( fabs( TriPts->X[C] - TriPts->X[B+1] ) < 0.0001 &&
						     fabs( TriPts->Y[C] - TriPts->Y[B+1] ) < 0.0001 ) ) {
						// Triangle 2
						A = A + 1;
						// B is the same
						C = B + 1;
				
						x[0] = TriPts->X[A];
						y[0] = TriPts->Y[A];
						x[1] = TriPts->X[B];
						y[1] = TriPts->Y[B];
						x[2] = TriPts->X[C];
						y[2] = TriPts->Y[C];						
						cell3[Ncell[3]++] = new Cell( 3, x, y, 3 );
						}
					}
				}
			
			delete dvec;
			delete id;
			}
			
		/**********************************************************************************/
		void ConnectTriangularMeshPoints( void ) {
			
			printf( "Connecting points!\n" );
			double x[3], y[3], x1, y1, x2, y2, dx, dy, *dvec, dist;
			int i, j, npts, *id;

			dvec = new double[TriPts->Npts];
			id = new int[TriPts->Npts];
			
			Ntri = 0;
			for ( i = 0; i < TriPts->Npts; i ++ ) {
				// Point A
				x1 = TriPts->X[i];
				y1 = TriPts->Y[i];
				npts = 0;
				for ( j = 0; j < TriPts->Npts; j ++ ) {
					if ( i == j )
						dvec[npts++] = 999999.; // unacceptable
					else {
						// Point B (candidate point)
						x2 = TriPts->X[j];
						y2 = TriPts->Y[j];
						dx = x2 - x1;
						dy = y2 - y1;
						dist = sqrt( dx * dx + dy * dy );
						dvec[npts++] = dist;
						}
					}
				// sort points around point A, 
				quicksort2( dvec, id, npts );
				// construct points B and C using nearest 2 pts
				if ( ( TriPts->X[id[0]] == TriPts->X[id[1]] ) &&
					 ( TriPts->Y[id[0]] == TriPts->Y[id[1]] ) ) {
					// Tri[Ntri++] = new idTriangle( i, id[0], id[2] );
					printf( "Woops\n" );
					}
				else if ( ( TriPts->X[id[1]] == TriPts->X[id[2]] ) &&
					      ( TriPts->Y[id[1]] == TriPts->Y[id[2]] ) )
					printf( "Damn\n" );
				else if ( ( TriPts->X[id[0]] == TriPts->X[id[2]] ) &&
					      ( TriPts->Y[id[0]] == TriPts->Y[id[2]] ) )
					printf( "Crap\n" );
				else
					Tri[Ntri++] = new idTriangle( i, id[0], id[1] );
				}

			delete dvec;				
			delete id;
					
			for ( i = 0; i < Ntri; i ++ ) {			
				printf( "%i %i %i\n", Tri[i]->A, Tri[i]->B, Tri[i]->C );
				x[0] = TriPts->X[Tri[i]->A];
				y[0] = TriPts->Y[Tri[i]->A];
				x[1] = TriPts->X[Tri[i]->B];
				y[1] = TriPts->Y[Tri[i]->B];
				x[2] = TriPts->X[Tri[i]->C];
				y[2] = TriPts->Y[Tri[i]->C];
				cell3[Ncell[3]++] = new Cell( 3, x, y, 3 );
				}				
			}
			
		/**********************************************************************************/
		void OutputGrid( void ) {
		
			printf( "Grid sent to 'grid.cell' and 'grid.vrt'\n" );
			
			// Do rectangular cell first
			int ijstep, i, j, k, n, npts_max, icell, nid, id2[4], qid[4];
			double xx, yy, dvec[4];
			FILE *fp;
			Cell *c;
			
			n = Ncell[1] + Ncell[2] + Ncell[3];
			npts_max = 4 * n;
			x_grid = new double[npts_max];
			y_grid = new double[npts_max];
			id_grid = new int[npts_max];
			npts_grid = nid = ncells = 0;
			
			//
			// Add all vertices and cells
			//
			for ( ijstep = 1; ijstep <= 3; ijstep ++ )
				for ( i = 0; i < Ncell[ijstep]; i ++ ) {
					switch( ijstep ) {
						case 1: c = cell1[i]; break;
						case 2: c = cell2[i]; break;
						case 3: c = cell3[i]; break;
						}
					for ( j = 0; j < c->Npts; j ++ ) {
						xx = c->X[j];
						yy = c->Y[j];
						// search for (xx,yy)
						for ( k = 0; k < npts_grid; k ++ ) {
							double tol = fabs( xx - x_grid[k] ) + fabs( yy - y_grid[k] );
							if ( tol < 0.000001 )
								break; // found it
							//if ( xx == x_grid[k] && yy == y_grid[k] )
							//	break; // found it
							}
						if ( k == npts_grid ) {
							// didn't find the point.  add it
							x_grid[npts_grid] = xx;
							y_grid[npts_grid] = yy;
							npts_grid ++;
							}
						c->Vid[j] = k + 1;
						id_grid[nid++] = k + 1;
						}
					cell[ncells++] = c;
					}
			
			// 1 2 3 4 5 6 7 8 (hexahedron)
			// 1 2 3 4 5 5 5 5 (pyramid)
			// 1 2 3 3 4 5 6 6 (wedge)
			// 1 2 3 3 4 4 4 4 (tetrahedron)
			
			k = icell = 0;
			fp = fopen( "STAR/grid.cel", "w" );
			for ( ijstep = 1; ijstep <= 3; ijstep ++ )
				for ( i = 0; i < Ncell[ijstep]; i ++ ) {
					icell ++;
					switch( ijstep ) {
						case 1: c = cell1[i]; break;
						case 2: c = cell2[i]; break;
						case 3: c = cell3[i]; break;
						}
						
			// doesn't work for some reason
			// for ( icell = 0; icell < ncells; icell ++ ) {				
			//		c = cell[icell];
					// I9, 6X, 9I9, 1X, I4
					fprintf( fp, "%9i", icell );
					fprintf( fp, "      " );
					//
					// order points clockwise around centroid
					//
					for ( j = 0; j < c->Npts; j ++ ) {
						id2[j] = id_grid[k++];
						int kk = id2[j] - 1;
						double dx = x_grid[kk] - c->Xc;
						double dy = y_grid[kk] - c->Yc;
						double theta = atan2( dy, dx );
						dvec[j] = theta;
						}
					quicksort2( dvec, qid, c->Npts );
					
					for ( j = 0; j < c->Npts; j ++ )
						fprintf( fp, "%9i", id2[qid[j]] );
					if ( c->Npts == 3 )
						fprintf( fp, "%9i", id2[qid[2]] );

					for ( j = 0; j < c->Npts; j ++ )
						fprintf( fp, "%9i", npts_grid + id2[qid[j]] );
					if ( c->Npts == 3 )
						fprintf( fp, "%9i", npts_grid + id2[qid[2]] );
					fprintf( fp, "%9i%4i\n", 1, 1 );
					}
			fclose( fp );

			fp = fopen( "STAR/grid.vrt", "w" );
			for ( i = 0; i < npts_grid; i ++ )
				fprintf( fp, "%9i      %16.9f%16.9f%16.9f\n",
						 i+1, x_grid[i], y_grid[i], 0. );
			for ( i = 0; i < npts_grid; i ++ )
				fprintf( fp, "%9i      %16.9f%16.9f%16.9f\n",
						 npts_grid+i+1, x_grid[i], y_grid[i], 1. );
			fclose( fp );
			}

	/**********************************************************************************/
	void ApplyBoundaryLocation( FILE *fp, char *in_out_str, int bloc, int x_or_y, double val ) {

		Cell *c;
		int ijstep, i, j, k, nloc, icell = 0, blocation[4];
		double dx, dy, dist, tol;
		for ( ijstep = 1; ijstep <= 3; ijstep ++ )
			for ( i = 0; i < Ncell[ijstep]; i ++ ) {
				icell ++;
				switch( ijstep ) {
					case 1: c = cell1[i]; break;
					case 2: c = cell2[i]; break;
					case 3: c = cell3[i]; break;
					}
				nloc = 0;
				// check if point is on boundary
				// if ( bloc == 3 && x_or_y == -1 ) {
				if ( bloc == 4 ) {
					// airfoil
					// printf( "In->Npts = %i %g %g  %g %g\n", In->Npts, 
					// 		In->X[0], In->Y[0], c->X[0], c->Y[0] );
					tol = 0.000001;
					for ( j = 0; j < c->Npts; j ++ ) {
						for ( k = 0; k < In->Npts; k ++ ) {
							dx = In->X[k] - c->X[j];
							dy = In->Y[k] - c->Y[j];
							dist = sqrt( dx * dx + dy * dy );
							if ( dist < tol )
								blocation[nloc++] = j;
							}
						}
					}
				else {
					if ( x_or_y == XVAR ) {
						for ( j = 0; j < c->Npts; j ++ )
							if ( c->X[j] == val )
								blocation[nloc++] = j;
						}
					else {
						for ( j = 0; j < c->Npts; j ++ )
							if ( c->Y[j] == val )
								blocation[nloc++] = j;
						}
					}
							
				if ( nloc == 2 ) {
					// printf( "%s boundary location found!\n", in_out_str );
					fprintf( fp, "bdef %i", bloc );
					for ( k = 0; k < nloc; k ++ ) {
						j = blocation[k];
						fprintf( fp, " %i", c->Vid[j] );
						}
					for ( k = nloc-1; k >= 0; k -- ) {
						j = blocation[k];
						fprintf( fp, " %i", npts_grid + c->Vid[j] );
						}
					fprintf( fp, " 1\n" );
					}
				}
			}

	/**********************************************************************************/
	void OutputStarFile( Design *design ) {
		
		FILE *fp = fopen( "STAR/script.star", "w" );
		fprintf( fp, "#! /bin/tcsh\n" );
		fprintf( fp, "\n" );
		fprintf( fp, "echo \"*******************************************************\"\n" );
		fprintf( fp, "echo \"************        DEFINE PROBLEM         ************\"\n" );
		fprintf( fp, "echo \"*******************************************************\"\n" );
		fprintf( fp, "\n" );
		fprintf( fp, "rm *.echo *.mdl tmp.* star.*\n" );
		fprintf( fp, "prostar x << /\n" );
		fprintf( fp, "star\n" );
		fprintf( fp, "title\n\n" );
		fprintf( fp, "vread,grid.vrt,0,,,coded\n" );
		fprintf( fp, "cread,grid.cel,0,,,add,coded,0,0\n" );
		
		fprintf( fp, "cset all\n" );
		fprintf( fp, "cplot\n" );

		fprintf( fp, "!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n" 
				 	 "! APPLY BOUNDARY LOCATIONS !\n" 
				 	 "!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n" );
		// Boundary 1:  Inlet
		// Boundary 2:  Outlet
		// Boundary 3:  Airfoil
		// Boundary 4:  Z symmetry

		double xmax, xmin, ymax, ymin;
		xmin = design->aa;
		xmax = design->bb;
		ymin = -design->cc;
		ymax = design->cc;

		printf( "ncells = %i\n", ncells );
		ApplyBoundaryLocation( fp, "Inlet", 1, XVAR, xmin );
		ApplyBoundaryLocation( fp, "Outlet(1)", 2, XVAR, xmax );
		ApplyBoundaryLocation( fp, "Outlet(2)", 3, YVAR, ymin );
		ApplyBoundaryLocation( fp, "Outlet(3)", 3, YVAR, ymax );
		ApplyBoundaryLocation( fp, "Airfoil", 4, -1, -99999. );

		fprintf( fp, "view 0 0 1\n" );
		fprintf( fp, "bzon 5 all\n" );
		fprintf( fp, "view 0 0 -1\n" );
		fprintf( fp, "bzon 5 all\n\n" );

		// BOUNDARY CONDITIONS
		fprintf( fp, "rdef 1 inlet\n" );
		fprintf( fp, "10 0 0 1 0 1.205\n" );
		fprintf( fp, "rdef 2 outlet\n" );
		fprintf( fp, "split 1.0\n" );
		fprintf( fp, "rdef 3 wall\n" );
		fprintf( fp, "noslip\n" );
		fprintf( fp, "0 0 0 1 0\n" );
		fprintf( fp, "rdef 4 wall\n" );
		fprintf( fp, "noslip\n" );
		fprintf( fp, "0 0 0 1 0\n" );
		fprintf( fp, "rdef 5 symp\n" );

		// apply turbulence
		if ( TURBULENCE ) {
			fprintf( fp, "turb,ke,1,stan\n" );
			fprintf( fp, "lowre,off\n" );
			fprintf( fp, "coke,0.09,1.44,1.92,1.44,-0.33,0.419,1,1.219,0.9,,,\n" );
			}
		fprintf( fp, "iter 500\n" );
//		fprintf( fp, "wdat rest 10\n" );
		fprintf( fp, "wdata,restart,10,0,star.pst\n" );
		fprintf( fp, "powall,y,n,n\n" );
		fprintf( fp, "prfield,all,,,user\n" );
		// fprintf( fp, "prwall,y,n,n\n" );
		// fprintf( fp, "cpma,all,,,1\n" );
		fprintf( fp, "geom\n\n" );
		fprintf( fp, "prob\n\n" );
		fprintf( fp, "quit\ny\n/\n\n\n" );
		fprintf( fp, "echo \"****************************************************\"\n" );
		fprintf( fp, "echo \"************  LINKING AND RUNNING STAR  ************\"\n" );
		fprintf( fp, "echo \"****************************************************\"\n\n" );
		fprintf( fp, "starlink << /\n" );
		fprintf( fp, "star\n" );
		fprintf( fp, "s\n" );
		fprintf( fp, "y\n" );
		fprintf( fp, "n\n" );
		fprintf( fp, "/\n\n" );

// fprintf( fp, "/\n\ny\n/\n\n" );
// fprintf( fp, "star > iter.usr << /\n" );

		fprintf( fp, "star << /\n" );
		fprintf( fp, "star\n/\n\n" );
		
//					 "getw,sftx,p,rela\n"
//					 "summ cset\n"
//					 "getw,fty,p,rela\n"
		
		fprintf( fp, "prostar x << /\n"
					 "star\n"
					 "y\n"
					 "y\n"
					 "load,star.pst,nomvgridload\n"
					 "getw,ftot,p,rela\n"
					 "bset news region 4\n"
					 "vset news bset\n"
					 "cset news vset any\n"
					 "summ cset\n"
					 "savu,star.usr,vector,coded\n"
					 "close star.usr\n"
					 "quit nosa\n"
					 "/\n" );

		fclose( fp );

		printf( "Script file generated.\n" );
		}

	/**********************************************************************************/
	void RunStar( void ) {
	
		// system( "rm STAR/*.echo STAR/tmp.*" );
		system( "chmod 777 STAR/script.star" );
		chdir( "STAR" );
		system( "dir" );
		system( "script.star" ); // > /dev/null" );
		chdir( ".." );
		}
		
	/**********************************************************************************/
	void GetForces( void ) {

		double fx, fy, fz, Lift=0., Drag=0.;
		int i, j, npat=0;
		FILE *fp = fopen( "STAR/star.usr", "r" );
		do {
			j = fscanf( fp, "%i%lf%lf%lf", &i, &fx, &fy, &fz );
			if ( j == 4 ) {
				Drag += fx;
				Lift += fy;
				npat ++;
				}
			}
		while ( !feof( fp ) );
		fclose( fp );
		printf( "# patterns: %i\n", npat );
		printf( "Lift: %g\n", Lift );
		printf( "Drag: %g\n", Drag );
		}
			
	/***********/
	/* PRIVATE */
	/***********/
	private:
		
		void GetRThetaFromXY( double x, double y, double &r, double &theta ) {	

			theta = atan2( y, x ); // In radians
			double stheta = sin( theta );
			if ( stheta != 0.0 )
				r = y / stheta;
			else
				r = x / cos( theta );
			}
	};

#endif
