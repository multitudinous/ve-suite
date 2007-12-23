#include "H/stove.h"
#include "starReaderBaff.h"
#include <ves/xplorer/util/readWriteVtkThings.h>
#include <ves/xplorer/util/cfdAccessoryFunctions.h>
#include <ves/xplorer/util/cleanVtk.h>
//#include <pthread.h>
//#include <sys/types.h>
//#include <unistd.h>
#include <cstdlib>
#include <vtkDataSet.h>
#include <vtkPointData.h>
#include <vtkContourFilter.h>
#include <vtkPolyData.h>
#include <vtkPolyDataNormals.h>
#include <vtkGeometryFilter.h>
#include <vtkFloatArray.h>
#include <vtkTriangleFilter.h>
#include <vtkSTLWriter.h>
#include <vtkPointSet.h>
#include <vtkStructuredGrid.h>
#include <vtkUnstructuredGrid.h>
#include <vtkTransform.h>
#include <vtkTransformFilter.h>
#include <vtkDecimatePro.h>
#include <vtkSmoothPolyDataFilter.h>

char Stove::filename[30]= {"./STAR/baff.cel"};
int  Stove::cell_type   = 3;	 
int  Stove::last_value  = 3;
int  Stove::last_cell   = 100000;	

using namespace std;


//****************/
//* CONSTRUCTOR  */
//*********************************/
//*********************************/
Stove::Stove(int baffCount0, int *params, int in, int out){
   baffCount = baffCount0;  
   for( int i = 0; i < baffCount; i ++ )
	   bafflelist.push_back(new Baffle(params,i));

   numOperators = 5;
      inlet  = new InletOutlet( 265, 257, 33, 8, 0,  9, 9, 9, 19969, 23047);
      outlet = new InletOutlet( 19405, 16897, 25, 0, 0, 25, 9, 9, 19905, 22966);

}

//**************/
//* DESTRUCTOR */
//*********************************/
Stove::~Stove() {
	for ( int i = 0; i < bafflelist.size(); i ++ )
		delete bafflelist[i];
	bafflelist.clear();
	fhist.clear();
   
   if ( in_out ) {
      delete inlet;
      delete outlet;
   }
}
//***************************************************************************/
//***************************************************************************/
//***************************************************************************/
void Stove::Copy(Stove *stoveCopy){
  baffCount=stoveCopy->baffCount;
  fitness1=stoveCopy->fitness1;
  fitness2=stoveCopy->fitness2;
  numOperators = stoveCopy->numOperators;

   if ( in_out ) {
      delete inlet;
	   delete outlet;
	   inlet  = new InletOutlet( stoveCopy->inlet  );
      outlet = new InletOutlet( stoveCopy->outlet );
   }  
  for(int i=0;i<baffCount;i++)       
    for(int j=0;j<5;j++)
      bafflelist[i]->baff[j]=stoveCopy->bafflelist[i]->baff[j];          
}
/***************************************************************************/
void Stove::Copy(Stove *stoveCopy, int i0, int i1) {
   int i, j;
   baffCount = stoveCopy->baffCount;
   fitness1  = stoveCopy->fitness1;
   fitness2  = stoveCopy->fitness2;
   numOperators = stoveCopy->numOperators;
   
   if ( in_out ) {
      if( i0 == 0 && i1 == 1 ) {
	      delete outlet;
         outlet = new InletOutlet( stoveCopy->outlet );

      } else if( i0 == 0 && i1 == 2 ) {
		   delete inlet;
		   delete outlet;
		   inlet  = new InletOutlet( stoveCopy->inlet);
		   outlet = new InletOutlet(stoveCopy->outlet);

      } else if( i0 == 0 && i1 != numOperators) {
         delete inlet;
	      delete outlet;
         inlet  = new InletOutlet( stoveCopy->inlet);
         outlet = new InletOutlet(stoveCopy->outlet);

         i1 -= (numOperators - baffCount);
         for( i=i0;i<i1;i++)       
		      for( j=0;j<5;j++)
               bafflelist[i]->baff[j]=stoveCopy->bafflelist[i]->baff[j];

      } else if( i0 == 1 ) {
         delete inlet;
         inlet  = new InletOutlet( stoveCopy->inlet);
     
         for( i=0;i<baffCount;i++)       
            for( j=0;j<5;j++)
               bafflelist[i]->baff[j]=stoveCopy->bafflelist[i]->baff[j];
      
	   } else {
         i1 -= (numOperators - baffCount);
         i0 -= (numOperators - baffCount);

         for( i=i0;i<i1;i++)       
            for( j=0;j<5;j++)
               bafflelist[i]->baff[j]=stoveCopy->bafflelist[i]->baff[j];
      }
   } else { 
      for( i=i0;i<i1;i++)       
         for( j=0;j<5;j++)
            bafflelist[i]->baff[j]=stoveCopy->bafflelist[i]->baff[j];
   }
}

/***************************************************************************/
void Stove::baffleWrite(){
  int cell_number=last_cell+1;
  ofstream cwrite;
  
  cwrite.open(filename, ios::out);     // open star-CD file to be written
  cwrite.setf(ios::right);
  
  for(int k=0; k<(int)bafflelist.size(); k++){
    for(int j=0; j<(int)bafflelist[k]->baff[4]; j++){
      // increment start_vertex to next layer down in Z direction
      bafflelist[k]->findFirst();
      bafflelist[k]->start_vertex-=j*bafflelist[k]->z_move;
      
      for(int i=0; i<(int)bafflelist[k]->baff[3]; i++){
	      bafflelist[k]->defineVerts(bafflelist[k]->baff);
        // write baff.cel file
	      cwrite << setw(9) << cell_number 
	             << setw(15)<< bafflelist[k]->vertex[0]
	             << setw(9) << bafflelist[k]->vertex[1]
	             << setw(9) << bafflelist[k]->vertex[2]
	             << setw(9) << bafflelist[k]->vertex[3]
	             << setw(9) << bafflelist[k]->vertex[4]
	             << setw(9) << bafflelist[k]->vertex[5]
	             << setw(9) << bafflelist[k]->vertex[6]
	             << setw(9) << bafflelist[k]->vertex[7]
	             << setw(9) << cell_type
	             << setw(5) << last_value << endl;
	    
	      switch (bafflelist[k]->baff[2]) {
	         case 0: bafflelist[k]->start_vertex += bafflelist[k]->x_move;
		         break;
	         case 1: bafflelist[k]->start_vertex += bafflelist[k]->y_move;
		         break;
	         case 2: bafflelist[k]->start_vertex -= bafflelist[k]->x_move;
		         break;
	         case 3: bafflelist[k]->start_vertex -= bafflelist[k]->y_move;
		         break;
	      }
	      cell_number++;  		    
      }
    }
  }
  cwrite.close();
}
/*************************************************************************/  
void Stove::stoveEval(){
  double avg;
  long double summ;
  int i;
  int counter;
  
 // fitness = 0.;
 // for (int j=0;j<baffCount;j++)
 //    for (i=0;i<5;i++)  			// THIS IS A TEST FITNESS
 //	 fitness += t->baffles[j]->b[i];
 // fitness = 9999-fitness;
 // return;
  
 // baffleWrite();
  
  //cout << "creature evaluated" << endl;
  //do stuff to find fitness here
  FILE* usr = fopen( "./STAR/star.usr", "r" );// open temperature file star.usr
  // **************CHANGE FOR DIFFERENT STOVES*************** //
  if ( this->in_out )
      counter = 1636; //CHANGE FOR DIFFERENT STOVE DESIGNS!!!!!!!!!!!!!!
  else 
      counter = 1556; 
  // **************CHANGE FOR DIFFERENT STOVES*************** //

  double* tmpVals= new double[counter];
  double* cell= new double[counter];
  summ = 0.0;		      // read in good data, calculate average
  for (i=0;i<counter;i++) {
    fscanf( usr, "%lf%lf", &cell[i],&tmpVals[i] );
    summ += (long double)(tmpVals[i]);
    // printf( "%lf\n",tmpVals[i] );
  }
  fclose( usr );
  avg = double(summ/((long double)(counter)));
 
  fitnessArea(counter,avg,tmpVals);
  fitnessVarience(counter,avg,tmpVals);
  
  delete [] tmpVals;
  delete [] cell;
}

//************************************************************************/
void Stove::fitnessArea(int counter, double avg, double* tmpVals){ 
  fitness2=0.;  			       // calculate the fitness
  int goodCell=0;
  for (int i=0;i<counter;i++)  
    if((tmpVals[i]>(avg-75)) && (tmpVals[i]<(avg+75)))
      goodCell++;
  fitness2=(double)goodCell/(double)counter;  
}

//************************************************************************/
void Stove::fitnessVarience(int counter,double avg, double* tmpVals){ 
  fitness1=0;  			       // calculate the fitness
  for (int i=0;i<counter;i++)  {
     tmpVals[i] = tmpVals[i]-avg;
     fitness1 += tmpVals[i]*tmpVals[i];
  }
}
//************************************************************************/
// mutation type 1
void Stove::mutateNewBaff(){//GOOD: Checked May 9,2002
	int mutpoint;
	int *baff = new int[5];
	mutpoint = Rand()%baffCount;
    int which;
	if ( in_out )
      which = Rand()%2;

	if ( in_out ) {
	   if( which ){
		   do {
			   bafflelist[0]->RandDef(baff);
			   for(int j=0;j<5;j++) 
				   bafflelist[mutpoint]->baff[j]=baff[j];
			   //cout << " Mutate  1";
		   } while ( checkBaffles() );
		   delete [] baff;
	   } else
		   mutateInletOutlet( inlet );
   } else {
      do {
	      bafflelist[0]->RandDef(baff);
			   for(int j=0;j<5;j++) 
				   bafflelist[mutpoint]->baff[j]=baff[j];
			   //cout << " Mutate  1";
		   } while ( checkBaffles() );
		   delete [] baff;
   }
}
//************************************************************************/
// mutation type 2
void Stove::mutateBaff(){//GOOD: Checked May 9,2002
    int mutbaf,mutloc;
	int *baff = new int[5];
    int which;
	if ( in_out )
      which = Rand()%2;
	mutbaf = Rand()%baffCount;
    mutloc = Rand()%5;
   
   if ( in_out ) {
	   if( which ){
		   do {
			   bafflelist[0]->RandDef(baff);
			   bafflelist[mutbaf]->baff[mutloc]=baff[mutloc];
			   //cout << "mutate 2";
		   } while ( checkBaffles() );
		   delete [] baff;
	   } else 
	      mutateInletOutlet( outlet);
   } else {
		   do {
			   bafflelist[0]->RandDef(baff);
			   bafflelist[mutbaf]->baff[mutloc]=baff[mutloc];
			   //cout << "mutate 2";
		   } while ( checkBaffles() );
		   delete [] baff;
   }   
}

//************************************************************************/
bool Stove::checkBaffles(){
   bool x, y;
	static int test =0;
	 for(int i=0;i<(int)bafflelist.size();i++){
      x = bafflelist[i]->intoWall();
      
	  if( x )
         return x;
      bafflelist[i]->alongWall();
	  
	  /*y = doubleBaffle();
	  
	  if( y )
		  test += 1;
	  if( test > 20 ){
		  test = 0;
		  return 0;
	  }
		
	  if( y )
		  return y;*/
    }
   return 0;
}

//************************************************************************/
bool Stove::doubleBaffle( void ) {
    int i,j;
	for( i = 0;i < baffCount; i++)
		for( j = 0;j < baffCount; j++)
			switch (bafflelist[i]->baff[2]) {
				case 0: //x_move
					if ( bafflelist[i]->baff[1] == bafflelist[j]->baff[1]) //y_value
						if ( bafflelist[i]->baff[0] >= bafflelist[j]->baff[0]){
							if( bafflelist[i]->baff[0] <= bafflelist[j]->baff[0] + bafflelist[j]->baff[3])
								return 1;
						} else if ( bafflelist[i]->baff[0] <= bafflelist[j]->baff[0]) 
							if( bafflelist[i]->baff[0] + bafflelist[i]->baff[3] >= bafflelist[j]->baff[0] )
								return 1;						
    				break;
				case 1: //y_move
					if ( bafflelist[i]->baff[0] == bafflelist[j]->baff[0])//x_value
						if ( bafflelist[i]->baff[1] >= bafflelist[j]->baff[1]){
							if( bafflelist[i]->baff[1] <= bafflelist[j]->baff[1] + bafflelist[j]->baff[3])
								return 1;
						} else if ( bafflelist[i]->baff[1] <= bafflelist[j]->baff[1]) 
							if( bafflelist[i]->baff[1] + bafflelist[i]->baff[3] >= bafflelist[j]->baff[1] )
								return 1;						
    				break;
			}	
	return 0;
}

//************************************************************************/
void Stove::writeStove(FILE *log){
   if ( in_out ) 
    fprintf( log, "%lf %lf %i %i \n",fitness1, fitness2, 
               inlet->startPosition, outlet->startPosition); 
   else
      fprintf( log, "%lf %lf \n",fitness1, fitness2 ); 
    for(int i=0;i<(int)bafflelist.size();i++)
      bafflelist[i]->writeBaffle(log);
    fprintf( log, "\n"); 
}
//*************************************************************************/
// New stuff for vanilla GA
void Stove::GetCrossPoints( int &crosspt ) {
	crosspt = Rand()%(numOperators - 1) + 1;
 	}

//*************************************************************************/
void Stove::CrossoverK1( Stove *s1, Stove *s2, int icrosspt ) {
	Copy( s1, 0, icrosspt );
	Copy( s2, icrosspt, numOperators );
	}

//*************************************************************************/
void Stove::CrossoverK2( Stove *s1, Stove *s2, int icrosspt ) {
	Copy( s2, 0, icrosspt );
	Copy( s1, icrosspt, numOperators );
	}

//*************************************************************************/
double Stove::Distance( Stove *s_compare ) {
	double x1, x2, dx, dist=0.;
	int i, j;

	for ( i = 0; i < baffCount; i ++ ) {
		for ( j = 0; j < 5; j ++ ) {
			x1 = bafflelist[i]->baff[j];
			x2 = s_compare->bafflelist[i]->baff[j];
			dx = x2 - x1;
			dist += dx * dx;
			}
		}

	dist = sqrt( dist );
	return dist;
	}

//*******************************************/
void Stove::CompileAndExecu( int runnum )
{
	baffleWrite();
   fhist.clear();
   if ( this->in_out )
   {
	   system("rm -f ./STAR/star.exe ./STAR/tmp.* ./STAR/setup ./STAR/ABORT ./STAR/FITNESS");
      writeBoundaryFile();
   } 
   else	
   {
      system("rm -f ./STAR/star.exe ./STAR/tmp.* ./STAR/ABORT ./STAR/FITNESS");
   }
   ofstream compileFile;
   compileFile.open( "compileRun", ios::out);
   compileFile << "#!/bin/csh -f" << endl;
	compileFile << "./STAR/setup" << endl;
	//compileFile << "./STAR/execu &" << endl;
	compileFile.close();
   cout << "|--- Running Setup and Executable -------------|"<<endl;
	//system("chmod a+rwx ./STAR/setup && chmod a+rwx ./STAR/execu && chmod a+rwx compileRun && ./compileRun > /dev/null"); // update file permissions
   system("chmod a+rwx ./STAR/setup && chmod a+rwx compileRun && ./compileRun > /dev/null"); // update file permissions
    system( "loaderToVtk -singleFile star.param -loader star -o . -w file" );

 /*
   ofstream vtkFile;
   vtkFile.open( "createvtk", ios::out);
   vtkFile << "#!/bin/csh -f" << endl;
	vtkFile << "translateToVtk 2 star.param /" << endl;
   vtkFile << endl;
   vtkFile << "mergeVertices flowdata.vtk outFile" << runnum << ".vtk /" << endl;
   vtkFile << endl;
	vtkFile.close();
   cout << "|--- Creating VTK Files -------------|"<<endl;
   system( "chmod a+rwx createvtk && ./createvtk > /dev/null" );
*/
   //BuildBaffSTL( runnum );

}


void Stove::BuildBaffSTL( int runnum )
{
/*
   char infilename[100];
   sprintf( infilename, "baff.param" );
   unsigned int scaleIndex;

   starReader* baffRead = new starReader( infilename );
   baffRead->ReadParameterFile();
   baffRead->SetDebugLevel( 0 );

   scaleIndex = baffRead->GetScaleIndex();

   vtkUnstructuredGrid * pointset = NULL; 
   pointset = baffRead->GetUnsGrid();

   // The following is a time extensive computation that is better done here
   // rather than at runtime in the virtual environment
   double meanCellBBLength = VE_Util::cfdAccessoryFunctions::
                             ComputeMeanCellBBLength( pointset );

   vtkFloatArray * array = vtkFloatArray::New();
   array->SetName( "meanCellBBLength" );
   array->SetNumberOfComponents( 1 );
   array->SetNumberOfTuples( 1 );
   array->SetTuple1( 0, meanCellBBLength );
   pointset->GetFieldData()->AddArray( array );
   array->Delete();
   VE_Util::dumpVerticesNotUsedByCells( pointset );

   vtkGeometryFilter *gFilter = vtkGeometryFilter::New();
   gFilter->SetInput( pointset );
   gFilter->MergingOff();

   vtkTriangleFilter *tFilter = vtkTriangleFilter::New();
   tFilter->SetInput( gFilter->GetOutput() );
   
   vtkDecimatePro *deci = vtkDecimatePro::New();
      deci->SetInput( tFilter->GetOutput() );
      deci->SetTargetReduction( 0.01 );
      deci->PreserveTopologyOn();

   vtkSmoothPolyDataFilter *smoother = vtkSmoothPolyDataFilter::New();
      smoother->SetInput( deci->GetOutput() );
      smoother->SetNumberOfIterations( 0 );   //was set to one
   
   smoother->GetOutput()->Update();
   int numberOfArrays = smoother->GetOutput()->GetFieldData()->GetNumberOfArrays();   

   // remove the first array numberOfArrays times...
   for(int i= 0; i < numberOfArrays; i++)
   {
      smoother->GetOutput()->GetFieldData()->RemoveArray( 
                    smoother->GetOutput()->GetFieldData()->GetArrayName(0) );
   }
   smoother->GetOutput()->Update();

   // add normals and vertices (and corresponding scalar/vector data)
   vtkPolyDataNormals *cNormal = vtkPolyDataNormals::New();
      cNormal->SetInput( smoother->GetOutput() );
      cNormal->SetInput( tFilter->GetOutput() );
      cNormal->ConsistencyOn();  //Enforce consistent polygon ordering

   vtkPolyData * uGrid = vtkPolyData::New();
      cNormal->GetOutput()->Update();
      uGrid->ShallowCopy( cNormal->GetOutput() );

   char stlfilename [100];
   sprintf( stlfilename, "bafflegeom_%i.stl", runnum );
   vtkSTLWriter *writer = vtkSTLWriter::New();
      writer->SetInput( uGrid );
      writer->SetFileName( stlfilename );
      writer->SetFileTypeToBinary();
      writer->Write();
      writer->Delete();

   delete baffRead;
   array->Delete();
   tFilter->Delete();
   gFilter->Delete();
   deci->Delete();
   smoother->Delete();
   cNormal->Delete();
   uGrid->Delete();
*/
}

void *Stove::ExecuStarCD( void *args ) 
{
   cout << "Executing 1" << endl;
   system( "./STAR/execu > /dev/null" );
   cout << "Executing 2" << endl;
   //pthread_exit(0);
   return (NULL);
}

//*******************************************/
void Stove::GetFitness( int cfd_iter ) {
	double avg, stdv, fit, fit2, fit1; 
   int iter, ncell; 
	int ncount, count=0;
	static int old_iter=0;
	
   CheckForFitness( cfd_iter );

	do {
         FILE *fp = fopen( "./STAR/FITNESS", "r" );
		   fscanf( fp, "%i%i%i%lf%lf%lf%lf", &iter, &ncell, 
				   &ncount, &avg, &stdv, &fit, &fit2 );
		   fclose( fp );
      /******* ANN SPECFIC **************/
;
	   fit1 = fit;
      numCells = ncell;
      // fitness2 = fit2;
      //fit = 400. - stdv;
      fit = fit2;
		//if ( fit < 0. )
		//	fit = 0.;
    
		//fit = fit*fit;
		/********************************/
		if ( (fhist.size() == 0 || fhist[fhist.size() - 1] != fit || fit == 0.0) && old_iter != iter ) 
      {
			fhist.push_back( fit );
			count=0;
         old_iter = iter;
         fitness2 = fit2;
         fitness1 = fit1;
	      //system("rm -f ./STAR/FITNESS");
			return;
		}
			count ++;
         //Tk_Sleep( 500 );
         system( "sleep 0.3" );
         //system( "sleep 1" );
         //cout << count <<"count " <<endl;
			// After 15 seconds of no change in fitness, break;
			if ( count >= 15 )
  				return;

	} while ( 1 );
}

//*******************************************/
void Stove::KillStarCD( void ) {

    ostringstream abortPath;
    abortPath << "./STAR/ABORT";
    string abortPathString = abortPath.str();
    ofstream abortSTAR;
    abortSTAR.open( abortPathString.c_str(), ios::out);
    abortSTAR.close();
    system ("sleep 5");

    //std::cout << std::endl;
    fhist.clear();

}

//*******************************************/
void Stove::CheckForFitness( int cfd_iter ) {
   FILE *fp;
   int count = 0;
	while( 1 ) {
		if ( ( fp = fopen( "./STAR/FITNESS", "r" ) ) != NULL ) {
			fclose( fp );
			break;
			}
      //Put code here to make StarCD start again if problems with time 
      //server
      if( cfd_iter == 0 ) {
         system( "sleep 0.3" );
         count++;
         if( count == 10 )
         {
            //system( "./STAR/execu > /dev/null &" );
            count = 0;
                //pthread_t thread1;
               //cout << " check for fitness 1" << endl;
                //pthread_create( &thread1, NULL,  ExecuStarCD, NULL);	
                //system( "./STAR/execu > /dev/null &" );
               //cout << " check for fitness 2" << endl;
            /*count = 0;
            ppid = fork ();
            if (ppid < 0) 
            {
               cout << "|***Error due to fork() command!|" << endl;
               exit(0);
            }
            else if (ppid == 0) 
            {
               ExecuStarCD ();
               exit(0);
            }*/
         }
      }
	};
}


//*******************************************/
void Stove::GetDesignSpace( DoubleList designSpace ) {
	int i, j;
	designSpace.clear();
	for( i = 0; i < baffCount; i ++ )
		for( j = 0; j < 5; j ++ )
			designSpace.push_back( (double)bafflelist[i]->baff[j] );	
}
//************************************************************************/
void Stove::mutateInletOutlet( InletOutlet *stuff ){
   return; //Changed 2-25-04, we know where we want the inlet and outlet

   int which = Rand()%2;
   int x = stuff->possibilities.size();
   if( which ){
	   stuff->startPosition += (Rand()%stuff->range) + 1;
	   if( stuff->startPosition >= x )
		   stuff->startPosition -= x;
   } else {
	   stuff->startPosition -= (Rand()%stuff->range) + 1;
	   if( stuff->startPosition < 0 )
		   stuff->startPosition = x + stuff->startPosition;
   }
   //cout << "start position = "<<stuff->startPosition<<endl;
   stuff->evolveInletOutlet();
}

//************************************************************************/
void Stove::writeBoundaryFile( void ){
   int i, j, flag, bloc, flag2;
   float x0, x1, xs, y0, y1, ys, z0, z1, zs;

   FILE *fp = fopen( "./STAR/setup", "w" );   
   fprintf( fp, "#! /bin/csh -f\n" );
	fprintf( fp, "cd ./STAR/\n" );
   fprintf( fp, "source /usr/local/starcd3.26/etc/setstar\n" );
   fprintf( fp, "\n" );
	fprintf( fp, "proam << EOF\n" );
	fprintf( fp, "x\n" );
	fprintf( fp, "star\n" );
	fprintf( fp, "n\n" );
	fprintf( fp, "y\n\n" );	
   // Clearing old baffles
	fprintf( fp, "cset none\n" );
	fprintf( fp, "cset news baff\n" );
	fprintf( fp, "cdel cset\n" );

/*   // Clearing Stove Inlet and oulet
	fprintf( fp, "cset all\n" );
	fprintf( fp, "cset dele group 3\n" );
	fprintf( fp, "cdel cset\n" );

   // Compress cell numbers
	fprintf( fp, "cset all\n" );
	//fprintf( fp, "vdel 12944 15535 1\n" ); //15016 17607 cells 12289 14336
	//fprintf( fp, "vdel 27886 30639 1\n" ); //15016 17607 cells 12289 14336
   fprintf( fp, "vdel 21103 30639 1\n" ); //15016 17607 cells 12289 14336
	fprintf( fp, "vcom cset\n" );
	fprintf( fp, "y\n" );
	fprintf( fp, "ccom\n" );
	fprintf( fp, "y\n" );
	fprintf( fp, "cset all\n" );
   fprintf( fp, "ctyp,6\n" );
   fprintf( fp, "cmod cset\n" );

   fprintf( fp, "ctyp,1\n" );
   // Define New Inlets and Oulets
   x0 = outlet->possibilities[ outlet->startPosition ].neighbors[ 0 ].x;
   x1 = outlet->possibilities[ outlet->startPosition ].neighbors[ 0 ].x + ((outlet->width-1)*0.5);
   xs = outlet->width - 1;
   y0 = outlet->possibilities[ outlet->startPosition ].neighbors[ 0 ].y;
   y1 = outlet->possibilities[ outlet->startPosition ].neighbors[ 0 ].y + ((outlet->depth-1)*0.5);
   ys = outlet->depth - 1;
   z0 = outlet->possibilities[ outlet->startPosition ].neighbors[ 0 ].z;
   z1 = outlet->possibilities[ outlet->startPosition ].neighbors[ 0 ].z + ((outlet->height-1)*0.5);
   zs = outlet->height - 1;
	fprintf( fp, "vc3dgen,%g,%g,%g,%g,%g,%g,%g,%g,%g\n",
                  x0, x1, xs, y0, y1, ys, z0, z1, zs );
   x0 = inlet->possibilities[ inlet->startPosition ].neighbors[ 0 ].x; //
   x1 = inlet->possibilities[ inlet->startPosition ].neighbors[ 0 ].x + ((inlet->width-1)*0.5); //
   xs = inlet->width - 1;
   y0 = inlet->possibilities[ inlet->startPosition ].neighbors[ 0 ].y; //
   y1 = inlet->possibilities[ inlet->startPosition ].neighbors[ 0 ].y + ((inlet->depth-1)*0.5); //
   ys = inlet->depth - 1;
   z0 = inlet->possibilities[ inlet->startPosition ].neighbors[ 0 ].z - ((inlet->height-1)*0.5); //
   z1 = inlet->possibilities[ inlet->startPosition ].neighbors[ 0 ].z; //
   zs = inlet->height - 1;
	fprintf( fp, "vc3dgen,%g,%g,%g,%g,%g,%g,%g,%g,%g\n",
                  x0, x1, xs, y0, y1, ys, z0, z1, zs );
	fprintf( fp, "vmerge,all\n" );
	fprintf( fp, "c\n" );
	fprintf( fp, "vcom \n" );
	fprintf( fp, "/\n" );
	fprintf( fp, "y\n" );
*/
   // Getting Baffles
	fprintf( fp, "cread baff.cel 0 all add coded 0\n" );
	fprintf( fp, "close baff.cel\n" );
   // Checking For Double Baffles
	fprintf( fp, "check,all,0,dblc,newset,nolist\n" );
	fprintf( fp, "cdel cset\n" );
   // Boundary Regions
   // Inlet  Region = 1
   // Outlet Region = 2
   // StoveTop Surface Region = 3
   // Define Boundary Regions
/*
	fprintf( fp, "bset news regi wall\n" );
	fprintf( fp, "bset news wall\n" );
	fprintf( fp, "bdel bset\n" );
	fprintf( fp, "bcom\n" );
	fprintf( fp, "y\n" );
   //Define stove top surface boundary
	bloc = 3;
   outlet->usable.clear();

   for ( i = 0; i < outlet->surface.size(); i ++ ) {
      flag = flag2 = 0;
      int cellNumber = outlet->surface[i].cellNumber;
      for ( j =0; j < outlet->bottom.size(); j++ ) {
         if( (outlet->bottom[j].cellNumber-1 != cellNumber) && (flag != 1))
            flag = 0;
         else 
            flag = 1;
         if( (inlet->bottom[ j ].cellNumber + ( bafflelist[0]->x_cell_ct * 
                  bafflelist[0]->y_cell_ct * (bafflelist[0]->z_cell_ct - 1) ) != cellNumber) && (flag2 != 1))
            flag2 = 0;
         else 
            flag2 = 1;              
              
      }
      if( !flag ) {
         fprintf( fp, "bdef %i", bloc );
	      for ( j = 0; j < outlet->surface[i].neighbors.size(); j ++ )
		      fprintf( fp, " %i", outlet->surface[i].neighbors[j].vertexNumber );
         fprintf( fp, " \n" );
         if( !flag2 )
            outlet->usable.push_back( Cell( cellNumber ) );
      }
	}

   fprintf( fp, " \n" );
  /* //Define outlet boundaries
	bloc = 2;
	for ( i = 0; i < outlet->top.size(); i ++ ) {
      
      fprintf( fp, "bdef %i", bloc );
	   for ( j = 0; j < outlet->top[i].neighbors.size(); j ++ )
		   fprintf( fp, " %i", outlet->top[i].neighbors[j].vertexNumber );
      fprintf( fp, " \n" );
	}
   fprintf( fp, " \n" );

   //Define inlet boundaries
	bloc = 1;
	for ( i = 0; i < inlet->top.size(); i ++ ) {
      fprintf( fp, "bdef %i", bloc );
	   for ( j = 0; j < inlet->top[i].neighbors.size(); j ++ )
		   fprintf( fp, " %i", inlet->top[i].neighbors[j].vertexNumber );
      fprintf( fp, " \n" );
	}
   fprintf( fp, " \n" );
*/
   //Setup stove top for fitness evaluation
   //findUsableCells();
/*   fprintf( fp, "ctyp,10\n" );
   fprintf( fp, "cset none\n" );
   fprintf( fp, "cset news crange %i\n",outlet->usable[0].cellNumber );
   for ( i = 1; i < outlet->usable.size(); i ++ ) 
      fprintf( fp, "cset add crange %i\n",outlet->usable[i].cellNumber );
   fprintf( fp, "cmod cset\n" );
*/
   fprintf( fp, "pmat,1,fluid\n" );
   fprintf( fp, "moni,1\n" );
   fprintf( fp, "pres,1.e+05,1\n" );
   fprintf( fp, "tdatum,273\n" );
   // Running Analysis
   // fprintf( fp, "chec,all,0,righ,news,noli\n" );
   // fprintf( fp, "cflip,cset\n" );
   fprintf( fp, "ccom\n" );
   fprintf( fp, "yes\n" );
   fprintf( fp, "vcom\n" );
   fprintf( fp, "\n" );
   fprintf( fp, "yes\n" );
	fprintf( fp, "geom,star.geom,0.0254,binary,check,serial\n" );
	fprintf( fp, "prob,star.prob,binary\n" );
	fprintf( fp, "quit,save\n" );
	fprintf( fp, "EOF\n" );
	fprintf( fp, "\n" );
   fprintf( fp, "$STARDIR/bin/star\n" );
   fprintf( fp, "\n" );
	fprintf( fp, "proam << EOF\n" );
	fprintf( fp, "x\n" );
	fprintf( fp, "star\n" );
	fprintf( fp, "n\n" );
	fprintf( fp, "y\n\n" );
   fprintf( fp, "load,star.pst\n" );
	fprintf( fp, "psys,1\n" );
	fprintf( fp, "oper,getv,su,1\n" );
	fprintf( fp, "oper,getv,sv,2\n" );
	fprintf( fp, "oper,getv,sw,3\n" );
	fprintf( fp, "oper,getv,ptot,4,relative\n" );
	fprintf( fp, "oper,getv,t,5,absolute\n" );
   fprintf( fp, "savu,star.usr,all,coded,all\n" );
	fprintf( fp, "cdsa,star.cel,star.vrt,star.inp,-1\n" );
	fprintf( fp, "cset newset baff\n" );
	fprintf( fp, "vset newset cset\n" );
   fprintf( fp, "savu,baffpoly.usr,scalar,coded,vset\n" );
	fprintf( fp, "cwrite,baffpoly.cel,coded,cset\n" );
   fprintf( fp, "vwrite,baffpoly.vrt,coded,vset\n" );
   fprintf( fp, "quit,save\n" );
   fprintf( fp, "EOF\n" );

   fclose(fp);
	system( "chmod 770 ./STAR/setup" );
   // StarLink
   /*fp = fopen( "./STAR/strlnk", "w" );   
	fprintf( fp, "#! /bin/tcsh\n" );
	fprintf( fp, "\n" );
	fprintf( fp, "cd ./STAR/\n" );
	fprintf( fp, "starlink << EOF\n" );
	fprintf( fp, "star.exe\n" );
	fprintf( fp, "s\n" );
	fprintf( fp, "y\n" );
	fprintf( fp, "n\n" );
	fprintf( fp, "\n" );
	fprintf( fp, "EOF\n" );
   fclose(fp);
	system( "chmod 770 ./STAR/strlnk" );*/
}

/*******************************************/
int Stove::Rand( void ) {
	#ifdef _WIN32
		  return rand();
	#else
		  return lrand48();
	#endif	
}

