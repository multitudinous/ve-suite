//works right now for the INEL Tecplot data
//needs to be compiled with main.cpp since this code needs to be integrated with 
//translateToVtk.cpp

#include "tecplotReader.h"

tecplotReader::tecplotReader( )
{
   uGrid = NULL;
   pts = NULL;
   parameterData = NULL;
   numCells = 0;
   x = NULL;
   y = NULL;
   u = NULL;
   v = NULL;
   w = NULL;
   measurement = NULL;
   absVel = NULL;
   numVertices = 0;
}
tecplotReader::~tecplotReader()
{
   fileI.close();
   delete [] x; x = NULL;
   delete [] y; y = NULL;
   delete [] u; u = NULL;
   delete [] v; v = NULL;
   delete [] w; w = NULL;
   delete [] measurement; measurement = NULL;
   delete [] absVel; absVel = NULL;
}
void tecplotReader::allocateVariables()
{
   numCells = (nX-1)*(nY-1);
   numVertices = nX*nY;
   x = new double [nX*nY];
   y = new double [nX*nY];
   u = new double [nX*nY];
   v = new double [nX*nY];
   w = new double [nX*nY];
   measurement = new double [nX*nY];
   absVel = new double [nX*nY];
   parameterData = new vtkFloatArray* [ 3 ]; //the number of parameters is 3 ( <U>, m, |U| )
   for ( int i=0;i<3;i++ )
   {      
      parameterData[ i ] = vtkFloatArray::New();
   }   
   parameterData[ 0 ]->SetNumberOfComponents( 3 ); //velocity vector
   parameterData[ 0 ]->SetNumberOfTuples( 2*nX*nY );
   parameterData[ 0 ]->SetName( "Velocity" );
   parameterData[ 1 ]->SetNumberOfComponents( 1 ); //measurement scalar
   parameterData[ 1 ]->SetNumberOfTuples( 2*nX*nY );
   parameterData[ 1 ]->SetName( "Measurement" );
   parameterData[ 2 ]->SetNumberOfComponents( 1 ); //absolute velocity scalar
   parameterData[ 2 ]->SetNumberOfTuples( 2*nX*nY );   
   parameterData[ 2 ]->SetName( "Absolute Velocity" );
}
vtkUnstructuredGrid* tecplotReader::tecplotToVTK( char* inFileName, int debug )
{
   int numChars;
   numChars = 0;
   //check existence of file
   fileI.open( inFileName );
   if ( fileI == NULL )
   {
      std::cout<<"Input file does not exist :"<<std::endl;
      return uGrid;
   }
   else  //file exists
   {
      do
      {
         numChars++;
         if ( debug ) std::cout<<(char)fileI.peek();
         fileI.get();
      } while ( (fileI.peek()!=0) && ((char)fileI.peek() != ')') );
      std::cout<<std::endl;
      if ( debug ) std::cout<<"Number of characters :"<<numChars<<std::endl;
      fileI.seekg( 0, std::ios::beg );      //reset file pointer
      //read the first 3 lines from the files
      char line[ 256 ];
      for ( int i=0;i<3;i++ )
      {
         fileI.getline( line, 256 );
         if ( debug ) std::cout<<line<<std::endl;
      }
      //search for "I=" in line
      char* pch;
      pch = strstr( line, "I=" ); pch = pch+2; nX = atoi(pch);
      if ( debug ) std::cout<<"nX :"<< nX <<std::endl;
      //search for "J=" in line
      pch = strstr( line, "J=" ); pch = pch+2; nY = atoi(pch);
      if ( debug ) std::cout<<"nY :"<< nY <<std::endl;
      //allocate memory since we know nX and nY now
      allocateVariables();
      std::cout<<"Input file        :"<<inFileName<<std::endl
               <<"Number of  cells  :"<<numCells<<std::endl
               <<"Number of Vertices:"<<nX*nY<<std::endl;   
      //reset file pointer to beginning to read in numbers
      fileI.seekg( 0, std::ios::beg );
      fileI.ignore( numChars+1 );        //ignore all characters from beginning until ")"
      for ( int j=0;j<nX*nY;j++ )
      {
         //std::cout<<j<<"\t";
         for ( int i=0;i<6;i++ )
         {
            fileI >>array[ i ];
            x[j] = array[ 0 ];
            y[j] = array[ 1 ];
            u[j] = array[ 2 ];
            v[j] = array[ 3 ];
            measurement[j] = array[ 4 ];
            absVel[j] = array[ 5] ;
            //std::cout<<array[ i ]<<"\t";
         }
         w[j] = 0.0;
         parameterData[ 0 ]->SetComponent( j, 0, u[j] );
         parameterData[ 0 ]->SetComponent( j+numVertices, 0, u[j] );
         parameterData[ 0 ]->SetComponent( j, 1, v[j] );
         parameterData[ 0 ]->SetComponent( j+numVertices, 1, v[j] );
         parameterData[ 0 ]->SetComponent( j, 2, w[j] );
         parameterData[ 0 ]->SetComponent( j+numVertices, 2, w[j] );
         parameterData[ 1 ]->SetComponent( j, 0, measurement[j] );
         parameterData[ 1 ]->SetComponent( j+numVertices, 0, measurement[j] );
         parameterData[ 2 ]->SetComponent( j, 0, absVel[j] );
         
         parameterData[ 2 ]->SetComponent( j+numVertices, 0, absVel[j] );
      }
      pts = vtkPoints::New();
      int* cPt = new int [ 8 ];  //HEX elements assumed, coz data is taken from PIV runs
      for ( int i=0;i<nX*nY;i++ )
      {
         pts->InsertPoint( i, x[i], y[i], 0.0 );
         pts->InsertPoint( i+numVertices, x[i], y[i], -100.0 );
      }
      uGrid = vtkUnstructuredGrid::New();
      uGrid->SetPoints( pts );
      int c=0;
      int alongX;
      while ( c<numCells )
      {
         for ( alongX=c;alongX<c+(nX-1);alongX++ )
         {
            //assign vertex numbers
            cPt[0] = alongX; cPt[1] = alongX+1; cPt[2] = alongX+nX+1; cPt[3] = alongX+nX;
            cPt[4] = alongX+numVertices; cPt[5] = alongX+1+numVertices; cPt[6] = alongX+nX+numVertices+1;
            cPt[7] = alongX+numVertices+nX;  
            uGrid->InsertNextCell( VTK_HEXAHEDRON, 8, cPt );
         }
         if ( debug ) std::cout<<"C :"<<c<<"\t"<<"I :"<<alongX<<std::endl;
         c = alongX + 1;
      }
      // Set selected scalar and vector quantities to be written to pointdata array
      letUsersAddParamsToField( 3, parameterData, uGrid->GetPointData() );
      for ( int i=0;i<3;i++ )
      {
         parameterData[ i ]->Delete();
      }
      delete [] parameterData;
      parameterData = NULL;
      uGrid->Delete();
      uGrid = NULL;
      pts->Delete();      
      pts = NULL;
      
      return uGrid;
   }
}

