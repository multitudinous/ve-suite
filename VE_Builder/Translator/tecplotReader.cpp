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
   
   colsOfData = 0;
   numOfParameters = 0;
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
   parameterData = new vtkFloatArray* [ numOfParameters ]; //the number of parameters is 3 ( <U>, m, |U| )
   for ( int i=0;i<numOfParameters;i++ )
   {      
      parameterData[ i ] = vtkFloatArray::New();
      if ( i==0 )
      {
         parameterData[ i ]->SetNumberOfComponents( 3 ); //velocity vector
         parameterData[ i ]->SetNumberOfTuples( 2*nX*nY );
         parameterData[ i ]->SetName( "Velocity" );
      }
      else
      {
         parameterData[ i ]->SetNumberOfComponents( 1 ); //scalar data
         parameterData[ i ]->SetNumberOfTuples( 2*nX*nY );
      }
      if ( i==1 ) parameterData[ i ]->SetName( "Measurement" );
      else if ( i==2 ) parameterData[ i ]->SetName( "Absolute Velocity" );
   }
}
vtkUnstructuredGrid* tecplotReader::tecplotToVTK( char* inFileName, int debug )
{
   int numChars;
   numChars = 0;
   char tempChar; //temporary storage for character data
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
         tempChar = (char)fileI.peek();
         header+=tempChar;//update the header
         //if ( debug ) 
            //std::cout<<;
         fileI.get();
      } 
      while ( (fileI.peek()!=0) && ((char)fileI.peek() != ')') );
      
      std::cout<<std::endl;
      if ( debug )
      {
         std::cout<<"Number of characters :"<<numChars<<std::endl;
         std::cout<<header<<std::endl; //print header to screen
      }
      //search for "VARIABLES="
      I_Lower = header.find( "VARIABLES" );
      if ( debug ) std::cout<<"VARIABLES found at :"<<I_Lower<<std::endl;
      //search for "ZONE"
      I_Upper = header.find( "ZONE" );
      if ( debug ) std::cout<<"ZONE found at :"<<I_Upper<<std::endl;
      for ( int i=I_Lower;i<I_Upper;i++ ) tempString+=header[ i ];
      //std::cout<<tempString<<std::endl;
      //parse through tempString to find all the "," their count+1 = number of columns of data
      I = tempString.begin();
      for ( I=tempString.begin(); I!=tempString.end(); ++I )
      {
         if ( (*I) == ',' ) colsOfData++;
      }
      colsOfData = colsOfData + 1;
      if ( debug ) std::cout<<" Columns of Data :"<<colsOfData<<std::endl;
      fileI.seekg( 0, std::ios::beg );      //reset file pointer

      //read the first 3 lines from the files
      char line[ 256 ];
      for ( int i=0;i<3;i++ )
      {
         fileI.getline( line, 256 );
         if ( debug ) 
            std::cout<<line<<std::endl;
      }

      //search for "I=" in line
      char* pch;
      pch = strstr( line, "I=" ); pch = pch+2; nX = atoi(pch);
      //search for "J=" in line
      pch = strstr( line, "J=" );
      if ( pch !=NULL ) //if there is a "J=" then
      {
         pch = pch+2;
         nY = atoi(pch);
      }
      else //if there is no "J=", it is a square grid 
      {
         nY = (int)(sqrt( (double)(nX) )); //use the sqrt of current value of nX as nY
         nX = (int)(sqrt( (double)(nX) )); //reset nX to its new valu
      }
      if ( debug ) 
         std::cout<<"nX :"<< nX <<std::endl<<"nY :"<< nY <<std::endl;
      fileI.getline( line, 256 ); //file pointer gets 4th line now
      pch = strchr( line, ',' );
      //search for all the ","
      /*while ( pch != NULL )
      {
         if ( debug )
            std::cout<<" Location at :"<<pch-line+1<<std::endl;
         pch = strchr ( pch+1, ',' );
         colsOfData++;
      }
      colsOfData = colsOfData+1;*/
      //===========TEH WAY numOfParameters IS SET HAS TO BE CHANGED
      if ( colsOfData == 4 ) numOfParameters = 1;
      else if ( colsOfData == 5 ) numOfParameters = 2;
      else if ( colsOfData == 6 ) numOfParameters = 3;
      //===========
      array = new double [ colsOfData ];
      //allocate memory since we know nX and nY now
      allocateVariables();
      std::cout<<"Input file        :"<<inFileName<<std::endl
               <<"Number of  cells  :"<<numCells<<std::endl
               <<"Number of Vertices:"<<nX*nY<<std::endl;   
      //reset file pointer to beginning to read in numbers
      fileI.seekg( 0, std::ios::beg );
      fileI.ignore( numChars+1 );        //ignore all characters from beginning until ")"
      //set points
      pts = vtkPoints::New();
      int* cPt = new int [ 8 ];  //HEX elements assumed, coz data is taken from PIV runs
      for ( int j=0;j<nX*nY;j++ )
      {
         //std::cout<<j<<"\t";
         for ( int i=0;i<colsOfData;i++ )
         {
            fileI >>array[ i ];
            /*x[j] = array[ 0 ];
            y[j] = array[ 1 ];
            u[j] = array[ 2 ];
            v[j] = array[ 3 ];
            measurement[j] = array[ 4 ];
            absVel[j] = array[ 5];*/
            //std::cout<<array[ i ]<<"\t";
         }
         
         data.push_back( array );
         pts->InsertPoint( j, array[0], array[1], 0.0 );
         pts->InsertPoint( j+numVertices, array[0], array[1], -100.0 );
         //std::cout<<data[j][3]<<std::endl;
         w[j] = 0.0;
         for ( int i=2;i<numOfParameters;i++ )
         {
            parameterData[ i ]->SetComponent( j, 0, data[j][i] );
            parameterData[ i ]->SetComponent( j+numVertices, 0, data[j][i] );
         }
         //parameterData[ 0 ]->
      }
         /*parameterData[ 0 ]->SetComponent( j, 0, u[j] );
         parameterData[ 0 ]->SetComponent( j+numVertices, 0, u[j] );
         parameterData[ 0 ]->SetComponent( j, 1, v[j] );
         parameterData[ 0 ]->SetComponent( j+numVertices, 1, v[j] );
         parameterData[ 0 ]->SetComponent( j, 2, w[j] );
         parameterData[ 0 ]->SetComponent( j+numVertices, 2, w[j] );
         parameterData[ 1 ]->SetComponent( j, 0, measurement[j] );
         parameterData[ 1 ]->SetComponent( j+numVertices, 0, measurement[j] );
         parameterData[ 2 ]->SetComponent( j, 0, absVel[j] );         
         parameterData[ 2 ]->SetComponent( j+numVertices, 0, absVel[j] );         
      }*/
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
      letUsersAddParamsToField( numOfParameters, parameterData, uGrid->GetPointData() );
      vtkUnstructuredGrid *finalUGrid = vtkUnstructuredGrid::New();
      finalUGrid->DeepCopy( uGrid );
      for ( int i=0;i<numOfParameters;i++ )
      {
         parameterData[ i ]->Delete();
      }
      delete [] parameterData;
      parameterData = NULL;
      uGrid->Delete();
      uGrid = NULL;
      pts->Delete();      
      pts = NULL;
      data.clear();
      return finalUGrid;
   }
}

