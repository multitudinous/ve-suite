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
   parameterData = new vtkFloatArray* [ numOfParameters ]; //the number of parameters
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
      /////////////////////////////////////////////////////////////////////////////////////////////////////////
      //HEADER PARSING SECTION
      ////////////////////////////////////////////////////////////////////////////////////////////////////////
      I_Lower = header.find( "VARIABLES" );//search for "VARIABLES" in the header
      if ( debug ) std::cout<<"VARIABLES found at :"<<I_Lower<<std::endl;
      I_Upper = header.find( "ZONE" );//search for "ZONE" in the header
      if ( debug ) std::cout<<"ZONE found at :"<<I_Upper<<std::endl;
      //assign that portion of the header to tempString
      for ( unsigned int i=I_Lower;i<I_Upper;i++ ) tempString+=header[ i ];
      //std::cout<<tempString<<std::endl;
      I = tempString.begin();
      I_Lower = 0;
      for ( I=tempString.begin(); I!=tempString.end(); ++I )
      {
         I_Lower++;
         if ( (*I) == '"' )
         {
            colsOfData++;//search the quotation marks "
            locationQuotMarks.push_back( I_Lower );
         }
      }
      //the number of quotation marks is twice the number of variables in the data
      colsOfData = colsOfData/2;
      std::string varNamString;
      if ( debug ) std::cout<<" Columns of Data :"<<colsOfData<<std::endl;
      //SEARCH FOR THE VARIABLE NAMES NOW
      for ( vectorIntIterator=locationQuotMarks.begin();vectorIntIterator!=locationQuotMarks.end(); vectorIntIterator+=2 )
      {
         //std::cout<<(*vectorIntIterator)<<std::endl;
         I_Lower = ( *vectorIntIterator ); //std::cout<<"I lower :"<<I_Lower<<std::endl;
         I_Upper = *(vectorIntIterator+1); //std::cout<<"I Upper :"<<I_Upper<<std::endl;
         //between I_Lower and I_Upper copy from tempString into varNamString
         varNamString.erase();   //empty string before next iteration so that it does not hold cumulative values 
         for ( unsigned int c=I_Lower;c<I_Upper-1; c++ ) varNamString+=tempString[c];
         variablNames.push_back( varNamString );
      }
      if ( debug )
      {
         std::cout<<"Variable names are :"<<std::endl;
         for ( vectorStringIterator=variablNames.begin();vectorStringIterator!=variablNames.end();++vectorStringIterator ) 
            std::cout<<(*vectorStringIterator)<<std::endl;               
      }
      
      tempString.erase();  //clear the contents of tempString
      char stringData[ 10 ];  //create a new character array
      I_Lower = header.find( "I=" );   //look for I= in the header
      I_Upper = header.find( "J=" );   //look for J= in the header
      if ( ( I_Upper == std::string::npos ) && ( I_Lower == std::string::npos ) ) //if both I= and J= are not found
         std::cout<<"Wrong Tecplot file.!!!! Please check :"<<std::endl;
      if ( I_Lower != std::string::npos ) //make sure if I= is found
      {
         I_Lower = I_Lower + 2;
         if ( debug ) std::cout<<" Location of I= :"<<I_Lower<<std::endl;
         for ( int i=0;i<10; i++ ) stringData[ i ] = header[ I_Lower + i ];
         nX = atoi( stringData );
         if ( debug ) std::cout<<" nX :"<<nX<<std::endl;
      }
      if ( I_Upper != std::string::npos ) //see if J= was found
      {
         I_Upper = I_Upper + 2;
         if ( debug ) std::cout<<" Location of J= :"<<I_Upper<<std::endl;
         for ( int i=0;i<10; i++ ) stringData[ i ] = header[ I_Upper + i ];
         nY = atoi( stringData );
         if ( debug ) std::cout<<" nY :"<<nY<<std::endl;
      }
      if (  I_Upper == std::string::npos ) //if J= was not found after finding I=
      {
         
         nY = ( int ) sqrt( (double) nX );
         nX = nY;    //square grid
         if ( debug )
         {
            std::cout<<"nY :"<<nY<<std::endl;
            std::cout<<"nX :"<<nX<<std::endl;
         }
      } 
      //////////////////////////////////////////////////////////////////////////////////////////////////////
      //END HEADER PARSING SECTION
      /////////////////////////////////////////////////////////////////////////////////////////////////////
      fileI.seekg( 0, std::ios::beg );      //reset file pointer
      //read the first 3 lines from the files
      char line[ 256 ];
      for ( int i=0;i<3;i++ )
      {
         fileI.getline( line, 256 );
         if ( debug ) 
            std::cout<<line<<std::endl;
      }
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
         //std::cout<<std::endl;
         data.push_back( array );
         pts->InsertPoint( j, array[0], array[1], 0.0 );
         pts->InsertPoint( j+numVertices, array[0], array[1], -0.1 );
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
      writeVtkThing( uGrid, "grid.vtk", 0 );
      // Set selected scalar and vector quantities to be written to pointdata array
      std::cout<<"Number of Parameters :"<<numOfParameters<<std::endl;
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

