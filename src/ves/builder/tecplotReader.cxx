/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/

//works right now for the INEL Tecplot data
#include <ves/builder/tecplotReader.h>
#include <vtkPoints.h>
#include <vtkUnstructuredGrid.h>
#include <vtkStructuredGrid.h>
#include <vtkUnstructuredGridWriter.h>
#include <vtkCellType.h>
#include <vtkFloatArray.h>
#include <ves/xplorer/util/readWriteVtkThings.h>
#include <ves/builder/converter.h>
tecplotReader::tecplotReader( )
{
    uGrid = NULL;
    pts = NULL;
    parameterData = NULL;
    numCells = 0;

    colsOfData = 0;
    numOfParameters = 0;
    numVertices = 0;
}
tecplotReader::~tecplotReader()
{
    fileI.close();
}
void tecplotReader::allocateVariables()
{
    numCells = ( nX - 1 ) * ( nY - 1 );
    numVertices = nX * nY;
    parameterData = new vtkFloatArray* [ numOfParameters ]; //the number of parameters
    for( int i = 0;i < numOfParameters;i++ )
    {
        parameterData[ i ] = vtkFloatArray::New();
        parameterData[ i ]->SetName( variablNames[ i+2 ].c_str() );
        std::cout << parameterData[ i ]->GetName() << std::endl;
        if( i != ( numOfParameters - 1 ) )
            parameterData[ i ]->SetNumberOfComponents( 1 );
        else
            parameterData[ i ]->SetNumberOfComponents( 3 ); //this is the velocity vector
        parameterData[ i ]->SetNumberOfTuples( 2*nX*nY );
    }
}
vtkUnstructuredGrid* tecplotReader::tecplotToVTK( std::string inFileName, int debug )
{
    int numChars;
    numChars = 0;
    char tempChar; //temporary storage for character data
    //check existence of file
    fileI.open( inFileName.c_str() );
    if( fileI == NULL )
    {
        std::cout << "Input file does not exist :" << std::endl;
        return uGrid;
    }
    else  //file exists
    {
        do
        {
            numChars++;
            tempChar = ( char )fileI.peek();
            header += tempChar;//update the header
            //if ( debug )
            //std::cout<<;
            fileI.get();
        }
        while (( fileI.peek() != 0 ) && (( char )fileI.peek() != ')' ) );

        std::cout << std::endl;
        if( debug )
        {
            std::cout << "Number of characters :" << numChars << std::endl;
            std::cout << header << std::endl; //print header to screen
        }
        /////////////////////////////////////////////////////////////////////////////////////////////////////////
        //HEADER PARSING SECTION
        ////////////////////////////////////////////////////////////////////////////////////////////////////////
        I_Lower = header.find( "VARIABLES" );//search for "VARIABLES" in the header
        if( debug ) std::cout << "VARIABLES found at :" << I_Lower << std::endl;
        I_Upper = header.find( "ZONE" );//search for "ZONE" in the header
        if( debug ) std::cout << "ZONE found at :" << I_Upper << std::endl;
        //assign that portion of the header to tempString
        for( unsigned int i = I_Lower;i < I_Upper;i++ ) tempString += header[ i ];
        //std::cout<<tempString<<std::endl;
        I = tempString.begin();
        I_Lower = 0;
        for( I = tempString.begin(); I != tempString.end(); ++I )
        {
            I_Lower++;
            if (( *I ) == '"' )
            {
                colsOfData++;//search the quotation marks "
                locationQuotMarks.push_back( I_Lower );
            }
        }
        //the number of quotation marks is twice the number of variables in the data
        colsOfData = colsOfData / 2;
        std::string varNamString;
        if( debug ) std::cout << " Columns of Data :" << colsOfData << std::endl;
        //////////////////////////////////////SEARCH FOR THE VARIABLE NAMES NOW
        for( vectorIntIterator = locationQuotMarks.begin();vectorIntIterator != locationQuotMarks.end(); vectorIntIterator += 2 )
        {
            I_Lower = ( *vectorIntIterator ); //std::cout<<"I lower :"<<I_Lower<<std::endl;
            I_Upper = *( vectorIntIterator + 1 ); //std::cout<<"I Upper :"<<I_Upper<<std::endl;
            //between I_Lower and I_Upper copy from tempString into varNamString
            varNamString.erase();   //empty string before next iteration so that it does not hold cumulative values
            for( unsigned int c = I_Lower;c < I_Upper - 1; c++ ) varNamString += tempString[c];
            variablNames.push_back( varNamString );
        }
        if( debug )
        {
            std::cout << "Variable names are :" << std::endl;
            for( vectorStringIterator = variablNames.begin();vectorStringIterator != variablNames.end();++vectorStringIterator )
                std::cout << ( *vectorStringIterator ) << std::endl;
        }
        ///////////////SEARCH VARIABLE NAMES FOR KEYWORDS
        int velocityCount;   //the number of times velocity occurs in the variable names
        velocityCount = 0;   //initialize to zero
        int coOrdinateCount; //the number of time either x or y occurs in the variables
        coOrdinateCount = 0; //initialize to zero
        for( vectorStringIterator = variablNames.begin();vectorStringIterator != variablNames.end();++vectorStringIterator )
        {
            if (( *vectorStringIterator ).find( "locity" ) != std::string::npos ) velocityCount++;
            if (( *vectorStringIterator ).find( "osition" ) != std::string::npos ) coOrdinateCount++;
        }
        if( debug ) std::cout << "Number of times velocity occurs :" << velocityCount << std::endl;
        if( debug ) std::cout << "Number of co-ordinates :" << coOrdinateCount << std::endl;
        //////////////////////TO SET THE NUMBER OF PARAMETERS IN THE DATA
        //check if colsOfData is > velocityCount + coOrdinateCount, if so there are parameters by other names
        int otherParameters;
        otherParameters = 0; //initialize to zero
        otherParameters = colsOfData - ( velocityCount + coOrdinateCount );
        if( debug ) std::cout << "Number of other parameters :" << otherParameters << std::endl;
        if( velocityCount < 3 ) //need to calculate absolute velocity
        {
            numOfParameters = velocityCount + otherParameters + 2; //one parameter for absolute velocity and the other for velocity vector
            variablNames.push_back( "Absolute Velocity" );
            variablNames.push_back( "Velocity vector" );
        }
        if( velocityCount == 3 )
        {
            numOfParameters = velocityCount + otherParameters + 1;   //one for velocity vector
            variablNames.push_back( "Velocity vector" );
        }
        //////////////////////COMPLETED SETTING NUMBER OF PARAMETERS IN DATA
        tempString.erase();  //clear the contents of tempString
        char stringData[ 10 ];  //create a new character array
        I_Lower = header.find( "I=" );   //look for I= in the header
        I_Upper = header.find( "J=" );   //look for J= in the header
        if (( I_Upper == std::string::npos ) && ( I_Lower == std::string::npos ) )  //if both I= and J= are not found
            std::cout << "Wrong Tecplot file.!!!! Please check :" << std::endl;
        if( I_Lower != std::string::npos ) //make sure if I= is found
        {
            I_Lower = I_Lower + 2;
            if( debug ) std::cout << " Location of I= :" << I_Lower << std::endl;
            for( int i = 0;i < 10; i++ ) stringData[ i ] = header[ I_Lower + i ];
            nX = atoi( stringData );
            if( debug ) std::cout << " nX :" << nX << std::endl;
        }
        if( I_Upper != std::string::npos ) //see if J= was found
        {
            I_Upper = I_Upper + 2;
            if( debug ) std::cout << " Location of J= :" << I_Upper << std::endl;
            for( int i = 0;i < 10; i++ ) stringData[ i ] = header[ I_Upper + i ];
            nY = atoi( stringData );
            if( debug ) std::cout << " nY :" << nY << std::endl;
        }
        if( I_Upper == std::string::npos ) //if J= was not found after finding I=
        {

            nY = ( int ) sqrt(( double ) nX );
            nX = nY;    //square grid
            if( debug )
            {
                std::cout << "nY :" << nY << std::endl;
                std::cout << "nX :" << nX << std::endl;
            }
        }
        //////////////////////////////////////////////////////////////////////////////////////////////////////
        //END HEADER PARSING SECTION
        /////////////////////////////////////////////////////////////////////////////////////////////////////
        //===========
        array = new double [ colsOfData ];
        //allocate memory since we know nX and nY now
        allocateVariables();
        std::cout << "Input file        :" << inFileName << std::endl
        << "Number of  cells  :" << numCells << std::endl
        << "Number of Vertices:" << nX*nY << std::endl;
        //reset file pointer to beginning to read in numbers
        fileI.seekg( 0, std::ios::beg );
        fileI.ignore( numChars + 1 );      //ignore all characters from beginning until ")"
        //set points
        pts = vtkPoints::New();
        int* cPt = new int [ 8 ];  //HEX elements assumed, coz data is taken from PIV runs
        double* dataArray = new double [ numOfParameters -1 ];   //last parameter is the vector
        for( int j = 0;j < nX*nY;j++ )//loop over the number of vertices
        {

            for( int i = 0;i < colsOfData;i++ )
            {
                fileI >> array[ i ]; //read from file into array
            }
            for( int i = 2;i < colsOfData;i++ )
            {
                dataArray[ i-2 ] = array[ i ];
            }
            if( velocityCount < 3 ) dataArray[ 2 ] = sqrt( array[ 2 ] * array[ 2] + array[ 3 ] * array[ 3 ] ); //assign velocity magnitud
            pts->InsertPoint( j, array[0], array[1], 0.0 );
            pts->InsertPoint( j + numVertices, array[0], array[1], -0.1 );
            //set scalar parameters
            for( int i = 0;i < numOfParameters - 1;i++ )
            {
                parameterData[ i ]->SetComponent( j, 0, dataArray[ i ] );
                parameterData[ i ]->SetComponent( j + numVertices, 0, dataArray[ i ] );
            }
            //set vector components
            parameterData[ numOfParameters-1 ]->SetComponent( j, 0, dataArray[ 0 ] );
            parameterData[ numOfParameters-1 ]->SetComponent( j + numVertices, 0, dataArray[ 0 ] );
            parameterData[ numOfParameters-1 ]->SetComponent( j, 1, dataArray[ 1 ] );
            parameterData[ numOfParameters-1 ]->SetComponent( j + numVertices, 1, dataArray[ 1 ] );
            parameterData[ numOfParameters-1 ]->SetComponent( j, 2, 0.0 );
            parameterData[ numOfParameters-1 ]->SetComponent( j + numVertices, 2, 0.0 );
        }

        uGrid = vtkUnstructuredGrid::New();
        uGrid->SetPoints( pts );
        int c = 0;
        int alongX;
        while( c < numCells )
        {
            for( alongX = c;alongX < c + ( nX - 1 );alongX++ )
            {
                //assign vertex numbers
                cPt[0] = alongX;
                cPt[1] = alongX + 1;
                cPt[2] = alongX + nX + 1;
                cPt[3] = alongX + nX;
                cPt[4] = alongX + numVertices;
                cPt[5] = alongX + 1 + numVertices;
                cPt[6] = alongX + nX + numVertices + 1;
                cPt[7] = alongX + numVertices + nX;
                uGrid->InsertNextCell( VTK_HEXAHEDRON, 8, cPt );
            }
            if( debug ) std::cout << "C :" << c << "\t" << "I :" << alongX << std::endl;
            c = alongX + 1;
        }
        // Set selected scalar and vector quantities to be written to pointdata array
        std::cout << "Number of Parameters :" << numOfParameters << std::endl;
        letUsersAddParamsToField( numOfParameters, parameterData, uGrid->GetPointData() );
        vtkUnstructuredGrid *finalUGrid = vtkUnstructuredGrid::New();
        finalUGrid->DeepCopy( uGrid );
        for( int i = 0;i < numOfParameters;i++ )
        {
            parameterData[ i ]->Delete();
        }
        delete [] parameterData;
        parameterData = NULL;
        uGrid->Delete();
        uGrid = NULL;
        pts->Delete();
        pts = NULL;
        return finalUGrid;
    }
}

