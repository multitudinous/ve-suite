#include <iostream>
#include "ansysReader.h"
#include "readWriteVtkThings.h"

using namespace std;

int main( int argc, char *argv[] )
{
   ansysReader * reader = NULL;
   if ( argc == 1 )
      reader = new ansysReader( "test_case.rst" );
   else
      reader = new ansysReader( argv[ 1 ] );

   reader->ReadHeader();
   reader->ReadRSTHeader();
   reader->ReadDOFBlock();
   reader->ReadNodalEquivalencyTable();
   reader->ReadElementEquivalencyTable();
   reader->ReadDataStepsIndexTable();
   reader->ReadTimeTable();
   reader->ReadGeometryTable();
   reader->ReadElementTypeIndexTable();
   reader->ReadNodalCoordinates();
   reader->ReadElementDescriptionIndexTable();
   reader->ReadSolutionDataHeader();
   reader->ReadNodalSolutions();

   // Now write to vtk format...
   writeVtkThing( (vtkDataSet*)reader->GetUGrid(), "test.vtk", 0 ); // 0=ascii

   cout << "\ndone!\n" << endl;

   delete reader;

   return 0;
}
