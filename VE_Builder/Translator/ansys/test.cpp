#include <iostream>
#include "ansysReader.h"
#include "readWriteVtkThings.h"
#include "vtkUnstructuredGrid.h"

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
   reader->ReadHeaderExtension();

   // Now write to vtk format...
   writeVtkThing( reader->GetUGrid(), "test.vtk", 0 ); // 0=ascii

   cout << "\ndone!\n" << endl;

   reader->GetUGrid()->Delete(); // this was removed from the destructor
   delete reader;

   return 0;
}
