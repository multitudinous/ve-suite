#include "ansysReader.h"
#include <iostream>

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
   return 0;

   int counter = 6; // incremented for backward comapatibility
   while( 1 )
   {
      cout << "\nReading block " << ++counter << endl;
      reader->ReadGenericIntBlock();
   }
   cout << "\ndone!\n" << endl;

   delete reader;

   return 0;
}
