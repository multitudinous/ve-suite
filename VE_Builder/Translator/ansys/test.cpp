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
   reader->ReadSecondBlock();
   reader->ReadThirdBlock();
   reader->ReadFourthBlock();
   reader->ReadFifthBlock();
   reader->ReadSixthBlock();

   cout << "\ndone!\n" << endl;

   delete reader;

   return 0;
}
