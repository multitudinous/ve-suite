#include "ansysReader.h"
#include <iostream>

using namespace std;

int main()
{
   ansysReader * reader = new ansysReader( "test_case.rst" );
   reader->ReadHeader();
   reader->ReadSecondBlock();
   reader->ReadThirdBlock();
   reader->ReadFourthBlock();
   reader->ReadFifthBlock();
   reader->ReadSixthBlock();
   cout << "\ndone!\n" << endl;
   return 0;
}
