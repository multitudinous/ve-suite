#include "transient.h"
#include <iostream.h>                 

int main ( int argc, char* argv[] ) {
   Transient *transient = new Transient();
   transient->writeScript();
   delete transient;
   cout << "Done!"<< endl;
   return 1;
}
