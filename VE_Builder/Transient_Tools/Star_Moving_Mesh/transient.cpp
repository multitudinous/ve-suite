#include <vector.h>                 
#include <stdlib.h>                 
#include <iostream.h>                 
#include <fstream.h>                 
#include "transient.h"               

Transient::Transient( void ){
   cout << "Enter number of time steps: " << endl;
   cin >> num_time_steps;

   cout << "Enter the time step to begin at: " << endl;
   cin >> begin_step;

   cout << "Enter the Post Frequency: " << endl;
   cin >> post_frequency;
   }

Transient::~Transient( void ){

}

Transient::Transient( Transient *copy ){

}

void Transient::writeScript( void ){
   int i;
   fstream outFile;

   outFile.open("transient", ios::out);
   outFile << "#! /bin/tcsh\n" 
            << "\n"
            << "proam << EOF\n"
            << "x\n"
            << "star\n"
            << "n\n"
            << "y\n\n"
            << "evfi conn\n"
            << "trload,star.pstt,mvgr\n"
            << "c\n";
   for ( i = 0; i < num_time_steps; i++ ) {
      outFile << "store iter " << begin_step + (i+1)*post_frequency << "\n"
            << "getv all vmag\n"
            << "savu,star_" << i << ".usr,all,coded,all\n"
            << "close star_" << i << ".usr\n"
            << "getv all\n"
            << "vwrite,star_" << i << ".vrt,all,coded,all\n"
            << "close star_" << i << ".vrt\n"
            << "getc all\n"
            << "cwrite,star_" << i << ".cel,all,coded,all\n"
            << "close star_" << i << ".cel\n";
   }
   outFile << "quit,nosave\n"
            << "/\n\n";
            
   outFile.close();
	system( "chmod 770 ./transient" );
        
   system("transient > /dev/null");
   system("rm -f transient");
}
