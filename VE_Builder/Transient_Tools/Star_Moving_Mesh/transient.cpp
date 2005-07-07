/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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
 * File:          $RCSfile: transient.cpp,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VE_Builder/Transient_Tools/Star_Moving_Mesh/transient.h"

#include <cstdlib>
#include <iostream>
#include <fstream>

using namespace std;

Transient::Transient( void )
{
   std::cout << "Enter number of time steps: " << std::endl;
   std::cin >> num_time_steps;

   std::cout << "Enter the time step to begin at: " << std::endl;
   std::cin >> begin_step;

   std::cout << "Enter the Post Frequency: " << std::endl;
   std::cin >> post_frequency;
}

Transient::~Transient( void )
{
}

Transient::Transient( Transient *copy )
{
}

void Transient::writeScript( void )
{
   ofstream outFile("transient");
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
   int i;
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
   outFile << "quit,nosave\n" << "/\n\n";
            
   outFile.close();
	system( "chmod 770 ./transient" );
        
   system("transient > /dev/null");
   system("rm -f transient");
}
