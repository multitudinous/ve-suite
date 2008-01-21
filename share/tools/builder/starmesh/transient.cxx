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

#include "VE_Builder/Transient_Tools/Star_Moving_Mesh/transient.h"

#include <cstdlib>
#include <iostream>
#include <fstream>
#include <sstream>

using namespace std;

Transient::Transient( void )
{

   std::cout << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" << std::endl;
   std::cout << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" << std::endl;
   std::cout << "~                                                               ~" << std::endl;
   std::cout << "~   Transient StarCD to VTK Routine (uses translateToVtk)       ~" << std::endl;
   std::cout << "~                                                               ~" << std::endl;
   std::cout << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" << std::endl;
   std::cout << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" << std::endl;



   std::cout << "Enter number of time steps: " << std::endl;
   std::cin >> num_time_steps;

   std::cout << "Enter the time step to begin at: " << std::endl;
   std::cin >> begin_step;

   std::cout << "Enter the Post Frequency: " << std::endl;
   std::cin >> post_frequency;

   std::cout << "Patience, please...." << std::endl;
}

Transient::~Transient( void )
{
}

Transient::Transient( Transient *copy )
{
}


///////////////////////////////////////////////////////////////
//
//    This writes the usr, cel and vrt files from StarCD
//
///////////////////////////////////////////////////////////////

void Transient::writeScript( void )
{
   ofstream outFile("transient");
   outFile << "#! /bin/tcsh\n" 
           << "\n"
           << "proam << EOF\n"
           << "x\n"
           << "star\n"  ///////////////////////////////make sure this changes for the casename
           << "n\n"
           << "y\n\n"
           << "evfi conn\n"
           << "trload,star.pstt,mvgr\n"  ///////////////////////////////make sure this changes for the casename
           << "c\n";
   int i;
   for ( i = 0; i < num_time_steps; i++ ) 
   {
      if (i < 10)  
      {
         outFile << "store iter " << begin_step + (i+1)*post_frequency << "\n"
            << "getv all vmag\n"
            //<< "oper getv conc 5 1\n"
            //<< "oper getv conc 6 2\n"                        
            << "savu,star_00" << i << ".usr,all,coded,all\n"
            << "close star_00" << i << ".usr\n"
            << "getv all\n"
            << "vwrite,star_00" << i << ".vrt,all,coded,all\n"
            << "close star_00" << i << ".vrt\n"
            << "getc all\n"
            << "cwrite,star_00" << i << ".cel,all,coded,all\n"
            << "close star_00" << i << ".cel\n";
      }        

      else if ( i > 9 && i < 100) 
      {
         outFile << "store iter " << begin_step + (i+1)*post_frequency << "\n"
            << "getv all vmag\n"
            //<< "oper getv conc 5 1\n"
            //<< "oper getv conc 6 2\n"                        
            << "savu,star_0" << i << ".usr,all,coded,all\n"
            << "close star_0" << i << ".usr\n"
            << "getv all\n"
            << "vwrite,star_0" << i << ".vrt,all,coded,all\n"
            << "close star_0" << i << ".vrt\n"
            << "getc all\n"
            << "cwrite,star_0" << i << ".cel,all,coded,all\n"
            << "close star_0" << i << ".cel\n";
      }

      else if ( i > 99 && i < 1000) 
      {
         outFile << "store iter " << begin_step + (i+1)*post_frequency << "\n"
            << "getv all vmag\n"
            //<< "oper getv conc 5 1\n"
            //<< "oper getv conc 6 2\n"                        
            << "savu,star_" << i << ".usr,all,coded,all\n"
            << "close star_" << i << ".usr\n"
            << "getv all\n"
            << "vwrite,star_" << i << ".vrt,all,coded,all\n"
            << "close star_" << i << ".vrt\n"
            << "getc all\n"
            << "cwrite,star_" << i << ".cel,all,coded,all\n"
            << "close star_" << i << ".cel\n";
      }
   }
   outFile << "quit,nosave\n" << "/\n\n";
            
   outFile.close();
	system( "chmod 770 ./transient" );  // eliminate system calls
        
   system("transient > /dev/null");    // eliminate system calls
   system("rm -f transient");          // eliminate system calls
}

//////////////////////////////////////////////////////////////////////////////////////////
//
// This creates the same number of param files to read in for the "translateToVtk" script
//
///////////////////////////////////////////////////////////////////////////////////////////

void Transient::writeStarParam( void )
{
   system("rm -rf transient_VTK_data");
   system("mkdir transient_VTK_data");   // these come out

   int i;
   for ( i = 0; i < num_time_steps/post_frequency; i++ ) 
   {
      std::ostringstream dirStringStream;

      if (i < 10)          
         dirStringStream << "star_00" << i << ".param";
      else if ( i > 9 && i < 100)     
         dirStringStream << "star_0" << i << ".param";
      else if ( i > 99 && i < 1000)        
         dirStringStream << "star_" << i << ".param";              

      std::string dirString = dirStringStream.str();
      const char * outputFileName;
      outputFileName = dirString.c_str();
      ofstream outputFile;
      outputFile.open(outputFileName,ios::out);

         if (i < 10)
         {
            outputFile << "STARCEL=star_00" << i << ".cel" << "\n"
                 << "STARVRT=star_00" << i << ".vrt" << "\n"  
                 << "STARUSR=star_00" << i << ".usr" << "\n" 
                 << "VECTORNAME=Velocity" << "\n"
                 << "SCALARNAME=scalar1" << "\n"
                 << "SCALARNAME=scalar2" << "\n"
                 << "SCALARNAME=scalar3" << "\n"
                 << "ROTATEX=0" << "\n"
                 << "ROTATEY=0" << "\n"
                 << "ROTATEZ=0" << "\n"
                 << "TRANSLATEX=0" << "\n"
                 << "TRANSLATEY=0" << "\n"
                 << "TRANSLATEZ=0" << "\n"
                 << "SCALEINDEX=1" << "\n"
                 << "SCALEFACTOR=0" << "\n"
                 << "WRITEOPTION=1" << "\n" 
                 << "OUTPUTFILENAME=transient_VTK_data/flowdata_00" << i << ".vtk" << "\n";
         }

         else if ( i > 9 && i < 100)
         {
            outputFile << "STARCEL=star_0" << i << ".cel" << "\n"
                 << "STARVRT=star_0" << i << ".vrt" << "\n"  
                 << "STARUSR=star_0" << i << ".usr" << "\n" 
                 << "VECTORNAME=Velocity" << "\n"
                 << "SCALARNAME=scalar1" << "\n"
                 << "SCALARNAME=scalar2" << "\n"
                 << "SCALARNAME=scalar3" << "\n"
                 << "ROTATEX=0" << "\n"
                 << "ROTATEY=0" << "\n"
                 << "ROTATEZ=0" << "\n"
                 << "TRANSLATEX=0" << "\n"
                 << "TRANSLATEY=0" << "\n"
                 << "TRANSLATEZ=0" << "\n"
                 << "SCALEINDEX=1" << "\n"
                 << "SCALEFACTOR=0" << "\n"
                 << "WRITEOPTION=1" << "\n" 
                 << "OUTPUTFILENAME=transient_VTK_data/flowdata_0" << i << ".vtk" << "\n";
            }

         else if ( i > 99 && i < 1000)  
         {  
            outputFile << "STARCEL=star_" << i << ".cel" << "\n"
                 << "STARVRT=star_" << i << ".vrt" << "\n"  
                 << "STARUSR=star_" << i << ".usr" << "\n" 
                 << "VECTORNAME=Velocity" << "\n"
                 << "SCALARNAME=scalar1" << "\n"
                 << "SCALARNAME=scalar2" << "\n"
                 << "SCALARNAME=scalar3" << "\n"
                 << "ROTATEX=0" << "\n"
                 << "ROTATEY=0" << "\n"
                 << "ROTATEZ=0" << "\n"
                 << "TRANSLATEX=0" << "\n"
                 << "TRANSLATEY=0" << "\n"
                 << "TRANSLATEZ=0" << "\n"
                 << "SCALEINDEX=1" << "\n"
                 << "SCALEFACTOR=0" << "\n"
                 << "WRITEOPTION=1" << "\n" 
                 << "OUTPUTFILENAME=transient_VTK_data/flowdata_" << i << ".vtk" << "\n";
         }
         
      outputFile.close();
   }
}


/////////////////////////////////////////////////////////////////////////////////
//
// This creates the script that will run the "translateToVtk" script recursively 
//
/////////////////////////////////////////////////////////////////////////////////

void Transient::writeTranslatorScript(void)
{
   ofstream translatorScript;
   translatorScript.open("transientStarToVtk.csh",ios::out);
   
   translatorScript << "#! /bin/csh -f" << "\n" << "\n"
                    << "setenv VE_SUITE_HOME /home/users/sgent/TSVEG/VE_Suite" << "\n"
                    << "source $VE_SUITE_HOME/VE_Installer/setup.tsh" << "\n"; 
 

   int j = 0;

   for ( j = 0; j < num_time_steps/post_frequency; j++ )
   {
      if (j < 10)          
         translatorScript << "translateToVtk" << " " << "2" << " "<< "star_00" << j << ".param" << "\n";
      else if ( j > 9 && j < 100)     
         translatorScript << "translateToVtk" << " " << "2" << " "<< "star_0" << j << ".param" << "\n"; 
      else if ( j > 99 && j < 1000)        
         translatorScript << "translateToVtk" << " " << "2" << " "<< "star_" << j << ".param" << "\n";                                      
   }

   translatorScript.close();
   system("tcsh transientStarToVtk.csh");    //eliminate this system call
}

