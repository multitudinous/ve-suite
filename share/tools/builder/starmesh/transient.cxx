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

#include "transient.h"

#include <cstdlib>
#include <iostream>
#include <fstream>
#include <sstream>

using namespace std;
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////
Transient::Transient( void )
{

}
////////////////////////////////////////////////////////////////////////////////////////////////////////////
Transient::~Transient( void )
{

}
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
void Transient::writeScript( std::string filename, std::string units, long int timeSteps, long int beginStep, long int postFreq )
{
    ofstream outFile("transient.csh");
    outFile << "#! /bin/tcsh\n" 
            << "\n"
            << "proam << EOF\n"
            << "x\n"
            << filename << "\n"
            << "n\n"
            << "y\n\n"
            << "evfi conn\n"
            << "trload," << filename << ".ccmt,nomvgr\n"  
            << "c\n"
	    << "memo maxsc2 20000000\n"
	    << "units " << units << "\n";
	   
    for ( int i = 0; i < timeSteps/postFreq; i++ ) 
    {
        if (i < 10)  
        {
            outFile << "store itst " << beginStep + (i+1)*postFreq << "\n"	    
            	    << "oper getv su 1\n"
	    	    << "oper getv sv 2\n"
	    	    << "oper getv sw 3\n"
	    	    << "oper getv p 4 relative\n"
            	    //<< "oper getv conc 5 1\n"
            	    //<< "oper getv conc 6 2\n"                        
            	    << "savu," << filename << "_00" << i << ".usr,all,coded,all\n"
            	    << "close " << filename << "_00" << i << ".usr\n"
            	    << "getv all\n"
            	    << "vwrite," << filename << "_00" << i << ".vrt,all,coded,all\n"
            	    << "close " << filename << "_00" << i << ".vrt\n"
            	    << "getc all\n"
            	    << "cwrite," << filename << "_00" << i << ".cel,all,coded,all\n"
            	    << "close " << filename << "_00" << i << ".cel\n";
      	 }        
         else if ( i > 9 && i < 100) 
         {
            outFile << "store itst " << beginStep + (i+1)*postFreq << "\n"	    
            	    << "oper getv su 1\n"
	    	    << "oper getv sv 2\n"
	    	    << "oper getv sw 3\n"
	    	    << "oper getv p 4 relative\n"
            	    //<< "oper getv conc 5 1\n"
            	    //<< "oper getv conc 6 2\n"                        
            	    << "savu," << filename << "_0" << i << ".usr,all,coded,all\n"
            	    << "close " << filename << "_0" << i << ".usr\n"
            	    << "getv all\n"
            	    << "vwrite," << filename << "_0" << i << ".vrt,all,coded,all\n"
            	    << "close " << filename << "_0" << i << ".vrt\n"
            	    << "getc all\n"
            	    << "cwrite," << filename << "_0" << i << ".cel,all,coded,all\n"
            	    << "close " << filename << "_0" << i << ".cel\n";
        }
        else if ( i > 99 && i < 1000) 
        {
            outFile << "store itst " << beginStep + (i+1)*postFreq << "\n"	    
            	    << "oper getv su 1\n"
	    	    << "oper getv sv 2\n"
	    	    << "oper getv sw 3\n"
	    	    << "oper getv p 4 relative\n"
            	    //<< "oper getv conc 5 1\n"
            	    //<< "oper getv conc 6 2\n"                        
            	    << "savu," << filename << "_" << i << ".usr,all,coded,all\n"
            	    << "close " << filename << "_" << i << ".usr\n"
            	    << "getv all\n"
            	    << "vwrite," << filename << "_" << i << ".vrt,all,coded,all\n"
            	    << "close " << filename << "_" << i << ".vrt\n"
            	    << "getc all\n"
            	    << "cwrite," << filename << "_" << i << ".cel,all,coded,all\n"
            	    << "close " << filename << "_" << i << ".cel\n";
        }
    }
    outFile << "quit,nosave\n" << "/\n\n";
            
    outFile.close();
    system( "chmod 770 ./transient.csh" );  // eliminate system calls
        
    system("tcsh transient.csh");    // eliminate system calls
   //system("rm -f transient");          // eliminate system calls
}
///////////////////////////////////////////////////////////////////////////////////////////////////////////////
void Transient::writeStarParam( std::string filename, long int timeSteps, long int postFreq )
{
    //system( "mkdir transient_VTK_data");
    for ( int i = 0; i < timeSteps/postFreq; i++ ) 
    {
        std::ostringstream dirStringStream;

        if (i < 10) 
	{         
            dirStringStream << filename << "_00" << i << ".param";
	}
      	else if ( i > 9 && i < 100)     
        {
	     dirStringStream << filename << "_0" << i << ".param";
     	}
	else if ( i > 99 && i < 1000)        
        {
	     dirStringStream << filename << "_" << i << ".param";              
	}
	
        std::string dirString = dirStringStream.str();
        const char * outputFileName;
        outputFileName = dirString.c_str();
        ofstream outputFile;
        outputFile.open(outputFileName,ios::out);

        if (i < 10)
        {
            outputFile 	<< "STARCEL=" << filename << "_00" << i << ".cel" << "\n"
        	       	<< "STARVRT=" << filename << "_00" << i << ".vrt" << "\n"  
        	       	<< "STARUSR=" << filename << "_00" << i << ".usr" << "\n" 
        		<< "VECTORNAME=Velocity" << "\n"
        		<< "SCALARNAME=Pressure" << "\n"
        		<< "ROTATEX=0" << "\n"
        		<< "ROTATEY=0" << "\n"
        		<< "ROTATEZ=0" << "\n"
        		<< "TRANSLATEX=0" << "\n"
        		<< "TRANSLATEY=0" << "\n"
        		<< "TRANSLATEZ=0" << "\n"
        		<< "SCALEINDEX=1" << "\n"
        		<< "SCALEFACTOR=0" << "\n"
        		<< "WRITEOPTION=1" << "\n" 
        		<< "OUTPUTFILENAME=flowdata_00" << i << ".vtk" << "\n";
        }

        else if ( i > 9 && i < 100)
        {
            outputFile 	<< "STARCEL=" << filename << "_0" << i << ".cel" << "\n"
        		<< "STARVRT=" << filename << "_0" << i << ".vrt" << "\n"  
        		<< "STARUSR=" << filename << "_0" << i << ".usr" << "\n"  
        		<< "VECTORNAME=Velocity" << "\n"
        		<< "SCALARNAME=Pressure" << "\n"
        		<< "ROTATEX=0" << "\n"
        		<< "ROTATEY=0" << "\n"
        		<< "ROTATEZ=0" << "\n"
        		<< "TRANSLATEX=0" << "\n"
        		<< "TRANSLATEY=0" << "\n"
        		<< "TRANSLATEZ=0" << "\n"
        		<< "SCALEINDEX=1" << "\n"
        		<< "SCALEFACTOR=0" << "\n"
        		<< "WRITEOPTION=1" << "\n" 
        		<< "OUTPUTFILENAME=flowdata_0" << i << ".vtk" << "\n";
           }

        else if ( i > 99 && i < 1000)  
        {  
            outputFile 	<< "STARCEL=" << filename << "_" << i << ".cel" << "\n"
        		<< "STARVRT=" << filename << "_" << i << ".vrt" << "\n"  
        		<< "STARUSR=" << filename << "_" << i << ".usr" << "\n" 
        		<< "VECTORNAME=Velocity" << "\n"
        		<< "SCALARNAME=Pressure" << "\n"
        		<< "ROTATEX=0" << "\n"
        		<< "ROTATEY=0" << "\n"
        		<< "ROTATEZ=0" << "\n"
        		<< "TRANSLATEX=0" << "\n"
        		<< "TRANSLATEY=0" << "\n"
        		<< "TRANSLATEZ=0" << "\n"
        		<< "SCALEINDEX=1" << "\n"
        		<< "SCALEFACTOR=0" << "\n"
        		<< "WRITEOPTION=1" << "\n" 
        		<< "OUTPUTFILENAME=flowdata_" << i << ".vtk" << "\n";
        }
        
        outputFile.close();
    }
}
///////////////////////////////////////////////////////////////////////////////////////////////////////////
void Transient::writeTranslatorScript( std::string filename, std::string outputDir, long int timeSteps, long int postFreq )
{
   ofstream translatorScript;
   translatorScript.open("transientStarToVtk.csh",ios::out);
   
   translatorScript << "#! /bin/csh -f" << "\n" << "\n"; 

   for ( int j = 0; j < timeSteps/postFreq; j++ )
   {
      if (j < 10)          
         translatorScript << "/usr/local/bin/loaderToVtk" << " " << "-singleFile" << " "<< filename << "_00" << j << ".param" << " " << "-loader" 
	 		  << " " << "star" << " " << "-o" << " " << outputDir << " " << "-w" << " " << "file" << "\n";
      else if ( j > 9 && j < 100)     
         translatorScript << "/usr/local/bin/loaderToVtk" << " " << "-singleFile" << " "<< filename << "_0" << j << ".param" << " " << "-loader" 
	 		  << " " << "star" << " " << "-o" << " " << outputDir << " " << "-w" << " " << "file" << "\n"; 
      else if ( j > 99 && j < 1000)        
         translatorScript << "/usr/local/bin/loaderToVtk" << " " << "-singleFile" << " "<< filename << "_" << j << ".param" << " " << "-loader" 
	 		  << " " << "star" << " " << "-o" << " " << outputDir << " " << "-w" << " " << "file" << "\n";                                      
   }

   translatorScript.close();
   system("tcsh transientStarToVtk.csh");    //eliminate this system call
}

