/*************** <auto-copyright.pl BEGIN do not edit this line> **************

 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *   Headed by: Kenneth Mark Bryden, Ph.D.
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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include <ves/xplorer/environment/cfdSoundHandler.h>
#include <ves/xplorer/environment/cfdEnum.h>
#include <ves/xplorer/cfdCommandArray.h>
#include <ves/xplorer/util/fileIO.h>
#include <ves/xplorer/environment/cfdSound.h>

#include <iostream>
#include <fstream>

#include <ves/xplorer/cfdDebug.h>
using namespace VE_Xplorer;
using namespace VE_Util;

cfdSoundHandler::cfdSoundHandler( std::string param )
{
   _param = param;
   //this->_readParam = new cfdReadParam();

   if ( param.empty() )
      return;
   // Read the sound objects from the parameter file...
   this->CreateObjects();

   if ( this->sounds.size() > 0 ) 
   {
      std::cout << "| 10b Initializing........................................... Sound |" << std::endl;

      vprDEBUG(vesDBG,1) << " cfdSoundHandler: this->sounds.size() = "
                             << this->sounds.size()
                             << std::endl << vprDEBUG_FLUSH;

      // start the sonix system with desired audio library...
      snx::sonix::instance()->changeAPI( "OpenAL" );
   }
}

cfdSoundHandler::~cfdSoundHandler( void )
{
   //delete this->_readParam;

   if ( this->sounds.size() > 0 )
   {
      //vprDEBUG(vesDBG,2) << "deleting sounds"
      //                       << std::endl << vprDEBUG_FLUSH;

      for ( unsigned int i = 0; i < this->sounds.size(); i++ )
      {
         delete this->sounds[ i ];
      }
      this->sounds.clear();
   }
}

int cfdSoundHandler::GetNumberOfSounds( void )
{
   return this->sounds.size();
}

std::string cfdSoundHandler::GetSoundFilename( int i )
{
   // used to generate a list of sounds on the gui
   vprDEBUG(vesDBG,0) << "\treturning \"" << this->sounds[ i ]->soundName
                          << "\"" << std::endl << vprDEBUG_FLUSH;
   //return this->sounds[ i ]->fileName;
   return this->sounds[ i ]->soundName;
}
///////////////////////////////////////////////////////////
std::vector< cfdSound* > cfdSoundHandler::GetSounds( void )
{
   return sounds;
}
bool cfdSoundHandler::CheckCommandId( cfdCommandArray* commandArray )
{
   /*if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == UPDATE_SOUNDS )
   {
      this->_readParam->convertDecimalToBinary( (long)
            commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) );

      vprDEBUG(vesDBG,1) << " raw decimal = "
         << commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE )
         << ", this->sounds.size() = " << this->sounds.size()
         << std::endl << vprDEBUG_FLUSH;

      this->_readParam->convertBinaryToArray( 0, this->sounds.size() );

      for ( unsigned int i = 0; i < this->sounds.size(); i++ )
      {
         // need to fix the gui value thing
         vprDEBUG(vesDBG,1) << " this->_readParam->guiVal[ " << i << " ] = "
            << this->_readParam->guiVal[ i ] << std::endl << vprDEBUG_FLUSH;

         if ( this->sounds[ i ]->IsSounding() && this->_readParam->guiVal[ i ] == 0 ) 
         {
            vprDEBUG(vesDBG,0) << " stopping sound " << i
               << std::endl << vprDEBUG_FLUSH;
            this->sounds[ i ]->stopSound();
         }              
         else if ( this->_readParam->guiVal[ i ] == 1 ) //allows sound restart
         {
            vprDEBUG(vesDBG,0) << " playing sound " << i
               << std::endl << vprDEBUG_FLUSH;
            this->sounds[ i ]->playSound();
         } 
      }      

      return true;
   }*/
   return false;
}

void cfdSoundHandler::UpdateCommand()
{
   vprDEBUG(vesDBG,0) << " Doing nothing in cfdSoundHandler::UpdateCommand()"
                          << std::endl  << vprDEBUG_FLUSH;
}

void cfdSoundHandler::CreateObjects( void )
{
   // open the parameter filer and read the number of objects...
   std::ifstream input;
   input.open( this->_param.c_str() );

   int numObjects;
   input >> numObjects; 

   char textLine[ 256 ];
   input.getline( textLine, 256 );   //skip past remainder of line

   vprDEBUG(vesDBG,1) << " Number of Objects in Interactive Geometry : "
                          << numObjects << std::endl  << vprDEBUG_FLUSH;

   for ( int i = 0; i < numObjects; i++ )
   {
      int id;
      input >> id;
      vprDEBUG(vesDBG,1) << "Id of object in Interactive Geometry : "
                             << id << std::endl << vprDEBUG_FLUSH;
      input.getline( textLine, 256 );   //skip past remainder of line

      if ( id == 11 )
      {
         this->sounds.push_back(new cfdSound());
         
         vprDEBUG(vesDBG,1) << "this->sounds.size() = "
            << this->sounds.size() << std::endl << vprDEBUG_FLUSH;

         int i = this->sounds.size() - 1;

         // read the values in the parameter file...
         input >> this->sounds[ i ]->ambient;
         input.getline( textLine, 256 );   //skip past remainder of line

         input >> this->sounds[ i ]->retriggerable;
         input.getline( textLine, 256 );   //skip past remainder of line

         input >> this->sounds[ i ]->repeat;
         input.getline( textLine, 256 );   //skip past remainder of line

         input >> this->sounds[ i ]->volume;
         input.getline( textLine, 256 );   //skip past remainder of line

         input >> this->sounds[ i ]->pitchbend;
         input.getline( textLine, 256 );   //skip past remainder of line

         input >> this->sounds[ i ]->cutoff;
         input.getline( textLine, 256 );   //skip past remainder of line

         input >> this->sounds[ i ]->soundPositionX;
         input.getline( textLine, 256 );   //skip past remainder of line   

         input >> this->sounds[ i ]->soundPositionY;
         input.getline( textLine, 256 );   //skip past remainder of line   

         input >> this->sounds[ i ]->soundPositionZ;
         input.getline( textLine, 256 );   //skip past remainder of line   

         input >> this->sounds[ i ]->fileName;
         input.getline( textLine, 256 );   //skip past remainder of line

         input >> this->sounds[ i ]->soundName;
         input.getline( textLine, 256 );   //skip past remainder of line

         if ( fileIO::isFileReadable( this->sounds[ i ]->fileName ) ) 
         {
            vprDEBUG(vesDBG,0) << "\tsound fileName = " 
                                   << this->sounds[ i ]->fileName
                                   << std::endl << vprDEBUG_FLUSH;
         }
         else
         {
            std::cerr << "ERROR: unreadable sound file = " // not currently listed in parameter file
                      << this->sounds[ i ]->fileName 
                      << ".  You may need to correct your param file."
                      << std::endl;
            exit( 1 );
         }

         // populate a SoundInfo struct with the values defined above...
         this->sounds[ i ]->initSound();
      }
      else
      {
         // Skip past block
         //this->_readParam->ContinueRead( input, id );
      }
   }
}

