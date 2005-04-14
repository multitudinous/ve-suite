/*************** <auto-copyright.pl BEGIN do not edit this line> **************

 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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
 * -----------------------------------------------------------------
 * File:          $RCSfile: cfdSoundHandler.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "cfdEnum.h"
#include "cfdCommandArray.h"
#include "cfdSoundHandler.h"
#include "cfdReadParam.h"
#include "fileIO.h"
#include "cfdFileInfo.h"
#include "cfdSound.h"

#include <iostream>
#include <fstream>

#include <vrj/Util/Debug.h>

cfdSoundHandler::cfdSoundHandler( char* param )
{
   _param = param;
   this->_readParam = new cfdReadParam();

   // Read the sound objects from the parameter file...
   CreateObjects();

   if ( this->soundFiles.size() > 0 ) 
   {
      std::cout << "| 10b Initializing........................................... Sound |" << std::endl;

      vprDEBUG(vprDBG_ALL,1) << " cfdSoundHandler: this->soundFiles.size() = "
                             << this->soundFiles.size()
                             << std::endl << vprDEBUG_FLUSH;

      snx::sonix::instance()->changeAPI( "OpenAL" );

      //this->sounds[i]->initApi();
      for ( int i = 0; i < this->soundFiles.size(); i++ )
      {
         this->sounds.push_back(new cfdSound());
         strcpy(this->sounds[i]->fileName, this->soundFiles[i]->fileName);
         this->sounds[i]->ambient        = this->soundFiles[i]->ambient;
         this->sounds[i]->retriggerable  = this->soundFiles[i]->retriggerable;
         this->sounds[i]->volume         = this->soundFiles[i]->volume;
         this->sounds[i]->pitchbend      = this->soundFiles[i]->pitchbend;
         this->sounds[i]->cutoff         = this->soundFiles[i]->cutoff;
         this->sounds[i]->soundPositionX = this->soundFiles[i]->soundPositionX;
         this->sounds[i]->soundPositionY = this->soundFiles[i]->soundPositionY;
         this->sounds[i]->soundPositionZ = this->soundFiles[i]->soundPositionZ;
         strcpy(this->sounds[i]->soundName, this->soundFiles[i]->soundName);

         this->sounds[ i ]->initSound();
         
         vprDEBUG(vprDBG_ALL,1) << "\tcfdSoundHandler:  fileName: "
                                << this->sounds[ i ]->fileName
                                << std::endl << vprDEBUG_FLUSH;
         vprDEBUG(vprDBG_ALL,1) << "\tcfdSoundHandler:    volume: "
                                << this->sounds[ i ]->volume
                                << std::endl << vprDEBUG_FLUSH;
         vprDEBUG(vprDBG_ALL,1) << "\tcfdSoundHandler: soundName: "
                                << this->sounds[ i ]->soundName
                                << std::endl << vprDEBUG_FLUSH;
      }
   }
}

cfdSoundHandler::~cfdSoundHandler( void )
{
   delete this->_readParam;

   if ( this->sounds.size() != 0 )
   {
      vprDEBUG(vprDBG_ALL,2) << "deleting sounds"
                             << std::endl << vprDEBUG_FLUSH;

      for ( int i = 0; i < this->sounds.size(); i++ )
      {
         delete this->sounds[ i ];
      }
      this->sounds.clear();
   }
}

int cfdSoundHandler::GetNumberOfSounds( void )
{
   return this->soundFiles.size();
}

char* cfdSoundHandler::GetSoundFilename( int i )
{
   return this->sounds[ i ]->fileName;
}

bool cfdSoundHandler::CheckCommandId( cfdCommandArray* commandArray )
{
   if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == UPDATE_SOUNDS )
   {
      this->_readParam->convertDecimalToBinary( (long)
            commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) );

      vprDEBUG(vprDBG_ALL,1) << " raw decimal = "
         << commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE )
         << ", this->sounds.size() = " << this->sounds.size()
         << std::endl << vprDEBUG_FLUSH;

      this->_readParam->convertBinaryToArray( 0, this->soundFiles.size() );

      for ( int i = 0; i < this->sounds.size(); i++ )
      {
         // need to fix the gui value thing
         vprDEBUG(vprDBG_ALL,1) << " this->_readParam->guiVal[ " << i << " ] = "
            << this->_readParam->guiVal[ i ] << std::endl << vprDEBUG_FLUSH;

         if ( this->sounds[ i ]->IsSounding() && this->_readParam->guiVal[ i ] == 0 ) 
         {
            vprDEBUG(vprDBG_ALL,0) << " stopping sound " << i
               << std::endl << vprDEBUG_FLUSH;
            this->sounds[ i ]->stopSound();
         }              
         else if ( this->_readParam->guiVal[ i ] == 1 ) //allows sound restart
         {
            vprDEBUG(vprDBG_ALL,0) << " playing sound " << i
               << std::endl << vprDEBUG_FLUSH;
            this->sounds[ i ]->playSound();
         } 
      }      

      return true;
   }
   return false;
}

void cfdSoundHandler::UpdateCommand()
{
   std::cerr << "doing nothing in cfdSoundHandler::UpdateCommand()" << std::endl;
}

void cfdSoundHandler::CreateObjects( void )
{
   int numObjects;
   char text[ 256 ];
   char textLine[ 256 ];
   std::ifstream input;
   input.open( this->_param );
   input >> numObjects; 
   input.getline( text, 256 );   //skip past remainder of line

   vprDEBUG(vprDBG_ALL,1) << " Number of Objects in Interactive Geometry : "
                          << numObjects << std::endl  << vprDEBUG_FLUSH;

   for( int i = 0; i < numObjects; i++ )
   {
      int id;
      input >> id;
      vprDEBUG(vprDBG_ALL,1) << "Id of object in Interactive Geometry : "
                             << id << std::endl << vprDEBUG_FLUSH;
      input.getline( text, 256 );   //skip past remainder of line
      if ( id == 11 )
      {
         this->soundFiles.push_back( new fileInfo() );

         vprDEBUG(vprDBG_ALL,1) << "this->soundFiles.size() = "
            << this->soundFiles.size() << std::endl << vprDEBUG_FLUSH;

         int index = this->soundFiles.size() - 1;

         //ambient
         input >> this->soundFiles[ index ]->ambient;
         input.getline( textLine, 256 );   //skip past remainder of line
         //retriggerable
         input >> this->soundFiles[ index ]->retriggerable;
         input.getline( textLine, 256 );   //skip past remainder of line
         //volume
         input >> this->soundFiles[ index ]->volume;
         input.getline( textLine, 256 );   //skip past remainder of line
         //pitchbend
         input >> this->soundFiles[ index ]->pitchbend;
         input.getline( textLine, 256 );   //skip past remainder of line
         //cutoff
         input >> this->soundFiles[ index ]->cutoff;
         input.getline( textLine, 256 );   //skip past remainder of line
         //PositionX
         input >> this->soundFiles[ index ]->soundPositionX;
         input.getline( textLine, 256 );   //skip past remainder of line   
         //PositionY
         input >> this->soundFiles[ index ]->soundPositionY;
         input.getline( textLine, 256 );   //skip past remainder of line   
         //PositionZ
         input >> this->soundFiles[ index ]->soundPositionZ;
         input.getline( textLine, 256 );   //skip past remainder of line   
         //filename
         input >> this->soundFiles[ index ]->fileName;
         input.getline( textLine, 256 );   //skip past remainder of line
         //Sound name
         input >> this->soundFiles[ index ]->soundName;
         input.getline( textLine, 256 );   //skip past remainder of line

         if (fileIO::isFileReadable( this->soundFiles[ index ]->fileName ) ) 
         {
            vprDEBUG(vprDBG_ALL,0) << "\tsound fileName = " 
                                   << this->soundFiles[ index ]->fileName
                                   << std::endl << vprDEBUG_FLUSH;
         }
         else
         {
            std::cerr << "ERROR: unreadable sound file = " 
                      << this->soundFiles[ index ]->fileName 
                      << ".  You may need to correct your param file."
                      << std::endl;
            exit(1);
         }
      }
      else
      {
         // Skip past block
         this->_readParam->ContinueRead( input, id );
      }
   }
}

