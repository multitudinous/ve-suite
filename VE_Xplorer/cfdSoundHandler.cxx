/*************** <auto-copyright.pl BEGIN do not edit this line> **************

 * VE-Suite is (C) Copyright 1998-2004 by Iowa State University
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
 * File:          $RCSfile: cfdSound.cxx,v $
 * Date modified: $Date: 2004-05-18 13:44:18 -0700 (Tue, 18 May 2004) $
 * Version:       $Rev: 382 $
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
   _readParam = new cfdReadParam( NULL );

   if ( this->soundFile ) 
   {
      std::cout << "| Special: Initializing................................... cfdSound |" << std::endl;

      sonix::instance()->changeAPI( "OpenAL" );

      //this->sounds[i]->initApi();
      // this->sounds = new cfdSound[this->paramReader->soundFile];
      for(int i = 0; i < this->soundFile; i++)
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
         this->sounds[i]->initSound();
         
         std::cout << "Success  fileName: " << this->sounds[i]->fileName << std::endl;
         std::cout << "Success    volume: " << this->sounds[i]->volume << std::endl;
         std::cout << "Success soundName: " << this->sounds[i]->soundName << std::endl;
      }
   }
}

cfdSoundHandler::~cfdSoundHandler( void )
{
}

bool cfdSoundHandler::CheckCommandId( cfdCommandArray* commandArray )
{
   if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == UPDATE_SOUNDS )
   {
      long int test = this->_readParam->convertDecimalToBinary( commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) );
      vprDEBUG(vprDBG_ALL,1)
         << " test : " << test << " : Number of Sound Files :" 
         << this->soundFile << vprDEBUG_FLUSH;

      this->_readParam->convertBinaryToArray( test, this->soundFile );

      for(int i = 0; i < this->soundFile; i++)
      {  // need to fix the gui value thing
         if ( this->sounds[i]->IsSounding() && _readParam->guiVal[ i ] == 0 ) 
         {
            this->sounds[i]->stopSound();  //Stop sound stuff here
         }              
         else
         {
            this->sounds[i]->playSound();   //Start sound again
         } 
      }      

      return true;
   }
   return false;
}

void cfdSoundHandler::UpdateCommand()
{
   cerr << "doing nothing in cfdVectorBase::UpdateCommand()" << endl;
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

   vprDEBUG(vprDBG_ALL,1) << " Number of Obejcts in Interactive Geometry : " << numObjects << std::endl  << vprDEBUG_FLUSH;
   for( int i = 0; i < numObjects; i++ )
   {
      int id;
      input >> id;
      vprDEBUG(vprDBG_ALL,1) << "Id of object in Interactive Geometry : " << id << std::endl << vprDEBUG_FLUSH;
      input.getline( text, 256 );   //skip past remainder of line
      if ( id == 11 )
      {
         soundFile = soundFile + 1;
         soundFiles.push_back( new fileInfo() );
         int size = soundFiles.size() - 1;

         //ambient
         input >> soundFiles[ size ]->ambient;
         input.getline( textLine, 256 );   //skip past remainder of line
         //retriggerable
         input >> soundFiles[ size ]->retriggerable;
         input.getline( textLine, 256 );   //skip past remainder of line
         //volume
         input >> soundFiles[ size ]->volume;
         input.getline( textLine, 256 );   //skip past remainder of line
         //pitchbend
         input >> soundFiles[ size ]->pitchbend;
         input.getline( textLine, 256 );   //skip past remainder of line
         //cutoff
         input >> soundFiles[ size ]->cutoff;
         input.getline( textLine, 256 );   //skip past remainder of line
         //PositionX
         input >> soundFiles[ size ]->soundPositionX;
         input.getline( textLine, 256 );   //skip past remainder of line   
         //PositionY
         input >> soundFiles[ size ]->soundPositionY;
         input.getline( textLine, 256 );   //skip past remainder of line   
         //PositionZ
         input >> soundFiles[ size ]->soundPositionZ;
         input.getline( textLine, 256 );   //skip past remainder of line   
         //filename
         input >> soundFiles[ size ]->fileName;
         input.getline( textLine, 256 );   //skip past remainder of line
         //Sound name
         input >> soundFiles[ size ]->soundName;
         input.getline( textLine, 256 );   //skip past remainder of line

         if (fileIO::isFileReadable( soundFiles[ size ]->fileName ) ) 
         {
            std::cout << "   fileName= " << soundFiles[ size ]->fileName << std::endl;
         }
         else
         {
            std::cerr << "ERROR: unreadable sound file = " 
                        << soundFiles[ size ]->fileName 
                        << ".  You may need to correct your param file." << std::endl;
            exit(1);
         }
      }
      else
      {
         // Skip past block
         _readParam->ContinueRead( input, id );
      }
   }
}


