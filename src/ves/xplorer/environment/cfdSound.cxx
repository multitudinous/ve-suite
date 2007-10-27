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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <ves/xplorer/environment/cfdSound.h>
#include <snx/sonix.h>
#include <ves/xplorer/cfdDebug.h>
#include <ves/xplorer/util/fileIO.h>

using namespace VE_Xplorer;
////////////////////
cfdSound::cfdSound()
:ambient(true),
retriggerable(true),
repeat(-1),
pitchbend(1),
cutoff(1),
volume(.5),
soundPositionX(0),
soundPositionY(0),
soundPositionZ(0)
{
   si.datasource = snx::SoundInfo::FILESYSTEM;
   si.streaming =false;
}
///////////////////////////////////////
cfdSound::cfdSound(const cfdSound& rhs)
:ambient(rhs.ambient),
retriggerable(rhs.retriggerable),
repeat(rhs.repeat),
pitchbend(rhs.pitchbend),
cutoff(rhs.cutoff),
volume(rhs.volume),
soundPositionX(rhs.soundPositionX),
soundPositionY(rhs.soundPositionY),
soundPositionZ(rhs.soundPositionZ)
{
   si.datasource = rhs.si.datasource;
   si.streaming = rhs.si.streaming;
}
/////////////////////
cfdSound::~cfdSound()
{
}
//////////////////////////
bool cfdSound::initSound()
{
   if(soundName.empty())
   {
      std::cout<<"Error!!"<<std::endl;
      std::cout<<"Sound name not specified!"<<std::endl;
      std::cout<<"cfdSound::initSound"<<std::endl;
      return false;
   }
   if(fileName.empty())
   {
      std::cout<<"Error!!"<<std::endl;
      std::cout<<"Filename not specified!"<<std::endl;
      std::cout<<"cfdSound::initSound"<<std::endl;
      return false;
   }
   if ( ves::xplorer::util::fileIO::isFileReadable( fileName ) ) 
   {
       vprDEBUG(vesDBG,0) << "\tsound fileName = " 
                          << fileName
                          << std::endl << vprDEBUG_FLUSH;
   }
   else
   {
      vprDEBUG(vesDBG,0) << "ERROR: unreadable sound file = " 
                         << fileName 
                         << ".  cfdSound::initSound."
                         << std::endl << vprDEBUG_FLUSH;
      return false;
   }
   // populate a SoundInfo struct...
   si.alias = this->soundName;
   //si.datasource = snx::SoundInfo::FILESYSTEM;
   si.filename = this->fileName;  
   si.ambient = this->ambient;
   si.retriggerable = this->retriggerable;
   si.repeat = this->repeat;
   si.pitchbend = this->pitchbend;
   si.cutoff = this->cutoff;
   si.volume = this->volume;
   si.position[ 0 ] = this->soundPositionX;
   si.position[ 1 ] = this->soundPositionY;
   si.position[ 2 ] = this->soundPositionZ;

   // create the sound object...
   this->soundHandle.init( this->soundName );
   this->soundHandle.configure( si );

   vprDEBUG(vesDBG,0) << "\tcfdSound:  fileName: " << this->fileName
                          << std::endl << vprDEBUG_FLUSH;
   vprDEBUG(vesDBG,0) << "\tcfdSound:    volume: " << this->volume
                          << std::endl << vprDEBUG_FLUSH;
   vprDEBUG(vesDBG,1) << "\tcfdSound: soundName: " << this->soundName
                          << std::endl << vprDEBUG_FLUSH;
   // start the sonix system with desired audio library...
   snx::sonix::instance()->changeAPI( "OpenAL" );
   return true;
}
//////////////////////////
void cfdSound::playSound()
{
   this->soundHandle.trigger( si.repeat );
}
//////////////////////////
void cfdSound::stopSound()
{
   this->soundHandle.stop();
}
///////////////////////////
bool cfdSound::IsSounding()
{
   return this->soundHandle.isPlaying();
}
///////////////////////////////////////////////////
cfdSound& cfdSound::operator =(const cfdSound& rhs)
{
   if(this != &rhs)
   {
      fileName = rhs.fileName;
      soundName = rhs.soundName;
      ambient = rhs.ambient;
      retriggerable = rhs.retriggerable;
      repeat = rhs.repeat;
      pitchbend = rhs.pitchbend;
      cutoff = rhs.cutoff;
      volume = rhs.volume;
      soundPositionX = rhs.soundPositionX;
      soundPositionY = rhs.soundPositionY;
      soundPositionZ = rhs.soundPositionZ ;
      si = rhs.si;
      soundHandle = rhs.soundHandle;
   }
   return *this;
}

