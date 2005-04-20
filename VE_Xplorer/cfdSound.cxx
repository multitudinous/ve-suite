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
 * File:          $RCSfile: cfdSound.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdSound.h"
#include <snx/sonix.h>
#include <vpr/Util/Debug.h>

cfdSound::cfdSound()
{
   this->repeat = 1; // not currently listed in parameter file
}

cfdSound::~cfdSound()
{
}

void cfdSound::initSound()
{
   // populate a SoundInfo struct...
   si.alias = this->soundName;
   si.datasource = snx::SoundInfo::FILESYSTEM;
   si.filename = this->fileName;  
   si.ambient = this->ambient;
   si.retriggerable = this->retriggerable;
   si.repeat = this->repeat;
   si.pitchbend = this->pitchbend;
   si.cutoff = this->cutoff;
   si.volume = this->volume;
   si.streaming = false;
   si.triggerOnNextBind = false;
   si.repeatCountdown = 0;
   si.position[ 0 ] = this->soundPositionX;
   si.position[ 1 ] = this->soundPositionY;
   si.position[ 2 ] = this->soundPositionZ;

   // create the sound object...
   this->soundHandle.init( this->soundName );
   this->soundHandle.configure( si );

   vprDEBUG(vprDBG_ALL,0) << "\tcfdSound:  fileName: " << this->fileName
                          << std::endl << vprDEBUG_FLUSH;
   vprDEBUG(vprDBG_ALL,0) << "\tcfdSound:    volume: " << this->volume
                          << std::endl << vprDEBUG_FLUSH;
   vprDEBUG(vprDBG_ALL,1) << "\tcfdSound: soundName: " << this->soundName
                          << std::endl << vprDEBUG_FLUSH;}

void cfdSound::playSound()
{
   this->soundHandle.trigger( si.repeat );
}

void cfdSound::stopSound()
{
   this->soundHandle.stop();
}

bool cfdSound::IsSounding()
{
   return this->soundHandle.isPlaying();
}

