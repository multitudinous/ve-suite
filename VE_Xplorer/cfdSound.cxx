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
#include <string>
#include <iostream>
#include <cstdlib>
#include <cstdio>
#include <snx/sonix.h>
#include <vpr/Util/Debug.h>

cfdSound::cfdSound()
{
   //snx::sonix::instance()->changeAPI( "Stub" );
   //////snx::sonix::instance()->changeAPI( "OpenAL" );
}

cfdSound::~cfdSound()
{
}

void cfdSound::initSound()
{
   this->sound.init( this->soundName );
   vprDEBUG(vprDBG_ALL,1) << "\tcfdSound: soundName = " << this->soundName
                          << std::endl << vprDEBUG_FLUSH;

   //snx::sonix::instance()->changeAPI( "Stub" );
   snx::SoundInfo si;
   si.filename = fileName;  
   vprDEBUG(vprDBG_ALL,1) << "\tcfdSound: fileName = " << fileName
                          << std::endl << vprDEBUG_FLUSH;
   si.ambient = false;
   si.datasource = snx::SoundInfo::FILESYSTEM;
   this->sound.configure( si );
   //snx::sonix::instance()->changeAPI( "OpenAL" );
}

void cfdSound::playSound()
{
   //snx::sonix::instance()->trigger(si);
   this->sound.trigger(-1);
}

void cfdSound::stopSound()
{
   this->sound.stop();
}

bool cfdSound::IsSounding()
{
   return this->sound.isPlaying();
}

