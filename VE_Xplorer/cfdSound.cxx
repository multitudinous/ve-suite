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
 * Date modified: $Date: 2004/03/23 16:29:18 $
 * Version:       $Revision: 1.3 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdSound.h"
#include <string.h>
#include <iostream>
#include <stdlib.h>
#include <stdio.h>
#include <snx/sonix.h>
#include <snx/SoundHandle.h>
//using namespace sonix;

cfdSound::cfdSound()
{
   //sonix::instance()->changeAPI( "Stub" );
   sonix::instance()->changeAPI( "OpenAL" );
}

cfdSound::~cfdSound()
{
}

void cfdSound::initSound()
{
   sound.init( soundName );
   std::cout << "soundName" << soundName << std::endl;
   // sonix::instance()->changeAPI( "Stub" );
   snx::SoundInfo si;
   //si.filename = "/home/users/jhynek/VE_SUITE/VE_Xplorer/sol.wav";  
   si.filename = fileName;  
   std::cout << "fileName" << fileName << std::endl;
   si.ambient = false;
   si.datasource = snx::SoundInfo::FILESYSTEM;
   sound.configure( si );
   // sonix::instance()->changeAPI( "OpenAL" );
}
void cfdSound::playSound()
{
   //sonix::instance()->trigger(si);
   sound.trigger(-1);
}
void cfdSound::stopSound()
{
   sound.stop();
}
bool cfdSound::IsSounding()
{
   return sound.isPlaying();
}

