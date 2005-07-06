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
 * File:          $RCSfile: cfdSound.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFDSOUND_H
#define CFDSOUND_H

#include <snx/SoundHandle.h>
#include "VE_Installer/include/VEConfig.h"
namespace VE_Xplorer
{
   class VE_XPLORER_EXPORTS cfdSound
   { 
      public:
         cfdSound();
         ~cfdSound();

         void initSound();
         void playSound();
         void stopSound();
         bool IsSounding();

         char  fileName[ 100 ];
         char  soundName[ 100 ];    // this is displayed on the gui

         // sound is either ambient or positional.  If the sound is ambient, it is
         // attached to the listener, and its volume does not change when the
         // listener moves. If the sound is positional, the volume changes when the
         // listener moves
         int   ambient;

         // retriggerable specifies whether a currently playing sound restarts
         // from the beginning when triggered.
         int   retriggerable;  

         // Number of times to play: -1 to loop, 1 (single shot) is default.
         int   repeat;      

         float volume;  // between 0.0 and 1.0 inclusive, 1 is loud

         float pitchbend;
         // pitchbend alters the frequency of the sample.  1.0 is neutral.
         // A value less than 1.0 is low; a value greater than 1.0 is high.

         float cutoff;
         float soundPositionX;
         float soundPositionY;
         float soundPositionZ;
   
         snx::SoundHandle soundHandle;
         snx::SoundInfo si;
   };
}
#endif
