/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2004 by Iowa State University
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
 * File:          $RCSfile: cfdAnimation.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFDANIMATION_H
#define CFDANIMATION_H

class cfdTransientFlowManager;
class cfdTempAnimation;
class cfdGroup;

#include <vector>

class cfdAnimation
{
   public:
      cfdAnimation();

      ~cfdAnimation();
   
      void AddAFlowManager( cfdTransientFlowManager * );
      
      //set the duration (in seconds) of the sequence
      void SetDuration( double time = 1.0 );

      // add pfGroups to the sequence node 
      void SetpfGroups( void );
      
      //get a pointer to the animation
      cfdTempAnimation* GetSequence( void );
      
      cfdGroup* GetpfGroup( int i );

      int GetNumberOfFrames( void ){ return numFrames;}

   private:
      int numFrames;
      double _duration;

      cfdTempAnimation* sequence;

      cfdGroup** groups;

      std::vector< cfdTransientFlowManager * > flowManagers;

      cfdAnimation(const cfdAnimation& src);
      cfdAnimation& operator=(const cfdAnimation& src);
};
#endif // CFDANIMATION_H
