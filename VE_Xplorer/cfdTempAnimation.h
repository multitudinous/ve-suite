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
 * File:          $RCSfile: cfdTempAnimation.h,v $
 * Date modified: $Date: 2004-07-26 16:27:49 -0500 (Mon, 26 Jul 2004) $
 * Version:       $Rev: 719 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFDTEMPANIMATION_H
#define CFDTEMPANIMATION_H

class cfdSequence;
class cfdGroup;
class cfdGeode;

#include <vector>

class cfdTempAnimation
{
   public:
      cfdTempAnimation();

      ~cfdTempAnimation();
   
      //set the duration (in seconds) of the sequence
      void SetDuration( double time = 1.0 );

      // add pfGroups to the sequence node 
      void SetpfGroups( void );
      
      //get a pointer to the animation
      cfdSequence* GetSequence( void );
      
      void SetSequence( cfdSequence* );

      cfdGroup* GetpfGroup( int i );

      int GetNumberOfFrames( void ){ return numFrames;}

      void SetNumberOfFrames( int );

      void StopSequence( void );

      void StartSequence( void );

      void ResumeSequence( void );

      void PauseSequence( void );

      void ReverseSequence( void );

      void ForwardSequence( void );

      int GetFrameOfSequence( void );

      void AddToSequence( int );

      void ClearSequence( void );

   private:
      int numFrames;
      double _duration;

      cfdSequence* _sequence;
      std::vector< cfdGeode* > _geodes;

      cfdGroup** groups;

      cfdTempAnimation(const cfdTempAnimation& src);
      cfdTempAnimation& operator=(const cfdTempAnimation& src);
};
#endif // cfdTempAnimation_H
