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
 * File:          $RCSfile: cfdAnimation.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdAnimation.h"
#include "cfdTransientFlowManager.h"

#include <iostream>

#ifndef _USE_CFD_SEQUENCE
#include <Performer/pf/pfSequence.h>
#else
#include "cfdSequence.h"
#endif

#include <Performer/pf/pfGroup.h>

#include <vpr/Util/Debug.h>

cfdAnimation::cfdAnimation()
{
#ifndef _USE_CFD_SEQUENCE
   this->sequence = new pfSequence();
#else
   this->sequence = new cfdSequence();
#endif
   this->numFrames = 0;
   this->groups = NULL;
}

cfdAnimation::~cfdAnimation()
{
   // This Delete also takes care of this->groups
   pfDelete( this->sequence );
   
   int num = this->flowManagers.size();
   
   for ( int i = 0; i < num; i++ )
   {
      delete this->flowManagers[ i ];
   }
   
   if ( num > 0 )
   {
      this->flowManagers.clear();
   }
}

void cfdAnimation::AddAFlowManager( cfdTransientFlowManager *manager )
{
   this->flowManagers.push_back( manager );
}

// create pfGroups and add to the pfSequence node
void cfdAnimation::SetpfGroups( void )
{
   // This function is called after the various flowManagers
   // are added to the vector of flowManagers.
   // And after the optional SetDuration function is called.

   // This function can only be called once...
   if ( this->groups != NULL )
   {
      std::cerr << "Error: SetpfGroups can only be called once" << std::endl;
      exit ( 1 );
   }

   // get the number of frames from the first flowManager 
   // (all flowMangers will have the same number of frames)
   if ( this->flowManagers.size() == 0 )
   {
      std::cerr << "Error: No flowMangers have been defined yet" << std::endl;
      exit ( 1 );
   }
   this->numFrames = this->flowManagers[ 0 ]->GetNumberOfFrames();

   vprDEBUG(vprDBG_ALL,1) << " Making groups" << std::endl << vprDEBUG_FLUSH;
   this->groups = new pfGroup*[ this->numFrames ];
   for ( int i = 0; i < this->numFrames; i++)
   {
      this->groups[ i ] = new pfGroup();
      this->sequence->addChild( this->groups[ i ] );
#ifndef _USE_CFD_SEQUENCE
      this->sequence->setTime( i, this->_duration / this->numFrames );
#endif
   }
#ifndef _USE_CFD_SEQUENCE
   this->sequence->setInterval( PFSEQ_CYCLE, 0 , this->numFrames - 1 );
   this->sequence->setDuration( 1.0, -1 );
#else
   this->sequence->setInterval( CFDSEQ_CYCLE, 0 , this->numFrames - 1 );
   this->sequence->setDuration( this->_duration );
#endif
}

// set the duration of the sequence (in seconds)
void cfdAnimation::SetDuration( double time )
{
   this->_duration = time;
}

pfGroup* cfdAnimation::GetpfGroup( int i )
{
   return this->groups[ i ];
}

#ifndef _USE_CFD_SEQUENCE
pfSequence* cfdAnimation::GetpfSequence( void )
#else
cfdSequence* cfdAnimation::GetpfSequence( void )
#endif
{
   return this->sequence;
}
