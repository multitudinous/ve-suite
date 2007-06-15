/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "cfdTransientInfo.h"
#include "cfdTransientSet.h"
#include "cfdDCS.h"

#include <iostream>
#include <vpr/Util/Debug.h>

cfdTransientInfo::cfdTransientInfo( )
{
   this->dcs = NULL;

   this->geometryDir = NULL;
   this->geometryDcs = NULL;

   this->trans = 0;  // transparency flag
   this->color = 0;  // is color specified ?

   this->duration = 0.0;
   this->numberOfFrames = 0;
}

cfdTransientInfo::~cfdTransientInfo( )
{
   vprDEBUG(vprDBG_ALL,2) << "Deleting cfdTransientInfo"
                          << std::endl << vprDEBUG_FLUSH;

   for ( int i=0; i<(int)this->transientSet.size(); i++ )
   {
      vprDEBUG(vprDBG_ALL,2) << "\tDeleting transientSet " << i 
                             << std::endl << vprDEBUG_FLUSH;
      delete this->transientSet[ i ];
   }
   this->transientSet.clear();
}

// get/set this dataset's DCS
cfdDCS* cfdTransientInfo::GetDCS()
{
   return this->dcs;
}

void cfdTransientInfo::SetDCS( cfdDCS * myDCS )
{
   this->dcs = myDCS;
}

cfdDCS * cfdTransientInfo::GetGeometryDCS()
{
   return this->geometryDcs;
}

void cfdTransientInfo::SetGeometryDCS( cfdDCS * myDCS )
{
   this->geometryDcs = myDCS;
}

char * cfdTransientInfo::GetGeometryDir()
{
   return this->geometryDir;
}

void cfdTransientInfo::LoadTransientSet( cfdTransientSet * tset )
{
   this->transientSet.push_back( tset );

   // compute the global scalar range...
   tset->ReadScalarRanges();

   // check that all directories have the same number of files...
   if ( this->numberOfFrames == 0 )
   {
      this->numberOfFrames = tset->GetNumberOfDataSets();
      vprDEBUG(vprDBG_ALL,2) << " cfdTransientInfo: numberOfFrames = "
                             << this->numberOfFrames
                             << std::endl << vprDEBUG_FLUSH;
   }
   else if ( this->numberOfFrames == tset->GetNumberOfDataSets() )
   {
      vprDEBUG(vprDBG_ALL,2) << " cfdTransientInfo: numberOfFrames = "
                             << this->numberOfFrames
                             << std::endl << vprDEBUG_FLUSH;
   }
   else
   {
      std:: cerr << "ERROR: the number of files in " << tset->GetDirectory() 
           << " does not match the expected value of " << this->numberOfFrames
           << std::endl;
      exit( 1 );
   }

}

int cfdTransientInfo::GetNumberOfTransSets()
{
   return (int)this->transientSet.size();
}

void cfdTransientInfo::SetGeometryDir( char * dir )
{
   this->geometryDir = dir;
}

cfdTransientSet * cfdTransientInfo::GetTransSet( int i )
{
   return transientSet[ i ];
}

cfdTransientSet * cfdTransientInfo::findTransientSetWithID( int id )
{
   for ( int i=0; i<(int)this->transientSet.size(); i++ )
   {
      if ( this->transientSet[ i ]->GetID() == id )
         return this->transientSet[ i ];
   }
   return NULL;
}

cfdTransientSet * cfdTransientInfo::GetFlowdataTransSet()
{
   return findTransientSetWithID( 0 );
}

cfdTransientSet * cfdTransientInfo::Get_X_planeTransSet()
{
   return findTransientSetWithID( 1 );
}

cfdTransientSet * cfdTransientInfo::Get_Y_planeTransSet()
{
   return findTransientSetWithID( 2 );
}

cfdTransientSet * cfdTransientInfo::Get_Z_planeTransSet()
{
   return findTransientSetWithID( 3 );
}

cfdTransientSet * cfdTransientInfo::GetDropletTransSet()
{
   return findTransientSetWithID( 4 );
}

double cfdTransientInfo::GetDuration()
{
   return this->duration;
}

void cfdTransientInfo::SetDuration( double dur )
{
   this->duration = dur;
}

