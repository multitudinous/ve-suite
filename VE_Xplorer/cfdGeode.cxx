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
 * File:          $RCSfile: cfdGeode.cxx,v $
 * Date modified: $Date: 2004/03/23 16:29:15 $
 * Version:       $Revision: 1.6 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdGeode.h"
#include <Performer/pf.h>
#include <vpr/Util/Debug.h>
#include <iostream>


void cfdGeodeSetsUpdate( pfGeoSet *gsets[], 
			 pfGeode *geode )
{
   int numGSets = geode->getNumGSets( );

   vprDEBUG(vprDBG_ALL, 0) << "Memory used: " << pfMemory::getArenaBytesUsed()
                           << std::endl << vprDEBUG_FLUSH;

   if ( numGSets != 0 )
   {
      for ( int i=0; i<numGSets; i++ ) 
      {
         pfGeoSet *g = geode->getGSet( 0 ); // children shift after a removal
         geode->removeGSet( g );
         pfDelete(g);
      }
   }

   for ( int i=0; i<4; i++ )  
   {
      if ( gsets[i] != NULL )	
      {  
         geode->addGSet( gsets[i] );
      }
   }
}

void cfdGeodeSetsFlush( pfGeoSet *gsets[],pfGeode *geode )
{
   int numGSets = geode->getNumGSets( );

   if ( numGSets != 0 )
   {
      pfGeoSet *g = geode -> getGSet(numGSets-1);
      geode->replaceGSet( g,gsets[0] );
      pfDelete(g);
   }
}

void cfdGeodeSetsDelete( pfGeode *geode )
{
   int numGSets = geode->getNumGSets( );

   vprDEBUG(vprDBG_ALL, 1) << "numGSets: " << numGSets 
                           << std::endl << vprDEBUG_FLUSH;

   if ( numGSets != 0 )
   {
      for ( int i=0; i<numGSets; i++ ) 
      {
         pfGeoSet *g = geode->getGSet( 0 ); // children shift after a removal
         geode->removeGSet( g );
         pfDelete(g);
      }
   }
}
