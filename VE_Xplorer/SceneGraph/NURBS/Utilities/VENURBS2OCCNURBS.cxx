/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 * Date modified: $Date:  $
 * Version:       $Rev:  $
 * Author:        $Author:  $
 * Id:            $Id:  $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/SceneGraph/NURBS/Utilities/VENURBS2OCCNURBS.h"
#include "VE_Xplorer/SceneGraph/NURBS/NSurface.h"
#include "VE_Xplorer/SceneGraph/NURBS/KnotVector.h"
#include "VE_Xplorer/SceneGraph/NURBS/ControlPoint.h"
#include <map>
#include <vector>
#include <iostream>

////////////////////////////////////////
///Constructor                        //
////////////////////////////////////////
VENURBS2OCCNURBS::VENURBS2OCCNURBS()
{
   _surfacePatch = 0;
}
/////////////////////////////////////////
///Destructor                          //
/////////////////////////////////////////
VENURBS2OCCNURBS::~VENURBS2OCCNURBS()
{
   if(_surfacePatch)
   {
      delete _surfacePatch;
   }
   _surfacePatch = 0;
}
////////////////////////////////////////////////////////////////////////////////
Handle_Geom_BSplineSurface VENURBS2OCCNURBS::GetOCCNURBSSurface( NURBS::NURBSSurface* veNURBSSurface )
{
   std::vector< std::vector<NURBS::ControlPoint> > points = veNURBSSurface->GetControlPoints();
   NURBS::KnotVector uKnotVector = _knotVectors[ "U" ];
   NURBS::KnotVector vKnotVector = _knotVectors[ "V" ];
   
   std::map< double , unsigned int > uKnots = uKnotVector.GetKnotMap();
   std::map< double , unsigned int > vKnots = vKnotVector.GetKnotMap();

   std::vector< unsigned int > uMultiplicity;
   std::vector< unsigned int > vMultiplicity;
   std::vector< double > uKnot;
   std::vector< double > vKnot;
   
   // get u info
   std::map< double , unsigned int >::iterator iter;
   for ( iter = uKnots.begin(); iter != uKnots.end(); ++iter )
   {
      uMultiplicity.push_back( iter.second );
      uKnot.push_back( iter.first );
   }

   // get v info
   for ( iter = vKnots.begin(); iter != vKnots.end(); ++iter )
   {
      vMultiplicity.push_back( iter.second );
      vKnot.push_back( iter.first );
   }

   return 0;
}

