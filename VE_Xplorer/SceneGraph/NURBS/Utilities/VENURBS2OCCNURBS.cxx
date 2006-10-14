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
   ;
}
/////////////////////////////////////////
///Destructor                          //
/////////////////////////////////////////
VENURBS2OCCNURBS::~VENURBS2OCCNURBS()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
Geom_BSplineSurface* VENURBS2OCCNURBS::GetOCCNURBSSurface( NURBS::NURBSSurface* veNURBSSurface )
{
   NURBS::KnotVector uKnotVector = veNURBSSurface->KnotVector( "U" );
   NURBS::KnotVector vKnotVector = veNURBSSurface->KnotVector( "V" );
   std::map< double , unsigned int > uKnots = uKnotVector.GetKnotMap();
   std::map< double , unsigned int > vKnots = vKnotVector.GetKnotMap();

   TColStd_Array1OfInteger UMults( 1, uKnots.size() );
   TColStd_Array1OfReal UKnots( 1, uKnots.size() );
   // get u info
   std::map< double , unsigned int >::iterator iter;
   size_t count = 1;
   for ( iter = uKnots.begin(); iter != uKnots.end(); ++iter )
   {
      UMults( count ) = iter.second;
      UKnots( count ) = iter.first;
      count++;
   }

   // get v info
   TColStd_Array1OfReal VKnots( 1, vKnots.size() );
   TColStd_Array1OfInteger VMults( 1, vKnots.size() );
   count = 0;
   for ( iter = vKnots.begin(); iter != vKnots.end(); ++iter )
   {
      VMults( count ) = iter.second;
      VKnots( count ) = iter.first;
      count++;
   }

   //Get control points
   std::vector< std::vector<NURBS::ControlPoint> > points = veNURBSSurface->GetControlPoints();
   TColgp_Array2OfPnt Poles( 1, points.at( 0 ).size(), 1, points.size() );
   TColStd_Array2OfReal Weights( 1, points.at( 0 ).size(), 1, points.size() );
   for ( size_t j = 0; j < points.size(); ++j )
   {
      for ( size_t i = 0; i < points.at( j ).size(); ++i )
      {
         double X = points( j ).at( i ).X();
         double Y = points( j ).at( i ).Y();
         double Z = points( j ).at( i ).Z();
         Weights( i, j ) = points( j ).at( i ).Weight();
         Poles( i, j ).SetCoord( X, Y, Z );
      }
   }

   int UDegree = surface->GetDegree( "U" );
   int VDegree = surface->GetDegree( "V" );
   //Now create occ nurb surface
   Geom_BSplineSurface* surface = new Geom_BSplineSurface ( Poles,  Weights, 
                                                            UKnots, VKnots, 
                                                            UMults, VMults, 
                                                            UDegree, VDegree );
   return surface;
}

