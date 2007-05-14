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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef VE_NURBS_2_OCC_NURBS_H
#define VE_NURBS_2_OCC_NURBS_H
/*!\file VENURBS2OCCNURBS.h
  VENURBS2OCCNURBS API
  */
/*!\class VENURBS2OCCNURBS
 */
#include "VE_Xplorer/SceneGraph/NURBS/NSurface.h"
#include "VE_Xplorer/SceneGraph/NURBS/KnotVector.h"
#include "VE_Xplorer/SceneGraph/NURBS/ControlPoint.h"
#include <map>
#include <vector>
#include <iostream>

#include <TColStd_Array1OfInteger.hxx>
#include <TColStd_Array1OfReal.hxx>
#include <TColStd_Array2OfReal.hxx>
#include <TColgp_Array2OfPnt.hxx>
#include <Geom_BSplineSurface.hxx>

#include <VE_Installer/include/VEConfig.h>
namespace NURBS
{
namespace Utilities
{
///???
class VE_NURBS_UTILS_EXPORTS VENURBS2OCCNURBS
{
public:
   ///Constructor
   VENURBS2OCCNURBS(){ ; }
   ///Destructor
   ~VENURBS2OCCNURBS(){ ; }

   ///Get a VE NURBS patch and return an OCC BSpline Surface
   ///\param veNURBSSurface NURBS surface to be converted
   Geom_BSplineSurface* GetOCCNURBSSurface( NURBS::NURBSSurface* veNURBSSurface )
   {
      NURBS::KnotVector uKnotVector = veNURBSSurface->KnotVector( "U" );
      NURBS::KnotVector vKnotVector = veNURBSSurface->KnotVector( "V" );
      std::vector< unsigned int > uMultiplicity = uKnotVector.GetMultiplicityVector();
      std::vector< double > uKnots = uKnotVector.GetDistinctKnotVector();
      std::vector< unsigned int > vMultiplicity = vKnotVector.GetMultiplicityVector();
      std::vector< double > vKnots = vKnotVector.GetDistinctKnotVector();
      
      TColStd_Array1OfInteger UMults( 1, uKnots.size() );
      TColStd_Array1OfReal UKnots( 1, uKnots.size() );
      // get u info
      //std::map< double , unsigned int >::iterator iter;
      size_t count = 1;
      for ( size_t i = 0; i < uKnots.size(); ++i )
      {
         UMults( count ) = uMultiplicity.at( i );
         UKnots( count ) = uKnots.at( i );
         count++;
      }
      
      // get v info
      TColStd_Array1OfReal VKnots( 1, vKnots.size() );
      TColStd_Array1OfInteger VMults( 1, vKnots.size() );
      count = 1;
      for ( size_t i = 0; i < vKnots.size(); ++i )
      {
         VMults( count ) = vMultiplicity.at( i );
         VKnots( count ) = vKnots.at( i );
         count++;
      }
      
      //Get control points
      std::vector< std::vector<NURBS::ControlPoint> > points = veNURBSSurface->GetControlPoints();
      TColgp_Array2OfPnt Poles( 1, points.at( 0 ).size(), 1, points.size() );
      TColStd_Array2OfReal Weights( 1, points.at( 0 ).size(), 1, points.size() );
      double X = 0;
      double Y = 0;
      double Z = 0;   
      for ( size_t j = 0; j < points.size(); ++j )
      {
         for ( size_t i = 0; i < points.at( j ).size(); ++i )
         {
            X = points.at( j ).at( i ).X();
            Y = points.at( j ).at( i ).Y();
            Z = points.at( j ).at( i ).Z();
            Weights( i, j ) = points.at( j ).at( i ).Weight();
            Poles( i, j ).SetCoord( X, Y, Z );
         }
      }
      
      int UDegree = veNURBSSurface->GetDegree( "U" );
      int VDegree = veNURBSSurface->GetDegree( "V" );
      //Now create occ nurb surface
      Geom_BSplineSurface* surface = new Geom_BSplineSurface ( Poles,  Weights, 
                                                               UKnots, VKnots, 
                                                               UMults, VMults, 
                                                               UDegree, VDegree );
      return surface;
   }
};
}
}
#endif //VE_NURBS_2_OCC_NURBS_H

