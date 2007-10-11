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
#ifndef OCC_NURBS_2_VE_NURBS_H
#define OCC_NURBS_2_VE_NURBS_H
/*!\file OCCNURBS2VENURBS.h
  OCCNURBS2VENURBS API
  */
/*!\class OCCNURBS2VENURBS
 */
#include <ves/xplorer/scenegraph/nurbs/NSurface.h>
#include <ves/xplorer/scenegraph/nurbs/KnotVector.h>
#include <ves/xplorer/scenegraph/nurbs/ControlPoint.h>

#include <Geom_BSplineSurface.hxx>
#include <TColStd_Array1OfReal.hxx>
#include <TColStd_Array1OfInteger.hxx>
#include <TColStd_Array2OfReal.hxx>
#include <TColgp_Array2OfPnt.hxx>

#include <ves/VEConfig.h>
namespace NURBS
{
namespace Utilities
{
///???
class VE_NURBS_UTILS_EXPORTS OCCNURBS2VENURBS
{
public:
   ///Constructor
   OCCNURBS2VENURBS(){ ; }
   ///Destructor
   ~OCCNURBS2VENURBS(){ ; }

   ///Get a OCC NURBS patch and return an VE NURBS Surface
   ///\param occNURBSSurface NURBS surface to be converted
   NURBS::NURBSSurface* GetVENURBSSurface( Handle_Geom_BSplineSurface occNURBSSurface )
   {
      NURBS::KnotVector uKnots;      
      // get the know uknots
      TColStd_Array1OfReal Ku( 1, occNURBSSurface->NbUKnots() );
      occNURBSSurface->UKnots( Ku );
      //Get u multiplicities
      TColStd_Array1OfInteger Mu( 1, occNURBSSurface->NbUKnots() );
      occNURBSSurface->UMultiplicities( Mu );
      for ( size_t i = Ku.Lower(); i <= Ku.Upper(); ++i )
      {
         for ( size_t j = 1; j <= Mu( i ); ++j )
         {
            uKnots.AddKnot( Ku( i ) );
         }
      }
      
      NURBS::KnotVector vKnots;      
      // get the vknots
      TColStd_Array1OfReal Kv( 1, occNURBSSurface->NbVKnots() );
      occNURBSSurface->VKnots( Kv );
      //Get v multiplicities
      TColStd_Array1OfInteger Mv( 1, occNURBSSurface->NbVKnots() );
      occNURBSSurface->VMultiplicities( Mv );
      for ( size_t i = Kv.Lower(); i <= Kv.Upper(); ++i )
      {
         for ( size_t j = 1; j <= Mv( i ); ++j )
         {
            vKnots.AddKnot( Kv( i ) );
         }
      }
      
      // get the weights
      TColStd_Array2OfReal W( 1, occNURBSSurface->NbUPoles(), 1, occNURBSSurface->NbVPoles() );
      occNURBSSurface->Weights( W );
      // get poles/control
      TColgp_Array2OfPnt Poles( 1, occNURBSSurface->NbUPoles(), 1, occNURBSSurface->NbVPoles() );
      occNURBSSurface->Poles( Poles );
      std::vector<NURBS::ControlPoint> surfaceCtrlPts;
      unsigned int nU = occNURBSSurface->NbUPoles();
      unsigned int nV = occNURBSSurface->NbVPoles();
      double x = 0;
      double y = 0;
      double z = 0;
      double w = 0;
      //This for loop may be wrong because the occ is row then column
      for ( size_t j = W.LowerCol(); j <= W.UpperCol(); ++j )
      {
         for ( size_t i = W.LowerRow(); i <= W.UpperRow(); ++i )
         {
            x = Poles( i, j ).X();
            y = Poles( i, j ).Y(); 
            z = Poles( i, j ).Z();
            w = W( i, j );
            surfaceCtrlPts.push_back(NURBS::ControlPoint(x,y,z,w));
         }
      }
      
      ///User responsible for deleting memory!!!
      NURBS::NURBSSurface* surfacePatch = new NURBS::NURBSSurface();
      surfacePatch->SetControlPoints(surfaceCtrlPts,nU,nV);
      surfacePatch->SetKnotVector(uKnots,"U");
      surfacePatch->SetKnotVector(vKnots,"V");
      
      return surfacePatch;
   }
};
}
}
#endif //OCC_NURBS_2_VE_NURBS_H

