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
#ifndef NURBS_SURFACE_H
#define NURBS_SURFACE_H

/*!\file NSurface.h
  NURBS Surface API
  */
/*!\file NSurface.cxx
  NURBS Surface code
  */
/*!\class NURBS::NURBSSurface
 * Class defining a NURBSSurface. 
 */
#include <ves/VEConfig.h>
#include <vector>
#include <map>
#include <VE_Xplorer/SceneGraph/NURBS/KnotVector.h>
#include <VE_Xplorer/SceneGraph/NURBS/NURBSObject.h>
namespace NURBS
{
class Point;
class ControlPoint;

///???
class VE_NURBS_EXPORTS NURBSSurface : public NURBSObject
{
public:
   ///Constructor
   NURBSSurface(unsigned int uDegree=3,unsigned int vDegree=3);

   ///Copy constructor
   NURBSSurface(const NURBSSurface& rhs);

   ///Destructor
   virtual ~NURBSSurface();

   ///Equal operator
   ///\param rhs The point to set this one to. 
   NURBSSurface& operator=(const NURBSSurface& rhs);

   ///Interpolate the surface.
   virtual void Interpolate();

   ///???
   std::map<unsigned int, std::map<unsigned int,std::vector<NURBS::Point> > > GetSurfaceDerivatives();

   ///Write the surface out in the VE-NURBS format
   ///\param stream ostrem to write to
   void Write( std::ostream& stream );
protected:
   ///Interpolate with a range of values...\n
   ///Used internally for re-tessellation when moving control points
   ///\param uBounds The min[0] and max[1] u params to interpolate between
   ///\param vBounds The min[0] and max v[1] params to interpolate between
   virtual void _interpolateWithinBounds(double* uBounds,double* vBounds);

   ///Interpolate with a range of values...\n
   ///Used internally for re-tessellation when moving control points
   ///\param umin The min u param to interpolate between
   ///\param umax The max u param to interpolate between
   ///\param vmin The min v param to interpolate between
   ///\param vmax The max v param to interpolate between
   virtual void _interpolateWithinRange(double umin,double umax,
                                        double vmin,double vmax);

   ///Calculate a point on a surface and the derivatives
   ///\param uparameter The interpolating u parameter
   ///\param vparameter The interpolating v parameter
   ///\param uspan The knot u span to interpolate the parameter on
   ///\param vspan The knot v span to interpolate the parameter on
   std::map<unsigned int,std::vector<NURBS::ControlPoint> > _calculatePointOnSurface(double uparameter,
                                         double vparameter,
                                         unsigned int uspan,
                                         unsigned int vspan);

   
   std::map<unsigned int, std::map<unsigned int,std::vector<NURBS::Point> > > _surfDerivatives;///<The surfaceDerivatives.

};
}
#endif //VE_POINT_H
