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
#ifndef NURBS_CURVE_H
#define NURBS_CURVE_H

/*!\file NCurve.h
  NURBS Curve API
  */
/*!\file NCurve.cxx
  NURBS Curve Code
  */
/*!\class NURBS::NURBSCurve
 * Class defining a NURBSCurve. 
 */
#include <ves/VEConfig.h>
#include <ves/xplorer/scenegraph/NURBS/NURBSObject.h>
#include <vector>
#include <map>
#include <VE_Xplorer/SceneGraph/NURBS/KnotVector.h>
namespace NURBS
{
class Point;
class ControlPoint;

///???
class VE_NURBS_EXPORTS NURBSCurve : public NURBSObject
{
public:
   ///Constructor
   NURBSCurve(unsigned int degree=3);

   ///Copy constructor
   NURBSCurve(const NURBSCurve& rhs);

   ///Destructor
   virtual ~NURBSCurve();

   ///Equal operator
   ///\param rhs The point to set this one to. 
   NURBSCurve& operator=(const NURBSCurve& rhs);

   ///Interpolate the curve.
   virtual void Interpolate();

protected:
  

   ///Interpolate with a range of values...\n
   ///Used internally for re-tessellation when moving control points
   ///\param umin The min u param to interpolate between
   ///\param umax The max u param to interpolate between
   ///\param vmin The min v param to interpolate between
   ///\param vmax The max v param to interpolate between
   virtual void _interpolateWithinRange(double umin,double umax,
                                        double vmin,double vmax);

   ///Calculate a point and it's derivatives on a curve
   ///\param parameter The interpolating parameter
   ///\param span The knot span to interpolate the parameter on
   std::vector<NURBS::ControlPoint> _calculatePointOnCurve(double parameter, unsigned int span);
};
}
#endif //VE_POINT_H
