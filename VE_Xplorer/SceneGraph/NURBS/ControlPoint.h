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
#ifndef VE_POINT_H
#define VE_POINT_H

/*!\file ControlPoint.h
  ControlPoint API
  */
/*!\class NURBS::ControlPoint
 * Class defining a Control Point for NURBS object.
 */
/*!\class NURBS::Point
 * Class defining a Control Point for NURBS object.
 */

/*!\namespace NURBS
 * NURBS API namespace.
 */
#include "VE_Installer/include/VEConfig.h"

#include <ostream>
namespace NURBS
{
class VE_NURBS_EXPORTS Point
{
public:
   ///Constructor
   Point();
   ///Constructor
   ///\param x First directional coordinate
   ///\param y Second directional coordinate
   ///\param z Third directional coordinate
   Point(double x,double y, double z);

   ///Copy constructor
   Point(const Point& rhs);

   ///Destructor
   virtual ~Point();

   ///Equal operator
   ///\param rhs The point to set this one to. 
   virtual Point& operator=(const Point& rhs);

   ///Translate the point by a delta
   void Translate(double dx,double dy, double dz);

   ///Set the x value.
   ///\param x The new value.
   void SetX(double x);

   ///Set the y value.
   ///\param y The new value.
   void SetY(double y);

   ///Set the z value.
   ///\param z The new value.
   void SetZ(double z);

   ///Get the value of the first directional coordinate.
   double X();

   ///Get the value of the second directional coordinate.
   double Y();

   ///Get the value of the third directional coordinate.
   double Z();

   ///Dot product
   inline double operator*(const Point& rhs) const
   {
      return _x*rhs._x + _y*rhs._x + _z*rhs._z;
   }

   ///Cross product. 
   inline const Point operator ^ (const Point& rhs) const
   {
      return Point(_y*rhs._z - _z*rhs._y,
                   _z*rhs._x - _x*rhs._z,
                   _x*rhs._y - _y*rhs._x);
   }
   ///override ostream operator
   inline friend std::ostream& operator<<(std::ostream& os,
                                          const Point& fpd)
   {
      os<<fpd._x<<" "<<fpd._y<<" "<<fpd._z<<" ";
      return os;
   }

   ///override ">" operator
   friend bool operator>(const Point& lhs,const Point& rhs)
   {
      if(lhs._x > rhs._x ||
         lhs._y > rhs._y ||
         lhs._z > rhs._z  )
      {
         return true;
      }
      return false;
   };
   ///addition operator
   Point operator+(const Point& lhs)
   {
      Point newPoint(lhs._x + _x,
                   lhs._y + _y,
                   lhs._z + _z);
      
      return newPoint;

   };

   ///multiplicaiton with a scalar operator
   Point operator*(const double& lhs)
   {
      Point newPoint(lhs*_x,
                   lhs*_y,
                   lhs*_z);
      
      return newPoint;

   };

protected:
   double _x;///<The raw coordianate data
   double _y;///<The raw coordianate data
   double _z;///<The raw coordianate data
};
class VE_NURBS_EXPORTS ControlPoint : public NURBS::Point
{
public:
   ///Constructor
   ControlPoint();
   ///Constructor
   ///\param x First directional coordinate
   ///\param y Second directional coordinate
   ///\param z Third directional coordinate
   ///\param w Weight of the control point.
   ControlPoint(double x, double y, double z, double w = 1.0);

   ///Copy constructor
   ControlPoint(const ControlPoint& rhs);

   ///Destructor
   virtual ~ControlPoint();

   ///Equal operator
   ///\param rhs The point to set this one to. 
   virtual ControlPoint& operator=(const ControlPoint& rhs);

   ///Set the weight of this control point.
   ///\param weight The weight of this control point
   void SetWeight(double weight);

   ///Get the weight of this point.
   double Weight();

   ///Returns P*weight
   NURBS::ControlPoint GetWeigthedPoint();

   ///Dot product
   inline double operator*(const ControlPoint& rhs) const
   {
      return _x*rhs._x + _y*rhs._x + _z*rhs._z;
   }

   ///Cross product. 
   inline const ControlPoint operator ^ (const ControlPoint& rhs) const
   {
      return ControlPoint(_y*rhs._z - _z*rhs._y,
                   _z*rhs._x - _x*rhs._z,
                   _x*rhs._y - _y*rhs._x);
   }
   ///override "<<"operator
   inline friend std::ostream& operator<<(std::ostream& os,
                                          const ControlPoint& fpd)
   {
      os<<fpd._x<<" "<<fpd._y<<" "<<fpd._z<<" "<<fpd._weight<<" ";
      return os;
   }

   ///override "+" operator
   ControlPoint operator+(const ControlPoint& lhs)
   {
      //not sure how to handle the weights here!!!
      ControlPoint newPoint(lhs._x + _x,
                            lhs._y + _y,
                            lhs._z + _z,
                            (lhs._weight+_weight)*.5f);
      
      return newPoint;

   };

   /// multiplication with a scalar
   ControlPoint operator*(const double& lhs)
   {
      //not sure how to handle the weights here!!!
      ControlPoint newPoint(lhs*_x,
                            lhs*_y,
                            lhs*_z,
                            _weight);
      
      return newPoint;

   };
   
protected:
   double _weight;///<The weight for this coordinate.
};
}
#endif //VE_POINT_H
