#ifndef THIRD_EYE_POINT_H
#define THIRD_EYE_POINT_H

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

   inline friend std::ostream& operator<<(std::ostream& os,
                                          const Point& fpd)
   {
      os<<fpd._x<<" "<<fpd._y<<" "<<fpd._z<<" ";
      return os;
   }

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
   Point operator+(const Point& lhs)
   {
      Point newPoint(lhs._x + _x,
                   lhs._y + _y,
                   lhs._z + _z);
      
      return newPoint;

   };

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

   inline friend std::ostream& operator<<(std::ostream& os,
                                          const ControlPoint& fpd)
   {
      os<<fpd._x<<" "<<fpd._y<<" "<<fpd._z<<" "<<fpd._weight<<" ";
      return os;
   }

   ControlPoint operator+(const ControlPoint& lhs)
   {
      //not sure how to handle the weights here!!!
      ControlPoint newPoint(lhs._x + _x,
                            lhs._y + _y,
                            lhs._z + _z,
                            (lhs._weight+_weight)*.5f);
      
      return newPoint;

   };

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
#endif //THIRD_EYE_POINT_H
