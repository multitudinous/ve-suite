#ifndef NURBS_CURVE_H
#define NURBS_CURVE_H

/*!\file NCurve.h
  NURBS Curve API
  */
/*!\class NURBS::NURBSCurve
 * Class defining a NURBSCurve. 
 */
#include "VE_Installer/include/VEConfig.h"
#include "VE_Xplorer/SceneGraph/NURBS/NURBSObject.h"
#include <vector>
#include <map>
#include <VE_Xplorer/SceneGraph/NURBS/KnotVector.h>
namespace NURBS
{
class Point;
class ControlPoint;

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
   ///Calculate a point on a curve
   ///\param parameter The interpolating parameter
   ///\param span The knot span to interpolate the parameter on
   NURBS::Point _calculatePointOnCurve(double parameter, unsigned int span);
};
}
#endif //VE_POINT_H
