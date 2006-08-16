#ifndef NURBS_SURFACE_H
#define NURBS_SURFACE_H

/*!\file NSurface.h
  NURBS Surface API
  */
/*!\class NURBS::NURBSSurface
 * Class defining a NURBSSurface. 
 */
#include "VE_Installer/include/VEConfig.h"
#include <vector>
#include <map>
#include <VE_Xplorer/SceneGraph/NURBS/KnotVector.h>
#include <VE_Xplorer/SceneGraph/NURBS/NURBSObject.h>
namespace NURBS
{
class Point;
class ControlPoint;

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

   std::map<unsigned int, std::map<unsigned int,std::vector<NURBS::Point> > > GetSurfaceDerivatives();

protected:

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
