#ifndef VE_NURBS_OBJECT_H
#define VE_NURBS_OBJECT_H

/*!\file NURBSObject.h
  NURBS Surface API
  */
/*!\class NURBS::NURBSObject
 * Base class defining a NURBS representation. 
 */
#include "VE_Installer/include/VEConfig.h"
#include <vector>
#include <map>
#include <string>
#include <VE_Xplorer/SceneGraph/NURBS/KnotVector.h>
namespace NURBS
{
class Point;
class ControlPoint;

class VE_NURBS_EXPORTS NURBSObject
{
public:
   enum Type
   {
      Curve,
      Surface
   };
   ///Constructor
   NURBSObject(Type type=Curve,unsigned int uDegree=3,unsigned int vDegree=3);

   ///Copy constructor
   NURBSObject(const NURBSObject& rhs);

   ///Destructor
   virtual ~NURBSObject();

   ///Equal operator
   ///\param rhs The point to set this one to. 
   NURBSObject& operator=(const NURBSObject& rhs);

   ///Set the vDegree of the NURBSObject.
   ///\param degree Set the degree of the NURBSObject.
   ///\param direction "U" or "V" direction
   void SetDegree(unsigned int degree = 3,std::string direction="U");

   ///Set the vKnot vector for this surface.
   ///\param knots The KnotVector for this surface.
   ///\param direction "U" or "V" direction
   void SetKnotVector(NURBS::KnotVector knots, std::string direction="U");

   ///Set the ControlPoint s for this NURBSObject.\n
   ///uSize and vSize are at least 1.
   ///1 <= uSize, 1 == vSize: NURBSCurve.\n
   ///Otherwise a NURBSSurface is created
   void SetControlPoints(std::vector<NURBS::ControlPoint> ctrlPts,
                         unsigned int uSize,unsigned int vSize=1);

   ///Set the size between u/v parameters when calculating the surface.
   ///\param stepSize The tessellation step size.
   ///\param direction "U" or "V" direction
   void SetInterpolationStepSize(double stepSize,std::string direction="U");

   ///Interpolate the NURBS object.
   virtual void Interpolate()=0;

   ///Get the NURBSObject::Type
   Type GetType();

   ///Get the u/v degree of the surface.
   ///\param direction "U" or "V" direction
   unsigned int GetDegree(std::string direction="U");

   ///Get the order of the NURBS.
   ///\param direction "U" or "V" direction
   unsigned int GetOrder(std::string direction="U");

   ///Number of control points in the u/v direction
   ///\param direction "U" or "V" direction
   unsigned int NumControlPoints(std::string direction="U");

   ///Number of interpolated points u/v direction
   ///\param direction "U" or "V" direction
   unsigned int NumInterpolatedPoints(std::string direction="U");

   ///Get a specified control point
   ///\param index The key to search for in the control point list
   NURBS::ControlPoint& GetControlPoint(size_t index);

   ///Get the ControlPoint for this surface.
   std::vector<NURBS::ControlPoint>& ControlPoints();

   ///Get the tessellated points.
   std::vector<NURBS::Point>& InterpolatedPoints();

protected:

   ///Calculate the basis functions that affect a given parameter
   ///\param parameter The interpolating parameter
   ///\param direction "U" or "V" direction
   void _calculateBasisFunctions(double parameter, 
                                 std::string direction);

   ///Calculate the basis functions and derivatives that affect a given parameter.\n
   ///Modification of the _calculateBasisFunctions algorithm
   ///\param parameter The interpolating parameter
   ///\param direction "U" or "V" direction
   void _calculateBasisFunctionsAndDerivatives(double parameter, 
                                 std::string direction);

   Type _type; ///<The NURBSObject type.

   bool _needsRetessellation;///<Means the paramaters have changed.

   std::map<std::string, double> _interpolationStepSize;///<The tessellation u/v step size

   std::map<std::string, unsigned int> _meshDimensions;///<The number of interpolated mesh points in the u/v direction

   std::map<std::string, unsigned int> _nControlPoints;///<The number of control points in the u/v direction

   std::map<std::string, unsigned int> _degree;///<The u/v degree
   std::map<std::string, unsigned int> _order;///<The u/v order
  
   unsigned int _nTotalControlPoints;///<The number of ControlPoint s.

   std::map<std::string, unsigned int> _currentSpan;///<The current span in the knot vector.

   std::map<std::string, NURBS::KnotVector> _knotVectors;///<The raw u/v knot vectors
 
   std::map<std::string, std::map< unsigned int, std::vector<double> > > _knotDifferences;///<Knot differences
   std::map<std::string, std::map< unsigned int, std::vector<double> > > _derivativeBasisFunctions;///<The u/v derivatives of basis functions

   std::map<unsigned int, std::vector<double> > _uBasisFunctionsDerivatives;///<The kth derivative u basis functions
   std::map<unsigned int, std::vector<double> > _vBasisFunctionsDerivatives;///<The kth derivative v basis functions

   std::vector<NURBS::ControlPoint> _controlPoints;///<The raw ControlPoint data
   std::vector<NURBS::Point> _interpolatedPoints;///<The tesselated points.
};
}
#endif //NURBS_OBJECT_H
