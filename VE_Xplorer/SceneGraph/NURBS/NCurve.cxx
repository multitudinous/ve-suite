#include "VE_Xplorer/SceneGraph/NURBS/NCurve.h"
#include "VE_Xplorer/SceneGraph/NURBS/ControlPoint.h"
#include "VE_Xplorer/SceneGraph/NURBS/KnotVector.h"
#include <iostream>

using namespace NURBS;
////////////////////////
//Constructor         //
////////////////////////
NURBSCurve::NURBSCurve(unsigned int degree)
:NURBSObject(NURBS::NURBSObject::Curve,degree)
{
   _needsRetessellation = true;
}
/////////////////////////////////////////////
//Copy constructor                         //
/////////////////////////////////////////////
NURBSCurve::NURBSCurve(const NURBSCurve& rhs)
{
  
}
/////////////////////////
NURBSCurve::~NURBSCurve()
{
   
}
////////////////////////////////////////////////////////
NURBSCurve& NURBSCurve::operator=(const NURBSCurve& rhs)
{
   if(this != &rhs)
   {
      NURBSObject::operator =(rhs);
   }
   return *this;
}
//////////////////////////////
void NURBSCurve::Interpolate()
{

   if(!_needsRetessellation)
      return;
   if(!_controlPoints.size())
   {
      std::cout<<"No control points specified!!"<<std::endl;
      std::cout<<"NURBSCurve::Interpolate()"<<std::endl;
      return;
   }

   if(!_knotVectors["U"].NumberOfKnots())
   {
      std::cout<<"No knots specified!!"<<std::endl;
      std::cout<<"NURBSCurve::Interpolate()"<<std::endl;
      return;
   }
   //Check our curve condition
   //m + 1 = (n + 1) + (p + 1)
   SetDegree(static_cast<unsigned int>(_knotVectors["U"].NumberOfKnots() - _controlPoints.size()) - 1);

   _interpolatedPoints.clear();
   
   _meshDimensions["U"] = static_cast<unsigned int>(1.0/(_interpolationStepSize["U"])+1);
 
   double param = 0.0;
   for(unsigned int i = 0; i < _meshDimensions["U"]; i++)
   {
      _calculateBasisFunctionsAndDerivatives(param,"U");
      _interpolatedPoints.push_back(_calculatePointOnCurve(param,_currentSpan["U"]));
      param += _interpolationStepSize["U"];
   }
}
////////////////////////////////////////////////////////////////
NURBS::Point NURBSCurve::_calculatePointOnCurve(double parameter, 
                                                unsigned int span)
{
   NURBS::ControlPoint resutlingWeightedPoint(0,0,0);

   for(unsigned int i = 0; i <= _degree["U"]; i++)
   {
      resutlingWeightedPoint = resutlingWeightedPoint.GetWeigthedPoint() 
                             + _controlPoints[span - _degree["U"] +i].GetWeigthedPoint()
                             *_derivativeBasisFunctions["U"][i].at(_degree["U"]);
   }
   double invWeight = 1.0f/resutlingWeightedPoint.Weight();
   return resutlingWeightedPoint.GetWeigthedPoint()*invWeight;
}
