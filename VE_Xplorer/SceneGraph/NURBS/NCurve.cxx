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
   if(!_controlPoints[0].size())
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
   SetDegree(static_cast<unsigned int>(_knotVectors["U"].NumberOfKnots() - _controlPoints[0].size()) - 1);

   _interpolatedPoints.clear();
   
   _meshDimensions["U"] = static_cast<unsigned int>(1.0/(_interpolationStepSize["U"])+1);
 
   double param = 0.0;
   std::vector<NURBS::ControlPoint> curveInfo;

   for(unsigned int i = 0; i < _meshDimensions["U"]; i++)
   {
      _calculateBasisFunctionsAndDerivatives(param,"U");
      curveInfo = _calculatePointOnCurve(param,_currentSpan["U"]);
      for(size_t k = 0; k < curveInfo.size(); k++)
      {
         _interpolatedPoints[k].push_back(curveInfo.at(k));
      }
      param += _interpolationStepSize["U"];
   }
}
/////////////////////////////////////////////////////////////////////////////////////
std::vector<NURBS::ControlPoint> NURBSCurve::_calculatePointOnCurve(double parameter, 
                                                                   unsigned int span)
{
   std::vector<NURBS::ControlPoint> resutlingWeightedPoint;
   double invWeight = 1.0f;
   for(unsigned int k = 0; k < _degree["U"]; k++)
   {
      resutlingWeightedPoint.push_back(ControlPoint());
      for(unsigned int i = 0; i <= _degree["U"]; i++)
      {
         resutlingWeightedPoint[k] = resutlingWeightedPoint[k].GetWeigthedPoint() 
                                + _controlPoints[0][span - _degree["U"] +i].GetWeigthedPoint()
                                *_derivativeBasisFunctions["U"][k].at(i);
      }
      invWeight/=resutlingWeightedPoint[k].Weight();
      resutlingWeightedPoint[k].GetWeigthedPoint()*invWeight;
   }
   return resutlingWeightedPoint;
}
