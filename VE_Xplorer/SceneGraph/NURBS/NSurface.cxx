#include "VE_Xplorer/SceneGraph/NURBS/NSurface.h"
#include "VE_Xplorer/SceneGraph/NURBS/ControlPoint.h"
#include "VE_Xplorer/SceneGraph/NURBS/KnotVector.h"
#include <iostream>
#include <cmath>

using namespace NURBS;
////////////////////////
//Constructor         //
////////////////////////
NURBSSurface::NURBSSurface(unsigned int udegree,
                           unsigned int vdegree)
:NURBSObject(NURBSObject::Surface,udegree,vdegree)
{
   _needsRetessellation = true;
}
/////////////////////////////////////////////
//Copy constructor                         //
/////////////////////////////////////////////
NURBSSurface::NURBSSurface(const NURBSSurface& rhs)
:NURBSObject(rhs)
{
   
}
/////////////////////////
NURBSSurface::~NURBSSurface()
{
   
}
//////////////////////////////////////////////////////////////
NURBSSurface& NURBSSurface::operator=(const NURBSSurface& rhs)
{
   if(this != &rhs)
   {
      NURBSObject::operator=(rhs);
   }
   return *this;
}
////////////////////////////////
void NURBSSurface::Interpolate()
{
   if(!_needsRetessellation)
      return;
   if(!_controlPoints.size())
   {
      std::cout<<"No control points specified!!"<<std::endl;
      std::cout<<"NURBSSurface::Interpolate()"<<std::endl;
      return;
   }

   if(!_knotVectors["U"].NumberOfKnots())
   {
      std::cout<<"No U knots specified!!"<<std::endl;
      std::cout<<"NURBSSurface::Interpolate()"<<std::endl;
      return;
   }
   if(!_knotVectors["V"].NumberOfKnots())
   {
      std::cout<<"No V knots specified!!"<<std::endl;
      std::cout<<"NURBSSurface::Interpolate()"<<std::endl;
      return;
   }
   //Check our U surface conditions eq 3.12 NURBS Book
   //r + 1 = (n + 1) + (p + 1)
   SetDegree(static_cast<unsigned int>(_knotVectors["U"].NumberOfKnots() - _nControlPoints["V"]) - 1,"U");

   //ensure our v surface conditions
   //s + 1 = (m + 1) + (q + 1)
   SetDegree(static_cast<unsigned int>(_knotVectors["V"].NumberOfKnots() - _nControlPoints["V"]) - 1,"V");

   _interpolatedPoints.clear();
 
   double uparam = 0.0;
   double vparam = 0.0;
   _meshDimensions["U"] = static_cast<unsigned int>(1.0/(_interpolationStepSize["U"])+1);
   _meshDimensions["V"] = static_cast<unsigned int>(1.0/(_interpolationStepSize["V"])+1);

   for(unsigned int v = 0;v < _meshDimensions["V"]; v++)
   {
      _calculateBasisFunctionsAndDerivatives(vparam,"V");
      for(unsigned int u = 0;u < _meshDimensions["U"]; u++)
      {
         _calculateBasisFunctionsAndDerivatives(uparam,"U");
         _interpolatedPoints.push_back(_calculatePointOnSurface(uparam,vparam,
                                                               _currentSpan["U"],_currentSpan["V"]));
         uparam += _interpolationStepSize["U"];
      }


      uparam = 0.0;
      vparam += _interpolationStepSize["V"];
   }
}
////////////////////////////////////////////////////////////////
NURBS::Point NURBSSurface::_calculatePointOnSurface(double u,
                                                    double v,
                                                    unsigned int uspan,
                                                    unsigned int vspan)
{
   NURBS::ControlPoint resutlingWeightedPoint(0,0,0);
   std::vector<NURBS::ControlPoint> tempUContribution;
   unsigned int uindex = 0;
   unsigned int vindex = 0;

   for(unsigned int l = 0; l <= _degree["V"]; l++)
   {
      tempUContribution.push_back(NURBS::ControlPoint(0,0,0));
      vindex = _currentSpan["V"] - _degree["V"] + l;

      for(unsigned int k = 0; k <= _degree["U"]; k++)
      {
         uindex = _currentSpan["U"] - _degree["U"] + k;
         
         tempUContribution[l] = tempUContribution[l] 
                               + _controlPoints[vindex*_nControlPoints["U"] + uindex]
                               *_derivativeBasisFunctions["U"][0].at(k);
      }

      
   }
   for(unsigned int l = 0; l <= _degree["V"]; l++)
   {
      resutlingWeightedPoint = resutlingWeightedPoint.GetWeigthedPoint() 
                             + tempUContribution[l]
                             *_derivativeBasisFunctions["V"][0].at(l);
   }
   double invWeight = 1.0f/resutlingWeightedPoint.Weight();
   return resutlingWeightedPoint.GetWeigthedPoint()*invWeight;
}

