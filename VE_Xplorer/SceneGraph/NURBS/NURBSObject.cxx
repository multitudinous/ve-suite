#include "VE_Xplorer/SceneGraph/NURBS/NURBSObject.h"
#include "VE_Xplorer/SceneGraph/NURBS/ControlPoint.h"
#include "VE_Xplorer/SceneGraph/NURBS/KnotVector.h"
#include <iostream>
#include <cmath>

using namespace NURBS;
////////////////////////
//Constructor         //
///////////////////////////////////////////////////////
NURBSObject::NURBSObject(Type type,unsigned int udegree,
                           unsigned int vdegree)
{
   _type = type;
   
   _nTotalControlPoints = 0;
   _needsRetessellation = true;
   _currentSpan["U"] = 0;
   _currentSpan["V"] = 0;
   
   _nControlPoints["U"] = 1;
   _nControlPoints["V"] = 1;

   _interpolationStepSize["U"] = .1;
   _interpolationStepSize["V"] = .1;

   _meshDimensions["U"] = 1;
   _meshDimensions["V"] = 1;
   

   _degree["U"] = udegree;
   _degree["V"] = vdegree;

   _order["U"] = _degree["U"] + 1;
   _order["V"] = _degree["V"] + 1;
  
}
/////////////////////////////////////////////
//Copy constructor                         //
/////////////////////////////////////////////
NURBSObject::NURBSObject(const NURBSObject& rhs)
{
 
   _type = rhs._type;
   _nTotalControlPoints = rhs._nTotalControlPoints;
   _nControlPoints = rhs._nControlPoints;
   _meshDimensions = rhs._meshDimensions;
   _needsRetessellation = rhs._needsRetessellation;
   _interpolationStepSize = rhs._interpolationStepSize;
   _currentSpan = rhs._currentSpan;
   _basisFunctions = rhs._basisFunctions;
   _knotVectors = rhs._knotVectors;
   _uBasisFunctionsDerivatives = rhs._uBasisFunctionsDerivatives;
   _vBasisFunctionsDerivatives = rhs._vBasisFunctionsDerivatives;
   _controlPoints = rhs._controlPoints;
   _interpolatedPoints = rhs._interpolatedPoints;
}
/////////////////////////
NURBSObject::~NURBSObject()
{
   _nControlPoints.clear();
   _interpolationStepSize.clear();
   _currentSpan.clear();
   _basisFunctions.clear();
   _knotVectors.clear();
   _uBasisFunctionsDerivatives.clear();
   _vBasisFunctionsDerivatives.clear();
   
   _controlPoints.clear();
   _interpolatedPoints.clear();
   
   _needsRetessellation = true;
}
////////////////////////////////////////////////////////
NURBSObject& NURBSObject::operator=(const NURBSObject& rhs)
{
   if(this != &rhs)
   {
      _type = rhs._type;
   
      _nControlPoints = rhs._nControlPoints;
      _nTotalControlPoints = rhs._nTotalControlPoints;
      _meshDimensions = rhs._meshDimensions;

      _needsRetessellation = rhs._needsRetessellation;
      _interpolationStepSize = rhs._interpolationStepSize;
   
      _currentSpan = rhs._currentSpan;

      _basisFunctions = rhs._basisFunctions;

      _knotVectors = rhs._knotVectors;

      _uBasisFunctionsDerivatives = rhs._uBasisFunctionsDerivatives;
      _vBasisFunctionsDerivatives = rhs._vBasisFunctionsDerivatives;

      _controlPoints = rhs._controlPoints;
      _interpolatedPoints = rhs._interpolatedPoints;
   }
   return *this;
}
///////////////////////////////////////////////////////////////////////
void NURBSObject::SetDegree(unsigned int degree, std::string direction)
{
   _degree[direction] = degree;
   _order[direction] = _degree[direction] + 1;
   _needsRetessellation = true;
}
///////////////////////////////////////////////////////
void NURBSObject::SetKnotVector(NURBS::KnotVector knots,
                                std::string direction)
{
   _knotVectors[direction] = knots;
   _needsRetessellation = true;
}
///////////////////////////////////////////////////////////////////////////
void NURBSObject::SetControlPoints(std::vector<NURBS::ControlPoint> ctrlPts,
                                    unsigned int nUCtlPts,
                                    unsigned int nVCtlPts)
{
   _controlPoints.clear();
   for(size_t i = 0; i < ctrlPts.size(); i++)
   {
      _controlPoints.push_back(ctrlPts.at(i));
   }
   _nControlPoints["U"] = nUCtlPts;
   _nControlPoints["V"] = nVCtlPts;
   _nTotalControlPoints = static_cast<unsigned int>(_controlPoints.size());
   _needsRetessellation = true;
}
//////////////////////////////////////////////////////////
void NURBSObject::SetInterpolationStepSize(double stepSize,
                                           std::string direction)
{
   if(stepSize < .5)
      _interpolationStepSize[direction] = stepSize;
   
   _needsRetessellation = true;
}
////////////////////////////////////////
NURBSObject::Type NURBSObject::GetType()
{
   return _type;
}
//////////////////////////////////////////////////////////////////////
unsigned int NURBSObject::NumInterpolatedPoints(std::string direction)
{
   return _meshDimensions[direction];
}
/////////////////////////////////////////////////////////////////
unsigned int NURBSObject::NumControlPoints(std::string direction)
{
   return _nControlPoints[direction];
}
//////////////////////////////////////////////////////////
unsigned int NURBSObject::GetDegree(std::string direction)
{
   return _degree[direction];
}
/////////////////////////////////////////////////////////
unsigned int NURBSObject::GetOrder(std::string direction)
{
   return _order[direction];
}
///////////////////////////////////////////////////////////////////////
std::vector<NURBS::ControlPoint>& NURBSObject::ControlPoints()
{
   return _controlPoints;
}
/////////////////////////////////////////////////////////
std::vector<NURBS::Point>& NURBSObject::InterpolatedPoints()
{
   return _interpolatedPoints;
}
//////////////////////////////////////////////////////////////
NURBS::ControlPoint& NURBSObject::GetControlPoint(size_t index)
{
   return _controlPoints[index];
}
///////////////////////////////
void NURBSObject::Interpolate()
{
   if(!_needsRetessellation)
      return;
   if(!_controlPoints.size())
   {
      std::cout<<"No control points specified!!"<<std::endl;
      std::cout<<"NURBSObject::Interpolate()"<<std::endl;
      return;
   }
}
////////////////////////////////////////////////////////////
void NURBSObject::_calculateBasisFunctions(double parameter, 
                                           std::string direction)
{
   _currentSpan[direction] = _knotVectors[direction].FindKnotSpan(parameter,
                                                   _nControlPoints[direction]-1,
                                                   _degree[direction]);
   _basisFunctions[direction].clear();
   _basisFunctions[direction].push_back(1.0);
  
   std::vector<double> left;
   std::vector<double> right;
   double saved = 0.0;
   double temp = 0.0;

   ///this will be ignored
   left.push_back(0.0);
   right.push_back(0.0);

   for(size_t j = 1; j <= _degree[direction]; j++)
   {
      left.push_back(parameter - _knotVectors[direction].Knot(_currentSpan[direction] + 1 - j));
      right.push_back(_knotVectors[direction].Knot(_currentSpan[direction] + j) - parameter);
      
      saved = 0.0;
      temp = 0.0;
      
      for(size_t r = 0; r < j; r++)
      {
         temp = _basisFunctions[direction][r]/(right[r+1] + left[j-r]);
         _basisFunctions[direction][r] = saved + right[r+1]*temp;
         saved = left[j-r]*temp;
      }
      _basisFunctions[direction].push_back(saved);
   }
}
