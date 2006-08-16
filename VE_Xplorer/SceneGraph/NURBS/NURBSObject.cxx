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
   _knotDifferences = rhs._knotDifferences;
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
   _knotDifferences.clear();
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

      _knotDifferences = rhs._knotDifferences;

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
      _controlPoints[0].push_back(ctrlPts.at(i));
   }
   _nControlPoints["U"] = nUCtlPts;
   _nControlPoints["V"] = nVCtlPts;
   _nTotalControlPoints = static_cast<unsigned int>(_controlPoints[0].size());
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
/////////////////////////////////////////////////////////////////////////////////////
std::vector<NURBS::ControlPoint>& NURBSObject::ControlPoints(unsigned int derivative)
{
   return _controlPoints[derivative];
}
////////////////////////////////////////////////////////////
std::vector<NURBS::Point>& NURBSObject::InterpolatedPoints()
{
   return _interpolatedPoints[0];
}
//////////////////////////////////////////////////////////////
NURBS::ControlPoint& NURBSObject::GetControlPoint(size_t index)
{
   return _controlPoints[0][index];
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
   std::cout<<"Not implemented!!"<<std::endl;
   std::cout<<"Use NURBSObject::_calculateBasisFunctionsAndDerivatives"<<std::endl;
   /*_currentSpan[direction] = _knotVectors[direction].FindKnotSpan(parameter,
                                                   _nControlPoints[direction]-1,
                                                   _degree[direction]);
   _knotDifferences[direction].clear();
   _knotDifferences[direction][0].push_back(1.0);
  
   std::vector<double> left;
   std::vector<double> right;
   double saved = 0.0;
   double temp = 0.0;

   
   left.push_back(0.0);
   right.push_back(0.0);

   ///Compute the basis functions -- Algo A2.2 Pigel
   for(size_t j = 1; j <= _degree[direction]; j++)
   {
      left.push_back(parameter - _knotVectors[direction].Knot(_currentSpan[direction] + 1 - j));
      right.push_back(_knotVectors[direction].Knot(_currentSpan[direction] + j) - parameter);
      
      saved = 0.0;
      temp = 0.0;
      
      for(size_t r = 0; r < j; r++)
      {
         temp = _knotDifferences[direction][r]/(right[r+1] + left[j-r]);
         _knotDifferences[direction][r] = saved + right[r+1]*temp;
         saved = left[j-r]*temp;
      }
      _knotDifferences[direction].push_back(saved);
   }*/
}
////////////////////////////////////////////////////////////
void NURBSObject::_calculateBasisFunctionsAndDerivatives(double parameter, 
                                                         std::string direction)
{
   _currentSpan[direction] = _knotVectors[direction].FindKnotSpan(parameter,
                                                   _nControlPoints[direction]-1,
                                                   _degree[direction]);
   
   _knotDifferences[direction].clear();
   _knotDifferences[direction][0].push_back(1.0);

   std::vector<double> left;
   std::vector<double> right;
   double saved = 0.0;
   double temp = 0.0;

   
   left.push_back(0.0);
   right.push_back(0.0);

   ///Compute the basis functions and derivatives -- Algo A2.3 Pigel
   for(size_t j = 1; j <= _degree[direction]; j++)
   {
      left.push_back(parameter - _knotVectors[direction].Knot(_currentSpan[direction] + 1 - j));
      right.push_back(_knotVectors[direction].Knot(_currentSpan[direction] + j) - parameter);
      
      saved = 0.0;
      temp = 0.0;

      for(size_t r = 0; r < j; r++)
      {
         //Lower triangle for basis function table
         _knotDifferences[direction][j].push_back(right[r+1] + left[j-r]);
         temp = _knotDifferences[direction][r][j-1]/_knotDifferences[direction][j][r];
         
         //Upper triangle for basis function table
         _knotDifferences[direction][r].push_back(saved + (right[r+1]*temp));
         saved = left[j-r]*temp;
      }
      _knotDifferences[direction][j].push_back(saved);
   }
   _derivativeBasisFunctions[direction].clear();
   
   //Initialize the "0th" derivative in our derivative map
   for(size_t j = 0; j <= _degree[direction]; j++)
   {
      _derivativeBasisFunctions[direction][0].push_back(_knotDifferences[direction][j].at(_degree[direction]));
   }

   int row1 = 0;
   int row2 = 0;
   int rk = 0;
   int pk = 0;

   int jone = 0;
   int jtwo = 0;
   int tempRow =0;

   unsigned int jthree = 0;
   double d = 0.0;
   double* a = new double [2*(_degree[direction]+1)];
   //Compute the derivatives
   for( int r = 0; r <= _degree[direction]; r++)
   {
      row1 = 0;
      row2 = 1;

      a[0] = 1.0;

      for( int k = 1; k <= _degree[direction]-1; k++)
      {
          d = 0.0;
          rk = r-k;
          pk = _degree[direction]-k;

          if(r >=k)
          {
             //a[s2][0] = a[s1][0]/ndu[pk+1][rk]
             a[row2*(_degree[direction])] = a[row1*(_degree[direction])]/_knotDifferences[direction][pk+1][rk];

             d = a[row2*(_degree[direction])]*_knotDifferences[direction][rk][pk];
          }

          if(rk >= -1)
          {
             jone = 1;
          }
          else
          {
             jone = (-rk);
          }

          if(r-1 <= pk)
          {
             jtwo = k-1;
          }
          else
          {
             jtwo = _degree[direction]-r;
          }

          for( int j = jone; j <= jtwo; j++)
          {
             a[row2*(_degree[direction]) + j] = (a[row1*(_degree[direction]) +(j)] 
                                                 - a[row1*(_degree[direction]) +(j-1)])
                                                  /_knotDifferences[direction][pk+1][rk+1];

             d+= a[row2*(_degree[direction]) + j]*_knotDifferences[direction][rk+j][pk];
          }

          if(r<=pk)
          {
             a[row2*(_degree[direction]) + k] = -a[row1*(_degree[direction]) + (k-1)]/_knotDifferences[direction][pk+1][r];
             d+= a[row2*(_degree[direction]) + k]*_knotDifferences[direction][r][pk];
          }
          _derivativeBasisFunctions[direction][k].push_back(d);
          
          //check this if things go bad!!!
          tempRow = row1;
          row1 = row2;
          row2 = tempRow;
      } 
      
   }
   int r= _degree[direction];
      for(size_t t=1; t <= (_degree[direction]-1); t++)
      {
         for(size_t m =0; m <=_degree[direction]; m++)
         {
            _derivativeBasisFunctions[direction][t][m]*=r;
         }
         r*=(_degree[direction]-t);
      }
   delete [] a;
   a = 0;
}