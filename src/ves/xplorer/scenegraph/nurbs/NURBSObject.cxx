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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <ves/xplorer/scenegraph/nurbs/NURBSObject.h>
#include <ves/xplorer/scenegraph/nurbs/ControlPoint.h>
#include <ves/xplorer/scenegraph/nurbs/KnotVector.h>
#include <iostream>
#include <cmath>

using namespace ves::xplorer::scenegraph::nurbs;
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
void NURBSObject::SetKnotVector(ves::xplorer::scenegraph::nurbs::KnotVector knots,
                                std::string direction)
{
   _knotVectors[direction] = knots;
   _needsRetessellation = true;
}
///////////////////////////////////////////////////////////////////////////
void NURBSObject::SetControlPoints(std::vector<ves::xplorer::scenegraph::nurbs::ControlPoint> ctrlPts,
                                    unsigned int columns,
                                    unsigned int rows)
{
   _controlPoints.clear();
   unsigned int row = 0;
   unsigned int column = 0;

   for(size_t i = 0; i < ctrlPts.size(); i++)
   {
      ctrlPts.at(i).SetRowColumnIndex(row,column );
      _controlPoints[0].push_back(ctrlPts.at(i));
      column++;

      if(column == columns)
      { 
         column=0;
         row++;
      }
   }
   _nControlPoints["U"] = columns;
   _nControlPoints["V"] = rows;
   _nTotalControlPoints = static_cast<unsigned int>(_controlPoints[0].size());
   _needsRetessellation = true;
}
//////////////////////////////////////////////////////////
void NURBSObject::SetInterpolationGridSize(unsigned int stepSize,
                                           std::string direction)
{
   //if(stepSize < .5)
   //_interpolationStepSize[direction] = stepSize;
   _meshDimensions[direction] = stepSize;
   
   _needsRetessellation = true;
}
////////////////////////////////////////
NURBSObject::Type NURBSObject::GetType()
{
   return _type;
}
//////////////////////////////////////////////////////////////////////////////////////
void NURBSObject::UpdateMesh(/*std::vector<*/ves::xplorer::scenegraph::nurbs::ControlPoint modifiedControlPoint)
{
   ///This assumes the control point data has already been updated!!!
   double ubounds[2] = {0.0,1.0};
   double vbounds[2] = {0.0,1.0};

   /*size_t nMovedControlPoints = controlPointIndecies.size();
   for(size_t i = 0; i < nMovedControlPoints; i++)
   {
    
   }*/

   unsigned int vIndex = modifiedControlPoint.GetRowIndex();
   unsigned int uIndex = modifiedControlPoint.GetColumnIndex();

   ubounds[0] = _knotVectors["U"].Knot(uIndex);
   ubounds[1] = _knotVectors["U"].Knot(uIndex + _degree["U"] + 1);

   if(_type == ves::xplorer::scenegraph::nurbs::NURBSObject::Surface)
   {
      vbounds[0] = _knotVectors["V"].Knot(vIndex);
      vbounds[1] = _knotVectors["V"].Knot(vIndex+_degree["V"]+1);
   }
   _interpolateWithinBounds(ubounds,vbounds);

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
/////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::nurbs::KnotVector& NURBSObject::KnotVector(std::string direction)
{
   return _knotVectors[direction];
}
/////////////////////////////////////////////////////////////////////////////////////
std::vector<ves::xplorer::scenegraph::nurbs::ControlPoint>& NURBSObject::ControlPoints(unsigned int derivative)
{
   return _controlPoints[derivative];
}
////////////////////////////////////////////////////////////
std::vector<ves::xplorer::scenegraph::nurbs::Point>& NURBSObject::InterpolatedPoints()
{
   return _interpolatedPoints[0];
}
//////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::nurbs::ControlPoint* NURBSObject::GetControlPoint(size_t index)
{
   return &_controlPoints[0][index];
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
////////////////////////////////////////////////////////////////////////////
void NURBSObject::_interpolateWithinBounds(double* uBounds,double* vBounds)
{
   
   _interpolateWithinRange(uBounds[0],uBounds[1],vBounds[0],vBounds[1]);
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
/////////////////////////////////////
unsigned int NURBSObject::GetMinimumDegree()
{
   if(_type==NURBSObject::Curve)
   {
      return _degree["U"];
   }
   if(_degree["U"] < _degree["V"])
      return _degree["U"];
   else
      return _degree["V"];
}
///////////////////////////////////////////////////////////////////////
unsigned int NURBSObject::_calculateBinomialCoefficients(unsigned int row, 
                                                 unsigned int column)
{
   if((row ==0||column ==0|| row==column+1))
      return 1;
   return _calculateBinomialCoefficients(row-1,column-1) 
         + _calculateBinomialCoefficients(row-1,column);
}
/////////////////////////////////////////////////////////////////////////
void NURBSObject::_calculateBasisFunctionsAndDerivatives(double parameter, 
                                                         std::string direction)
{
   _currentSpan[direction] = _knotVectors[direction].FindKnotSpan(parameter,
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
   for(size_t j = 1; j <= /*_degree[direction]*/1; j++)
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
   for(size_t j = 0; j <= /*_degree[direction]*/1; j++)
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
   for( int r = 0; r <= /*static_cast<int>(_degree[direction])*/1; r++)
   {
      row1 = 0;
      row2 = 1;

      a[0] = 1.0;

      for( int k = 1; k <= /*static_cast<int>(_degree[direction])*/1; k++)
      {
          d = 0.0;
          rk = r-k;
          pk = _degree[direction]-k;

          if(r >=k)
          {
             //a[s2][0] = a[s1][0]/ndu[pk+1][rk]
             a[row2*(_degree[direction]+1)] = a[row1*(_degree[direction]+1)]/_knotDifferences[direction][pk+1][rk];

             d = a[row2*(_degree[direction]+1)]*_knotDifferences[direction][rk][pk];
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
             a[row2*(_degree[direction]+1) + j] = (a[row1*(_degree[direction]+1) +(j)] 
                                                 - a[row1*(_degree[direction]+1) +(j-1)])
                                                  /_knotDifferences[direction][pk+1][rk+j];

             d+= a[row2*(_degree[direction]+1) + j]*_knotDifferences[direction][rk+j][pk];
          }

          if(r<=pk)
          {
             a[row2*(_degree[direction]+1) + k] = -a[row1*(_degree[direction]+1) + (k-1)]/_knotDifferences[direction][pk+1][r];
             d+= a[row2*(_degree[direction]+1) + k]*_knotDifferences[direction][r][pk];
          }
          _derivativeBasisFunctions[direction][k].push_back(d);
          
          //check this if things go bad!!!
          tempRow = row1;
          row1 = row2;
          row2 = tempRow;
      } 
      
   }
   int r= _degree[direction];
   for(int k=1; k <= /*static_cast<int>(_degree[direction])*/1; k++)
   {
      for(int j =0; j <= /*int(_degree[direction])*/1; j++)
      {
         _derivativeBasisFunctions[direction][k][j]*=r;
      }
      r*=((_degree[direction])-k);
   }
   delete [] a;
}
//////////////////////////////////////////////////////////////////////////
unsigned int NURBSObject::_findNearestParameterIndex(std::string direction,
                                                     double parameter)
{
   std::map<double, unsigned int >::iterator lowerNearestValue;
   //endpoints
   if(parameter == 0.f)
   {
      return _parameterValues[direction].begin()->second;
   }
   if(parameter == 1.f)
   {
      return _parameterValues[direction].rbegin()->second;
   }

   lowerNearestValue = _parameterValues[direction].lower_bound(parameter);
   
   while((float)parameter <(float)lowerNearestValue->first)
      lowerNearestValue--;
   return lowerNearestValue->second;
}
////////////////////////////////////////////////////////////////////////////////
std::vector< std::vector<ves::xplorer::scenegraph::nurbs::ControlPoint> > NURBSObject::GetControlPoints( unsigned int derivative )
{
   size_t numUPoints = _nControlPoints["U"];
   size_t numVPoints = _nControlPoints["V"];
   std::vector< ves::xplorer::scenegraph::nurbs::ControlPoint > tempPoints = _controlPoints[ derivative ];
   std::vector< std::vector< ves::xplorer::scenegraph::nurbs::ControlPoint > > controlPoints;

   for ( size_t i =0; i < numVPoints; ++i )
   {
      std::vector< ves::xplorer::scenegraph::nurbs::ControlPoint > points;
      for ( size_t j = 0; j < numUPoints; ++j )
      {
         points.push_back( tempPoints.at( (i * numUPoints) + j ) );
      }
      controlPoints.push_back( points );
   }
   return controlPoints;
}

