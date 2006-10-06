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
 * Date modified: $Date:  $
 * Version:       $Rev:  $
 * Author:        $Author:  $
 * Id:            $Id:  $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
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
   _surfDerivatives = rhs._surfDerivatives;
}
/////////////////////////
NURBSSurface::~NURBSSurface()
{
   _surfDerivatives.clear();
}
//////////////////////////////////////////////////////////////
NURBSSurface& NURBSSurface::operator=(const NURBSSurface& rhs)
{
   if(this != &rhs)
   {
      NURBSObject::operator=(rhs);
      _surfDerivatives = rhs._surfDerivatives;
   }
   return *this;
}
////////////////////////////////
void NURBSSurface::Interpolate()
{
   if(!_needsRetessellation)
      return;
   if(!_controlPoints[0].size())
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

   _interpolationStepSize["U"] = 1.0/(_meshDimensions["U"]-1);
   _interpolationStepSize["V"] = 1.0/(_meshDimensions["V"]-1);

   std::map<unsigned int,std::vector<NURBS::ControlPoint> > surfaceInfo;
   for(unsigned int v = 0;v < _meshDimensions["V"]; v++)
   {
      _calculateBasisFunctionsAndDerivatives(vparam,"V");
      for(unsigned int u = 0;u < _meshDimensions["U"]; u++)
      {
         _calculateBasisFunctionsAndDerivatives(uparam,"U");
         surfaceInfo = _calculatePointOnSurface(uparam,vparam,_currentSpan["U"],_currentSpan["V"]);
         _interpolatedPoints[0].push_back(surfaceInfo[0].at(0));

         //S(u,v)
         _surfDerivatives[0][0].push_back(surfaceInfo[0].at(0));
         //dS/du
         _surfDerivatives[0][1].push_back(surfaceInfo[0].at(1));
         //dS/dv
         _surfDerivatives[1][0].push_back(surfaceInfo[1].at(0));
         //ds/dudv
         _surfDerivatives[1][1].push_back(surfaceInfo[1].at(1));
         
         /*for(size_t k = 1; k < surfaceInfo.size(); k++)
         {
            _surfDerivatives[k] = surfaceInfo[k];
         }*/
         uparam += _interpolationStepSize["U"];
      }

      uparam = 0.0;
      vparam += _interpolationStepSize["V"];
   }
}
////////////////////////////////////////////////////////////////
std::map<unsigned int,std::vector<NURBS::ControlPoint> > NURBSSurface::_calculatePointOnSurface(double u,
                                                                        double v,
                                                                        unsigned int uspan,
                                                                        unsigned int vspan)
{
   std::map<unsigned int,std::vector<NURBS::ControlPoint> > resutlingWeightedPoint;//(0,0,0);
   std::vector<NURBS::ControlPoint> tempUContribution;
   unsigned int uindex = 0;
   unsigned int vindex = 0;
   double invWeight = 1.0;///resutlingWeightedPoint.Weight();

   for(unsigned int n = 0; n < _degree["U"]; n++)
   {
      tempUContribution.clear();
      for(unsigned int l = 0; l <= _degree["V"]; l++)
      {
         tempUContribution.push_back(NURBS::ControlPoint(0,0,0));
         vindex = _currentSpan["V"] - _degree["V"] + l;

         for(unsigned int k = 0; k <= _degree["U"]; k++)
         {
            uindex = _currentSpan["U"] - _degree["U"] + k;
         
            tempUContribution[l] = tempUContribution[l] 
            + _controlPoints[0][vindex*_nControlPoints["U"] + uindex].GetWeigthedPoint()
                                 *_derivativeBasisFunctions["U"][n].at(k);
         }
      }
   

      for(unsigned int j = 0; j < _degree["V"]; j++)
      {
         resutlingWeightedPoint[n].push_back(ControlPoint());
         for(unsigned int l = 0; l <= _degree["V"]; l++)
         {
            resutlingWeightedPoint[n][j] = resutlingWeightedPoint[n][j] 
                                      + tempUContribution[l]
                                      *_derivativeBasisFunctions["V"][j].at(l);
         }
      
         invWeight /= resutlingWeightedPoint[n][j].Weight();
         resutlingWeightedPoint[n][j]= resutlingWeightedPoint[n][j]*invWeight;
      }
   }
   return resutlingWeightedPoint;
}
//////////////////////////////////////////////////////////////////////////
std::map<unsigned int, std::map<unsigned int,std::vector<NURBS::Point> > > 
NURBSSurface::GetSurfaceDerivatives()
{
   return _surfDerivatives;
}
