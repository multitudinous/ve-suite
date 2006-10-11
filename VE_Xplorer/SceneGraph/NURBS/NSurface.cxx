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
   SetDegree(static_cast<unsigned int>(_knotVectors["U"].NumberOfKnots() - _nControlPoints["U"]) - 1,"U");

   //ensure our v surface conditions
   //s + 1 = (m + 1) + (q + 1)
   SetDegree(static_cast<unsigned int>(_knotVectors["V"].NumberOfKnots() - _nControlPoints["V"]) - 1,"V");

   _interpolatedPoints.clear();
 
   double uparam = 0.0;
   double vparam = 0.0;

   _interpolationStepSize["U"] = 1.0/(_meshDimensions["U"]-1);
   _interpolationStepSize["V"] = 1.0/(_meshDimensions["V"]-1);

   std::map<unsigned int,std::vector<NURBS::ControlPoint> > surfaceInfo;
   for(unsigned int u = 0;u < _meshDimensions["U"]; u++)
   {
      _calculateBasisFunctionsAndDerivatives(uparam,"U");
      for(unsigned int v = 0;v < _meshDimensions["V"]; v++)
      {
         //don't need to calculate this every time through!!!!
         //need to look into moving this calculateBasisFunctionsAndDerivatives out...
         _calculateBasisFunctionsAndDerivatives(vparam,"V");
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

         vparam += _interpolationStepSize["V"];
      }

      vparam = 0.0;
      uparam += _interpolationStepSize["U"];
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
   double invWeight = 0.0;///resutlingWeightedPoint.Weight();
   
   double ctrlPtWeight = 1.0;
   double ucontrib [4] = {0.,0.,0.,0.0};
   for(unsigned int n = 0; n < _degree["U"]; n++)
   {
      tempUContribution.clear();
      for(unsigned int l = 0; l <= _degree["V"]; l++)
      {
         vindex = _currentSpan["V"] - _degree["V"] + l;
         ucontrib[0] = 0;
         ucontrib[1] = 0;
         ucontrib[2] = 0;
         ucontrib[3] = 0.0;
         for(unsigned int k = 0; k <= _degree["U"]; k++)
         {
            uindex = _currentSpan["U"] - _degree["U"] + k;
         
            ucontrib[0]+=(_controlPoints[0][uindex*_nControlPoints["V"] + vindex].WeightedX()
                         *_derivativeBasisFunctions["U"][n].at(k));
            ucontrib[1]+=(_controlPoints[0][uindex*_nControlPoints["V"] + vindex].WeightedY()
                         *_derivativeBasisFunctions["U"][n].at(k));
            ucontrib[2]+=(_controlPoints[0][uindex*_nControlPoints["V"] + vindex].WeightedZ()
                         *_derivativeBasisFunctions["U"][n].at(k));
            ucontrib[3]+=_controlPoints[0][uindex*_nControlPoints["V"] + vindex].Weight()
                          *_derivativeBasisFunctions["U"][n].at(k);
         }
         invWeight = 1.0/ucontrib[3];
         tempUContribution.push_back(NURBS::ControlPoint(ucontrib[0]*invWeight,
                                                         ucontrib[1]*invWeight,
                                                         ucontrib[2]*invWeight,
                                                         ucontrib[3]));
      }

      double sw [4] = {0.,0.,0.,0.0};
      for(unsigned int j = 0; j < _degree["V"]; j++)
      {
         sw[0] = 0;
         sw[1] = 0;
         sw[2] = 0;
         sw[3] = 0.0;
         for(unsigned int l = 0; l <= _degree["V"]; l++)
         {
            sw[0]+=(tempUContribution[l].WeightedX()
                    *_derivativeBasisFunctions["V"][j].at(l));
            sw[1]+=(tempUContribution[l].WeightedY()
                    *_derivativeBasisFunctions["V"][j].at(l));
            sw[2]+=(tempUContribution[l].WeightedZ()
                    *_derivativeBasisFunctions["V"][j].at(l));
            sw[3]+=(tempUContribution[l].Weight()
                    *_derivativeBasisFunctions["V"][j].at(l));
         }
         invWeight = 1.0/sw[3];
         resutlingWeightedPoint[n].push_back(ControlPoint(sw[0]*invWeight,
                                                          sw[1]*invWeight,
                                                          sw[2]*invWeight,
                                                          sw[3]));
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
