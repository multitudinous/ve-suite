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
#include "KnotVector.h"
#include <iostream>
#include <string>
#include <cmath>
using namespace NURBS;
////////////////////////
//Constructor         //
////////////////////////
KnotVector::KnotVector()
{
   _spacing = "Uniform";
   _nKnots = 0;
   //_interiorKnots = 4;
}
/////////////////////////////////////////////
KnotVector::KnotVector(const KnotVector& rhs)
{
   _spacing = rhs._spacing;
   _knotMultiplicityMap = rhs._knotMultiplicityMap;
   _nKnots = rhs._nKnots;
}
/////////////////////////
KnotVector::~KnotVector()
{
   _knotMultiplicityMap.clear();
}
///////////////////////////////////////////////////////
KnotVector& KnotVector::operator=(const KnotVector& rhs)
{
   if(this != &rhs)
   {
      _spacing = rhs._spacing;
      _knotMultiplicityMap = rhs._knotMultiplicityMap;
      _nKnots = rhs._nKnots;
   }
   return *this;
}
/////////////////////////////////////////
void KnotVector::SetKnotSpacing(std::string type)
{
   if(type == "Uniform" ||
      type == "Non-Uniform")
   {
      _spacing = type;
   }
   else
   {
      std::cout<<"Invalid type specified for knot vector: "<<type<<std::endl;
   }
}
/////////////////////////////////////
void KnotVector::AddKnot(double knot)
{
   std::map< double, unsigned int >::iterator foundKnot;
   foundKnot = _knotMultiplicityMap.find(knot);

   if(foundKnot != _knotMultiplicityMap.end())
   {
      foundKnot->second++;
   }
   else
   {
      _knotMultiplicityMap[knot] = 1;
   }
   _currentSpan = _knotMultiplicityMap.begin();
   _nKnots++;
}
/////////////////////////////////////
std::string KnotVector::KnotSpacing()
{
   return _spacing;
}
//////////////////////////////////////////////////////
//should this be in the NURBSCurve class???         //
//////////////////////////////////////////////////////
unsigned int KnotVector::FindKnotSpan(double parameterValue, 
                                unsigned int nControlPts,
                                unsigned int degree )
{
   if(fabs(parameterValue - (--_knotMultiplicityMap.end())->first) <= 1.0e-6)
   {
      //this might not be correct!!!!!
      return static_cast<unsigned int>(_nKnots)-degree-2;
      //return _knotMultiplicityMap.size() - 2; 
   }

   unsigned int low = degree;
   unsigned int high = nControlPts+1;
   unsigned int mid = (low+high)/2;
   while(parameterValue < Knot(mid) || 
         parameterValue >= Knot(mid +1))
   {
      if(parameterValue < Knot(mid))
      {
         high = mid;
      }
      else
      {
         low = mid;
      }
      mid = (low+high)/2;
   }
   return mid;
}
//////////////////////////////////////////////////////////////////////////////////////
std::map< double, unsigned int >::iterator KnotVector::FindSpan(double parameterValue)
{
   ///span -> 0,numberOfKnots-1
   std::map< double, unsigned int >::iterator foundKnot;
   foundKnot = _knotMultiplicityMap.lower_bound(parameterValue);
   _currentSpan = foundKnot;
   return foundKnot;
}
///////////////////////////////////////////////////////////////////////
std::map< double, unsigned int >::iterator KnotVector::GetCurrentSpan()
{
   return _currentSpan;
}
///////////////////////////////////////////
double KnotVector::Knot(unsigned int index)
{

   if(!index)
      return _knotMultiplicityMap.begin()->first;

   unsigned int currentIndex = 0;
   unsigned int multiplicity = 1;
   for ( std::map<double ,unsigned int>::iterator itr = _knotMultiplicityMap.begin();
                                       itr != _knotMultiplicityMap.end(); itr++)
   {
      multiplicity = 1;
      while (multiplicity <= itr->second)
      {
         if(currentIndex == index)
         {
            return itr->first;
         }
         currentIndex++;
         multiplicity++;
      }
   }
   std::cout<<"Invalid knot index!!: "<<index<<std::endl;
   return -1.0;
}
//////////////////////////////////
size_t KnotVector::NumberOfKnots()
{
   return _nKnots;
}
