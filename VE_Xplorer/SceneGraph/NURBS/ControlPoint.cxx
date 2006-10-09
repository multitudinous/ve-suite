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
#include "ControlPoint.h"
using namespace NURBS;
////////////////////////////
//Constructor             //
////////////////////////////
Point::Point()
{
   _x = 0.0;
   _y = 0.0;
   _z = 0.0;
}
///////////////////////////////////////
//Constructor                        //
///////////////////////////////////////
Point::Point(double x,double y, double z)
{
   _x = x;
   _y = y;
   _z = z;
}
///////////////////////////////////////////////////
///Copy constructor                              //
///////////////////////////////////////////////////
Point::Point(const Point& rhs)
{ 
   _x = rhs._x;
   _y = rhs._y;
   _z = rhs._z;
}
///////////////
//Destructor //
///////////////
Point::~Point()
{
}
//////////////////////////////////////////////////////////////
///Equal operator                                           //
//////////////////////////////////////////////////////////////
Point& Point::operator=(const Point& rhs)
{
   if(this !=&rhs)
   {
      _x = rhs._x;
      _y = rhs._y;
      _z = rhs._z;
   }
   return *this;
}
/////////////////////////////////
///Set the x coord of          //
/// this control point.        //
/////////////////////////////////
void Point::SetX(double x)
{
   _x = x;
}
/////////////////////////////////
///Set the y coord of          //
/// this control point.        //
/////////////////////////////////
void Point::SetY(double y)
{
   _y = y;
}
/////////////////////////////////
///Set the z coord of          //
/// this control point.        //
/////////////////////////////////
void Point::SetZ(double z)
{
   _z = z;
}
//////////////////////////////////////////////////
void Point::Translate(double dx,double dy, double dz)
{
   _x += dx;
   _y += dy;
   _z += dz;
}
///////////////////////
//Get the weight of  //
//this point.        //
///////////////////////
double Point::X()
{
   return _x;
}
//////////////////////////////////////
//Get the value of                  //
//the second directional coordinate.//
//////////////////////////////////////
double Point::Y()
{
   return _y;
}
//////////////////////////////////
///Get the value of the         //
// third directional coordinate.//
//////////////////////////////////
double Point::Z()
{
   return _z;
}
////////////////////////////
//ControlPoint class      //
////////////////////////////
ControlPoint::ControlPoint()
:NURBS::Point()
{
   _weight = 1.0;
}
///////////////////////////////////////////////////////////////
ControlPoint::ControlPoint(double x, double y, double z, double w)
:NURBS::Point(x,y,z)
{
   SetWeight(w);
}
///////////////////////////////////////////////////
ControlPoint::ControlPoint(const ControlPoint& rhs)
:NURBS::Point(rhs)
{
   SetWeight(rhs._weight);
}
/////////////////////////////
ControlPoint::~ControlPoint()
{
}
//////////////////////////////////////////
void ControlPoint::SetWeight(double weight)
{
   _weight = weight;
}
////////////////////////////
double ControlPoint::Weight()
{
   return _weight;
}
//////////////////////////////////////////////
ControlPoint ControlPoint::GetWeigthedPoint()
{
   return NURBS::ControlPoint(_x*_weight,_y*_weight,_z*_weight,_weight);
}
///////////////////////////////////////////////////////
ControlPoint ControlPoint::operator*(const double& lhs)
{
      //not sure how to handle the weights here!!!
      ControlPoint newPoint(lhs*_x,
                            lhs*_y,
                            lhs*_z,
                            /*lhs*/_weight);
      
      return newPoint;
}
////////////////////////////////////////////////////////////
///override "+" operator                                  //
////////////////////////////////////////////////////////////
ControlPoint ControlPoint::operator+(const ControlPoint& lhs)
{
   //not sure how to handle the weights here!!!
   ControlPoint newPoint(lhs._x + _x,
                         lhs._y + _y,
                         lhs._z + _z,
                         (lhs._weight+_weight)*.5);
      
   return newPoint;
}
//////////////////////////////////////////////////////////////
ControlPoint& ControlPoint::operator=(const ControlPoint& rhs)
{
   if(this != &rhs)
   {
      NURBS::Point::operator=(rhs);
      SetWeight(rhs._weight);
   }
   return *this;
}

