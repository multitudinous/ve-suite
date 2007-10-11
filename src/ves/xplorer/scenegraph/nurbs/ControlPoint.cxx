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
#include <ves/xplorer/scenegraph/NURBS/ControlPoint.h>
using namespace NURBS;
////////////////////////////
//Constructor             //
////////////////////////////
Point::Point()
{
   _x = 0.0;
   _y = 0.0;
   _z = 0.0;
   _row = 0;
   _column = 0;
   _isSelected = false;
}
///////////////////////////////////////
//Constructor                        //
///////////////////////////////////////
Point::Point(double x,double y, double z)
{
   _x = x;
   _y = y;
   _z = z;
   _row = 0;
   _column = 0;
   _isSelected = false;
}
///////////////////////////////////////////////////
//Copy constructor                               //
///////////////////////////////////////////////////
Point::Point(const Point& rhs)
{ 
   _x = rhs._x;
   _y = rhs._y;
   _z = rhs._z;
   _row = rhs._row;
   _column = rhs._column;
   _isSelected = rhs._isSelected;
}
///////////////
//Destructor //
///////////////
Point::~Point()
{
}
//////////////////////////////////////////////////////////////
//Equal operator                                            //
//////////////////////////////////////////////////////////////
Point& Point::operator=(const Point& rhs)
{
   if(this !=&rhs)
   {
      _x = rhs._x;
      _y = rhs._y;
      _z = rhs._z;
      _row = rhs._row;
      _column = rhs._column;
      _isSelected = rhs._isSelected;
   }
   return *this;
}
///////////////////////////////////////
void Point::SetSelected(bool trueFalse)
{
   _isSelected = trueFalse;
}
//////////////////////////////////////
void Point::SetCoordinates(double* pt)
{
   _x = pt[0];
   _y = pt[1];
   _z = pt[2];
}
/////////////////////////////////
//Set the x coord of           //
// this control point.         //
/////////////////////////////////
void Point::SetX(double x)
{
   _x = x;
}
/////////////////////////////////
//Set the y coord of           //
// this control point.         //
/////////////////////////////////
void Point::SetY(double y)
{
   _y = y;
}
/////////////////////////////////
//Set the z coord of           //
// this control point.         //
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
//////////////////////////////////////////////
void Point::SetRowColumnIndex(unsigned int row,
                              unsigned int column)
{
   _row = row;
   _column = column;
}
////////////////////////////////////
unsigned int Point::GetRowIndex()
{
   return _row;
}
////////////////////////////////////
unsigned int Point::GetColumnIndex()
{
   return _column;
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
//Get the value of the         //
// third directional coordinate.//
//////////////////////////////////
double Point::Z()
{
   return _z;
}
////////////////////////
bool Point::IsSelected()
{
   return _isSelected;
}
////////////////////////////
//ControlPoint class      //
////////////////////////////
ControlPoint::ControlPoint()
:NURBS::Point()
{
   _xW = _x;
   _yW = _y;
   _zW = _z;
   _weight = 1.0;
   _eyeSpaceTranslation[0] = 0;
   _eyeSpaceTranslation[1] = 0;
   _eyeSpaceTranslation[2] = 0;
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
   _eyeSpaceTranslation[0] = rhs._eyeSpaceTranslation[0];
   _eyeSpaceTranslation[1] = rhs._eyeSpaceTranslation[1];
   _eyeSpaceTranslation[2] = rhs._eyeSpaceTranslation[2];
}
/////////////////////////////
ControlPoint::~ControlPoint()
{
}
//////////////////////////////////////////
void ControlPoint::SetWeight(double weight)
{
   _weight = weight;
   _xW = _x*_weight;
   _yW = _y*_weight;
   _zW = _z*_weight;
}
//////////////////////////////////////////////////////////
void ControlPoint::SetEyeSpaceTranslation(double* deltaPt)
{
   _eyeSpaceTranslation[0] = deltaPt[0];
   _eyeSpaceTranslation[1] = deltaPt[1];
   _eyeSpaceTranslation[2] = deltaPt[2];
}
//////////////////////////////////////////////
double* ControlPoint::GetEyeSpaceTranslation()
{
   return _eyeSpaceTranslation;
}
////////////////////////////
double ControlPoint::Weight()
{
   return _weight;
}
//////////////////////////////////////////////
ControlPoint ControlPoint::GetWeightedPoint()
{
   return NURBS::ControlPoint(_xW,_yW,_zW,_weight);
}
////////////////////////////////
//Weighted component X        //
////////////////////////////////
double ControlPoint::WeightedX()
{
   return _x*_weight;
}
////////////////////////////////
//Weighted component Y        //
////////////////////////////////
double ControlPoint::WeightedY()
{
   return _y*_weight;
}
////////////////////////////////
//Weighted component Z        //
////////////////////////////////
double ControlPoint::WeightedZ()
{
   return _z*_weight;
}
///////////////////////////////////////////////////////
ControlPoint ControlPoint::operator*(const double& lhs)
{
    //not sure how to handle the weights here!!!
    ControlPoint newPoint(lhs*_x,
                          lhs*_y,
                          lhs*_z,
                          _weight);
      
      return newPoint;
}
////////////////////////////////////////////////////////////
//override "+" operator                                   //
////////////////////////////////////////////////////////////
ControlPoint ControlPoint::operator+(const ControlPoint& lhs)
{
   //not sure how to handle the weights here!!!
   ControlPoint newPoint(lhs._x + _x,
                         lhs._y + _y,
                         lhs._z + _z,
                         _weight);
      
   return newPoint;
}
//////////////////////////////////////////////////////////////
ControlPoint& ControlPoint::operator=(const ControlPoint& rhs)
{
   if(this != &rhs)
   {
      NURBS::Point::operator=(rhs);
      SetWeight(rhs._weight);
      _eyeSpaceTranslation[0] = rhs._eyeSpaceTranslation[0];
      _eyeSpaceTranslation[1] = rhs._eyeSpaceTranslation[1];
      _eyeSpaceTranslation[2] = rhs._eyeSpaceTranslation[2];
   }
   return *this;
}

