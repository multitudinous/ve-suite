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