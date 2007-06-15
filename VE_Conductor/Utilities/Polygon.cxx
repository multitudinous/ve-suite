/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
#include "VE_Conductor/Utilities/Polygon.h"

#include <cmath>
#include <iostream>

using namespace VE_Conductor::GUI_Utilities;

////////////////////////////////////////////////
Polygon::Polygon( void )
{
   ;//Do nothing since this class doesn't manage any of the pointers memory
}
////////////////////////////////////////////////
Polygon::~Polygon( void )
{
   poly.clear();
}
////////////////////////////////////////////////
Polygon::Polygon( const Polygon& input )
{
   poly = input.poly;
}
////////////////////////////////////////////////
Polygon& Polygon::operator= ( const Polygon& input )
{
   if ( this != &input )
   {
      poly.clear();
      poly = input.poly;
   }
   return *this;
}
////////////////////////////////////////////////
wxPoint* Polygon::GetPoint( size_t i )
{
   try
   {
      return &(poly.at( i ));
   }
   catch (...)
   {
      poly.push_back( wxPoint() );
      return &(poly.at( i ));
   }
}
////////////////////////////////////////////////
void Polygon::SetPoint( wxPoint newPoint )
{
//std::cout << " set point " << std::endl;
   poly.push_back( newPoint );
}
////////////////////////////////////////////////
std::vector< wxPoint >* Polygon::GetPolygon( void )
{
   return &(poly);
}
////////////////////////////////////////////////
size_t Polygon::GetNumberOfPoints( void )
{
   return poly.size();
}
/////////////////////////////////////////////////////////////
int Polygon::inside( wxPoint pt ) 
{
   int i, count = 0, j = 0;

   Polygon lt, lp, lv;

   lt.GetPolygon()->push_back( pt ); 
   lt.GetPolygon()->push_back( pt ); 
   lt.GetPoint( 1 )->x = 999999;
   lp.GetPolygon()->push_back( pt ); 
   lp.GetPolygon()->push_back( pt );
   lv.GetPolygon()->push_back( pt ); 
   lv.GetPolygon()->push_back( pt );

   wxPoint p( poly.back() );
   poly.insert( poly.begin(), 1, p );

   double numsides = poly.size()-1;
   for(i=1; i<=numsides; i++) 
   {
      *(lp.GetPoint( 0 )) = poly[i];
      *(lp.GetPoint( 1 )) = poly[i-1];
      if ( intersect(lv, lp) ) 
         return 1;
   
      *(lp.GetPoint( 1 )) = poly[i];

      if ( !intersect(lp, lt) ) 
      {
         *(lp.GetPoint( 1 )) = poly[j];
         if ( intersect(lp, lt) ) 
            count++;
         else
            if ( i!=j+1 && ((ccw(lt.GetPoint( 0 ), lt.GetPoint( 1 ), &poly[j])*(ccw(lt.GetPoint( 0 ), lt.GetPoint( 1 ), &poly[i])) < 1)))
               count++;
         
         j = i;
      }
   }
   
   if(j!=numsides && ccw(lt.GetPoint( 0 ), lt.GetPoint( 1 ), &poly[j])*ccw(lt.GetPoint( 0 ), lt.GetPoint( 1 ), &poly[1]) == 1)
      count--;

   return count & 1;
}

//////////////////////////////////////////////////////////////////
double Polygon::nearpnt( wxPoint pt, Polygon& Near )
{
   double dist = 99999;
   int numsides = poly.size();

   Near.GetPolygon()->clear();

   for( int i=0; i< numsides; i++) 
   {
      int i2 = i+1;
      if ( i2 == numsides ) 
         i2 = 0;

      wxRealPoint p;
      wxRealPoint v( poly[i2].x-poly[i].x, poly[i2].y-poly[i].y );
      wxRealPoint n( pt.x - poly[i].x, pt.y - poly[i].y );

      double t = (n.x*v.x + n.y*v.y) / (v.x*v.x + v.y*v.y);
      if(t <= 0) 
      {
         p.x = poly[i].x;
         p.y = poly[i].y;
      } 
      else if(t >= 1) 
      {
         p.x = poly[i2].x;
         p.y = poly[i2].y;
      } 
      else 
      {
         p.x = poly[i].x+t*v.x;
         p.y = poly[i].y+t*v.y;
      }

      double d = computenorm(pt, wxPoint(((int) p.x), ((int) p.y)));
      if(d < dist) 
      {
         Near.GetPolygon()->clear();
         dist = d;
         Near.GetPolygon()->push_back(wxPoint(((int) p.x), ((int) p.y)));
      } 
      else if(d==dist)
         Near.GetPolygon()->push_back(wxPoint(((int) p.x), ((int) p.y)));
  }

  return dist;
}

/////////////////////////////////////////////////////////////
void Polygon::TransPoly( int x, int y, Polygon &newpoly)
{
  newpoly.GetPolygon()->clear();  
  for ( size_t i=0; i < poly.size(); ++i )
    newpoly.GetPolygon()->push_back( wxPoint( poly[i].x+x, poly[i].y+y) );
  
}
///////////////////////////////////////////////////////////////
int Polygon::intersect(Polygon l1, Polygon l2)
{
   int ccw11 = ccw(l1.GetPoint( 0 ), l1.GetPoint( 1 ), l2.GetPoint( 0 ));
   int ccw12 = ccw(l1.GetPoint( 0 ), l1.GetPoint( 1 ), l2.GetPoint( 1 ));
   int ccw21 = ccw(l2.GetPoint( 0 ), l2.GetPoint( 1 ), l1.GetPoint( 0 ));
   int ccw22 = ccw(l2.GetPoint( 0 ), l2.GetPoint( 1 ), l1.GetPoint( 1 ));

   return( ((ccw11*ccw12 < 0) && (ccw21*ccw22 < 0)) ||
      (ccw11*ccw12*ccw21*ccw22 == 0) );
}
///////////////////////////////////////////////////////////////
int Polygon::ccw( wxPoint* pt1, wxPoint* pt2, wxPoint* pt3 )
{
   double dx1 = pt2->x - pt1->x;
   double dx2 = pt3->x - pt1->x;
   double dy1 = pt2->y - pt1->y;
   double dy2 = pt3->y - pt1->y;
  
   if(dx1*dy2 > dy1*dx2) 
      return 1;
  
   if(dx1*dy2 < dy1*dx2) 
      return -1;
  
   if(dx1*dy2 == dy1*dx2)
      if(dx1*dx2 < 0 || dy1*dy2 < 0) 
         return -1;
      else if(dx1*dx1+dy1*dy1 >= dx2*dx2+dy2*dy2) 
         return 0;
  
   return 1;
}
///////////////////////////////////////////////////////////////
double Polygon::computenorm( wxPoint pt1, wxPoint pt2 )
{
  return std::sqrt(double((pt1.x - pt2.x)*(pt1.x - pt2.x) + (pt1.y - pt2.y)*(pt1.y - pt2.y)));
}

void Polygon::clear()
{
	poly.clear();
}
