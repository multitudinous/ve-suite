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
 * File:          $RCSfile: Tag.h,v $
 * Date modified: $Date: 2006-01-24 23:17:17 -0600 (Tue, 24 Jan 2006) $
 * Version:       $Rev: 3578 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef _VE_CONDUCTOR_GUI_UTILITIES_POLYGON_
#define _VE_CONDUCTOR_GUI_UTILITIES_POLYGON_
namespace VE_Conductor
{
namespace GUI_Utilites
{
class Polygon
{
public:
   ///Constructor
   Polygon( void );
   ///Destructor
   ~Polygon( void );
   ///Copy Constructor
   Polygon( const Polygon& );
   ///equal operator
   Polygon& operator= ( const Polygon& );

   ///Get the polygon
   std::vector< wxPoint >* GetPolygon( void );
   ///Get the i'th point from the polygon
   ///\param i i'th point
   wxPoint* GetPoint( size_t i );
   ///Translates this polygon from oldpoly by x and y
   ///\param oldpoly polygon that you are translating from
   ///\param x x translation distance
   ///\param y y translation distance
   void TransPoly( Polygon oldpoly, int x, int y );
   double nearpnt( wxPoint pt, Polygon poly, Polygon& Near);
   int intersect( POLY l1, POLY l2 );
   int inside( wxPoint pt, POLY poly );

private:
   std::vector< wxPoint > poly;
};
}
}
#endif
