/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/
#ifndef CONDUCTOR_GUI_UTILITIES_POLYGON_
#define CONDUCTOR_GUI_UTILITIES_POLYGON_
/*!\file Polygon.h
Polygon API
*/
/*!\class Polygon
*
*/
#include <vector>
#include <wx/gdicmn.h>
#include <ves/VEConfig.h>

namespace ves
{
namespace conductor
{
namespace util
{
class VE_CONDUCTOR_UTILS_EXPORTS Polygon
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
    ///Add new point to the polygon
    ///\param newPoint The point to be added
    void SetPoint( wxPoint newPoint );
    size_t GetNumberOfPoints( void );
    ///Translates this polygon from oldpoly by x and y
    ///\param oldpoly polygon that you are translating from
    ///\param x x translation distance
    ///\param y y translation distance
    //void TransPoly( Polygon oldpoly, int x, int y );
    double nearpnt( wxPoint pt, Polygon poly, Polygon& Near );
    ///Is this point inside this polygon
    int inside( wxPoint pt );
    double nearpnt( wxPoint pt, Polygon& Near );
    void TransPoly( int x, int y, Polygon& newpoly );
    void clear(); //clear all points out.
    ///Print the list of points
    void print();

private:
    int ccw( wxPoint* pt1, wxPoint* pt2, wxPoint* pt3 );
    double computenorm( wxPoint pt1, wxPoint pt2 );
    int intersect( Polygon l1, Polygon l2 );
    std::vector< wxPoint > poly;
};
}
}
}
#endif
