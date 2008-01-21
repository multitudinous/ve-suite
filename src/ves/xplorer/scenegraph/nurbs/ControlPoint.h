/*************** <auto-copyright.rb BEGIN do not edit this line> **************
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
 *************** <auto-copyright.rb END do not edit this line> ***************/
#ifndef VE_POINT_H
#define VE_POINT_H

/*!\file ControlPoint.h
  Point and ControlPoint API
  */
/*!\file ControlPoint.cxx
  Point and ControlPoint code
  */
/*!\class ves::xplorer::scenegraph::nurbs::ControlPoint
 * Class defining a Control Point for NURBS object.
 */
/*!\class ves::xplorer::scenegraph::nurbs::Point
 * Class defining a Point for NURBS object.
 */
/*!\namespace ves::xplorer::scenegraph::nurbs
 * NURBS API namespace.
 */
#include <osg/Vec3>
#include <ves/VEConfig.h>

#include <ostream>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace nurbs
{
///???
class VE_NURBS_EXPORTS Point : public osg::Vec3
{
public:
    ///Constructor
    Point();
    ///Constructor
    ///\param x First directional coordinate
    ///\param y Second directional coordinate
    ///\param z Third directional coordinate
    Point( double x, double y, double z );

    ///Copy constructor
    ///\param rhs The point to copy this one from.
    Point( const Point& rhs );

    ///Destructor
    virtual ~Point();

    ///Equal operator.
    ///\param rhs The point to set this one to.
    virtual Point& operator=( const Point& rhs );

    ///Translate the point by a delta
    ///\param dx Change to point's x-position.
    ///\param dy Change to point's y-position.
    ///\param dz Change to point's z-position.
    void Translate( double dx, double dy, double dz );

    ///Set the point coordinates
    ///\param pt The new point values
    void SetCoordinates( double* pt );

    ///Set the x value.
    ///\param x The new value.
    void SetX( double x );

    ///Set the y value.
    ///\param y The new value.
    void SetY( double y );

    ///Set the z value.
    ///\param z The new value.
    void SetZ( double z );

    ///Set the (Row,Column) position of this control point in the mesh
    ///\param row Control point's new row.
    ///\param col Control point's new column.
    void SetRowColumnIndex( unsigned int row, unsigned int col );

    ///Set the selection status of this point
    ///\param trueFalse true == selected\n false == not selected
    void SetSelected( bool trueFalse = true );

    ///Get the value of the first directional coordinate.
    double X();

    ///Get the value of the second directional coordinate.
    double Y();

    ///Get the value of the third directional coordinate.
    double Z();

    ///Get the u Index of this control point in the overall mesh
    unsigned int GetRowIndex();

    ///Get the v Index of this control point in the overall mesh
    unsigned int GetColumnIndex();

    ///Query selection status
    bool IsSelected();

    ///Dot product.
    ///\param rhs ???
    inline double operator*( const Point& rhs ) const
    {
        return _x*rhs._x + _y*rhs._x + _z*rhs._z;
    }

    ///Cross product.
    ///\param rhs ???
    inline const Point operator ^( const Point& rhs ) const
    {
        return Point( _y*rhs._z - _z*rhs._y,
                      _z*rhs._x - _x*rhs._z,
                      _x*rhs._y - _y*rhs._x );
    }
    ///Override ostream operator.
    ///\param os ???
    ///\param fpd ???
    inline friend std::ostream& operator<<( std::ostream& os,
                                            const Point& fpd )
    {
        os << fpd._x << " " << fpd._y << " " << fpd._z << " ";
        return os;
    }

    ///Override ">" operator.
    ///\param lhs ???
    ///\param rhs ???
    friend bool operator>( const Point& lhs, const Point& rhs )
    {
        if( lhs._x > rhs._x ||
                lhs._y > rhs._y ||
                lhs._z > rhs._z )
        {
            return true;
        }
        return false;
    };
    ///Addition operator
    ///\param lhs ???
    Point operator+( const Point& lhs )
    {
        Point newPoint( lhs._x + _x,
                        lhs._y + _y,
                        lhs._z + _z );

        return newPoint;

    };

    ///Multiplication with a scalar operator.
    Point operator*( const double& lhs )
    {
        Point newPoint( lhs*_x,
                        lhs*_y,
                        lhs*_z );

        return newPoint;

    };

protected:
    bool _isSelected;///<Selection status
    double _x;///<The raw coordianate data
    double _y;///<The raw coordianate data
    double _z;///<The raw coordianate data

    unsigned int _row;///<The row of this control point in the overall mesh
    unsigned int _column;///<The column of this control point in the overall mesh
};

///???
class VE_NURBS_EXPORTS ControlPoint : public ves::xplorer::scenegraph::nurbs::Point
{
public:
    ///Constructor
    ControlPoint();
    ///Constructor
    ///\param x First directional coordinate
    ///\param y Second directional coordinate
    ///\param z Third directional coordinate
    ///\param w Weight of the control point.
    ControlPoint( double x, double y, double z, double w = 1.0 );

    ///Copy constructor
    ControlPoint( const ControlPoint& rhs );

    ///Destructor
    virtual ~ControlPoint();

    ///Equal operator
    ///\param rhs The point to set this one to.
    virtual ControlPoint& operator=( const ControlPoint& rhs );

    ///Set the weight of this control point.
    ///\param weight The weight of this control point
    void SetWeight( double weight );

    ///Set the translation values to be performed in eye space.
    ///\param deltaPt The translation in eye space
    void SetEyeSpaceTranslation( double* deltaPt );

    ///Get the eye space translation.
    double* GetEyeSpaceTranslation();

    ///Get the weight of this point.
    double Weight();

    ///Weighted component X
    double WeightedX();
    ///Weighted component Y
    double WeightedY();
    ///Weighted component Z
    double WeightedZ();
    ///Returns P*weight
    ves::xplorer::scenegraph::nurbs::ControlPoint GetWeightedPoint();

    ///Dot product
    inline double operator*( const ControlPoint& rhs ) const
    {
        return _x*rhs._x + _y*rhs._x + _z*rhs._z;
    }

    ///Cross product.
    inline const ControlPoint operator ^( const ControlPoint& rhs ) const
    {
        return ControlPoint( _y*rhs._z - _z*rhs._y,
                             _z*rhs._x - _x*rhs._z,
                             _x*rhs._y - _y*rhs._x );
    }
    ///override "<<"operator
    inline friend std::ostream& operator<<( std::ostream& os,
                                            const ControlPoint& fpd )
    {
        os << fpd._x << " " << fpd._y << " " << fpd._z << " " << fpd._weight << " ";
        return os;
    }
    ///override "<<"operator
    inline friend std::ostream& operator<<( std::ostream& os,
                                            const ControlPoint* fpd )
    {
        os << fpd->_x << " " << fpd->_y << " " << fpd->_z << " " << fpd->_weight << " ";
        return os;
    }

    ///Addition with a ControlPoint.
    ControlPoint operator+( const ControlPoint& lhs );

    ///Multiplication with a scalar.
    ControlPoint operator*( const double& lhs );

protected:
    double _eyeSpaceTranslation[3];///<Eye space translation
    double _xW;///<Weighted x-coord
    double _yW;///<Weighted y-coord
    double _zW;///<Weighted z-coord

    double _weight;///<The weight for this coordinate.

};
}
}
}
}

#endif //VE_POINT_H
