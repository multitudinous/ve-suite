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
#ifndef VE_KNOT_VECTOR_H
#define VE_KNOT_VECTOR_H

/*!\file KnotVector.h
  KnotVector API
  */
/*!\file KnotVector.cxx
  KnotVector code
  */
/*!\class ves::xplorer::scenegraph::nurbs::KnotVector
 * Class defining a KnotVector for NURBS object.
 */

#include <ves/VEConfig.h>

#include <vector>
#include <map>
#include <string>
#include <iostream>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace nurbs
{
///???
class VE_NURBS_EXPORTS KnotVector
{
public:
    ///Constructor
    KnotVector();

    ///Copy constructor
    KnotVector( const KnotVector& rhs );

    ///Destructor
    virtual ~KnotVector();

    ///Equal operator
    ///\param rhs The point to set this one to.
    KnotVector& operator=( const KnotVector& rhs );

    ///Set the knot spacing of KnotVector
    ///\param type The knot vector type\n
    ///Valid values are:\n
    ///Uniform == uniform spacing.
    ///Non-Uniform == non uniform spacing.
    void SetKnotSpacing( std::string type = "Uniform" );

    ///Insert a knot in the KnotVector
    ///\param knot The knot value to insert.
    void AddKnot( double knot );


    ///Get the KnotVector spacing.
    std::string KnotSpacing();

    ///Find the span that a parameter lies in within the KnotVector
    ///\param parameterValue The value to search the span for.
    std::map< double, unsigned int >::iterator FindSpan( double parameterValue );

    ///Find the span that a parameter lies in within the knot vector
    ///\param parameterValue The value to search the span for.
    ///\param degree The degree of the curve this knot vector is associated with.
    unsigned int FindKnotSpan( double parameterValue,
                               unsigned int degree );

    ///Get a knot value at the specified index.
    double Knot( unsigned int index );

    ///Get the current span
    std::map< double , unsigned int >::iterator GetCurrentSpan( void );

    ///Get the current knots
    std::map< double , unsigned int > GetKnotMap( void );

    ///Get the number of knots currently in the vector.
    size_t NumberOfKnots();

    ///Get a vector containing multiplicity for each unique knot.
    std::vector< unsigned int > GetMultiplicityVector( void );
    //Get vector with all knot values (with multiplicity) as 1D array
    //std::vector< double > GetKnotVector( void );
    ///Get knot vector WITHOUT multiplicity.
    std::vector< double > GetDistinctKnotVector( void );

    ///Write out the knot vector.
    inline friend std::ostream& operator<<( std::ostream& os, ves::xplorer::scenegraph::nurbs::KnotVector& knotVector )
    {
        unsigned int nKnots = knotVector.NumberOfKnots();
        for( unsigned int i = 0; i < nKnots; i++ )
        {
            os << knotVector.Knot( i ) << " ";
        }
        os << std::endl;
        return os;
    }

protected:
    size_t _nKnots;///< Number of knots
    std::map< double , unsigned int > _knotMultiplicityMap;///<The map values and multiplicity
    std::string _spacing;///<Type of KnotVector, describes the spacing of the knots.
    std::map< double, unsigned int >::iterator _currentSpan;///<The current span we are investigating
};
}
}
}
}

#endif
