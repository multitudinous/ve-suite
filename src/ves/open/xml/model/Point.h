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
#ifndef _VES_OPEN_XML_MODEL_POINT_H_
#define _VES_OPEN_XML_MODEL_POINT_H_

#include <ves/open/xml/model/PointPtr.h>

#include <ves/open/xml/XMLObject.h>

#include <xercesc/dom/DOM.hpp>

#include <utility>
#include <iostream>

namespace ves
{
namespace open
{
namespace xml
{
namespace model
{
class VE_MODEL_EXPORTS Point : public ves::open::xml::XMLObject
{
/*!\file Point.h
  2D Point API
  */
/*!\class ves::open::xml::model::Point
 *This class basically manages two unsigned ints for constructing points
 *on the design canvas.
 */
public:
    ///Constructor
    Point( );
    ///Destructor
    virtual ~Point();
    ///Copy Constructor
    Point( const Point& );
    ///equal operator
    Point& operator= ( const Point& );

    ///Set this array from an input vector
    ///\param newPoint The new values to set to this point.
    void SetPoint( const std::pair< unsigned int, unsigned int >& newPoint );

    ///Get the internal data.
    const std::pair< unsigned int, unsigned int >& GetPoint( void );

    ///Populate the VEXMLObject data from an XML element.
    ///\param inputXML The input data.
    virtual void SetObjectFromXMLData( XERCES_CPP_NAMESPACE_QUALIFIER DOMNode* inputXML );

protected:
    ///Internally update the data.
    ///\param tagName The tag name of this element.
    virtual void _updateVEElement( const std::string& tagName );
    ///point holds two unsigned ints used by wx to construct
    ///the network diagram in Conductor
    ///The first component is x, the second component is y
    std::pair< unsigned int, unsigned int > mPoint;///<Raw data.
};

}
}
}
}
#endif// XML_POINT_H_
