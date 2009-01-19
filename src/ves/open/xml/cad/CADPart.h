/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
#ifndef _CAD_PART_H_
#define _CAD_PART_H_

#include <ves/open/xml/cad/CADPartPtr.h>

/*!\file CADPart.h
 * CADPart API
 */

/*!\class VE_XML::VE_CAD::CADPart
 * Class to represent a part file (the actual CAD geometry)
 */
#include <xercesc/dom/DOM.hpp>
#include <ves/open/xml/cad/CADNode.h>
#include <string>

XERCES_CPP_NAMESPACE_USE


namespace ves
{
namespace open
{
namespace xml
{
namespace cad
{
class VE_CAD_EXPORTS CADPart: public ves::open::xml::cad::CADNode
{
public:
    ///Constructor
    ///\param name The name of the part
    CADPart( const std::string& name = std::string( "Part" ) );

    ///Destructor
    virtual ~CADPart();

    ///\param cadFileName The name of the part
    ///Set the name of the CAD file this node represents
    void SetCADFileName( const std::string& cadFileName );

    ///Set the object from XML data
    ///\param xmlNode Node to set this object from
    virtual void SetObjectFromXMLData( DOMNode* xmlNode );

    ///Get the name of the CAD file this node represents
    std::string GetCADFileName();

    ///Copy constructor
    //\param rhs The CADPart to copy
    ///\param clone Create a clone of this node
    CADPart( CADPart& rhs, bool clone = false );

    ///Equal operator
    CADPart& operator=( const CADPart& rhs );
protected:


    ///Internally update the XML data for this node.
    ///\param input The XML data for this element.
    virtual void _updateVEElement( const std::string& input );

    ///Internally update the XML data for the CAD filename that this part represents.
    void _updateCADFileName();

    std::string m_cadFileName; ///<The name of the CAD file on disk
};

}
}
}
}
#endif// _CAD_PART_H_
