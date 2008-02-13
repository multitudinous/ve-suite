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
#ifndef _VES_OPEN_XML_MODEL_TAG_H_
#define _VES_OPEN_XML_MODEL_TAG_H_

#include <ves/open/xml/model/TagPtr.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/model/PointPtr.h>

#include <xercesc/dom/DOM.hpp>

#include <string>
#include <vector>


namespace ves
{
namespace open
{
namespace xml
{
namespace model
{
/*!\file Tag.h
  *Data ports API
  */

/*!\class ves::open::xml::model::Tag
 *Class that manages the port data for a specific model.
 *These class holds the raw data and the necessary info to draw the port
 *as well as the port direction (input or output) data
 */
class VE_MODEL_EXPORTS Tag : public ves::open::xml::XMLObject
{
public:
    ///Constructor
    Tag( );
    ///Destructor
    virtual ~Tag();
    ///Copy Constructor
    Tag( const Tag& );
    ///equal operator
    Tag& operator= ( const Tag& );

    ///Set the text for the tag
    ///\param text string containing text for the tag
    void SetText( const std::string& text );

    ///set the data from an string representing the xml
    ///\param xmlInput The input XML data.
    virtual void SetObjectFromXMLData(
        XERCES_CPP_NAMESPACE_QUALIFIER DOMNode* xmlInput );

    ///Get the i'th point for a Tag.
    ///\param i The i'th point you are after.
    PointPtr GetPoint( size_t i );

    ///Get the tag text
    const std::string& GetText( void );

    ///Add a new point to the tag
    ///\param newPoint The new point to be added
    void AddPoint( PointPtr newPoint );

protected:
    ///Internally update the data.
    ///\param tagName The tag name of this element.
    virtual void _updateVEElement( const std::string& tagName );

private:
    ///raw datatypes of Tag that are specified in the verg_model.xsd file
    std::vector< PointPtr > mTagPoints;///<Vector of Points.
    std::string mTagText;///<string that contains text for the tag
};

}
}
}
}
#endif// Tag_H_
