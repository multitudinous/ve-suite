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

#ifndef _VE_Tag_H_
#define _VE_Tag_H_
/*!\file Tag.h
  *Data ports API
  */

/*!\class VE_XML::VE_Model::Tag
 *Class that manages the port data for a specific model.
 *These class holds the raw data and the necessary info to draw the port
 *as well as the port direction (input or output) data
 */
#include <string>
#include <vector>
#include "VE_Open/XML/XMLObject.h"

#include <xercesc/dom/DOM.hpp>

namespace VE_XML
{
namespace VE_Model
{
   class Point;
}
}

namespace VE_XML
{
namespace VE_Model
{
class VE_MODEL_EXPORTS Tag : public VE_XML::XMLObject
{
public:
   ///Constructor
   Tag(  );
   ///Destructor
   virtual ~Tag();
   ///Copy Constructor
   Tag( const Tag& );
   ///equal operator
   Tag& operator= ( const Tag& );

   ///Set the text for the tag
   ///\param text string containing text for the tag
   void SetTagText( std::string text );
   ///set the data from an string representing the xml
   ///\param xmlInput The input XML data.
   virtual void SetObjectFromXMLData( 
    XERCES_CPP_NAMESPACE_QUALIFIER DOMNode* xmlInput);
   
   ///Get the i'th point for a Tag.
   ///\param i The i'th point you are after.
   Point* GetTagPoint( unsigned int i );
   ///Get the tag text
   std::string GetTagText( void );

protected:
   ///Internally update the data.
   ///\param tagName The tag name of this element.
   virtual void _updateVEElement( std::string tagName );

private:
   ///raw datatypes of Tag that are specified in the verg_model.xsd file
   std::vector< Point* > tagPoints;///<Vector of Points.
   std::string tagText;///<string that contains text for the tag
};
}
template<>
inline XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* XMLObject::SetSubElement(
    const std::string subElementTagName, VE_Model::Tag* val)
{
   val->SetOwnerDocument( _rootDocument );
   XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* childElement = val->GetXMLData( 
    subElementTagName );
   _veElement->appendChild( childElement );
   return childElement;
}
}
#endif// _VE_Tag_H_
