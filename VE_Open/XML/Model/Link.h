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
 * File:          $RCSfile: Link.h,v $
 * Date modified: $Date: 2006-01-14 18:41:24 -0600 (Sat, 14 Jan 2006) $
 * Version:       $Rev: 3503 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#ifndef _VE_LINK_H_
#define _VE_LINK_H_
/*!\file Link.h
  *Data ports API
  */

/*!\class VE_XML::Link
 *Class that manages the port data for a specific model.
 *These class holds the raw data and the necessary info to draw the port
 *as well as the port direction (input or output) data
 */
#include <string>
#include <vector>
#include <utility>
#include "VE_Open/XML/XMLObject.h"

#include <xercesc/dom/DOM.hpp>

namespace VE_XML
{
   class DataValuePair;
}

namespace VE_Model
{
   class Point;
}

namespace VE_Model
{
class VE_MODEL_EXPORTS Link : public VE_XML::XMLObject
{
public:
   ///Constructor
   ///\param rootDoc The owning DOMDocument
   Link( DOMDocument* rootDoc );
   ///Destructor
   virtual ~Link();
   ///Copy Constructor
   Link( const Link& );
   ///equal operator
   Link& operator= ( const Link& );
   
   ///set the data from an string representing the xml
   ///\param xmlInput The input XML data.
   virtual void SetObjectFromXMLData(DOMNode* xmlInput);
   
   ///Get the portInfo for the fromPort.
   VE_XML::DataValuePair* GetFromPort( void );
   ///Get the portInfo for the toPort.
   VE_XML::DataValuePair* GetToPort( void );
   ///Get the i'th point for a link.
   ///\param i The i'th point you are after.
   Point* GetLinkPoint( unsigned int i );

protected:
   ///Internally update the data.
   ///\param tagName The tag name of this element.
   virtual void _updateVEElement( std::string tagName );

private:
   ///raw datatypes of Link that are specified in the verg_model.xsd file
   std::vector< Point* > linkPoints;///<Vector of Points.
   ///The data value pair will contain the model and port number of the appropriate port to be linked
   std::pair< VE_XML::DataValuePair*, VE_XML::DataValuePair* > portInfo;///<The classes hold the fromPort in first and the toPort in second.
};
}
#endif// _VE_LINK_H_
