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
 * File:          $RCSfile: VENetwork.h,v $
 * Date modified: $Date: 2006-01-14 18:41:24 -0600 (Sat, 14 Jan 2006) $
 * Version:       $Rev: 3503 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#ifndef _VE_NETWORK_H_
#define _VE_NETWORK_H_
/*!\file VENetwork.h
  *System Network API
  */

/*!\class VE_XML::VENetwork
 *Class that manages the system network for conductor.
 */
#include <string>
#include <vector>
#include "VE_Open/VE_XML/VEXMLObject.h"

#include <xercesc/dom/DOM.hpp>

namespace VE_XML
{
   class VELink;
}

namespace VE_XML
{
class VE_XML_EXPORTS VENetwork : public VEXMLObject
{
public:
   ///Constructor
   ///\param rootDoc The owning DOMDocument
   VENetwork( DOMDocument* rootDoc );
   ///Destructor
   virtual ~VENetwork();
   ///Copy Constructor
   VENetwork( const VENetwork& );
   ///equal operator
   VENetwork& operator= ( const VENetwork& );
   
   ///set the data from an string representing the xml
   ///\param xmlInput The input XML data.
   virtual void SetObjectFromXMLData(DOMNode* xmlInput);
   
   ///Get the i'th link from the Network.
   ///\param i The i'th link you are after.
   VE_XML::VELink* GetLink( unsigned int i );

protected:
   ///Internally update the data.
   ///\param tagName The tag name of this element.
   virtual void _updateVEElement( std::string tagName );

private:
   ///raw datatypes of VENetwork that are specified in the verg_model.xsd file
   std::vector< VE_XML::VELink* > links;///<Vector of VELinks.
};
}
#endif// _VE_NETWORK_H_
