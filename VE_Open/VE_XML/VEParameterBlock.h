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
 * File:          $RCSfile: VEParameterBlock.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#ifndef _XML_VE_PARAMETER_BLOCK_H_
#define _XML_VE_PARAMETER_BLOCK_H_
/*!\file VEParameterBlock.h
  Parameter Block Information API
  */
/*!\class VE_XML::VEParameterBlock
 * This class manages a parameter block which is really a
 * list of VEDataValuePair s and an optional VETransform
 */
#include <string>
#include <vector>
#include "VE_Installer/include/VEConfig.h"
#include "VE_Open/VE_XML/VEXMLObject.h"
namespace VE_XML{
   class VETransform;
   class VEDataValuePair;
}

#include <xercesc/dom/DOM.hpp>
#include <iostream>

namespace VE_XML{
class VE_XML_EXPORTS VEParameterBlock : public VEXMLObject{
public:
   ///Constructor
   ///\param rootDoc The owning DOMDocument.
   ///\param id The identification number of this parameter block
   VEParameterBlock(DOMDocument* rootDoc,unsigned int id = 0);
   ///Destructor
   virtual ~VEParameterBlock();

   ///Set the identification number.
   ///\param id The number specifiying what type of data is in this parameter block.
   void SetId(unsigned int id);

   ///Optional. Set the VETransform.
   ///\param transform The VETransform information. Commonly used with CFD datasets and CAD information.
   void SetTransform(VE_XML::VETransform* transform);

   ///Add a property, which is held in a VEDataValuePair.
   ///\param prop The VEDataValuePair holding the information such as a CFD filename.
   void AddProperty(VE_XML::VEDataValuePair* prop);
   
   ///set the data from an string representing the xml
   ///\param xmlInput The input XML data.
   virtual void SetObjectFromXMLData(DOMNode* xmlInput);
   
   ///Return the block ID.
   unsigned int GetId();
   ///Return the VETransform.
   VE_XML::VETransform* GetTransform();
   ///Return a VEDataValuePair based on a name.
   ///\param name The name of the VEDataValuePair to search for.
   VE_XML::VEDataValuePair* GetProperty(std::string name);
   ///Return the VEDataValuePair at the index.
   ///\param index The index of the VEDataValuePair.
   VE_XML::VEDataValuePair* GetProperty(unsigned int index);


protected:
   ///Internally update the XML data.
   ///\param tagName The tag name for this element
	virtual void _updateVEElement( std::string tagName);
   unsigned int _id;///<The block ID.
   VE_XML::VETransform* _dcs;///<The optional VETransform.
   std::vector<VE_XML::VEDataValuePair*> _properties;///<The VEDataValuePair list containing the block properties.
};
}
#endif// _XML_VE_PARAMETER_BLOCK_H_
