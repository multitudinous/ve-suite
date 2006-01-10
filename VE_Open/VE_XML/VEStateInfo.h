/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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
 * File:          $RCSfile: VEStateInfo.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#ifndef _XML_VE_STATE_INFO_H_
#define _XML_VE_STATE_INFO_H_
#include <vector>
#include <string>
#include "VE_Installer/include/VEConfig.h"
#include "VE_Open/VE_XML/VEXMLObject.h"
namespace VE_XML
{
   class VECommand;
}

#include <xercesc/dom/DOM.hpp>
#include <iostream>

namespace VE_XML{
class VE_XML_EXPORTS VEStateInfo : public VEXMLObject{
public:
   VEStateInfo(DOMDocument* rootDoc);
   virtual ~VEStateInfo();

   void AddState(VE_XML::VECommand* state);
   void ClearState();
   
   //set the data from an string representing the xml
   virtual void SetObjectFromXMLData(DOMNode* xmlInput);

   VE_XML::VECommand* GetState(std::string name);
   VE_XML::VECommand* GetState(unsigned int index);

protected:
	virtual void _updateVEElement( std::string );
   void _updateCommands();
   std::vector<VE_XML::VECommand*> _stateInfo;
};
}
#endif// _XML_VE_STATE_INFO_H_
