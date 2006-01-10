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
 * File:          $RCSfile: VECommand.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#ifndef _XML_VE_COMMAND_H_
#define _XML_VE_COMMAND_H_

#include <string>
#include <vector>
#include "VE_Installer/include/VEConfig.h"
#include "VE_Open/VE_XML/VEXMLObject.h"

namespace VE_XML
{
   class VEDataValuePair;
}

#include <xercesc/dom/DOM.hpp>
#include <iostream>

namespace VE_XML
{
class VE_XML_EXPORTS VECommand : public VEXMLObject
{
public:

   VECommand(DOMDocument* rootDoc);
   virtual ~VECommand();
   VECommand( const VECommand& );
   //equal operator
   VECommand& operator= ( const VECommand& );
   
   //The name of the command to execute
   void SetCommandName( std::string name );

   void AddDataValuePair(VE_XML::VEDataValuePair* commandValuePair);

   void ExtractCmdNameFromElement(DOMElement* commandElement);

   
   //set the data from an string representing the xml
   virtual void SetObjectFromXMLData(DOMNode* xmlInput);
   
   std::string GetCommandName();

   VE_XML::VEDataValuePair* GetDataValuePair(std::string dataValueName);
   VE_XML::VEDataValuePair* GetDataValuePair(unsigned int index);

   unsigned int GetNumberOfDataValuePairs();

protected:
   void _updateVEElement( std::string );
   void _updateCommandName( void );
   void _updateDataValuePairs( void );
   unsigned int _nDataValuePairs;
   std::string _cmdName;
   std::vector< VE_XML::VEDataValuePair* > _dataValuePairs;  
};
}
#endif// _XML_VE_COMMAND_H_
