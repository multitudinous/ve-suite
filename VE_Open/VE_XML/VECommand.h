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
/*!\file VECommand.h
  *XML Command API
  */
/*!\class VE_XML::VECommand
 * This class contains commands that are sent from VE-Conductor
 * and interpretted in VE-Xplorer.
 */
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
   ///Constructor
   VECommand(DOMDocument* rootDoc);
   ///Destructor
   virtual ~VECommand();
   ///Copy Constructor
   VECommand( const VECommand& );
   ///equal operator
   VECommand& operator= ( const VECommand& );
   
   ///Set the name of the command.
   ///\param name The name of the command to execute.
   void SetCommandName( std::string name );

   ///Add a data value pair for the command.
   ///\param commandValuePair The data value pair representing command information.
   void AddDataValuePair(VE_XML::VEDataValuePair* commandValuePair);

   ///Utility function to extract a command name from an element.
   ///\param commandElement The command element.
   void ExtractCmdNameFromElement(DOMElement* commandElement);

   ///Populate the VECommand data from an XML element.
   ///\param xmlInput The element to populate the command data from.
   virtual void SetObjectFromXMLData(DOMNode* xmlInput);
   
   ///Return the name of this command.
   std::string GetCommandName();

   ///Get a specific VEDataValuePair by name.
   ///\param dataValueName The name of the VEDataValuePair to search for.
   VE_XML::VEDataValuePair* GetDataValuePair(std::string dataValueName);
   
   ///Get a VEDataValuePair at the index.
   ///\param index The index of the VEDataValuePair to return.
   VE_XML::VEDataValuePair* GetDataValuePair(unsigned int index);

   ///Return the number of VEDataValuePair s in this command.
   unsigned int GetNumberOfDataValuePairs();

protected:
   ///Internally update the command element.
   ///\param tagName The tagName of this element
   void _updateVEElement( std::string tagName);
   ///Internally update the command name from the input XML data.
   void _updateCommandName( void );
   ///Internally update the VEDataValuePair s from the input XML data.
   void _updateDataValuePairs( void );

   unsigned int _nDataValuePairs;///<The number of VEDataValuePair s in this command.
   std::string _cmdName;///<The name of this command.
   std::vector< VE_XML::VEDataValuePair* > _dataValuePairs;///<The list of VEDataValuePair s in this command.  
};
}
#endif// _XML_VE_COMMAND_H_
