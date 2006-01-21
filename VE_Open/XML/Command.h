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
 * File:          $RCSfile: Command.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#ifndef _XML_VE_COMMAND_H_
#define _XML_VE_COMMAND_H_
/*!\file Command.h
  *XML Command API
  */
/*!\class VE_XML::Command
 * This class contains commands that are sent from VE-Conductor
 * and interpretted in VE-Xplorer.
 */
#include <string>
#include <vector>

#include "VE_Open/XML/XMLObject.h"


namespace VE_XML
{
   class DataValuePair;
}

#include <xercesc/dom/DOM.hpp>
#include <iostream>

namespace VE_XML
{
class VE_XML_EXPORTS Command : public XMLObject
{
public:
   ///Constructor
   Command(DOMDocument* rootDoc);
   ///Destructor
   virtual ~Command();
   ///Copy Constructor
   Command( const Command& );
   ///equal operator
   Command& operator= ( const Command& );
   
   ///Set the name of the command.
   ///\param name The name of the command to execute.
   void SetCommandName( std::string name );

   ///Add a data value pair for the command.
   ///\param commandValuePair The data value pair representing command information.
   void AddDataValuePair(VE_XML::DataValuePair* commandValuePair);

   ///Utility function to extract a command name from an element.
   ///\param commandElement The command element.
   void ExtractCmdNameFromElement(DOMElement* commandElement);

   ///Populate the Command data from an XML element.
   ///\param xmlInput The element to populate the command data from.
   virtual void SetObjectFromXMLData(DOMNode* xmlInput);
   
   ///Return the name of this command.
   std::string GetCommandName();

   ///Get a specific DataValuePair by name.
   ///\param dataValueName The name of the DataValuePair to search for.
   VE_XML::DataValuePair* GetDataValuePair(std::string dataValueName);
   
   ///Get a DataValuePair at the index.
   ///\param index The index of the DataValuePair to return.
   VE_XML::DataValuePair* GetDataValuePair(unsigned int index);

   ///Return the number of DataValuePair s in this command.
   unsigned int GetNumberOfDataValuePairs();

protected:
   ///Internally update the command element.
   ///\param tagName The tagName of this element
   void _updateVEElement( std::string tagName);
   ///Internally update the command name from the input XML data.
   void _updateCommandName( void );
   ///Internally update the DataValuePair s from the input XML data.
   void _updateDataValuePairs( void );

   unsigned int _nDataValuePairs;///<The number of DataValuePair s in this command.
   std::string _cmdName;///<The name of this command.
   std::vector< VE_XML::DataValuePair* > _dataValuePairs;///<The list of DataValuePair s in this command.  
};
}
#endif// _XML_VE_COMMAND_H_
