/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
#include <map>

#include "VE_Open/XML/XMLObject.h"
#include "VE_Open/XML/DataValuePairPtr.h"

#include <xercesc/dom/DOM.hpp>
#include <iostream>

namespace VE_XML
{
class VE_XML_EXPORTS Command : public XMLObject
{
public:
   ///Constructor
   Command();
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
   void AddDataValuePair( VE_XML::DataValuePair* commandValuePair );
   ///Add a data value pair for the command.
   ///\param commandValuePair The data value pair representing command information.
   void AddDataValuePair( VE_XML::DataValuePairPtr commandValuePair );
   
   ///Utility function to extract a command name from an element.
   ///\param commandElement The command element.
   void ExtractCmdNameFromElement( XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* commandElement);

   ///Populate the Command data from an XML element.
   ///\param xmlInput The element to populate the command data from.
   virtual void SetObjectFromXMLData( XERCES_CPP_NAMESPACE_QUALIFIER DOMNode* xmlInput);
   
   ///Return the name of this command.
   ///\return The command name of this element
   std::string GetCommandName();

   ///Get a specific DataValuePair by name.
   ///\param dataValueName The name of the DataValuePair to search for.
   ///\return The dvp with the requested name
   VE_XML::DataValuePair* GetDataValuePair(std::string dataValueName);
   
   ///Get a DataValuePair at the index.
   ///\param index The index of the DataValuePair to return.
   ///\return The dvp at the requested index
   VE_XML::DataValuePair* GetDataValuePair( int index );

   ///Return the number of DataValuePair s in this command.
   ///\return The number of dvps stored in this command
   size_t GetNumberOfDataValuePairs();

protected:
   ///Internally update the command element.
   ///\param tagName The tagName of this element
   void _updateVEElement( std::string tagName);
   ///Internally update the command name from the input XML data.
   void _updateCommandName( void );
   ///Internally update the DataValuePair s from the input XML data.
   void _updateDataValuePairs( void );

   std::string _cmdName;///<The name of this command.
   std::vector< VE_XML::DataValuePair* > _dataValuePairs;///<The list of DataValuePair s in this command.  
   std::map< std::string, VE_XML::DataValuePair* > nameToDataValuePairMap;///<The list of DataValuePair s in this command.  
};
template<>
inline XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* XMLObject::SetSubElement(const std::string subElementTagName, Command* val)
{
   val->SetOwnerDocument( _rootDocument );
   XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* childElement = val->GetXMLData( subElementTagName );
   _veElement->appendChild( childElement );
   return childElement;
}
}
#endif// _XML_VE_COMMAND_H_
