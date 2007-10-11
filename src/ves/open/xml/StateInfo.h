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

#ifndef _XML_VE_STATE_INFO_H_
#define _XML_VE_STATE_INFO_H_
/*!\file StateInfo.h
  State Information API
  */
/*!\class VE_XML::StateInfo
 * This class is used to hold state information which is essentially a list
 * of Command s.
 */
#include <vector>
#include <string>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/CommandPtr.h>

#include <xercesc/dom/DOM.hpp>
#include <iostream>

namespace VE_XML
{
class VE_XML_EXPORTS StateInfo : public XMLObject
{
public:
   ///Constructor 
   StateInfo();
   ///Copy Constructor
   StateInfo( const StateInfo& );
   ///equal operator
   StateInfo& operator= ( const StateInfo& );
   ///Destructor
   virtual ~StateInfo();
   
   ///Add new state information.
   ///\param state The new state to add which is held in a Command object.
   //void AddState(VE_XML::CommandPtr state);
   ///Add new state information.
   ///\param state The new state to add which is held in a Command object.
   void AddState( VE_XML::CommandWeakPtr state );
   
   ///Clear all current state information.
   void ClearState();
   
   ///Set the data from an string representing the xml
   ///\param xmlInput The input xml data.
   virtual void SetObjectFromXMLData( XERCES_CPP_NAMESPACE_QUALIFIER DOMNode* xmlInput);

   ///Get a state based on the name.
   ///\param name The name of the state to search for
   VE_XML::CommandWeakPtr GetState(std::string name);
   ///Get the state based on an index
   ///\param index The index to search for.
   VE_XML::CommandWeakPtr GetState( size_t index);
   ///Get the vector of states for this user
   std::vector< VE_XML::CommandWeakPtr > GetStateVector( void );

protected:
   ///Internally update the XML data.
   ///\param tagName The tag name to use for this element.
	virtual void _updateVEElement( std::string tagName );
   ///Internally update the Command list.
   void _updateCommands();

   std::vector<VE_XML::CommandPtr> _stateInfo;///<The Command list holding state information.
};
template<>
inline XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* XMLObject::SetSubElement(const std::string subElementTagName, StateInfo* val)
{
   val->SetOwnerDocument( _rootDocument );
   XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* childElement = val->GetXMLData( subElementTagName );
   _veElement->appendChild( childElement );
   return childElement;
}
}
#endif// _XML_VE_STATE_INFO_H_
