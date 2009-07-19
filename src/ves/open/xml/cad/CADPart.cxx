/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/
#include <ves/open/xml/cad/CADPart.h>
XERCES_CPP_NAMESPACE_USE

using namespace ves::open::xml::cad;
using namespace ves::open::xml;

////////////////////////////////////////////////////////////
//Constructor                                             //
////////////////////////////////////////////////////////////
CADPart::CADPart( const std::string& name )
    : 
    ves::open::xml::cad::CADNode( name ),
    m_cadFileName( "CADFile" )
{
    m_occlusionCulling = "Off";
    m_type = "Part";

    SetObjectType( "CADPart" );
}
///////////////////
//Destructor     //
///////////////////
CADPart::~CADPart()
{}
/////////////////////////////////////////////////////
void CADPart::SetCADFileName( const std::string& cadFileName )
{
    m_cadFileName = cadFileName;
}
/////////////////////////////////////
std::string CADPart::GetCADFileName()
{
    return m_cadFileName;
}
//////////////////////////////////
void CADPart::_updateCADFileName()
{
    /*DOMElement* nameElement  = mRootDocument->createElement(
                               Convert( "fileName" ).toXMLString() );

    mVeElement->appendChild( nameElement );

    DOMText* fileName = mRootDocument->createTextNode(
                        Convert( m_cadFileName ).toXMLString() );

    nameElement->appendChild( fileName );*/
    
    SetSubElement( "fileName", m_cadFileName );
}
/////////////////////////////////////////////////
void CADPart::_updateVEElement( const std::string& input )
{
    ves::open::xml::cad::CADNode::_updateVEElement( input );

    SetAttribute( "occlusionCulling", m_occlusionCulling );

    _updateCADFileName();
}
/////////////////////////////////////////////////////
void CADPart::SetObjectFromXMLData( DOMNode* xmlNode )
{
    DOMElement* currentElement = 0;

    if( xmlNode->getNodeType() == DOMNode::ELEMENT_NODE )
    {
        currentElement = dynamic_cast<DOMElement*>( xmlNode );
    }

    if( !currentElement )
    {
        return;
    }
    //populate the base elements in node
    ves::open::xml::cad::CADNode::SetObjectFromXMLData( currentElement );

    if( currentElement->hasChildNodes() )
    {
        DOMElement* fileNameElement = 
            GetSubElement( currentElement, std::string( "fileName" ), 0 );
        GetDataFromElement( fileNameElement, m_cadFileName );
    }
    
    if( currentElement->getAttributeNode(
        Convert( "occlusionCulling" ).toXMLString() ) )
    {
        XMLObject::GetAttribute( currentElement, "occlusionCulling", m_occlusionCulling );
    }
    else
    {
        m_occlusionCulling = "Off";
    }
    
}
////////////////////////////////////////////////
CADPart::CADPart( CADPart& rhs, bool clone )
        : ves::open::xml::cad::CADNode( rhs, clone )
{
    m_cadFileName = rhs.m_cadFileName;
}
////////////////////////////////////////////////
CADPart& CADPart::operator=( const CADPart& rhs )
{
    if( this != &rhs )
    {
        ves::open::xml::cad::CADNode::operator =( rhs );
        m_cadFileName = rhs.m_cadFileName;
    }
    return *this;
}

