/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
#include <ves/open/xml/cad/CADAssembly.h>
#include <ves/open/xml/cad/CADPart.h>
#include <ves/open/xml/XMLObjectFactory.h>
#include <sstream>
XERCES_CPP_NAMESPACE_USE

using namespace ves::open::xml::cad;
using namespace ves::open::xml;

////////////////////////////////////////////////////////////////////
CADAssembly::CADAssembly( std::string name )
        : ves::open::xml::cad::CADNode( name )
{

    m_associatedDataset = "NONE";
    m_numChildren = 0;
    m_type = std::string( "Assembly" );
    SetObjectType( "CADAssembly" );
}
///////////////////////////
///Destructor            //
///////////////////////////
CADAssembly::~CADAssembly()
{
    for( unsigned int i = 0; i < m_numChildren; i++ )
    {
        try
        {
            //if(m_children.at(i))
            {

                delete m_children.at( i );
            }

            m_children.at( i ) = 0;
        }
        catch ( ... )
        {
            std::cout << "Child deleted!" << std::endl;
        }
    }

    m_children.clear();
    m_numChildren = 0;
}
/////////////////////////////////////////////////
void CADAssembly::AddChild( ves::open::xml::cad::CADNode* node )
{
    m_children.push_back( node );
    m_children.back()->SetParent( uuid );
    /*if(node->GetNodeType() == "Assembly")
    {
       CADAssembly temp(*dynamic_cast<CADAssembly*>(node));
       temp.SetParent(_uID);
       m_children.push_back(temp);
    }
    else if(node->GetNodeType() == "Part")
    {
       CADPart temp(*dynamic_cast<CADPart*>(node));
       temp.SetParent(_uID);
       m_children.push_back(temp);
    }*/
    m_numChildren = static_cast< unsigned int >( m_children.size() );
}
/////////////////////////////////////////////////
/*void CADAssembly::AddChild(VE_XML::VE_CAD::CADNode node)
{
   m_children.push_back(node);
   m_children.back().SetParent(_uID);
   m_numChildren = static_cast< unsigned int >(m_children.size());
}*/
//////////////////////////////////////////////////////////////////////
void CADAssembly::SetAssociatedDataset( std::string parameterBlockUUID )
{
    m_associatedDataset = parameterBlockUUID;
}
///////////////////////////////////////////////////////////////////////
bool CADAssembly::GetAssociatedDataset( std::string& parameterBlockUUID )
{
    if( m_associatedDataset != "NONE" )
    {
        parameterBlockUUID = m_associatedDataset;
        return true;
    }
    return false;
}
////////////////////////////////////////////////////
bool CADAssembly::RemoveChild( ves::open::xml::cad::CADNode node )
{
    RemoveChild( node.GetID() );
    return false;
}
//////////////////////////////////////////////////////
bool CADAssembly::RemoveChild( std::string whichChildID )
{
    std::vector<CADNode*>::iterator childToRemove;
    for( childToRemove = m_children.begin();
            childToRemove != m_children.end();
            childToRemove++ )
    {
        CADNode* childNode = ( *childToRemove );
        if( whichChildID  == childNode->GetID() )
        {
            m_children.erase( childToRemove );
            m_numChildren = static_cast< unsigned int >( m_children.size() );
            return true;
        }
    }
    return false;
}
///////////////////////////////////////////////
unsigned int CADAssembly::GetNumberOfChildren()
{
    return m_numChildren;
}
/////////////////////////////////////////////////////////
ves::open::xml::cad::CADNode* CADAssembly::GetChild( std::string name )
{
    for( size_t i = 0; i < m_numChildren; i++ )
    {
        if( m_children.at( i )->GetNodeName() == name )
        {
            return m_children.at( i );
        }
    }
    return 0;
}
///////////////////////////////////////////////////////////////
ves::open::xml::cad::CADNode* CADAssembly::GetChild( unsigned int whichChild )
{
    return m_children.at( whichChild );
}
///////////////////////////////////
void CADAssembly::_updateChildren()
{
    DOMElement* childList = _rootDocument->createElement(
                            Convert( "children" ).toXMLString() );

    //the number of children
    DOMElement* nchildrenElement = _rootDocument->createElement(
                                   Convert( "numChildren" ).toXMLString() );

    std::stringstream int2string;
    int2string << m_numChildren;
    DOMText* numberOfChildren = _rootDocument->createTextNode(
                                Convert( int2string.str() ).toXMLString() );

    nchildrenElement->appendChild( numberOfChildren );
    _veElement->appendChild( nchildrenElement );

    //add the children nodes to the list
    for( unsigned int i = 0; i < m_numChildren;  i++ )
    {
        m_children.at( i )->SetOwnerDocument( _rootDocument );
        m_children.at( i )->SetParent( uuid );
        childList->appendChild( m_children.at( i )->GetXMLData( "child" ) );
    }
    _veElement->appendChild( childList );
}
/////////////////////////////////////////////////////
void CADAssembly::_updateVEElement( const std::string& input )
{
    //Get the base elements from CADNode
    ves::open::xml::cad::CADNode::_updateVEElement( input );
    _updateChildren();
    SetAttribute( "associatedDataset", m_associatedDataset );
}
/////////////////////////////////////////////////////
void CADAssembly::SetObjectFromXMLData( DOMNode* xmlNode )
{
    DOMElement* currentElement = 0;

    if( xmlNode->getNodeType() == DOMNode::ELEMENT_NODE )
    {
        currentElement = dynamic_cast<DOMElement*>( xmlNode );
    }

    if( currentElement )
    {
        //populate the base elements in node
        ves::open::xml::cad::CADNode::SetObjectFromXMLData( xmlNode );

        //clear out the current list of children
        if( m_numChildren )
        {
            m_children.clear();
        }
        //get the new number of children
        {
            DOMElement* nChildrenElement = GetSubElement( currentElement, std::string( "numChildren" ), 0 );
            XMLObject::GetAttribute( nChildrenElement, "numChildren", m_numChildren );
        }
        if( currentElement->getAttributeNode(
            Convert( "associatedDataset" ).toXMLString() ) )
        {
            dynamic_cast<ves::open::xml::XMLObject*>( this )->GetAttribute( currentElement,
                    "associatedDataset",
                    m_associatedDataset );
        }
        else
        {
            m_associatedDataset = "NONE";
        }
        //populate the childList
        {
            DOMNodeList* childList = currentElement->getElementsByTagName(
                                     Convert( "children" ).toXMLString() );

            DOMElement* childListElement = dynamic_cast<DOMElement*>( childList->item( 0 ) );
            DOMNodeList* childrenNodes = childListElement->getElementsByTagName(
                                         Convert( "child" ).toXMLString() );

            size_t nChilderenReally = childrenNodes->getLength();
            for( unsigned int i = 0; i < nChilderenReally; i++ )
            {
                DOMElement* cadNode = dynamic_cast<DOMElement*>( childrenNodes->item( i ) );
                unsigned int g = 0;
                while( cadNode->getParentNode() != childListElement )
                {
                    i++;
                    cadNode = dynamic_cast<DOMElement*>( childrenNodes->item( i ) );
                    if( i == nChilderenReally )return;
                }
                DOMElement* nodeType = 0;
                unsigned int k = 0;
                while( !nodeType )
                {
                    nodeType = GetSubElement( cadNode, std::string( "type" ), k );
                    k++;
                }
                if( nodeType )
                {
                    std::string tmpNodeType;
                    XMLObject::GetAttribute( nodeType, "type", tmpNodeType );
                    if( tmpNodeType == std::string( "Assembly" ) )
                    {
                        //this is an Assembly
                        ves::open::xml::cad::CADAssembly* newAssembly = new ves::open::xml::cad::CADAssembly();
                        //VE_XML::VE_CAD::CADAssembly newAssembly;// = new VE_XML::VE_CAD::CADAssembly();
                        newAssembly->SetObjectFromXMLData( cadNode );
                        newAssembly->SetParent( uuid );
                        m_children.push_back( newAssembly );
                    }
                    else if( tmpNodeType == std::string( "Part" ) )
                    {
                        //this is a Part
                        ves::open::xml::cad::CADPart* newPart = new ves::open::xml::cad::CADPart();
                        //VE_XML::VE_CAD::CADPart newPart;// = new VE_XML::VE_CAD::CADPart();
                        newPart->SetObjectFromXMLData( cadNode );
                        newPart->SetParent( uuid );
                        m_children.push_back( newPart );
                    }
                    else
                    {
                        std::cout << "ERROR!" << std::endl;
                        std::cout << "Unknown node type:"
                        << tmpNodeType << std::endl;
                    }
                }
            }
        }
    }
}
///////////////////////////////////////////////////////////
CADAssembly::CADAssembly( const CADAssembly& rhs, bool clone )
        : ves::open::xml::cad::CADNode( rhs, clone )
{

    m_numChildren = rhs.m_numChildren;
    m_associatedDataset = rhs.m_associatedDataset;
    for( unsigned int i = 0; i < m_numChildren; i++ )
    {

        m_children.push_back( dynamic_cast<CADNode*>( XMLObjectFactory::Instance()->CreateXMLObjectCopy( rhs.m_children.at( i ) ) ) );
    }
}
///////////////////////////////////////////////////////////
CADAssembly& CADAssembly::operator=( const CADAssembly& rhs )
{
    if( this != &rhs )
    {
        ves::open::xml::cad::CADNode::operator =( rhs );
        for( int i = m_numChildren - 1; i >= 0; i-- )
        {
            delete m_children.at( i );
        }
        m_children.clear();
        m_numChildren = rhs.m_numChildren;

        for( unsigned int i = 0; i < m_numChildren; i++ )
        {
            m_children.push_back( rhs.m_children.at( i ) );
        }
        m_associatedDataset = rhs.m_associatedDataset;
    }
    return *this;
}

