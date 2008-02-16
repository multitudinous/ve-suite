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
#include <ves/open/xml/Transform.h>
#include <ves/open/xml/shader/Program.h>
#include <ves/open/xml/cad/CADAssembly.h>
#include <ves/open/xml/cad/CADMaterial.h>
#include <ves/open/xml/cad/CADCreator.h>
#include <ves/open/xml/cad/CADNodeAnimation.h>

#include <ves/open/xml/XMLObjectFactory.h>

#include <ves/open/xml/shader/Shader.h>
#include <ves/open/xml/shader/Program.h>
#include <ves/open/xml/shader/ShaderCreator.h>

#include <ctime>

#include <apr_uuid.h>

using namespace ves::open::xml::cad;
using namespace ves::open::xml::shader;
using namespace ves::open::xml;


//////////////////////////////////
///Constructor                  //
//////////////////////////////////
CADNode::CADNode( std::string name )
        :
        ves::open::xml::XMLObject()
{
    m_name = name;
    m_parent = "";
    m_transform = new Transform();
    m_type = std::string( "Node" );
    m_visibility = true;

    m_physics = false;
    m_mass = 1.0f;
    m_friction = 1.0f;
    m_restitution = 0.0f;
    m_physicsMesh = "Bounding Box";

    m_activeAttributeName = std::string( "" );
    SetObjectType( "CADNode" );
    SetObjectNamespace( "CAD" );
    //This may need to be somewhere else
    if( !XMLObjectFactory::Instance()->ObjectCreatorIsRegistered( "CAD" ) )
    {
        XMLObjectFactory::Instance()->RegisterObjectCreator( "CAD", new CADCreator() );
    }

    if( !XMLObjectFactory::Instance()->ObjectCreatorIsRegistered( "Shader" ) )
    {
        XMLObjectFactory::Instance()->RegisterObjectCreator( "Shader", new ShaderCreator() );
    }

}
///////////////////
///Destructor    //
///////////////////
CADNode::~CADNode()
{
    m_attributeList.clear();
    m_animations.clear();
}
//////////////////////////////////////////////////////////////////////////
void CADNode::AddAnimation( std::string name, std::string animationFileName )
{
    CADNodeAnimation newAnimation;
    newAnimation.SetAnimationFileName( animationFileName );
    newAnimation.SetAnimationName( name );
    m_animations.push_back( newAnimation );
}
///////////////////////////////////////////
void CADNode::SetNodeName( std::string name )
{
    m_name = name;
}
////////////////////////////////////////////////////
void CADNode::SetParent( std::string parent )
{
    m_parent = parent;
}
///////////////////////////////////////////////////////
void CADNode::SetTransform( ves::open::xml::TransformPtr transform )
{
    m_transform = new ves::open::xml::Transform( *transform );
}
///////////////////////////////////////////////////////////
void CADNode::AddAttribute( ves::open::xml::cad::CADAttribute attribute )
{
    m_attributeList.push_back( attribute );
}
////////////////////////////////////////////////////////
void CADNode::RemoveAttribute( std::string attributeName )
{
    for( std::vector<CADAttribute>::iterator itr = m_attributeList.begin();
            itr != m_attributeList.end();
            itr++ )
    {
        if (( *itr ).GetAttributeName() == attributeName )
        {
            m_attributeList.erase( itr );
            break;
        }
    }
}
///////////////////////////////////////////////////////////
void CADNode::SetActiveAttribute( std::string attributeName )
{
    m_activeAttributeName = attributeName;
}
////////////////////////////
bool CADNode::HasAnimation()
{
    return ( !m_animations.empty() );
}
//////////////////////////////////
bool CADNode::HasPhysics()
{
    return m_physics;
}
//////////////////////////////////
void CADNode::EnablePhysics()
{
    m_physics = true;
}
//////////////////////////////////
void CADNode::SetMass( double mass )
{
    m_mass = mass;
}
//////////////////////////////////
double CADNode::GetMass()
{
    return m_mass;
}
//////////////////////////////////
void CADNode::SetFriction( double friction )
{
    m_friction = friction;
}
//////////////////////////////////
double CADNode::GetFriction()
{
    return m_friction;
}
//////////////////////////////////
void CADNode::SetRestitution( double restitution )
{
    m_restitution = restitution;
}
//////////////////////////////////
double CADNode::GetRestitution()
{
    return m_restitution;
}
//////////////////////////////////
void CADNode::SetPhysicsMesh( std::string physicsMesh )
{
    m_physicsMesh = physicsMesh;
}
//////////////////////////////////
std::string CADNode::GetPhysicsMesh()
{
    return m_physicsMesh;
}
//////////////////////////////////
std::string CADNode::GetNodeType()
{
    return m_type;
}
//////////////////////////////////
std::string CADNode::GetNodeName()
{
    return m_name;
}
/////////////////////////////////////////
std::string CADNode::GetParent()
{
    return m_parent;
}
//////////////////////////////////////////
ves::open::xml::TransformPtr CADNode::GetTransform()
{
    return m_transform;
}
///////////////////////////////////////////////////////////////////////
ves::open::xml::cad::CADAttribute& CADNode::GetAttribute( unsigned int index )
{
    try
    {
        return m_attributeList.at( index );
    }
    catch ( ... )
    {
        std::cout << "ERROR!!!!!" << std::endl;
        std::cout << "Invalid index!!!" << std::endl;
        std::cout << "CADNode::GetAttribute(): " << index << std::endl;
        return m_attributeList.at( 0 );
        ;
    }
    return m_attributeList.at( 0 );
    ;
}
/////////////////////////////////////////////////////////////////////
ves::open::xml::cad::CADAttribute& CADNode::GetAttribute( std::string name )
{
    size_t nAttributes = m_attributeList.size();
    for( size_t i = 0; i < nAttributes; i++ )
    {
        if( m_attributeList.at( i ).GetAttributeName() == name )
        {
            return m_attributeList.at( i );
        }
    }
}
///////////////////////////////////////////////////////////
ves::open::xml::cad::CADAttribute& CADNode::GetActiveAttribute()
{
    return GetAttribute( m_activeAttributeName );
}
/////////////////////////////
/*unsigned int CADNode::GetID()
{
   return _uID;
}*/
/////////////////////////////////////////////////
void CADNode::_updateVEElement( const std::string& input )
{
    _updateNodeType();
    _updateNodeName();

    SetAttribute( "id", mUuid );
    SetAttribute( "visibility", m_visibility );

    SetAttribute( "physics", m_physics );
    SetAttribute( "mass", m_mass );
    SetAttribute( "friction", m_friction );
    SetAttribute( "restitution", m_restitution );
    //SetAttribute( "physics mesh", wxString( m_physicsMesh ) );

    SetSubElement( std::string( "parent" ), m_parent );

    if( !m_transform )
    {
        m_transform = new Transform();
    }
    m_transform->SetOwnerDocument( mRootDocument );
    mVeElement->appendChild( m_transform->GetXMLData( "transform" ) );

    if( m_attributeList.size() )
    {
        size_t nAttributes = m_attributeList.size();
        for( size_t i = 0; i < nAttributes; i++ )
        {
            m_attributeList.at( i ).SetOwnerDocument( mRootDocument );
            mVeElement->appendChild( m_attributeList.at( i ).GetXMLData( "attribute" ) );
        }
        SetSubElement( std::string( "activeAttributeName" ), m_activeAttributeName );
    }

    if( m_animations.size() )
    {
        size_t nAnimations = m_animations.size();
        for( size_t i = 0; i < nAnimations; i++ )
        {
            m_animations.at( i ).SetOwnerDocument( mRootDocument );
            mVeElement->appendChild( m_animations.at( i ).GetXMLData( "animation" ) );
        }
    }
}
///////////////////////////////
void CADNode::_updateNodeName()
{
    DOMElement* nodeNameElement = mRootDocument->createElement(
                                  Convert( "name" ).toXMLString() );

    DOMText* nodeName = mRootDocument->createTextNode(
                        Convert( m_name ).toXMLString() );

    nodeNameElement->appendChild( nodeName );
    mVeElement->appendChild( nodeNameElement );
}
////////////////////////////////////////////
void CADNode::_updateNodeType()
{
    DOMElement* nodeTypeElement = mRootDocument->createElement(
                                  Convert( "type" ).toXMLString() );

    DOMText* nodeType = mRootDocument->createTextNode(
                        Convert( m_type ).toXMLString() );

    nodeTypeElement->appendChild( nodeType );
    mVeElement->appendChild( nodeTypeElement );
}
/////////////////////////////////////////////////////
void CADNode::SetObjectFromXMLData( DOMNode* xmlNode )
{
    DOMElement* currentElement = 0;
    const XMLCh* name;
    if( xmlNode->getNodeType() == DOMNode::ELEMENT_NODE )
    {
        name = xmlNode->getNodeName();
        currentElement = dynamic_cast<DOMElement*>( xmlNode );
    }

    if( currentElement )
    {
        //break down the element
        {
            if( currentElement->hasChildNodes() )
            {
                if( currentElement->getAttributeNode(
                    Convert( "visibility" ).toXMLString() ) )
                {
                    XMLObject::GetAttribute( currentElement, "visibility", m_visibility );
                }
                else
                {
                    m_visibility = true;
                }

                if( currentElement->getAttributeNode(
                    Convert( "physics" ).toXMLString() ) )
                {
                    XMLObject::GetAttribute( currentElement, "physics", m_physics );
                }
                else
                {
                    m_physics = false;
                }

                if( currentElement->getAttributeNode(
                    Convert( "mass" ).toXMLString() ) )
                {
                    XMLObject::GetAttribute( currentElement, "mass", m_mass );
                }
                else
                {
                    m_mass = 1.0f;
                }

                if( currentElement->getAttributeNode(
                    Convert( "friction" ).toXMLString() ) )
                {
                    XMLObject::GetAttribute( currentElement, "friction", m_friction );
                }
                else
                {
                    m_friction = 1.0f;
                }

                if( currentElement->getAttributeNode(
                    Convert( "restitution" ).toXMLString() ) )
                {
                    XMLObject::GetAttribute( currentElement, "restitution", m_restitution );
                }
                else
                {
                    m_restitution = 0.0f;
                }

                if( currentElement->getAttributeNode(
                    Convert( "physics mesh" ).toXMLString() ) )
                {
                    XMLObject::GetAttribute( currentElement, "physics mesh", m_physicsMesh );
                }
                else
                {
                    m_physicsMesh = "Bounding Box";
                }

                //Is there a better way to do this
                DOMElement* nameNode = GetSubElement( currentElement, std::string( "name" ), 0 );
                if( nameNode )
                {
                    GetDataFromElement( nameNode, m_name );
                }

                DOMElement* idNode = GetSubElement( currentElement, std::string( "nodeID" ), 0 );
                if( idNode )
                {
                    unsigned int tmp_id = 0;
                    GetDataFromElement( idNode, tmp_id );
                    ves::open::xml::XMLObject::SetID( tmp_id );
                }
                else
                {
                    XMLObject::GetAttribute( currentElement, "id", mUuid );
                }
                
                DOMElement* typeNode = GetSubElement( currentElement, std::string( "type" ), 0 );
                if( typeNode )
                {
                    GetDataFromElement( typeNode, m_type );
                }
                
                DOMElement* parentNode = GetSubElement( currentElement, std::string( "parent" ), 0 );
                if( parentNode )
                {
                    GetDataFromElement( typeNode, m_parent );
                }
                
                m_attributeList.clear();
                DOMNodeList* attributeNodes = currentElement->getElementsByTagName( Convert( "attribute" ).toXMLString() );
                XMLSize_t nNewAttributes = attributeNodes->getLength();
                for( XMLSize_t  i = 0; i < nNewAttributes ; i++ )
                {
                    DOMElement* attributeNode = dynamic_cast<DOMElement*>( attributeNodes->item( i ) );

                    //Need to check if the returned attribute belongs to this node
                    if( attributeNode->getParentNode() == currentElement )
                    {
                        //CADAttribute* newAttribute = new CADAttribute();
                        CADAttribute newAttribute;
                        newAttribute.SetObjectFromXMLData( attributeNode );
                        m_attributeList.push_back( newAttribute );
                    }
                }

                m_animations.clear();
                DOMNodeList* animationNodes = currentElement->getElementsByTagName( Convert( "animation" ).toXMLString() );
                XMLSize_t nNewAnimations = animationNodes->getLength();
                for( XMLSize_t  i = 0; i < nNewAnimations ; i++ )
                {
                    DOMElement* animationNode = dynamic_cast<DOMElement*>( animationNodes->item( i ) );

                    if( animationNode->getParentNode() == currentElement )
                    {
                        CADNodeAnimation newAnimation;
                        newAnimation.SetObjectFromXMLData( animationNode );
                        m_animations.push_back( newAnimation );
                    }
                }


                DOMElement* activeAttribNode = GetSubElement( currentElement, std::string( "activeAttributeName" ), 0 );
                if( activeAttribNode )
                {
                    GetDataFromElement( activeAttribNode, m_activeAttributeName);
                    SetActiveAttribute( m_activeAttributeName );
                }


                DOMElement* transformNode = GetSubElement( currentElement, std::string( "transform" ), 0 );
                if( transformNode )
                {
                    if( !m_transform )
                    {
                        m_transform = new Transform();
                    }
                    m_transform->SetObjectFromXMLData( transformNode );
                }

            }
        }
    }
}
///////////////////////////////////////
void CADNode::SetVisibility( bool onOff )
{
    m_visibility = onOff;
}
/////////////////////////////
bool CADNode::GetVisibility()
{
    return m_visibility;
}
/////////////////////////////////////////////////////
std::vector<CADAttribute> CADNode::GetAttributeList()
{
    return m_attributeList;
}
///////////////////////////////////////////////////////////////////
ves::open::xml::cad::CADNodeAnimation& CADNode::GetAnimation( unsigned int index )
{
    try
    {
        return m_animations.at( index );
    }
    catch ( ... )
    {
        std::cout << "Invalid animation index: " << index << std::endl;
        std::cout << "CADNode::GetAnimation()" << std::endl;
    }
}
/////////////////////////////////////////////////////////
CADNodeAnimation& CADNode::GetAnimation( std::string name )
{
    size_t nAnimations = m_animations.size();
    for( size_t i = 0; i < nAnimations; i++ )
    {
        if( m_animations.at( i ).GetAnimationName() == name )
        {
            return m_animations.at( i );
        }
    }
}
///////////////////////////////////////
size_t CADNode::GetNumberOfAnimations()
{
    return m_animations.size();
}
///////////////////////////////////////////////
CADNode::CADNode( const CADNode& rhs, bool clone )
        : ves::open::xml::XMLObject( rhs )
{
    m_parent = "";
    m_transform = 0;

    if( rhs.m_transform )
    {
        m_transform = new ves::open::xml::Transform( *rhs.m_transform );
    }
    else
    {
        m_transform = new Transform();
    }

    if( m_attributeList.size() )
    {
        m_attributeList.clear();
    }
    for( size_t i = 0; i < rhs.m_attributeList.size(); i++ )
    {
        m_attributeList.push_back( rhs.m_attributeList.at( i ) );
    }
    for( size_t i = 0; i < rhs.m_animations.size(); i++ )
    {
        m_animations.push_back( rhs.m_animations.at( i ) );
    }
    m_activeAttributeName = rhs.m_activeAttributeName;
    m_parent = rhs.m_parent;
    m_name = rhs.m_name;
    m_type = rhs.m_type;
    m_visibility = rhs.m_visibility;

    m_physics = rhs.m_physics;
    m_mass = rhs.m_mass;
    m_friction = rhs.m_friction;
    m_restitution = rhs.m_restitution;
    m_physicsMesh = rhs.m_physicsMesh;

    //maintain a unique ID
    if( clone )
    {
        apr_uuid_t tempuuid;
        apr_uuid_get( &tempuuid );
        char* buffer = new char[ APR_UUID_FORMATTED_LENGTH + 1 ];
        apr_uuid_format( buffer, &tempuuid );
        mUuid.assign( buffer );
        delete [] buffer;
    }
}
////////////////////////////////////////////////
CADNode& CADNode::operator=( const CADNode& rhs )
{
    //std::cout<<"CADNode operator= "<<std::endl;
    //std::cout<<"rhs: "<<rhs._uID<<std::endl;
    if( this != &rhs )
    {
        XMLObject::operator =( rhs );
        if( m_attributeList.size() )
        {
            m_attributeList.clear();
        }

        for( size_t i = 0; i < rhs.m_attributeList.size(); i++ )
        {
            m_attributeList.push_back( rhs.m_attributeList.at( i ) );
        }

        if( m_animations.size() )
        {
            m_animations.clear();
        }
        for( size_t i = 0; i < rhs.m_animations.size(); i++ )
        {
            m_animations.push_back( rhs.m_animations.at( i ) );
        }

        m_transform = new Transform( *rhs.m_transform );
        m_activeAttributeName = rhs.m_activeAttributeName;
        m_visibility = rhs.m_visibility;

        m_physics = rhs.m_physics;
        m_mass = rhs.m_mass;
        m_friction = rhs.m_friction;
        m_restitution = rhs.m_restitution;
        m_physicsMesh = rhs.m_physicsMesh;

        //_uID = rhs._uID;
        m_parent = rhs.m_parent;
        m_name = rhs.m_name;
    }
    return *this;
}

