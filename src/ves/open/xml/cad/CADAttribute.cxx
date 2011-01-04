/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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
#include <ves/open/xml/cad/CADAttribute.h>
#include <ves/open/xml/cad/CADMaterial.h>
#include <ves/open/xml/shader/ShaderCreator.h>
#include <ves/open/xml/cad/CADCreator.h>
#include <ves/open/xml/XMLObjectFactory.h>
#include <ves/open/xml/shader/Program.h>

using namespace ves::open::xml::shader;
using namespace ves::open::xml::cad;
using namespace ves::open::xml;
/////////////////////////////////////////////////////////////////////////////////////
//Constructor                                                                      //
/////////////////////////////////////////////////////////////////////////////////////
CADAttribute::CADAttribute( )
        : XMLObject()
{
    _attributeType = std::string( "Material" );
    _material = CADMaterialPtr();
    _blending = false;
    _glslProgram = ProgramPtr();
    SetObjectType( "CADAttribute" );
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
/////////////////////////////
//Destructor               //
/////////////////////////////
CADAttribute::~CADAttribute()
{
    _attributeType.clear();
}
///////////////////////////////////
void CADAttribute::EnableBlending()
{
    _blending = true;
}
////////////////////////////////////
void CADAttribute::DisableBlending()
{
    _blending = false;
}
//////////////////////////////////////////////////////////////
void CADAttribute::SetAttributeType( const std::string& attributeType )
{
    _attributeType = attributeType;
}
/////////////////////////////////////////////////////////////
void CADAttribute::SetMaterial( ves::open::xml::cad::CADMaterial material )
{
    _material = CADMaterialPtr( new CADMaterial(  material ) );
    _attributeType = std::string( "Material" );
}
//////////////////////////////////////////////////////////////
void CADAttribute::SetProgram( ves::open::xml::shader::Program glslProgram )
{
    _glslProgram = ProgramPtr( new Program(  glslProgram ) );
    _attributeType = std::string( "Program" );
}
//////////////////////////////////////////////////////////
void CADAttribute::SetObjectFromXMLData( DOMNode* xmlNode )
{
    DOMElement* currentElement = 0;
    const XMLCh* name;
    if( xmlNode->getNodeType() == DOMNode::ELEMENT_NODE )
    {
        name = xmlNode->getNodeName();
        currentElement = dynamic_cast<DOMElement*>( xmlNode );
    }

    if( !currentElement )
    {
        return;
    }

    if( !currentElement->hasChildNodes() )
    {
        return;
    }
    
    DOMElement* typeNode = GetSubElement( currentElement, std::string( "type" ), 0 );
    if( typeNode )
    {
        GetDataFromElement( typeNode, _attributeType );
    }

    DOMElement* blendNode = GetSubElement( currentElement, std::string( "blending" ), 0 );
    if( blendNode )
    {
        GetDataFromElement( blendNode, _blending );
    }

    if( _attributeType == std::string( "Material" ) )
    {
        DOMElement* materialNode = GetSubElement( currentElement, std::string( "material" ), 0 );
        if( materialNode )
        {
            if( !_material )
            {
                _material = CADMaterialPtr( new CADMaterial() );
            }
            _material->SetObjectFromXMLData( materialNode );
        }
    }
    else if( _attributeType == std::string( "Program" ) )
    {
        DOMElement* programNode = GetSubElement( currentElement, std::string( "program" ), 0 );
        if( programNode )
        {
            if( !_glslProgram )
            {
                _glslProgram = ProgramPtr( new Program() );
            }
            _glslProgram->SetObjectFromXMLData( programNode );
        }
    }
}
//////////////////////////////////
bool CADAttribute::NeedsBlending()
{
    return _blending;
}
////////////////////////////////////////////
std::string CADAttribute::GetAttributeType()
{
    return _attributeType;
}
////////////////////////////////////////////////
ves::open::xml::cad::CADMaterialPtr CADAttribute::GetMaterial()
{
    return _material;
}
//////////////////////////////////////////////////
ves::open::xml::shader::ProgramPtr CADAttribute::GetGLSLProgram()
{
    return _glslProgram;
}
///////////////////////////////////////////////////
CADAttribute::CADAttribute( const CADAttribute& rhs )
        : XMLObject( rhs )
{
    _attributeType = rhs._attributeType;
    _material = CADMaterialPtr();
    _glslProgram = ProgramPtr();
    _blending = rhs._blending;

    if( _attributeType == std::string( "Material" ) )
    {
        _material = CADMaterialPtr( new CADMaterial(  *rhs._material ) );
    }
    else if( _attributeType == std::string( "Program" ) )
    {
        _glslProgram = ProgramPtr( new Program(  *rhs._glslProgram ) );
    }
}
//////////////////////////////////////////////////////////////
CADAttribute& CADAttribute::operator=( const CADAttribute& rhs )
{
    if( this != &rhs )
    {
        XMLObject::operator =( rhs );
        _attributeType = rhs._attributeType;
        _blending = rhs._blending;

        if( _attributeType == std::string( "Material" ) )
        {
            _material = CADMaterialPtr( new CADMaterial(  *rhs._material ) );
        }
        else if( _attributeType == std::string( "Program" ) )
        {
            _glslProgram = ProgramPtr( new Program(  *rhs._glslProgram ) );
        }
    }
    return *this;
}
//////////////////////////////////////////////////////
void CADAttribute::_updateVEElement( const std::string& input )
{
    SetSubElement( "type", _attributeType );
    SetSubElement( "blending", _blending );

    if( _attributeType == std::string( "Material" ) )
    {
        if( _material )
        {
            SetSubElement<ves::open::xml::XMLObjectPtr>( "material", _material );
        }
    }
    else if( _attributeType == std::string( "Program" ) )
    {
        if( _glslProgram )
        {
            SetSubElement<ves::open::xml::XMLObjectPtr>( "program", _glslProgram );
        }
    }
}
////////////////////////////////////////////
std::string CADAttribute::GetAttributeName()
{
    if( _attributeType == std::string( "Material" ) )
    {
        if( _material )
        {
            return _material->GetMaterialName();
        }
    }
    else if( _attributeType == std::string( "Program" ) )
    {
        if( _glslProgram )
        {
            return _glslProgram->GetProgramName();
        }
    }
    return std::string( "" );
}
