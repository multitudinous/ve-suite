/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
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
#include <ves/open/xml/shader/Program.h>

#include <ves/open/xml/shader/Shader.h>
#include <ves/open/xml/shader/ShaderCreator.h>
#include <ves/open/xml/XMLObjectFactory.h>

XERCES_CPP_NAMESPACE_USE

using namespace ves::open::xml::shader;
using namespace ves::open::xml;
//////////////////////////////////////////////////////////////////////////
//Constructor                                                           //
//////////////////////////////////////////////////////////////////////////
Program::Program()
        : XMLObject()
{
    mName = std::string( "VEProgram" );
    mVertexShader = ShaderPtr();
    mFragmentShader = ShaderPtr();
    SetObjectType( "Program" );
    SetObjectNamespace( "Shader" );

    if( !XMLObjectFactory::Instance()->ObjectCreatorIsRegistered( "Shader" ) )
    {
        XMLObjectFactory::Instance()->RegisterObjectCreator( "Shader",  new ShaderCreator() );
    }
}
///////////////////
//Destructor     //
///////////////////
Program::~Program()
{
    mName.clear();
}
/////////////////////////////////////
//Copy constructor                 //
/////////////////////////////////////
Program::Program( const Program& rhs )
        : XMLObject( rhs )
{
    mName = std::string( "VEProgram" );

    if( rhs.mVertexShader )
    {
        mVertexShader = ShaderPtr( new Shader(  *rhs.mVertexShader ) );
    }
    if( rhs.mFragmentShader )
    {
        mFragmentShader = ShaderPtr( new Shader(  *rhs.mFragmentShader ) );
    }
    mName = rhs.mName;
}
/////////////////////////////////////////////////
void Program::SetVertexShader( ShaderPtr vertShader )
{
    mVertexShader = vertShader;
}
///////////////////////////////////////////////////
void Program::SetFragmentShader( ShaderPtr fragShader )
{
    mFragmentShader = fragShader;
}
//////////////////////////////////////////////
void Program::SetProgramName( const std::string& name )
{
    mName = name;
}
/////////////////////////////////////////////////////
void Program::SetObjectFromXMLData( DOMNode* xmlInput )
{
    DOMElement* currentElement = 0;

    if( xmlInput->getNodeType() == DOMNode::ELEMENT_NODE )
    {
        currentElement = dynamic_cast<DOMElement*>( xmlInput );
    }

    if( currentElement )
    {
        //break down the element
        {
            if( currentElement->hasChildNodes() )
            {
                //Get the source
                DOMElement* vertexShader = GetSubElement( currentElement, std::string( "vertexShader" ), 0 );
                if( vertexShader )
                {
                    if( !mVertexShader )
                    {
                        mVertexShader = ShaderPtr( new Shader() );
                    }
                    mVertexShader->SetObjectFromXMLData( vertexShader );
                }
                DOMElement* fragShader = GetSubElement( currentElement, std::string( "fragmentShader" ), 0 );
                if( fragShader )
                {
                    if( !mFragmentShader )
                    {
                        mFragmentShader = ShaderPtr( new Shader() );
                    }
                    mFragmentShader->SetObjectFromXMLData( fragShader );
                }
                DOMElement* nameNode = GetSubElement( currentElement, std::string( "name" ), 0 );
                if( nameNode )
                {
                    GetDataFromElement( nameNode, mName );
                }
            }
        }
    }
}
////////////////////////////////////
ShaderPtr Program::GetFragmentShader()
{
    return mFragmentShader;
}
//////////////////////////////////
ShaderPtr Program::GetVertexShader()
{
    return mVertexShader;
}
/////////////////////////////////////
const std::string& Program::GetProgramName()
{
    return mName;
}
/////////////////////////////////////////////////
void Program::_updateVEElement( const std::string& input )
{
    _updateProgramName();
    if( mVertexShader )
    {
        mVertexShader->SetOwnerDocument( mRootDocument );
        mVeElement->appendChild( mVertexShader->GetXMLData( "vertexShader" ) );
    }
    if( mFragmentShader )
    {
        mFragmentShader->SetOwnerDocument( mRootDocument );
        mVeElement->appendChild( mFragmentShader->GetXMLData( "fragmentShader" ) );
    }
}
/////////////////////////////////
void Program::_updateProgramName()
{
    DOMElement* nameElement = mRootDocument->createElement(
                              Convert( "name" ).toXMLString() );

    DOMText* name = mRootDocument->createTextNode(
                    Convert( mName ).toXMLString() );

    nameElement->appendChild( name );
    mVeElement->appendChild( nameElement );
}
///////////////////////////////////////////////
Program& Program::operator=( const Program& rhs )
{

    if( this != &rhs )
    {
        XMLObject::operator=( rhs );
        if( rhs.mVertexShader )
        {
            mVertexShader = ShaderPtr( new Shader(  *rhs.mVertexShader ) );
        }
        if( rhs.mFragmentShader )
        {
            mFragmentShader = ShaderPtr( new Shader(  *rhs.mFragmentShader ) );
        }
        mName = rhs.mName;
    }
    return *this;
}
