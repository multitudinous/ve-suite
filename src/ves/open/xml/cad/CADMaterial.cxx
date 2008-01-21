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
#include <ves/open/xml/cad/CADMaterial.h>
#include <ves/open/xml/cad/CADCreator.h>
#include <ves/open/xml/FloatArray.h>
#include <ves/open/xml/XMLObjectFactory.h>
XERCES_CPP_NAMESPACE_USE

using namespace ves::open::xml;
using namespace ves::open::xml::cad;

////////////////////////////////////////////////////////////////////
//Constructor                                                     //
////////////////////////////////////////////////////////////////////
CADMaterial::CADMaterial( std::string name )
        : ves::open::xml::XMLObject()
{
    std::vector< double > temp;
    temp.assign( 4, 1.0f );

    _kDiffuse = new ves::open::xml::FloatArray();
    _kDiffuse->SetArray( temp );

    _kEmissive = new ves::open::xml::FloatArray();
    _kEmissive->AddElementToArray( 0.0 );
    _kEmissive->AddElementToArray( 0.0 );
    _kEmissive->AddElementToArray( 0.0 );
    _kEmissive->AddElementToArray( 1.0 );

    _ambient = new ves::open::xml::FloatArray();
    _ambient->SetArray( temp );

    _specular = new ves::open::xml::FloatArray();
    _specular->SetArray( temp );
    _shininess = 50.0;
    _materialName = name;
    _opacity = 1.0;

    _colorMode = std::string( "Ambient_and_Diffuse" );
    _face = std::string( "Front_and_Back" );
    SetObjectType( "CADMaterial" );
    SetObjectNamespace( "CAD" );
    //This may need to be somewhere else
    if( !XMLObjectFactory::Instance()->ObjectCreatorIsRegistered( "CAD" ) )
    {
        XMLObjectFactory::Instance()->RegisterObjectCreator( "CAD", new CADCreator() );
    }
}
///////////////////////////
//Destructor             //
///////////////////////////
CADMaterial::~CADMaterial()
{
    if( _kDiffuse )
    {
        delete _kDiffuse;
        _kDiffuse = 0;
    }
    if( _kEmissive )
    {
        delete _kEmissive;
        _kEmissive = 0;
    }
    if( _ambient )
    {
        delete _ambient;
        _ambient = 0;
    }
    if( _specular )
    {
        delete _specular;
        _specular = 0;
    }
}
////////////////////////////////////////////////////
void CADMaterial::SetFace( std::string faceToApplyTo )
{
    _face = faceToApplyTo;
}
////////////////////////////////////////////////////
void CADMaterial::SetColorMode( std::string colorMode )
{
    _colorMode = colorMode;
}
//////////////////////////////////////////////////////////////////
void CADMaterial::SetDiffuseComponent( ves::open::xml::FloatArray* diffuse )
{
    _kDiffuse = diffuse;
}
////////////////////////////////////////////////////////////////////////
void CADMaterial::SetComponent( std::string componentName, std::vector<double> values )
{
    if( componentName == "Diffuse" )
    {
        _kDiffuse->SetArray( values );
    }
    else if( componentName == "Ambient" )
    {
        _ambient->SetArray( values );
    }
    else if( componentName == "Emissive" )
    {
        _kEmissive->SetArray( values );
    }
    else if( componentName == "Specular" )
    {
        _specular->SetArray( values );
    }
    else
    {
        std::cout << "Invalid component specified: " << componentName << " for material: " << _materialName << std::endl;
    }
}
////////////////////////////////////////////////////////////////////////
void CADMaterial::SetComponent( std::string componentName, double* values )
{
    std::vector<double> temp;
    temp.push_back( values[0] );
    temp.push_back( values[1] );
    temp.push_back( values[2] );
    temp.push_back( values[3] );
    SetComponent( componentName, temp );
}
///////////////////////////////////////////////////////
void CADMaterial::SetEmissiveComponent( ves::open::xml::FloatArray* emissive )
{
    _kEmissive = emissive;
}
/////////////////////////////////////////////////////
void CADMaterial::SetAmbientComponent( ves::open::xml::FloatArray* ambient )
{
    _ambient = ambient;
}
//////////////////////////////////////////////////////
void CADMaterial::SetSpecularComponent( ves::open::xml::FloatArray* specular )
{
    _specular = specular;
}
///////////////////////////////////////////////
void CADMaterial::SetShininess( float shininess )
{
    _shininess = shininess;
}
///////////////////////////////////////////////////
void CADMaterial::SetMaterialName( std::string name )
{
    _materialName = name;
}
///////////////////////////////////
std::string CADMaterial::GetFace()
{
    return _face;
}
///////////////////////////////////////
std::string CADMaterial::GetColorMode()
{
    return _colorMode;
}
/////////////////////////////////
double CADMaterial::GetShininess()
{
    return _shininess;
}
////////////////////////////////////////////
ves::open::xml::FloatArray* CADMaterial::GetDiffuse()
{
    return _kDiffuse;
}
/////////////////////////////////////////////
ves::open::xml::FloatArray* CADMaterial::GetEmissive()
{
    return _kEmissive;
}
////////////////////////////////////////////
ves::open::xml::FloatArray* CADMaterial::GetAmbient()
{
    return _ambient;
}
/////////////////////////////////////////////
ves::open::xml::FloatArray* CADMaterial::GetSpecular()
{
    return _specular;
}
//////////////////////////////////////////
std::string CADMaterial::GetMaterialName()
{
    return _materialName;
}
///////////////////////////////////////
void CADMaterial::_updateMaterialName()
{
    DOMElement* nameElement  = _rootDocument->createElement( xercesString( "materialName" ) );
    _veElement->appendChild( nameElement );

    DOMText* materialName = _rootDocument->createTextNode( xercesString( _materialName ) );
    nameElement->appendChild( materialName );
}
////////////////////////////////////
void CADMaterial::_updateShininess()
{
    DOMElement* shineElement  = _rootDocument->createElement( xercesString( "shininess" ) );
    _veElement->appendChild( shineElement );

    DOMText* shininess = _rootDocument->createTextNode( xercesString( _shininess ) );
    shineElement->appendChild( shininess );
}
//////////////////////////////////////////
void CADMaterial::_updateColorProperties()
{
    _kDiffuse->SetOwnerDocument( _rootDocument );
    _veElement->appendChild( _kDiffuse->GetXMLData( "kDiffuse" ) );

    _kEmissive->SetOwnerDocument( _rootDocument );
    _veElement->appendChild( _kEmissive->GetXMLData( "kEmissive" ) );

    _ambient->SetOwnerDocument( _rootDocument );
    _veElement->appendChild( _ambient->GetXMLData( "kAmbient" ) );

    _specular->SetOwnerDocument( _rootDocument );
    _veElement->appendChild( _specular->GetXMLData( "specular" ) );

    DOMElement* opacityElement  = _rootDocument->createElement( xercesString( "opacity" ) );
    _veElement->appendChild( opacityElement );

    DOMText* opacity = _rootDocument->createTextNode( xercesString( _opacity ) );
    opacityElement->appendChild( opacity );
}
/////////////////////////////////////////////////////
void CADMaterial::_updateVEElement( std::string input )
{
    _updateColorProperties();
    _updateShininess();
    _updateMaterialName();
    _updateMaterialFace();
    _updateColorMode();
}
////////////////////////////////////////
void CADMaterial::_updateMaterialFace()
{
    DOMElement* faceElement = _rootDocument->createElement( xercesString( "face" ) );
    DOMText* faceName = _rootDocument->createTextNode( xercesString( _face ) );
    faceElement->appendChild( faceName );

    _veElement->appendChild( faceElement );
}
////////////////////////////////////
void CADMaterial::_updateColorMode()
{
    DOMElement* cModeElement = _rootDocument->createElement( xercesString( "colorMode" ) );
    DOMText* cMode = _rootDocument->createTextNode( xercesString( _colorMode ) );
    cModeElement->appendChild( cMode );

    _veElement->appendChild( cModeElement );
}
/////////////////////////////////////////////////////////
void CADMaterial::SetObjectFromXMLData( DOMNode* xmlNode )
{
    DOMElement* currentElement = 0;

    if( xmlNode->getNodeType() == DOMNode::ELEMENT_NODE )
    {
        currentElement = dynamic_cast<DOMElement*>( xmlNode );
    }

    if( currentElement )
    {
        if( currentElement->hasChildNodes() )
        {
            // do we need to delete the old one or does xerces handle this???
            _kDiffuse->SetObjectFromXMLData( GetSubElement( currentElement, std::string( "kDiffuse" ), 0 ) );

            _kEmissive->SetObjectFromXMLData( GetSubElement( currentElement, std::string( "kEmissive" ), 0 ) );

            _ambient->SetObjectFromXMLData( GetSubElement( currentElement, std::string( "kAmbient" ), 0 ) );

            _specular->SetObjectFromXMLData( GetSubElement( currentElement, std::string( "specular" ), 0 ) );

            _shininess = ExtractFromSimpleElement< double >( GetSubElement( currentElement, std::string( "shininess" ), 0 ) );
            //this is only needed to check with files that were created before we added opacity but won't be needed by the public
            //for 1.0 release.
            if( GetSubElement( currentElement, std::string( "opacity" ), 0 ) )
            {
                _opacity = ExtractFromSimpleElement< double >( GetSubElement( currentElement, std::string( "opacity" ), 0 ) );
            }
            _materialName = ExtractFromSimpleElement< std::string >( GetSubElement( currentElement, std::string( "materialName" ), 0 ) );
            _face = ExtractFromSimpleElement< std::string >( GetSubElement( currentElement, std::string( "face" ), 0 ) );
            _colorMode = ExtractFromSimpleElement< std::string >( GetSubElement( currentElement, std::string( "colorMode" ), 0 ) );
        }
    }
}
////////////////////////////////////////////
void CADMaterial::SetOpacity( double opacity )
{
    _opacity = opacity;
}
////////////////////////////////
double CADMaterial::GetOpacity()
{
    return _opacity;
}
////////////////////////////////////////////////
CADMaterial::CADMaterial( const CADMaterial& rhs )
        : XMLObject( rhs )
{
    _kDiffuse = new ves::open::xml::FloatArray( *rhs._kDiffuse );
    _kEmissive = new ves::open::xml::FloatArray( *rhs._kEmissive );
    _ambient = new ves::open::xml::FloatArray( *rhs._ambient );
    _specular = new ves::open::xml::FloatArray( *rhs._specular );
    _shininess = rhs._shininess;
    _materialName = rhs._materialName;
    _face = rhs._face;
    _colorMode = rhs._colorMode;
    _opacity = rhs._opacity;
}
////////////////////////////////////////////////////////////
CADMaterial& CADMaterial::operator=( const CADMaterial& rhs )
{
    if( this != &rhs )
    {
        XMLObject::operator =( rhs );
        if( _kDiffuse )
        {
            delete _kDiffuse;
            _kDiffuse = 0;
        }
        if( _kEmissive )
        {
            delete _kEmissive;
            _kEmissive = 0;
        }
        if( _ambient )
        {
            delete _ambient;
            _ambient = 0;
        }
        if( _specular )
        {
            delete _specular;
            _specular = 0;
        }

        _kDiffuse = new ves::open::xml::FloatArray( *rhs._kDiffuse );
        _kEmissive = new ves::open::xml::FloatArray( *rhs._kEmissive );
        _ambient = new ves::open::xml::FloatArray( *rhs._ambient );
        _specular = new ves::open::xml::FloatArray( *rhs._specular );
        _shininess = rhs._shininess;
        _materialName = rhs._materialName;
        _colorMode = rhs._colorMode;
        _face = rhs._face;
        _opacity = rhs._opacity;
    }
    return *this;
}

