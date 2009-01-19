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
CADMaterial::CADMaterial( const std::string& name )
        : ves::open::xml::XMLObject()
{
    std::vector< double > temp;
    temp.assign( 4, 1.0f );

    _kDiffuse = ves::open::xml::FloatArrayPtr( new ves::open::xml::FloatArray() );
    _kDiffuse->SetArray( temp );

    _kEmissive = ves::open::xml::FloatArrayPtr( new ves::open::xml::FloatArray() );
    _kEmissive->AddElementToArray( 0.0 );
    _kEmissive->AddElementToArray( 0.0 );
    _kEmissive->AddElementToArray( 0.0 );
    _kEmissive->AddElementToArray( 1.0 );

    _ambient = ves::open::xml::FloatArrayPtr( new ves::open::xml::FloatArray() );
    _ambient->SetArray( temp );

    _specular = ves::open::xml::FloatArrayPtr( new ves::open::xml::FloatArray() );
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
    ;
}
////////////////////////////////////////////////////
void CADMaterial::SetFace( const std::string& faceToApplyTo )
{
    _face = faceToApplyTo;
}
////////////////////////////////////////////////////
void CADMaterial::SetColorMode( const std::string& colorMode )
{
    _colorMode = colorMode;
}
//////////////////////////////////////////////////////////////////
void CADMaterial::SetDiffuseComponent( ves::open::xml::FloatArrayPtr diffuse )
{
    _kDiffuse = diffuse;
}
////////////////////////////////////////////////////////////////////////
void CADMaterial::SetComponent( const std::string& componentName, std::vector<double> values )
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
void CADMaterial::SetComponent( const std::string& componentName, double* values )
{
    std::vector<double> temp;
    temp.push_back( values[0] );
    temp.push_back( values[1] );
    temp.push_back( values[2] );
    temp.push_back( values[3] );
    SetComponent( componentName, temp );
}
///////////////////////////////////////////////////////
void CADMaterial::SetEmissiveComponent( ves::open::xml::FloatArrayPtr emissive )
{
    _kEmissive = emissive;
}
/////////////////////////////////////////////////////
void CADMaterial::SetAmbientComponent( ves::open::xml::FloatArrayPtr ambient )
{
    _ambient = ambient;
}
//////////////////////////////////////////////////////
void CADMaterial::SetSpecularComponent( ves::open::xml::FloatArrayPtr specular )
{
    _specular = specular;
}
///////////////////////////////////////////////
void CADMaterial::SetShininess( float shininess )
{
    _shininess = shininess;
}
///////////////////////////////////////////////////
void CADMaterial::SetMaterialName( const std::string& name )
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
ves::open::xml::FloatArrayPtr CADMaterial::GetDiffuse()
{
    return _kDiffuse;
}
/////////////////////////////////////////////
ves::open::xml::FloatArrayPtr CADMaterial::GetEmissive()
{
    return _kEmissive;
}
////////////////////////////////////////////
ves::open::xml::FloatArrayPtr CADMaterial::GetAmbient()
{
    return _ambient;
}
/////////////////////////////////////////////
ves::open::xml::FloatArrayPtr CADMaterial::GetSpecular()
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
    DOMElement* nameElement  = mRootDocument->createElement(
                               Convert( "materialName" ).toXMLString() );

    mVeElement->appendChild( nameElement );

    DOMText* materialName = mRootDocument->createTextNode(
                            Convert( _materialName ).toXMLString() );
    nameElement->appendChild( materialName );
}
////////////////////////////////////
void CADMaterial::_updateShininess()
{
    DOMElement* shineElement  = mRootDocument->createElement(
                                Convert( "shininess" ).toXMLString() );

    mVeElement->appendChild( shineElement );

    DOMText* shininess = mRootDocument->createTextNode(
                         Convert( _shininess ).toXMLString() );

    shineElement->appendChild( shininess );
}
//////////////////////////////////////////
void CADMaterial::_updateColorProperties()
{
    _kDiffuse->SetOwnerDocument( mRootDocument );
    mVeElement->appendChild( _kDiffuse->GetXMLData( "kDiffuse" ) );

    _kEmissive->SetOwnerDocument( mRootDocument );
    mVeElement->appendChild( _kEmissive->GetXMLData( "kEmissive" ) );

    _ambient->SetOwnerDocument( mRootDocument );
    mVeElement->appendChild( _ambient->GetXMLData( "kAmbient" ) );

    _specular->SetOwnerDocument( mRootDocument );
    mVeElement->appendChild( _specular->GetXMLData( "specular" ) );

    DOMElement* opacityElement  = mRootDocument->createElement(
                                  Convert( "opacity" ).toXMLString() );

    mVeElement->appendChild( opacityElement );

    DOMText* opacity = mRootDocument->createTextNode(
                       Convert( _opacity ).toXMLString() );

    opacityElement->appendChild( opacity );
}
/////////////////////////////////////////////////////
void CADMaterial::_updateVEElement( const std::string& input )
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
    DOMElement* faceElement = mRootDocument->createElement(
                              Convert( "face" ).toXMLString() );

    DOMText* faceName = mRootDocument->createTextNode(
                        Convert( _face ).toXMLString() );

    faceElement->appendChild( faceName );

    mVeElement->appendChild( faceElement );
}
////////////////////////////////////
void CADMaterial::_updateColorMode()
{
    DOMElement* cModeElement = mRootDocument->createElement(
                               Convert( "colorMode" ).toXMLString() );

    DOMText* cMode = mRootDocument->createTextNode(
                     Convert( _colorMode ).toXMLString() );

    cModeElement->appendChild( cMode );

    mVeElement->appendChild( cModeElement );
}
/////////////////////////////////////////////////////////
void CADMaterial::SetObjectFromXMLData( DOMNode* xmlNode )
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
    
    if( !currentElement->hasChildNodes() )
    {
        return;
    }

    // do we need to delete the old one or does xerces handle this???
    _kDiffuse->SetObjectFromXMLData( GetSubElement( currentElement, std::string( "kDiffuse" ), 0 ) );

    _kEmissive->SetObjectFromXMLData( GetSubElement( currentElement, std::string( "kEmissive" ), 0 ) );

    _ambient->SetObjectFromXMLData( GetSubElement( currentElement, std::string( "kAmbient" ), 0 ) );

    _specular->SetObjectFromXMLData( GetSubElement( currentElement, std::string( "specular" ), 0 ) );

    GetDataFromElement(
       GetSubElement( currentElement, std::string( "shininess" ), 0 ),
                       _shininess );
    //this is only needed to check with files that were created before we added opacity but won't be needed by the public
    //for 1.0 release.
    if( GetSubElement( currentElement, std::string( "opacity" ), 0 ) )
    {
       GetDataFromElement(
          GetSubElement( currentElement, std::string( "opacity" ), 0 ),
                          _opacity);
    }

    GetDataFromElement(
       GetSubElement( currentElement, std::string( "materialName" ), 0 ),
                       _materialName);

    GetDataFromElement(
       GetSubElement( currentElement, std::string( "face" ), 0 ),
                       _face);

    GetDataFromElement(
       GetSubElement( currentElement, std::string( "colorMode" ), 0 ),
                       _colorMode);
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
    _kDiffuse = ves::open::xml::FloatArrayPtr( new ves::open::xml::FloatArray(  *rhs._kDiffuse ) );
    _kEmissive = ves::open::xml::FloatArrayPtr( new ves::open::xml::FloatArray(  *rhs._kEmissive ) );
    _ambient = ves::open::xml::FloatArrayPtr( new ves::open::xml::FloatArray(  *rhs._ambient ) );
    _specular = ves::open::xml::FloatArrayPtr( new ves::open::xml::FloatArray(  *rhs._specular ) );
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

        _kDiffuse = ves::open::xml::FloatArrayPtr( new ves::open::xml::FloatArray(  *rhs._kDiffuse ) );
        _kEmissive = ves::open::xml::FloatArrayPtr( new ves::open::xml::FloatArray(  *rhs._kEmissive ) );
        _ambient = ves::open::xml::FloatArrayPtr( new ves::open::xml::FloatArray(  *rhs._ambient ) );
        _specular = ves::open::xml::FloatArrayPtr( new ves::open::xml::FloatArray(  *rhs._specular ) );
        _shininess = rhs._shininess;
        _materialName = rhs._materialName;
        _colorMode = rhs._colorMode;
        _face = rhs._face;
        _opacity = rhs._opacity;
    }
    return *this;
}

