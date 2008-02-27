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
#include <ves/open/xml/FloatArray.h>
XERCES_CPP_NAMESPACE_USE
using namespace ves::open::xml;
//////////////////////////
//Constructor           //
//////////////////////////
Transform::Transform()
        : XMLObject()
{
    std::vector< double > temp;
    temp.assign( 3, 0.0f );

    std::vector< double > temp1;
    temp1.assign( 3, 1.0f );

    mRotationArray= FloatArrayPtr( new FloatArray() );
    mRotationArray->SetArray( temp );

    mScaleArray= FloatArrayPtr( new FloatArray() );
    mScaleArray->SetArray( temp1 );

    mTranslationArray= FloatArrayPtr( new FloatArray() );
    mTranslationArray->SetArray( temp );
    SetObjectType( "Transform" );
}
///////////////////////////
//Destructor             //
///////////////////////////
Transform::~Transform()
{
    ;
}
///////////////////////////////////////////
Transform::Transform( const Transform& input )
        : XMLObject( input )
{
    mRotationArray = FloatArrayPtr( new FloatArray(  *input.mRotationArray ) );
    mScaleArray = FloatArrayPtr( new FloatArray(  *input.mScaleArray ) );
    mTranslationArray = FloatArrayPtr( new FloatArray(  *input.mTranslationArray ) );
}
/////////////////////////////////////////////////////
Transform& Transform::operator=( const Transform& input )
{
    if( this != &input )
    {
        //biv-- make sure to call the parent =
        XMLObject::operator =( input );
        *mRotationArray = *input.mRotationArray;
        *mScaleArray = *input.mScaleArray;
        *mTranslationArray = *input.mTranslationArray;
    }
    return *this;
}
//////////////////////////////////////////////
void Transform::SetTranslation( float* trans )
{
    if( trans )
    {
        mTranslationArray->Clear();
        mTranslationArray->AddElementToArray( trans[0] );
        mTranslationArray->AddElementToArray( trans[1] );
        mTranslationArray->AddElementToArray( trans[2] );
    }
}
////////////////////////////////////////
void Transform::SetScale( float* scale )
{
    if( scale )
    {
        mScaleArray->Clear();
        mScaleArray->AddElementToArray( scale[0] );
        mScaleArray->AddElementToArray( scale[1] );
        mScaleArray->AddElementToArray( scale[2] );
    }
}
//////////////////////////////////////////////
void Transform::SetRotation( float* rotation )
{
    if( rotation )
    {
        mRotationArray->Clear();
        mRotationArray->AddElementToArray( rotation[0] );
        mRotationArray->AddElementToArray( rotation[1] );
        mRotationArray->AddElementToArray( rotation[2] );
    }
}
/////////////////////////////////////////////////////
void Transform::_updateVEElement( const std::string& input )
{
    // name comes from verg.xsd
    mTranslationArray->SetOwnerDocument( mRootDocument );
    DOMElement* translationTag  = mTranslationArray->GetXMLData( "translation" );
    mVeElement->appendChild( translationTag );

    mScaleArray->SetOwnerDocument( mRootDocument );
    DOMElement* scaleTag  = mScaleArray->GetXMLData( "scale" );
    mVeElement->appendChild( scaleTag );

    mRotationArray->SetOwnerDocument( mRootDocument );
    DOMElement* rotationTag  = mRotationArray->GetXMLData( "rotation" );
    mVeElement->appendChild( rotationTag );
}
//////////////////////////////////////////////////////////////
void Transform::SetObjectFromXMLData( DOMNode* xmlInput )
{
    DOMElement* currentElement = 0;

    if( xmlInput->hasChildNodes() )
    {
        if( xmlInput->getNodeType() == DOMNode::ELEMENT_NODE )
        {
            currentElement = dynamic_cast<DOMElement*>( xmlInput );
        }

        if( currentElement )
        {

            // do we need to delete the old one or does xerces handle this???
            mTranslationArray->SetObjectFromXMLData( 
                currentElement->getElementsByTagName( 
                ves::open::xml::Convert( "translation" ).toXMLString() )->
                item( 0 ) );

            mScaleArray->SetObjectFromXMLData( 
                currentElement->getElementsByTagName( 
                ves::open::xml::Convert( "scale" ).toXMLString() )->
                item( 0 ) );

            mRotationArray->SetObjectFromXMLData( 
                currentElement->getElementsByTagName( 
                ves::open::xml::Convert( "rotation" ).toXMLString() )->
                item( 0 ) );
        }
    }
    else
    {
        std::cerr << " ERROR : Transform::SetObjectFromXMLData :" <<
        " This node has no children which means there is probably a problem." << std::endl;
    }
}
//////////////////////////////////////////////////////////////
FloatArrayPtr Transform::GetTranslationArray( void )
{
    return mTranslationArray;
}
//////////////////////////////////////////////////////////////
FloatArrayPtr Transform::GetScaleArray( void )
{
    return mScaleArray;
}
//////////////////////////////////////////////////////////////
FloatArrayPtr Transform::GetRotationArray( void )
{
    return mRotationArray;
}
//////////////////////////////////////////////////////////////
void Transform::SetTranslationArray( FloatArrayPtr input )
{
    mTranslationArray = input;
}
//////////////////////////////////////////////////////////////
void Transform::SetScaleArray( FloatArrayPtr input )
{
    mScaleArray = input;
}
//////////////////////////////////////////////////////////////
void Transform::SetRotationArray( FloatArrayPtr input )
{
    mRotationArray = input;
}
//////////////////////////////////////////////////////////////
