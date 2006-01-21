/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 * File:          $RCSfile: Transform.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VE_Open/XML/Transform.h"
#include "VE_Open/XML/FloatArray.h"

using namespace VE_XML;
//////////////////////////
//Constructor           //
//////////////////////////
Transform::Transform(DOMDocument* rootDoc)
:XMLObject(rootDoc)
{
   /*
   _translation = 0;
   _scale = 0;
   _rotation = 0;
   */

   std::vector< double > temp;
   temp.assign( 3, 0.0f );

   rotationArray = new FloatArray( rootDoc );
   rotationArray->SetArray( temp );

   scaleArray = new FloatArray( rootDoc );
   scaleArray->SetArray( temp );

   translationArray = new FloatArray( rootDoc );
   translationArray->SetArray( temp );
}
///////////////////////////
//Destructor             //
///////////////////////////
Transform::~Transform()
{
   /*
   if(_translation)
   {
      delete [] _translation;
      _scale = 0;
   }
   if(_scale)
   {
      delete [] _scale;
      _scale = 0;
   }
   if(_rotation)
   {
      delete [] _rotation;
      _rotation = 0;
   }
   */

   delete rotationArray;
   rotationArray = 0;

   delete scaleArray;
   scaleArray = 0;

   delete translationArray;
   translationArray = 0;
}
///////////////////////////////////////////
Transform::Transform( const Transform& input )
:XMLObject(input)
{
   rotationArray = new FloatArray( *input.rotationArray );
   scaleArray = new FloatArray( *input.scaleArray );
   translationArray = new FloatArray( *input.translationArray );
}
/////////////////////////////////////////////////////
Transform& Transform::operator=( const Transform& input)
{
   if ( this != &input )
   {
      //biv-- make sure to call the parent =
      XMLObject::operator =(input);
      *rotationArray = *input.rotationArray;
      *scaleArray = *input.scaleArray;
      *translationArray = *input.translationArray;
   }
   return *this;
}
/*
//////////////////////////////////////////////
void Transform::SetTranslation(float* trans)
{
   if(trans)
   {
      if(!_translation)
      {
         _translation = new float[3];
      }
      _translation[0] = trans[0];
      _translation[1] = trans[1];
      _translation[2] = trans[2];
   }
}
////////////////////////////////////////
void Transform::SetScale(float* scale)
{
   if(scale)
   {
      if(!_scale)
      {
         _scale = new float[3];
      }
      _scale[0] = scale[0];
      _scale[1] = scale[1];
      _scale[2] = scale[2];
   }
}
//////////////////////////////////////////////
void Transform::SetRotation(float* rotation)
{
   if(rotation)
   {
      if(!_rotation)
      {
         _rotation = new float[3];
      }
      _rotation[0] = rotation[0];
      _rotation[1] = rotation[1];
      _rotation[2] = rotation[2];
   }
}
////////////////////////////////////
float* Transform::GetTranslation()
{
   return _translation;
}
//////////////////////////////
float* Transform::GetScale()
{
   return _scale;
}
/////////////////////////////////
float* Transform::GetRotation()
{
   return _rotation;
}
*/
////////////////////////////////////
void Transform::_updateVEElement( std::string input )
{
   if(!_veElement)
   {
      _veElement = _rootDocument->createElement( xercesString( input ) );
   }
   //Be sure to set the number of children (_nChildren) either here or in the updating subElements code
   //we know this to be 3 float arrays
   _nChildren = 3;
   // name comes from verg.xsd
   DOMElement* translationTag  = translationArray->GetXMLData( "translation" );
   _veElement->appendChild( translationTag );      

   DOMElement* scaleTag  = scaleArray->GetXMLData( "scale" );
   _veElement->appendChild( scaleTag );      

   DOMElement* rotationTag  = rotationArray->GetXMLData( "rotation" );
   _veElement->appendChild( rotationTag );      
}
//////////////////////////////////////////////////////////////
void Transform::SetObjectFromXMLData( DOMNode* xmlInput )
{
   DOMElement* currentElement = 0;

   if ( xmlInput->hasChildNodes() )
   {
      if(xmlInput->getNodeType() == DOMNode::ELEMENT_NODE)
      {
         currentElement = dynamic_cast<DOMElement*>(xmlInput);
      }
   
      if(currentElement)
      {
   
         // do we need to delete the old one or does xerces handle this???
         _nChildren = 3;

         translationArray->SetObjectFromXMLData( currentElement->getElementsByTagName(xercesString("translation"))->item(0) );
         scaleArray->SetObjectFromXMLData( currentElement->getElementsByTagName(xercesString("scale"))->item(0) );
         rotationArray->SetObjectFromXMLData( currentElement->getElementsByTagName(xercesString("rotation"))->item(0) );
      }
   }
   else
   {
      std::cerr << " ERROR : Transform::SetObjectFromXMLData :" << 
                  " This node has no children which means there is probably a problem." << std::endl;
   }
}
//////////////////////////////////////////////////////////////
FloatArray* Transform::GetTranslationArray( void )
{
   return translationArray;
}
//////////////////////////////////////////////////////////////
FloatArray* Transform::GetScaleArray( void )
{
   return scaleArray;
}
//////////////////////////////////////////////////////////////
FloatArray* Transform::GetRotationArray( void )
{
   return rotationArray;
}
//////////////////////////////////////////////////////////////
void Transform::SetTranslationArray( FloatArray* input )
{
   *translationArray = *input;
}
//////////////////////////////////////////////////////////////
void Transform::SetScaleArray( FloatArray* input )
{
   *scaleArray = *input;
}
//////////////////////////////////////////////////////////////
void Transform::SetRotationArray( FloatArray* input )
{
   *rotationArray = *input;
}
    
    

