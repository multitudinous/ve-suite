#include "VE_Open/VE_XML/VETransform.h"
#include "VE_Open/VE_XML/VEFloatArray.h"

using namespace VE_XML;
//////////////////////////
//Constructor           //
//////////////////////////
VETransform::VETransform(DOMDocument* rootDoc)
:VEXMLObject(rootDoc)
{
   /*
   _translation = 0;
   _scale = 0;
   _rotation = 0;
   */

   std::vector< double > temp;
   temp.assign( 3, 0.0f );

   rotationArray = new VEFloatArray( rootDoc );
   rotationArray->SetArray( temp );

   scaleArray = new VEFloatArray( rootDoc );
   scaleArray->SetArray( temp );

   translationArray = new VEFloatArray( rootDoc );
   translationArray->SetArray( temp );
}
///////////////////////////
//Destructor             //
///////////////////////////
VETransform::~VETransform()
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
VETransform::VETransform( const VETransform& input )
:VEXMLObject(input)
{
   rotationArray = new VEFloatArray( *input.rotationArray );
   scaleArray = new VEFloatArray( *input.scaleArray );
   translationArray = new VEFloatArray( *input.translationArray );
}
/////////////////////////////////////////////////////
VETransform& VETransform::operator=( const VETransform& input)
{
   if ( this != &input )
   {
      //biv-- make sure to call the parent =
      VEXMLObject::operator =(input);
      *rotationArray = *input.rotationArray;
      *scaleArray = *input.scaleArray;
      *translationArray = *input.translationArray;
   }
   return *this;
}
/*
//////////////////////////////////////////////
void VETransform::SetTranslation(float* trans)
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
void VETransform::SetScale(float* scale)
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
void VETransform::SetRotation(float* rotation)
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
float* VETransform::GetTranslation()
{
   return _translation;
}
//////////////////////////////
float* VETransform::GetScale()
{
   return _scale;
}
/////////////////////////////////
float* VETransform::GetRotation()
{
   return _rotation;
}
*/
////////////////////////////////////
void VETransform::_updateVEElement( std::string input )
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
void VETransform::SetObjectFromXMLData( DOMNode* xmlInput )
{
   if ( xmlInput->hasChildNodes() )
   {
      // do we need to delete the old one or does xerces handle this???
      _nChildren = 3;

      translationArray->SetObjectFromXMLData( xmlInput );
      scaleArray->SetObjectFromXMLData( xmlInput );
      rotationArray->SetObjectFromXMLData( xmlInput );
   }
   else
   {
      std::cerr << " ERROR : VETransform::SetObjectFromXMLData :" << 
                  " This node has no children which means there is probably a problem." << std::endl;
   }
}
//////////////////////////////////////////////////////////////
VEFloatArray* VETransform::GetTranslationArray( void )
{
   return translationArray;
}
//////////////////////////////////////////////////////////////
VEFloatArray* VETransform::GetScaleArray( void )
{
   return scaleArray;
}
//////////////////////////////////////////////////////////////
VEFloatArray* VETransform::GetRotationArray( void )
{
   return rotationArray;
}
//////////////////////////////////////////////////////////////
void VETransform::SetTranslationArray( VEFloatArray* input )
{
   *translationArray = *input;
}
//////////////////////////////////////////////////////////////
void VETransform::SetScaleArray( VEFloatArray* input )
{
   *scaleArray = *input;
}
//////////////////////////////////////////////////////////////
void VETransform::SetRotationArray( VEFloatArray* input )
{
   *rotationArray = *input;
}
    
    

