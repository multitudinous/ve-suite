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
 * File:          $RCSfile: cfdTextureManager.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifdef VE_PATENTED
#include "VE_TextureBased/cfdTextureManager.h"
#include "VE_Xplorer/readWriteVtkThings.h"

#include <fstream>
//#include <vtkZLibDataCompressor.h>
#include <vtkImageData.h>
//#include <vtkXMLImageDataReader.h>
#include <vtkFloatArray.h>
#include <vtkIntArray.h>
#include <vtkUnsignedIntArray.h>
#include <vtkUnsignedCharArray.h>
//#include <vtkDataArray.h>
#include <vtkPointData.h>
#include <sstream>

using namespace VE_TextureBased;
////////////////////////////////
//Constructor                 //
////////////////////////////////
cfdTextureManager::cfdTextureManager()
{
   _curField = 0;
   _resolution = 0;
   _range[0] = 0;
   _range[1] = 0;
   _prevTime = 0;
   _transientRange[0] = 1000000;
   _transientRange[1] = -1000000;
   _direction = 1;
   _mode = PLAY;
   _useShaders = false;
   
}
/////////////////////////////////////////////////////////////////
cfdTextureManager::cfdTextureManager(const cfdTextureManager& tm)
{
   if(_dataFields.size()){
      _dataFields.clear();
   }
   if(_types.size()){
      _types.clear();
   }
   if(_ranges.size())
   {
      _ranges.clear();
   }
   size_t nFields = tm._dataFields.size();
   
   for ( size_t i = 0; i < nFields; i++ )
   {
      _dataFields.push_back(tm._dataFields[i]);   
      _types.push_back(tm._types.at(i));
      _ranges.push_back(tm._ranges.at(i));
   }
   _mode = tm._mode;
   _curField = tm._curField;
   _prevTime = tm._prevTime;
   _resolution = new int[3];
   _resolution[0] = tm._resolution[0];
   _resolution[1] = tm._resolution[1];
   _resolution[2] = tm._resolution[2];

   _bbox[0] = tm._bbox[0];
   _bbox[1] = tm._bbox[1];
   _bbox[2] = tm._bbox[2];
   _bbox[3] = tm._bbox[3];
   _bbox[4] = tm._bbox[4];
   _bbox[5] = tm._bbox[5];

   _transientRange[0] = tm._transientRange[0];
   _transientRange[1] = tm._transientRange[1];
   _range[0] = tm._range[0];
   _range[1] = tm._range[1];
   _direction = tm._direction;
   _useShaders =  tm._useShaders;
}
/////////////////////////////////
cfdTextureManager::~cfdTextureManager()
{
   if(_dataFields.size() > 0 )
   {
      size_t nFields = _dataFields.size();
      for( size_t i = 0; i < nFields; i++)
      {
         if(_dataFields.at(i))
         {
            delete [] _dataFields.at(i);
            _dataFields.at(i) = 0;
         }
      }
      _dataFields.clear();
   }

   if(_types.size()){
      _types.clear();
   }

   if(_resolution){
      delete [] _resolution;
      _resolution = 0;
   }

   if(_ranges.size())
   {
      _ranges.clear();
   }
}
//////////////////////////////////////////////////////
void cfdTextureManager::SetUseShaders(bool useShaders)
{
   _useShaders = useShaders;
}

///////////////////////////////////////////////////////////
void cfdTextureManager::SetCurrentFrame(unsigned int frame)
{
   _curField = frame;
}
/////////////////////////////////////////////////
unsigned int cfdTextureManager::GetCurrentFrame()
{
   return _curField;
}
//////////////////////////////////////////////
unsigned int cfdTextureManager::getNextFrame()
{
   size_t numFields = _dataFields.size();
   //now update the current field
   if(_direction == 1){
      //haven't reached the end yet
      //so return the next field 
      if(_curField < numFields-1){
         _curField += _direction;
      }else if(_curField == numFields-1){
         //we're at the end so we need to
         //return the beginning
         _curField = 0;
      }
   }else{
      if(_curField > 0){
         _curField += _direction;
      }else if(_curField == 0){
         //we're at the end so we need to
         //return the beginning
         _curField = static_cast< int >( numFields-1 );
      }
   }
   return _curField;
}
/////////////////////////////////////////////////////////////////////
//add a vector field from a file                                   //
/////////////////////////////////////////////////////////////////////
void cfdTextureManager::addFieldTextureFromFile(std::string textureFile)
{
   double tempMag[6] = {0,0,0,0,0,0};
   std::ifstream fin(textureFile.c_str());

   if ( fin.is_open() )
   {
      fin.close();
      
      vtkImageData* flowImage = dynamic_cast< vtkImageData* >( VE_Util::readVtkThing( textureFile, 0 ) );
      if ( flowImage->GetPointData()->GetNumberOfArrays() > 1 )
      {
         std::cerr << " ERROR : cfdTextureManager::addFieldTextureFromFile : There are too many scalars in this texture " << std::endl;
      }
      else if(flowImage->GetPointData()->GetNumberOfArrays() == 0)
      {
         std::cerr << " ERROR : cfdTextureManager::addFieldTextureFromFile : No scalars in this texture " << std::endl;
      }

      std::cout<<"2: "<<flowImage->GetPointData()->GetNumberOfArrays()<<std::endl;
      std::cout<<"3: "<<flowImage->GetPointData()->GetArray(0)->GetDataType()<<std::endl;
      vtkDataArray* flowData = dynamic_cast< vtkDataArray* >( flowImage->GetPointData()->GetArray( 0 ) );
      if(!flowData)
      {
         std::cout<<"no flowdata!!"<<std::endl;
      }
      dataName = flowData->GetName();

     std::cout<<"3"<<std::endl;
      DataType curType;
      //read the file type
      if ( flowData->GetNumberOfComponents() == 1 )
      {
         //scalar
         _types.push_back(SCALAR);
         curType = SCALAR;
      }
      else if ( flowData->GetNumberOfComponents() == 4 )
      {
         //vector file
         _types.push_back(VECTOR);
         curType = VECTOR;
      }
      else
      {
         std::cerr << " ERROR : cfdTextureManager::addFieldTextureFromFile :" << 
                        " There are too many components in this texture " << std::endl;
      }

     std::cout<<"4"<<std::endl;
      //data range(magnitude) for scalars
      //ignore this value for vectors
      ScalarRange newRange;
      double* range = flowData->GetRange();
      newRange.range[0] = range[ 0 ];
      newRange.range[1] = range[ 1 ];
     
      _ranges.push_back(newRange);
      _range[0] = newRange.range[0];
      _range[1] = newRange.range[1];

     std::cout<<"5"<<std::endl;
      _transientRange[0] = (_range[0] < _transientRange[0])?_range[0]:_transientRange[0];
      _transientRange[1] = (_range[1] > _transientRange[1])?_range[1]:_transientRange[1];
      //bounding box
      double* bbox = flowImage->GetBounds(); 
     std::cout<<"6"<<std::endl;
      for ( unsigned int i = 0; i < 6; ++i )
      {
         _bbox[ i ] = bbox[ i ];
      }

      if ( !_resolution )
      {
         _resolution = new int[3];
      }

     std::cout<<"7"<<std::endl;
      //the dimensions  
      flowImage->GetDimensions( &*_resolution );

      if ( !_resolution[2] ) 
      {
         _resolution[2] = 1; 
      }
   
      double R,G,B,A;
      float alpha = 0;
      
     std::cout<<"8"<<std::endl;
      int nPixels = _resolution[0]*_resolution[1]*_resolution[2];
      unsigned char* pixels = 0;
      float invSRange = 1.0/(_range[1]-_range[0]);

      if(curType == VECTOR)
      {
         pixels = new unsigned char[nPixels*4];
         for(int p = 0; p < nPixels; p++)
         {
            double* rawData = flowData->GetTuple4( p );
            pixels[p*4   ]  = static_cast< unsigned char >( rawData[ 0 ] );
            pixels[p*4 + 1] = static_cast< unsigned char >( rawData[ 1 ] );
            pixels[p*4 + 2] = static_cast< unsigned char >( rawData[ 2 ] );
            A = static_cast< unsigned char>( rawData[ 3 ] );
            alpha = (A -_range[0])*invSRange;
            pixels[p*4 + 3] = (unsigned char)(alpha*255);
         }
      }
      else if(curType == SCALAR)
      {
         //the scalar data         
         float scalarValue = 0;
         if (_useShaders)
         {
            pixels = new unsigned char[nPixels];
         }
         else
         {
            pixels = new unsigned char[nPixels*4];
         }

         for(int p = 0; p < nPixels; p++)
         {
            scalarValue = flowData->GetTuple1( p );
            
            alpha = (scalarValue-_range[0])*invSRange;
            if(_useShaders)
            {
               pixels[p]  = (unsigned char)(255.0*alpha);
               //std::cout<<255.0*alpha<<std::endl;
            }
            else
            {
               //set the scalar pixel data
               if(alpha <= .5){
                  R = 0;
                  G = (2.0*alpha)*255,      
                  B = (1.0-2.0*alpha)*255;
                  A = 255*alpha*.5;
               }else{
                  R = (2.0*alpha-1.0)*255;
                  G = (2.0 - 2.0*alpha)*255;       
                  B = 0.;
                  A = 255*alpha*.5;
               }
               pixels[p*4   ]  = (unsigned char)R;
               pixels[p*4 + 1] = (unsigned char)G;
               pixels[p*4 + 2] = (unsigned char)B;
               pixels[p*4 + 3] = (unsigned char)A;      
            }
         }
      }
     std::cout<<"9"<<std::endl;
      //add the field
      _dataFields.push_back(pixels);
      flowImage->Delete();
   }
   else
   {
      std::cout<<"Invalid file: "<<textureFile<<std::endl;
      std::cout<<"cfdTextureManager couldn't load new vector field!!"<<std::endl;
      return;
   }
}
//////////////////////////////////////////////////
//get the next vector field                     //
//////////////////////////////////////////////////
unsigned char* cfdTextureManager::getNextField(/*int plusNeg*/)
{
   //int dir = 1;
   //get the appropriate direction
   //positive
   //if(!plusNeg)dir = -1;

   size_t numFields = _dataFields.size();
   //now update the current field
   if(_direction == 1){
      //haven't reached the end yet
      //so return the next field 
      if(_curField < numFields-1){
         _curField += _direction;
      }else if(_curField == numFields-1){
         //we're at the end so we need to
         //return the beginning
         _curField = 0;
      }
   }else{
      if(_curField > 0){
         _curField += _direction;
      }else if(_curField == 0){
         //we're at the end so we need to
         //return the beginning
         _curField = static_cast< int >( numFields-1 );
      }
   }

   //return _vectorFields.at(_curField);
   unsigned char* field = 0; 
   field = _dataFields.at(_curField);
   if(field){
      return field;//_dataFields[_curField];
   }
   else return 0;
}
//////////////////////////////////////////////////////////////
int cfdTextureManager::timeToUpdate(double curTime,double delay)
{
   size_t numFields = _dataFields.size();

   if ( (numFields > 1) && _mode == PLAY  )
   {
      if ( curTime - _prevTime >= delay )
      {
         _prevTime = curTime;
         return 1;
      }
   }
   return 0;
}
/////////////////////////////////////////////////////////
void cfdTextureManager::setDirection(int forwardBackward)
{
   _direction = forwardBackward;
}
/////////////////////////////////////////////////////////
std::string cfdTextureManager::GetDataName( void )
{
   return dataName;
}

////////////////////////////////////////////////////////////////////////////
//equal operator                                                          //
////////////////////////////////////////////////////////////////////////////
cfdTextureManager& cfdTextureManager::operator=(const cfdTextureManager& tm)
{
   if(this != &tm)
   {
      if ( _dataFields.size() )
      {
         _dataFields.clear();
      }

      if ( _types.size() )
      {
         _types.clear();
      }

      if(_ranges.size())
      {
         _ranges.clear();
      }
      size_t nFields = tm._dataFields.size();
   
      for(size_t i = 0; i < nFields; i++){
         _dataFields.push_back(tm._dataFields.at(i));
         _types.push_back(tm._types.at(i));
         _ranges.push_back(tm._ranges.at(i));
      }
      _curField = tm._curField;

      if(!_resolution)_resolution = new int[3];
      _resolution[0] = tm._resolution[0];
      _resolution[1] = tm._resolution[1];
      _resolution[2] = tm._resolution[2];
      _bbox[0] = tm._bbox[0];
      _bbox[1] = tm._bbox[1];
      _bbox[2] = tm._bbox[2];
      _bbox[3] = tm._bbox[3];
      _bbox[4] = tm._bbox[4];
      _bbox[5] = tm._bbox[5];

      _prevTime = tm._prevTime;
      _range[0] = tm._range[0];
      _range[1] = tm._range[1];
      _direction = tm._direction;
      _mode = tm._mode;
   }
   return *this;

}
#endif // VE_PATENTED
