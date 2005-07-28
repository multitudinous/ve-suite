/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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
 * File:          $RCSfile: filename,v $
 * Date modified: $Date: date $
 * Version:       $Rev: 999999 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifdef VE_PATENTED
#include "VE_TextureBased/cfdTextureManager.h"
#include <fstream>
#include <vtkZLibDataCompressor.h>
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
////////////////////////////////////////////////////////
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
   int nFields = tm._dataFields.size();
   
   for(int i = 0; i < nFields; i++){
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
   if(_dataFields.size()){
      unsigned int nFields = _dataFields.size();
      for(unsigned int i = 0; i < nFields; i++)
      {
         if(_dataFields.at(i))
            delete [] _dataFields.at(i);
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
   int numFields = _dataFields.size();
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
         _curField = numFields-1;
      }
   }
   return _curField;
}
/////////////////////////////////////////////////////////////////////
//add a vector field from a file                                   //
/////////////////////////////////////////////////////////////////////
void cfdTextureManager::addFieldTextureFromFile(char* textureFile)
{
   double tempMag[6] = {0,0,0,0,0,0};
   std::ifstream fin(textureFile);


   //std::ifstream fileIn(textureFile, std::ios::in);
   //if ( fileIn.is_open() )
   if ( fin.is_open() )
   {
/*      unsigned long l,m, rlen, grid_data_length2;
      fileIn >> rlen >> grid_data_length2;
      l = fileIn.tellg();
      fileIn.seekg (0, std::ios::end);
      m = fileIn.tellg();
      //fileIn.close();

      //fileIn.open(textureFile, std::ios::in);
      //fileIn >> rlen >> grid_data_length2;

      char* tempCharBuf = new char[ (m-(l+1)) ];
      // get pointer to associated buffer object
      std::filebuf *pbuf;
      pbuf=fileIn.rdbuf();
      pbuf->pubseekpos(l+1,ios::in);

      // get file data  
      pbuf->sgetn(tempCharBuf,rlen);

      fileIn.close();
   
      // uncompress the data
      const unsigned char* compressed_data_Array = reinterpret_cast< const unsigned char* >( tempCharBuf );
      unsigned char* ucbuffer = new unsigned char[ grid_data_length2 ];
      vtkZLibDataCompressor* compressor = vtkZLibDataCompressor::New();
      rlen = compressor->Uncompress(compressed_data_Array, rlen, ucbuffer, grid_data_length2);

      delete [] tempCharBuf;

      // Create buffer for the code below to opperate one
      std::string buffer( reinterpret_cast< const char* >( ucbuffer ) );
      delete [] ucbuffer;

      std::istringstream fin( buffer );

      compressor->Delete();
      compressor = 0;
*/
      char type[16];
      DataType curType;
      //read the file type
      fin>>type;
      if(!strcmp(type,"s")){
         //scalar
         _types.push_back(SCALAR);
         curType = SCALAR;
      }else if(!strcmp(type,"v")){
         //vector file
         _types.push_back(VECTOR);
         curType = VECTOR;
      }

      //data range(magnitude) for scalars
      //ignore this value for vectors
      ScalarRange newRange;
      fin>>newRange.range[0];
      fin>>newRange.range[1];
     
      _ranges.push_back(newRange);
      _range[0] = newRange.range[0];
      _range[1] = newRange.range[1];

      _transientRange[0] = (_range[0] < _transientRange[0])?_range[0]:_transientRange[0];
      _transientRange[1] = (_range[1] > _transientRange[1])?_range[1]:_transientRange[1];
      //bounding box
      fin>>_bbox[0]>>_bbox[1]>>_bbox[2]>>_bbox[3]>>_bbox[4]>>_bbox[5];
      
      if ( !_resolution )
      {
         _resolution = new int[3];
      }

      //the dimensions  
      fin>>_resolution[0]>>_resolution[1]>>_resolution[2];

      if ( !_resolution[2] ) 
      {
         _resolution[2] = 1; 
      }
   
      double R,G,B,A;
      float alpha = 0;
      
      int nPixels = _resolution[0]*_resolution[1]*_resolution[2];
      unsigned char* pixels = 0;
      float invSRange = 1.0/(_range[1]-_range[0]);
      if(curType == VECTOR)
      {
         pixels = new unsigned char[nPixels*4];
         for(int p = 0; p < nPixels; p++){
            fin >> R;// >> G >> B >> A;
            pixels[p*4   ] = (unsigned char)R;
            fin>>G;
            pixels[p*4 + 1] = (unsigned char)G;
            fin>>B;
            pixels[p*4 + 2] = (unsigned char)B;
            fin>>A;
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
            fin>>scalarValue;
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
      //add the field
      _dataFields.push_back(pixels);
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

   int numFields = _dataFields.size();
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
         _curField = numFields-1;
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
   int numFields = _dataFields.size();

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
///////////////////////////////////////////////////////////////////
//equal operator                                                 //
///////////////////////////////////////////////////////////////////
cfdTextureManager& cfdTextureManager::operator=(const cfdTextureManager& tm)
{
   if(this != &tm){
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
      int nFields = tm._dataFields.size();
   
      for(int i = 0; i < nFields; i++){
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
