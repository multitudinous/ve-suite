#include "cfdTextureManager.h"
#include <fstream>
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
   int nFields = tm._dataFields.size();
   
   for(int i = 0; i < nFields; i++){
      _dataFields.push_back(tm._dataFields[i]);   
      _types.push_back(tm._types.at(i));
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
}
/////////////////////////////////
cfdTextureManager::~cfdTextureManager()
{
   if(_dataFields.size()){
      _dataFields.clear();
   }
   if(_types.size()){
      _types.clear();
   }
   if(_resolution){
      delete [] _resolution;
      _resolution = 0;
   }
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
/////////////////////////////////////////////////////////////////////
//add a vector field from a file                                   //
/////////////////////////////////////////////////////////////////////
void cfdTextureManager::addFieldTextureFromFile(char* textureFile)
{
   double tempMag[6] = {0,0,0,0,0,0};
   std::ifstream fin(textureFile);
   if(fin.is_open()){
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
      fin>>_range[0];
      fin>>_range[1];
      
      _transientRange[0] = (_range[0] < _transientRange[0])?_range[0]:_transientRange[0];
      _transientRange[1] = (_range[1] > _transientRange[1])?_range[1]:_transientRange[1];
      //bounding box
      fin>>_bbox[0]>>_bbox[1]>>_bbox[2]>>_bbox[3]>>_bbox[4]>>_bbox[5];
      
      if(!_resolution){
         _resolution = new int[3];
      }
      //the dimensions  
      fin>>_resolution[0]>>_resolution[1]>>_resolution[2];
      if(!_resolution[2]) _resolution[2] = 1; 

   
      double R,G,B,A;
      float alpha = 0;
      double distanceFactor = 0;
      
      int nPixels = _resolution[0]*_resolution[1]*_resolution[2];
      int i = 0; 
      int j = 0;
      int k = 0;
      unsigned char* pixels = new unsigned char[nPixels*4];
      float invSRange = 1.0/(_range[1]-_range[0]);
      if(curType == VECTOR){
         for(int p = 0; p < nPixels; p++){
            fin>>R;
            pixels[p*4   ] = (unsigned char)R;
            fin>>G;
            pixels[p*4 + 1] = (unsigned char)G;
            fin>>B;
            pixels[p*4 + 2] = (unsigned char)B;
            fin>>A;
            alpha = (A -_range[0])*invSRange;
            pixels[p*4 + 3] = (unsigned char)(alpha*255);
         }
      }else if(curType == SCALAR){
         //the scalar data
         
         float scalarValue = 0;
         for(int p = 0; p < nPixels; p++){
            fin>>scalarValue;
            alpha = (scalarValue-_range[0])*invSRange;
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
      //add the field
      _dataFields.push_back(pixels);
   }else{
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
   if(field)
      return field;//_dataFields[_curField];
   else return 0;
}
//////////////////////////////////////////////////////////////
int cfdTextureManager::timeToUpdate(double curTime,double delay)
{
   if(_mode == PLAY){
     if(curTime - _prevTime >= delay){
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
      int nFields = tm._dataFields.size();
   
      for(int i = 0; i < nFields; i++){
         _dataFields.push_back(tm._dataFields.at(i));
         _types.push_back(tm._types.at(i));
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
