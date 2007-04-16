
#include "VE_Xplorer/TextureBased/LuminanceTransferFunction.h"
#include "VE_Xplorer/TextureBased/TransferFunction.h"
#include <iostream>
#include <cmath>
using namespace VE_TextureBased;
//////////////////////////////////////////////
LuminanceTF::LuminanceTF(unsigned int s)
:TransferFunction1D(s)
{
}
//////////////////////////////////////////////////////
LuminanceTF::LuminanceTF(const LuminanceTF &rhs)
:TransferFunction(rhs)
{
}
///////////////////////////////
LuminanceTF::~LuminanceTF()
{
}
/////////////////////////////////////
void LuminanceTF::InitializeData()
{
   if(_classification)
   {
      std::cout<<"WARNING!!!!!"<<std::endl;
      std::cout<<"Transfer function already allocated!!!!"<<std::endl;
      //Can't re-allocate for now so that we don't have to re-create
      //texture in GL
      return;
   }
   _classification = new float[_resolution[0]*4];
   _textureData = new unsigned char[_resolution[0]*4];
   
   unsigned char* gTable = new unsigned char[_resolution[0]];
   double gamma = 2.4;
   for (unsigned int i=0; i<_resolution[0]; i++) 
   {       
      double y = (double)(i)/(_resolution[0]-1.0);   
      y = pow(y, 1.0/gamma);     
      gTable[i] = (int) floor((_resolution[0]-1.0) * y + 0.5);  
   }

   float inverseRange = 1.f/(float)_resolution[0];
   float alpha = 0;
   for(unsigned int i = 0; i < _resolution[0]; i++)
   {
      alpha = gTable[i]*inverseRange;
      {
         _classification[i*4    ] = 
         _classification[i*4 + 1] =        
         _classification[i*4 + 2] =
         _classification[i*4 + 3] = alpha*255.f;
       }
       _textureData[i*4    ] = static_cast<unsigned char>(_classification[i*4]);
       _textureData[i*4 + 1] = static_cast<unsigned char>(_classification[i*4+1]);
       _textureData[i*4 + 2] = static_cast<unsigned char>(_classification[i*4+2]);
       _textureData[i*4 + 3] = static_cast<unsigned char>(_classification[i*4+3]);
   }
   delete [] gTable;
   gTable = 0;
}
/////////////////////////////////////////////////
void LuminanceTF::Update(unsigned int component,
                           void* data,
                           float rangeMin,
                           float rangeMax)
{

}
/////////////////////////////////////////////////////////////////
LuminanceTF& LuminanceTF::operator=(const LuminanceTF& rhs)
{
   if(this != &rhs)
   {
      TransferFunction::operator =(rhs);
   }
   return *this;
}
