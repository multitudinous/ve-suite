
#include "./RedYellowGreenCyanBlueTransferFunction.h"
#include "./TransferFunction.h"
#include <iostream>
////////////////////////////////////////////
RYGCBLinearTF::RYGCBLinearTF(unsigned int s)
:TransferFunction1D(s)
{
   _types[0] = TransferFunction::RAMP;
   _types[1] = TransferFunction::RAMP;
   _types[2] = TransferFunction::RAMP;
}
//////////////////////////////////////////////////////
RYGCBLinearTF::RYGCBLinearTF(const RYGCBLinearTF &rhs)
:TransferFunction(rhs)
{
}
///////////////////////////////
RYGCBLinearTF::~RYGCBLinearTF()
{
}
/////////////////////////////////////
void RYGCBLinearTF::InitializeData()
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

   float inverseRange = 1.f/(float)_resolution[0];
   float alpha = 0;
   for(unsigned int i = 0; i < _resolution[0]; i++)
   {
      alpha = i*inverseRange;
      if(alpha <= .25)
      {
         _classification[i*4    ] = 0;
         _classification[i*4 + 1] = (4.f*alpha)*255.f,      
         _classification[i*4 + 2] = (1.0)*255.f;
         _classification[i*4 + 3] = alpha*255.f;
      }
      else if(alpha <= .5)
      {
         _classification[i*4    ] = 0;
         _classification[i*4 + 1] = (1.f)*255.f,      
         _classification[i*4 + 2] = (2.f-4.f*alpha)*255.f;
         _classification[i*4 + 3] = alpha*255.f;
       }
       else if(alpha <= .75)
       {
         _classification[i*4    ] = (4.f*alpha-2.f)*255.f;
         _classification[i*4 + 1] = (1.f)*255.f;       
         _classification[i*4 + 2] = 0.;
         _classification[i*4 + 3] = alpha*255.f;
       }
       else if(alpha <= 1.0)
       {
         _classification[i*4    ]  = (1.f)*255.f;
         _classification[i*4 + 1] = (4.f-4.f*alpha)*255.f;       
         _classification[i*4 + 2] = 0.;
         _classification[i*4 + 3] = alpha*255.f;
       }
       _textureData[i*4    ] = static_cast<unsigned char>(_classification[i*4]);
       _textureData[i*4 + 1] = static_cast<unsigned char>(_classification[i*4+1]);
       _textureData[i*4 + 2] = static_cast<unsigned char>(_classification[i*4+2]);
       _textureData[i*4 + 3] = static_cast<unsigned char>(_classification[i*4+3]);
   }   
}
/////////////////////////////////////////////////
void RYGCBLinearTF::Update(unsigned int component,
                           void* data,
                           float rangeMin,
                           float rangeMax)
{

} 
/////////////////////////////////////////////////////////////////
RYGCBLinearTF& RYGCBLinearTF::operator=(const RYGCBLinearTF& rhs)
{
   if(this != &rhs)
   {
      TransferFunction::operator =(rhs);
   }
   return *this;
}
