#include "VE_Xplorer/TextureBased/PreIntegrationTexture.h"
#include "VE_Xplorer/TextureBased/TransferFunction.h"
#include <iostream>
using namespace VE_TextureBased;
///////////////////////////////////////////////////////////////
PreIntegrationTexture2D::PreIntegrationTexture2D()
{
   _tf = 0;
   _sliceIntegrationValues = 0;
   _rawData = 0;
   _preIntegratedTexture = new osg::Texture2D();
   _imageData = new osg::Image();
}
////////////////////////////////////////////////////////////////////////////////////////////////////////////
PreIntegrationTexture2D::PreIntegrationTexture2D(const PreIntegrationTexture2D& rhs)
{
   _tf = rhs._tf;
   _sliceIntegrationValues = new float[_tf->GetResolution(0)*4];
   for(unsigned int i = 0; i < _tf->GetResolution(0); ++i)
   {
	   _sliceIntegrationValues[i*4] = rhs._sliceIntegrationValues[i*4];
	   _sliceIntegrationValues[i*4+ 1] = rhs._sliceIntegrationValues[i*4 + 1];
	   _sliceIntegrationValues[i*4+ 2] = rhs._sliceIntegrationValues[i*4 + 2];
	   _sliceIntegrationValues[i*4+ 3] = rhs._sliceIntegrationValues[i*4 + 3];
   }
   _rawData = new unsigned char[_tf->GetResolution(0)*_tf->GetResolution(0)*4];
   for(unsigned int i = 0; i < _tf->GetResolution(0)*_tf->GetResolution(0); ++i)
   {
	   _rawData[i*4] = rhs._rawData[i*4];
	   _rawData[i*4+ 1] = rhs._rawData[i*4 + 1];
	   _rawData[i*4+ 2] = rhs._rawData[i*4 + 2];
	   _rawData[i*4+ 3] = rhs._rawData[i*4 + 3];
   }
   _preIntegratedTexture = new osg::Texture2D(*rhs._preIntegratedTexture.get());
   _imageData = new osg::Image(*rhs._imageData.get());
}
///////////////////////////////////////////////////////////////////////////////////////////
PreIntegrationTexture2D& PreIntegrationTexture2D::operator=(const PreIntegrationTexture2D& rhs)
{
	if(this != &rhs)
	{
		_tf = rhs._tf;
	   _sliceIntegrationValues = new float[_tf->GetResolution(0)*4];
	   for(unsigned int i = 0; i < _tf->GetResolution(0); ++i)
	   {
		   _sliceIntegrationValues[i*4] = rhs._sliceIntegrationValues[i*4];
		   _sliceIntegrationValues[i*4+ 1] = rhs._sliceIntegrationValues[i*4 + 1];
		   _sliceIntegrationValues[i*4+ 2] = rhs._sliceIntegrationValues[i*4 + 2];
		   _sliceIntegrationValues[i*4+ 3] = rhs._sliceIntegrationValues[i*4 + 3];
	   }
	   _rawData = new unsigned char[_tf->GetResolution(0)*_tf->GetResolution(0)*4];
	   for(unsigned int i = 0; i < _tf->GetResolution(0)*_tf->GetResolution(0); ++i)
	   {
		   _rawData[i*4] = rhs._rawData[i*4];
		   _rawData[i*4+ 1] = rhs._rawData[i*4 + 1];
		   _rawData[i*4+ 2] = rhs._rawData[i*4 + 2];
		   _rawData[i*4+ 3] = rhs._rawData[i*4 + 3];
	   }
	   _preIntegratedTexture = new osg::Texture2D(*rhs._preIntegratedTexture.get());
	   _imageData = new osg::Image(*rhs._imageData.get());
	}
	return *this;
}
/////////////////////////////////////////////////////////////////
PreIntegrationTexture2D::~PreIntegrationTexture2D()
{
   if(_sliceIntegrationValues)
   {
      delete [] _sliceIntegrationValues;
      _sliceIntegrationValues = 0;
   }
   if(_rawData)
   {
      delete [] _rawData;
      _rawData = 0;
   }
}
////////////////////////////////////////////////////////////////////////////////////////
void PreIntegrationTexture2D::SetTransferFunction(TransferFunction* tf)
{
   if(tf->GetDimension() != 1)
   {
		std::cout<<"Error!"<<std::endl;
		std::cout<<"Only 1D transfer functions are supported for PreIntegration!"<<std::endl;
		return;
   }
  
   _tf = tf;
   ///transfer function resolution shouldn't change but just in case
   if(_sliceIntegrationValues)
   {
      delete []  _sliceIntegrationValues;
      _sliceIntegrationValues = 0;
   }
  
   if(_rawData)
   {
      delete [] _rawData;
	  _rawData = 0;
   }
   _sliceIntegrationValues = new float[_tf->GetResolution(0)*4];
   _rawData = new unsigned char[_tf->GetResolution(0)*_tf->GetResolution(0)*4];

   _imageData->setImage(_tf->GetResolution(0),_tf->GetResolution(0),
									_tf->GetResolution(2),
									GL_RGBA, 
									GL_RGBA, 
									GL_UNSIGNED_BYTE,
									_rawData, 
									osg::Image::USE_NEW_DELETE);
   _preIntegratedTexture->setImage(_imageData.get());
}
////////////////////////////////////////////////
void PreIntegrationTexture2D::Update()
{
   if(!_tf)
   {
      std::cout<<"Error!!!"<<std::endl;
      std::cout<<"Transfer function not set!!"<<std::endl;
      std::cout<<"PreIntegrationTexture2D::Update()"<<std::endl;

   }
   //make sure we have a texture
   if(!_preIntegratedTexture.valid())
   {
	   _preIntegratedTexture = new osg::Texture2D;
   }
   //Initialize the slice integration table
   _initializeSliceIntegrationValues();

   unsigned int dataDims = _tf->GetResolution(0);
   //integrate simpsons rule for each sf,sb combination
   unsigned int row = -1;
   unsigned int col = 0;
   unsigned char data[4] = {0,0,0,0};
   unsigned int index = 0;
   unsigned int sliceMin = 0;
   unsigned int sliceMax = 0;
   unsigned char* rawData = _preIntegratedTexture->getImage()->data();
   float deltaSlice = 0;
   for(unsigned int i = 0; i < dataDims*dataDims; i++)
   {
      col = i%dataDims;
      row = (i%dataDims==0)?row+1:row;
      sliceMin = col;
	  sliceMax = row;
      if(row < col)
	  {
         sliceMin = row;
		 sliceMax = col;
	  }
	  deltaSlice = 1./(float)(sliceMax - sliceMin);

	  if(sliceMin != sliceMax)
	  {
         rawData[index++] = _calculateComponent(deltaSlice,0,sliceMin,sliceMax);
         rawData[index++] = _calculateComponent(deltaSlice,1,sliceMin,sliceMax);
         rawData[index++] = _calculateComponent(deltaSlice,2,sliceMin,sliceMax);
         rawData[index++] = _calculateComponent(deltaSlice,3,sliceMin,sliceMax);
	  }
	  else
	  {
         rawData[index++] = _tf->unsignedByteDataAt(sliceMin*4     );
         rawData[index++] = _tf->unsignedByteDataAt(sliceMin*4 + 1);
         rawData[index++] = _tf->unsignedByteDataAt(sliceMin*4 + 2);
         rawData[index++] = _tf->unsignedByteDataAt(sliceMin*4 +  3);
	  }
	  /*if(col == row)
	  {
         fout<<"("<<(int)dependentTexture[i*4]<<","
               <<(int)dependentTexture[i*4 + 1]<<","
               <<(int)dependentTexture[i*4 + 2]<<","
               <<(int)dependentTexture[i*4 + 3]<<")";
         fout<<std::endl;
	  }*/
		 
   }
   _preIntegratedTexture->dirtyTextureObject();
   _preIntegratedTexture->dirtyTextureParameters();
}
////////////////////////////////////////////////////////////////////////////////////
osg::Texture2D* PreIntegrationTexture2D::GetPreIntegratedTexture()
{
	if(_preIntegratedTexture.valid())
	{
		return _preIntegratedTexture.get();
	}
	return 0;
}
//////////////////////////////////////////////////////////////////////////////
void PreIntegrationTexture2D::_initializeSliceIntegrationValues()
{
   //std::fstream fout("./interpo.txt",std::ios::out);
   float* curRGBA = 0;//[4]={0,0,0,0};
   float* prevRGBA = 0;//[4]={0,0,0,0};
   float totalRGBA[4] = {0,0,0,0};
   _sliceIntegrationValues[0] = _tf->EvaluateAt(0)[0];
   _sliceIntegrationValues[1] = _tf->EvaluateAt(0)[1];
   _sliceIntegrationValues[2] = _tf->EvaluateAt(0)[2];
   _sliceIntegrationValues[3] = _tf->EvaluateAt(0)[3];
   for(unsigned int i = 1; i < _tf->GetResolution(0); ++i)
   {
      curRGBA = _tf->EvaluateAt(i*4);
      prevRGBA = _tf->EvaluateAt((i*4)-1);

      totalRGBA[0] = totalRGBA[0] + .5*(curRGBA[0]+prevRGBA[0]);
      totalRGBA[1] = totalRGBA[1] + .5*(curRGBA[1]+prevRGBA[1]);
      totalRGBA[2] = totalRGBA[2] + .5*(curRGBA[2]+prevRGBA[2]);
      totalRGBA[3] = totalRGBA[3] + .5*(curRGBA[3]+prevRGBA[3]);
      _sliceIntegrationValues[i*4] = totalRGBA[0];
      _sliceIntegrationValues[i*4 + 1] = totalRGBA[1];
      _sliceIntegrationValues[i*4 + 2] = totalRGBA[2];
      _sliceIntegrationValues[i*4 + 3] = totalRGBA[3];
    /*fout<<"("<<sliceIntegrationValues[i*4]<<","
               <<sliceIntegrationValues[i*4 + 1]<<","
               <<sliceIntegrationValues[i*4 + 2]<<","
               <<sliceIntegrationValues[i*4 + 3]<<")";
      fout<<std::endl;*/
   }
}
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
unsigned char PreIntegrationTexture2D::_calculateComponent(float ds, unsigned int component,
                                                                 unsigned int sliceMin, unsigned int sliceMax)
{
   float frontData = 0;
   float backData = 0;
   //front slice
   frontData = _sliceIntegrationValues[sliceMin*4 + component];
   //back slice
   backData = _sliceIntegrationValues[sliceMax*4 + component];
   
   ///Klaus. eq. 8
   //alpha
   if(component==3)
      return static_cast<unsigned char>(255.0 * (1.0 -(exp(-ds*(backData - frontData)))));
   return static_cast<unsigned char>(ds*(backData - frontData));
}






