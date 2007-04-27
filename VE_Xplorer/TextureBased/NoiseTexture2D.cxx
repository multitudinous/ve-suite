#include "VE_Xplorer/TextureBased/NoiseTexture2D.h"
#include <iostream>
#include <fstream>
using namespace VE_TextureBased;
///////////////////////////////////////////////////////////////
NoiseTexture2D::NoiseTexture2D(unsigned int x, unsigned int y)
{
   _resolution[0] = x;
   _resolution[1] = y;
   _noiseData = new unsigned char[_resolution[0]*_resolution[1]];
   srand((unsigned)time(NULL));
   for(unsigned int i = 0; i < _resolution[0]*_resolution[1];++i)
   {
      _noiseData[i]= 255.*rand()/(float)RAND_MAX;
   }
	 
   _noiseTexture = new osg::Texture2D();
   _noiseTexture->setDataVariance(osg::Object::DYNAMIC);
   _noiseTexture->setFilter(osg::Texture2D::MIN_FILTER,osg::Texture2D::NEAREST);
   _noiseTexture->setFilter(osg::Texture2D::MAG_FILTER,osg::Texture2D::NEAREST);
   _noiseTexture->setWrap(osg::Texture2D::WRAP_T,osg::Texture2D::REPEAT);
   _noiseTexture->setWrap(osg::Texture2D::WRAP_S,osg::Texture2D::REPEAT);
   _noiseTexture->setInternalFormat(GL_LUMINANCE);
   osg::ref_ptr<osg::Image> imageData = new osg::Image();
   imageData->setImage(_resolution[0],_resolution[1],1,
			              GL_LUMINANCE8, 
			              GL_LUMINANCE, 
                        GL_UNSIGNED_BYTE,
                        _noiseData, 
                        osg::Image::USE_NEW_DELETE);
   _noiseTexture->setImage(imageData.get());
}
/////////////////////////////////////////////////////////
NoiseTexture2D::NoiseTexture2D(const NoiseTexture2D& rhs)
{
   _resolution[0] = rhs._resolution[0];
   _resolution[1] = rhs._resolution[1];
   _noiseData = new unsigned char[_resolution[0]*_resolution[1]];
   for(unsigned int i = 0; i < _resolution[0]*_resolution[1];++i)
   {
      _noiseData[i]= 255.*rand()/(float)RAND_MAX;
   }
   _noiseTexture = new osg::Texture2D(*rhs._noiseTexture.get());
}
///////////////////////////////////////////////////////////////////////////////////////////
NoiseTexture2D& NoiseTexture2D::operator=(const NoiseTexture2D& rhs)
{
   if(this != &rhs)
   {
      _resolution[0] = rhs._resolution[0];
      _resolution[1] = rhs._resolution[1];
      _noiseData = new unsigned char[_resolution[0]*_resolution[1]];
      for(unsigned int i = 0; i < _resolution[0]*_resolution[1];++i)
      {
         _noiseData[i]= 255.*rand()/(float)RAND_MAX;
      }
      _noiseTexture = new osg::Texture2D(*rhs._noiseTexture.get());
   }
   return *this;
}
/////////////////////////////////////////////////////////////////
NoiseTexture2D::~NoiseTexture2D()
{
   /*if(_noiseData)
   {
      delete [] _noiseData;
      _noiseData = 0;
   }*/
}
////////////////////////////////////////////////////////////////////////////////////
osg::Texture2D* NoiseTexture2D::GetNoiseTexture()
{
	if(_noiseTexture.valid())
	{
		return _noiseTexture.get();
	}
	return 0;
}

