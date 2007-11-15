/*************** <auto-copyright.pl BEGIN do not edit this line> *************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> **************/
#include <ves/xplorer/volume/NoiseTexture2D.h>
#include <iostream>
#include <fstream>
using namespace ves::xplorer::volume;
///////////////////////////////////////////////////////////////
NoiseTexture2D::NoiseTexture2D(unsigned int x, unsigned int y)
{
   _resolution[0] = x;
   _resolution[1] = y;
   _noiseData = new unsigned char[_resolution[0]*_resolution[1]];
   srand((unsigned)time(NULL));
   //float fData = 0;
   int index = 0;
   unsigned int data[256];
   for(unsigned int i = 0; i < 256; ++i)
   {
      data[i] = rand()%256;
   }
   for(unsigned int i = 0; i < _resolution[0]*_resolution[1];++i)
   {
      
      index = (1./(float)_resolution[0])*255.*rand()/(float)RAND_MAX;
      _noiseData[i] = static_cast<unsigned char>(data[index]);
      std::cout<<data<<" "<<index<<std::endl;
   }
	 
   _noiseTexture = new osg::Texture2D();
   _noiseTexture->setFilter(osg::Texture2D::MIN_FILTER,osg::Texture2D::NEAREST);
   _noiseTexture->setFilter(osg::Texture2D::MAG_FILTER,osg::Texture2D::NEAREST);
   _noiseTexture->setWrap(osg::Texture2D::WRAP_T,osg::Texture2D::REPEAT);
   _noiseTexture->setWrap(osg::Texture2D::WRAP_S,osg::Texture2D::REPEAT);
   _noiseTexture->setInternalFormat(GL_ALPHA);
   osg::ref_ptr<osg::Image> imageData = new osg::Image();
   imageData->setImage(_resolution[0],_resolution[1],1,
			              GL_ALPHA, 
			              GL_ALPHA, 
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

