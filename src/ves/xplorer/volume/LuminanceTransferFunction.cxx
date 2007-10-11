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

#include "ves/xplorer/volume/LuminanceTransferFunction.h"
#include "ves/xplorer/volume/TransferFunction.h"
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
/////////////////////////////////////
LuminanceTF::~LuminanceTF()
{
}
////////////////////////////////////////
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
/*void LuminanceTF::Update(unsigned int component,
                           void* data,
                           float rangeMin,
                           float rangeMax)
{

}*/
///////////////////////////////////
void LuminanceTF::_update()
{
   float newRange[2] = {0.,1.};
   if((_originalScalarRange[1]-_originalScalarRange[0]) == 0.0)
   {
      newRange[0] = 0.0;
      newRange[1] = 255.0;
   }
   else
   {
      newRange[0] = (_currentScalarRange[0] - _originalScalarRange[0])/
                (_originalScalarRange[1]-_originalScalarRange[0]);
      newRange[0] *= 255.0;
      newRange[1] = (_currentScalarRange[1] - _originalScalarRange[0])/
                (_originalScalarRange[1]-_originalScalarRange[0]);
      newRange[1] *= 255.0;
   }
   
   float invSRange = 0;
   float isoRange [2]= {0,1};
   float isoValue = 0;
   float alpha = 0;
   invSRange =  1.f/(newRange[1]-newRange[0]);
   for(unsigned int i = 0; i < _resolution[0]; i++)
   {
      {
         if(i < newRange[0])
         {
            _classification[ i*4 ] = 0;
            _classification[i*4 + 1] = 0;
            _classification[i*4 + 2] = 0;
            _classification[i*4 + 3] = 0;
         }
	      else if( i > newRange[1])
         {
            _classification[i*4 ] = 0;//255;
            _classification[i*4 + 1] = 0;
            _classification[i*4 + 2] = 0;
            _classification[i*4 + 3] = 0;
         }
         else
         {
            if(_isoSurface)
            {
               isoValue = newRange[0] + _percentIsoValue*(newRange[1] - newRange[0]);
               isoRange[0] = isoValue - 4.f;
               isoRange[1] = isoValue + 4.f;

               if(i >= isoRange[0] && i <= isoRange[1])
		         {
                  alpha = (i - newRange[0])*invSRange; 
				      _classification[i*4 ] = 
                  _classification[i*4 + 1] = 
                  _classification[i*4 + 2] = 
                  _classification[i*4 + 3] = alpha*(_resolution[0]-1);
               }
		         else
               {
                  _classification[i*4 ] = 
                  _classification[i*4 + 1] = 
                  _classification[i*4 + 2] = 
                  _classification[i*4 + 3] = 0;
               }
            }
	         else
            {
               alpha = (i - newRange[0])*invSRange;
			      _classification[i*4 ] = 
               _classification[i*4 + 1] = 
               _classification[i*4 + 2] = 
               _classification[i*4 + 3] =alpha*alpha*(_resolution[0]-1);
            }
         }
         _textureData[i*4 ]  = (unsigned char)_classification[i*4 ];
         _textureData[i*4 + 1] = (unsigned char)_classification[i*4 + 1];
         _textureData[i*4 + 2] = (unsigned char)_classification[i*4 + 2];
         _textureData[i*4 + 3] = (unsigned char)_classification[i*4 + 3]; 
      }
   }
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
