/*************** <auto-copyright.pl BEGIN do not edit this line> **************
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <iostream>
#ifdef _OSG
#include <osg/Texture3D>
#include <osg/Texture2D>
#include <osg/BlendFunc>
#include <osg/TexEnv>
#include <osg/TexMat>
#include <osg/TexGen>
#include "VE_Xplorer/TextureBased/cfdTextureManager.h"
#include "VE_Xplorer/TextureBased/cfdUpdateTextureCallback.h"
#include "VE_Xplorer/TextureBased/cfdScalarShaderManager.h"
#include "VE_Xplorer/TextureBased/GreyScaleShaderManager.h"
#include "VE_Xplorer/TextureBased/LuminanceTransferFunction.h"
#include "VE_Xplorer/TextureBased/PreIntegrationTexture.h"
using namespace VE_TextureBased;

////////////////////////////////////////////////////////
/*void GreyScaleShaderManager::_updateTransferFunction(bool preIntegrated)
{
   if(!_tf)
   {
	  std::cout<<"Transfer function not set!"<<std::endl;
	  std::cout<<"cfdScalarShaderManager::_updateTransferFunction"<<std::endl;
      return;
   }
   if(fastUpdate)
   {
      _preIntTexture->FastUpdate();
   }
   else
   {
	   _preIntTexture->FullUpdate();
   }
   //This in no different than the regular shader except that
   //the range is between 20->346.48;
   GLubyte* lutex =0;
   GLfloat R,G,B,A;
   GLfloat newMid = 0;
   GLfloat newRange[2];
   ScalarRange origRange ;
   GLfloat alpha = 0;
   GLfloat isoVal = 0;
   float invSRange = 0;
   
   //gamma table
   GLubyte gTable[256];
   double gamma = 2.4;
   //double y = 0;
   for (int i=0; i<256; i++)
   {       
      double y = (double)(i)/255.0;   
      y = pow(y, 1.0/gamma);     
      gTable[i] = (int) floor(255.0 * y + 0.5);  
   }
   osg::ref_ptr<osg::Texture2D> tFunc = _transferFunctions.at(0);
   if(tFunc.valid())
   {
      lutex = tFunc->getImage()->data();
      if(!lutex)
      {
         std::cout<<"ERROR!"<<std::endl;
         std::cout<<"Invalid data for transfer function!!"<<std::endl;
         std::cout<<"GreyScaleShaderManager::_updateTransferFunction()"<<std::endl;
         return;
      }
   }
   
   origRange = _tm->dataRange(_tm->GetCurrentFrame());
   if((origRange.range[1]-origRange.range[0]) == 0.0)
   {
      newRange[0] = 0.0;
      newRange[1] = 255.0;
   }
   else
   {
      newRange[0] = (_scalarRange[0] - origRange.range[0])/
                (origRange.range[1]-origRange.range[0]);
      newRange[0] *= 255.0;
      newRange[1] = (_scalarRange[1] - origRange.range[0])/
                (origRange.range[1]-origRange.range[0]);
      newRange[1] *= 255.0;
   }
   newMid = newRange[0] + .5*(newRange[1] - newRange[0]);
   invSRange =  1.0/(newRange[1]-newRange[0]);
   float opacity = 1.0/128.0;
  //Only update the diagonal so that mods are fast
   {
      for(int i = 0; i < 256; i++)
      {
         {
            if(i < newRange[0])
            {
               lutex[i*(256*4) + i*4 ] = 0;
               lutex[i*(256*4) + i*4 + 1] = 0;
               lutex[i*(256*4) + i*4 + 2] = 0;
               lutex[i*(256*4) + i*4 + 3] = 0;
            }
	         else if( i > newRange[1])
	         {
               lutex[i*(256*4) + i*4 ] = 0;//255;
               lutex[i*(256*4) + i*4 + 1] = 0;
               lutex[i*(256*4) + i*4 + 2] = 0;
               lutex[i*(256*4) + i*4 + 3] = 0;
            }
	        else
	        {
               if(_isoSurface)
               {
                  GLfloat isoRange [2];
                  isoVal = newRange[0] + _percentScalarRange*(newRange[1] - newRange[0]);
                  isoRange[0] = isoVal - 4.0;
                  isoRange[1] = isoVal + 4.0;

                  if(i >= isoRange[0] && i <= isoRange[1])
			         {
                     alpha = (i - newRange[0])*invSRange; 
			            R = 
                     G =       
                     B = 
                     A = alpha*255.0f;
                  }
			         else
                  {
                     R = 0;
                     G = 0;
                     B = 0;
                     A = 0;
                  }
               }
		         else
               {
                  alpha = (i - newRange[0])*invSRange; 
			         R = 
                  G =       
                  B = 
                  A = alpha*alpha*255.0f;
               }
               lutex[i*(256*4) + i*4 ]  = (unsigned char)R;
               lutex[i*(256*4) + i*4 + 1] = (unsigned char)G;
               lutex[i*(256*4) + i*4 + 2] = (unsigned char)B;
               lutex[i*(256*4) + i*4 + 3] = (unsigned char)A; 
            }
         }
      }
   }
   tFunc->dirtyTextureParameters();
   tFunc->dirtyTextureObject();
   
}*/
///////////////////////////////////////////////////////////////////
void GreyScaleShaderManager::_initTransferFunctions()
{
   if(_transferFunctions.empty())
   {
      //create transfer functions
      //_createTransferFunction(); 
      if(_tf)
	  {
		  delete _tf;
		  _tf = 0;
	  }
	  if(_preIntTexture)
	  {
		  delete _preIntTexture;
		  _preIntTexture = 0;
	  }
	  _tf = new VE_TextureBased::LuminanceTF();
	  _tf->InitializeData();
	  _preIntTexture = new VE_TextureBased::PreIntegrationTexture2D();
	  _preIntTexture->SetTransferFunction(_tf);
	  _preIntTexture->FullUpdate();
      _transferFunctions.push_back(_preIntTexture->GetPreIntegratedTexture());
   }
}
#endif//_OSG
