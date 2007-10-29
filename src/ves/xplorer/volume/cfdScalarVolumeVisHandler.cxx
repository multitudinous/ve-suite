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
#ifdef _OSG

#include <ves/xplorer/volume/cfdTextureManager.h>
#include <ves/xplorer/volume/cfdScalarVolumeVisHandler.h>
#include <ves/xplorer/volume/cfdTextureMatrixCallback.h>

#include <ves/xplorer/volume/cfdScalarShaderManager.h>
#include <ves/xplorer/volume/GreyScaleShaderManager.h>


#include <osg/TexGen>
#include <osg/TexMat>
#include <osg/Group>
using namespace ves::xplorer::volume;
//////////////////////////////////////////////////////
//Constructors                                      //
//////////////////////////////////////////////////////
cfdScalarVolumeVisHandler::cfdScalarVolumeVisHandler()
:cfdVolumeVisNodeHandler()
{
   //_transferSM = 0;
}
//////////////////////////////////////////////////////////
cfdScalarVolumeVisHandler::cfdScalarVolumeVisHandler(const cfdScalarVolumeVisHandler& vvnh)
:cfdVolumeVisNodeHandler(vvnh)
{
   //_transferSM = new cfdScalarShaderManager(*vvnh._transferSM);
}
///////////////////////////////////////////////////////
cfdScalarVolumeVisHandler::~cfdScalarVolumeVisHandler()
{

   /*if(_transferSM)
   {
      delete _transferSM;
      _transferSM = 0;
   }*/

}
//////////////////////////////////////
void cfdScalarVolumeVisHandler::Init()
{
   cfdVolumeVisNodeHandler::Init();
   //set our names for debugging purposes
   SetBoundingBoxName("Scalar VVNH BBox");
   SetDecoratorName("Scalar VV Fragment PG");
}
/////////////////////////////////////////////////
void cfdScalarVolumeVisHandler::_setUpDecorator()
{
   if(!_tm)
   {
      return;
   }
   _createDefaultShaders();

}
///////////////////////////////////////////////////////
void cfdScalarVolumeVisHandler::_createDefaultShaders()
{
   if(!GetShaderManager("BLUE_RED_LINEAR_SHADER") && _tm)
   {
      int* fieldSize = _tm->fieldResolution();
      AddShaderManager("BLUE_RED_LINEAR_SHADER",new cfdScalarShaderManager());
	  AddShaderManager("GREY_SCALE_SHADER", new GreyScaleShaderManager());
      SetActiveShader("BLUE_RED_LINEAR_SHADER");
   }
}
////////////////////////////////////////////////////////////////////////
void cfdScalarVolumeVisHandler::SetTextureManager(cfdTextureManager* tm)
{
   cfdVolumeVisNodeHandler::SetTextureManager(tm);
   size_t nShaderManagers = _shaderManagers.size();
   if(nShaderManagers)
   { 
      size_t nShaderManagers = _shaderManagers.size();
      for ( std::map<std::string ,
         ves::xplorer::volume::cfdOSGShaderManager*>::iterator itr = _shaderManagers.begin();
         itr != _shaderManagers.end(); itr++ )
      {
         dynamic_cast<ves::xplorer::volume::cfdScalarShaderManager*>(itr->second)->UpdateTextureManager(_tm);
      }
   }
}
/////////////////////////////////////////////////////
void cfdScalarVolumeVisHandler::_applyTextureMatrix()
{
   unsigned int tUnit = 0;
   tUnit = GetShaderManager(_activeShader)->GetAutoGenTextureUnit();

   osg::ref_ptr<osg::TexMat> tMat = new osg::TexMat();
   tMat->setMatrix(osg::Matrix::identity());
   _decoratorGroup->getStateSet()->setTextureAttributeAndModes(tUnit,
                                                        tMat.get(),
                                                        osg::StateAttribute::ON);
   float trans[3] = {0,0,0};
   //_center = osg::Vec3(0,0,0);
   _decoratorGroup->setUpdateCallback(new cfdTextureMatrixCallback(tMat.get(),
                                                                  _center,
                                                                  _scale,
                                                                  trans));
   _updateTexGenUnit(tUnit);
}
//////////////////////////////////////////////////////////////////////////
cfdScalarVolumeVisHandler&
cfdScalarVolumeVisHandler::operator=(const cfdScalarVolumeVisHandler& vvnh)
{
   if(this != &vvnh){
      cfdVolumeVisNodeHandler::operator=(vvnh);
      //_transferSM = vvnh._transferSM;
   }
   return *this;
}
#endif //_OSG
