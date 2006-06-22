/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 * File:          $RCSfile: cfdTextureMatrixCallback.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
//class to update the texture matrix appropriately
#ifdef VE_PATENTED
#ifdef _OSG
#include "VE_TextureBased/cfdTextureMatrixCallback.h"
#include <osg/TexMat>
#include <osg/Matrix>
#include <osg/Node>
using namespace VE_TextureBased;
////////////////////////////////////////////////////////////////////////
//Constructor                                                         //
////////////////////////////////////////////////////////////////////////
cfdTextureMatrixCallback::cfdTextureMatrixCallback(osg::TexMat* texmat,
                                             osg::Vec3f center,
                                        float* scale,float* trans)
:_texMat(texmat),_center(center)
{
   _scale[0] = scale[0];
   _scale[1] = scale[1];
   _scale[2] = scale[2];

   _trans[0] = trans[0];
   _trans[1] = trans[1];
   _trans[2] = trans[2];
}
////////////////////////////////////////////////////////////////////////////
void cfdTextureMatrixCallback::operator()(osg::Node* node,osg::NodeVisitor* nv)
{
   //osgUtil::CullVisitor* cv = dynamic_cast<osgUtil::CullVisitor*>(nv);
   if (_texMat.valid()){
      osg::Matrix translate = osg::Matrix::translate(_trans[0],_trans[1],_trans[2]);
      osg::Matrix scale = osg::Matrix::scale(_scale[0],_scale[1],_scale[2]);
      osg::Matrix center = osg::Matrix::translate(-_center[0],-_center[1],-_center[2]);
      
      _texMat->setMatrix(center*scale*translate);
   }
   traverse(node,nv);
}
#endif //_OSG
#endif
