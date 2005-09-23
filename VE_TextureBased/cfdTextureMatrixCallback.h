/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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
 * File:          $RCSfile: filename,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
//class to update the texture matrix appropriately
#ifndef CFD_TEXTURE_MATRIX_CALLBACK_H 
#define CFD_TEXTURE_MATRIX_CALLBACK_H 
#ifdef VE_PATENTED
#ifdef _OSG

namespace osg
{
   class TexMat;
   class Node;
}
#include <osg/NodeCallback>
#include <osg/Vec3f>
#include "VE_Installer/include/VEConfig.h"

namespace VE_TextureBased
{
   class VE_TEXTURE_BASED_EXPORTS cfdTextureMatrixCallback : public osg::NodeCallback
   {
      public:
         cfdTextureMatrixCallback(osg::TexMat* texmat,osg::Vec3f center,
                       float* scale,float* trans);
         virtual void operator()(osg::Node* node,osg::NodeVisitor* nv);
    
      protected:
         float _trans[3];
         float _scale[3];
         osg::Vec3f _center;
        mutable osg::ref_ptr<osg::TexMat> _texMat;
   };
}
#endif //_OSG
#endif
#endif// CFD_TEXTURE_MATRIX_CALLBACK_H 
