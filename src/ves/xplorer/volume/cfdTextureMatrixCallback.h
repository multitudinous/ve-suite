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
//class to update the texture matrix appropriately
#ifndef CFD_TEXTURE_MATRIX_CALLBACK_H 
#define CFD_TEXTURE_MATRIX_CALLBACK_H 
/*!\file cfdTextureMatrixCallback.h
* cfdTextureMatrixCallback API
*/

/*!\class VE_TextureBased::cfdTextureMatrixCallback
*
*/
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
         ///Callback that updates the texture matrix
         ///\param texmat The osg::TexMat to update
         ///\param center The center of the data
         ///\param scale The scale of the data (compared to unit cube)
         ///\param trans The translation for the texture matrix
         cfdTextureMatrixCallback(osg::TexMat* texmat,osg::Vec3f center,
                       float* scale,float* trans);
         ///Update the texture matrix
         ///\param node The osg::Node to update
         ///\param nv The osg::NodeVisitor
         virtual void operator()(osg::Node* node,osg::NodeVisitor* nv);
    
      protected:
         float _trans[3];///<Translation vector
         float _scale[3];///<The scale vector
         osg::Vec3f _center;///<The center
        mutable osg::ref_ptr<osg::TexMat> _texMat;///<The texture matrix
   };
}
#endif //_OSG
#endif// CFD_TEXTURE_MATRIX_CALLBACK_H 
