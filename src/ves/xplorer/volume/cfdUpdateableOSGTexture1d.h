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
#ifndef CFD_UPDATEABLE_TRANSFER_FUNCTION_TEXTURE_1D_H
#define CFD_UPDATEABLE_TRANSFER_FUNCTION_TEXTURE_1D_H
/*!\file cfdUpdateableOSGTexture1d.h
* cfdUpdateableOSGTexture1d API
*/

/*!\class VE_TextureBased::cfdUpdateableOSGTexture1d
*
*/
#ifdef _PERFORMER
#elif _OSG
#include <osg/Texture1D>
#include "ves/VEConfig.h"
namespace VE_TextureBased
{
   class VE_TEXTURE_BASED_EXPORTS cfdUpdateableOSGTexture1d
      : public  osg::Texture1D::SubloadCallback{
      public:
         cfdUpdateableOSGTexture1d();
         cfdUpdateableOSGTexture1d(const cfdUpdateableOSGTexture1d& cb);
         virtual ~cfdUpdateableOSGTexture1d();

         enum TransType{ALPHA_CUTOFF,GAMMA_CORRECTION};
   
         void UpdateParam(TransType type,GLfloat param);
         void SetGamma(GLfloat gamma);
         void SetAlphaCutoff(GLfloat aCutoff);
         void SetTransferFunctionType(TransType type);
   
         void subload(const osg::Texture1D& texture,osg::State& state) const;
         void load(const osg::Texture1D& texture,osg::State&) const;

         cfdUpdateableOSGTexture1d& operator=(const cfdUpdateableOSGTexture1d& cb);
      protected:	
         void _updateData();
         bool _needsUpdate()const;

         unsigned char* _data;
         GLfloat _alphaCutoff;
         GLfloat _gamma;
         GLfloat _lastAlpha;
         GLfloat _lastGamma;

         TransType _type;
         mutable GLsizei _textureWidth,_oWidth;
   };
}
#endif //_OSG
#endif //CFD_UPDATEABLE_TRANSFER_FUNCTION_TEXTURE_1D_H
