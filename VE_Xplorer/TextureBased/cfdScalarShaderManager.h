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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_SCALAR_SHADER_MANAGER_H
#define CFD_SCALAR_SHADER_MANAGER_H
/*!\file cfdScalarShaderManager.h
* cfdScalarShaderManager API
*/

/*!\class VE_TextureBased::cfdScalarShaderManager
*
*/
#ifdef VE_PATENTED
#ifdef _OSG
#include "VE_Xplorer/TextureBased/cfdOSGTransferShaderManager.h"
namespace VE_TextureBased
{
   class cfdTextureManager;
}
namespace VE_TextureBased
{
   class VE_TEXTURE_BASED_EXPORTS cfdScalarShaderManager
      :public cfdOSGTransferShaderManager{
      public:
         cfdScalarShaderManager();
         virtual ~cfdScalarShaderManager(){}
         virtual void Init();
         virtual void UpdateTextureManager(cfdTextureManager* tm);
         void SetScalarRange(float* range);
         void UpdateScalarMax(float maxScalar);
         void UpdateScalarMin(float minScalar);
         void ActivateIsoSurface();
         void DeactivateIsoSurface();
         void SetIsoSurfaceValue(float percentScalarRange);
         void SetCurrentTransientTexture(unsigned int whichTimeStep, bool makeSlave = false);
         void SetDelayTime(double delay);
         void EnsureScalarRange();
      protected:
         void _setupStateSetForGLSL();
         bool _isoSurface;
         virtual void _updateTransferFunction(bool preIntegrated=false);
         float _scalarRange[2];
         float _percentScalarRange;
         float _stepSize[3];
         void _initTransferFunctions();
         void _initPropertyTexture();
   };
}
#endif//_OSG
#endif
#endif// CFD_SCALAR_SHADER_MANAGER_H
