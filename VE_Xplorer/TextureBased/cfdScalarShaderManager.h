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
   ///Construtor
   cfdScalarShaderManager();
   ///Destructor
   virtual ~cfdScalarShaderManager(){}
   ///Initialize parameters
   virtual void Init();
   ///Update the cfdTextureManager
   ///\param tm cfdTextureManager pointer
   virtual void UpdateTextureManager(cfdTextureManager* tm);
   ///Set the entire scalar range
   ///\param range The scalar range
   void SetScalarRange(float* range);
   ///Set the scalar maximum The maximum scalar value
   void UpdateScalarMax(float maxScalar);
   ///Set the scalar minimum
   ///\param minScalar The minimum scalar value
   void UpdateScalarMin(float minScalar);
   ///Activate the iso-surface shader
   void ActivateIsoSurface();
   ///Deactivate the iso-surface shader
   void DeactivateIsoSurface();
   ///Set the iso-surface value
   ///\param percentScalarRange The percentage of the scalar range to create an iso-surface
   void SetIsoSurfaceValue(float percentScalarRange);
   ///Set the current timestep
   ///\param whichTimeStep The current time steap
   ///\param makeSlave Cluster information
   void SetCurrentTransientTexture(unsigned int whichTimeStep, bool makeSlave = false);
   ///Set the delay between timesteps for transient data
   ///\param delay Delay time in seconds
   void SetDelayTime(double delay);
   ///Make sure the scalar range is current
   void EnsureScalarRange();

   ///Update the entire transfer function...slower but better quality
   void FullTransferFunctionUpdate();

   ///Update the diagonal of the 2D tranfer function...faster but lower quality
   void FastTransferFunctionUpdate();

protected:
    void _setupStateSetForGLSL();
    bool _isoSurface;///<Activate iso surface
	 bool _preIntegrate;///<Flag determining how to update Pre-Integration table
	 ///Update the transfer function.
	 ///\param fastUpdate If preintegration is used, do a fast update to remain interactive.
     virtual void _updateTransferFunction(bool fastUpdate=true);
     float _scalarRange[2];///<Scalar range
     float _percentScalarRange;///<Percentage of scalar range
     float _stepSize[3];///<Step size
     ///Initialize transfer functions
     virtual void _initTransferFunctions();
     ///Initialize property texture
     void _initPropertyTexture();
};
}
#endif//_OSG
#endif// CFD_SCALAR_SHADER_MANAGER_H
