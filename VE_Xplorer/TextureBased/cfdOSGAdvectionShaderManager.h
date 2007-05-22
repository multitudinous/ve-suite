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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_OSG_ADVECTION_SHADER_MANAGER_H
#define CFD_OSG_ADVECTION_SHADER_MANAGER_H
/*!\file cfdOSGAdvectionShaderManager.h
* cfdOSGAdvectionShaderManager API
*/

/*!\class VE_TextureBased::cfdOSGAdvectionShaderManager
*Class that handles 3d texture advection
*/
#ifdef _OSG
namespace osg{
   class Texture3D;
   class Texture1D;
   class State;
}
#include <vector>
#include "VE_Xplorer/TextureBased/cfdOSGShaderManager.h"
#include "VE_Xplorer/TextureBased/cfdUpdateableOSGNoiseTexture3d.h"

namespace VE_TextureBased
{
   class cfdUpdateParameterCallback;
   class cfdUpdateMatrixParameterCallback;
}

namespace VE_TextureBased
{
   class VE_TEXTURE_BASED_EXPORTS cfdOSGAdvectionShaderManager
      : public cfdOSGShaderManager
   {
      public:
         ///Cosntructor        
         cfdOSGAdvectionShaderManager();
         ///Copy Constructor 
         ///\param sm The cfdOSGAdvectionShaderManager to create a copy from
         cfdOSGAdvectionShaderManager(const cfdOSGAdvectionShaderManager& sm);
         ///Destructor
         virtual ~cfdOSGAdvectionShaderManager();

         enum NoiseParam{TAO_H,TAO_I};
  
         ///Initialze the parameters such as textures and shaders.
         virtual void Init();
         ///Set the property texture dimensions
         ///\param x Texture dimenision corresponding to the s direction
         ///\param y Texture dimenision corresponding to the t direction
         ///\param z Texture dimenision corresponding to the r direction
         void SetFieldSize(unsigned int x,unsigned int y,unsigned int z);
         ///Set the texture repesenting the velocity field
         ///\param velocity osg::Texture3D representing the velocity
         void SetVelocityTexture(osg::Texture3D* velocity);
         ///Set the period for injecting new particles in the field
         ///\param period The time between injections
         void UpdateInjectionPeriod(GLfloat period);
         ///Update the time from the application
         ///\param time The clock time from the application
         void UpdateTime(GLfloat time);
         ///Update a parameter of the noise in the advection algorithm
         ///\param param New value of the parameter
         ///\param whichFunction The parameter TAO_H or TAO_I to update
         void UpdateNoiseFunction(float param, NoiseParam whichFunction);

         ///Update the step size for Euler integration
         ///\param deltaT The step size for integration
         void UpdateDeltaT(float deltaT);

         ///Update the scale of the noise
         ///\param scale The scale of the noise
         void UpdateNoiseScale(float* scale);
         ///Update the size of the texture representing the injection dye
         ///\param scale The scale of the dye
         void UpdateDyeScale(float* scale);
         ///Update the position of the injection dye
         ///\param translation The position of the dye
         void UpdateDyeTranslation(float* translation);
         ///Update the bounds of the data
         ///\param bounds The bounds of the dataset
         void UpdateBounds(float* bounds);
         ///Update the weight of a specified injection parameter
         ///0,1 ==> noise\n
         ///2 ==>dye\n
         ///\param whichMaterial The material to change the weight
         void UpdateWeight(GLfloat* param, int whichMaterial = 0);

         ///Set the state
         ///\param state osg::State
         void SetState(osg::State* state);
         ///Set the center of the volume
         ///\param center The center of the volume
         void SetCenter(osg::Vec3 center);

         ///Get the property texture
         osg::Texture3D* GetPropertyTexture();

         ///Equal operator
         ///\param sm The cfdOSGAdvectionShaderManager to set equal to
         virtual cfdOSGAdvectionShaderManager& operator=(const 
                                     cfdOSGAdvectionShaderManager& sm);
      protected:
         ///Initialize property texture
         void _initPropertyTexture();
         ///Initialize dye texture
         void _initDyeTexture();
         ///Initialize noise texture
         void _initNoiseTexture();
         ///Initialize weight texture
         void _initWeightFunctions();
         ///Initialize transfer texture
         void _initLookUpFunction();
         ///Initialize shader program callbacks for uniforms
         void _initFragProgramCallbacks();
         ///Set up the shader
         void _setupStateSetForGLSL();
         unsigned int _fieldSize[3];///<The texture dimenisions

         bool _isFrag;///<The fragment shader flag.
   
         osg::ref_ptr<cfdUpdateParameterCallback> _noiseScaleCallback;///<Uniform update callback for noise scale
         osg::ref_ptr<cfdUpdateParameterCallback> _deltaCallback;///<Uniform update callback for delta
         osg::ref_ptr<cfdUpdateParameterCallback> _timeCallback;///<Uniform update callback for time
         osg::ref_ptr<cfdUpdateParameterCallback> _periodCallback;///<Uniform update callback for period
         osg::ref_ptr<cfdUpdateParameterCallback> _dyeScaleCallback;///<Uniform update callback for dye size
         osg::ref_ptr<cfdUpdateParameterCallback> _dyeTransCallback;///<Uniform update callback for dye position
         osg::ref_ptr<cfdUpdateParameterCallback> _minBoundsCallback;///<Uniform update callback for minimum bounds
         osg::ref_ptr<cfdUpdateParameterCallback> _maxBoundsCallback;///<Uniform update callback for maximum bounds
         osg::ref_ptr<cfdUpdateParameterCallback> _dyeCoordCallback;///<Uniform update callback for dye coordinate
         osg::ref_ptr<cfdUpdateParameterCallback> _weightWCallback;///<Uniform update callback for weight of material 1
         osg::ref_ptr<cfdUpdateParameterCallback> _weightVCallback;///<Uniform update callback for weight of material 2

         osg::ref_ptr<osg::Texture3D> _velocity;///<Velocity field
         osg::ref_ptr<osg::Texture3D> _propertyToAdvect;///<Property that is being advected by velocity
         float _weightW[4];///<Weight of material 1
         float _weightV[4];///<Weight of material 2
         osg::ref_ptr<osg::Texture3D> _dye;///<Injection dye
         osg::ref_ptr<osg::Texture3D> _noiseTexture;///<Noise texture
         osg::ref_ptr<osg::Texture1D> _lookUpFunction;///<Transfer function
         osg::ref_ptr<cfdUpdateableOSGNoiseTexture3d> _noiseCbk;///<Callback for updating noise
         bool _reinit;///<Flag to re-initialize parameters
         osg::Vec3 _center;///<Center of the data
   };
}
#endif//_OSG
#endif// CFD_OSG_SCALAR_SHADER_MANAGER_H

