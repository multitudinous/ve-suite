#ifndef CFD_OSG_ADVECTION_SHADER_MANAGER_H
#define CFD_OSG_ADVECTION_SHADER_MANAGER_H

#ifdef _OSG
namespace osg{
   class Texture3D;
   class Texture1D;
   class State;
};
#include <vector>
#ifdef CFD_USE_SHADERS
#include "cfdOSGShaderManager.h"
#include "cfdUpdateableOSGNoiseTexture3d.h"
#include "cfdUpdateParameterCallback.h"

class cfdOSGAdvectionShaderManager: public cfdOSGShaderManager{
public:
   cfdOSGAdvectionShaderManager();
   cfdOSGAdvectionShaderManager(const cfdOSGAdvectionShaderManager& sm);
   virtual ~cfdOSGAdvectionShaderManager();

   enum NoiseParam{TAO_H,TAO_I};

   virtual void Init();
   void SetFieldSize(unsigned int x,unsigned int y,unsigned int z);
   void SetVelocityTexture(osg::Texture3D* velocity);
   void UpdateInjectionPeriod(GLfloat period);
   void UpdateTime(GLfloat time);
   void UpdateNoiseFunction(float param, NoiseParam whichFunction);
   void UpdateDeltaT(float deltaT);
   void UpdateNoiseScale(float* scale);
   void UpdateDyeScale(float* scale);
   void UpdateDyeTranslation(float* translation);
   //whichMaterial
   //0,1 ==> noise
   //2 ==>dye
   void UpdateWeight(GLfloat param, int whichMaterial = 0);
   void SetState(osg::State* state);

   //need to add the dye interface also
   osg::Texture3D* GetPropertyTexture();

   virtual cfdOSGAdvectionShaderManager& operator=(const 
                                     cfdOSGAdvectionShaderManager& sm);
protected:
   void _initPropertyTexture();
   void _initDyeTexture();
   void _initNoiseTexture();
   void _initWeightFunctions();
   void _initLookUpFunction();
   void _initFragProgramCallbacks();
   virtual void _setupCGShaderProgram(osg::StateSet* ss,
		                      char* progName, char* funcName);
   unsigned int _fieldSize[3];
   /*float _dyeScale[3];
   float _dyeTranslation[3];
   float _noiseScale[3];
   float _period;
   float _time;
   float _deltaT;
*/
   cfdUpdateParameterCallback* _noiseScaleCallback;
   cfdUpdateParameterCallback* _deltaCallback;
   cfdUpdateParameterCallback* _timeCallback;
   cfdUpdateParameterCallback* _periodCallback;
   cfdUpdateParameterCallback* _dyeScaleCallback;
   cfdUpdateParameterCallback* _dyeTransCallback;

   osg::ref_ptr<osg::Texture3D> _velocity;
   osg::ref_ptr<osg::Texture3D> _propertyToAdvect;
   osg::ref_ptr<osg::Texture3D> _weightW;
   osg::ref_ptr<osg::Texture3D> _weightV;
   osg::ref_ptr<osg::Texture3D> _dye;
   osg::ref_ptr<osg::Texture1D> _lookUpFunction;
   cfdUpdateableOSGNoiseTexture3d* _noise;
   osg::ref_ptr<osg::State> _state;
   bool _reinit;
};
#endif// _CFD_USE_SHADERS
#endif//_OSG
#endif// CFD_OSG_SCALAR_SHADER_MANAGER_H
