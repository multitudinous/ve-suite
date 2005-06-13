#ifndef CFD_OSG_ADVECTION_SHADER_MANAGER_H
#define CFD_OSG_ADVECTION_SHADER_MANAGER_H
#ifdef VE_PATENTED
#ifdef _OSG
namespace osg{
   class Texture3D;
   class Texture1D;
   class State;
};
#include <vector>
#include "cfdOSGShaderManager.h"
#include "cfdUpdateableOSGNoiseTexture3d.h"
#include "cfdUpdateParameterCallback.h"
class cfdUpdateMatrixParameterCallback;

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
   void UpdateBounds(float* bounds);
   //whichMaterial
   //0,1 ==> noise
   //2 ==>dye
   void UpdateWeight(GLfloat* param, int whichMaterial = 0);
   void SetState(osg::State* state);
   void SetCenter(osg::Vec3 center);

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
   void _setupStateSetForGLSL();
   unsigned int _fieldSize[3];

   bool _isFrag;
   
   cfdUpdateParameterCallback* _noiseScaleCallback;
   cfdUpdateParameterCallback* _deltaCallback;
   cfdUpdateParameterCallback* _timeCallback;
   cfdUpdateParameterCallback* _periodCallback;
   cfdUpdateParameterCallback* _dyeScaleCallback;
   cfdUpdateParameterCallback* _dyeTransCallback;
   cfdUpdateParameterCallback* _minBoundsCallback;
   cfdUpdateParameterCallback* _maxBoundsCallback;
   cfdUpdateParameterCallback* _dyeCoordCallback;
   cfdUpdateParameterCallback* _weightWCallback;
   cfdUpdateParameterCallback* _weightVCallback;

   osg::ref_ptr<osg::Texture3D> _velocity;
   osg::ref_ptr<osg::Texture3D> _propertyToAdvect;
   float _weightW[4];
   float _weightV[4];
   osg::ref_ptr<osg::Texture3D> _dye;
   osg::ref_ptr<osg::Texture3D> _noiseTexture;
   osg::ref_ptr<osg::Texture1D> _lookUpFunction;
   cfdUpdateableOSGNoiseTexture3d* _noiseCbk;
   bool _reinit;
   osg::Vec3 _center;
};

#endif//_OSG
#endif
#endif// CFD_OSG_SCALAR_SHADER_MANAGER_H

