#ifndef CFD_OSG_SCALAR_SHADER_MANAGER_H
#define CFD_OSG_SCALAR_SHADER_MANAGER_H

#ifdef _OSG
namespace osg{
   class Texture3D;
};
class cfdTextureManager;
class cfdUpdateTextureCallback;

#ifdef CFD_USE_SHADERS
#include "cfdOSGShaderManager.h"

class cfdOSGScalarShaderManager: public cfdOSGShaderManager{
public:
   cfdOSGScalarShaderManager();
   cfdOSGScalarShaderManager(const cfdOSGScalarShaderManager& sm);
   virtual ~cfdOSGScalarShaderManager();

   virtual void Init();
   void InitTextureManager(cfdTextureManager* tm);
   void UpdateTextureManager(cfdTextureManager* tm);

   virtual cfdOSGScalarShaderManager& operator=(const 
		                               cfdOSGScalarShaderManager& sm);
protected:
   cfdTextureManager* _tm;
   cfdUpdateTextureCallback* _utCbk;
   osg::ref_ptr<osg::Texture3D> _scalarProp;
   osg::ref_ptr<osg::Image> _image;
   bool _reinit;
};
#endif// _CFD_USE_SHADERS
#endif//_OSG
#endif// CFD_OSG_SCALAR_SHADER_MANAGER_H
