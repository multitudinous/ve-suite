#ifndef CFD_OSG_TRANSFER_SHADER_MANAGER_H
#define CFD_OSG_TRANSFER_SHADER_MANAGER_H
#ifdef VE_PATENTED
#ifdef _OSG
namespace osg{
   class Texture3D;
   class Texture1D;
   class TexMat;
};
#include <vector>
namespace VE_TextureBased{
   class cfdTextureManager;
   class cfdUpdateTextureCallback;
}

#include "VE_TextureBased/cfdOSGShaderManager.h"
#include "VE_TextureBased/cfdUpdateableOSGTexture1d.h"


namespace VE_TextureBased{
   class VE_TEXTURE_BASED_EXPORTS cfdOSGTransferShaderManager
      : public cfdOSGShaderManager{
      public:
         cfdOSGTransferShaderManager();
         cfdOSGTransferShaderManager(const cfdOSGTransferShaderManager& sm);
         virtual ~cfdOSGTransferShaderManager();
         void SetUseTextureManagerForProperty(bool trueFalse);
         virtual void Init();
         void SetFieldSize(unsigned int x,unsigned int y,unsigned int z);
         void UpdateTransferFunction(cfdUpdateableOSGTexture1d::TransType type,
                                              float param,int whichFunction);
         void SetPropertyTexture(osg::Texture3D* property);
         void SetTextureMatrix(osg::TexMat* tmat);
         void InitTextureManager(cfdTextureManager* tm);
         void UpdateTextureManager(cfdTextureManager* tm);
   
         osg::Texture3D* GetPropertyTexture();

         virtual cfdOSGTransferShaderManager& operator=(const 
		                               cfdOSGTransferShaderManager& sm);
      protected:
         void _initTransferFunctions();
         void _createTransferFunction(bool gamma = false,
                                   bool clearList = false);
         virtual void _initPropertyTexture();
         void _setupStateSetForGLSL();
         unsigned int _fieldSize[3];
   
         osg::ref_ptr<osg::TexMat> _texMat;
         osg::ref_ptr<osg::Texture3D> _property;
         typedef osg::ref_ptr<osg::Texture1D> TransferFunction ;
         std::vector<TransferFunction> _transferFunctions;
         bool _reinit;
         bool _useTM;
         cfdTextureManager* _tm;
         osg::ref_ptr<cfdUpdateTextureCallback> _utCbk;
   };
}
#endif//_OSG
#endif
#endif// CFD_OSG_SCALAR_SHADER_MANAGER_H
