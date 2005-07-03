#ifndef CFD_SCALAR_VOLUME_VIS_HANDLER_H
#define CFD_SCALAR_VOLUME_VIS_HANDLER_H
#ifdef VE_PATENTED
#ifdef _OSG
namespace osg { class Group; }

namespace VE_TextureBased
{
   class cfdScalarShaderManager;
   class cfdTextureManager;
}
#include "VE_TextureBased/cfdVolumeVisNodeHandler.h" 

namespace VE_TextureBased
{
   class VE_TEXTURE_BASED_EXPORTS cfdScalarVolumeVisHandler 
      : public cfdVolumeVisNodeHandler{
      public:
         cfdScalarVolumeVisHandler();
         cfdScalarVolumeVisHandler(const cfdScalarVolumeVisHandler& vvnh);
         virtual ~cfdScalarVolumeVisHandler();
         virtual void Init();
         virtual void SetTextureManager(cfdTextureManager* tm);
         cfdScalarShaderManager* GetScalarShaderManager(){return _transferSM;}
         cfdScalarVolumeVisHandler& operator=(const cfdScalarVolumeVisHandler& vvnh);

      protected:
         virtual void _setUpDecorator();
         virtual void _applyTextureMatrix();
         void _createTransferShader();
         cfdScalarShaderManager* _transferSM;
   };
}
#endif //_OSG
#endif
#endif// CFD_SCALAR_VOLUME_VIS_HANDLER_H
