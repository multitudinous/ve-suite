#ifndef CFD_OSG_TRANSFER_SHADER_MANAGER_H
#define CFD_OSG_TRANSFER_SHADER_MANAGER_H

#ifdef _OSG
namespace osg{
   class Texture3D;
   class Texture1D;
   class TexMat;
};
#include <vector>
#ifdef CFD_USE_SHADERS
#include "cfdOSGShaderManager.h"
#include "cfdUpdateableOSGTexture1d.h"

class cfdOSGTransferShaderManager: public cfdOSGShaderManager{
public:
   cfdOSGTransferShaderManager();
   cfdOSGTransferShaderManager(const cfdOSGTransferShaderManager& sm);
   virtual ~cfdOSGTransferShaderManager();

   virtual void Init();
   void SetFieldSize(unsigned int x,unsigned int y,unsigned int z);
   void UpdateTransferFunction(cfdUpdateableOSGTexture1d::TransType type,
                                              float param,int whichFunction);
   void SetPropertyTexture(osg::Texture3D* property);
   void SetTextureMatrix(osg::TexMat* tmat);
   osg::Texture3D* GetPropertyTexture();

   virtual cfdOSGTransferShaderManager& operator=(const 
		                               cfdOSGTransferShaderManager& sm);
protected:
   void _initTransferFunctions();
   void _createTransferFunction(bool clearList = true);
   void _initPropertyTexture();
   unsigned int _fieldSize[3];
   osg::ref_ptr<osg::TexMat> _texMat;
   osg::ref_ptr<osg::Texture3D> _property;
   typedef osg::ref_ptr<osg::Texture1D> TransferFunction ;
   std::vector<TransferFunction> _transferFunctions;
   bool _reinit;
};
#endif// _CFD_USE_SHADERS
#endif//_OSG
#endif// CFD_OSG_SCALAR_SHADER_MANAGER_H
