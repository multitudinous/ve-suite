#ifndef CFD_TEXTURE_BASED_MODEL_HANDLER_H
#define CFD_TEXTURE_BASED_MODEL_HANDLER_H

class cfdDCS;
class cfdCursor;
class cfdNavigate;
class cfdCommandArray;
class cfdSwitch;
#include "cfdTextureBasedModel.h"
#ifdef _PERFORMER
#elif _OPENSG
#elif _OSG
class cfdTextureBasedModelHandler{
public:
   cfdTextureBasedModelHandler();
   cfdTextureBasedModelHandler(const cfdTextureBasedModelHandler& tbvh);
   ~cfdTextureBasedModelHandler();

   void InitScene();
   void PreFrameUpdate();
   void SetParameterFile(char* paramFile);
   void SetCommandArray(cfdCommandArray* cmdArray);
   void SetWorldDCS(cfdDCS* dcs);
   void SetNavigate(cfdNavigate* navigate);
   void SetCursor(cfdCursor* cursor);

   cfd3DTextureBasedModel* GetActiveTextureBasedModel();
   cfd3DTextureBasedModel* GetTextureBasedModel(int whichModel);

   bool InitTextureBasedModelsModels();
   cfdTextureBasedModelHandler& operator=(const cfdTextureBasedModelHandler& tbvh);
protected:	
   char* _paramFile;
   cfdCommandArray* _cmdArray;
   cfdDCS* _worldDCS;
   cfdNavigate* _nav;
   cfdCursor* _cursor;
   cfd3DTextureBasedModel* _activeTextureModel;
   std::vector<cfd3DTextureBasedModel> _textureModels;
};
#endif //OSG
#endif// CFD_TEXTURE_BASED_VIZ_HANDLER_H
