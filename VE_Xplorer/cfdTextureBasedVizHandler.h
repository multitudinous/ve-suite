#ifndef CFD_TEXTURE_BASED_MODEL_HANDLER_H
#define CFD_TEXTURE_BASED_MODEL_HANDLER_H

class cfdDCS;
class cfdGroup;
class cfdCursor;
class cfdNavigate;
class cfdCommandArray;
class cfdSwitch;
class cfdGraphicsObject;

class cfdTextureManager;
#include <vector>
#ifdef _PERFORMER
#elif _OPENSG
#elif _OSG
class cfdVolumeVisualization;
class cfdTextureBasedVizHandler{
public:
  cfdTextureBasedVizHandler();
   cfdTextureBasedVizHandler(const cfdTextureBasedVizHandler& tbvh);
   ~cfdTextureBasedVizHandler();

   void PreFrameUpdate();
   void SetParameterFile(char* paramFile);
   void SetCommandArray(cfdCommandArray* cmdArray);
   void SetWorldDCS(cfdDCS* dcs);
   void SetParentNode(cfdGroup* parent);
   void SetNavigate(cfdNavigate* navigate);
   void SetCursor(cfdCursor* cursor);
   void SetActiveTextureManager(cfdTextureManager* tm);
   bool InitVolumeVizNodes();
   cfdVolumeVisualization* GetVolumeVizNode(int index);
   cfdVolumeVisualization* GetActiveVolumeVizNode();
   cfdTextureBasedVizHandler& operator=(const cfdTextureBasedVizHandler& tbvh);
protected:	
   char* _paramFile;
   cfdCommandArray* _cmdArray;
   cfdDCS* _worldDCS;
   cfdNavigate* _nav;
   cfdCursor* _cursor;
   cfdTextureManager* _activeTM;
   std::vector<cfdVolumeVisualization> _volumeVisNodes;
   cfdVolumeVisualization* _activeVolumeVizNode;
   cfdGroup* _parent;
   float* _currentBBox;
};
#endif //OSG
#endif// CFD_TEXTURE_BASED_VIZ_HANDLER_H
