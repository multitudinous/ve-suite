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
namespace osgUtil{
   class SceneView;
};
class cfdVolumeVisualization;
class cfdTextureBasedVizHandler
{
   public:
      cfdTextureBasedVizHandler( void );
      cfdTextureBasedVizHandler(const cfdTextureBasedVizHandler& tbvh);
      ~cfdTextureBasedVizHandler( void );

      void PreFrameUpdate( void );
      void SetParameterFile(char* paramFile);
      void SetCommandArray(cfdCommandArray* cmdArray);
      void SetWorldDCS(cfdDCS* dcs);
      void SetParentNode(cfdGroup* parent);
      void SetNavigate(cfdNavigate* navigate);
      void SetCursor(cfdCursor* cursor);
      void SetActiveTextureManager(cfdTextureManager* tm);
      //once we get pf side this may need to be ifdef'd
      void SetSceneView(osgUtil::SceneView* sv);

      bool InitVolumeVizNodes( void );
      cfdVolumeVisualization* GetVolumeVizNode(int index);
      cfdVolumeVisualization* GetActiveVolumeVizNode( void );
      cfdTextureBasedVizHandler& operator=(const cfdTextureBasedVizHandler& tbvh);

   protected:	
      char* _paramFile;
      cfdCommandArray* _cmdArray;
      cfdDCS* _worldDCS;
      cfdNavigate* _nav;
      cfdCursor* _cursor;
      cfdTextureManager* _activeTM;
      std::vector<cfdVolumeVisualization*> _volumeVisNodes;
      cfdVolumeVisualization* _activeVolumeVizNode;
      cfdGroup* _parent;
      osgUtil::SceneView* _sceneView;
      float* _currentBBox;
      bool _cleared;
};
#endif //OSG
#endif// CFD_TEXTURE_BASED_VIZ_HANDLER_H
