#ifndef CFD_TEXTURE_BASED_MODEL_HANDLER_H
#define CFD_TEXTURE_BASED_MODEL_HANDLER_H
#ifdef VE_PATENTED
#include <vpr/Util/Singleton.h>

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
namespace osgUtil { class SceneView; }
class cfdPBufferManager;
class cfdVolumeVisualization;

class cfdTextureDataSet;

class cfdVolumeVisNodeHandler;
class cfdScalarVolumeVisHandler;
class cfdVectorVolumeVisHandler;
#include "VE_Xplorer/cfdConfig.h"

class WXPLUGIN_DECLSPEC cfdTextureBasedVizHandler //: public vpr::Singleton< cfdTextureBasedVizHandler >
{
   public:
   
      void PreFrameUpdate( void );
      void CleanUp( void );
      void SetParameterFile(char* paramFile);
      void SetCommandArray(cfdCommandArray* cmdArray);
      void SetWorldDCS(cfdDCS* dcs);
      void SetParentNode(cfdGroup* parent);
      void SetNavigate(cfdNavigate* navigate);
      void SetCursor(cfdCursor* cursor);
      void SetActiveTextureDataSet(cfdTextureDataSet* tdset);
  
      void ViewTextureBasedVis(bool trueFalse);
      //once we get pf side this may need to be ifdef'd
      //void SetSceneView(osgUtil::SceneView* sv); 
      void SetPBuffer(cfdPBufferManager* pbm);
      void PingPongTextures();
      cfdPBufferManager* GetPBuffer();
      //bool InitVolumeVizNodes( void );
      cfdVolumeVisualization* GetVolumeVizNode(int index);
      cfdVolumeVisualization* GetActiveVolumeVizNode( void );
  
   protected:
      void _updateShaderState();
      void _updateGraph();
      void _updateVisualization();
      void _updateShaders();
      void _updateScalarVisHandler();
      void _updateVectorVisHandler();

      char* _paramFile;
      cfdCommandArray* _cmdArray;
      cfdDCS* _worldDCS;
      cfdNavigate* _nav;
      cfdCursor* _cursor;
      cfdTextureDataSet* _activeTDSet;
      cfdTextureManager* _activeTM;

      //std::vector<cfdVolumeVisualization*> _volumeVisNodes;
      cfdVolumeVisualization* _activeVolumeVizNode;
      cfdGroup* _parent;
      cfdPBufferManager* _pbm;
      osgUtil::SceneView* _sceneView;
      cfdVolumeVisNodeHandler* activeVisNodeHdlr;
      cfdScalarVolumeVisHandler* _svvh;
      cfdVectorVolumeVisHandler* _vvvh;
      
      //cfdSwitch* _visOptionSwitch;
      float* _currentBBox;
      bool _cleared;
      bool _textureBaseSelected;
   private:
      // Required so that vpr::Singleton can instantiate this class.
      //friend class vpr::Singleton< cfdTextureBasedVizHandler >;
      cfdTextureBasedVizHandler( void );
  
      ~cfdTextureBasedVizHandler( void ){ ; }// Never gets called, don't implement
      vprSingletonHeader( cfdTextureBasedVizHandler );   
};
#endif //OSG
#endif //
#endif// CFD_TEXTURE_BASED_VIZ_HANDLER_H
