#ifndef CFD_TEXTURE_BASED_MODEL_HANDLER_H
#define CFD_TEXTURE_BASED_MODEL_HANDLER_H
#ifdef VE_PATENTED
#include <vpr/Util/Singleton.h>
#include "VE_Installer/include/VEConfig.h"

namespace VE_SceneGraph
{
   class cfdDCS;
   class cfdGroup;
   class cfdSwitch;
}

namespace VE_Xplorer
{
   class cfdCursor;
   class cfdNavigate;
   class cfdCommandArray;
   class cfdGraphicsObject;
}

#include <vector>
#ifdef _PERFORMER
#elif _OPENSG
#elif _OSG 
namespace osgUtil { class SceneView; }

namespace VE_TextureBased
{
   class cfdTextureManager;
   class cfdPBufferManager;
   class cfdVolumeVisualization;
   class cfdTextureDataSet;
   class cfdVolumeVisNodeHandler;
   class cfdScalarVolumeVisHandler;
   class cfdVectorVolumeVisHandler;
}

namespace VE_TextureBased
{
   class VE_XPLORER_EXPORTS cfdTextureBasedVizHandler //: public vpr::Singleton< cfdTextureBasedVizHandler >
   {
      public:
         void PreFrameUpdate( void );
         void CleanUp( void );
         void SetParameterFile(char* paramFile);
         void SetCommandArray( VE_Xplorer::cfdCommandArray* cmdArray);
         void SetWorldDCS( VE_SceneGraph::cfdDCS* dcs);
         void SetParentNode( VE_SceneGraph::cfdGroup* parent);
         void SetNavigate( VE_Xplorer::cfdNavigate* navigate);
         void SetCursor( VE_Xplorer::cfdCursor* cursor);
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
         VE_Xplorer::cfdCommandArray* _cmdArray;
         VE_SceneGraph::cfdDCS* _worldDCS;
         VE_Xplorer::cfdNavigate* _nav;
         VE_Xplorer::cfdCursor* _cursor;
         cfdTextureDataSet* _activeTDSet;
         cfdTextureManager* _activeTM;

      //std::vector<cfdVolumeVisualization*> _volumeVisNodes;
         cfdVolumeVisualization* _activeVolumeVizNode;
         VE_SceneGraph::cfdGroup* _parent;
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
}
#endif //OSG
#endif //
#endif// CFD_TEXTURE_BASED_VIZ_HANDLER_H
