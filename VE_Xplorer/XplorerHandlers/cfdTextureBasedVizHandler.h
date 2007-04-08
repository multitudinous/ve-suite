/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_TEXTURE_BASED_MODEL_HANDLER_H
#define CFD_TEXTURE_BASED_MODEL_HANDLER_H
/*!\file cfdTextureBasedVizHandler.h
cfdTextureBasedVizHandler API
*/
/*!\class VE_TextureBased::cfdTextureBasedVizHandler
* 
*/
#ifdef VE_PATENTED
#include <vpr/Util/Singleton.h>
#include "VE_Installer/include/VEConfig.h"

#include "VE_Xplorer/SceneGraph/DCS.h"
#include "VE_Xplorer/SceneGraph/Group.h"

#include "VE_Xplorer/XplorerHandlers/TextureBasedEventHandler.h"

#include <string>
namespace VE_SceneGraph
{
   class DCS;
	class Group;
}

namespace VE_Xplorer
{
   class cfdCommandArray;
   class cfdGraphicsObject;
}

#include <vector>
#include <map>
#ifdef _PERFORMER
#elif _OPENSG
#elif _OSG 
#include <osg/ref_ptr>

//namespace osgUtil { class SceneView; }

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
         void SetParameterFile(std::string paramFile);
         void SetCommandArray( VE_Xplorer::cfdCommandArray* cmdArray);
         void SetWorldDCS( VE_SceneGraph::DCS* dcs);
         void SetParentNode( VE_SceneGraph::Group* parent);
         void SetActiveTextureDataSet(cfdTextureDataSet* tdset);

         ///Update the active cfdTextureManager by pinging the cfdTextureDataSet
         void UpdateActiveTextureManager();

         ///Update the bounding box
         void UpdateBoundingBox(bool value);

         ///Update the clip plane position
         ///\param planeCoordinate The "X","Y","Z" specification for a plane.
         ///\param planeDirection The direction "Negative" or "Positive"
         ///\param alpha The plane position.
         void UpdateClipPlane(std::string planeCoordinate,
                       std::string planeDirection,
                       double alpha);

         ///Update the scalar range
         ///\param range The new scalar range
         void UpdateScalarRange(float* range);

         ///Activate/deactivate isosurface visualization.
         ///\param onOff Turn it off or on.
         void EnsureIsosurface(bool onOff);

         ///Activate/deactivate phong shading.
         ///\param onOff Turn it off or on.
         void EnsurePhongShading(bool onOff);

         ///Activate the isosurface
         ///\param value The new isosurface value.
         void UpdateIsosurface(double value);

		 ///Set the number of Slice planes per brick
		 ///\param nSlices The number of slices per brick
		 void UpdateNumberOfSlicePlanes(unsigned int nSlices);
         ///Clear the texture-based visualize
         void ClearAll();

		 ///Set the active shader mananger
		 ///\param The name of the shader mananger to set active
		 void SetActiveShaderManager(std::string name);

         ///Set the current frame
         ///\param frame The current step in the transient visualization.
         void SetCurrentFrame(unsigned int frame);

         ///Step the current visualization
         ///\param direction The direction to step
         void StepTransientVisualization(std::string direction);

         ///Update the duration of the current visualization
         ///\param duration The duration
         void UpdateTransientDuration(double duration);

         ///Play the transient visualization
         void PlayTransientVisualization();

         ///Stop the transient visualization
         void StopTransientVisualization();

         ///Set the direction of the visualization
         void SetTransientDirection(std::string direction);

         void SetCurrentTime(double time);
  
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

         double _appTime;
         double _animationDelay;
         std::string _paramFile;
         VE_Xplorer::cfdCommandArray* _cmdArray;
         osg::ref_ptr< VE_SceneGraph::DCS > _worldDCS;
         cfdTextureDataSet* _activeTDSet;
         cfdTextureManager* _activeTM;

			//std::vector<cfdVolumeVisualization*> _volumeVisNodes;
         cfdVolumeVisualization* _activeVolumeVizNode;
			osg::ref_ptr< VE_SceneGraph::Group > _parent;
         cfdPBufferManager* _pbm;
         //osgUtil::SceneView* _sceneView;
         cfdVolumeVisNodeHandler* _activeVisNodeHdlr;
         cfdScalarVolumeVisHandler* _svvh;
         cfdVectorVolumeVisHandler* _vvvh;
      
      //osg::ref_ptr< VE_SceneGraph::Switch > _visOptionSwitch;
         float* _currentBBox;
         bool _cleared;
         bool _textureBaseSelected;

         std::map<std::string,VE_EVENTS::TextureBasedEventHandler* > _eventHandlers;///<Map of event handlers for texture-based vis

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
