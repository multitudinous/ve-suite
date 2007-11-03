/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
/*!\class ves::xplorer::volume::cfdTextureBasedVizHandler
* 
*/
#include <vpr/Util/Singleton.h>
#include <ves/VEConfig.h>

#include <ves/xplorer/scenegraph/DCS.h>
#include <ves/xplorer/scenegraph/Group.h>

#include <ves/xplorer/event/volume/TextureBasedEventHandler.h>

#include <string>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
    class DCS;
    class Group;
}
}
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

namespace ves
{
namespace xplorer
{
namespace volume
{
class cfdTextureManager;
class cfdPBufferManager;
class cfdVolumeVisualization;
class cfdTextureDataSet;
class cfdVolumeVisNodeHandler;
class cfdScalarVolumeVisHandler;
class cfdVectorVolumeVisHandler;
class VE_XPLORER_EXPORTS cfdTextureBasedVizHandler //: public vpr::Singleton< cfdTextureBasedVizHandler >
   {
      public:
         void PreFrameUpdate( void );
         ///Update the frame number of the transient animation
         void UpdateTransientFrame();
         //void CleanUp( void );
         void SetParameterFile(std::string paramFile);
         void SetCommandArray( VE_Xplorer::cfdCommandArray* cmdArray);
         void SetWorldDCS( ves::xplorer::scenegraph::DCS* dcs);
         void SetParentNode( ves::xplorer::scenegraph::Group* parent);
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

		 ///Flag how the PreIntegration Table is update.\n true == full update\nfalse==fast update\n
		 void UpdatePreIntegrationTable(bool trueFalse);
         
		 ///Clear the texture-based visualization
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
        
         //Set the master node
         ///\param isMaster Flag depicting the master node
         void SetMasterNode( bool isMaster );
        
         void ViewTextureBasedVis(bool trueFalse);
         //once we get pf side this may need to be ifdef'd
         //void SetSceneView(osgUtil::SceneView* sv); 
         void SetPBuffer(cfdPBufferManager* pbm);
         void PingPongTextures();
         cfdPBufferManager* GetPBuffer();
         //bool InitVolumeVizNodes( void );
         cfdVolumeVisualization* GetVolumeVizNode(int index);
         cfdVolumeVisualization* GetActiveVolumeVizNode( void );
         
         ///The active texture manager
         cfdTextureManager* GetActiveTextureManager();
  
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
         osg::ref_ptr< ves::xplorer::scenegraph::DCS > _worldDCS;
         cfdTextureDataSet* _activeTDSet;
         cfdTextureManager* _activeTM;

			//std::vector<cfdVolumeVisualization*> _volumeVisNodes;
         cfdVolumeVisualization* _activeVolumeVizNode;
			osg::ref_ptr< ves::xplorer::scenegraph::Group > _parent;
         cfdPBufferManager* _pbm;
         //osgUtil::SceneView* _sceneView;
         cfdVolumeVisNodeHandler* _activeVisNodeHdlr;
         cfdScalarVolumeVisHandler* _svvh;
         cfdVectorVolumeVisHandler* _vvvh;
         bool m_isMaster;///<Flag defining the master node
      //osg::ref_ptr< ves::xplorer::scenegraph::Switch > _visOptionSwitch;
         float* _currentBBox;
         bool _cleared;
         bool _textureBaseSelected;

         std::map<std::string,ves::xplorer::event::TextureBasedEventHandler* > _eventHandlers;///<Map of event handlers for texture-based vis

      private:
         // Required so that vpr::Singleton can instantiate this class.
         //friend class vpr::Singleton< cfdTextureBasedVizHandler >;
         cfdTextureBasedVizHandler( void );
  
         ~cfdTextureBasedVizHandler( void );
         vprSingletonHeader( cfdTextureBasedVizHandler );   
   };
}
}
}
#endif //OSG
#endif// CFD_TEXTURE_BASED_VIZ_HANDLER_H
