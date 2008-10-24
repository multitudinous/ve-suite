/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/
#ifndef VE_XPLORER_TEXTURE_BASED_MODEL_HANDLER_H
#define VE_XPLORER_TEXTURE_BASED_MODEL_HANDLER_H

#include <ves/xplorer/TextureBasedVizHandlerPtr.h>
#include <ves/VEConfig.h>


#include <ves/xplorer/event/volume/TextureBasedEventHandler.h>

#include <ves/xplorer/scenegraph/DCS.h>
#include <ves/xplorer/scenegraph/Group.h>


#include <vpr/Util/Singleton.h>

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

namespace ves
{
namespace xplorer
{
class cfdGraphicsObject;
}
}

#include <vector>
#include <map>
#ifdef _PERFORMER
#elif _OPENSG
#elif _OSG
#include <osg/ref_ptr>

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
}
}
}

//namespace osgUtil { class SceneView; }

namespace ves
{
namespace xplorer
{
/*!\file TextureBasedVizHandler.h
TextureBasedVizHandler API
*/
/*!\class ves::xplorer::TextureBasedVizHandler
*
*/
class VE_XPLORER_EXPORTS TextureBasedVizHandler
{
public:
    void PreFrameUpdate( void );
    ///Update the frame number of the transient animation
    void UpdateTransientFrame();
    void SetWorldDCS( ves::xplorer::scenegraph::DCS* dcs );
    void SetParentNode( ves::xplorer::scenegraph::Group* parent );
    void SetActiveTextureDataSet( ves::xplorer::volume::cfdTextureDataSet* tdset );

    ///Update the active cfdTextureManager by pinging the cfdTextureDataSet
    void UpdateActiveTextureManager();

    ///Update the bounding box
    void UpdateBoundingBox( bool value );

    ///Update the clip plane position
    ///\param planeCoordinate The "X","Y","Z" specification for a plane.
    ///\param planeDirection The direction "Negative" or "Positive"
    ///\param alpha The plane position.
    void UpdateClipPlane( std::string planeCoordinate,
                          std::string planeDirection,
                          double alpha );


    ///Update the scalar range
    ///\param range The new scalar range
    void UpdateScalarRange( float* range );

    ///Activate/deactivate isosurface visualization.
    ///\param onOff Turn it off or on.
    void EnsureIsosurface( bool onOff );

    ///Activate/deactivate phong shading.
    ///\param onOff Turn it off or on.
    void EnsurePhongShading( bool onOff );

    ///Activate the isosurface
    ///\param value The new isosurface value.
    void UpdateIsosurface( double value );

    ///Set the number of Slice planes per brick
    ///\param nSlices The number of slices per brick
    void UpdateNumberOfSlicePlanes( unsigned int nSlices );

    ///Flag how the PreIntegration Table is update.\n true == full update\nfalse==fast update\n
    void UpdatePreIntegrationTable( bool trueFalse );

    ///Clear the texture-based visualization
    void ClearAll();

    ///Set the active shader mananger
    ///\param The name of the shader mananger to set active
    void SetActiveShaderManager( std::string name );

    ///Set the current frame
    ///\param frame The current step in the transient visualization.
    void SetCurrentFrame( unsigned int frame );

    ///Step the current visualization
    ///\param direction The direction to step
    void StepTransientVisualization( std::string direction );

    ///Update the duration of the current visualization
    ///\param duration The duration
    void UpdateTransientDuration( double duration );

    ///Play the transient visualization
    void PlayTransientVisualization();

    ///Stop the transient visualization
    void StopTransientVisualization();

    ///Set the direction of the visualization
    void SetTransientDirection( std::string direction );

    void SetCurrentTime( double time );

    //Set the master node
    ///\param isMaster Flag depicting the master node
    void SetMasterNode( bool isMaster );

    void ViewTextureBasedVis( bool trueFalse );
    //once we get pf side this may need to be ifdef'd
    //void SetSceneView(osgUtil::SceneView* sv);
    void SetPBuffer( ves::xplorer::volume::cfdPBufferManager* pbm );
    void PingPongTextures();
    ves::xplorer::volume::cfdPBufferManager* GetPBuffer();
    //bool InitVolumeVizNodes( void );
    ves::xplorer::volume::cfdVolumeVisualization* GetVolumeVizNode( int index );
    ves::xplorer::volume::cfdVolumeVisualization* GetActiveVolumeVizNode( void );

    ///The active texture manager
    ves::xplorer::volume::cfdTextureManager* GetActiveTextureManager();

protected:
    void _updateShaderState();
    void _updateGraph();
    void _updateVisualization();
    void _updateShaders();
    void _updateScalarVisHandler();
    void _updateVectorVisHandler();

    double _appTime;
    double _animationDelay;
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > _worldDCS;
    ves::xplorer::volume::cfdTextureDataSet* _activeTDSet;
    ves::xplorer::volume::cfdTextureManager* _activeTM;

    //std::vector<cfdVolumeVisualization*> _volumeVisNodes;
    ves::xplorer::volume::cfdVolumeVisualization* _activeVolumeVizNode;
    osg::ref_ptr< ves::xplorer::scenegraph::Group > _parent;
    ves::xplorer::volume::cfdPBufferManager* _pbm;
    //osgUtil::SceneView* _sceneView;
    ves::xplorer::volume::cfdVolumeVisNodeHandler* _activeVisNodeHdlr;
    ves::xplorer::volume::cfdScalarVolumeVisHandler* _svvh;
    ves::xplorer::volume::cfdVectorVolumeVisHandler* _vvvh;
    bool m_isMaster;///<Flag defining the master node
    //osg::ref_ptr< ves::xplorer::scenegraph::Switch > _visOptionSwitch;
    float* _currentBBox;
    bool _cleared;
    bool _textureBaseSelected;

    std::map<std::string, ves::xplorer::event::TextureBasedEventHandler* > _eventHandlers;///<Map of event handlers for texture-based vis

private:
    // Required so that vpr::Singleton can instantiate this class.
    //friend class vpr::Singleton< TextureBasedVizHandler >;
    TextureBasedVizHandler( void );

    ~TextureBasedVizHandler( void );
    vprSingletonHeader( TextureBasedVizHandler );
};
}
}
#endif //OSG
#endif// CFD_TEXTURE_BASED_VIZ_HANDLER_H
