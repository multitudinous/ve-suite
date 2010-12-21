/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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

#ifndef VE_XPLORER_ENVIRONMENTHANDLER_H
#define VE_XPLORER_ENVIRONMENTHANDLER_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/EnvironmentHandlerPtr.h>

#include <ves/xplorer/scenegraph/DCS.h>

#include <ves/xplorer/event/EventHandlerPtr.h>

#include <ves/xplorer/event/data/SeedPoints.h>

// --- VTK Includes --- //
class vtkPolyData;

// --- VR Juggler Includes --- //
#include <vpr/Util/Singleton.h>
#include <vrj/vrjParam.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>

#include <osgEphemeris/EphemerisModel.h>

// --- C/C++ Includes --- //
#include <map>
#include <vector>

namespace ves
{
namespace xplorer
{

class cfdTeacher;
class cfdQuatCamHandler;
class cfdDisplaySettings;
class SeedPoints;

namespace scenegraph
{
class HeadsUpDisplay;
}

namespace device
{
class cfdCursor;
}

/*!\file EnvironmentHandler.h
 * EnvironmentHandler API
 */

/*!\class ves::xplorer::EnvironmentHandler
 *
 */
class VE_XPLORER_EXPORTS EnvironmentHandler
{
private:
    // Required so that vpr::Singleton can instantiate this class.
    //friend class vpr::Singleton< EnvironmentHandler >;
    //EnvironmentHandler(const EnvironmentHandler& o) { ; }
    //EnvironmentHandler& operator=(const EnvironmentHandler& o) { ; }

    ///Constructor
    EnvironmentHandler();

    ///Destructor
    ~EnvironmentHandler();
    vprSingletonHeader( EnvironmentHandler );

public:
    ///Initialize environment.
    void Initialize();

    ///Initialize scene.
    void InitScene();

    ///Pre frame update.
    void PreFrameUpdate();

    ///Late pre-frame update
    void LatePreFrameUpdate();

    ///Accessor for cfdCursor
    ves::xplorer::device::cfdCursor* GetCursor();

    ///Accessor for cfdTeacher
    cfdTeacher* GetTeacher();

    ///Accessor for cfdDisplaySettings
    cfdDisplaySettings* GetDisplaySettings();
    ///Accessor to set desktop size information for
    /// runtime reconfiguration of desktop windows
    ///\param width The desktop width
    ///\param height The desktop height
    void SetDesktopSize( int width, int height );
    ///Accessor to get desktop size information
    ///\param width The desktop width
    ///\param height The desktop height
    void GetDesktopSize( int &width, int &height );

    ///Set the window dimension
    ///\param width The window width
    ///\param height The window height
    void SetWindowDimensions( unsigned int width, unsigned int height );

    ///Set up the frustum values
    ///\param _left
    ///\param _right
    ///\param _top
    ///\param _bottom
    ///\param _near
    ///\param _far
    void SetFrustumValues( float _left, float _right, float _bottom, float _top, float _near, float _far );

    ///Get the frustum values
    void GetFrustumValues( std::vector<float>& values );

    ///Get the window width
    unsigned int GetWindowWidth();

    ///Get the window height
    unsigned int GetWindowHeight();

    ///Set the frame rate
    void SetFrameRate( float value );

    ///Get the frame rate
    float GetFrameRate();

    ///Post frame update (look this up)
    void PostFrameUpdate();
    ///Accessor for HeadsUpDisplay
    ves::xplorer::scenegraph::HeadsUpDisplay* GetHeadsUpDisplay();

    ///Activate geometry picking functionality.
    void ActivateGeometryPicking();

    ///Deactivate geoometry picking functionality.
    void DeactivateGeometryPicking();

    ///Set the globalLOD scale
    ///\param lodScale The scale to set 
    void SetGlobalLODScale( double lodScale );

    ///Get the globalLOD scale
    double GetGlobalLODScale();

    ///Get the seed points drawable
    ///\return Get the seed points
    SeedPoints* GetSeedPoints();

    ///Get the seed points drawable
    ///\return Return the DCS for the seed points
    ves::xplorer::scenegraph::DCS* GetSeedPointsDCS();

    ///Get the ephemeris data
    ///\param createIfDoesNotExist Force creation of EphemerisModel if it doesn't exist
    osgEphemeris::EphemerisModel* GetEphemerisModel( bool createIfDoesNotExist = false );

private:
    osg::ref_ptr<ves::xplorer::SeedPoints> _seedPoints;///<The seed points for this dataset
    osg::ref_ptr<ves::xplorer::scenegraph::DCS> _seedPointsDCS;///<The DCS for the seed points
    cfdTeacher* _teacher;///<Handle teacher functionality.
    cfdQuatCamHandler* _camHandler;///<Handle quat cam functionality.

    ves::xplorer::scenegraph::HeadsUpDisplay* mHeadsUpDisplay;///<???

    ///Flag for active geometry picking.
    bool _activeGeomPicking;

    ves::xplorer::device::cfdCursor* cursor;///<The cursor.

    // cur_box will eventually be used to define bounding box
    // for data interagation
    double cur_box[6];///<???
    vtkPolyData * arrow;///<???
    float worldScale[ 3 ];///<World scale.
    float worldTrans[ 3 ];///<World translation
    float worldRot[ 3 ];///<World rotation.

    ///<The class used to change juggler configuration settings during runtime
    cfdDisplaySettings* displaySettings;

    int desktopWidth;///<Desktop width.
    int desktopHeight;///<Desktop height.

    int _windowWidth;///<Window width.
    int _windowHeight;///<Window height.

    float _frustumLeft;///<Left frustum.
    float _frustumRight;///<Right frustum.
    float _frustumTop;///<Top frustum.
    float _frustumBottom;///<Bottom frustum.
    float _frustumNear;///<Near frustum.
    float _frustumFar;///<Far frustum.

    double m_lodScale;///<Global geometry LOD scale
    float framerate;///<Frame rate.
    std::map< std::string, ves::xplorer::event::EventHandler* > _eventHandlers;///<The event handler for commands.

    osg::ref_ptr<osgEphemeris::EphemerisModel> m_ephemerisModel;///<The model containing ephemeris data
};
}
}
#endif //CFD_ENVIRONMENTHANDLER_H
