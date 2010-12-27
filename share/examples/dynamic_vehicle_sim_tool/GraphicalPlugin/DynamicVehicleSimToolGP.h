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
#ifndef DYNAMIC_VEHICLE_TOOL_GP_H
#define DYNAMIC_VEHICLE_TOOL_GP_H

// --- VE-Suite Includes --- //
#include <ves/xplorer/plugin/PluginBase.h>
#include <ves/open/xml/DataValuePairPtr.h>
#include <ves/open/xml/CommandPtr.h>

#include <map>
#include <vector>
#include <utility>
#include <string>

#include <vpr/Thread/Thread.h>
#include <vpr/Sync/Mutex.h>

#include <gmtl/Matrix.h>

namespace ves
{
namespace xplorer
{
namespace device
{
    class KeyboardMouse;
}
namespace scenegraph
{
    class CADEntity;
    class TextTexture;
    class GroupedTextTextures;
}
}
}
namespace warrantytool
{
class VE_USER_PLUGIN_EXPORTS DynamicVehicleSimToolGP :
    public ves::xplorer::plugin::PluginBase
{
public:
    DynamicVehicleSimToolGP();
    virtual ~DynamicVehicleSimToolGP();

    virtual void InitializeNode( osg::Group* veworldDCS );
    virtual void PreFrameUpdate();
    virtual void SetCurrentCommand( ves::open::xml::CommandPtr command );

protected:

private:
    ///Thread for grabbing data from sim
    void SimulatorCaptureThread();
    ///
    void UpdateSelectedGeometryPositions();
    ///
    void SetupGeometryDataMaps();
    ///
    void SimulatorControlUpdate();
    ///Set the position data
    void SetPositionData( std::vector< double >& temp );
    ///Get the position data
    void GetPositionData( std::vector< double >& temp );
    ///
    void SetSimState( std::string& temp );
    ///
    void GetSimState( std::string& temp );
    ///
    void SetComputerData( std::string& computerName, std::string& computerPort );
    ///
    void GetComputerData( std::string& computerName, std::string& computerPort );
        
    ///Sample thread
    vpr::Thread* m_sampleThread;
    ///Position buffer
    std::vector< double > m_positionBuffer;
    ///A mutex to protect variables accesses
    vpr::Mutex mValueLock;
    ///
    std::string m_simState;
    ///
    std::string m_computerName;
    ///
    std::string m_computerPort;
    ///
    ves::open::xml::CommandPtr m_currentCommand;
    
    ///Control wether the thread continues to run
    bool m_runSampleThread;
    ///Matrix stack containing position data for the geometry
    std::vector< gmtl::Matrix44d > m_positionStack;
    ///vector of names
    std::vector< std::pair< std::string, osg::ref_ptr< ves::xplorer::scenegraph::DCS > > > m_animationedNodes;
    ///cm to feet conversion
    double cm2ft;
    ///The constrained geom pointer
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > m_constrainedGeom;
};

CREATE_VES_XPLORER_PLUGIN_ENTRY_POINT( DynamicVehicleSimToolGP )

} //end warrantytool

#endif //DYNAMIC_VEHICLE_TOOL_GP_H
