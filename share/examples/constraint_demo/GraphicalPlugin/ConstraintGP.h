/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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

#ifndef Constraint_GP_H
#define Constraint_GP_H

// --- VE-Suite Includes --- //
#include <ves/xplorer/plugin/PluginBase.h>
#include <ves/open/xml/DataValuePairPtr.h>
#include <ves/open/xml/CommandPtr.h>

#include <vpr/Thread/Thread.h>
#include <vpr/Sync/Mutex.h>

#include <map>
#include <vector>
#include <utility>
#include <string>

#include <switchwire/EventManager.h>
#include <switchwire/OptionalMacros.h>

#include <Poco/Tuple.h>
#include <Poco/Data/Statement.h>
#include <Poco/Data/RecordSet.h>

class btDiscreteDynamicsWorld;
namespace osgbInteraction
{
    class SaveRestoreHandler;
}

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
class VE_USER_PLUGIN_EXPORTS ConstraintGP :
    public ves::xplorer::plugin::PluginBase
{
public:
    ConstraintGP();
    virtual ~ConstraintGP();

    virtual void InitializeNode( osg::Group* veworldDCS );
    virtual void PreFrameUpdate();
    virtual void SetCurrentCommand( ves::open::xml::CommandPtr command );
    virtual void RemoveSelfFromSG();

protected:

private:
    ///Initialize the vpaint demo graph
    int InitializeConstraintGraph();

    ///
    osg::Node* fixWalls( osg::Node* wallsNode );
    
    ///
    void makeStaticObject( btDiscreteDynamicsWorld* bw, 
        osg::Node* node, const osg::Matrix& m );

    ///
    osg::Transform* makeGate( btDiscreteDynamicsWorld* bw, 
        osgbInteraction::SaveRestoreHandler* srh, osg::Node* node, 
        const osg::Matrix& m );

    ///
    osg::Node* findNamedNode( osg::Node* model, const std::string& name, 
        osg::Matrix& xform );

    ves::xplorer::device::KeyboardMouse* m_keyboard;

    ///Column number for the promise date
    size_t m_promiseDateColumn;
    ///Determine if we have a promise date column
    bool m_hasPromiseDate;
    //Column number for the part numbers
    size_t m_partNumberColumn;
    ///Vector map to be used to create the DB
    std::map< int, std::vector< std::string > > m_csvDataMap;
    ///Control mouse selection
    bool m_mouseSelection;
    
    /// Required to be able to connect up to signals.
    switchwire::ScopedConnectionList m_connections;

    ///Control wether the thread continues to run
    bool m_runSampleThread;
    ///A mutex to protect variables accesses
    vpr::Mutex mValueLock;
    
    btRigidBody* gateBody;
};

CREATE_VES_XPLORER_PLUGIN_ENTRY_POINT( ConstraintGP )

} //end warrantytool

#endif //WARRANTY_TOOL_GP_H