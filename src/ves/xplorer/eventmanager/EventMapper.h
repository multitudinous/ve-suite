/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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

#pragma once

#include <string>
#include <map>
#include <vector>

#include <ves/VEConfig.h>
#include <ves/xplorer/eventmanager/ScopedConnectionList.h>

#include <boost/signals2/signal.hpp>

#include <gadget/Type/KeyboardMouse/Keys.h>

// --- VR Juggler includes --- //
#include <vpr/Util/Singleton.h>

namespace ves
{
namespace xplorer
{
namespace eventmanager
{

/**
 * EventMapper exists to provide a way to map key presses / releases and button
 * presses / releases to signals that cause certain behaviors to be executed.
 * It's our version of "hot-key" mappings.
 *
 * In its current state, EventMapper just sets up some sensible default bindings.
 * In future, EventManager will expose an interface that can be used to re-map
 * the bindings at runtime.
 **/
class VE_EVENTMANAGER_EXPORTS EventMapper
{
public:

    void MapEvent( const std::string& KeyButton, const std::string& Behavior );
    void LatePreFrameUpdate();

private:
    /// Constructor
    EventMapper( );

    /// Destructor
    virtual ~EventMapper( );

    /// Singleton declarations
    vprSingletonHeader( EventMapper );

    void SetupDefaultBindings();

    /// Receives mouse double clicks connected via EventManager
    void MouseDoubleClickEvent( gadget::Keys button, int x, int y, int z, int state );

    /// Receives mouse buttonpress events connected via EventManager
    /// state = modifier mask OR'd with button mask
    void ButtonPressEvent( gadget::Keys button, int x, int y, int state );

    /// Receives mouse buttonrelease events connected via EventManager
    /// state = modifier mask OR'd with button mask
    void ButtonReleaseEvent( gadget::Keys button, int x, int y, int state );

    /// Receives key press events connected via EventManager
    void KeyPressEvent( gadget::Keys key, int modifiers, char unicode );

    /// Receives key release events connected via EventManager
    void KeyReleaseEvent( gadget::Keys key, int modifiers, char unicode );

    void QueueAndEmit( std::string& eventName, gadget::Keys key );

    void EmitSyncGraphicsSignals();

    const std::string getKeyName(const gadget::Keys keyId) const;

private:
    ves::xplorer::eventmanager::ScopedConnectionList mConnections;

    typedef boost::signals2::signal< void () > voidSignalType;

    typedef std::map< std::string, std::string > EventBehaviorMapType;
    EventBehaviorMapType mEventBehaviorMap;

    typedef std::map< std::string, voidSignalType* > BehaviorMapType;
    BehaviorMapType mSyncNoneBehaviorMap;

    BehaviorMapType mSyncGraphicsBehaviorMap;

    std::vector< std::string > mSyncGraphicsQueue;



    // All the basic behavior signals go here. All must have the signature
    // void () since there are no logical, generic arguments to a key or button
    // mapping.
    voidSignalType mFrameAllSignal; ///< Show everything in the scene
    voidSignalType mHideShowUISignal; ///< Toggle visibility of UI

};

}
}
}

