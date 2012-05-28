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
#pragma once

#include "ManagerAbstract.h"
#include "ApplicationEventMonitorInterface.h"

namespace tecplot
{
namespace sdk
{
namespace integration
{

class Manager : public ManagerAbstract
{
private:
    Manager();
    virtual ~Manager();
    //Manager( Manager const& rhs );
    //Manager& operator=( Manager const& ){;}

public:
    ///Singleton accessor method
    ///http://www.devarticles.com/c/a/Cplusplus/C-plus-plus-In-Theory-The-Singleton-Pattern-Part-I/4/
    ///http://en.wikipedia.org/wiki/Singleton_pattern
    static Manager& instance();
    ///Setup the manager to start loading data
    void OneTimeSetup();
    ///Clean up the manager
    void OneTimeCleanup();
    //
    virtual PageManagerInterface& getPageManager();
    virtual EventManagerInterface& getEventManager();
    virtual DialogManagerInterface& getDialogManager();
    virtual services::ServiceRepositoryInterface& getServiceRepository();
    virtual ManagerStartReturnCode_e init( std::string homeDir,
                                           std::string oemAuthorizationCode = "" );
    virtual ManagerStartReturnCode_e init( int          argc,
                                           char* const* argv,
                                           std::string homeDir,
                                           std::string oemAuthorizationCode = "" );
    virtual ManagerInterface::ManagerStartReturnCode_e start();
    virtual void stop();
    virtual std::string getHelpAbout( void );
    virtual void addManagerListener( ManagerListenerInterface* const listener );
    virtual void removeManagerListener( ManagerListenerInterface* const listener );
    void setApplicationEventMonitor( ApplicationEventMonitorInterface* appEventMonitor );
protected:
    virtual ApplicationEventMonitorInterface& getApplicationEventMonitor();
    virtual void installTimerEventProvider();
private:
    std::auto_ptr<ApplicationEventMonitorInterface> m_appEventMonitor;
};
}
}
}
