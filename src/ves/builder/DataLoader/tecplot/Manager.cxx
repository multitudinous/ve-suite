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
#include <ves/builder/DataLoader/tecplot/Manager.h>
#include <ves/builder/DataLoader/tecplot/ApplicationEventMonitor.h>

#include "Exception.h"

#include <iostream>

using tecplot::toolbox::Exception;


namespace tecplot { namespace sdk { namespace integration { 

/************************************************************************/
/* Manager takes ownership of appEventMonitor and will destoy it on Manager::stop()*/
/************************************************************************/
////////////////////////////////////////////////////////////////////////////////
Manager& Manager::instance()
{
    static Manager self;
    return self;
}
////////////////////////////////////////////////////////////////////////////////
Manager::Manager() :
    ManagerAbstract()
{
    m_appEventMonitor.reset(NULL);
}
////////////////////////////////////////////////////////////////////////////////
/*Manager::Manager( Manager const& rhs )
:
ManagerAbstract( rhs )
{;}*/
////////////////////////////////////////////////////////////////////////////////
Manager::~Manager(void)
{
}
////////////////////////////////////////////////////////////////////////////////
PageManagerInterface& Manager::getPageManager()
{
    return ManagerAbstract::getPageManager();
}
////////////////////////////////////////////////////////////////////////////////
EventManagerInterface& Manager::getEventManager()
{
    return ManagerAbstract::getEventManager();
}

DialogManagerInterface& Manager::getDialogManager()
{
    return ManagerAbstract::getDialogManager();
}

services::ServiceRepositoryInterface& Manager::getServiceRepository()
{
    return ManagerAbstract::getServiceRepository();
}

ManagerInterface::ManagerStartReturnCode_e Manager::init(std::string homeDir,
                                                         std::string oemAuthorizationCode)
{
    char* dummyArgV[] = {"dummy"};
    return init(1,dummyArgV,homeDir,oemAuthorizationCode);
}

ManagerInterface::ManagerStartReturnCode_e Manager::init(int          argc,
                                                         char* const* argv,
                                                         std::string homeDir,
                                                         std::string oemAuthorizationCode)
{
    return ManagerAbstract::init(argc, argv, homeDir, oemAuthorizationCode);
}

ManagerInterface::ManagerStartReturnCode_e Manager::start()
{
    return ManagerAbstract::start();
}

void Manager::stop()
{
    ManagerAbstract::stop();
}

std::string Manager::getHelpAbout(void)
{
    return ManagerAbstract::getHelpAbout();
}

void Manager::addManagerListener(ManagerListenerInterface * const listener)
{
    ManagerAbstract::addManagerListener(listener);
}

void Manager::removeManagerListener(ManagerListenerInterface * const listener)
{
    ManagerAbstract::removeManagerListener(listener);
}

void Manager::setApplicationEventMonitor(ApplicationEventMonitorInterface* appEventMonitor)
{
    m_appEventMonitor.reset(appEventMonitor);
}

ApplicationEventMonitorInterface& Manager::getApplicationEventMonitor()
{
    REQUIRE(m_appEventMonitor.get());
    return *(m_appEventMonitor.get());
}
////////////////////////////////////////////////////////////////////////////////
void Manager::installTimerEventProvider()
{
}
////////////////////////////////////////////////////////////////////////////////
void Manager::OneTimeSetup()
{
    Manager::ManagerStartReturnCode_e ret;
    
    setApplicationEventMonitor( new ApplicationEventMonitor() );
    
    char* tecSDKHomeDir = getenv("TECSDKHOME");
    /*if( tecSDKHomeDir )
    {
        //std::cout << "TECSDKHOME=" << tecSDKHomeDir << std::endl;
        // the following produces screen output showing tecplot version and location of your tecplot.cfg
        ret = init( tecSDKHomeDir );
    }
    else*/
    {
        std::string tempHome("./");
        //std::cout << "TECSDKHOME=" << tempHome << std::endl;
        ret = init( tempHome, "IowaStateBrydenResearchGroup 112 01/10/2011 0 1723380535" );
        //std::cerr << "The environment variable TECSDKHOME must be defined to run Tecplot SDK applications.\n" << std::endl;
        //ret = Manager::ManagerStartReturnCode_HomeDirectoryNotSpecified;
    }
    
    if( ret == Manager::ManagerStartReturnCode_Ok )
    {
        // the following produces screen output showing location of tecplot executable and tecplot home dir
        ret = start();
    }
    else
    {
        std::cerr << "Unable to initialize the this->manager\n" << std::endl;
        exit( 0 );
    }
    
    if( ret == Manager::ManagerStartReturnCode_Ok )
    {
        TecUtilParentLockStart();
        
        // Tecplot starts up "pageless/frameless".
        // Create a Page, and get a Frame, which is required to load data.
        TecUtilPageCreateNew();
    }
    else
    {
        std::cerr << "Failed to initialize the Tecplot SDK: " << returnCodeString( ret ) << std::endl;
    }
    
    //return ((ret == Manager::ManagerStartReturnCode_Ok) ? 0 : 1);
}
////////////////////////////////////////////////////////////////////////////////
void Manager::OneTimeCleanup()
{
    TecUtilParentLockFinish();
    stop();
}
////////////////////////////////////////////////////////////////////////////////
}}}
////////////////////////////////////////////////////////////////////////////////
