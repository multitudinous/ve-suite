#include "Manager.h"
#include "Exception.h"
using tecplot::toolbox::Exception;


namespace tecplot { namespace sdk { namespace integration { 

/************************************************************************/
/* Manager takes ownership of appEventMonitor and will destoy it on Manager::stop()
/************************************************************************/
Manager::Manager() :
    ManagerAbstract()
{
    m_appEventMonitor.reset(NULL);
}

Manager::~Manager(void)
{
}

PageManagerInterface& Manager::getPageManager()
{
    return ManagerAbstract::getPageManager();
}

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

void Manager::installTimerEventProvider()
{
}

}}}