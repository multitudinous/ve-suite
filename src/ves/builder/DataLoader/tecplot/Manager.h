#pragma once

#include "ManagerAbstract.h"
#include "ApplicationEventMonitorInterface.h"

namespace tecplot { namespace sdk { namespace integration { 

class Manager : public ManagerAbstract
{
public:
    Manager();
    virtual ~Manager(void);
    virtual PageManagerInterface& getPageManager();
    virtual EventManagerInterface& getEventManager();
    virtual DialogManagerInterface& getDialogManager();
    virtual services::ServiceRepositoryInterface& getServiceRepository();
    virtual ManagerStartReturnCode_e init(std::string homeDir,
                                          std::string oemAuthorizationCode = "");
    virtual ManagerStartReturnCode_e init(int          argc,
                                          char* const* argv,
                                          std::string homeDir,
                                          std::string oemAuthorizationCode = "");
    virtual ManagerInterface::ManagerStartReturnCode_e start();
    virtual void stop();
    virtual std::string getHelpAbout(void);
    virtual void addManagerListener(ManagerListenerInterface * const listener);
    virtual void removeManagerListener(ManagerListenerInterface * const listener);
    void setApplicationEventMonitor(ApplicationEventMonitorInterface* appEventMonitor);
protected:
    virtual ApplicationEventMonitorInterface& getApplicationEventMonitor();
    virtual void installTimerEventProvider();
private:
    std::auto_ptr<ApplicationEventMonitorInterface> m_appEventMonitor;
};
}}}
