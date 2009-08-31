#pragma once

#include "ApplicationEventMonitorInterface.h"

namespace tecplot { namespace sdk { namespace integration {

class ApplicationEventMonitor : public ApplicationEventMonitorInterface
{
public:
    ApplicationEventMonitor(void);
    virtual ~ApplicationEventMonitor(void);

    virtual bool isBusy();
    virtual bool isInterrupted();

    virtual void addEventMonitorListener(ApplicationEventMonitorListenerInterface* const listener);
    virtual void removeEventMonitorListener(ApplicationEventMonitorListenerInterface* const listener);
    virtual void addMouseListener(MouseEventListenerInterface* const listener);
    virtual void removeMouseListener(MouseEventListenerInterface* const listener);
    virtual void addKeyboardListener(KeyEventListenerInterface* const listener);
    virtual void removeKeyboardListener(KeyEventListenerInterface* const listener);
    virtual void addOnIdleListener(OnIdleListenerInterface* const listener);
    virtual void removeOnIdleListener(OnIdleListenerInterface* const listener);

    virtual void processWhileBusy_Internal();

    virtual bool start();
    virtual bool stop();
    virtual void interrupt();
};

}}}
