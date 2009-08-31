#include "ApplicationEventMonitor.h"

#include "Exception.h"
using tecplot::toolbox::Exception;

namespace tecplot { namespace sdk { namespace integration {

ApplicationEventMonitor::ApplicationEventMonitor(void)
{
}

ApplicationEventMonitor::~ApplicationEventMonitor(void)
{
}

bool ApplicationEventMonitor::isBusy()
{
    return false;
}

bool ApplicationEventMonitor::isInterrupted()
{
    return false;
}


void ApplicationEventMonitor::addEventMonitorListener(ApplicationEventMonitorListenerInterface* const listener)
{
}

void ApplicationEventMonitor::removeEventMonitorListener(ApplicationEventMonitorListenerInterface* const listener)
{
}
void ApplicationEventMonitor::addMouseListener(MouseEventListenerInterface* const listener)
{
}
void ApplicationEventMonitor::removeMouseListener(MouseEventListenerInterface* const listener)
{
}
void ApplicationEventMonitor::addKeyboardListener(KeyEventListenerInterface* const listener)
{
}
void ApplicationEventMonitor::removeKeyboardListener(KeyEventListenerInterface* const listener)
{
}
void ApplicationEventMonitor::addOnIdleListener(OnIdleListenerInterface* const listener)
{
}
void ApplicationEventMonitor::removeOnIdleListener(OnIdleListenerInterface* const listener)
{
}
void ApplicationEventMonitor::processWhileBusy_Internal()
{
}

bool ApplicationEventMonitor::start()
{
    return true;
}

bool ApplicationEventMonitor::stop()
{
    return true;
}

void ApplicationEventMonitor::interrupt()
{
}

}}}
