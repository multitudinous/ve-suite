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
