/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
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

#include <vrj/vrjConfig.h>

#import <Foundation/NSDictionary.h>
#import <Foundation/NSLock.h>
#import <Foundation/NSNotification.h>
#import <Foundation/NSPathUtilities.h>
#import <AppKit/NSApplication.h>

#include <gadget/Devices/KeyboardMouseDevice/InputAreaCocoa.h>

#import <vrj/Kernel/Kernel.h>
#import "VESDelegate.h"
#import "App.h"

//NSString* VRJMaxRecentFiles = @"VRJMaxRecentFiles";
//NSString* VRJRecentCfgFiles = @"VRJRecentCfgFiles";

//To surpress a Cocoa compile warning
@interface NSApplication ()
- (BOOL)qt_sendEvent:(NSEvent *)event;
@end

@implementation VESDelegate
    -(void) preRun
    {
        m_app->preRun();
    }

    -(void) runLoop
    {
        m_app->runLoop();

        NSEvent* event = [NSApp nextEventMatchingMask:NSAnyEventMask
                                          untilDate:[NSDate distantFuture]
                                             inMode:NSDefaultRunLoopMode
                                            dequeue:YES];
        
        if (![NSApp qt_sendEvent:event])
            [NSApp sendEvent:event];
    }

    -(void) setApp:(ves::xplorer::App*) app
    {
        m_app = app;
    }
@end
