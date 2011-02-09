/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VR Juggler is (C) Copyright 1998-2010 by Iowa State University
 *
 * Original Authors:
 *   Allen Bierbaum, Christopher Just,
 *   Patrick Hartling, Kevin Meinert,
 *   Carolina Cruz-Neira, Albert Baker
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
 *************** <auto-copyright.pl END do not edit this line> ***************/

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

#include <iostream>

//NSString* VRJMaxRecentFiles = @"VRJMaxRecentFiles";
//NSString* VRJRecentCfgFiles = @"VRJRecentCfgFiles";
//enum {
//    QtCocoaEventSubTypeWakeup       = SHRT_MAX,
//    QtCocoaEventSubTypePostMessage  = SHRT_MAX-1
//};

//To surpress a Cocoa compile warning
@interface NSApplication ()
- (BOOL)qt_sendEvent:(NSEvent *)event;
- (void)qt_sendPostedMessage:(NSEvent *)event;
@end

@implementation VESDelegate
    -(void) preRun
    {
        m_app->preRun();
    }

    -(void) runLoop
    {
        m_app->runLoop();

        //if( m_app->AcquireQtLock() )
        {
            NSEvent* event = [NSApp nextEventMatchingMask:NSAnyEventMask
                                          untilDate:[NSDate distantFuture]
                                             inMode:NSDefaultRunLoopMode
                                            dequeue:YES];
            //If it is not a qt event...send it normally
            if( ![NSApp qt_sendEvent:event] )
            {
            //std::cout << "here 1 " << std::endl << std::flush;
                [NSApp sendEvent:event];
            }
        }
    }

    -(void) setApp:(ves::xplorer::App*) app
    {
        m_app = app;
    }

    -(BOOL) ves_sendEvent:( NSEvent* ) event
    {
        /*if ([event type] == NSApplicationDefined) 
        {
            std::cout << [event subtype] << " " << QtCocoaEventSubTypePostMessage << std::endl << std::flush;
            switch ([event subtype]) {
                case QtCocoaEventSubTypePostMessage:
                    //if( m_app->AcquireQtLock() )
                    {
                        [NSApp qt_sendPostedMessage:event];
                        //m_app->ReleaseQtLock();
                    }
                    return true;
                default:
                    break;
            }
        }*/
        return false;
    }
@end
