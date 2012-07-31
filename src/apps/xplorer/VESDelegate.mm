/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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
 * Date modified: $Date: 2011-12-30 16:58:13 -0600 (Fri, 30 Dec 2011) $
 * Version:       $Rev: 16779 $
 * Author:        $Author: mccdo $
 * Id:            $Id: App.cxx 16779 2011-12-30 22:58:13Z mccdo $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
#include <vrj/vrjConfig.h>

#import <Foundation/NSDictionary.h>
#import <Foundation/NSLock.h>
#import <Foundation/NSNotification.h>
#import <Foundation/NSPathUtilities.h>
#import <AppKit/NSApplication.h>

#ifdef __APPLE__
//https://bugs.launchpad.net/hugin/+bug/910160
#ifdef check
#undef check
#endif
#endif

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

        NSEvent* event = [NSApp nextEventMatchingMask:NSAnyEventMask
                                      untilDate:[NSDate distantFuture]
                                         inMode:NSDefaultRunLoopMode
                                        dequeue:YES];
        //For more information on the code below please see the qt source:
        //http://google.com/codesearch/p?hl=en#yTMZqbVYb-M/src/gui/kernel/qcocoaapplication_mac.mm&q=qt_sendEvent&sa=N&cd=14&ct=rc
        //http://google.com/codesearch/p?hl=en#pcZOP4ePwcM/trunk/Ogitor/qt-src-4.7.1/src/gui/kernel/qcocoaapplication_mac.mm&q=qt_sendEvent&d=3
        //http://qt.gitorious.org/qt/qt/blobs/4.8/src/gui/kernel/qapplication_mac.mm
        //http://qt.gitorious.org/qt/qt/blobs/4.8/src/gui/kernel/qapplication_mac.mm#line2611
        //qcocoaaplication_mac.mm
        //If it is not a qt event...send it normally
        if( ![NSApp qt_sendEvent:event] )
        {
            [NSApp sendEvent:event];
        }
    }

    -(void) setApp:(ves::xplorer::App*) app
    {
        m_app = app;
    }

    /*-(BOOL) ves_sendEvent:( NSEvent* ) event
    {
        if ([event type] == NSApplicationDefined) 
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
        }
        return false;
    }*/
@end
