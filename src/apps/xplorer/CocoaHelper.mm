#import <Foundation/Foundation.h>
#import <AppKit/NSApplication.h>
#import <Cocoa/Cocoa.h>
#import "VESDelegate.h"

void CocoaInit( ves::xplorer::App* app )
{
    [[NSApp delegate] setApp:app];
}
