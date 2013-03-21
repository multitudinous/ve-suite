#import "CocoaScreenResolution.h"

#import <Foundation/NSGeometry.h>
#import <Foundation/NSArray.h>

#import <AppKit/NSScreen.h>

std::pair< int, int > getScreenResolution()
{
    
    //NSArray* screenArray = [NSScreen screens];
    NSScreen* mainScreen = [NSScreen mainScreen];
    //unsigned screenCount = [screenArray count];
    //unsigned index  = 0;
    
    //for (index; index < screenCount; index++)
    {
        //NSScreen *screen = [screenArray objectAtIndex: index];
        //NSRect screenRect = [screen visibleFrame];
        NSRect screenRect = [mainScreen visibleFrame];

        //NSString *mString = ((mainScreen == screen) ? @"Main" : @"not-main");
        
        return std::make_pair< int, int >( screenRect.size.width, screenRect.size.height );
        //NSLog(@"Screen #%d (%@) Frame: %@", index, mString, NSStringFromRect(screenRect));
    }
}

