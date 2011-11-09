// Name: VirtualPaint Demo
// RTTScene.h
//
// Authors:
//   Chris 'Xenon' Hanson, AlphaPixel.
//   John Enright, Digital Transforms.
//
// Origination Date: July 19, 2011

#include <osg/Node>

osg::Node* createPreRenderGraph( const unsigned int &width, const unsigned int &height);
void RTTSpray(double xPos, double yPos, double xMax, double yMax, float strength = 1.0f);
void StopRTTSpray(void);
void setClearOnNextFrame(void);
void frameUpdate(void);

// normally private
#include <osg/Camera>
osg::Node* createPreRenderSubGraph( double aspect );
void configureCameraReferenceFrame(osg::Camera *camera);


