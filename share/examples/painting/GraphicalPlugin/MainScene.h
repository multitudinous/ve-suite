// Name: VirtualPaint Demo
// MainScene.h
//
// Authors:
//   Chris 'Xenon' Hanson, AlphaPixel.
//   John Enright, Digital Transforms.
//
// Origination Date: July 19, 2011

#include <osg/Camera>
#include <osg/Texture2D>

void createMainRenderGraph( osg::Camera *mainCamera, osg::Texture2D* tex);
osg::Node *BuildBackground(void);
static osg::Node *LoadPaintSurface(void);
