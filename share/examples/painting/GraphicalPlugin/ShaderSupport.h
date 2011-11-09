// Name: VirtualPaint Demo
// ShaderSupport.h
//
// Authors:
//   Chris 'Xenon' Hanson, AlphaPixel.
//   John Enright, Digital Transforms.
//
// Origination Date: July 19, 2011

#include <osg/Node>

// --------------------------------------------------------------------------

#define TEXUNIT_BG                  0
#define TEXUNIT_ACCUM               1
#define TEXUNIT_PERM                2
#define TEXUNIT_SPATTER             0 // not used simultaneously with above

// vertex attribute subscripts, not related to above
#define TANGENT_ATR_UNIT	6
#define BINORMAL_ATR_UNIT	7


void toggleAccumEnabled(void);
void setupShaders(osg::Node* background, osg::Node* model);
void setupSpatterShaders(osg::Node* background);
void setSpatterStrength(float newStrength);