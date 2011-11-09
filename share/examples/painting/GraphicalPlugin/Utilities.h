#ifndef UTILITIES_H
#define UTILITIES_H

// Name: VirtualPaint Demo
// Utilities.h
//
// Authors:
//   Chris 'Xenon' Hanson, AlphaPixel.
//   John Enright, Digital Transforms.
//
// Origination Date: July 19, 2011
// 

#include <string>

#include <osg/Node>

osg::Node *LoadMousePointer(const std::string &mouseCursorImage, float sizex = 0.03f, float sizey = 0.03f);
int getNumScreens(void);


#endif