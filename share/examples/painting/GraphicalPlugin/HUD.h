#ifndef HUD_H
#define HUD_H

// Name: VirtualPaint Demo
// HUD.h
//
// Authors:
//   Chris 'Xenon' Hanson, AlphaPixel.
//   John Enright, Digital Transforms.
//
// Origination Date: July 19, 2011

#include <osg/Node>

#include "PaintGun.h"
#include "Lesson.h"

osg::Node *BuildHud(const Lesson &lesson, const PaintGun &gun);
void UpdateHud(const Lesson &lesson, const PaintGun &gun);

#endif