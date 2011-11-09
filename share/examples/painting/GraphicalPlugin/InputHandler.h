#ifndef INPUTHANDLER_H
#define INPUTHANDLER_H

// Name: VirtualPaint Demo
// InputHandler.h
//
// Authors:
//   Chris 'Xenon' Hanson, AlphaPixel.
//   John Enright, Digital Transforms.
//
// Origination Date: July 19, 2011

#include <osgGA/GUIEventHandler>

// --------------------------------------------------------------------------
// This is the main event handler for the app. It will handle keyboard and 
// mouse button presses.

class InputHandler : public osgGA::GUIEventHandler
{
public:
	virtual bool handle(const osgGA::GUIEventAdapter &ea, osgGA::GUIActionAdapter &aa);
};

#endif