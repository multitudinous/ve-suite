// Name: VirtualPaint Demo
// InputHandler.cpp
//
// Authors:
//   Chris 'Xenon' Hanson, AlphaPixel.
//   John Enright, Digital Transforms.
//
// Origination Date: July 19, 2011

// STL includes
#include <string>
#include <iostream>
#include <sstream>

#include "InputHandler.h"
#include "ShaderSupport.h"
#include "Lesson.h"
#include "RTTScene.h"

// a convenience so don't have to prefix STL stuff with "std::".
using namespace std;
// ditto osg
using namespace osg;

extern Lesson gLesson;

// **************************************************************************
// Handle input for application global things.

bool InputHandler::handle(const osgGA::GUIEventAdapter &ea, osgGA::GUIActionAdapter &aa)

{
	/*switch (ea.getEventType())
	{
	case osgGA::GUIEventAdapter::KEYDOWN:
      switch(ea.getKey())
      {
      case 'a': toggleAccumEnabled(); break;
      case 'c':
         setClearOnNextFrame();
         gLesson.SetTotalPaintAccumulation(0);
         break;
      default:
         break;
      }
      break;
   case osgGA::GUIEventAdapter::KEYUP:
      break;
   default:
      break;
   }*/
   return false;
}
