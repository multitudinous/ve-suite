// Name: VirtualPaint Demo
// GunInputAdapterMouse.cpp
//
// Authors:
//   Chris 'Xenon' Hanson, AlphaPixel.
//   John Enright, Digital Transforms.
//
// Origination Date: July 19, 2011

#include <osg/CameraNode>

#include "GunInputAdapterMouse.h"
#include "SoundSystem.h"
#include "RTTScene.h"

extern SoundSystem g_SoundSystem;

// **************************************************************************
// This will move the mouse cursor / pointer object to the current mouse position
// in order to act as a 3D mouse pointer object.

bool MouseTracker::handle(const osgGA::GUIEventAdapter &ea, osgGA::GUIActionAdapter &aa, osg::Object *object, osg::NodeVisitor *visitor)

{
	/*osgGA::GUIEventAdapter::EventType evtType = ea.getEventType();
	if ((evtType != osgGA::GUIEventAdapter::MOVE) && (evtType != osgGA::GUIEventAdapter::DRAG))
		return false;
	osg::CameraNode *camera = dynamic_cast<osg::CameraNode*>(object);
	if (camera)
		camera->setViewMatrix(osg::Matrixd::translate(ea.getXnormalized(), ea.getYnormalized(), 0.0));*/
	return false;
}

// **************************************************************************

bool GunInputAdapterMouse::handle(const osgGA::GUIEventAdapter &ea, osgGA::GUIActionAdapter &aa)

{
   /*switch (ea.getEventType())
      {
      case osgGA::GUIEventAdapter::FRAME:          // this event occurs every frame render.
		 frameUpdate(); // this is not a good arrangement
         if (_gun.valid() && _gun->GetTriggerPullAmount())
            RTTSpray(ea.getX(), ea.getY(), ea.getXmax(), ea.getYmax(), _gun->GetTriggerPullAmount());
         break;
      case osgGA::GUIEventAdapter::SCROLL:         // the mouse scroll wheel
         switch (ea.getScrollingMotion())
            {
            case osgGA::GUIEventAdapter::SCROLL_UP:
               if(ea.getModKeyMask() & osgGA::GUIEventAdapter::MODKEY_CTRL)
               {
                   incButtonTriggerPullAmount();
               } // if
               else
               {
                   if (_gun.valid())
                       _gun->DecDistance();
               }
               break;
            case osgGA::GUIEventAdapter::SCROLL_DOWN:
                if(ea.getModKeyMask() & osgGA::GUIEventAdapter::MODKEY_CTRL)
                {
                    decButtonTriggerPullAmount();
                } // if
                else
                {
                    if (_gun.valid())
                        _gun->IncDistance();
                }
               break;
            case osgGA::GUIEventAdapter::SCROLL_LEFT:
               if (_gun.valid())
                  _gun->DecPitch();
               break;
            case osgGA::GUIEventAdapter::SCROLL_RIGHT:
               if (_gun.valid())
                  _gun->IncPitch();
               break;
            default:
               break;
            }
         break;
      case osgGA::GUIEventAdapter::PUSH:           // mouse button press
         if (_gun.valid())
            _gun->SetTriggerPullAmount(getButtonTriggerPullAmount());
         g_SoundSystem.StartSpray();
         break;
      case osgGA::GUIEventAdapter::RELEASE:        // mouse button release
         if (_gun.valid())
            _gun->SetTriggerPullAmount(0);
         g_SoundSystem.StopSpray();
		 StopRTTSpray();
         break;
      default:
         break;
      }*/
	return false;
}
